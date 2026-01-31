"""
AIFPL Analyzer - performs static analysis on AST for compilation.

This analyzer runs after desugaring/optimization to collect metadata needed
for efficient code generation:
- Free variable analysis for lambdas (closure capture)
- Self-recursion detection (for tail call optimization)
- Mutual recursion detection (for letrec bindings)
- Scope analysis (for variable resolution)

The analysis results are used by the compiler to generate correct bytecode
without runtime patching.
"""

from typing import Dict, Set, List, Tuple, Optional
from dataclasses import dataclass, field

from aifpl.aifpl_value import AIFPLValue, AIFPLSymbol, AIFPLList
from aifpl.aifpl_dependency_analyzer import AIFPLDependencyAnalyzer, AIFPLBindingGroup
from aifpl.aifpl_builtins import BUILTIN_TABLE, SPECIAL_FORMS


@dataclass
class LambdaInfo:
    """Analysis information for a lambda expression."""
    ast_node: AIFPLList  # The lambda expression itself
    params: List[str]  # Parameter names
    free_vars: List[str]  # Free variables (need to be captured)
    self_name: Optional[str] = None  # Name if bound in let/letrec (for self-recursion)
    is_self_recursive: bool = False  # Does it reference itself?
    lambda_siblings: Set[str] = field(default_factory=set)  # Lambda siblings that need patching (mutual recursion)


@dataclass
class LetrecInfo:
    """Analysis information for a letrec expression."""
    ast_node: AIFPLList  # The letrec expression itself
    binding_groups: List[AIFPLBindingGroup]  # Dependency groups from analyzer
    recursive_bindings: Set[str]  # Which bindings are recursive
    mutual_recursion_groups: List[Set[str]]  # Groups of mutually recursive bindings


@dataclass
class AnalysisResult:
    """
    Results of static analysis on an AST.

    Provides metadata that the compiler can query to generate efficient code.
    """
    # Map from AST node id to lambda info
    lambda_info: Dict[int, LambdaInfo] = field(default_factory=dict)

    # Map from AST node id to letrec info
    letrec_info: Dict[int, LetrecInfo] = field(default_factory=dict)

    def get_lambda_info(self, lambda_node: AIFPLList) -> Optional[LambdaInfo]:
        """Get analysis info for a lambda node."""
        return self.lambda_info.get(id(lambda_node))

    def get_letrec_info(self, letrec_node: AIFPLList) -> Optional[LetrecInfo]:
        """Get analysis info for a letrec node."""
        return self.letrec_info.get(id(letrec_node))


class AIFPLAnalyzer:
    """
    Performs static analysis on AIFPL AST.

    This analyzer collects metadata needed for efficient compilation:
    - Free variables for closures
    - Recursion patterns
    - Scope information
    """

    def __init__(self) -> None:
        self.dependency_analyzer = AIFPLDependencyAnalyzer()

        # Build set of builtin names that should not be treated as free variables
        # These are globally available and don't need to be captured
        self.builtins = set(BUILTIN_TABLE) | SPECIAL_FORMS

    def analyze(self, expr: AIFPLValue) -> AnalysisResult:
        """
        Analyze an expression and return metadata.

        Args:
            expr: AST to analyze (after desugaring/optimization)

        Returns:
            AnalysisResult containing metadata for compilation
        """
        result = AnalysisResult()

        # Traverse AST and collect information
        self._analyze_expr(expr, result, bound_vars=set(), let_context=None)

        return result

    def _analyze_expr(
        self,
        expr: AIFPLValue,
        result: AnalysisResult,
        bound_vars: Set[str],
        let_context: Optional[Tuple[str, Set[str], Set[str]]] = None  # (binding_name, all_siblings, lambda_siblings)
    ) -> None:
        """
        Recursively analyze an expression.

        Args:
            expr: Expression to analyze
            result: AnalysisResult to populate
            bound_vars: Variables bound in enclosing scopes
            let_context: Context if we're inside a let/letrec binding (name, siblings)
        """
        # Only lists need analysis
        if not isinstance(expr, AIFPLList) or expr.is_empty():
            return

        first = expr.first()

        # Check for special forms
        if isinstance(first, AIFPLSymbol):
            name = first.name

            if name == 'lambda':
                self._analyze_lambda(expr, result, bound_vars, let_context)
                return

            if name == 'let':
                self._analyze_let(expr, result, bound_vars)
                return

            if name == 'letrec':
                self._analyze_letrec(expr, result, bound_vars)
                return

            if name == 'if':
                self._analyze_if(expr, result, bound_vars, let_context)
                return

            # Other special forms (quote, and, or, etc.) - just recurse

        # Regular list - recurse into all elements
        for elem in expr.elements:
            self._analyze_expr(elem, result, bound_vars, let_context)

    def _analyze_lambda(
        self,
        expr: AIFPLList,
        result: AnalysisResult,
        bound_vars: Set[str],
        let_context: Optional[Tuple[str, Set[str], Set[str]]]  # (binding_name, all_siblings, lambda_siblings)
    ) -> None:
        """Analyze a lambda expression."""
        # Extract parameters and body
        # Structure: (lambda (params...) body)
        params_list = expr.elements[1]
        body = expr.elements[2]

        assert isinstance(params_list, AIFPLList), "Lambda params must be list (validated by semantic analyzer)"

        # Extract parameter names
        param_names = []
        for param in params_list.elements:
            assert isinstance(param, AIFPLSymbol), "Lambda params must be symbols (validated by semantic analyzer)"
            param_names.append(param.name)

        # Find free variables in body
        # Use _find_variables which correctly handles nested scopes
        # It returns variables that aren't bound within the body itself
        # (handles nested lambdas, lets, letrecs properly)
        body_vars = self._find_variables(body)

        # Variables bound by this lambda
        lambda_bound = set(param_names)

        # Free variables: used in body, not bound by lambda params
        # Exclude builtins (globally available, don't need capture)
        # Include variables from bound_vars (outer scope) - these need to be captured!
        # Also include undefined variables (will error at runtime, but that's not our job)
        free_vars = [v for v in body_vars if v not in lambda_bound and v not in self.builtins]

        # Check for self-recursion
        self_name = None
        is_self_recursive = False
        lambda_siblings_set = set()

        if let_context:
            binding_name, sibling_names, lambda_siblings = let_context
            self_name = binding_name

            # Check if this lambda references its own binding
            if binding_name in body_vars:
                is_self_recursive = True
                # Remove self-reference from free vars (will be patched)
                free_vars = [v for v in free_vars if v != binding_name]

            # Check for references to sibling bindings (mutual recursion)
            for var in body_vars:
                if var in sibling_names and var != binding_name:
                    # Check if this sibling is a lambda
                    if var in lambda_siblings:
                        # Lambda sibling - needs patching
                        lambda_siblings_set.add(var)
                        # Remove from free_vars (will be patched, not captured)
                        free_vars = [v for v in free_vars if v != var]
                    # else: non-lambda sibling stays in free_vars for normal capture

        # Store lambda info
        info = LambdaInfo(
            ast_node=expr,
            params=param_names,
            free_vars=free_vars,
            self_name=self_name,
            is_self_recursive=is_self_recursive,
            lambda_siblings=lambda_siblings_set
        )
        result.lambda_info[id(expr)] = info

        # Recurse into body with extended bound vars
        new_bound = bound_vars | lambda_bound
        self._analyze_expr(body, result, new_bound, let_context)

    def _analyze_let(
        self,
        expr: AIFPLList,
        result: AnalysisResult,
        bound_vars: Set[str]
    ) -> None:
        """Analyze a let expression."""
        # Structure: (let ((var val) ...) body)
        bindings_list = expr.elements[1]
        body = expr.elements[2]

        assert isinstance(bindings_list, AIFPLList), "Let bindings must be list (validated by semantic analyzer)"

        # Extract binding names
        binding_names = set()
        for binding in bindings_list.elements:
            assert isinstance(binding, AIFPLList), "Let binding must be list (validated by semantic analyzer)"
            name_expr = binding.elements[0]
            assert isinstance(name_expr, AIFPLSymbol), "Let binding name must be symbol (validated by semantic analyzer)"
            binding_names.add(name_expr.name)

        # Analyze binding values
        # For let (not letrec), bindings can't reference each other or themselves
        # But they can reference outer scope
        for binding in bindings_list.elements:
            value_expr = binding.elements[1]
            # No let_context for regular let (not recursive)
            self._analyze_expr(value_expr, result, bound_vars, let_context=None)

        # Analyze body with extended scope
        new_bound = bound_vars | binding_names
        self._analyze_expr(body, result, new_bound, let_context=None)

    def _analyze_letrec(
        self,
        expr: AIFPLList,
        result: AnalysisResult,
        bound_vars: Set[str]
    ) -> None:
        """Analyze a letrec expression."""
        # Structure: (letrec ((var val) ...) body)
        bindings_list = expr.elements[1]
        body = expr.elements[2]

        assert isinstance(bindings_list, AIFPLList), "Letrec bindings must be list (validated by semantic analyzer)"

        # Extract bindings
        bindings = []
        binding_names = set()
        for binding in bindings_list.elements:
            assert isinstance(binding, AIFPLList), "Letrec binding must be list (validated by semantic analyzer)"
            name_expr = binding.elements[0]
            value_expr = binding.elements[1]
            assert isinstance(name_expr, AIFPLSymbol), "Letrec binding name must be symbol (validated by semantic analyzer)"
            bindings.append((name_expr.name, value_expr))
            binding_names.add(name_expr.name)

        # Use dependency analyzer to find SCCs (strongly connected components)
        binding_groups = self.dependency_analyzer.analyze_letrec_bindings(bindings)

        # Identify recursive bindings and mutual recursion groups
        recursive_bindings = set()
        mutual_recursion_groups = []

        for group in binding_groups:
            if group.is_recursive:
                # This group has recursion
                recursive_bindings.update(group.names)

                if len(group.names) > 1:
                    # Mutual recursion
                    mutual_recursion_groups.append(group.names)

        # Store letrec info
        letrec_info = LetrecInfo(
            ast_node=expr,
            binding_groups=binding_groups,
            recursive_bindings=recursive_bindings,
            mutual_recursion_groups=mutual_recursion_groups
        )
        result.letrec_info[id(expr)] = letrec_info

        # Analyze binding values with full letrec scope
        # All bindings can see each other (including themselves)
        new_bound = bound_vars | binding_names

        # Identify which bindings are lambdas
        lambda_bindings = set()
        for name, value_expr in bindings:
            if isinstance(value_expr, AIFPLList) and not value_expr.is_empty():
                first = value_expr.first()
                if isinstance(first, AIFPLSymbol) and first.name == 'lambda':
                    lambda_bindings.add(name)

        for name, value_expr in bindings:
            # Pass let_context: (binding_name, all_siblings, lambda_siblings)
            let_context = (name, binding_names, lambda_bindings)
            self._analyze_expr(value_expr, result, new_bound, let_context)

        # Analyze body
        self._analyze_expr(body, result, new_bound, let_context=None)

    def _analyze_if(
        self,
        expr: AIFPLList,
        result: AnalysisResult,
        bound_vars: Set[str],
        let_context: Optional[Tuple[str, Set[str], Set[str]]]
    ) -> None:
        """Analyze an if expression."""
        # Structure: (if condition then else)
        condition = expr.elements[1]
        then_expr = expr.elements[2]
        else_expr = expr.elements[3]

        # Recurse into all branches
        self._analyze_expr(condition, result, bound_vars, let_context)
        self._analyze_expr(then_expr, result, bound_vars, let_context)
        self._analyze_expr(else_expr, result, bound_vars, let_context)

    def _collect_all_symbols(self, expr: AIFPLValue) -> List[str]:
        """
        Collect all symbol references in an expression, without any scope handling.

        This is used for finding free variables in lambda bodies, where we need
        to collect all symbols and then filter based on the context we're in.

        Returns list (not set) to preserve order.
        """
        symbols = []

        if isinstance(expr, AIFPLSymbol):
            symbols.append(expr.name)

        elif isinstance(expr, AIFPLList):
            if not expr.is_empty():
                first = expr.first()

                # Stop at lambda boundaries - nested lambdas will be analyzed separately
                if isinstance(first, AIFPLSymbol) and first.name == 'lambda':
                    # Don't recurse into nested lambdas
                    return symbols

                # For quote, don't collect symbols (they're data, not code)
                if isinstance(first, AIFPLSymbol) and first.name == 'quote':
                    return symbols

                # For everything else, recurse into all elements
                for elem in expr.elements:
                    symbols.extend(self._collect_all_symbols(elem))

        # Other types (numbers, strings, etc.) have no symbols
        return symbols

    def _find_variables(self, expr: AIFPLValue) -> List[str]:
        """
        Find all variable references in an expression.

        Returns list (not set) to preserve order and allow duplicates for counting.
        """
        variables = []

        if isinstance(expr, AIFPLSymbol):
            variables.append(expr.name)

        elif isinstance(expr, AIFPLList):
            if expr.is_empty():
                return variables

            first = expr.first()

            # Handle special forms that bind variables
            if isinstance(first, AIFPLSymbol):
                name = first.name

                if name == 'lambda':
                    # (lambda (params...) body)
                    # Only look at body, params are bound
                    if len(expr.elements) >= 3:
                        params_list = expr.elements[1]
                        body = expr.elements[2]

                        if isinstance(params_list, AIFPLList):
                            # Extract param names
                            param_names = set()
                            for param in params_list.elements:
                                if isinstance(param, AIFPLSymbol):
                                    param_names.add(param.name)

                            # Find vars in body, excluding params
                            body_vars = self._find_variables(body)
                            variables.extend(v for v in body_vars if v not in param_names)

                    return variables

                if name == 'let':
                    # (let ((var val) ...) body)
                    if len(expr.elements) >= 3:
                        bindings_list = expr.elements[1]
                        body = expr.elements[2]

                        if isinstance(bindings_list, AIFPLList):
                            # Find vars in binding values (before bindings are in scope)
                            for binding in bindings_list.elements:
                                if isinstance(binding, AIFPLList) and len(binding.elements) >= 2:
                                    value_expr = binding.elements[1]
                                    variables.extend(self._find_variables(value_expr))

                            # Extract binding names
                            binding_names = set()
                            for binding in bindings_list.elements:
                                if isinstance(binding, AIFPLList) and len(binding.elements) >= 1:
                                    name_expr = binding.elements[0]
                                    if isinstance(name_expr, AIFPLSymbol):
                                        binding_names.add(name_expr.name)

                            # Find vars in body, excluding bindings
                            body_vars = self._find_variables(body)
                            variables.extend(v for v in body_vars if v not in binding_names)

                    return variables

                if name == 'letrec':
                    # (letrec ((var val) ...) body)
                    # All bindings can reference each other
                    if len(expr.elements) >= 3:
                        bindings_list = expr.elements[1]
                        body = expr.elements[2]

                        if isinstance(bindings_list, AIFPLList):
                            # Extract binding names first
                            binding_names = set()
                            for binding in bindings_list.elements:
                                if isinstance(binding, AIFPLList) and len(binding.elements) >= 1:
                                    name_expr = binding.elements[0]
                                    if isinstance(name_expr, AIFPLSymbol):
                                        binding_names.add(name_expr.name)

                            # Find vars in binding values (can reference bindings)
                            for binding in bindings_list.elements:
                                if isinstance(binding, AIFPLList) and len(binding.elements) >= 2:
                                    value_expr = binding.elements[1]
                                    value_vars = self._find_variables(value_expr)
                                    variables.extend(v for v in value_vars if v not in binding_names)

                            # Find vars in body (can reference bindings)
                            body_vars = self._find_variables(body)
                            variables.extend(v for v in body_vars if v not in binding_names)

                    return variables

                if name == 'quote':
                    # Quoted expressions are data, not code
                    return variables

            # Regular list - recurse into all elements
            for elem in expr.elements:
                variables.extend(self._find_variables(elem))

        return variables
