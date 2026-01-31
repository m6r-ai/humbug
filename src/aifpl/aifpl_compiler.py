"""AIFPL bytecode compiler - compiles AST to bytecode."""

from typing import List, Dict, Tuple, Set, cast
from dataclasses import dataclass, field

from aifpl.aifpl_bytecode import CodeObject
from aifpl.aifpl_compilation_plan import (
    ExprPlan, ConstantPlan, VariablePlan, IfPlan, LetPlan, LetrecPlan,
    LambdaPlan, CallPlan, QuotePlan, ErrorPlan, EmptyListPlan,
    AndPlan, OrPlan, ReturnPlan
)
from aifpl.aifpl_codegen import AIFPLCodeGenerator
from aifpl.aifpl_dependency_analyzer import AIFPLDependencyAnalyzer
from aifpl.aifpl_desugarer import AIFPLDesugarer
from aifpl.aifpl_error import AIFPLEvalError
from aifpl.aifpl_optimizer import AIFPLOptimizer
from aifpl.aifpl_value import (
    AIFPLValue, AIFPLInteger, AIFPLFloat, AIFPLComplex,
    AIFPLString, AIFPLBoolean, AIFPLSymbol, AIFPLList
)


@dataclass
class CompilationScope:
    """
    Tracks variable bindings in a lexical scope.

    Maps variable names to their index within the scope.
    """
    bindings: Dict[str, int] = field(default_factory=dict)

    def add_binding(self, name: str, index: int) -> None:
        """
        Add a binding with the given index.

        Note: If a binding with the same name already exists, it will be
        overwritten with a new index. This is intentional to support shadowing.

        Args:
            name: Variable name
            index: Global index for this variable
        """
        self.bindings[name] = index

    def get_binding(self, name: str) -> int | None:
        """Get binding index, or None if not found."""
        return self.bindings.get(name)


@dataclass
class AnalysisContext:
    """
    Analysis context for Phase 1 - tracks scopes for variable resolution.

    This context does NOT emit bytecode or track constants/names.
    It only performs analysis and builds the compilation plan.
    """
    scopes: List[CompilationScope] = field(default_factory=lambda: [CompilationScope()])
    parent_ctx: 'AnalysisContext | None' = None
    next_local_index: int = 0
    max_locals: int = 0
    current_function_name: str | None = None
    current_letrec_binding: str | None = None
    sibling_bindings: List[str] = field(default_factory=list)
    # Track names for global resolution (we need to know what's a global vs local)
    # But we don't assign indices - that's for codegen
    names: Set[str] = field(default_factory=set)
    parent_ref_names: Set[str] = field(default_factory=set)  # Names that are parent references (recursive bindings)

    def push_scope(self) -> None:
        """Enter a new lexical scope."""
        self.scopes.append(CompilationScope())
        total_locals = sum(len(scope.bindings) for scope in self.scopes)
        self.max_locals = max(self.max_locals, total_locals)

    def pop_scope(self) -> CompilationScope:
        """Exit current lexical scope."""
        return self.scopes.pop()

    def update_max_locals(self) -> None:
        """Update max locals based on current scope depth."""
        self.max_locals = max(self.max_locals, self.next_local_index)

    def allocate_local_index(self) -> int:
        """Allocate a new local variable index from the global counter."""
        index = self.next_local_index
        self.next_local_index += 1
        return index

    def current_scope(self) -> CompilationScope:
        """Get current scope."""
        return self.scopes[-1]

    def resolve_variable(self, name: str) -> Tuple[str, int, int]:
        """
        Resolve variable to (type, depth, index).

        Depth is the number of context boundaries (lambda frames) to cross.
        This is used for LOAD_PARENT_VAR to walk the parent frame chain.

        Returns:
            ('local', depth, index) for local variables (depth = context nesting level)
            ('global', 0, 0) for global variables (index assigned during codegen)
        """
        # Search from innermost to outermost scope in current context
        for scope in reversed(self.scopes):
            index = scope.get_binding(name)
            if index is not None:
                return ('local', 0, index)

        # Search parent contexts (each parent context is one frame boundary)
        if self.parent_ctx is not None:
            var_type, parent_depth, index = self.parent_ctx.resolve_variable(name)
            if var_type == 'local':
                # Found in parent context - increment depth
                return ('local', parent_depth + 1, index)
            return (var_type, parent_depth, index)

        # Not found in local scopes, must be global
        self.names.add(name)
        return ('global', 0, 0)

    def add_name(self, name: str) -> None:
        """Track a name (for global resolution)."""
        self.names.add(name)

    def create_child_context(self) -> 'AnalysisContext':
        """Create a child context for nested lambda analysis."""
        child = AnalysisContext()
        child.parent_ctx = self
        child.sibling_bindings = []
        child.parent_ref_names = self.parent_ref_names.copy()  # Inherit parent refs
        return child


class AIFPLCompiler:
    """
    Compiles AIFPL AST to bytecode.

    Uses a single-pass compiler with lexical addressing for variables.
    """

    # Builtin functions that get special treatment
    BUILTIN_ARITHMETIC = {'+', '-', '*', '/', '//', '%', '**'}
    BUILTIN_COMPARISON = {'=', '!=', '<', '>', '<=', '>='}
    BUILTIN_LOGIC = {'and', 'or', 'not'}

    # Map builtin names to indices (for CALL_BUILTIN)
    BUILTIN_TABLE = [
        '+', '-', '*', '/', '//', '%', '**',
        '=', '!=', '<', '>', '<=', '>=',
        'and', 'or', 'not',
        'bit-or', 'bit-and', 'bit-xor', 'bit-not', 'bit-shift-left', 'bit-shift-right',
        'list', 'cons', 'append', 'reverse', 'first', 'rest', 'length', 'last',
        'member?', 'null?', 'position', 'take', 'drop', 'remove', 'list-ref',
        'number?', 'integer?', 'float?', 'complex?', 'string?', 'boolean?', 'list?', 'alist?', 'function?',
        'range',
        'string-append', 'string-length', 'string-upcase', 'string-downcase',
        'string-trim', 'string-replace', 'string-split', 'string-join',
        'string-contains?', 'string-prefix?', 'string-suffix?', 'string-ref',
        'substring', 'string->number', 'number->string', 'string=?', 'string->list', 'list->string',
        'alist', 'alist-get', 'alist-set', 'alist-remove', 'alist-has?',
        'alist-keys', 'alist-values', 'alist-merge', 'alist?',
        'sqrt', 'abs', 'min', 'max', 'pow',
        'sin', 'cos', 'tan', 'log', 'log10', 'exp',
        'round', 'floor', 'ceil',
        'bin', 'hex', 'oct', 'real', 'imag', 'complex',
    ]

    def __init__(self, optimize: bool = True) -> None:
        """
        Initialize compiler.

        Args:
            optimize: Enable AST optimizations (constant folding, etc.)
        """
        self.optimize = optimize
        self.builtin_indices = {name: i for i, name in enumerate(self.BUILTIN_TABLE)}
        self.desugarer = AIFPLDesugarer()
        if optimize:
            self.optimizer = AIFPLOptimizer()

    def compile(self, expr: AIFPLValue, name: str = "<module>") -> CodeObject:
        """
        Compile an AIFPL expression to bytecode.

        Uses two-phase compilation: analysis (build plan) → code generation.

        Args:
            expr: AST to compile
            name: Name for the code object (for debugging)

        Returns:
            Compiled code object
        """
        # Desugar the expression first
        expr = self.desugarer.desugar(expr)

        # Apply optimizations if enabled
        if self.optimize:
            expr = self.optimizer.optimize(expr)

        # Phase 1: Analysis - build compilation plan
        analysis_ctx = AnalysisContext()
        plan = self._analyze_expression(expr, analysis_ctx, in_tail_position=True)

        # Wrap the top-level expression in a ReturnPlan
        # (unless it's already a tail call that doesn't need a return)
        if not (isinstance(plan, CallPlan) and plan.is_tail_call):
            plan = ReturnPlan(value_plan=plan)

        # Phase 2: Code generation - generate bytecode from plan
        codegen = AIFPLCodeGenerator()
        return codegen.generate(plan, name)

    def _analyze_expression(self, expr: AIFPLValue, ctx: AnalysisContext, in_tail_position: bool = False) -> ExprPlan:
        """Analyze an expression and return a compilation plan (Phase 1)."""

        # Cache the type - check once instead of multiple isinstance() calls
        expr_type = type(expr)

        # Self-evaluating values (constants)
        if expr_type in (AIFPLInteger, AIFPLFloat, AIFPLComplex, AIFPLString):
            return ConstantPlan(value=expr)

        if expr_type is AIFPLBoolean:
            return ConstantPlan(value=expr)

        # Symbol (variable reference)
        if expr_type is AIFPLSymbol:
            return self._analyze_variable(cast(AIFPLSymbol, expr).name, ctx)

        # List (function call or special form)
        if expr_type is AIFPLList:
            return self._analyze_list(cast(AIFPLList, expr), ctx, in_tail_position)

        raise AIFPLEvalError(
            message=f"Cannot analyze expression of type {type(expr).__name__}",
            received=str(expr)
        )

    def _analyze_variable(self, name: str, ctx: AnalysisContext) -> VariablePlan:
        """Analyze a variable reference."""
        var_type, depth, index = ctx.resolve_variable(name)
        # Check if this is a parent reference (recursive binding)
        # Only use parent ref if it's from a parent context (depth > 0) and is a recursive binding
        is_parent_ref = (depth > 0) and (name in ctx.parent_ref_names)
        return VariablePlan(
            name=name,
            var_type=var_type,
            depth=depth,
            index=index,
            is_parent_ref=is_parent_ref
        )

    def _analyze_list(self, expr: AIFPLList, ctx: AnalysisContext, in_tail_position: bool) -> ExprPlan:
        """Analyze a list expression (function call or special form)."""
        if expr.is_empty():
            return EmptyListPlan()

        first = expr.first()
        first_type = type(first)

        # Check for special forms
        if first_type is AIFPLSymbol:
            name = cast(AIFPLSymbol, first).name

            if name == 'if':
                return self._analyze_if(expr, ctx, in_tail_position)

            if name == 'let':
                return self._analyze_let(expr, ctx, in_tail_position)

            if name == 'letrec':
                return self._analyze_letrec(expr, ctx, in_tail_position)

            if name == 'lambda':
                return self._analyze_lambda(expr, ctx)

            if name == 'and':
                return self._analyze_and(expr, ctx)

            if name == 'or':
                return self._analyze_or(expr, ctx)

            if name == 'quote':
                return self._analyze_quote(expr)

            if name == 'error':
                return self._analyze_error(expr)

        # Regular function call
        return self._analyze_function_call(expr, ctx, in_tail_position)

    def _analyze_quote(self, expr: AIFPLList) -> QuotePlan:
        """Analyze a quote expression."""
        assert len(expr.elements) == 2, "Quote expression should have exactly 2 elements"
        quoted = expr.elements[1]
        return QuotePlan(quoted_value=quoted)

    def _analyze_error(self, expr: AIFPLList) -> ErrorPlan:
        """Analyze an error expression."""
        assert len(expr.elements) == 2, "Error expression should have exactly 2 elements"
        message = expr.elements[1]
        return ErrorPlan(message=message)

    def _analyze_if(self, expr: AIFPLList, ctx: AnalysisContext, in_tail_position: bool) -> IfPlan:
        """Analyze an if expression."""
        assert len(expr.elements) == 4, "If expression should have exactly 4 elements"

        _, condition, then_expr, else_expr = expr.elements

        # Analyze all three sub-expressions
        condition_plan = self._analyze_expression(condition, ctx, in_tail_position=False)
        then_plan = self._analyze_expression(then_expr, ctx, in_tail_position=in_tail_position)
        else_plan = self._analyze_expression(else_expr, ctx, in_tail_position=in_tail_position)

        # Wrap branches in ReturnPlan when in tail position
        # (unless they're already tail calls that don't need a return)
        if in_tail_position:
            if not (isinstance(then_plan, CallPlan) and then_plan.is_tail_call):
                then_plan = ReturnPlan(value_plan=then_plan)

            if not (isinstance(else_plan, CallPlan) and else_plan.is_tail_call):
                else_plan = ReturnPlan(value_plan=else_plan)

        return IfPlan(
            condition_plan=condition_plan,
            then_plan=then_plan,
            else_plan=else_plan,
            in_tail_position=in_tail_position
        )

    def _analyze_and(self, expr: AIFPLList, ctx: AnalysisContext) -> AndPlan:
        """Analyze an and expression."""
        args = list(expr.elements[1:])
        arg_plans = [self._analyze_expression(arg, ctx, in_tail_position=False) for arg in args]
        return AndPlan(arg_plans=arg_plans)

    def _analyze_or(self, expr: AIFPLList, ctx: AnalysisContext) -> OrPlan:
        """Analyze an or expression."""
        args = list(expr.elements[1:])
        arg_plans = [self._analyze_expression(arg, ctx, in_tail_position=False) for arg in args]
        return OrPlan(arg_plans=arg_plans)

    def _analyze_let(self, expr: AIFPLList, ctx: AnalysisContext, in_tail_position: bool) -> LetPlan:
        """Analyze a let expression."""
        assert len(expr.elements) == 3, "Let expression should have exactly 3 elements"

        _, bindings_list, body = expr.elements
        assert isinstance(bindings_list, AIFPLList), "Binding list should be a list"

        # Push new scope
        ctx.push_scope()

        # Parse and analyze bindings
        binding_plans = []
        for binding in bindings_list.elements:
            assert isinstance(binding, AIFPLList) and len(binding.elements) == 2
            name_expr, value_expr = binding.elements
            assert isinstance(name_expr, AIFPLSymbol)

            name = name_expr.name

            # Analyze value BEFORE adding binding to scope (for shadowing)
            value_plan = self._analyze_expression(value_expr, ctx, in_tail_position=False)

            # NOW allocate index and add to scope
            var_index = ctx.allocate_local_index()
            ctx.current_scope().add_binding(name, var_index)
            ctx.update_max_locals()

            binding_plans.append((name, value_plan, var_index))

        # Analyze body
        body_plan = self._analyze_expression(body, ctx, in_tail_position=in_tail_position)

        # Pop scope
        ctx.pop_scope()

        return LetPlan(
            bindings=binding_plans,
            body_plan=body_plan,
            in_tail_position=in_tail_position
        )

    def _analyze_letrec(self, expr: AIFPLList, ctx: AnalysisContext, in_tail_position: bool) -> LetrecPlan:
        """Analyze a letrec expression."""
        assert len(expr.elements) == 3, "Letrec expression should have exactly 3 elements"

        _, bindings_list, body = expr.elements
        assert isinstance(bindings_list, AIFPLList), "Binding list should be a list"

        # Push new scope
        ctx.push_scope()

        # First pass: Add all binding names to scope (for recursive references)
        binding_pairs = []
        for binding in bindings_list.elements:
            assert isinstance(binding, AIFPLList) and len(binding.elements) == 2
            name_expr, value_expr = binding.elements
            assert isinstance(name_expr, AIFPLSymbol)

            name = name_expr.name
            var_index = ctx.allocate_local_index()
            ctx.current_scope().add_binding(name, var_index)
            binding_pairs.append((name, value_expr, var_index))

        ctx.update_max_locals()

        # Analyze dependencies
        analyzer = AIFPLDependencyAnalyzer()
        binding_info = [(name, value_expr) for name, value_expr, _ in binding_pairs]
        binding_groups = analyzer.analyze_letrec_bindings(binding_info)

        # Determine which bindings are recursive
        recursive_bindings = set()
        for group in binding_groups:
            if group.is_recursive:
                recursive_bindings.update(group.names)

        # Add recursive bindings to parent_ref_names so nested lambdas know about them
        ctx.parent_ref_names.update(recursive_bindings)

        # Second pass: Analyze binding values in topological order
        # The binding_groups are already in topological order from the analyzer
        binding_plans = []

        # Create a map from name to (value_expr, var_index)
        binding_map = {name: (value_expr, var_index) for name, value_expr, var_index in binding_pairs}

        # Process groups in topological order
        for group in binding_groups:
            for name in group.names:
                value_expr, var_index = binding_map[name]

                # Set context for recursive bindings
                old_letrec_binding = ctx.current_letrec_binding
                old_sibling_bindings = ctx.sibling_bindings

                if name in recursive_bindings:
                    ctx.current_letrec_binding = name
                    ctx.sibling_bindings = list(group.names)

                value_plan = self._analyze_expression(value_expr, ctx, in_tail_position=False)

                # Restore context
                ctx.current_letrec_binding = old_letrec_binding
                ctx.sibling_bindings = old_sibling_bindings

                binding_plans.append((name, value_plan, var_index))

        # Analyze body
        body_plan = self._analyze_expression(body, ctx, in_tail_position=in_tail_position)

        # Pop scope
        ctx.pop_scope()

        return LetrecPlan(
            bindings=binding_plans,
            body_plan=body_plan,
            binding_groups=binding_groups,
            recursive_bindings=recursive_bindings,
            in_tail_position=in_tail_position
        )

    def _analyze_lambda(self, expr: AIFPLList, ctx: AnalysisContext) -> LambdaPlan:
        """Analyze a lambda expression."""
        assert len(expr.elements) == 3, "Lambda expression should have exactly 3 elements"

        _, params_list, body = expr.elements
        assert isinstance(params_list, AIFPLList), "Parameter list should be a list"

        # Extract parameter names
        param_names = []
        for param in params_list.elements:
            assert isinstance(param, AIFPLSymbol)
            param_names.append(param.name)

        # Find free variables
        bound_vars = set(param_names)
        free_vars = self._find_free_variables(body, bound_vars, ctx)

        # Separate free variables into captured (from outer scopes) and parent references (recursive bindings)
        captured_vars = []
        free_var_plans = []
        parent_refs = []
        parent_ref_plans = []

        current_binding = ctx.current_letrec_binding
        current_siblings = ctx.sibling_bindings

        for free_var in free_vars:
            # Create a plan for loading this free variable from parent scope
            var_type, depth, index = ctx.resolve_variable(free_var)

            # Check if this is a self-reference or sibling (parent reference)
            is_parent_ref = False
            if current_binding and free_var == current_binding:
                is_parent_ref = True

            elif current_siblings and free_var in current_siblings:
                is_parent_ref = True

            elif free_var in ctx.parent_ref_names:
                # Also check if it's a recursive binding from a parent letrec
                is_parent_ref = True

            parent_refs.append(free_var)
            parent_ref_plans.append(VariablePlan(
                name=free_var, var_type=var_type, depth=depth, index=index, is_parent_ref=is_parent_ref
            ))

        # Create child context for lambda body analysis
        lambda_ctx = ctx.create_child_context()
        lambda_ctx.push_scope()

        # Add parameters to lambda scope
        for param_name in param_names:
            param_index = lambda_ctx.allocate_local_index()
            lambda_ctx.current_scope().add_binding(param_name, param_index)

        # Add captured free variables to lambda scope
        for free_var in captured_vars:
            free_var_index = lambda_ctx.allocate_local_index()
            lambda_ctx.current_scope().add_binding(free_var, free_var_index)

        # Set function name for tail recursion detection
        lambda_ctx.current_function_name = ctx.current_letrec_binding

        # Mark parent refs so they can be identified during body analysis
        lambda_ctx.parent_ref_names = set(parent_refs)
        lambda_ctx.update_max_locals()

        # Analyze lambda body (in tail position)
        body_plan = self._analyze_expression(body, lambda_ctx, in_tail_position=True)

        # Wrap the lambda body in a ReturnPlan
        # (unless it's already a tail call that doesn't need a return)
        if not (isinstance(body_plan, CallPlan) and body_plan.is_tail_call):
            body_plan = ReturnPlan(value_plan=body_plan)

        lambda_ctx.pop_scope()

        return LambdaPlan(
            params=param_names,
            body_plan=body_plan,
            free_vars=captured_vars,
            free_var_plans=free_var_plans,
            parent_refs=parent_refs,
            parent_ref_plans=parent_ref_plans,
            param_count=len(param_names),
            binding_name=ctx.current_letrec_binding,
            sibling_bindings=ctx.sibling_bindings,
            max_locals=lambda_ctx.max_locals
        )

    def _analyze_function_call(self, expr: AIFPLList, ctx: AnalysisContext, in_tail_position: bool) -> CallPlan:
        """Analyze a function call."""
        func_expr = expr.first()
        arg_exprs = list(expr.elements[1:])

        func_type = type(func_expr)

        # Check if calling a known builtin
        if func_type is AIFPLSymbol and cast(AIFPLSymbol, func_expr).name in self.builtin_indices:
            builtin_name = cast(AIFPLSymbol, func_expr).name
            builtin_index = self.builtin_indices[builtin_name]

            # Analyze arguments
            arg_plans = [self._analyze_expression(arg, ctx, in_tail_position=False) for arg in arg_exprs]

            return CallPlan(
                func_plan=VariablePlan(name=builtin_name, var_type='global', depth=0, index=0),
                arg_plans=arg_plans,
                is_tail_call=False,  # Builtins are never tail-called
                is_tail_recursive=False,
                is_builtin=True,
                builtin_index=builtin_index
            )

        # Check for tail-recursive call
        is_tail_recursive = (
            in_tail_position and
            ctx.current_function_name is not None and
            func_type is AIFPLSymbol and
            cast(AIFPLSymbol, func_expr).name == ctx.current_function_name
        )

        # Analyze function and arguments
        func_plan: ExprPlan
        if is_tail_recursive:
            func_plan = VariablePlan(name='<tail-recursive>', var_type='local', depth=0, index=0)
        else:
            func_plan = self._analyze_expression(func_expr, ctx, in_tail_position=False)

        arg_plans = [self._analyze_expression(arg, ctx, in_tail_position=False) for arg in arg_exprs]

        return CallPlan(
            func_plan=func_plan,
            arg_plans=arg_plans,
            is_tail_call=in_tail_position,
            is_tail_recursive=is_tail_recursive,
            is_builtin=False,
            builtin_index=None
        )

    def _find_free_variables(self, expr: AIFPLValue, bound_vars: Set[str], parent_ctx: AnalysisContext) -> List[str]:
        """
        Find free variables in an expression.

        Free variables are those that are:
        - Referenced in the expression
        - Not in bound_vars (parameters)
        - Not globals
        - Defined in parent scopes
        """
        free: List[str] = []
        self._collect_free_vars(expr, bound_vars, parent_ctx, free, set())
        return free

    def _collect_free_vars(
        self,
        expr: AIFPLValue,
        bound_vars: Set[str],
        parent_ctx: AnalysisContext,
        free: List[str],
        seen: Set[str]
    ) -> None:
        """Recursively collect free variables."""
        expr_type = type(expr)

        if expr_type is AIFPLSymbol:
            name = cast(AIFPLSymbol, expr).name
            if name in seen or name in bound_vars:
                return

            # Check if it's defined in parent scopes
            var_type, _, _ = parent_ctx.resolve_variable(name)
            if var_type == 'local' and name not in seen:
                free.append(name)
                seen.add(name)

        elif expr_type is AIFPLList:
            if cast(AIFPLList, expr).is_empty():
                return

            first = cast(AIFPLList, expr).first()
            first_type = type(first)

            # Handle special forms that bind variables
            if first_type is AIFPLSymbol:
                if cast(AIFPLSymbol, first).name == 'lambda':
                    # Nested lambda: we need to find what free variables it uses
                    # that come from outer scopes, so the parent lambda can capture them.
                    # The nested lambda will then capture them from the parent.
                    if len(cast(AIFPLList, expr).elements) >= 3:
                        nested_params = cast(AIFPLList, expr).elements[1]
                        nested_body = cast(AIFPLList, expr).elements[2]

                        # Get parameter names from nested lambda
                        nested_bound = bound_vars.copy()
                        if isinstance(nested_params, AIFPLList):
                            for param in nested_params.elements:
                                if isinstance(param, AIFPLSymbol):
                                    nested_bound.add(param.name)

                        # Find free variables in nested lambda's body
                        # These are variables the nested lambda needs, which might come
                        # from the parent lambda or from even outer scopes
                        self._collect_free_vars(nested_body, nested_bound, parent_ctx, free, seen)

                    # Don't recurse into the lambda's parameters or other parts
                    return

                if cast(AIFPLSymbol, first).name == 'let':
                    # Let bindings create new bound variables
                    # Extract binding names and recurse with updated bound_vars
                    if len(cast(AIFPLList, expr).elements) >= 3:
                        bindings_list = cast(AIFPLList, expr).elements[1]
                        body = cast(AIFPLList, expr).elements[2]

                        # Collect let binding names
                        new_bound = bound_vars.copy()
                        if isinstance(bindings_list, AIFPLList):
                            for binding in bindings_list.elements:
                                if isinstance(binding, AIFPLList) and len(binding.elements) >= 2:
                                    name_expr = binding.elements[0]
                                    if isinstance(name_expr, AIFPLSymbol):
                                        new_bound.add(name_expr.name)

                        # Recurse into binding values first (to find free vars in lambda definitions)
                        if isinstance(bindings_list, AIFPLList):
                            for binding in bindings_list.elements:
                                if isinstance(binding, AIFPLList) and len(binding.elements) >= 2:
                                    value_expr = binding.elements[1]
                                    # Use original bound_vars, not new_bound, because bindings can't reference each other yet
                                    self._collect_free_vars(value_expr, bound_vars, parent_ctx, free, seen)

                        # Recurse into let body with new bound variables
                        self._collect_free_vars(body, new_bound, parent_ctx, free, seen)
                    return

            # Recursively check all elements
            for elem in cast(AIFPLList, expr).elements:
                self._collect_free_vars(elem, bound_vars, parent_ctx, free, seen)
