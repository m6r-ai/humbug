"""AIFPL Analyzer - Pass 1 of two-pass compiler.

The analyzer traverses the AST and produces analyzed IR (Intermediate Representation)
with all information needed for code generation:
- Variable resolution (depths and indices)
- Jump offset calculation
- Closure analysis (free variables)
- Recursion detection
- Tail call identification

The analyzer does NOT emit bytecode - it only analyzes and annotates the AST.
"""

from typing import List, Dict, Set, Optional

from aifpl.aifpl_analysis_ir import (
    AnalyzedExpression,
    AnalyzedLiteral,
    AnalyzedVariable,
    AnalyzedIf,
    AnalyzedLet,
    AnalyzedLambda,
    AnalyzedCall,
    AnalyzedAnd,
    AnalyzedOr,
    AnalyzedMatch,
    AnalyzedMatchClause,
    AnalyzedLiteralPattern,
    AnalyzedVariablePattern,
    AnalyzedWildcardPattern,
    AnalyzedTypePattern,
    AnalyzedEmptyListPattern,
    AnalyzedFixedListPattern,
    AnalyzedConsPattern,
    AnalyzedQuote,
    AnalyzedMakeList,
)
from aifpl.aifpl_symbol_table import SymbolTable, SymbolInfo
from aifpl.aifpl_value import (
    AIFPLValue,
    AIFPLNumber,
    AIFPLString,
    AIFPLBoolean,
    AIFPLSymbol,
    AIFPLList,
)
from aifpl.aifpl_error import AIFPLEvalError
from aifpl.aifpl_compiler import AIFPLCompiler
from aifpl.aifpl_dependency_analyzer import AIFPLDependencyAnalyzer


class AIFPLAnalyzer:
    """Pass 1: Analyzes AST and produces analyzed IR.
    
    The analyzer:
    1. Builds a symbol table tracking all variables
    2. Resolves variable references to (depth, index) pairs
    3. Calculates jump offsets for control flow
    4. Identifies free variables for closures
    5. Detects recursive and tail-recursive calls
    
    The output is analyzed IR that Pass 2 (generator) can directly
    translate to bytecode without any patching or fixups.
    """
    
    def __init__(self, globals_dict: Optional[Dict[str, AIFPLValue]] = None):
        """Initialize analyzer.
        
        Args:
            globals_dict: Dictionary of global variables (constants like pi, e, j)
        """
        self.symbol_table = SymbolTable()
        
        # Add globals to symbol table
        if globals_dict:
            for name in globals_dict.keys():
                self.symbol_table.add_symbol(name, "global")
        
        # Add builtins to symbol table
        for builtin_name in AIFPLCompiler.BUILTIN_TABLE:
            self.symbol_table.add_symbol(builtin_name, "builtin")
        
        # Constant and name pools (shared with generator)
        self.constants: List[AIFPLValue] = []
        self.constant_map: Dict[AIFPLValue, int] = {}
        self.names: List[str] = []
        self.name_map: Dict[str, int] = {}
        
        # Code objects for nested lambdas
        self.code_objects: List[AnalyzedLambda] = []
        
        # Track current function name for tail recursion detection
        self.current_function_name: Optional[str] = None
    
    def analyze(self, expr: AIFPLValue, name: str = "<module>") -> AnalyzedExpression:
        """Analyze an AIFPL expression.
        
        Args:
            expr: AST to analyze
            name: Name for the top-level code object
            
        Returns:
            Analyzed IR
        """
        # Enter a new scope for module-level code
        # This ensures module locals start at index 0, not after builtins/globals
        self.symbol_table.enter_scope()
        
        # Module-level code is not in tail position  
        return self._analyze_expression(expr, in_tail_position=False)
    
    def _analyze_expression(self, expr: AIFPLValue, in_tail_position: bool) -> AnalyzedExpression:
        """Main analysis dispatch.
        
        Args:
            expr: AST node to analyze
            in_tail_position: Whether this expression is in tail position
            
        Returns:
            Analyzed IR node
        """
        # Cache the type for faster dispatch
        expr_type = type(expr)
        
        # Self-evaluating values
        if expr_type is AIFPLNumber:
            return self._analyze_literal(expr)
        
        if expr_type is AIFPLString:
            return self._analyze_literal(expr)
        
        if expr_type is AIFPLBoolean:
            return self._analyze_literal(expr)
        
        # Symbol (variable reference)
        if expr_type is AIFPLSymbol:
            return self._analyze_variable(expr)
        
        # List (function call or special form)
        if expr_type is AIFPLList:
            expr_list = expr
            if expr_list.is_empty():
                # Empty list literal
                return self._analyze_empty_list()
            
            return self._analyze_list(expr_list, in_tail_position)
        
        raise AIFPLEvalError(
            message=f"Cannot analyze expression of type {type(expr).__name__}",
            received=str(expr)
        )
    
    def _analyze_literal(self, expr: AIFPLValue) -> AnalyzedLiteral:
        """Analyze a literal value (number, string, boolean).
        
        Args:
            expr: Literal value
            
        Returns:
            AnalyzedLiteral with constant pool index
        """
        const_index = self._add_constant(expr)
        
        return AnalyzedLiteral(
            expr_type='literal',
            source_expr=expr,
            value=expr,
            const_index=const_index,
            instruction_count=1  # One LOAD_CONST (or LOAD_TRUE/FALSE for booleans)
        )
    
    def _analyze_variable(self, symbol: AIFPLSymbol) -> AnalyzedVariable:
        """Analyze a variable reference.
        
        Args:
            symbol: Symbol to resolve
            
        Returns:
            AnalyzedVariable with resolution information
        """
        name = symbol.name
        
        # Try to resolve in symbol table
        sym_info = self.symbol_table.resolve(name)
        
        if sym_info:
            # Found in symbol table
            if sym_info.symbol_type == 'local' or sym_info.symbol_type == 'parameter':
                # Local variable - calculate offset
                try:
                    depth, index = self.symbol_table.calculate_variable_offset(name)
                    return AnalyzedVariable(
                        expr_type='variable',
                        source_expr=symbol,
                        name=name,
                        var_type='local',
                        depth=depth,
                        index=index,
                        instruction_count=1  # LOAD_VAR
                    )
                except ValueError:
                    # Shouldn't happen, but fall through to global
                    pass
            
            # Global or builtin
            name_index = self._add_name(name)
            return AnalyzedVariable(
                expr_type='variable',
                source_expr=symbol,
                name=name,
                var_type=sym_info.symbol_type,
                depth=0,
                index=name_index,
                instruction_count=1  # LOAD_NAME
            )
        
        # Not found in symbol table - assume it's a global that will be
        # resolved at runtime (this allows forward references)
        name_index = self._add_name(name)
        return AnalyzedVariable(
            expr_type='variable',
            source_expr=symbol,
            name=name,
            var_type='global',
            depth=0,
            index=name_index,
            instruction_count=1  # LOAD_NAME
        )
    
    def _analyze_empty_list(self) -> AnalyzedLiteral:
        """Analyze empty list literal.
        
        Returns:
            AnalyzedLiteral representing empty list
        """
        empty_list = AIFPLList(())
        const_index = self._add_constant(empty_list)
        
        return AnalyzedLiteral(
            expr_type='literal',
            source_expr=empty_list,
            value=empty_list,
            const_index=const_index,
            instruction_count=1  # LOAD_EMPTY_LIST
        )
    
    def _analyze_list(self, expr: AIFPLList, in_tail_position: bool) -> AnalyzedExpression:
        """Analyze a list expression (function call or special form).
        
        Args:
            expr: List expression
            in_tail_position: Whether in tail position
            
        Returns:
            Analyzed expression (type depends on what the list represents)
        """
        first = expr.first()
        
        # Check for special forms
        if isinstance(first, AIFPLSymbol):
            name = first.name
            
            # Special forms
            if name == 'if':
                return self._analyze_if(expr, in_tail_position)
            
            if name == 'let':
                return self._analyze_let(expr, in_tail_position)
            
            if name == 'lambda':
                return self._analyze_lambda(expr, in_tail_position)
            
            if name == 'quote':
                return self._analyze_quote(expr)
            
            if name == 'and':
                return self._analyze_and(expr)
            
            if name == 'or':
                return self._analyze_or(expr)
            
            if name == 'match':
                return self._analyze_match(expr, in_tail_position)
            
            # Special handling for list construction
            if name == 'list':
                return self._analyze_make_list(expr)
        
        # Regular function call
        return self._analyze_call(expr, in_tail_position)
    
    def _analyze_if(self, expr: AIFPLList, in_tail_position: bool) -> AnalyzedIf:
        """Analyze if expression.
        
        Analyzes expressions like:
        - (if condition then-expr else-expr)
        
        Calculates jump offsets so Pass 2 can emit correct bytecode without patching.
        
        Args:
            expr: If expression
            in_tail_position: Whether this if is in tail position
            
        Returns:
            AnalyzedIf with pre-calculated jump offsets
        """
        if len(expr.elements) != 4:
            raise AIFPLEvalError(
                message="If expression has wrong number of arguments",
                received=f"Got {len(expr.elements) - 1} arguments",
                expected="Exactly 3 arguments: (if condition then else)",
                example="(if (> x 0) \"positive\" \"negative\")",
                suggestion="If needs condition, then-branch, and else-branch"
            )
        
        _, condition_expr, then_expr, else_expr = expr.elements
        
        # Analyze condition (not in tail position)
        analyzed_condition = self._analyze_expression(condition_expr, in_tail_position=False)
        
        # Analyze branches (both inherit tail position from if)
        analyzed_then = self._analyze_expression(then_expr, in_tail_position=in_tail_position)
        analyzed_else = self._analyze_expression(else_expr, in_tail_position=in_tail_position)
        
        # Calculate jump offsets
        # Instruction layout:
        # 0: [condition instructions]
        # N: POP_JUMP_IF_FALSE <else_offset>
        # N+1: [then instructions]
        # M: JUMP <end_offset> (only if not in tail position)
        # M+1: [else instructions]
        # End
        
        condition_size = analyzed_condition.instruction_count
        then_size = analyzed_then.instruction_count
        else_size = analyzed_else.instruction_count
        
        # Jump to else: skip condition + POP_JUMP_IF_FALSE + then + JUMP (if not tail)
        jump_to_else_offset = condition_size + 1 + then_size
        if not in_tail_position:
            jump_to_else_offset += 1  # +1 for JUMP instruction
        
        # Jump past else: from start to after else
        jump_past_else_offset = jump_to_else_offset + else_size
        
        # Total instruction count
        total_instructions = condition_size + 1  # condition + POP_JUMP_IF_FALSE
        total_instructions += then_size
        if not in_tail_position:
            total_instructions += 1  # JUMP past else
        total_instructions += else_size
        
        return AnalyzedIf(
            expr_type='if',
            source_expr=expr,
            condition=analyzed_condition,
            then_branch=analyzed_then,
            else_branch=analyzed_else,
            jump_to_else_offset=jump_to_else_offset,
            jump_past_else_offset=jump_past_else_offset,
            instruction_count=total_instructions
        )
    
    def _analyze_let(self, expr: AIFPLList, in_tail_position: bool) -> AnalyzedLet:
        """Analyze let expression.
        
        Analyzes expressions like:
        - (let ((x 5) (y 10)) (+ x y))
        - (let ((fact (lambda (n) ...))) (fact 10))
        
        Uses dependency analysis to identify recursive bindings and determine
        evaluation order.
        
        Args:
            expr: Let expression
            in_tail_position: Whether this let is in tail position
            
        Returns:
            AnalyzedLet with binding analysis and recursion information
        """
        if len(expr.elements) < 3:
            raise AIFPLEvalError(
                message="Let expression structure is incorrect",
                received=f"Got {len(expr.elements)} elements",
                expected="Exactly 3 elements: (let ((bindings...)) body)",
                example="(let ((x 5) (y 10)) (+ x y))",
                suggestion="Let needs binding list and body"
            )
        
        _, bindings_list, body_expr = expr.elements[0], expr.elements[1], expr.elements[2]
        
        if not isinstance(bindings_list, AIFPLList):
            raise AIFPLEvalError(
                message="Let binding list must be a list",
                received=f"Binding list: {bindings_list.type_name()}",
                expected="List of bindings: ((var1 val1) (var2 val2) ...)",
                example="(let ((x 5) (y (* x 2))) (+ x y))",
                suggestion="Wrap bindings in parentheses"
            )
        
        # Extract and validate binding pairs
        binding_pairs = []
        for i, binding in enumerate(bindings_list.elements):
            if not isinstance(binding, AIFPLList):
                raise AIFPLEvalError(
                    message=f"Let binding {i+1} must be a list",
                    received=f"Binding {i+1}: {binding.type_name()}",
                    expected="List with variable and value: (var val)",
                    example='Correct: (x 5)',
                    suggestion="Wrap each binding in parentheses"
                )
            
            if len(binding.elements) != 2:
                raise AIFPLEvalError(
                    message=f"Let binding {i+1} has wrong number of elements",
                    received=f"Binding {i+1}: has {len(binding.elements)} elements",
                    expected="Each binding needs exactly 2 elements: (variable value)",
                    example='Correct: (x 5)',
                    suggestion="Each binding: (variable-name value-expression)"
                )
            
            name_expr, value_expr = binding.elements
            if not isinstance(name_expr, AIFPLSymbol):
                raise AIFPLEvalError(
                    message=f"Let binding {i+1} variable must be a symbol",
                    received=f"Variable: {name_expr.type_name()}",
                    expected="Unquoted symbol (variable name)",
                    example='Correct: (x 5)',
                    suggestion='Use unquoted variable names'
                )
            
            binding_pairs.append((name_expr.name, value_expr))
        
        # Check for duplicate binding names
        var_names = [name for name, _ in binding_pairs]
        if len(var_names) != len(set(var_names)):
            duplicates = [name for name in var_names if var_names.count(name) > 1]
            raise AIFPLEvalError(
                message="Let binding variables must be unique",
                received=f"Duplicate variables: {duplicates}",
                expected="All variable names should be different",
                example='Correct: (let ((x 1) (y 2)) ...)',
                suggestion="Use different names for each variable"
            )
        
        # Analyze dependencies
        analyzer = AIFPLDependencyAnalyzer()
        binding_groups = analyzer.analyze_let_bindings(binding_pairs)
        
        # DON'T enter new scope for let bindings!
        # Let bindings add locals to the current frame, they don't create new frames
        # Only lambdas create new frames
        # self.symbol_table.enter_scope()
        
        # Add all binding names to symbol table first (for recursive references)
        for name, _ in binding_pairs:
            self.symbol_table.add_symbol(name, "local", defined_at=expr)
        
        # Analyze binding values
        analyzed_bindings = []
        recursive_bindings = set()
        sibling_groups = []
        
        for name, value_expr in binding_pairs:
            # Check if this binding is recursive
            is_recursive = self._references_variable(value_expr, name)
            if is_recursive:
                recursive_bindings.add(name)
            
            # Get variable index for this binding
            sym_info = self.symbol_table.resolve(name)
            var_index = sym_info.var_index if sym_info else 0
            
            # Analyze the value expression
            # Binding values are not in tail position
            # Check if this is a lambda - if so, pass the binding name for recursion detection
            if (isinstance(value_expr, AIFPLList) and 
                not value_expr.is_empty() and
                isinstance(value_expr.first(), AIFPLSymbol) and
                value_expr.first().name == 'lambda'):
                # Analyze lambda with binding name
                analyzed_value = self._analyze_lambda(value_expr, in_tail_position=False, binding_name=name)
            else:
                analyzed_value = self._analyze_expression(value_expr, in_tail_position=False)
            
            analyzed_bindings.append((name, analyzed_value, var_index))
        
        # Identify sibling groups (mutually recursive bindings)
        for group in binding_groups:
            if group.is_recursive and len(group.names) > 1:
                sibling_groups.append(group.names)
        
        # Analyze body (inherits tail position)
        analyzed_body = self._analyze_expression(body_expr, in_tail_position=in_tail_position)
        
        # DON'T exit scope since we didn't enter one
        # self.symbol_table.exit_scope()
        
        # Calculate instruction count
        # Each binding: value instructions + STORE_VAR
        instr_count = sum(av.instruction_count + 1 for _, av, _ in analyzed_bindings)
        instr_count += analyzed_body.instruction_count
        
        return AnalyzedLet(
            expr_type='let',
            source_expr=expr,
            bindings=analyzed_bindings,
            body=analyzed_body,
            binding_groups=binding_groups,
            recursive_bindings=recursive_bindings,
            sibling_groups=sibling_groups,
            instruction_count=instr_count
        )
    
    def _references_variable(self, expr: AIFPLValue, var_name: str) -> bool:
        """Check if an expression references a variable.
        
        This is used to detect self-recursive lambdas in let bindings.
        
        Args:
            expr: Expression to check
            var_name: Variable name to look for
            
        Returns:
            True if expression references the variable
        """
        expr_type = type(expr)
        
        if expr_type is AIFPLSymbol:
            return expr.name == var_name
        
        if expr_type is AIFPLList:
            return any(self._references_variable(elem, var_name) for elem in expr.elements)
        
        return False
    
    def _analyze_lambda(self, expr: AIFPLList, in_tail_position: bool,
                       binding_name: Optional[str] = None) -> AnalyzedLambda:
        """Analyze lambda expression.
        
        Analyzes expressions like:
        - (lambda (x) x)
        - (lambda (x y) (+ x y))
        - (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))  ; recursive
        
        Identifies free variables for closure creation and detects recursion.
        
        Args:
            expr: Lambda expression
            in_tail_position: Whether this lambda is in tail position (not used for lambdas)
            binding_name: Name of the binding if this lambda is in a let (for recursion detection)
            
        Returns:
            AnalyzedLambda with closure analysis
        """
        if len(expr.elements) != 3:
            raise AIFPLEvalError(
                message="Lambda expression structure is incorrect",
                received=f"Got {len(expr.elements)} elements",
                expected="Exactly 3 elements: (lambda (params...) body)",
                example="(lambda (x y) (+ x y))",
                suggestion="Lambda needs parameter list and body"
            )
        
        _, params_list, body_expr = expr.elements
        
        if not isinstance(params_list, AIFPLList):
            raise AIFPLEvalError(
                message="Lambda parameters must be a list",
                received=f"Parameter list: {params_list.type_name()}",
                expected="List of symbols: (param1 param2 ...)",
                example="(lambda (x y z) (+ x y z))",
                suggestion="Parameters should be unquoted variable names"
            )
        
        # Extract and validate parameters
        param_names = []
        for i, param in enumerate(params_list.elements):
            if not isinstance(param, AIFPLSymbol):
                raise AIFPLEvalError(
                    message=f"Lambda parameter {i+1} must be a symbol",
                    received=f"Parameter {i+1}: {param.type_name()}",
                    expected="Unquoted symbol (variable name)",
                    example='Correct: (lambda (x y) (+ x y))',
                    suggestion='Use unquoted names: x, not "x" or 1'
                )
            param_names.append(param.name)
        
        # Check for duplicate parameters
        if len(param_names) != len(set(param_names)):
            duplicates = [p for p in param_names if param_names.count(p) > 1]
            raise AIFPLEvalError(
                message="Lambda parameters must be unique",
                received=f"Duplicate parameters: {duplicates}",
                expected="All parameter names should be different",
                example='Correct: (lambda (x y z) ...)',
                suggestion="Use different names for each parameter"
            )
        
        # Enter new scope for lambda
        self.symbol_table.enter_scope()
        
        # Add parameters to scope
        for param_name in param_names:
            self.symbol_table.add_symbol(param_name, "parameter", defined_at=expr)
        
        # Save and set current function name for tail recursion detection
        old_function_name = self.current_function_name
        if binding_name:
            self.current_function_name = binding_name
        
        # Analyze body (always in tail position within lambda)
        analyzed_body = self._analyze_expression(body_expr, in_tail_position=True)
        
        # Restore function name
        self.current_function_name = old_function_name
        
        # Find free variables (variables used in body but not parameters)
        free_vars = self._find_free_variables_in_analyzed(analyzed_body, set(param_names))
        
        # Exit lambda scope
        self.symbol_table.exit_scope()
        
        # Check for self-recursion
        is_recursive = binding_name is not None and binding_name in free_vars
        
        # Identify recursive siblings (for mutual recursion)
        # These would be other bindings in the same let that this lambda references
        recursive_siblings = []
        if binding_name:
            # Look for references to other variables in the same let scope
            # We'll identify these by checking if they're in free_vars
            # This is a simplified approach - full mutual recursion detection
            # would require tracking let binding groups
            pass
        
        # Add to code objects list
        code_index = len(self.code_objects)
        
        # Calculate instruction count
        # Lambda creation is just MAKE_CLOSURE (1 instruction)
        # The body instructions are in the lambda's code object, not inline
        instr_count = 1
        
        result = AnalyzedLambda(
            expr_type='lambda',
            source_expr=expr,
            params=param_names,
            body=analyzed_body,
            free_vars=list(free_vars),
            is_recursive=is_recursive,
            recursive_siblings=recursive_siblings,
            code_index=code_index,
            instruction_count=instr_count
        )
        
        # Add to code objects
        self.code_objects.append(result)
        
        return result
    
    def _find_free_variables_in_analyzed(self, analyzed: AnalyzedExpression, 
                                        bound: Set[str]) -> Set[str]:
        """Find free variables in an analyzed expression.
        
        A free variable is one that:
        - Is referenced in the expression
        - Is not in the bound set (parameters)
        - Is a local variable (not global or builtin)
        
        Args:
            analyzed: Analyzed expression to search
            bound: Set of bound variable names (parameters)
            
        Returns:
            Set of free variable names
        """
        free = set()
        
        if isinstance(analyzed, AnalyzedVariable):
            if analyzed.var_type == 'local' and analyzed.name not in bound:
                free.add(analyzed.name)
        
        elif isinstance(analyzed, AnalyzedIf):
            free.update(self._find_free_variables_in_analyzed(analyzed.condition, bound))
            free.update(self._find_free_variables_in_analyzed(analyzed.then_branch, bound))
            free.update(self._find_free_variables_in_analyzed(analyzed.else_branch, bound))
        
        elif isinstance(analyzed, AnalyzedLet):
            # Binding values can reference outer scope
            for _, value_analyzed, _ in analyzed.bindings:
                free.update(self._find_free_variables_in_analyzed(value_analyzed, bound))
            
            # Body has bindings in scope
            new_bound = bound | {name for name, _, _ in analyzed.bindings}
            free.update(self._find_free_variables_in_analyzed(analyzed.body, new_bound))
        
        elif isinstance(analyzed, AnalyzedLambda):
            # Nested lambda parameters are bound
            new_bound = bound | set(analyzed.params)
            free.update(self._find_free_variables_in_analyzed(analyzed.body, new_bound))
        
        elif isinstance(analyzed, AnalyzedCall):
            free.update(self._find_free_variables_in_analyzed(analyzed.func, bound))
            for arg in analyzed.args:
                free.update(self._find_free_variables_in_analyzed(arg, bound))
        
        elif isinstance(analyzed, AnalyzedMakeList):
            for elem in analyzed.elements:
                free.update(self._find_free_variables_in_analyzed(elem, bound))
        
        # Other types (literals, quote, etc.) have no free variables
        
        return free
    
    def _analyze_quote(self, expr: AIFPLList) -> AnalyzedQuote:
        """Analyze quote expression.
        
        Quote simply returns the quoted value as a constant.
        """
        if len(expr.elements) != 2:
            raise AIFPLEvalError(
                message="Quote expression has wrong number of arguments",
                received=f"Got {len(expr.elements) - 1} arguments",
                expected="Exactly 1 argument",
                example="(quote expr) or 'expr"
            )
        
        quoted = expr.elements[1]
        const_index = self._add_constant(quoted)
        
        return AnalyzedQuote(
            expr_type='quote',
            source_expr=expr,
            quoted_value=quoted,
            const_index=const_index,
            instruction_count=1  # LOAD_CONST
        )
    
    def _analyze_and(self, expr: AIFPLList) -> AnalyzedAnd:
        """Analyze 'and' expression with short-circuit evaluation.
        
        (and) -> #t  
        (and a b c) -> if any is false, return #f; else return #t
        
        Bytecode structure:
        - Evaluate arg1
        - POP_JUMP_IF_FALSE <false_section>
        - Evaluate arg2
        - POP_JUMP_IF_FALSE <false_section>
        - ...
        - LOAD_TRUE
        - JUMP <end>
        - false_section: LOAD_FALSE
        - end:
        
        Args:
            expr: And expression
            
        Returns:
            AnalyzedAnd with analyzed arguments
        """
        args = list(expr.elements[1:])  # Skip 'and' symbol
        
        # Analyze all arguments
        analyzed_args = [self._analyze_expression(arg, False) for arg in args]
        
        # Instruction count (approximate, for reference)
        # Each arg + POP_JUMP + final LOAD_TRUE/FALSE + JUMP
        total_instructions = sum(a.instruction_count for a in analyzed_args) + len(analyzed_args) + 3
        
        return AnalyzedAnd(
            expr_type='and',
            source_expr=expr,
            args=analyzed_args,
            instruction_count=total_instructions
        )
    
    def _analyze_or(self, expr: AIFPLList) -> AnalyzedOr:
        """Analyze 'or' expression with short-circuit evaluation.
        
        (or) -> #f  
        (or a b c) -> if any is true, return #t; else return #f
        
        Bytecode structure:
        - Evaluate arg1
        - POP_JUMP_IF_TRUE <true_section>
        - Evaluate arg2
        - POP_JUMP_IF_TRUE <true_section>
        - ...
        - LOAD_FALSE
        - JUMP <end>
        - true_section: LOAD_TRUE
        - end:
        
        Args:
            expr: Or expression
            
        Returns:
            AnalyzedOr with analyzed arguments
        """
        args = list(expr.elements[1:])  # Skip 'or' symbol
        
        # Analyze all arguments
        analyzed_args = [self._analyze_expression(arg, False) for arg in args]
        
        # Instruction count (approximate, for reference)
        # Each arg + POP_JUMP + final LOAD_TRUE/FALSE + JUMP
        total_instructions = sum(a.instruction_count for a in analyzed_args) + len(analyzed_args) + 3
        
        return AnalyzedOr(
            expr_type='or',
            source_expr=expr,
            args=analyzed_args,
            instruction_count=total_instructions
        )
    
    def _analyze_match(self, expr: AIFPLList, in_tail_position: bool) -> AnalyzedMatch:
        """Analyze match expression.
        
        Analyzes expressions like:
        - (match x ((42) "found") (_ "default"))
        - (match lst ((number? n) n) ((string? s) s))
        
        Args:
            expr: Match expression
            in_tail_position: Whether this match is in tail position
            
        Returns:
            AnalyzedMatch with pattern analysis and temp variable allocation
        """
        if len(expr.elements) < 3:
            raise AIFPLEvalError(
                message="Match expression has wrong number of arguments",
                received=f"Got {len(expr.elements) - 1} arguments",
                expected="At least 2 arguments: (match value (pattern1 result1) ...)",
                example="(match x ((42) \"found\") (_ \"default\"))"
            )
        
        value_expr = expr.elements[1]
        clause_exprs = expr.elements[2:]
        
        # Validate all clauses upfront
        for i, clause in enumerate(clause_exprs):
            if not isinstance(clause, AIFPLList) or len(clause.elements) != 2:
                raise AIFPLEvalError(
                    message=f"Match clause {i+1} has wrong number of elements",
                    received=f"Clause {i+1}: {clause}",
                    expected="Each clause needs exactly 2 elements: (pattern result)",
                    example="(match x ((42) \"found\") ((string? s) s))",
                    suggestion="Each clause: (pattern result-expression)"
                )
        
        # Validate pattern syntax for all clauses
        for i, clause in enumerate(clause_exprs):
            pattern = clause.elements[0]
            try:
                self._validate_pattern_syntax(pattern)
            except AIFPLEvalError as e:
                raise AIFPLEvalError(
                    message=f"Invalid pattern in clause {i+1}",
                    context=str(e)
                ) from e
        
        # Analyze the value expression
        analyzed_value = self._analyze_expression(value_expr, in_tail_position=False)
        
        # Save current bindings state
        saved_bindings_count = len(self.symbol_table.current_scope.symbols)
        saved_next_index = self.symbol_table.current_scope.next_var_index
        
        # Allocate temporary for match value
        match_temp_index = self.symbol_table.add_symbol("<match-temp>", "local", defined_at=expr).var_index
        
        # Analyze each clause
        analyzed_clauses = []
        max_locals_used = saved_next_index + 1  # At least match-temp
        for i, clause in enumerate(clause_exprs):
            pattern_expr = clause.elements[0]
            result_expr = clause.elements[1]
            
            # Analyze pattern (will allocate temps and bind pattern variables)
            analyzed_pattern = self._analyze_pattern(pattern_expr, match_temp_index)
            
            # Analyze result expression (pattern variables are in scope)
            analyzed_result = self._analyze_expression(result_expr, in_tail_position=in_tail_position)
            
            # Create clause
            analyzed_clauses.append(AnalyzedMatchClause(
                pattern=analyzed_pattern,
                result=analyzed_result
            ))
            
            # Track max locals used
            current_locals = self.symbol_table.current_scope.next_var_index
            max_locals_used = max(max_locals_used, current_locals)
            
            # Reset bindings for next clause (keep match-temp)
            # Remove pattern variables and their temps
            self.symbol_table.current_scope.symbols = dict(
                list(self.symbol_table.current_scope.symbols.items())[:saved_bindings_count + 1]
            )
            self.symbol_table.current_scope.next_var_index = saved_next_index + 1
        
        # Calculate instruction count (approximate)
        # This is complex, so we'll use a simple estimate
        instr_count = analyzed_value.instruction_count  # Value
        instr_count += 1  # STORE_VAR for match temp
        for clause in analyzed_clauses:
            instr_count += 10  # Pattern matching (approximate)
            instr_count += clause.result.instruction_count
            instr_count += 2  # Jumps
        
        return AnalyzedMatch(
            expr_type='match',
            source_expr=expr,
            value=analyzed_value,
            match_temp_index=match_temp_index,
            saved_bindings_count=saved_bindings_count,
            saved_next_index=saved_next_index,
            max_locals_used=max_locals_used,
            clauses=analyzed_clauses,
            instruction_count=instr_count
        )
    
    def _validate_pattern_syntax(self, pattern: AIFPLValue) -> None:
        """Validate pattern syntax before analysis.
        
        This is a simple validation - detailed checking happens during analysis.
        For now, we'll accept any pattern and let analysis catch errors.
        """
        # TODO: Add comprehensive pattern validation
        # For now, just check basic structure
        pass
    
    def _analyze_pattern(self, pattern: AIFPLValue, match_value_index: int):
        """Analyze a pattern recursively.
        
        Args:
            pattern: Pattern to analyze
            match_value_index: Index of temp var holding value to match
            
        Returns:
            AnalyzedPattern (specific subclass based on pattern type)
        """
        # Literal patterns: numbers, strings, booleans
        if isinstance(pattern, (AIFPLNumber, AIFPLString, AIFPLBoolean)):
            const_index = self._add_constant(pattern)
            return AnalyzedLiteralPattern(
                pattern_type='literal',
                source_expr=pattern,
                value=pattern,
                const_index=const_index,
                match_value_index=match_value_index,
                instruction_count=3  # LOAD_VAR + LOAD_CONST + CALL_BUILTIN(=)
            )
        
        # Variable pattern: binds the value
        if isinstance(pattern, AIFPLSymbol):
            if pattern.name == '_':
                # Wildcard - always matches, no binding
                return AnalyzedWildcardPattern(
                    pattern_type='wildcard',
                    source_expr=pattern,
                    instruction_count=1  # LOAD_TRUE
                )
            
            # Bind variable
            var_index = self.symbol_table.add_symbol(pattern.name, "local", defined_at=pattern).var_index
            return AnalyzedVariablePattern(
                pattern_type='variable',
                source_expr=pattern,
                name=pattern.name,
                var_index=var_index,
                match_value_index=match_value_index,
                instruction_count=3  # LOAD_VAR + STORE_VAR + LOAD_TRUE
            )
        
        # List patterns
        if isinstance(pattern, AIFPLList):
            return self._analyze_list_pattern(pattern, match_value_index)
        
        raise AIFPLEvalError(f"Unknown pattern type: {type(pattern).__name__}")
    
    def _analyze_list_pattern(self, pattern: AIFPLList, match_value_index: int):
        """Analyze a list pattern.
        
        Args:
            pattern: List pattern to analyze
            match_value_index: Index of temp var holding value to match
            
        Returns:
            AnalyzedPattern (specific subclass based on list pattern type)
        """
        # Empty list pattern
        if pattern.is_empty():
            return AnalyzedEmptyListPattern(
                pattern_type='empty_list',
                source_expr=pattern,
                match_value_index=match_value_index,
                instruction_count=2  # LOAD_VAR + CALL_BUILTIN(null?)
            )
        
        # Type pattern: (type? var)
        if (len(pattern.elements) == 2 and
            isinstance(pattern.elements[0], AIFPLSymbol) and
            pattern.elements[0].name.endswith('?')):
            
            type_pred = pattern.elements[0].name
            var_pattern = pattern.elements[1]
            
            # Check if it's a valid type predicate
            valid_types = {'number?', 'integer?', 'float?', 'complex?',
                          'string?', 'boolean?', 'list?', 'alist?', 'function?'}
            
            if type_pred in valid_types:
                # Get variable name and allocate binding
                var_name = var_pattern.name if isinstance(var_pattern, AIFPLSymbol) else '_'
                var_index = -1
                
                if var_name != '_':
                    var_index = self.symbol_table.add_symbol(var_name, "local", defined_at=pattern).var_index
                
                return AnalyzedTypePattern(
                    pattern_type='type',
                    source_expr=pattern,
                    type_predicate=type_pred,
                    var_name=var_name,
                    var_index=var_index,
                    match_value_index=match_value_index,
                    instruction_count=8  # Approximate: type check + bind + jumps
                )
        
        # Check for cons pattern (head . tail)
        dot_position = None
        for i, elem in enumerate(pattern.elements):
            if isinstance(elem, AIFPLSymbol) and elem.name == '.':
                dot_position = i
                break
        
        if dot_position is not None:
            return self._analyze_cons_pattern(pattern, match_value_index, dot_position)
        
        # Fixed-length list pattern: (p1 p2 p3)
        return self._analyze_fixed_list_pattern(pattern, match_value_index)
    
    def _analyze_fixed_list_pattern(self, pattern: AIFPLList, match_value_index: int):
        """Analyze a fixed-length list pattern like (a b c).
        
        Args:
            pattern: List pattern
            match_value_index: Index of temp var holding value to match
            
        Returns:
            AnalyzedFixedListPattern
        """
        # Allocate temps for each element and recursively analyze sub-patterns
        element_patterns = []
        element_temp_indices = []
        
        for i, elem_pattern in enumerate(pattern.elements):
            # Allocate temp for this element
            elem_temp_index = self.symbol_table.add_symbol(f"<elem-temp-{i}>", "local", defined_at=pattern).var_index
            element_temp_indices.append(elem_temp_index)
            
            # Recursively analyze the element pattern
            analyzed_elem = self._analyze_pattern(elem_pattern, elem_temp_index)
            element_patterns.append(analyzed_elem)
        
        # Calculate instruction count
        instr_count = 2  # LOAD_VAR + CALL_BUILTIN(list?)
        instr_count += 4  # Length check
        for elem in element_patterns:
            instr_count += 4  # Extract element
            instr_count += elem.instruction_count  # Match element
            instr_count += 1  # POP_JUMP_IF_FALSE
        instr_count += 3  # Success/failure paths
        
        return AnalyzedFixedListPattern(
            pattern_type='fixed_list',
            source_expr=pattern,
            element_patterns=element_patterns,
            element_temp_indices=element_temp_indices,
            match_value_index=match_value_index,
            instruction_count=instr_count
        )
    
    def _analyze_cons_pattern(self, pattern: AIFPLList, match_value_index: int, dot_position: int):
        """Analyze a cons pattern like (head . tail) or (a b . rest).
        
        Args:
            pattern: Cons pattern
            match_value_index: Index of temp var holding value to match
            dot_position: Position of the dot in the pattern
            
        Returns:
            AnalyzedConsPattern
        """
        # Analyze head patterns (before dot)
        head_patterns = []
        head_temp_indices = []
        
        for i in range(dot_position):
            elem_pattern = pattern.elements[i]
            elem_temp_index = self.symbol_table.add_symbol(f"<cons-elem-temp-{i}>", "local", defined_at=pattern).var_index
            head_temp_indices.append(elem_temp_index)
            
            analyzed_elem = self._analyze_pattern(elem_pattern, elem_temp_index)
            head_patterns.append(analyzed_elem)
        
        # Analyze tail pattern (after dot)
        tail_pattern_expr = pattern.elements[dot_position + 1]
        tail_temp_index = self.symbol_table.add_symbol("<cons-tail-temp>", "local", defined_at=pattern).var_index
        analyzed_tail = self._analyze_pattern(tail_pattern_expr, tail_temp_index)
        
        # Calculate instruction count (approximate)
        instr_count = 10  # Type checks, length check
        for head in head_patterns:
            instr_count += 4 + head.instruction_count
        instr_count += 4 + analyzed_tail.instruction_count
        
        return AnalyzedConsPattern(
            pattern_type='cons',
            source_expr=pattern,
            head_patterns=head_patterns,
            head_temp_indices=head_temp_indices,
            tail_pattern=analyzed_tail,
            tail_temp_index=tail_temp_index,
            match_value_index=match_value_index,
            dot_position=dot_position,
            instruction_count=instr_count
        )
    
    def _analyze_make_list(self, expr: AIFPLList) -> AnalyzedMakeList:
        """Analyze list construction: (list 1 2 3).
        
        Args:
            expr: List construction expression
            
        Returns:
            AnalyzedMakeList
        """
        # Elements are everything after 'list'
        element_exprs = expr.elements[1:]
        
        # Analyze each element
        analyzed_elements = []
        total_instructions = 0
        
        for elem_expr in element_exprs:
            analyzed_elem = self._analyze_expression(elem_expr, in_tail_position=False)
            analyzed_elements.append(analyzed_elem)
            total_instructions += analyzed_elem.instruction_count
        
        # Add instruction for MAKE_LIST
        total_instructions += 1
        
        return AnalyzedMakeList(
            expr_type='make_list',
            source_expr=expr,
            elements=analyzed_elements,
            instruction_count=total_instructions
        )
    
    def _analyze_call(self, expr: AIFPLList, in_tail_position: bool) -> AnalyzedCall:
        """Analyze function call.
        
        Analyzes expressions like:
        - (+ 1 2)
        - (f x y)
        - ((lambda (x) x) 5)
        
        Args:
            expr: Function call expression
            in_tail_position: Whether this call is in tail position
            
        Returns:
            AnalyzedCall with function, arguments, and call properties
        """
        func_expr = expr.first()
        arg_exprs = list(expr.elements[1:])
        
        # Analyze function expression
        # Function itself is not in tail position (only the call result is)
        analyzed_func = self._analyze_expression(func_expr, in_tail_position=False)
        
        # Analyze arguments
        # Arguments are never in tail position
        analyzed_args = []
        for arg_expr in arg_exprs:
            analyzed_arg = self._analyze_expression(arg_expr, in_tail_position=False)
            analyzed_args.append(analyzed_arg)
        
        # Check if this is a builtin call
        is_builtin = False
        builtin_index = -1
        
        if isinstance(func_expr, AIFPLSymbol):
            func_name = func_expr.name
            if func_name in AIFPLCompiler.BUILTIN_TABLE:
                is_builtin = True
                builtin_index = AIFPLCompiler.BUILTIN_TABLE.index(func_name)
        
        # Check if this is a tail call
        # A call is a tail call if:
        # 1. It's in tail position
        # 2. It's calling the current function (tail recursion)
        is_tail_call = False
        if in_tail_position and self.current_function_name is not None:
            if isinstance(func_expr, AIFPLSymbol):
                if func_expr.name == self.current_function_name:
                    is_tail_call = True
        
        # Calculate instruction count
        # Function + arguments + CALL instruction
        # For builtins, we don't load the function, we just call it directly
        if is_builtin:
            instr_count = 0  # Don't count loading the builtin
        else:
            instr_count = analyzed_func.instruction_count
        
        for arg in analyzed_args:
            instr_count += arg.instruction_count
        instr_count += 1  # CALL_FUNCTION or CALL_BUILTIN
        
        return AnalyzedCall(
            expr_type='call',
            source_expr=expr,
            func=analyzed_func,
            args=analyzed_args,
            is_tail_call=is_tail_call,
            is_builtin=is_builtin,
            builtin_index=builtin_index,
            instruction_count=instr_count
        )
    
    def _is_builtin(self, name: str) -> bool:
        """Check if a name is a builtin function.
        
        Args:
            name: Function name
            
        Returns:
            True if builtin, False otherwise
        """
        return name in AIFPLCompiler.BUILTIN_TABLE
    
    def _get_builtin_index(self, name: str) -> int:
        """Get the index of a builtin function.
        
        Args:
            name: Builtin function name
            
        Returns:
            Index in BUILTIN_TABLE
            
        Raises:
            ValueError: If name is not a builtin
        """
        return AIFPLCompiler.BUILTIN_TABLE.index(name)
    
    # Helper methods
    
    def _add_constant(self, value: AIFPLValue) -> int:
        """Add constant to pool and return its index.
        
        Args:
            value: Constant value
            
        Returns:
            Index in constant pool
        """
        if value in self.constant_map:
            return self.constant_map[value]
        
        index = len(self.constants)
        self.constants.append(value)
        self.constant_map[value] = index
        return index
    
    def _add_name(self, name: str) -> int:
        """Add name to pool and return its index.
        
        Args:
            name: Variable/function name
            
        Returns:
            Index in name pool
        """
        if name in self.name_map:
            return self.name_map[name]
        
        index = len(self.names)
        self.names.append(name)
        self.name_map[name] = index
        return index
