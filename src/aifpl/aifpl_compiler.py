"""AIFPL bytecode compiler - compiles AST to bytecode."""

from typing import List, Dict, Tuple, Set
from dataclasses import dataclass, field

from aifpl.aifpl_bytecode import CodeObject, Instruction, Opcode, make_instruction
from aifpl.aifpl_dependency_analyzer import AIFPLDependencyAnalyzer
from aifpl.aifpl_error import AIFPLEvalError
from aifpl.aifpl_value import (
    AIFPLValue, AIFPLNumber, AIFPLString, AIFPLBoolean,
    AIFPLSymbol, AIFPLList, AIFPLAList, AIFPLFunction, AIFPLBuiltinFunction
)


@dataclass
class CompilationScope:
    """Tracks variable bindings in a lexical scope.

    Maps variable names to their index within the scope.
    """
    bindings: Dict[str, int] = field(default_factory=dict)
    next_index: int = 0  # Track next available index

    def add_binding(self, name: str) -> int:
        """Add a binding and return its index.

        Note: If a binding with the same name already exists, it will be
        overwritten with a new index. This is intentional to support shadowing.
        """
        index = self.next_index
        self.bindings[name] = index
        self.next_index += 1
        return index

    def get_binding(self, name: str) -> int | None:
        """Get binding index, or None if not found."""
        return self.bindings.get(name)


@dataclass
class CompilationContext:
    """Compilation context tracking scopes and code generation.

    Maintains a stack of scopes for lexical addressing.
    """
    scopes: List[CompilationScope] = field(default_factory=lambda: [CompilationScope()])
    constants: List[AIFPLValue] = field(default_factory=list)
    names: List[str] = field(default_factory=list)
    code_objects: List[CodeObject] = field(default_factory=list)
    instructions: List[Instruction] = field(default_factory=list)
    max_locals: int = 0  # Track maximum locals needed
    parent_ctx: 'CompilationContext | None' = None  # Parent context for nested lambdas
    sibling_bindings: List[str] = field(default_factory=list)  # Sibling bindings for mutual recursion

    def push_scope(self) -> None:
        """Enter a new lexical scope."""
        self.scopes.append(CompilationScope())
        # Update max locals
        total_locals = sum(len(scope.bindings) for scope in self.scopes)
        self.max_locals = max(self.max_locals, total_locals)

    def pop_scope(self) -> CompilationScope:
        """Exit current lexical scope."""
        return self.scopes.pop()

    def update_max_locals(self) -> None:
        """Update max locals based on current scope depth."""
        total_locals = sum(scope.next_index for scope in self.scopes)
        self.max_locals = max(self.max_locals, total_locals)

    def current_scope(self) -> CompilationScope:
        """Get current scope."""
        return self.scopes[-1]

    def add_constant(self, value: AIFPLValue) -> int:
        """Add constant to pool and return its index."""
        # Reuse existing constants for efficiency
        try:
            return self.constants.index(value)

        except ValueError:
            index = len(self.constants)
            self.constants.append(value)
            return index

    def add_name(self, name: str) -> int:
        """Add name to pool and return its index."""
        try:
            return self.names.index(name)

        except ValueError:
            index = len(self.names)
            self.names.append(name)
            return index

    def add_code_object(self, code_obj: CodeObject) -> int:
        """Add nested code object and return its index."""
        index = len(self.code_objects)
        self.code_objects.append(code_obj)
        return index

    def resolve_variable(self, name: str) -> Tuple[str, int, int]:
        """Resolve variable to (type, depth, index).

        For variables in let bindings, we use flat indexing within the same frame.
        All lets in the same function/module use depth=0 with different flat indices.

        Returns:
            ('local', depth, index) for local variables
            ('global', 0, name_index) for global variables
        """
        # Search from innermost to outermost scope
        flat_index_offset = 0
        for depth, scope in enumerate(reversed(self.scopes)):
            index = scope.get_binding(name)
            if index is not None:
                # Calculate flat index: sum of all previous scope sizes + index within scope
                # All scopes in the same function use depth=0 (same frame)
                for i in range(len(self.scopes) - depth - 1):
                    flat_index_offset += len(self.scopes[i].bindings)

                flat_index = flat_index_offset + index
                # Always use depth=0 for let variables (they're all in the same frame)
                return ('local', 0, flat_index)

        # Not found in local scopes, must be global
        name_index = self.add_name(name)
        return ('global', 0, name_index)

    def emit(self, opcode: Opcode, arg1: int = 0, arg2: int = 0) -> int:
        """Emit an instruction and return its index."""
        instr = make_instruction(opcode, arg1, arg2)
        index = len(self.instructions)
        self.instructions.append(instr)
        return index

    def patch_jump(self, instr_index: int, target: int) -> None:
        """Patch a jump instruction to point to target."""
        self.instructions[instr_index].arg1 = target

    def current_instruction_index(self) -> int:
        """Get index of next instruction to be emitted."""
        return len(self.instructions)


class AIFPLCompiler:
    """Compiles AIFPL AST to bytecode.

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
        'map', 'filter', 'fold', 'range', 'find', 'any?', 'all?',
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

    def __init__(self) -> None:
        """Initialize compiler."""
        self.builtin_indices = {name: i for i, name in enumerate(self.BUILTIN_TABLE)}

    def compile(self, expr: AIFPLValue, name: str = "<module>") -> CodeObject:
        """Compile an AIFPL expression to bytecode.

        Args:
            expr: AST to compile
            name: Name for the code object (for debugging)

        Returns:
            Compiled code object
        """
        ctx = CompilationContext()

        # Compile the expression
        self._compile_expression(expr, ctx)

        # Add RETURN instruction
        ctx.emit(Opcode.RETURN)

        # Build code object
        return CodeObject(
            instructions=ctx.instructions,
            constants=ctx.constants,
            names=ctx.names,
            code_objects=ctx.code_objects,
            param_count=0,
            local_count=ctx.max_locals,
            name=name
        )

    def _compile_expression(self, expr: AIFPLValue, ctx: CompilationContext) -> None:
        """Compile an expression, leaving result on stack."""

        # Self-evaluating values
        if isinstance(expr, AIFPLNumber):
            const_index = ctx.add_constant(expr)
            ctx.emit(Opcode.LOAD_CONST, const_index)
            return

        if isinstance(expr, AIFPLString):
            const_index = ctx.add_constant(expr)
            ctx.emit(Opcode.LOAD_CONST, const_index)
            return

        if isinstance(expr, AIFPLBoolean):
            if expr.value:
                ctx.emit(Opcode.LOAD_TRUE)
                return

            ctx.emit(Opcode.LOAD_FALSE)
            return

        # Symbol (variable reference)
        if isinstance(expr, AIFPLSymbol):
            self._compile_variable_load(expr.name, ctx)
            return

        # List (function call or special form)
        if isinstance(expr, AIFPLList):
            if expr.is_empty():
                ctx.emit(Opcode.LOAD_EMPTY_LIST)
                return

            self._compile_list(expr, ctx)
            return

        raise AIFPLEvalError(
            message=f"Cannot compile expression of type {type(expr).__name__}",
            received=str(expr)
        )

    def _compile_variable_load(self, name: str, ctx: CompilationContext) -> None:
        """Compile a variable load."""
        var_type, depth, index = ctx.resolve_variable(name)

        if var_type == 'local':
            ctx.emit(Opcode.LOAD_VAR, depth, index)
            return

        ctx.emit(Opcode.LOAD_NAME, index)

    def _compile_list(self, expr: AIFPLList, ctx: CompilationContext) -> None:
        """Compile a list expression (function call or special form)."""
        if expr.is_empty():
            ctx.emit(Opcode.LOAD_EMPTY_LIST)
            return

        first = expr.first()

        # Check for special forms
        if isinstance(first, AIFPLSymbol):
            if first.name == 'if':
                self._compile_if(expr, ctx)
                return

            if first.name == 'let':
                self._compile_let(expr, ctx)
                return

            if first.name == 'lambda':
                self._compile_lambda(expr, ctx)
                return

            if first.name == 'match':
                self._compile_match(expr, ctx)
                return

            if first.name == 'and':
                self._compile_and(expr, ctx)
                return

            if first.name == 'or':
                self._compile_or(expr, ctx)
                return

            if first.name == 'quote':
                # Quote: return the quoted value as a constant
                if len(expr.elements) != 2:
                    raise AIFPLEvalError(
                        message="Quote expression has wrong number of arguments",
                        received=f"Got {len(expr.elements) - 1} arguments: {self.format_result(expr)}",
                        expected="Exactly 1 argument",
                        example="(quote expr) or 'expr",
                        suggestion="Quote requires exactly one expression to quote"
                    )

                quoted = expr.elements[1]
                const_index = ctx.add_constant(quoted)
                ctx.emit(Opcode.LOAD_CONST, const_index)
                return

            if first.name == 'alist':
                # Special handling for alist: each pair is a list that needs evaluation
                # (alist ("key" value-expr) ...) -> create list for each pair
                arg_exprs = list(expr.elements[1:])
                for pair_expr in arg_exprs:
                    # Each pair should be a list - compile its elements and make a list
                    if not isinstance(pair_expr, AIFPLList):
                        raise AIFPLEvalError("alist pairs must be lists")
                    if len(pair_expr.elements) != 2:
                        raise AIFPLEvalError("alist pairs must have exactly 2 elements")

                    # Compile each element of the pair
                    for elem in pair_expr.elements:
                        self._compile_expression(elem, ctx)
                    # Create a list from the two elements
                    ctx.emit(Opcode.MAKE_LIST, 2)

                # Now call alist with the evaluated pairs
                builtin_index = self.builtin_indices['alist']
                ctx.emit(Opcode.CALL_BUILTIN, builtin_index, len(arg_exprs))
                return

            if first.name in ['map', 'filter', 'fold', 'range', 'find', 'any?', 'all?']:
                self._compile_higher_order_function(expr, ctx)
                return

        # Regular function call
        self._compile_function_call(expr, ctx)

    def _compile_if(self, expr: AIFPLList, ctx: CompilationContext) -> None:
        """Compile if expression: (if condition then else)"""
        if len(expr.elements) != 4:
            raise AIFPLEvalError(
                message="if requires exactly 3 arguments",
                received=f"Got {len(expr.elements) - 1} arguments"
            )

        _, condition, then_expr, else_expr = expr.elements

        # Compile condition
        self._compile_expression(condition, ctx)

        # Jump to else if condition is false
        jump_to_else = ctx.emit(Opcode.POP_JUMP_IF_FALSE, 0)  # Will patch later

        # Compile then branch
        self._compile_expression(then_expr, ctx)

        # Jump past else branch
        jump_past_else = ctx.emit(Opcode.JUMP, 0)  # Will patch later

        # Patch jump to else
        else_start = ctx.current_instruction_index()
        ctx.patch_jump(jump_to_else, else_start)

        # Compile else branch
        self._compile_expression(else_expr, ctx)

        # Patch jump past else
        after_else = ctx.current_instruction_index()
        ctx.patch_jump(jump_past_else, after_else)

    def _compile_and(self, expr: AIFPLList, ctx: CompilationContext) -> None:
        """Compile and expression with short-circuit evaluation.

        (and) -> #t
        (and a) -> a (must be boolean)
        (and a b c) -> if any is false, return #f; else return #t

        Implementation:
        - Evaluate first argument
        - If false, jump to end and return #f
        - Otherwise, pop it and continue to next argument
        - If all are true, return #t
        """
        args = list(expr.elements[1:])  # Skip 'and' symbol

        if len(args) == 0:
            # (and) -> #t
            ctx.emit(Opcode.LOAD_TRUE)
            return

        # Multiple arguments: short-circuit evaluation
        jump_to_false = []  # List of jumps to return #f

        for arg in args:
            # Compile argument
            self._compile_expression(arg, ctx)

            # If false, jump to "return false" section
            jump = ctx.emit(Opcode.POP_JUMP_IF_FALSE, 0)
            jump_to_false.append(jump)

        # All arguments were true - return #t
        ctx.emit(Opcode.LOAD_TRUE)
        jump_to_end = ctx.emit(Opcode.JUMP, 0)

        # Return false section
        false_section = ctx.current_instruction_index()
        for jump in jump_to_false:
            ctx.patch_jump(jump, false_section)

        ctx.emit(Opcode.LOAD_FALSE)

        # Patch jump to end
        end = ctx.current_instruction_index()
        ctx.patch_jump(jump_to_end, end)

    def _compile_or(self, expr: AIFPLList, ctx: CompilationContext) -> None:
        """Compile or expression with short-circuit evaluation.

        (or) -> #f
        (or a) -> a (must be boolean)
        (or a b c) -> if any is true, return #t; else return #f

        Implementation:
        - Evaluate first argument
        - If true, jump to end and return #t
        - Otherwise, pop it and continue to next argument
        - If all are false, return #f
        """
        args = list(expr.elements[1:])  # Skip 'or' symbol

        if len(args) == 0:
            # (or) -> #f
            ctx.emit(Opcode.LOAD_FALSE)
            return

        # Multiple arguments: short-circuit evaluation
        jump_to_true = []  # List of jumps to return #t

        for arg in args:
            # Compile argument
            self._compile_expression(arg, ctx)

            # If true, jump to "return true" section
            jump = ctx.emit(Opcode.POP_JUMP_IF_TRUE, 0)
            jump_to_true.append(jump)

        # All arguments were false - return #f
        ctx.emit(Opcode.LOAD_FALSE)
        jump_to_end = ctx.emit(Opcode.JUMP, 0)

        # Return true section
        true_section = ctx.current_instruction_index()
        for jump in jump_to_true:
            ctx.patch_jump(jump, true_section)

        ctx.emit(Opcode.LOAD_TRUE)

        # Patch jump to end
        end = ctx.current_instruction_index()
        ctx.patch_jump(jump_to_end, end)

    def _compile_let(self, expr: AIFPLList, ctx: CompilationContext,
                    current_binding_name: str | None = None) -> None:
        """Compile let expression: (let ((var val) ...) body)"""
        if len(expr.elements) < 3:
            raise AIFPLEvalError(
                message="Let expression structure is incorrect",
                received=f"Got {len(expr.elements)} elements",
                expected="Exactly 3 elements: (let ((bindings...)) body)",
                example="(let ((x 5) (y 10)) (+ x y))",
                suggestion="Let needs binding list and body: (let ((var1 val1) (var2 val2) ...) body)"
            )

        _, bindings_list, body = expr.elements[0], expr.elements[1], expr.elements[2]

        if not isinstance(bindings_list, AIFPLList):
            raise AIFPLEvalError(
                message="Let binding list must be a list",
                received=f"Binding list: {bindings_list.type_name()}",
                expected="List of bindings: ((var1 val1) (var2 val2) ...)",
                example="(let ((x 5) (y (* x 2))) (+ x y))",
                suggestion="Wrap bindings in parentheses: ((var val) (var val) ...)"
            )

        # First pass: Add all binding names to scope (for recursive references)
        binding_pairs = []
        for i, binding in enumerate(bindings_list.elements):
            if not isinstance(binding, AIFPLList):
                raise AIFPLEvalError(
                    message=f"Let binding {i+1} must be a list",
                    received=f"Binding {i+1}: {binding.type_name()}",
                    expected="List with variable and value: (var val)",
                    example='Correct: (x 5)\\nIncorrect: x or "x"',
                    suggestion="Wrap each binding in parentheses: (variable value)"
                )

            if len(binding.elements) != 2:
                raise AIFPLEvalError(
                    message=f"Let binding {i+1} has wrong number of elements",
                    received=f"Binding {i+1}: has {len(binding.elements)} elements",
                    expected="Each binding needs exactly 2 elements: (variable value)",
                    example='Correct: (x 5)\\nIncorrect: (x) or (x 5 6)',
                    suggestion="Each binding: (variable-name value-expression)"
                )

            name_expr, value_expr = binding.elements
            if not isinstance(name_expr, AIFPLSymbol):
                raise AIFPLEvalError(
                    message=f"Let binding {i+1} variable must be a symbol",
                    received=f"Variable: {name_expr.type_name()}",
                    expected="Unquoted symbol (variable name)",
                    example='Correct: (x 5)\\nIncorrect: ("x" 5) or (1 5)',
                    suggestion='Use unquoted variable names: x, not "x"'
                )

            binding_pairs.append((name_expr.name, value_expr))
            # Add binding to scope NOW so recursive lambdas can reference it
            ctx.current_scope().add_binding(name_expr.name)

        # Check for duplicate binding names
        var_names = [name for name, _ in binding_pairs]
        if len(var_names) != len(set(var_names)):
            duplicates = [name for name in var_names if var_names.count(name) > 1]
            raise AIFPLEvalError(
                message="Let binding variables must be unique",
                received=f"Duplicate variables: {duplicates}",
                expected="All variable names should be different",
                example='Correct: (let ((x 1) (y 2)) ...)\\nIncorrect: (let ((x 1) (x 2)) ...)',
                suggestion="Use different names for each variable"
            )

        # Update max locals after adding all bindings
        ctx.update_max_locals()

        # Analyze dependencies to determine evaluation order
        analyzer = AIFPLDependencyAnalyzer()
        binding_groups = analyzer.analyze_let_bindings(binding_pairs)

        # Topologically sort the groups based on their dependencies
        # (The SCC algorithm should return them in the right order, but let's be explicit)
        sorted_groups = []
        remaining_groups = list(binding_groups)
        processed_names: Set[str] = set()

        while remaining_groups:
            # Find a group with no unprocessed dependencies
            for i, group in enumerate(remaining_groups):
                if group.depends_on.issubset(processed_names):
                    sorted_groups.append(group)
                    processed_names.update(group.names)
                    remaining_groups.pop(i)
                    break
            else:
                # No group found - circular dependency (shouldn't happen)
                raise AIFPLEvalError("Circular dependency detected in let bindings")

        binding_groups = sorted_groups

        # Second pass: Compile values and store them in dependency order
        # Track which bindings are lambdas that reference siblings (for mutual recursion)
        mutual_recursion_patches: List[Tuple[int, str, int]] = []  # List of (closure_var_index, sibling_name, sibling_var_index)

        # Compile each group in topological order
        for group in binding_groups:
            # Get all binding names in this group for mutual recursion detection
            group_names = list(group.names)

            # Compile each binding in the group
            for name, value_expr in group.bindings:
                # Compile this binding
                self._compile_single_let_binding(
                    ctx, name, value_expr,
                    is_recursive_group=group.is_recursive,
                    group_names=group_names,
                    all_binding_names=[n for n, _ in binding_pairs],
                    mutual_recursion_patches=mutual_recursion_patches
                )

        # Third pass: Patch all mutual recursion references
        # Now all lambdas have been stored, so we can safely reference them
        for closure_var_index, sibling_name, sibling_var_index in mutual_recursion_patches:
            name_index = ctx.add_name(sibling_name)
            # We need to pass 3 pieces of info but only have 2 args
            # Solution: Store (sibling_var_index, name_index) as a tuple in constants
            # and pass closure_var_index and const_index
            patch_info = AIFPLList((AIFPLNumber(sibling_var_index), AIFPLNumber(name_index)))
            const_index = ctx.add_constant(patch_info)
            # arg1=closure_var_index, arg2=const_index (contains sibling_var_index and name_index)
            ctx.emit(Opcode.PATCH_CLOSURE_SIBLING, closure_var_index, const_index)

        # Compile body
        self._compile_expression(body, ctx)

    def _compile_single_let_binding(
        self,
        ctx: CompilationContext,
        name: str,
        value_expr: AIFPLValue,
        is_recursive_group: bool,
        group_names: List[str],
        all_binding_names: List[str],
        mutual_recursion_patches: List[Tuple[int, str, int]]
    ) -> None:
        """Compile a single let binding."""
        # Get the index for this variable
        var_type, depth, var_index = ctx.resolve_variable(name)

        # Check if this is a self-referential lambda
        first_elem = value_expr.first() if isinstance(value_expr, AIFPLList) and not value_expr.is_empty() else None
        is_self_ref_lambda = (isinstance(value_expr, AIFPLList) and
                             not value_expr.is_empty() and
                             isinstance(first_elem, AIFPLSymbol) and
                             first_elem.name == 'lambda')

        # Check if lambda references itself (recursive)
        is_recursive = False
        if is_self_ref_lambda:
            # Check if the lambda body references 'name'
            is_recursive = self._references_variable(value_expr, name)

        # Compile the value expression
        # Check which siblings this lambda references (for mutual recursion)
        sibling_refs = []
        if is_self_ref_lambda and is_recursive_group and len(group_names) > 1:
            # Only check siblings in the same recursive group
            for other_name in group_names:
                if other_name != name and self._references_variable(value_expr, other_name):
                    sibling_refs.append(other_name)

        # Pass the binding name if it's a lambda (for self-reference detection)
        if is_self_ref_lambda:
            # For mutual recursion, pass sibling names in the same recursive group
            # These are bindings that haven't been evaluated yet and need to be patched
            sibling_bindings = group_names if is_recursive_group else None
            # Type narrowing: we know value_expr is AIFPLList from is_self_ref_lambda check
            if not isinstance(value_expr, AIFPLList):
                raise AIFPLEvalError("Expected lambda expression to be a list")
            self._compile_lambda(value_expr, ctx, binding_name=name, let_bindings=sibling_bindings)

        else:
            self._compile_expression(value_expr, ctx)

        # Store in local variable
        ctx.emit(Opcode.STORE_VAR, depth, var_index)

        # For recursive closures, patch them to reference themselves
        if is_recursive:
            # PATCH_CLOSURE_SELF: arg1 = name index, arg2 = var index (within current scope)
            # The VM will need to use depth=0 since we just stored there
            name_index = ctx.add_name(name)
            ctx.emit(Opcode.PATCH_CLOSURE_SELF, name_index, var_index)

        # Track sibling references for later patching
        if sibling_refs:
            for sibling_name in sibling_refs:
                # Find the sibling's variable index
                _, _, sibling_var_index = ctx.resolve_variable(sibling_name)
                mutual_recursion_patches.append((var_index, sibling_name, sibling_var_index))

    def _references_variable(self, expr: AIFPLValue, var_name: str) -> bool:
        """Check if an expression references a variable."""
        if isinstance(expr, AIFPLSymbol):
            return expr.name == var_name

        if isinstance(expr, AIFPLList):
            return any(self._references_variable(elem, var_name) for elem in expr.elements)

        return False

    def _compile_lambda(self, expr: AIFPLList, ctx: CompilationContext,
                       binding_name: str | None = None, let_bindings: List[str] | None = None) -> None:
        """Compile lambda expression: (lambda (params...) body)"""
        if len(expr.elements) != 3:
            raise AIFPLEvalError(
                message="Lambda expression structure is incorrect",
                received=f"Got {len(expr.elements)} elements",
                expected="Exactly 3 elements: (lambda (params...) body)",
                example="(lambda (x y) (+ x y))",
                suggestion="Lambda needs parameter list and body: (lambda (param1 param2 ...) body-expression)"
            )

        _, params_list, body = expr.elements

        if not isinstance(params_list, AIFPLList):
            raise AIFPLEvalError(
                message="Lambda parameters must be a list",
                received=f"Parameter list: {params_list.type_name()}",
                expected="List of symbols: (param1 param2 ...)",
                example="(lambda (x y z) (+ x y z))",
                suggestion="Parameters should be unquoted variable names"
            )

        # Extract parameter names
        param_names = []
        for i, param in enumerate(params_list.elements):
            if not isinstance(param, AIFPLSymbol):
                raise AIFPLEvalError(
                    message=f"Lambda parameter {i+1} must be a symbol",
                    received=f"Parameter {i+1}: {param.type_name()}",
                    expected="Unquoted symbol (variable name)",
                    example='Correct: (lambda (x y) (+ x y))\\nIncorrect: (lambda ("x" 1) ...)',
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
                example='Correct: (lambda (x y z) ...)\\nIncorrect: (lambda (x y x) ...)',
                suggestion="Use different names for each parameter"
            )

        # Find free variables (variables used in body but not parameters)
        bound_vars = set(param_names)
        free_vars = self._find_free_variables(body, bound_vars, ctx)

        # Emit instructions to load free variables onto stack (for capture).
        # Only capture variables from outer scopes, not self-references in current scope.
        captured_vars = []
        for free_var in free_vars:
            var_type, depth, index = ctx.resolve_variable(free_var)
            if var_type == 'local':
                # Check if this is a self-reference
                if free_var == binding_name:
                    # Self-reference - don't capture, will be patched later
                    pass

                elif let_bindings and free_var in let_bindings:
                    # Sibling in the same recursive group - don't capture, will be patched later
                    # This handles mutual recursion where siblings reference each other
                    pass

                else:
                    # Variable from an outer scope - capture it
                    # With dependency analysis, all dependencies are evaluated before
                    # the lambda is created, so we can safely capture them
                    ctx.emit(Opcode.LOAD_VAR, depth, index)
                    captured_vars.append(free_var)

                # else: self-reference, skip capturing
                # else: in current scope - it's a self-reference
                # Don't capture, lambda will look it up via LOAD_NAME -> closure_env
            # else: global variable - don't capture or track as free

        # Create new compilation context for lambda body
        lambda_ctx = CompilationContext()
        lambda_ctx.push_scope()

        # Add parameters to lambda scope
        for param_name in param_names:
            lambda_ctx.current_scope().add_binding(param_name)

        # Add captured free variables to lambda scope
        for free_var in captured_vars:
            lambda_ctx.current_scope().add_binding(free_var)

        # For sibling bindings that weren't captured, we need to track them differently
        # They should be resolved from parent frame at runtime
        sibling_bindings = []
        if let_bindings:
            for free_var in free_vars:
                if free_var != binding_name and free_var in let_bindings:
                    sibling_bindings.append(free_var)

        # Store parent context in lambda context for variable resolution
        lambda_ctx.parent_ctx = ctx
        lambda_ctx.sibling_bindings = sibling_bindings

        # Update max locals for the lambda (only its own locals, not parent's)
        lambda_ctx.update_max_locals()

        # Compile lambda body - free vars will be resolved as locals
        self._compile_expression(body, lambda_ctx)
        lambda_ctx.emit(Opcode.RETURN)

        # Create code object for lambda
        lambda_code = CodeObject(
            instructions=lambda_ctx.instructions,
            constants=lambda_ctx.constants,
            names=lambda_ctx.names,
            code_objects=lambda_ctx.code_objects,
            free_vars=captured_vars,  # List of captured variable names
            param_count=len(param_names),
            local_count=lambda_ctx.max_locals,
            name="<lambda>"
        )

        # Add to parent's code objects
        code_index = ctx.add_code_object(lambda_code)

        # Emit MAKE_CLOSURE instruction with capture count
        ctx.emit(Opcode.MAKE_CLOSURE, code_index, len(captured_vars))

    def _find_free_variables(self, expr: AIFPLValue, bound_vars: Set[str],
                            parent_ctx: CompilationContext) -> List[str]:
        """Find free variables in an expression.

        Free variables are those that are:
        - Referenced in the expression
        - Not in bound_vars (parameters)
        - Not globals
        - Defined in parent scopes
        """
        free: List[str] = []
        self._collect_free_vars(expr, bound_vars, parent_ctx, free, set())
        return free

    def _collect_free_vars(self, expr: AIFPLValue, bound_vars: Set[str],
                          parent_ctx: CompilationContext, free: List[str],
                          seen: Set[str]) -> None:
        """Recursively collect free variables."""
        if isinstance(expr, AIFPLSymbol):
            name = expr.name
            if name in seen or name in bound_vars:
                return

            # Check if it's defined in parent scopes
            var_type, _, _ = parent_ctx.resolve_variable(name)
            if var_type == 'local' and name not in seen:
                free.append(name)
                seen.add(name)

        elif isinstance(expr, AIFPLList):
            if expr.is_empty():
                return

            first = expr.first()

            # Handle special forms that bind variables
            if isinstance(first, AIFPLSymbol):
                if first.name == 'lambda':
                    # Nested lambda: we need to find what free variables it uses
                    # that come from outer scopes, so the parent lambda can capture them.
                    # The nested lambda will then capture them from the parent.
                    if len(expr.elements) >= 3:
                        nested_params = expr.elements[1]
                        nested_body = expr.elements[2]

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

                if first.name == 'let':
                    # Let bindings create new bound variables
                    # Extract binding names and recurse with updated bound_vars
                    if len(expr.elements) >= 3:
                        bindings_list = expr.elements[1]
                        body = expr.elements[2]

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
            for elem in expr.elements:
                self._collect_free_vars(elem, bound_vars, parent_ctx, free, seen)

    def _compile_function_call(self, expr: AIFPLList, ctx: CompilationContext) -> None:
        """Compile a function call."""
        func_expr = expr.first()
        arg_exprs = list(expr.elements[1:])

        # Check if calling a known builtin
        if isinstance(func_expr, AIFPLSymbol) and func_expr.name in self.builtin_indices:
            # Compile arguments
            for arg in arg_exprs:
                self._compile_expression(arg, ctx)

            # Emit builtin call
            builtin_index = self.builtin_indices[func_expr.name]
            ctx.emit(Opcode.CALL_BUILTIN, builtin_index, len(arg_exprs))
        else:
            # General function call
            # Compile function expression
            self._compile_expression(func_expr, ctx)

            # Compile arguments
            for arg in arg_exprs:
                self._compile_expression(arg, ctx)

            # Emit call
            ctx.emit(Opcode.CALL_FUNCTION, len(arg_exprs))

    def _compile_higher_order_function(self, expr: AIFPLList, ctx: CompilationContext) -> None:
        """Compile higher-order functions (map, filter, fold, etc).

        The first argument is the function to apply. If it's a builtin symbol,
        we pass it as a symbol constant so the VM can resolve it.
        """
        first_elem = expr.first()
        if not isinstance(first_elem, AIFPLSymbol):
            raise AIFPLEvalError("Higher-order function name must be a symbol")
        func_name = first_elem.name
        arg_exprs = list(expr.elements[1:])

        if not arg_exprs:
            raise AIFPLEvalError(f"{func_name} requires at least one argument")

        # First argument: the function to apply
        func_arg = arg_exprs[0]

        # Compile the function argument (lambda, variable, or builtin name)
        self._compile_expression(func_arg, ctx)

        # Compile remaining arguments
        for arg in arg_exprs[1:]:
            self._compile_expression(arg, ctx)

        # Call the higher-order function
        builtin_index = self.builtin_indices[func_name]
        ctx.emit(Opcode.CALL_BUILTIN, builtin_index, len(arg_exprs))

    def _compile_match(self, expr: AIFPLList, ctx: CompilationContext) -> None:
        """Compile match expression: (match value (pattern1 result1) ...)"""
        if len(expr.elements) < 3:
            raise AIFPLEvalError(
                message="Match expression has wrong number of arguments",
                received=f"Got {len(expr.elements) - 1} arguments",
                expected="At least 2 arguments: (match value (pattern1 result1) ...)",
                example="(match x ((42) \"found\") (_ \"default\"))"
            )

        value_expr = expr.elements[1]
        clauses = expr.elements[2:]

        # Validate all clauses upfront
        for i, clause in enumerate(clauses):
            if not isinstance(clause, AIFPLList) or len(clause.elements) != 2:
                raise AIFPLEvalError(
                    message=f"Match clause {i+1} must be a list with 2 elements",
                    received=f"Clause {i+1}: {clause}",
                    expected="(pattern result)",
                    example="(match x ((42) \"found\") (_ \"default\"))"
                )

        # Validate pattern syntax for all clauses
        for i, clause in enumerate(clauses):
            if not isinstance(clause, AIFPLList):
                raise AIFPLEvalError(f"Clause {i+1} must be a list")

            pattern = clause.elements[0]
            try:
                self._validate_pattern_syntax(pattern)
            except AIFPLEvalError as e:
                raise AIFPLEvalError(
                    message=f"Invalid pattern in clause {i+1}",
                    context=str(e)
                ) from e

        # Compile value and store in temporary local
        self._compile_expression(value_expr, ctx)

        # Save the current bindings state - pattern variables should not be visible after match
        saved_bindings = ctx.current_scope().bindings.copy()
        saved_next_index = ctx.current_scope().next_index

        # Allocate a temporary local for the match value
        match_temp_index = ctx.current_scope().add_binding("<match-temp>")
        ctx.update_max_locals()

        ctx.emit(Opcode.STORE_VAR, 0, match_temp_index)

        # Now save state again, including match-temp
        saved_bindings_with_temp = ctx.current_scope().bindings.copy()
        saved_next_index_with_temp = ctx.current_scope().next_index

        # Compile each clause
        end_label = None  # Will be patched at the end
        jump_to_end_indices = []  # Track all jumps to end

        for i, clause in enumerate(clauses):
            if not isinstance(clause, AIFPLList):
                raise AIFPLEvalError(f"Clause {i+1} must be a list")

            pattern = clause.elements[0]
            result_expr = clause.elements[1]

            # Compile pattern test
            # Returns True if pattern matches, False otherwise
            # May bind variables as side effect
            next_pattern_label = ctx.current_instruction_index()

            self._compile_pattern(pattern, match_temp_index, ctx)

            # If pattern didn't match, jump to next pattern
            if i < len(clauses) - 1:
                # Not the last pattern, jump to next on failure
                next_pattern_jump = ctx.emit(Opcode.POP_JUMP_IF_FALSE, 0)
            else:
                # Last pattern - if it fails, we need to error
                # For now, assume last pattern always matches (should be _ wildcard)
                # TODO: Add runtime check for no-match case
                next_pattern_jump = ctx.emit(Opcode.POP_JUMP_IF_FALSE, 0)  # Will patch to error

            # Pattern matched! Compile result expression
            self._compile_expression(result_expr, ctx)

            # Jump to end
            jump_idx = ctx.emit(Opcode.JUMP, 0)
            jump_to_end_indices.append(jump_idx)

            # Reset bindings for next pattern - each pattern gets a fresh scope
            ctx.current_scope().bindings = saved_bindings_with_temp.copy()
            ctx.current_scope().next_index = saved_next_index_with_temp

            # Patch the jump to next pattern (if not last)
            if i < len(clauses) - 1:
                next_pattern_start = ctx.current_instruction_index()
                ctx.patch_jump(next_pattern_jump, next_pattern_start)
            else:
                # Last pattern failed - need to generate error
                # For now, just patch to same location (will fall through)
                error_location = ctx.current_instruction_index()
                ctx.patch_jump(next_pattern_jump, error_location)
                # Emit error for no match
                error_msg = ctx.add_constant(AIFPLString("No patterns matched in match expression"))
                ctx.emit(Opcode.RAISE_ERROR, error_msg)

        # Patch all jumps to end
        end_location = ctx.current_instruction_index()
        for jump_idx in jump_to_end_indices:
            ctx.patch_jump(jump_idx, end_location)

        # Restore bindings to remove pattern variables from scope
        ctx.current_scope().bindings = saved_bindings
        ctx.current_scope().next_index = saved_next_index

    def _compile_pattern(self, pattern: AIFPLValue, match_value_index: int,
                        ctx: CompilationContext) -> None:
        """
        Compile a pattern test.

        Leaves a boolean on the stack:
        - True if pattern matches (and binds any variables)
        - False if pattern doesn't match

        Args:
            pattern: Pattern to match
            match_value_index: Index of local variable holding value to match
            ctx: Compilation context
        """
        # Literal patterns: numbers, strings, booleans
        if isinstance(pattern, (AIFPLNumber, AIFPLString, AIFPLBoolean)):
            # Load value and compare
            ctx.emit(Opcode.LOAD_VAR, 0, match_value_index)
            const_index = ctx.add_constant(pattern)
            ctx.emit(Opcode.LOAD_CONST, const_index)
            builtin_index = self.builtin_indices['=']
            ctx.emit(Opcode.CALL_BUILTIN, builtin_index, 2)
            return

        # Variable pattern: binds the value
        if isinstance(pattern, AIFPLSymbol):
            if pattern.name == '_':
                # Wildcard - always matches, no binding
                ctx.emit(Opcode.LOAD_TRUE)
                return
            else:
                # Bind variable
                var_index = ctx.current_scope().add_binding(pattern.name)
                ctx.update_max_locals()
                ctx.emit(Opcode.LOAD_VAR, 0, match_value_index)
                ctx.emit(Opcode.STORE_VAR, 0, var_index)
                ctx.emit(Opcode.LOAD_TRUE)
                return

        # List patterns
        if isinstance(pattern, AIFPLList):
            self._compile_list_pattern(pattern, match_value_index, ctx)
            return

        raise AIFPLEvalError(f"Unknown pattern type: {type(pattern).__name__}")

    def _compile_list_pattern(self, pattern: AIFPLList, match_value_index: int,
                             ctx: CompilationContext) -> None:
        """Compile a list pattern."""

        # Empty list pattern
        if pattern.is_empty():
            # Check if value is empty list
            ctx.emit(Opcode.LOAD_VAR, 0, match_value_index)
            builtin_index = self.builtin_indices['null?']
            ctx.emit(Opcode.CALL_BUILTIN, builtin_index, 1)
            return

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
                # Load value and call type predicate
                ctx.emit(Opcode.LOAD_VAR, 0, match_value_index)

                # Type predicates need special handling - they're not all builtins
                if type_pred in self.builtin_indices:
                    builtin_index = self.builtin_indices[type_pred]
                    ctx.emit(Opcode.CALL_BUILTIN, builtin_index, 1)
                else:
                    # Use LOAD_NAME for type predicates not in builtin table
                    name_index = ctx.add_name(type_pred)
                    ctx.emit(Opcode.LOAD_NAME, name_index)
                    ctx.emit(Opcode.CALL_FUNCTION, 1)

                # If type matches, bind the variable
                fail_label = ctx.emit(Opcode.POP_JUMP_IF_FALSE, 0)

                # Type matched - bind variable
                if isinstance(var_pattern, AIFPLSymbol) and var_pattern.name != '_':
                    var_index = ctx.current_scope().add_binding(var_pattern.name)
                    ctx.update_max_locals()
                    ctx.emit(Opcode.LOAD_VAR, 0, match_value_index)
                    ctx.emit(Opcode.STORE_VAR, 0, var_index)

                ctx.emit(Opcode.LOAD_TRUE)
                success_jump = ctx.emit(Opcode.JUMP, 0)

                # Type didn't match
                fail_location = ctx.current_instruction_index()
                ctx.patch_jump(fail_label, fail_location)
                ctx.emit(Opcode.LOAD_FALSE)

                # Patch success jump
                success_location = ctx.current_instruction_index()
                ctx.patch_jump(success_jump, success_location)
                return

        # Check for cons pattern (head . tail)
        dot_position = None
        for i, elem in enumerate(pattern.elements):
            if isinstance(elem, AIFPLSymbol) and elem.name == '.':
                dot_position = i
                break

        if dot_position is not None:
            self._compile_cons_pattern(pattern, match_value_index, dot_position, ctx)
            return

        # Fixed-length list pattern: (p1 p2 p3)
        self._compile_fixed_list_pattern(pattern, match_value_index, ctx)

    def _compile_fixed_list_pattern(self, pattern: AIFPLList, match_value_index: int,
                                    ctx: CompilationContext) -> None:
        """Compile a fixed-length list pattern like (a b c)."""

        # Check if value is a list
        ctx.emit(Opcode.LOAD_VAR, 0, match_value_index)
        builtin_index = self.builtin_indices['list?']
        ctx.emit(Opcode.CALL_BUILTIN, builtin_index, 1)
        fail_jump = ctx.emit(Opcode.POP_JUMP_IF_FALSE, 0)

        # Check length
        ctx.emit(Opcode.LOAD_VAR, 0, match_value_index)
        builtin_index = self.builtin_indices['length']
        ctx.emit(Opcode.CALL_BUILTIN, builtin_index, 1)
        ctx.emit(Opcode.LOAD_CONST, ctx.add_constant(AIFPLNumber(len(pattern.elements))))
        builtin_index = self.builtin_indices['=']
        ctx.emit(Opcode.CALL_BUILTIN, builtin_index, 2)
        length_fail_jump = ctx.emit(Opcode.POP_JUMP_IF_FALSE, 0)

        # Match each element
        # We need to extract each element and match it against the sub-pattern
        fail_jumps = []
        for i, elem_pattern in enumerate(pattern.elements):
            # Extract element into a temporary
            elem_temp_index = ctx.current_scope().add_binding(f"<elem-temp-{i}>")
            ctx.update_max_locals()

            ctx.emit(Opcode.LOAD_VAR, 0, match_value_index)
            ctx.emit(Opcode.LOAD_CONST, ctx.add_constant(AIFPLNumber(i)))
            # Use direct access to ensure type is int
            builtin_index = self.builtin_indices['list-ref']
            ctx.emit(Opcode.CALL_BUILTIN, builtin_index, 2)
            ctx.emit(Opcode.STORE_VAR, 0, elem_temp_index)

            # Recursively match the element pattern
            self._compile_pattern(elem_pattern, elem_temp_index, ctx)

            # If element pattern fails, whole list pattern fails
            elem_fail = ctx.emit(Opcode.POP_JUMP_IF_FALSE, 0)
            fail_jumps.append(elem_fail)

        # Success
        ctx.emit(Opcode.LOAD_TRUE)
        success_jump = ctx.emit(Opcode.JUMP, 0)

        # Failure paths
        fail_location = ctx.current_instruction_index()
        ctx.patch_jump(fail_jump, fail_location)
        ctx.patch_jump(length_fail_jump, fail_location)
        for fj in fail_jumps:
            ctx.patch_jump(fj, fail_location)
        ctx.emit(Opcode.LOAD_FALSE)

        success_location = ctx.current_instruction_index()
        ctx.patch_jump(success_jump, success_location)

    def _compile_cons_pattern(self, pattern: AIFPLList, match_value_index: int,
                             dot_position: int, ctx: CompilationContext) -> None:
        """Compile a cons pattern like (head . tail) or (a b . rest)."""

        # Check if value is a non-empty list
        ctx.emit(Opcode.LOAD_VAR, 0, match_value_index)
        builtin_index = self.builtin_indices['list?']
        ctx.emit(Opcode.CALL_BUILTIN, builtin_index, 1)
        fail_jump = ctx.emit(Opcode.POP_JUMP_IF_FALSE, 0)

        # Check not empty
        ctx.emit(Opcode.LOAD_VAR, 0, match_value_index)
        builtin_index = self.builtin_indices['null?']
        ctx.emit(Opcode.CALL_BUILTIN, builtin_index, 1)
        empty_fail_jump = ctx.emit(Opcode.POP_JUMP_IF_TRUE, 0)  # Jump to fail if empty

        # Check list has enough elements for the head patterns
        # For pattern (a b . rest), we need at least 2 elements
        if dot_position > 0:
            ctx.emit(Opcode.LOAD_VAR, 0, match_value_index)
            builtin_index = self.builtin_indices['length']
            ctx.emit(Opcode.CALL_BUILTIN, builtin_index, 1)
            ctx.emit(Opcode.LOAD_CONST, ctx.add_constant(AIFPLNumber(dot_position)))
            builtin_index = self.builtin_indices['>=']
            ctx.emit(Opcode.CALL_BUILTIN, builtin_index, 2)
            length_fail_jump = ctx.emit(Opcode.POP_JUMP_IF_FALSE, 0)
        else:
            length_fail_jump = None

        # Extract head elements (before dot)
        fail_jumps = []
        for i in range(dot_position):
            elem_pattern = pattern.elements[i]

            # Extract element into temporary
            elem_temp_index = ctx.current_scope().add_binding(f"<cons-elem-temp-{i}>")
            ctx.update_max_locals()

            ctx.emit(Opcode.LOAD_VAR, 0, match_value_index)
            ctx.emit(Opcode.LOAD_CONST, ctx.add_constant(AIFPLNumber(i)))
            builtin_index = self.builtin_indices['list-ref']
            ctx.emit(Opcode.CALL_BUILTIN, builtin_index, 2)
            ctx.emit(Opcode.STORE_VAR, 0, elem_temp_index)

            # Recursively match the element pattern
            self._compile_pattern(elem_pattern, elem_temp_index, ctx)
            elem_fail = ctx.emit(Opcode.POP_JUMP_IF_FALSE, 0)
            fail_jumps.append(elem_fail)

        # Extract tail (after dot)
        tail_pattern = pattern.elements[dot_position + 1]

        # Extract tail into temporary
        tail_temp_index = ctx.current_scope().add_binding("<cons-tail-temp>")
        ctx.update_max_locals()

        ctx.emit(Opcode.LOAD_CONST, ctx.add_constant(AIFPLNumber(dot_position)))
        ctx.emit(Opcode.LOAD_VAR, 0, match_value_index)
        builtin_index = self.builtin_indices['drop']
        ctx.emit(Opcode.CALL_BUILTIN, builtin_index, 2)
        ctx.emit(Opcode.STORE_VAR, 0, tail_temp_index)

        # Recursively match the tail pattern
        self._compile_pattern(tail_pattern, tail_temp_index, ctx)
        tail_fail = ctx.emit(Opcode.POP_JUMP_IF_FALSE, 0)
        fail_jumps.append(tail_fail)

        # Success
        ctx.emit(Opcode.LOAD_TRUE)
        success_jump = ctx.emit(Opcode.JUMP, 0)

        # Patch failure jumps
        fail_location = ctx.current_instruction_index()
        ctx.patch_jump(fail_jump, fail_location)
        ctx.patch_jump(empty_fail_jump, fail_location)
        if length_fail_jump is not None:
            ctx.patch_jump(length_fail_jump, fail_location)
        for fj in fail_jumps:
            ctx.patch_jump(fj, fail_location)
        ctx.emit(Opcode.LOAD_FALSE)

        success_location = ctx.current_instruction_index()
        ctx.patch_jump(success_jump, success_location)

    def _validate_pattern_syntax(self, pattern: AIFPLValue) -> None:
        """Validate pattern syntax before compilation."""
        # Literals are always valid
        if isinstance(pattern, (AIFPLNumber, AIFPLString, AIFPLBoolean)):
            return

        # Variable patterns (symbols)
        if isinstance(pattern, AIFPLSymbol):
            return

        if isinstance(pattern, AIFPLList):
            self._validate_list_pattern_syntax(pattern)
            return

        raise AIFPLEvalError(f"Invalid pattern type: {type(pattern).__name__}")

    def _validate_list_pattern_syntax(self, pattern: AIFPLList) -> None:
        """Validate list pattern syntax."""
        # Empty list is valid
        if pattern.is_empty():
            return

        # Check for type patterns: (type? var)
        if (len(pattern.elements) == 2 and
            isinstance(pattern.elements[0], AIFPLSymbol) and
            pattern.elements[0].name.endswith('?')):

            type_pred = pattern.elements[0].name
            var_pattern = pattern.elements[1]

            # Validate type predicate is known
            valid_predicates = {
                'number?', 'integer?', 'float?', 'complex?',
                'string?', 'boolean?', 'list?', 'alist?', 'function?', 'symbol?'
            }

            if type_pred not in valid_predicates:
                raise AIFPLEvalError(
                    message="Invalid type pattern",
                    received=f"Type pattern: ({type_pred} ...)",
                    expected="Valid type predicate like number?, string?, list?, etc.",
                    example="(number? n) or (string? s)",
                    suggestion="Use a valid type predicate ending with ?"
                )

            # Variable in type pattern must be a symbol
            if not isinstance(var_pattern, AIFPLSymbol):
                raise AIFPLEvalError(
                    message="Pattern variable must be a symbol",
                    received=f"Variable in type pattern: {var_pattern} ({var_pattern.type_name()})",
                    expected="Symbol (variable name)",
                    example="(number? x) not (number? 42)",
                    suggestion="Use unquoted variable names in type patterns"
                )

            return

        # Check for malformed type patterns (wrong number of arguments)
        first_elem = pattern.elements[0]
        if isinstance(first_elem, AIFPLSymbol):
            valid_predicates = {
                'number?', 'integer?', 'float?', 'complex?',
                'string?', 'boolean?', 'list?', 'alist?', 'function?', 'symbol?'
            }

            if first_elem.name in valid_predicates:
                type_pred = first_elem.name

                # Missing variable: (number?)
                if len(pattern.elements) == 1:
                    raise AIFPLEvalError(
                        message="Invalid type pattern",
                        received=f"Type pattern: ({type_pred}) - missing variable",
                        expected="Type pattern with variable: (type? var)",
                        example="(number? x) not (number?)",
                        suggestion="Add a variable name after the type predicate"
                    )

                # Too many variables: (number? x y)
                if len(pattern.elements) > 2:
                    raise AIFPLEvalError(
                        message="Invalid type pattern",
                        received=f"Type pattern: {pattern} - too many variables",
                        expected="Type pattern with one variable: (type? var)",
                        example="(number? x) not (number? x y)",
                        suggestion="Use only one variable in type patterns"
                    )

        # Comprehensive dot pattern validation
        dot_positions = []
        for i, element in enumerate(pattern.elements):
            if isinstance(element, AIFPLSymbol) and element.name == ".":
                dot_positions.append(i)

        if len(dot_positions) > 1:
            raise AIFPLEvalError(
                message="Invalid cons pattern",
                received=f"Pattern: {pattern} - multiple dots",
                expected="At most one dot in cons pattern",
                example="(head . tail) or (a b . rest) not (a . b . c)",
                suggestion="Use only one dot to separate head from tail"
            )

        if len(dot_positions) == 1:
            dot_pos = dot_positions[0]

            # Dot at beginning: (. a b)
            if dot_pos == 0:
                raise AIFPLEvalError(
                    message="Invalid cons pattern",
                    received=f"Pattern: {pattern} - dot at beginning",
                    expected="Elements before dot in cons pattern",
                    example="(head . tail) or (a b . rest) not (. a b)",
                    suggestion="Put at least one element before the dot"
                )

            # Dot at end: (a b .)
            if dot_pos == len(pattern.elements) - 1:
                raise AIFPLEvalError(
                    message="Invalid cons pattern",
                    received=f"Pattern: {pattern} - dot at end",
                    expected="Pattern like (head . tail) or (a b . rest)",
                    example="(head . tail) or (first second . rest)",
                    suggestion="Put a tail pattern after the dot"
                )

            # Multiple elements after dot: (a . b c)
            if dot_pos != len(pattern.elements) - 2:
                raise AIFPLEvalError(
                    message="Invalid cons pattern",
                    received=f"Pattern: {pattern} - multiple elements after dot",
                    expected="Pattern like (head . tail) or (a b . rest)",
                    example="(head . tail) or (first second . rest)",
                    suggestion="Use only one tail variable after the dot"
                )

        # Recursively validate all elements (except dots which are structural)
        for element in pattern.elements:
            # Skip dot symbols - they're structural, not patterns
            if isinstance(element, AIFPLSymbol) and element.name == ".":
                continue

            self._validate_pattern_syntax(element)

    def format_result(self, result: AIFPLValue) -> str:
        """
        Format result for display, using LISP conventions for lists and booleans.

        Args:
            result: The result to format

        Returns:
            String representation of the result
        """
        if isinstance(result, AIFPLBoolean):
            return "#t" if result.value else "#f"

        if isinstance(result, AIFPLString):
            escaped_content = self._escape_string_for_lisp(result.value)
            return f'"{escaped_content}"'

        if isinstance(result, AIFPLNumber):
            if isinstance(result.value, float):
                nice_number = self._is_close_to_nice_number(result.value)
                if nice_number is not None:
                    # If it's close to an integer, show as integer
                    if nice_number == int(nice_number):
                        return str(int(nice_number))

                    return str(nice_number)

            return str(result.value)

        if isinstance(result, AIFPLList):
            # Format list in LISP notation: (element1 element2 ...)
            if result.is_empty():
                return "()"

            formatted_elements = []
            for element in result.elements:
                formatted_elements.append(self.format_result(element))

            return f"({' '.join(formatted_elements)})"

        if isinstance(result, AIFPLAList):
            # Format alist in LISP notation: (alist (key1 val1) (key2 val2) ...)
            if result.is_empty():
                return "(alist)"

            formatted_pairs = []
            for key, value in result.pairs:
                formatted_key = self.format_result(key)
                formatted_value = self.format_result(value)
                formatted_pairs.append(f"({formatted_key} {formatted_value})")

            pairs_str = ' '.join(formatted_pairs)
            return f"(alist {pairs_str})"

        if isinstance(result, AIFPLFunction):
            # Format lambda functions
            param_str = " ".join(result.parameters)
            return f"<lambda ({param_str})>"

        if isinstance(result, AIFPLBuiltinFunction):
            # Format builtin functions
            return f"<builtin {result.name}>"

        # For other types, use standard string representation
        return str(result)

    def _escape_string_for_lisp(self, s: str) -> str:
        """Escape a string for LISP display format."""
        result = []
        for char in s:
            if char == '"':
                result.append('\\"')

            elif char == '\\':
                result.append('\\\\')

            elif char == '\n':
                result.append('\\n')

            elif char == '\t':
                result.append('\\t')

            elif char == '\r':
                result.append('\\r')

            elif ord(char) < 32:  # Other control characters
                result.append(f'\\u{ord(char):04x}')

            else:
                result.append(char)  # Keep Unicode as-is

        return ''.join(result)

    def _is_close_to_nice_number(self, value: float) -> float | None:
        """Check if a float is very close to a 'nice' number and return the nice number if so."""
        # Check if it's close to common fractions with small denominators
        for denominator in range(1, 11):  # Check denominators 1-10
            for numerator in range(-50, 51):  # Check reasonable range
                nice_value = numerator / denominator
                # Use a small tolerance for floating point comparison
                if abs(value - nice_value) < 1e-10:
                    return nice_value

        return None
