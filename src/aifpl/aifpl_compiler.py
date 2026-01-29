"""AIFPL bytecode compiler - compiles AST to bytecode."""

from typing import List, Dict, Tuple, Set, cast
from dataclasses import dataclass, field

from aifpl.aifpl_bytecode import CodeObject, Instruction, Opcode, make_instruction
from aifpl.aifpl_dependency_analyzer import AIFPLDependencyAnalyzer
from aifpl.aifpl_desugarer import AIFPLDesugarer
from aifpl.aifpl_error import AIFPLEvalError
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
class CompilationContext:
    """
    Compilation context tracking scopes and code generation.

    Maintains a stack of scopes for lexical addressing.
    """
    scopes: List[CompilationScope] = field(default_factory=lambda: [CompilationScope()])
    constants: List[AIFPLValue] = field(default_factory=list)
    names: List[str] = field(default_factory=list)
    constant_map: Dict[AIFPLValue, int] = field(default_factory=dict)  # Fast lookup for constants
    name_map: Dict[str, int] = field(default_factory=dict)  # Fast lookup for names
    code_objects: List[CodeObject] = field(default_factory=list)
    instructions: List[Instruction] = field(default_factory=list)
    max_locals: int = 0  # Track maximum locals needed
    parent_ctx: 'CompilationContext | None' = None  # Parent context for nested lambdas
    sibling_bindings: List[str] = field(default_factory=list)  # Sibling bindings for mutual recursion
    next_local_index: int = 0  # Global counter for local variable indices (for flat indexing)
    in_tail_position: bool = False  # Whether we're compiling in tail position
    current_function_name: str | None = None  # Name of the function being compiled (for tail recursion detection)

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
        # For flat indexing, max_locals is just the highest index we've used
        self.max_locals = max(self.max_locals, self.next_local_index)

    def allocate_local_index(self) -> int:
        """Allocate a new local variable index from the global counter."""
        index = self.next_local_index
        self.next_local_index += 1
        return index

    def current_scope(self) -> CompilationScope:
        """Get current scope."""
        return self.scopes[-1]

    def add_constant(self, value: AIFPLValue) -> int:
        """Add constant to pool and return its index."""
        # Use dict lookup for O(1) performance instead of list.index() which is O(n)
        if value in self.constant_map:
            return self.constant_map[value]

        # Not found - add new constant
        index = len(self.constants)
        self.constants.append(value)
        self.constant_map[value] = index
        return index

    def add_name(self, name: str) -> int:
        """Add name to pool and return its index."""
        # Use dict lookup for O(1) performance instead of list.index() which is O(n)
        if name in self.name_map:
            return self.name_map[name]

        # Not found - add new name
        index = len(self.names)
        self.names.append(name)
        self.name_map[name] = index
        return index

    def add_code_object(self, code_obj: CodeObject) -> int:
        """Add nested code object and return its index."""
        index = len(self.code_objects)
        self.code_objects.append(code_obj)
        return index

    def resolve_variable(self, name: str) -> Tuple[str, int, int]:
        """
        Resolve variable to (type, depth, index).

        With global index allocation, the index stored in each binding is already
        the final flat index. We just need to find which scope has the binding.

        Returns:
            ('local', depth, index) for local variables
            ('global', 0, name_index) for global variables
        """
        # Search from innermost to outermost scope
        for scope in reversed(self.scopes):
            index = scope.get_binding(name)
            if index is not None:
                # Index is already the global flat index
                return ('local', 0, index)

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

    def __init__(self) -> None:
        """Initialize compiler.
        """
        self.builtin_indices = {name: i for i, name in enumerate(self.BUILTIN_TABLE)}
        self.desugarer = AIFPLDesugarer()

    def compile(self, expr: AIFPLValue, name: str = "<module>") -> CodeObject:
        """
        Compile an AIFPL expression to bytecode.

        Args:
            expr: AST to compile
            name: Name for the code object (for debugging)

        Returns:
            Compiled code object
        """
        # Desugar the expression first
        expr = self.desugarer.desugar(expr)

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

        # Cache the type - check once instead of multiple isinstance() calls
        # Using type() + 'is' is faster than isinstance() for exact type matches
        expr_type = type(expr)

        # Self-evaluating values
        if expr_type in (AIFPLInteger, AIFPLFloat, AIFPLComplex, AIFPLString):
            const_index = ctx.add_constant(expr)
            ctx.emit(Opcode.LOAD_CONST, const_index)
            return

        if expr_type is AIFPLBoolean:
            if cast(AIFPLBoolean, expr).value:
                ctx.emit(Opcode.LOAD_TRUE)
                return

            ctx.emit(Opcode.LOAD_FALSE)
            return

        # Symbol (variable reference)
        if expr_type is AIFPLSymbol:
            self._compile_variable_load(cast(AIFPLSymbol, expr).name, ctx)
            return

        # List (function call or special form)
        if expr_type is AIFPLList:
            self._compile_list(cast(AIFPLList, expr), ctx)
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

        # Cache type check for the first element
        first_type = type(first)

        # Check for special forms
        if first_type is AIFPLSymbol:
            name = cast(AIFPLSymbol, first).name
            if name == 'if':
                self._compile_if(expr, ctx)
                return

            if name == 'let':
                self._compile_let(expr, ctx)
                return

            if name == 'letrec':
                self._compile_letrec(expr, ctx)
                return

            if name == 'lambda':
                self._compile_lambda(expr, ctx)
                return

            if name == 'and':
                self._compile_and(expr, ctx)
                return

            if name == 'or':
                self._compile_or(expr, ctx)
                return

            if name == 'quote':
                # Quote: return the quoted value as a constant
                # Validation already done by semantic analyzer
                assert len(expr.elements) == 2, "Quote expression should have exactly 2 elements (validated by semantic analyzer)"

                quoted = expr.elements[1]
                const_index = ctx.add_constant(quoted)
                ctx.emit(Opcode.LOAD_CONST, const_index)
                return

            if name == 'error':
                # Special form for raising errors (generated by desugarer)
                # (error "message") -> RAISE_ERROR
                if len(expr.elements) != 2:
                    raise AIFPLEvalError(
                        message="Error expression has wrong number of arguments",
                        received=f"Got {len(expr.elements) - 1} arguments",
                        expected="Exactly 1 argument: (error \"message\")"
                    )

                error_msg = expr.elements[1]
                const_index = ctx.add_constant(error_msg)
                ctx.emit(Opcode.RAISE_ERROR, const_index)
                return

        # Regular function call
        self._compile_function_call(expr, ctx)

    def _compile_if(self, expr: AIFPLList, ctx: CompilationContext) -> None:
        """Compile if expression: (if condition then else)"""
        # Validation already done by semantic analyzer
        assert len(expr.elements) == 4, "If expression should have exactly 4 elements (validated by semantic analyzer)"

        _, condition, then_expr, else_expr = expr.elements

        # Compile condition
        # Condition is NOT in tail position
        old_tail = ctx.in_tail_position
        ctx.in_tail_position = False
        self._compile_expression(condition, ctx)
        ctx.in_tail_position = old_tail

        # Jump to else if condition is false
        jump_to_else = ctx.emit(Opcode.JUMP_IF_FALSE, 0)  # Will patch later

        # Compile then branch
        # Then branch IS in tail position if the if is
        self._compile_expression(then_expr, ctx)

        # Jump past else branch
        # If we're in tail position, emit RETURN after then branch
        # Otherwise, emit JUMP to skip else branch
        jump_past_else = None
        if ctx.in_tail_position:
            # Then branch is in tail position, so emit RETURN
            ctx.emit(Opcode.RETURN)

        else:
            # Not in tail position, emit jump past else
            jump_past_else = ctx.emit(Opcode.JUMP, 0)  # Will patch later

        # Patch jump to else
        else_start = ctx.current_instruction_index()
        ctx.patch_jump(jump_to_else, else_start)

        # Compile else branch
        # Else branch IS in tail position if the if is
        self._compile_expression(else_expr, ctx)

        # Patch jump past else
        if jump_past_else is not None:
            after_else = ctx.current_instruction_index()
            ctx.patch_jump(jump_past_else, after_else)

    def _compile_and(self, expr: AIFPLList, ctx: CompilationContext) -> None:
        """
        Compile and expression with short-circuit evaluation.

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
            jump = ctx.emit(Opcode.JUMP_IF_FALSE, 0)
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
        """
        Compile or expression with short-circuit evaluation.

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
            jump = ctx.emit(Opcode.JUMP_IF_TRUE, 0)
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

    def _compile_let(self, expr: AIFPLList, ctx: CompilationContext) -> None:
        """
        Compile let expression with simple sequential binding: (let ((var val) ...) body)

        Bindings are evaluated left-to-right, with each able to reference previous bindings.
        Self-references see the outer scope (shadowing works).
        """
        # Validation already done by semantic analyzer
        assert len(expr.elements) == 3, "Let expression should have exactly 3 elements (validated by semantic analyzer)"

        _, bindings_list, body = expr.elements[0], expr.elements[1], expr.elements[2]

        assert isinstance(bindings_list, AIFPLList), "Binding list should be a list (validated by semantic analyzer)"

        # Push a new scope for let bindings
        ctx.push_scope()

        # Parse bindings (already validated by semantic analyzer)
        binding_pairs = []
        for i, binding in enumerate(bindings_list.elements):
            assert isinstance(binding, AIFPLList) and len(binding.elements) == 2, \
                f"Binding {i+1} should be a list with 2 elements (validated by semantic analyzer)"

            name_expr, value_expr = binding.elements
            assert isinstance(name_expr, AIFPLSymbol), f"Binding {i+1} variable should be a symbol (validated by semantic analyzer)"

            binding_pairs.append((name_expr.name, value_expr))

        # Simple sequential evaluation - compile and store each binding in order
        for name, value_expr in binding_pairs:
            # Compile value expression FIRST (before adding binding to scope)
            # This ensures self-references see the outer scope (shadowing works)
            old_tail = ctx.in_tail_position
            ctx.in_tail_position = False
            self._compile_expression(value_expr, ctx)
            ctx.in_tail_position = old_tail

            # NOW add binding to scope (after evaluating the value)
            # This makes it available for subsequent bindings
            var_index = ctx.allocate_local_index()
            ctx.current_scope().add_binding(name, var_index)
            ctx.update_max_locals()

            # Store in local variable (depth=0 for current frame)
            ctx.emit(Opcode.STORE_VAR, 0, var_index)

        # Compile body
        self._compile_expression(body, ctx)

        # Pop the scope
        ctx.pop_scope()

    def _compile_letrec(self, expr: AIFPLList, ctx: CompilationContext) -> None:
        """
        Compile letrec expression with recursive binding: (letrec ((var val) ...) body)

        All bindings can reference themselves and each other (mutual recursion).
        Uses dependency analysis and placeholders to support this.
        """
        # Validation already done by semantic analyzer
        assert len(expr.elements) == 3, "Letrec expression should have exactly 3 elements (validated by semantic analyzer)"

        _, bindings_list, body = expr.elements[0], expr.elements[1], expr.elements[2]

        assert isinstance(bindings_list, AIFPLList), "Binding list should be a list (validated by semantic analyzer)"

        # Push a new scope for letrec bindings
        ctx.push_scope()

        # First pass: Add all binding names to scope (for recursive references) - already validated
        binding_pairs = []
        for i, binding in enumerate(bindings_list.elements):
            assert isinstance(binding, AIFPLList) and len(binding.elements) == 2, \
                f"Binding {i+1} should be a list with 2 elements (validated by semantic analyzer)"

            name_expr, value_expr = binding.elements
            assert isinstance(name_expr, AIFPLSymbol), f"Binding {i+1} variable should be a symbol (validated by semantic analyzer)"

            binding_pairs.append((name_expr.name, value_expr))
            # Add binding to scope NOW so recursive lambdas can reference it
            var_index = ctx.allocate_local_index()
            ctx.current_scope().add_binding(name_expr.name, var_index)

        # Update max locals after adding all bindings
        ctx.update_max_locals()

        # Analyze dependencies for letrec (supports mutual recursion)
        analyzer = AIFPLDependencyAnalyzer()
        binding_groups = analyzer.analyze_letrec_bindings(binding_pairs)

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
                raise AIFPLEvalError("Circular dependency detected in letrec bindings")

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
                    ctx,
                    name,
                    value_expr,
                    is_recursive_group=group.is_recursive,
                    group_names=group_names,
                    mutual_recursion_patches=mutual_recursion_patches
                )

        # Third pass: Patch all mutual recursion references
        # Now all lambdas have been stored, so we can safely reference them
        for closure_var_index, sibling_name, sibling_var_index in mutual_recursion_patches:
            name_index = ctx.add_name(sibling_name)
            # We need to pass 3 pieces of info but only have 2 args
            # Solution: Store (sibling_var_index, name_index) as a tuple in constants
            # and pass closure_var_index and const_index
            patch_info = AIFPLList((AIFPLInteger(sibling_var_index), AIFPLInteger(name_index)))
            const_index = ctx.add_constant(patch_info)
            # arg1=closure_var_index, arg2=const_index (contains sibling_var_index and name_index)
            ctx.emit(Opcode.PATCH_CLOSURE_SIBLING, closure_var_index, const_index)

        # Compile body
        self._compile_expression(body, ctx)

        # Pop the scope to remove let bindings
        # Note: We don't emit any cleanup instructions because the VM uses flat indexing
        ctx.pop_scope()

    def _compile_single_let_binding(
        self,
        ctx: CompilationContext,
        name: str,
        value_expr: AIFPLValue,
        is_recursive_group: bool,
        group_names: List[str],
        mutual_recursion_patches: List[Tuple[int, str, int]]
    ) -> None:
        """Compile a single let binding."""
        # Get the index for this variable
        _, depth, var_index = ctx.resolve_variable(name)

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
            # Binding values are NOT in tail position
            old_tail = ctx.in_tail_position
            ctx.in_tail_position = False
            self._compile_expression(value_expr, ctx)
            ctx.in_tail_position = old_tail

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
        expr_type = type(expr)

        if expr_type is AIFPLSymbol:
            return cast(AIFPLSymbol, expr).name == var_name

        if expr_type is AIFPLList:
            return any(self._references_variable(elem, var_name) for elem in cast(AIFPLList, expr).elements)

        return False

    def _compile_lambda(self, expr: AIFPLList, ctx: CompilationContext,
                       binding_name: str | None = None, let_bindings: List[str] | None = None) -> None:
        """Compile lambda expression: (lambda (params...) body)"""
        # Validation already done by semantic analyzer
        assert len(expr.elements) == 3, "Lambda expression should have exactly 3 elements (validated by semantic analyzer)"

        _, params_list, body = expr.elements

        assert isinstance(params_list, AIFPLList), "Parameter list should be a list (validated by semantic analyzer)"

        # Extract parameter names (already validated)
        param_names = []
        for i, param in enumerate(params_list.elements):
            assert isinstance(param, AIFPLSymbol), f"Parameter {i+1} should be a symbol (validated by semantic analyzer)"
            param_names.append(param.name)

        # Duplicate parameters already checked by semantic analyzer

        # Find free variables (variables used in body but not parameters)
        bound_vars = set(param_names)
        free_vars = self._find_free_variables(body, bound_vars, ctx)

        # Emit instructions to load free variables onto stack (for capture).
        # Only capture variables from outer scopes, not self-references in current scope.
        captured_vars = []
        for free_var in free_vars:
            var_type, depth, index = ctx.resolve_variable(free_var)
            if var_type == 'local':
                # Check if this is a self-reference or sibling
                if free_var == binding_name:
                    # Self-reference - don't capture, will be patched later
                    continue

                if let_bindings and free_var in let_bindings:
                    # Sibling in the same recursive group - don't capture, will be patched later
                    # This handles mutual recursion where siblings reference each other
                    continue

                # Variable from an outer scope - capture it
                ctx.emit(Opcode.LOAD_VAR, depth, index)
                captured_vars.append(free_var)


        # Create new compilation context for lambda body
        lambda_ctx = CompilationContext()
        lambda_ctx.push_scope()

        # Add parameters to lambda scope
        for param_name in param_names:
            param_index = lambda_ctx.allocate_local_index()
            lambda_ctx.current_scope().add_binding(param_name, param_index)

        # Add captured free variables to lambda scope
        for free_var in captured_vars:
            free_var_index = lambda_ctx.allocate_local_index()
            lambda_ctx.current_scope().add_binding(free_var, free_var_index)

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

        # Set current function name for tail recursion detection
        # Use binding_name if available (for named functions in let), otherwise None
        lambda_ctx.current_function_name = binding_name

        # Update max locals for the lambda (only its own locals, not parent's)
        lambda_ctx.update_max_locals()

        # Generate function prologue: pop arguments from stack into locals
        # Arguments are on stack in order: arg0 at bottom, argN at top
        # Stack is LIFO, so we pop in reverse: argN first, then arg(N-1), ..., arg0
        # Store them in locals: argN -> local(N-1), ..., arg0 -> local(0)
        # This means we emit STORE_VAR in reverse order
        for i in range(len(param_names) - 1, -1, -1):
            # STORE_VAR depth=0 (current frame), index=i (parameter position)
            lambda_ctx.emit(Opcode.STORE_VAR, 0, i)

        # Compile lambda body - free vars will be resolved as locals
        # The body is in tail position (last expression before RETURN)
        old_tail_position = lambda_ctx.in_tail_position
        lambda_ctx.in_tail_position = True
        self._compile_expression(body, lambda_ctx)
        lambda_ctx.in_tail_position = old_tail_position

        # Only emit RETURN if the body didn't already emit one (or JUMP)
        # Check if the last instruction is RETURN or JUMP
        if lambda_ctx.instructions:
            last_op = lambda_ctx.instructions[-1].opcode
            if last_op not in (Opcode.RETURN, Opcode.JUMP):
                lambda_ctx.emit(Opcode.RETURN)

        else:
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

    def _collect_free_vars(self, expr: AIFPLValue, bound_vars: Set[str],
                          parent_ctx: CompilationContext, free: List[str],
                          seen: Set[str]) -> None:
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

    def _compile_function_call(self, expr: AIFPLList, ctx: CompilationContext) -> None:
        """Compile a function call."""
        func_expr = expr.first()
        arg_exprs = list(expr.elements[1:])

        # Cache type check for function expression
        func_type = type(func_expr)

        # Check if calling a known builtin
        if func_type is AIFPLSymbol and cast(AIFPLSymbol, func_expr).name in self.builtin_indices:
            # Compile arguments
            # Arguments are NOT in tail position
            old_tail = ctx.in_tail_position
            ctx.in_tail_position = False
            for arg in arg_exprs:
                self._compile_expression(arg, ctx)

            ctx.in_tail_position = old_tail

            # Emit builtin call
            builtin_index = self.builtin_indices[cast(AIFPLSymbol, func_expr).name]
            ctx.emit(Opcode.CALL_BUILTIN, builtin_index, len(arg_exprs))
            return

        # General function call

        # Check for tail-recursive call
        # If we're in tail position and calling the current function by name, use JUMP
        is_tail_recursive = (
            ctx.in_tail_position and
            ctx.current_function_name is not None and
            func_type is AIFPLSymbol and
            cast(AIFPLSymbol, func_expr).name == ctx.current_function_name
        )

        if is_tail_recursive:
            # Tail recursion! Compile arguments and jump to start
            # Don't compile the function expression - we're not calling, we're jumping
            for arg in arg_exprs:
                # Arguments are not in tail position
                old_tail = ctx.in_tail_position
                ctx.in_tail_position = False
                self._compile_expression(arg, ctx)
                ctx.in_tail_position = old_tail

            # Emit JUMP to instruction 0 (start of function, after prologue)
            # The prologue will pop these args from the stack
            ctx.emit(Opcode.JUMP, 0)
            # No RETURN needed - we're jumping, not returning
            return

        # Not tail recursive - normal function call
        if func_type is AIFPLList and cast(AIFPLList, func_expr).elements:
            first_elem = cast(AIFPLList, func_expr).first()
            if isinstance(first_elem, AIFPLSymbol) and first_elem.name == 'lambda':
                # Direct lambda call: ((lambda (x y) ...) arg1 arg2)
                # Extract parameter list
                if len(cast(AIFPLList, func_expr).elements) >= 2 and isinstance(cast(AIFPLList, func_expr).elements[1], AIFPLList):
                    param_list = cast(AIFPLList, cast(AIFPLList, func_expr).elements[1])
                    expected_arity = len(param_list.elements)
                    actual_arity = len(arg_exprs)

                    if expected_arity != actual_arity:
                        arguments_text = f"argument{'s' if expected_arity != 1 else ''}"
                        raise AIFPLEvalError(
                            message=f"Lambda expects {expected_arity} {arguments_text}, got {actual_arity}",
                            expected=f"Lambda parameters: {expected_arity}",
                            received=f"Arguments provided: {actual_arity}",
                            suggestion=f"Provide exactly {expected_arity} {arguments_text}"
                        )

        self._compile_expression(func_expr, ctx)

        # Compile arguments
        # Arguments are NOT in tail position
        old_tail = ctx.in_tail_position
        ctx.in_tail_position = False
        for arg in arg_exprs:
            self._compile_expression(arg, ctx)

        ctx.in_tail_position = old_tail

        # Emit call
        ctx.emit(Opcode.CALL_FUNCTION, len(arg_exprs))
