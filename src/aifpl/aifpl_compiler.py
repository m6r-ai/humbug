"""AIFPL bytecode compiler - compiles AST to bytecode."""

from typing import List, Dict, Tuple, Optional, Set
from dataclasses import dataclass, field

from aifpl.aifpl_value import (
    AIFPLValue, AIFPLNumber, AIFPLString, AIFPLBoolean, 
    AIFPLSymbol, AIFPLList
)
from aifpl.aifpl_bytecode import (
    CodeObject, Instruction, Opcode, make_instruction
)
from aifpl.aifpl_error import AIFPLEvalError


@dataclass
class CompilationScope:
    """Tracks variable bindings in a lexical scope.

    Maps variable names to their index within the scope.
    """
    bindings: Dict[str, int] = field(default_factory=dict)

    def add_binding(self, name: str) -> int:
        """Add a binding and return its index."""
        index = len(self.bindings)
        self.bindings[name] = index
        return index

    def get_binding(self, name: str) -> Optional[int]:
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
        total_locals = sum(len(scope.bindings) for scope in self.scopes)
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
        'list', 'cons', 'append', 'reverse', 'first', 'rest', 'length', 'last',
        'member?', 'null?', 'position', 'take', 'drop', 'remove',
        'map', 'filter', 'fold', 'range',
        'string-append', 'string-length', 'string-upcase', 'string-downcase',
        'string-trim', 'string-replace', 'string-split', 'string-join',
        'string-contains?', 'string-prefix?', 'string-suffix?', 'string-ref',
        'substring', 'string->number', 'number->string',
        'alist', 'alist-get', 'alist-set', 'alist-remove', 'alist-has?',
        'alist-keys', 'alist-values', 'alist-merge', 'alist?',
        'sqrt', 'abs', 'min', 'max', 'pow',
    ]

    def __init__(self):
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
            else:
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
            ctx.emit(Opcode.LOAD_LOCAL, depth, index)
        else:  # global
            ctx.emit(Opcode.LOAD_GLOBAL, index)

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
            elif first.name == 'let':
                self._compile_let(expr, ctx)
                return
            elif first.name == 'lambda':
                self._compile_lambda(expr, ctx)
                return
            elif first.name == 'quote':
                # Quote: return the quoted value as a constant
                if len(expr.elements) != 2:
                    raise AIFPLEvalError("quote requires exactly 1 argument")
                quoted = expr.elements[1]
                const_index = ctx.add_constant(quoted)
                ctx.emit(Opcode.LOAD_CONST, const_index)
                return
            elif first.name == 'alist':
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
            elif first.name in ['map', 'filter', 'fold', 'range', 'find', 'any?', 'all?']:
                # These are special forms - treat as regular calls for now
                # The VM handles them
                self._compile_function_call(expr, ctx)
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

    def _compile_let(self, expr: AIFPLList, ctx: CompilationContext, 
                    current_binding_name: str = None) -> None:
        """Compile let expression: (let ((var val) ...) body)"""
        if len(expr.elements) < 3:
            raise AIFPLEvalError("let requires bindings and body")

        _, bindings_list, body = expr.elements[0], expr.elements[1], expr.elements[2]

        if not isinstance(bindings_list, AIFPLList):
            raise AIFPLEvalError("let bindings must be a list")

        # Enter new scope for tracking variables
        ctx.push_scope()

        # First pass: Add all binding names to scope (for recursive references)
        binding_pairs = []
        for binding in bindings_list.elements:
            if not isinstance(binding, AIFPLList) or len(binding.elements) != 2:
                raise AIFPLEvalError("Each let binding must be (name value)")

            name_expr, value_expr = binding.elements
            if not isinstance(name_expr, AIFPLSymbol):
                raise AIFPLEvalError("Binding name must be a symbol")

            binding_pairs.append((name_expr.name, value_expr))
            # Add binding to scope NOW so recursive lambdas can reference it
            ctx.current_scope().add_binding(name_expr.name)

        # Update max locals after adding all bindings
        ctx.update_max_locals()

        # Second pass: Compile values and store them
        for i, (name, value_expr) in enumerate(binding_pairs):
            # Get the index for this variable
            # Use resolve_variable to get the correct flat index
            var_type, depth, var_index = ctx.resolve_variable(name)
            
            # Check if this is a self-referential lambda
            is_self_ref_lambda = (isinstance(value_expr, AIFPLList) and 
                                 not value_expr.is_empty() and
                                 isinstance(value_expr.first(), AIFPLSymbol) and
                                 value_expr.first().name == 'lambda')

            # Check if lambda references itself (recursive)
            is_recursive = False
            if is_self_ref_lambda:
                # Check if the lambda body references 'name'
                is_recursive = self._references_variable(value_expr, name)

            # Compile the value expression
            # Pass the binding name if it's a lambda (for self-reference detection)
            if is_self_ref_lambda:
                self._compile_lambda(value_expr, ctx, binding_name=name)
            else:
                self._compile_expression(value_expr, ctx)

            # Store in local variable
            ctx.emit(Opcode.STORE_LOCAL, depth, var_index)

            # For recursive closures, patch them to reference themselves
            if is_recursive:
                # PATCH_CLOSURE_SELF: arg1 = name index, arg2 = var index (within current scope)
                # The VM will need to use depth=0 since we just stored there
                name_index = ctx.add_name(name)
                ctx.emit(Opcode.PATCH_CLOSURE_SELF, name_index, var_index)

        # Compile body
        self._compile_expression(body, ctx)

        # Exit scope (for variable tracking)
        ctx.pop_scope()

    def _references_variable(self, expr: AIFPLValue, var_name: str) -> bool:
        """Check if an expression references a variable."""
        if isinstance(expr, AIFPLSymbol):
            return expr.name == var_name
        elif isinstance(expr, AIFPLList):
            return any(self._references_variable(elem, var_name) for elem in expr.elements)
        else:
            return False

    def _compile_lambda(self, expr: AIFPLList, ctx: CompilationContext,
                       binding_name: str = None) -> None:
        """Compile lambda expression: (lambda (params...) body)"""
        if len(expr.elements) != 3:
            raise AIFPLEvalError("lambda requires parameters and body")

        _, params_list, body = expr.elements

        if not isinstance(params_list, AIFPLList):
            raise AIFPLEvalError("lambda parameters must be a list")

        # Extract parameter names
        param_names = []
        for param in params_list.elements:
            if not isinstance(param, AIFPLSymbol):
                raise AIFPLEvalError("lambda parameter must be a symbol")
            param_names.append(param.name)

        # Find free variables (variables used in body but not parameters)
        bound_vars = set(param_names)
        free_vars = self._find_free_variables(body, bound_vars, ctx)
        
        # Emit instructions to load free variables onto stack (for capture).
        # Only capture variables from outer scopes, not self-references in current scope.
        captured_vars = []
        for free_var in free_vars:
            var_type, depth, index = ctx.resolve_variable(free_var)
            if var_type == 'local':
                # Check if this variable is a self-reference (the binding we're currently compiling)
                if free_var != binding_name:
                    # Not a self-reference - capture it
                    ctx.emit(Opcode.LOAD_LOCAL, depth, index)
                    captured_vars.append(free_var)
                # else: self-reference, skip capturing
                # else: in current scope - it's a self-reference
                # Don't capture, lambda will look it up via LOAD_GLOBAL -> closure_env
            # else: global variable - don't capture or track as free
        
        # Create new compilation context for lambda body
        lambda_ctx = CompilationContext()
        lambda_ctx.push_scope()

        # Add parameters to lambda scope
        for param_name in param_names:
            lambda_ctx.current_scope().add_binding(param_name)
        
        # Add captured free variables to lambda scope (they come after parameters in locals)
        # Self-references are NOT added here - they'll be looked up in closure_env
        for free_var in captured_vars:
            lambda_ctx.current_scope().add_binding(free_var)

        # Update max locals for the lambda
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
        free = []
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
                    # Don't recurse into nested lambdas - they have their own closure
                    return
                elif first.name == 'let':
                    # Let bindings create new bound variables
                    # We'd need to track these, but for now skip
                    return

            # Recursively check all elements
            for elem in expr.elements:
                self._collect_free_vars(elem, bound_vars, parent_ctx, free, seen)

    def _compile_lambda_old(self, expr: AIFPLList, ctx: CompilationContext) -> None:
        """OLD VERSION - keeping for reference, will delete"""
        if len(expr.elements) != 3:
            raise AIFPLEvalError("lambda requires parameters and body")

        _, params_list, body = expr.elements

        if not isinstance(params_list, AIFPLList):
            raise AIFPLEvalError("lambda parameters must be a list")

        # Extract parameter names
        param_names = []
        for param in params_list.elements:
            if not isinstance(param, AIFPLSymbol):
                raise AIFPLEvalError("lambda parameter must be a symbol")
            param_names.append(param.name)

        # Create new compilation context for lambda body
        lambda_ctx = CompilationContext()
        lambda_ctx.push_scope()

        # Add parameters to lambda scope
        for param_name in param_names:
            lambda_ctx.current_scope().add_binding(param_name)

        # Compile lambda body
        self._compile_expression(body, lambda_ctx)
        lambda_ctx.emit(Opcode.RETURN)

        # Create code object for lambda
        lambda_code = CodeObject(
            instructions=lambda_ctx.instructions,
            constants=lambda_ctx.constants,
            names=lambda_ctx.names,
            code_objects=lambda_ctx.code_objects,
            param_count=len(param_names),
            local_count=len(lambda_ctx.current_scope().bindings),
            name="<lambda>"
        )

        # Add to parent's code objects
        code_index = ctx.add_code_object(lambda_code)

        # Emit MAKE_CLOSURE instruction
        ctx.emit(Opcode.MAKE_CLOSURE, code_index)

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
