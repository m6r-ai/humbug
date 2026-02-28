"""
Menai code generator - generates bytecode from IR.
"""

from dataclasses import dataclass, field
from typing import List, Dict, Any

from menai.menai_bytecode import BUILTIN_OPCODE_MAP, CodeObject, Instruction, Opcode
from menai.menai_ir import (
    MenaiIRExpr, MenaiIRConstant, MenaiIRVariable, MenaiIRIf,
    MenaiIRQuote, MenaiIRError, MenaiIRLet, MenaiIRLetrec, MenaiIRLambda, MenaiIRCall,
    MenaiIREmptyList, MenaiIRReturn, MenaiIRTrace
)
from menai.menai_value import MenaiValue, MenaiInteger, MenaiFloat, MenaiComplex, MenaiBoolean, MenaiNone, MenaiString


# Derived opcode maps for the codegen — built from the single source of truth
# in BUILTIN_OPCODE_MAP. The codegen uses these to emit direct opcodes instead
# builtins called with the correct fixed arity.
UNARY_OPS  = {name: op for name, (op, arity) in BUILTIN_OPCODE_MAP.items() if arity == 1}
BINARY_OPS = {name: op for name, (op, arity) in BUILTIN_OPCODE_MAP.items() if arity == 2}
TERNARY_OPS = {name: op for name, (op, arity) in BUILTIN_OPCODE_MAP.items() if arity == 3}

# Variadic collection-building opcodes.
# Unlike fold-reducible ops these are NOT desugared to binary form; instead the
# count of elements is encoded directly in the instruction argument.
BUILD_OPS = {
    'list': Opcode.LIST,
    'dict': Opcode.DICT,
}


@dataclass
class MenaiCodeGenContext:
    """
    Code generation context - tracks bytecode emission.

    This context does NOT track scopes or perform analysis.
    It only handles bytecode emission and resource management.
    """
    instructions: List[Instruction] = field(default_factory=list)
    constants: List[MenaiValue] = field(default_factory=list)
    names: List[str] = field(default_factory=list)
    constant_map: Dict[tuple, int] = field(default_factory=dict)  # Key is (type, value)
    name_map: Dict[str, int] = field(default_factory=dict)
    code_objects: List[CodeObject] = field(default_factory=list)
    max_locals: int = 0

    def add_constant(self, value: MenaiValue) -> int:
        """
        Add constant to pool and return its index.

        Uses (type_name, python_value) as key to ensure 1 and 1.0 are treated as different constants,
        since MenaiInteger(1) == MenaiFloat(1.0) due to cross-type numeric equality.
        """
        # For numeric types, booleans, and strings that have cross-type equality,
        # use (type, value) as key to prevent incorrect deduplication
        if isinstance(value, (MenaiInteger, MenaiFloat, MenaiComplex, MenaiBoolean, MenaiString)):
            key: Any = (type(value).__name__, value.value)

        else:
            # For other types (lists, dicts, functions, symbols), use the value itself as key
            # These types don't have problematic cross-type equality
            key = value

        if key in self.constant_map:
            return self.constant_map[key]

        index = len(self.constants)
        self.constants.append(value)
        self.constant_map[key] = index
        return index

    def add_name(self, name: str) -> int:
        """Add name to pool and return its index."""
        if name in self.name_map:
            return self.name_map[name]

        index = len(self.names)
        self.names.append(name)
        self.name_map[name] = index
        return index

    def add_code_object(self, code_obj: CodeObject) -> int:
        """Add nested code object and return its index."""
        index = len(self.code_objects)
        self.code_objects.append(code_obj)
        return index

    def emit(self, opcode: Opcode, arg1: int = 0, arg2: int = 0) -> int:
        """Emit an instruction and return its index."""
        instr = Instruction(opcode, arg1, arg2)
        index = len(self.instructions)
        self.instructions.append(instr)
        return index

    def patch_jump(self, instr_index: int, target: int) -> None:
        """Patch a jump instruction to point to target."""
        self.instructions[instr_index].arg1 = target

    def current_instruction_index(self) -> int:
        """Get index of next instruction to be emitted."""
        return len(self.instructions)


class MenaiCodeGen:
    """
    Generates bytecode from compilation plans.

    This is a "dumb" code generator - it just follows the plan
    without performing any analysis.
    """

    def __init__(self) -> None:
        self.lambda_counter = 0  # Counter for anonymous lambdas

    def generate(self, plan: MenaiIRExpr, name: str = "<module>") -> CodeObject:
        """
        Generate bytecode from a compilation plan.

        Args:
            plan: The compilation plan to generate code from
            name: Name for the code object (for debugging)

        Returns:
            Compiled code object
        """
        ctx = MenaiCodeGenContext()

        # Generate code for the expression
        self._generate_expr(plan, ctx)

        # No automatic RETURN - the plan must explicitly include MenaiIRReturn

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

    def _generate_expr(self, plan: MenaiIRExpr, ctx: MenaiCodeGenContext) -> None:
        """Generate code for an expression plan."""
        # Dispatch based on plan type
        if isinstance(plan, MenaiIRConstant):
            self._generate_constant(plan, ctx)

        elif isinstance(plan, MenaiIRVariable):
            self._generate_variable(plan, ctx)

        elif isinstance(plan, MenaiIRIf):
            self._generate_if(plan, ctx)

        elif isinstance(plan, MenaiIRQuote):
            self._generate_quote(plan, ctx)

        elif isinstance(plan, MenaiIRError):
            self._generate_error(plan, ctx)

        elif isinstance(plan, MenaiIRLet):
            self._generate_let(plan, ctx)

        elif isinstance(plan, MenaiIRLetrec):
            self._generate_letrec(plan, ctx)

        elif isinstance(plan, MenaiIRLambda):
            self._generate_lambda(plan, ctx)

        elif isinstance(plan, MenaiIRCall):
            self._generate_call(plan, ctx)

        elif isinstance(plan, MenaiIREmptyList):
            self._generate_empty_list(plan, ctx)

        elif isinstance(plan, MenaiIRReturn):
            self._generate_return(plan, ctx)

        elif isinstance(plan, MenaiIRTrace):
            self._generate_trace(plan, ctx)

        else:
            raise ValueError(f"Unknown plan type: {type(plan)}")

    def _generate_constant(self, plan: MenaiIRConstant, ctx: MenaiCodeGenContext) -> None:
        """Generate code for a constant."""
        # Optimise #none: use dedicated opcode instead of constant pool entry
        if isinstance(plan.value, MenaiNone):
            ctx.emit(Opcode.LOAD_NONE)
            return

        # Optimise #t / #f: use dedicated opcodes instead of constant pool entries
        if isinstance(plan.value, MenaiBoolean):
            ctx.emit(Opcode.LOAD_TRUE if plan.value.value else Opcode.LOAD_FALSE)
            return

        const_index = ctx.add_constant(plan.value)
        ctx.emit(Opcode.LOAD_CONST, const_index)

    def _generate_variable(self, plan: MenaiIRVariable, ctx: MenaiCodeGenContext) -> None:
        """Generate code for a variable reference."""
        if plan.var_type == 'local':
            if plan.is_parent_ref:
                # Load from parent frame (for recursive bindings)
                ctx.emit(Opcode.LOAD_PARENT_VAR, plan.index, plan.depth)

            else:
                ctx.emit(Opcode.LOAD_VAR, plan.index)

        else:  # global
            # For globals, we need to assign the name index during codegen
            name_index = ctx.add_name(plan.name)
            ctx.emit(Opcode.LOAD_NAME, name_index)

    def _generate_if(self, plan: MenaiIRIf, ctx: MenaiCodeGenContext) -> None:
        """Generate code for an if expression."""
        # Generate condition
        self._generate_expr(plan.condition_plan, ctx)

        # Jump to else if condition is false
        jump_to_else = ctx.emit(Opcode.JUMP_IF_FALSE, 0)

        # Generate then branch
        self._generate_expr(plan.then_plan, ctx)

        # Check if then branch terminates (ends with RETURN, TAIL_CALL, or unconditional JUMP)
        # If it terminates, we don't need to emit a jump past the else branch
        then_terminates = False
        if ctx.instructions:
            last_op = ctx.instructions[-1].opcode
            if last_op in (Opcode.RETURN, Opcode.TAIL_CALL, Opcode.TAIL_APPLY, Opcode.JUMP, Opcode.RAISE_ERROR):
                then_terminates = True

        # Only emit jump past else if then branch doesn't terminate
        jump_past_else = None if then_terminates else ctx.emit(Opcode.JUMP, 0)

        else_start = ctx.current_instruction_index()
        ctx.patch_jump(jump_to_else, else_start)

        # Generate else branch
        self._generate_expr(plan.else_plan, ctx)

        # Patch jump past else (if we emitted one)
        if jump_past_else is not None:
            # Patch to the next instruction after the else branch
            after_else = ctx.current_instruction_index()
            ctx.patch_jump(jump_past_else, after_else)

    def _generate_quote(self, plan: MenaiIRQuote, ctx: MenaiCodeGenContext) -> None:
        """Generate code for a quote expression."""
        const_index = ctx.add_constant(plan.quoted_value)
        ctx.emit(Opcode.LOAD_CONST, const_index)

    def _generate_error(self, plan: MenaiIRError, ctx: MenaiCodeGenContext) -> None:
        """Generate code for an error expression."""
        const_index = ctx.add_constant(plan.message)
        ctx.emit(Opcode.RAISE_ERROR, const_index)

    def _generate_let(self, plan: MenaiIRLet, ctx: MenaiCodeGenContext) -> None:
        """Generate code for a let expression."""
        # Generate and store each binding
        for _, value_plan, var_index in plan.bindings:
            # Generate value
            self._generate_expr(value_plan, ctx)

            # Store in local variable
            ctx.emit(Opcode.STORE_VAR, var_index)

            # Update max locals
            ctx.max_locals = max(ctx.max_locals, var_index + 1)

        # Generate body
        self._generate_expr(plan.body_plan, ctx)

    def _generate_letrec(self, plan: MenaiIRLetrec, ctx: MenaiCodeGenContext) -> None:
        """Generate code for a letrec expression."""
        # Generate and store each binding
        for _, value_plan, var_index in plan.bindings:
            # Generate value
            self._generate_expr(value_plan, ctx)

            # Store in local variable
            ctx.emit(Opcode.STORE_VAR, var_index)

            # Update max locals
            ctx.max_locals = max(ctx.max_locals, var_index + 1)

        # Generate body
        self._generate_expr(plan.body_plan, ctx)

    def _generate_lambda(self, plan: MenaiIRLambda, ctx: MenaiCodeGenContext) -> None:
        """Generate code for a lambda expression."""
        # Emit LOAD_VAR for each free variable (for capture)
        for free_var_plan in plan.free_var_plans:
            # Generate code to load the free variable from parent scope
            self._generate_variable(free_var_plan, ctx)

        # Create nested context for lambda body
        lambda_ctx = MenaiCodeGenContext()

        # Generate function prologue: pop all N arguments from stack into locals 0..N-1
        if plan.params:
            lambda_ctx.emit(Opcode.ENTER, len(plan.params))

        # Set max locals from plan
        lambda_ctx.max_locals = plan.max_locals

        # Generate body
        self._generate_expr(plan.body_plan, lambda_ctx)

        # Generate a descriptive name for the lambda
        if plan.binding_name:
            # Use the binding name if available (from let/letrec)
            lambda_name = plan.binding_name

        else:
            # Generate a unique name for anonymous lambdas
            lambda_name = f"<lambda-{self.lambda_counter}>"
            self.lambda_counter += 1

        # Add parameter info to the name for better debugging
        param_word = "param" if len(plan.params) == 1 else "params"
        lambda_name = f"{lambda_name}({len(plan.params)} {param_word})"

        # Create code object for lambda
        lambda_code = CodeObject(
            instructions=lambda_ctx.instructions,
            constants=lambda_ctx.constants,
            names=lambda_ctx.names,
            code_objects=lambda_ctx.code_objects,
            free_vars=plan.free_vars,
            param_names=plan.params,
            param_count=plan.param_count,
            local_count=lambda_ctx.max_locals,
            is_variadic=plan.is_variadic,
            name=lambda_name,
            source_line=plan.source_line,
            source_file=plan.source_file
        )

        # Add to parent's code objects
        code_index = ctx.add_code_object(lambda_code)

        # Emit MAKE_CLOSURE instruction
        ctx.emit(Opcode.MAKE_CLOSURE, code_index, len(plan.free_vars))

    def _generate_call(self, plan: MenaiIRCall, ctx: MenaiCodeGenContext) -> None:
        """Generate code for a function call."""
        # Check for tail-recursive call (jump to start)
        if plan.is_tail_recursive:
            # Generate arguments
            for arg_plan in plan.arg_plans:
                self._generate_expr(arg_plan, ctx)

            # Jump to instruction 0 (start of function)
            ctx.emit(Opcode.JUMP, 0)
            return

        # Check for builtin call
        if plan.is_builtin:
            assert plan.builtin_name is not None
            builtin_name = plan.builtin_name

            # Handle range: synthesise missing step as 1
            if builtin_name == 'range':
                for arg_plan in plan.arg_plans:
                    self._generate_expr(arg_plan, ctx)

                if len(plan.arg_plans) == 2:
                    const_index = ctx.add_constant(MenaiInteger(1))
                    ctx.emit(Opcode.LOAD_CONST, const_index)

                ctx.emit(Opcode.RANGE)
                return

            # Handle integer->complex: synthesise missing imaginary part as 0
            if builtin_name == 'integer->complex':
                self._generate_expr(plan.arg_plans[0], ctx)
                if len(plan.arg_plans) == 1:
                    const_index = ctx.add_constant(MenaiInteger(0))
                    ctx.emit(Opcode.LOAD_CONST, const_index)

                else:
                    self._generate_expr(plan.arg_plans[1], ctx)

                ctx.emit(Opcode.INTEGER_TO_COMPLEX)
                return

            # Handle integer->string: synthesise missing radix as 10
            if builtin_name == 'integer->string':
                self._generate_expr(plan.arg_plans[0], ctx)
                if len(plan.arg_plans) == 1:
                    const_index = ctx.add_constant(MenaiInteger(10))
                    ctx.emit(Opcode.LOAD_CONST, const_index)

                else:
                    self._generate_expr(plan.arg_plans[1], ctx)

                ctx.emit(Opcode.INTEGER_TO_STRING)
                return

            # Handle float->complex: synthesise missing imaginary part as 0.0
            if builtin_name == 'float->complex':
                self._generate_expr(plan.arg_plans[0], ctx)
                if len(plan.arg_plans) == 1:
                    const_index = ctx.add_constant(MenaiFloat(0.0))
                    ctx.emit(Opcode.LOAD_CONST, const_index)

                else:
                    self._generate_expr(plan.arg_plans[1], ctx)

                ctx.emit(Opcode.FLOAT_TO_COMPLEX)
                return

            # Handle string->integer: synthesise missing radix as 10
            if builtin_name == 'string->integer':
                self._generate_expr(plan.arg_plans[0], ctx)
                if len(plan.arg_plans) == 1:
                    const_index = ctx.add_constant(MenaiInteger(10))
                    ctx.emit(Opcode.LOAD_CONST, const_index)

                else:
                    self._generate_expr(plan.arg_plans[1], ctx)

                ctx.emit(Opcode.STRING_TO_INTEGER)
                return

            # Handle string-slice: synthesise missing end as (string-length str)
            if builtin_name == 'string-slice':
                # Push the string argument first
                self._generate_expr(plan.arg_plans[0], ctx)

                # Push the start argument
                self._generate_expr(plan.arg_plans[1], ctx)
                if len(plan.arg_plans) == 2:
                    # Synthesise end = (string-length str): push str again, emit STRING_LENGTH
                    self._generate_expr(plan.arg_plans[0], ctx)
                    ctx.emit(Opcode.STRING_LENGTH)

                else:
                    self._generate_expr(plan.arg_plans[2], ctx)

                ctx.emit(Opcode.STRING_SLICE)
                return

            # Handle string->list: synthesise missing delimiter as ""
            if builtin_name == 'string->list':
                self._generate_expr(plan.arg_plans[0], ctx)
                if len(plan.arg_plans) == 1:
                    const_index = ctx.add_constant(MenaiString(""))
                    ctx.emit(Opcode.LOAD_CONST, const_index)

                else:
                    self._generate_expr(plan.arg_plans[1], ctx)

                ctx.emit(Opcode.STRING_TO_LIST)
                return

            # Handle list-slice: synthesise missing end as (list-length lst)
            if builtin_name == 'list-slice':
                # Push the list argument first
                self._generate_expr(plan.arg_plans[0], ctx)

                # Push the start argument
                self._generate_expr(plan.arg_plans[1], ctx)
                if len(plan.arg_plans) == 2:
                    # Synthesise end = (list-length lst): push lst again, emit LIST_LENGTH
                    self._generate_expr(plan.arg_plans[0], ctx)
                    ctx.emit(Opcode.LIST_LENGTH)

                else:
                    self._generate_expr(plan.arg_plans[2], ctx)

                ctx.emit(Opcode.LIST_SLICE)
                return

            # Handle list->string: synthesise missing separator as ""
            if builtin_name == 'list->string':
                self._generate_expr(plan.arg_plans[0], ctx)
                if len(plan.arg_plans) == 1:
                    const_index = ctx.add_constant(MenaiString(""))
                    ctx.emit(Opcode.LOAD_CONST, const_index)

                else:
                    self._generate_expr(plan.arg_plans[1], ctx)

                ctx.emit(Opcode.LIST_TO_STRING)
                return

            # Handle dict-get: synthesise missing default as #f
            if builtin_name == 'dict-get':
                for arg_plan in plan.arg_plans:
                    self._generate_expr(arg_plan, ctx)

                if len(plan.arg_plans) == 2:
                    ctx.emit(Opcode.LOAD_NONE)

                ctx.emit(Opcode.DICT_GET)
                return

            # Generate arguments
            for arg_plan in plan.arg_plans:
                self._generate_expr(arg_plan, ctx)

            # Handle apply: emit TAIL_APPLY or APPLY depending on tail position.
            # apply is the only builtin that dispatches a further call, so it
            # needs the tail/non-tail distinction like regular CALL/TAIL_CALL.
            # Stack convention: function first, then arg list (both already pushed).
            if builtin_name == 'apply':
                if plan.is_tail_call:
                    ctx.emit(Opcode.TAIL_APPLY)
                    return

                ctx.emit(Opcode.APPLY)
                return

            # Check if this is a primitive operation with correct arity
            if builtin_name in BINARY_OPS:
                primitive_opcode = BINARY_OPS[builtin_name]
                ctx.emit(primitive_opcode)
                return

            if builtin_name in UNARY_OPS:
                primitive_opcode = UNARY_OPS[builtin_name]
                ctx.emit(primitive_opcode)
                return

            if builtin_name in TERNARY_OPS:
                primitive_opcode = TERNARY_OPS[builtin_name]
                ctx.emit(primitive_opcode)
                return

            assert builtin_name in BUILD_OPS, f"No opcode for '{builtin_name}' with {len(plan.arg_plans)} args"
            build_opcode = BUILD_OPS[builtin_name]
            ctx.emit(build_opcode, len(plan.arg_plans))
            return

        # Regular function call
        # Convention: args are pushed first (bottom of frame), function on top.
        for arg_plan in plan.arg_plans:
            self._generate_expr(arg_plan, ctx)

        self._generate_expr(plan.func_plan, ctx)

        # Emit call
        if plan.is_tail_call:
            ctx.emit(Opcode.TAIL_CALL, len(plan.arg_plans))

        else:
            ctx.emit(Opcode.CALL, len(plan.arg_plans))

    def _generate_empty_list(self, _plan: MenaiIREmptyList, ctx: MenaiCodeGenContext) -> None:
        """Generate code for an empty list literal."""
        ctx.emit(Opcode.LOAD_EMPTY_LIST)

    def _generate_return(self, plan: MenaiIRReturn, ctx: MenaiCodeGenContext) -> None:
        """Generate code for a return statement."""
        # Generate the value to return
        self._generate_expr(plan.value_plan, ctx)
        # Emit RETURN instruction
        ctx.emit(Opcode.RETURN)

    def _generate_trace(self, plan: MenaiIRTrace, ctx: MenaiCodeGenContext) -> None:
        """
        Generate code for a trace expression.

        Emits each message via EMIT_TRACE, then generates code for the value expression.
        The value expression's result is left on the stack.
        """
        # Generate and emit each message
        for message_plan in plan.message_plans:
            # Generate code to evaluate the message
            self._generate_expr(message_plan, ctx)

            # Emit EMIT_TRACE (pops value, emits to watcher)
            ctx.emit(Opcode.EMIT_TRACE)

        # Generate code for the return value (leaves result on stack)
        self._generate_expr(plan.value_plan, ctx)
