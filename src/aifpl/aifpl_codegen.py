"""
AIFPL code generator - generates bytecode from IR.
"""

from dataclasses import dataclass, field
from typing import List, Dict, Any

from aifpl.aifpl_builtin_registry import AIFPLBuiltinRegistry
from aifpl.aifpl_bytecode import CodeObject, Instruction, Opcode
from aifpl.aifpl_ir import (
    AIFPLIRExpr, AIFPLIRConstant, AIFPLIRVariable, AIFPLIRIf, AIFPLIRAnd, AIFPLIROr,
    AIFPLIRQuote, AIFPLIRError, AIFPLIRLet, AIFPLIRLetrec, AIFPLIRLambda, AIFPLIRCall,
    AIFPLIREmptyList, AIFPLIRReturn, AIFPLIRTrace
)
from aifpl.aifpl_value import AIFPLValue, AIFPLInteger, AIFPLFloat, AIFPLComplex, AIFPLBoolean, AIFPLString


# Mapping of builtin names to unary opcodes
UNARY_OPS = {
    # Unary type predicate operations
    'number?': Opcode.NUMBER_P,
    'integer?': Opcode.INTEGER_P,
    'float?': Opcode.FLOAT_P,
    'complex?': Opcode.COMPLEX_P,
    'string?': Opcode.STRING_P,
    'boolean?': Opcode.BOOLEAN_P,
    'list?': Opcode.LIST_P,
    'alist?': Opcode.ALIST_P,
    'function?': Opcode.FUNCTION_P,
    'not': Opcode.NOT,
    'sin': Opcode.SIN,
    'cos': Opcode.COS,
    'tan': Opcode.TAN,
    'log': Opcode.LOG,
    'log10': Opcode.LOG10,
    'exp': Opcode.EXP,
    'sqrt': Opcode.SQRT,
    'abs': Opcode.ABS,
    'floor': Opcode.FLOOR,
    'ceil': Opcode.CEIL,
    'reverse': Opcode.REVERSE,
    'first': Opcode.FIRST,
    'rest': Opcode.REST,
    'last': Opcode.LAST,
    'length': Opcode.LENGTH,
    'round': Opcode.ROUND,
    'integer': Opcode.TO_INTEGER,
    'float': Opcode.TO_FLOAT,
    'real': Opcode.REAL,
    'imag': Opcode.IMAG,
    'bin': Opcode.BIN,
    'hex': Opcode.HEX,
    'oct': Opcode.OCT,
    'bit-not': Opcode.BIT_NOT,
    'null?': Opcode.NULL_P,
    'string-length': Opcode.STRING_LENGTH,
    'string-upcase': Opcode.STRING_UPCASE,
    'string-downcase': Opcode.STRING_DOWNCASE,
    'string-trim': Opcode.STRING_TRIM,
    'string->number': Opcode.STRING_TO_NUMBER,
    'number->string': Opcode.NUMBER_TO_STRING,
    'string->list': Opcode.STRING_TO_LIST,
    'list->string': Opcode.LIST_TO_STRING,
    'alist-keys': Opcode.ALIST_KEYS,
    'alist-values': Opcode.ALIST_VALUES,
    'alist-length': Opcode.ALIST_LENGTH,
}

# Mapping of builtin names to binary opcodes
BINARY_OPS = {
    # Binary arithmetic operations
    '+': Opcode.ADD,
    '-': Opcode.SUB,
    '*': Opcode.MUL,
    '/': Opcode.DIV,
    'pow': Opcode.POW,
    'cons': Opcode.CONS,
    'list-ref': Opcode.LIST_REF,
    '//': Opcode.FLOOR_DIV,
    '%': Opcode.MOD,
    '**': Opcode.STAR_STAR,
    'bit-shift-left': Opcode.BIT_SHIFT_LEFT,
    'bit-shift-right': Opcode.BIT_SHIFT_RIGHT,
    'complex': Opcode.MAKE_COMPLEX,
    'member?': Opcode.MEMBER_P,
    'position': Opcode.POSITION,
    'take': Opcode.TAKE,
    'drop': Opcode.DROP,
    'remove': Opcode.REMOVE,
    'string-ref': Opcode.STRING_REF,
    'string-contains?': Opcode.STRING_CONTAINS_P,
    'string-prefix?': Opcode.STRING_PREFIX_P,
    'string-suffix?': Opcode.STRING_SUFFIX_P,
    'string-split': Opcode.STRING_SPLIT,
    'string-join': Opcode.STRING_JOIN,
    'alist-has?': Opcode.ALIST_HAS_P,
    'alist-remove': Opcode.ALIST_REMOVE,
    'alist-merge': Opcode.ALIST_MERGE,
    # Fold-reducible variadic ops (desugared to binary by desugarer)
    'bit-or': Opcode.BIT_OR,
    'bit-and': Opcode.BIT_AND,
    'bit-xor': Opcode.BIT_XOR,
    'append': Opcode.APPEND,
    'string-append': Opcode.STRING_APPEND,
    'min': Opcode.MIN,
    'max': Opcode.MAX,
    '=': Opcode.EQ,
    '!=': Opcode.NEQ,
    '<': Opcode.LT,
    '>': Opcode.GT,
    '<=': Opcode.LTE,
    '>=': Opcode.GTE,
    'string=?': Opcode.STRING_EQ_P,
    'number=?': Opcode.NUMBER_EQ_P,
    'integer=?': Opcode.INTEGER_EQ_P,
    'float=?': Opcode.FLOAT_EQ_P,
    'complex=?': Opcode.COMPLEX_EQ_P,
    'boolean=?': Opcode.BOOLEAN_EQ_P,
    'list=?': Opcode.LIST_EQ_P,
    'alist=?': Opcode.ALIST_EQ_P,
}

# Mapping of builtin names to ternary opcodes
TERNARY_OPS = {
    'substring': Opcode.SUBSTRING,
    'string-replace': Opcode.STRING_REPLACE,
    'alist-set': Opcode.ALIST_SET,
}

# Variadic collection-building opcodes.
# Unlike fold-reducible ops these are NOT desugared to binary form; instead the
# count of elements is encoded directly in the instruction argument.
BUILD_OPS = {
    'list': Opcode.BUILD_LIST,
    'alist': Opcode.BUILD_ALIST,
}


@dataclass
class AIFPLCodeGenContext:
    """
    Code generation context - tracks bytecode emission.

    This context does NOT track scopes or perform analysis.
    It only handles bytecode emission and resource management.
    """
    instructions: List[Instruction] = field(default_factory=list)
    constants: List[AIFPLValue] = field(default_factory=list)
    names: List[str] = field(default_factory=list)
    constant_map: Dict[tuple, int] = field(default_factory=dict)  # Key is (type, value)
    name_map: Dict[str, int] = field(default_factory=dict)
    code_objects: List[CodeObject] = field(default_factory=list)
    max_locals: int = 0

    def add_constant(self, value: AIFPLValue) -> int:
        """
        Add constant to pool and return its index.

        Uses (type_name, python_value) as key to ensure 1 and 1.0 are treated as different constants,
        since AIFPLInteger(1) == AIFPLFloat(1.0) due to cross-type numeric equality.
        """
        # For numeric types, booleans, and strings that have cross-type equality,
        # use (type, value) as key to prevent incorrect deduplication
        if isinstance(value, (AIFPLInteger, AIFPLFloat, AIFPLComplex, AIFPLBoolean, AIFPLString)):
            key: Any = (type(value).__name__, value.value)

        else:
            # For other types (lists, alists, functions, symbols), use the value itself as key
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


class AIFPLCodeGen:
    """
    Generates bytecode from compilation plans.

    This is a "dumb" code generator - it just follows the plan
    without performing any analysis.
    """

    def __init__(self) -> None:
        self.lambda_counter = 0  # Counter for anonymous lambdas

    def generate(self, plan: AIFPLIRExpr, name: str = "<module>") -> CodeObject:
        """
        Generate bytecode from a compilation plan.

        Args:
            plan: The compilation plan to generate code from
            name: Name for the code object (for debugging)

        Returns:
            Compiled code object
        """
        ctx = AIFPLCodeGenContext()

        # Generate code for the expression
        self._generate_expr(plan, ctx)

        # No automatic RETURN - the plan must explicitly include AIFPLIRReturn

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

    def _generate_expr(self, plan: AIFPLIRExpr, ctx: AIFPLCodeGenContext) -> None:
        """Generate code for an expression plan."""
        # Dispatch based on plan type
        if isinstance(plan, AIFPLIRConstant):
            self._generate_constant(plan, ctx)

        elif isinstance(plan, AIFPLIRVariable):
            self._generate_variable(plan, ctx)

        elif isinstance(plan, AIFPLIRIf):
            self._generate_if(plan, ctx)

        elif isinstance(plan, AIFPLIRAnd):
            self._generate_and(plan, ctx)

        elif isinstance(plan, AIFPLIROr):
            self._generate_or(plan, ctx)

        elif isinstance(plan, AIFPLIRQuote):
            self._generate_quote(plan, ctx)

        elif isinstance(plan, AIFPLIRError):
            self._generate_error(plan, ctx)

        elif isinstance(plan, AIFPLIRLet):
            self._generate_let(plan, ctx)

        elif isinstance(plan, AIFPLIRLetrec):
            self._generate_letrec(plan, ctx)

        elif isinstance(plan, AIFPLIRLambda):
            self._generate_lambda(plan, ctx)

        elif isinstance(plan, AIFPLIRCall):
            self._generate_call(plan, ctx)

        elif isinstance(plan, AIFPLIREmptyList):
            self._generate_empty_list(plan, ctx)

        elif isinstance(plan, AIFPLIRReturn):
            self._generate_return(plan, ctx)

        elif isinstance(plan, AIFPLIRTrace):
            self._generate_trace(plan, ctx)

        else:
            raise ValueError(f"Unknown plan type: {type(plan)}")

    def _generate_constant(self, plan: AIFPLIRConstant, ctx: AIFPLCodeGenContext) -> None:
        """Generate code for a constant."""
        const_index = ctx.add_constant(plan.value)
        ctx.emit(Opcode.LOAD_CONST, const_index)

    def _generate_variable(self, plan: AIFPLIRVariable, ctx: AIFPLCodeGenContext) -> None:
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

    def _generate_if(self, plan: AIFPLIRIf, ctx: AIFPLCodeGenContext) -> None:
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
            if last_op in (Opcode.RETURN, Opcode.TAIL_CALL_FUNCTION, Opcode.JUMP, Opcode.RAISE_ERROR):
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

    def _generate_and(self, plan: AIFPLIRAnd, ctx: AIFPLCodeGenContext) -> None:
        """Generate code for an and expression with short-circuit evaluation."""
        if len(plan.arg_plans) == 0:
            # (and) -> #t
            ctx.emit(Opcode.LOAD_TRUE)
            return

        # Multiple arguments: short-circuit evaluation
        jump_to_false = []

        for arg_plan in plan.arg_plans:
            # Generate argument
            self._generate_expr(arg_plan, ctx)

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

    def _generate_or(self, plan: AIFPLIROr, ctx: AIFPLCodeGenContext) -> None:
        """Generate code for an or expression with short-circuit evaluation."""
        if len(plan.arg_plans) == 0:
            # (or) -> #f
            ctx.emit(Opcode.LOAD_FALSE)
            return

        # Multiple arguments: short-circuit evaluation
        jump_to_true = []

        for arg_plan in plan.arg_plans:
            # Generate argument
            self._generate_expr(arg_plan, ctx)

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

    def _generate_quote(self, plan: AIFPLIRQuote, ctx: AIFPLCodeGenContext) -> None:
        """Generate code for a quote expression."""
        const_index = ctx.add_constant(plan.quoted_value)
        ctx.emit(Opcode.LOAD_CONST, const_index)

    def _generate_error(self, plan: AIFPLIRError, ctx: AIFPLCodeGenContext) -> None:
        """Generate code for an error expression."""
        const_index = ctx.add_constant(plan.message)
        ctx.emit(Opcode.RAISE_ERROR, const_index)

    def _generate_let(self, plan: AIFPLIRLet, ctx: AIFPLCodeGenContext) -> None:
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

    def _generate_letrec(self, plan: AIFPLIRLetrec, ctx: AIFPLCodeGenContext) -> None:
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

    def _generate_lambda(self, plan: AIFPLIRLambda, ctx: AIFPLCodeGenContext) -> None:
        """Generate code for a lambda expression."""
        # Emit LOAD_VAR for each free variable (for capture)
        for free_var_plan in plan.free_var_plans:
            # Generate code to load the free variable from parent scope
            self._generate_variable(free_var_plan, ctx)

        # Create nested context for lambda body
        lambda_ctx = AIFPLCodeGenContext()

        # Generate function prologue: pop arguments from stack into locals
        for i in range(len(plan.params) - 1, -1, -1):
            lambda_ctx.emit(Opcode.STORE_VAR, i)

        # Set max locals from plan
        lambda_ctx.max_locals = plan.max_locals

        # Generate body
        self._generate_expr(plan.body_plan, lambda_ctx)

        # No automatic RETURN - the body_plan must explicitly include AIFPLIRReturn

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
            param_count=plan.param_count,
            local_count=lambda_ctx.max_locals,
            name=lambda_name,
            source_line=plan.source_line,
            source_file=plan.source_file
        )

        # Add to parent's code objects
        code_index = ctx.add_code_object(lambda_code)

        # Emit MAKE_CLOSURE instruction
        ctx.emit(Opcode.MAKE_CLOSURE, code_index, len(plan.free_vars))

    def _generate_call(self, plan: AIFPLIRCall, ctx: AIFPLCodeGenContext) -> None:
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
            assert plan.builtin_index is not None

            # Get builtin name to check if it's a primitive
            builtin_name = AIFPLBuiltinRegistry.BUILTIN_TABLE[plan.builtin_index]

            # Handle alist-get: synthesise missing default as #f
            if builtin_name == 'alist-get':
                for arg_plan in plan.arg_plans:
                    self._generate_expr(arg_plan, ctx)
                if len(plan.arg_plans) == 2:
                    ctx.emit(Opcode.LOAD_FALSE)
                ctx.emit(Opcode.ALIST_GET)
                return

            # Handle range: synthesise missing step as 1
            if builtin_name == 'range':
                for arg_plan in plan.arg_plans:
                    self._generate_expr(arg_plan, ctx)
                if len(plan.arg_plans) == 2:
                    const_index = ctx.add_constant(AIFPLInteger(1))
                    ctx.emit(Opcode.LOAD_CONST, const_index)
                ctx.emit(Opcode.RANGE)
                return

            # Generate arguments
            for arg_plan in plan.arg_plans:
                self._generate_expr(arg_plan, ctx)

            # Check if this is a primitive operation with correct arity
            if builtin_name in BINARY_OPS:
                if len(plan.arg_plans) == 2:
                    primitive_opcode = BINARY_OPS[builtin_name]
                    ctx.emit(primitive_opcode)
                    return

            elif builtin_name in UNARY_OPS:
                if len(plan.arg_plans) == 1:
                    primitive_opcode = UNARY_OPS[builtin_name]
                    ctx.emit(primitive_opcode)
                    return

            elif builtin_name in TERNARY_OPS:
                if len(plan.arg_plans) == 3:
                    primitive_opcode = TERNARY_OPS[builtin_name]
                    ctx.emit(primitive_opcode)
                    return

            elif builtin_name in BUILD_OPS:
                build_opcode = BUILD_OPS[builtin_name]
                ctx.emit(build_opcode, len(plan.arg_plans))
                return

            # Regular builtin call (wrong arity or non-primitive)
            ctx.emit(Opcode.CALL_BUILTIN, plan.builtin_index, len(plan.arg_plans))

            return

        # Regular function call
        # Generate function expression
        self._generate_expr(plan.func_plan, ctx)

        # Generate arguments
        for arg_plan in plan.arg_plans:
            self._generate_expr(arg_plan, ctx)

        # Emit call
        if plan.is_tail_call:
            ctx.emit(Opcode.TAIL_CALL_FUNCTION, len(plan.arg_plans))

        else:
            ctx.emit(Opcode.CALL_FUNCTION, len(plan.arg_plans))

    def _generate_empty_list(self, _plan: AIFPLIREmptyList, ctx: AIFPLCodeGenContext) -> None:
        """Generate code for an empty list literal."""
        ctx.emit(Opcode.LOAD_EMPTY_LIST)

    def _generate_return(self, plan: AIFPLIRReturn, ctx: AIFPLCodeGenContext) -> None:
        """Generate code for a return statement."""
        # Generate the value to return
        self._generate_expr(plan.value_plan, ctx)
        # Emit RETURN instruction
        ctx.emit(Opcode.RETURN)

    def _generate_trace(self, plan: AIFPLIRTrace, ctx: AIFPLCodeGenContext) -> None:
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
