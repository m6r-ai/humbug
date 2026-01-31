"""AIFPL code generator - generates bytecode from compilation plans.

This is Phase 2 of the two-phase compiler. It takes a compilation plan
(produced by the analyzer) and generates bytecode. It does NOT perform
any analysis - all necessary information is in the plan.
"""

from dataclasses import dataclass, field
from typing import List, Dict

from aifpl.aifpl_bytecode import CodeObject, Instruction, Opcode
from aifpl.aifpl_compilation_plan import (
    ExprPlan, ConstantPlan, VariablePlan, IfPlan, AndPlan, OrPlan,
    QuotePlan, ErrorPlan, LetPlan, LetrecPlan, LambdaPlan, CallPlan,
    EmptyListPlan, ReturnPlan
)
from aifpl.aifpl_value import AIFPLValue


@dataclass
class CodeGenContext:
    """
    Code generation context - tracks bytecode emission.

    This context does NOT track scopes or perform analysis.
    It only handles bytecode emission and resource management.
    """
    instructions: List[Instruction] = field(default_factory=list)
    constants: List[AIFPLValue] = field(default_factory=list)
    names: List[str] = field(default_factory=list)
    constant_map: Dict[AIFPLValue, int] = field(default_factory=dict)
    name_map: Dict[str, int] = field(default_factory=dict)
    code_objects: List[CodeObject] = field(default_factory=list)
    max_locals: int = 0

    def add_constant(self, value: AIFPLValue) -> int:
        """Add constant to pool and return its index."""
        if value in self.constant_map:
            return self.constant_map[value]

        index = len(self.constants)
        self.constants.append(value)
        self.constant_map[value] = index
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


class AIFPLCodeGenerator:
    """
    Generates bytecode from compilation plans.

    This is a "dumb" code generator - it just follows the plan
    without performing any analysis.
    """

    def generate(self, plan: ExprPlan, name: str = "<module>") -> CodeObject:
        """
        Generate bytecode from a compilation plan.

        Args:
            plan: The compilation plan to generate code from
            name: Name for the code object (for debugging)

        Returns:
            Compiled code object
        """
        ctx = CodeGenContext()

        # Generate code for the expression
        self._generate_expr(plan, ctx)

        # No automatic RETURN - the plan must explicitly include ReturnPlan

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

    def _generate_expr(self, plan: ExprPlan, ctx: CodeGenContext) -> None:
        """Generate code for an expression plan."""
        # Dispatch based on plan type
        if isinstance(plan, ConstantPlan):
            self._generate_constant(plan, ctx)

        elif isinstance(plan, VariablePlan):
            self._generate_variable(plan, ctx)

        elif isinstance(plan, IfPlan):
            self._generate_if(plan, ctx)

        elif isinstance(plan, AndPlan):
            self._generate_and(plan, ctx)

        elif isinstance(plan, OrPlan):
            self._generate_or(plan, ctx)

        elif isinstance(plan, QuotePlan):
            self._generate_quote(plan, ctx)

        elif isinstance(plan, ErrorPlan):
            self._generate_error(plan, ctx)

        elif isinstance(plan, LetPlan):
            self._generate_let(plan, ctx)

        elif isinstance(plan, LetrecPlan):
            self._generate_letrec(plan, ctx)

        elif isinstance(plan, LambdaPlan):
            self._generate_lambda(plan, ctx)

        elif isinstance(plan, CallPlan):
            self._generate_call(plan, ctx)

        elif isinstance(plan, EmptyListPlan):
            self._generate_empty_list(plan, ctx)

        elif isinstance(plan, ReturnPlan):
            self._generate_return(plan, ctx)

        else:
            raise ValueError(f"Unknown plan type: {type(plan)}")

    def _generate_constant(self, plan: ConstantPlan, ctx: CodeGenContext) -> None:
        """Generate code for a constant."""
        const_index = ctx.add_constant(plan.value)
        ctx.emit(Opcode.LOAD_CONST, const_index)

    def _generate_variable(self, plan: VariablePlan, ctx: CodeGenContext) -> None:
        """Generate code for a variable reference."""
        if plan.var_type == 'local':
            if plan.is_parent_ref:
                # Load from parent frame (for recursive bindings)
                ctx.emit(Opcode.LOAD_PARENT_VAR, plan.depth, plan.index)
            else:
                ctx.emit(Opcode.LOAD_VAR, plan.depth, plan.index)

        else:  # global
            # For globals, we need to assign the name index during codegen
            name_index = ctx.add_name(plan.name)
            ctx.emit(Opcode.LOAD_NAME, name_index)

    def _generate_if(self, plan: IfPlan, ctx: CodeGenContext) -> None:
        """Generate code for an if expression."""
        # Generate condition
        self._generate_expr(plan.condition_plan, ctx)

        # Jump to else if condition is false
        jump_to_else = ctx.emit(Opcode.JUMP_IF_FALSE, 0)

        # Generate then branch
        self._generate_expr(plan.then_plan, ctx)

        # Check if then branch ends with RETURN or TAIL_CALL (no need to jump in that case)
        then_returns = False
        if ctx.instructions:
            last_op = ctx.instructions[-1].opcode
            if last_op in (Opcode.RETURN, Opcode.TAIL_CALL_FUNCTION):
                then_returns = True

        # Only jump past else if then branch doesn't return
        jump_past_else = None if then_returns else ctx.emit(Opcode.JUMP, 0)

        else_start = ctx.current_instruction_index()
        ctx.patch_jump(jump_to_else, else_start)

        # Generate else branch
        self._generate_expr(plan.else_plan, ctx)

        # Patch jump past else (if we emitted one)
        if jump_past_else is not None:
            after_else = ctx.current_instruction_index()
            ctx.patch_jump(jump_past_else, after_else)

    def _generate_and(self, plan: AndPlan, ctx: CodeGenContext) -> None:
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

    def _generate_or(self, plan: OrPlan, ctx: CodeGenContext) -> None:
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

    def _generate_quote(self, plan: QuotePlan, ctx: CodeGenContext) -> None:
        """Generate code for a quote expression."""
        const_index = ctx.add_constant(plan.quoted_value)
        ctx.emit(Opcode.LOAD_CONST, const_index)

    def _generate_error(self, plan: ErrorPlan, ctx: CodeGenContext) -> None:
        """Generate code for an error expression."""
        const_index = ctx.add_constant(plan.message)
        ctx.emit(Opcode.RAISE_ERROR, const_index)

    def _generate_let(self, plan: LetPlan, ctx: CodeGenContext) -> None:
        """Generate code for a let expression."""
        # Generate and store each binding
        for _, value_plan, var_index in plan.bindings:
            # Generate value
            self._generate_expr(value_plan, ctx)

            # Store in local variable
            ctx.emit(Opcode.STORE_VAR, 0, var_index)

            # Update max locals
            ctx.max_locals = max(ctx.max_locals, var_index + 1)

        # Generate body
        self._generate_expr(plan.body_plan, ctx)

    def _generate_letrec(self, plan: LetrecPlan, ctx: CodeGenContext) -> None:
        """Generate code for a letrec expression."""
        # Generate and store each binding
        for name, value_plan, var_index in plan.bindings:
            # Generate value
            self._generate_expr(value_plan, ctx)

            # Store in local variable
            ctx.emit(Opcode.STORE_VAR, 0, var_index)

            # Update max locals
            ctx.max_locals = max(ctx.max_locals, var_index + 1)

        # Generate body
        self._generate_expr(plan.body_plan, ctx)

    def _generate_lambda(self, plan: LambdaPlan, ctx: CodeGenContext) -> None:
        """Generate code for a lambda expression."""
        # Emit LOAD_VAR for each free variable (for capture)
        for free_var_plan in plan.free_var_plans:
            # Generate code to load the free variable from parent scope
            self._generate_variable(free_var_plan, ctx)

        # Create nested context for lambda body
        lambda_ctx = CodeGenContext()

        # Generate function prologue: pop arguments from stack into locals
        for i in range(len(plan.params) - 1, -1, -1):
            lambda_ctx.emit(Opcode.STORE_VAR, 0, i)

        # Set max locals from plan
        lambda_ctx.max_locals = plan.max_locals

        # Generate body
        self._generate_expr(plan.body_plan, lambda_ctx)

        # No automatic RETURN - the body_plan must explicitly include ReturnPlan

        # Create code object for lambda
        lambda_code = CodeObject(
            instructions=lambda_ctx.instructions,
            constants=lambda_ctx.constants,
            names=lambda_ctx.names,
            code_objects=lambda_ctx.code_objects,
            free_vars=plan.free_vars,
            param_count=plan.param_count,
            local_count=lambda_ctx.max_locals,
            name="<lambda>"
        )

        # Add to parent's code objects
        code_index = ctx.add_code_object(lambda_code)

        # Emit MAKE_CLOSURE instruction
        ctx.emit(Opcode.MAKE_CLOSURE, code_index, len(plan.free_vars))

    def _generate_call(self, plan: CallPlan, ctx: CodeGenContext) -> None:
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
            # Generate arguments
            for arg_plan in plan.arg_plans:
                self._generate_expr(arg_plan, ctx)

            # Emit builtin call
            assert plan.builtin_index is not None
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

    def _generate_empty_list(self, _plan: EmptyListPlan, ctx: CodeGenContext) -> None:
        """Generate code for an empty list literal."""
        ctx.emit(Opcode.LOAD_EMPTY_LIST)

    def _generate_return(self, plan: ReturnPlan, ctx: CodeGenContext) -> None:
        """Generate code for a return statement."""
        # Generate the value to return
        self._generate_expr(plan.value_plan, ctx)
        # Emit RETURN instruction
        ctx.emit(Opcode.RETURN)
