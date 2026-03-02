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
from menai.menai_value import (
    MenaiValue, MenaiInteger, MenaiFloat, MenaiComplex,
    MenaiBoolean, MenaiString, MenaiFunction, MenaiNone
)


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
    current_lambda_name: str | None = None  # Name of the lambda currently being compiled.
                                            # Used by _generate_call to detect direct
                                            # self-recursive tail calls and emit JUMP 0.

    # Letrec two-phase support: when non-None, _generate_lambda intercepts any
    # lambda whose free_vars intersect this set and defers all captures to Phase 2.
    # Set by _generate_letrec for the duration of binding-value generation.
    letrec_sibling_names: 'set | None' = None

    # List of (var_index, lambda_plan) tuples for closures that were deferred
    # during letrec binding-value generation.  Each entry records the local slot
    # where the closure was stored (via a temp STORE_VAR) and the lambda IR node
    # so Phase 2 can emit PATCH_CLOSURE for every free_var slot.
    #
    # Entry format: (var_index: int, lambda_plan: MenaiIRLambda)
    letrec_deferred_patches: 'List[tuple] | None' = None

    # Counter for allocating temp slots for deferred lambdas inside letrec
    # binding values.  Incremented each time a new temp slot is needed.
    # Reset to None when letrec binding-value generation ends.
    letrec_next_temp_slot: 'int | None' = None

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
        # Collect the set of binding names for this letrec group so we can
        # identify sibling references in free_vars.  Set on the context
        # so that _generate_lambda can intercept ANY lambda with sibling captures,
        # even when nested inside a call expression (e.g. (list (lambda () x))).
        binding_names = {name for name, _, _ in plan.bindings}

        # Pre-scan all binding value plans to find the highest let-binding slot
        # index used anywhere within them.  Temp slots for deferred closures
        # must be allocated above this high-water mark to avoid colliding with
        # slots pre-assigned by the IR addresser to lambda-lifter let bindings.
        # The scan must be fully recursive because the lambda lifter can produce
        # MenaiIRLet nodes nested inside calls, ifs, or other expressions
        # (e.g. (list (lambda () x)) lifts to a MenaiIRLet inside the LIST
        # call's argument list, not at the top level of the binding value).
        #
        # Lambda bodies are NOT descended into — those are separate frames.
        # The letrec's own binding slots are accounted for via ctx.max_locals.
        max_let_slot = -1
        for _, value_plan, _ in plan.bindings:
            max_let_slot = max(max_let_slot, self._max_let_slot(value_plan))

        # Phase 1: create all closures / binding values.  Any lambda that
        # captures a letrec sibling is intercepted by _generate_lambda and
        # deferred to Phase 2 via PATCH_CLOSURE.

        # Activate the interception context.  letrec_next_temp_slot is
        # initialised to one above the highest let-binding slot found in any
        # binding value plan (pre-scanned above), so temp slots never collide
        # with slots pre-assigned by the IR addresser to lambda-lifter helpers.
        ctx.letrec_sibling_names = binding_names
        ctx.letrec_deferred_patches = []
        # Temp slots start above the highest let-binding slot pre-scanned above,
        # and also above the current max_locals (letrec binding slots).  This
        # guarantees no collision with any slot already in use in this frame.
        ctx.letrec_next_temp_slot = max(ctx.max_locals, max_let_slot + 1)

        for _, value_plan, var_index in plan.bindings:
            self._generate_expr(value_plan, ctx)
            ctx.emit(Opcode.STORE_VAR, var_index)
            ctx.max_locals = max(ctx.max_locals, var_index + 1)

        # Deactivate the interception context before Phase 2.
        deferred = ctx.letrec_deferred_patches
        ctx.letrec_sibling_names = None
        ctx.letrec_deferred_patches = None
        ctx.letrec_next_temp_slot = None

        # Phase 2: patch all deferred closures.
        # Each entry is (closure_slot, lambda_plan) where closure_slot is the
        # local slot holding the closure (either a letrec slot or a temp slot
        # allocated during Phase 1).
        for closure_slot, lambda_plan in deferred:
            for capture_slot, _ in enumerate(lambda_plan.free_vars):
                fvp = lambda_plan.free_var_plans[capture_slot]
                self._generate_expr(fvp, ctx)
                ctx.emit(Opcode.PATCH_CLOSURE, closure_slot, capture_slot)

        # Generate body
        self._generate_expr(plan.body_plan, ctx)

    def _max_let_slot(self, plan: MenaiIRExpr) -> int:
        """
        Return the highest var_index found in any MenaiIRLet binding within
        *plan*, recursing into all node types but NOT into MenaiIRLambda bodies
        (those are a separate frame and have their own slot namespace).

        Used by _generate_letrec to pre-compute a safe base for temp slot
        allocation so that deferred-closure temp slots never collide with
        lambda-lifter helper slots pre-assigned by the IR addresser.
        """
        if isinstance(plan, MenaiIRLet):
            hi = max((vi for _, _, vi in plan.bindings), default=-1)
            hi = max(hi, self._max_let_slot(plan.body_plan))
            for _, vp, _ in plan.bindings:
                hi = max(hi, self._max_let_slot(vp))
            return hi
        if isinstance(plan, MenaiIRLetrec):
            hi = max((self._max_let_slot(vp) for _, vp, _ in plan.bindings), default=-1)
            hi = max(hi, self._max_let_slot(plan.body_plan))
            return hi
        if isinstance(plan, MenaiIRLambda):
            # Do NOT recurse into body — separate frame.
            # Do recurse into free_var_plans (evaluated in enclosing frame).
            return max((self._max_let_slot(p) for p in plan.free_var_plans), default=-1)
        if isinstance(plan, MenaiIRIf):
            return max(self._max_let_slot(plan.condition_plan),
                       self._max_let_slot(plan.then_plan),
                       self._max_let_slot(plan.else_plan))
        if isinstance(plan, MenaiIRCall):
            return max(self._max_let_slot(plan.func_plan),
                       max((self._max_let_slot(a) for a in plan.arg_plans), default=-1))
        if isinstance(plan, MenaiIRReturn):
            return self._max_let_slot(plan.value_plan)
        if isinstance(plan, MenaiIRTrace):
            return max(self._max_let_slot(plan.value_plan),
                       max((self._max_let_slot(m) for m in plan.message_plans), default=-1))
        # Leaf nodes: MenaiIRConstant, MenaiIRVariable, MenaiIRQuote,
        #             MenaiIREmptyList, MenaiIRError
        return -1

    def _find_effective_lambda(self, plan: MenaiIRExpr) -> 'MenaiIRLambda | None':
        """
        Find the lambda that produces the closure stored in a letrec slot.

        For a plain MenaiIRLambda, returns it directly.
        For a MenaiIRLet chain (as produced by the lambda lifter), follows the
        let body chain until it finds the wrapper MenaiIRLambda.
        Returns None if no lambda is found (e.g. non-lambda binding).
        """
        node = plan
        while isinstance(node, MenaiIRLet):
            node = node.body_plan
        if isinstance(node, MenaiIRLambda):
            return node
        return None

    def _generate_expr_with_full_deferral(
        self,
        plan: MenaiIRExpr,
        effective_lambda: MenaiIRLambda,
        ctx: MenaiCodeGenContext,
    ) -> None:
        """
        Generate code for a letrec binding value whose effective lambda has
        sibling free_vars.  ALL free_var captures are deferred to Phase 2 via
        PATCH_CLOSURE — capture_count=0 is emitted so the VM pre-allocates all
        slots as None.  This avoids ordering issues between sibling and
        non-sibling free_vars in the captured_values list.

        Walks any MenaiIRLet chain normally (the helper is fully closed and
        needs no special treatment).  When it reaches the effective lambda
        (the wrapper), emits no free_var_plans and calls _generate_letrec_lambda
        with capture_count=0.
        """
        if isinstance(plan, MenaiIRLet):
            # Generate the let's binding values and store them normally.
            for _, val_plan, var_index in plan.bindings:
                self._generate_expr(val_plan, ctx)
                ctx.emit(Opcode.STORE_VAR, var_index)
                ctx.max_locals = max(ctx.max_locals, var_index + 1)

            # Recurse into the body.
            self._generate_expr_with_full_deferral(plan.body_plan, effective_lambda, ctx)

        elif isinstance(plan, MenaiIRLambda) and plan is effective_lambda:
            # This is the wrapper lambda — emit NO free_var_plans; all slots
            # will be patched in Phase 2.
            self._generate_letrec_lambda(plan, 0, ctx)

        else:
            # Fallback: shouldn't happen but generate normally.
            self._generate_expr(plan, ctx)

    def _generate_lambda(self, plan: MenaiIRLambda, ctx: MenaiCodeGenContext) -> None:
        """Generate code for a lambda expression."""
        # Letrec interception: if we are currently generating a letrec binding
        # value AND this lambda captures any letrec sibling, defer ALL captures
        # to Phase 2 via PATCH_CLOSURE.
        #
        # We emit MAKE_CLOSURE with capture_count=0 (VM pre-allocates None slots),
        # store the closure in a fresh temp slot, record it for Phase 2, then
        # load it back so the surrounding expression (e.g. LIST) gets the value.
        if (ctx.letrec_sibling_names is not None
                and any(fv in ctx.letrec_sibling_names for fv in plan.free_vars)):
            # Allocate a temp slot for this closure.
            # letrec_next_temp_slot is pre-initialised in _generate_letrec to
            # sit above all letrec and let-binding slots in this group.
            assert ctx.letrec_next_temp_slot is not None
            temp_slot = ctx.letrec_next_temp_slot
            ctx.letrec_next_temp_slot += 1
            ctx.max_locals = max(ctx.max_locals, temp_slot + 1)

            # Emit the closure with capture_count=0; VM pre-allocates None slots.
            self._generate_letrec_lambda(plan, 0, ctx)

            # Store in temp slot and record for Phase 2.
            ctx.emit(Opcode.STORE_VAR, temp_slot)
            assert ctx.letrec_deferred_patches is not None
            ctx.letrec_deferred_patches.append((temp_slot, plan))

            # Load back so the surrounding expression receives the closure.
            ctx.emit(Opcode.LOAD_VAR, temp_slot)
            return

        # Emit code to load each free variable value (for capture).
        # After copy propagation these may be any trivially-copyable IR node,
        # not necessarily a MenaiIRVariable, so we use the general dispatcher.
        for free_var_plan in plan.free_var_plans:
            self._generate_expr(free_var_plan, ctx)

        # Create nested context for lambda body
        lambda_ctx = MenaiCodeGenContext()

        # Record this lambda's name so _generate_call can detect self-recursive
        # tail calls and emit JUMP 0 instead of TAIL_CALL.
        lambda_ctx.current_lambda_name = plan.binding_name

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

        # Add to parent's code objects list.  Always needed: the validator and
        # disassembler walk code_objects; MAKE_CLOSURE also references it by index.
        code_index = ctx.add_code_object(lambda_code)

        if len(plan.free_vars) != 0:
            # Has free vars — must capture values at runtime via MAKE_CLOSURE.
            ctx.emit(Opcode.MAKE_CLOSURE, code_index, len(plan.free_vars))
            return

        # No captures needed — pre-build the MenaiFunction and store it in
        # the constant pool.  Replaces a Python object allocation on every
        # execution of the enclosing code with a single constant-pool lookup.
        # MenaiFunction is not hashable (bytecode contains lists), so we
        # bypass add_constant and key by id(lambda_code) instead.
        func = MenaiFunction(
            parameters=tuple(lambda_code.param_names),
            name=lambda_code.name,
            bytecode=lambda_code,
            is_variadic=lambda_code.is_variadic,
        )
        key = ('function', id(lambda_code))
        if key not in ctx.constant_map:
            ctx.constant_map[key] = len(ctx.constants)
            ctx.constants.append(func)

        const_index = ctx.constant_map[key]
        ctx.emit(Opcode.LOAD_CONST, const_index)

    def _generate_letrec_lambda(
        self, plan: MenaiIRLambda, non_sibling_count: int, ctx: MenaiCodeGenContext
    ) -> None:
        """
        Emit the CodeObject and MAKE_CLOSURE instruction for a letrec lambda
        that has sibling free_vars requiring two-phase initialisation.

        The caller has already pushed `non_sibling_count` values onto the stack
        (the non-sibling free_var_plans).  This method emits the CodeObject and
        a MAKE_CLOSURE instruction with capture_count=non_sibling_count.

        The VM's _op_make_closure detects that capture_count < len(code.free_vars)
        and pre-allocates None slots for the remaining (sibling) entries, which
        PATCH_CLOSURE will fill in Phase 2.
        """
        # Build the nested CodeObject (body only — no free_var_plan emission here,
        # that was done by the caller for non-sibling vars).
        lambda_ctx = MenaiCodeGenContext()
        lambda_ctx.current_lambda_name = plan.binding_name

        if plan.params:
            lambda_ctx.emit(Opcode.ENTER, len(plan.params))

        lambda_ctx.max_locals = plan.max_locals
        self._generate_expr(plan.body_plan, lambda_ctx)

        if plan.binding_name:
            lambda_name = plan.binding_name
        else:
            lambda_name = f"<lambda-{self.lambda_counter}>"
            self.lambda_counter += 1

        param_word = "param" if len(plan.params) == 1 else "params"
        lambda_name = f"{lambda_name}({len(plan.params)} {param_word})"

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
            source_file=plan.source_file,
        )

        code_index = ctx.add_code_object(lambda_code)
        # capture_count = non_sibling_count < len(free_vars): VM pre-allocates
        # None slots for the remaining sibling entries.
        ctx.emit(Opcode.MAKE_CLOSURE, code_index, non_sibling_count)

    def _generate_call(self, plan: MenaiIRCall, ctx: MenaiCodeGenContext) -> None:
        """Generate code for a function call."""
        # Detect direct self-recursive tail calls and emit JUMP 0.
        # A call is a self-recursive tail call when:
        #   - it is in tail position
        #   - the callee is a local variable whose name matches the current lambda
        #   - the argument count matches the current lambda's param_count exactly
        #     (ensured by the IR builder: self-calls always pass the same arity)
        if (plan.is_tail_call
                and not plan.is_builtin
                and ctx.current_lambda_name is not None
                and isinstance(plan.func_plan, MenaiIRVariable)
                and plan.func_plan.var_type == 'local'
                and plan.func_plan.name == ctx.current_lambda_name):
            for arg_plan in plan.arg_plans:
                self._generate_expr(arg_plan, ctx)

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
