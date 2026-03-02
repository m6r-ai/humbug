"""
Menai code generator - generates bytecode from IR.

Receives fully-addressed IR from MenaiIRAddresser.  All MenaiIRVariable nodes
have resolved depth/index; all MenaiIRLet/MenaiIRLetrec binding tuples carry
(name, value_plan, slot) as produced by the addresser.
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
    constant_map: Dict[tuple, int] = field(default_factory=dict)
    name_map: Dict[str, int] = field(default_factory=dict)
    code_objects: List[CodeObject] = field(default_factory=list)
    max_locals: int = 0
    current_lambda_name: str | None = None  # Used by _generate_call to detect
                                            # self-recursive tail calls (JUMP 0).

    # Letrec two-phase support: when non-None, _generate_lambda intercepts any
    # lambda whose sibling_free_vars intersect this set and defers all captures
    # to Phase 2 via PATCH_CLOSURE.
    letrec_sibling_names: 'set | None' = None

    # List of (closure_slot, lambda_plan) for closures deferred to Phase 2.
    letrec_deferred_patches: 'List[tuple] | None' = None

    def add_constant(self, value: MenaiValue) -> int:
        """Add constant to pool and return its index."""
        if isinstance(value, (MenaiInteger, MenaiFloat, MenaiComplex, MenaiBoolean, MenaiString)):
            key: Any = (type(value).__name__, value.value)
        else:
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

    This is a "dumb" code generator — it just follows the plan without
    performing any analysis.  All slot indices and max_locals values are
    pre-computed by MenaiIRAddresser before this pass runs.
    """

    def __init__(self) -> None:
        self.lambda_counter = 0

    def generate(self, plan: MenaiIRExpr, name: str = "<module>") -> CodeObject:
        """Generate bytecode from a compilation plan."""
        ctx = MenaiCodeGenContext()
        self._generate_expr(plan, ctx)
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
        if isinstance(plan.value, MenaiNone):
            ctx.emit(Opcode.LOAD_NONE)
            return

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
            name_index = ctx.add_name(plan.name)
            ctx.emit(Opcode.LOAD_NAME, name_index)

    def _generate_if(self, plan: MenaiIRIf, ctx: MenaiCodeGenContext) -> None:
        """Generate code for an if expression."""
        self._generate_expr(plan.condition_plan, ctx)
        jump_to_else = ctx.emit(Opcode.JUMP_IF_FALSE, 0)
        self._generate_expr(plan.then_plan, ctx)

        then_terminates = False
        if ctx.instructions:
            last_op = ctx.instructions[-1].opcode
            if last_op in (Opcode.RETURN, Opcode.TAIL_CALL, Opcode.TAIL_APPLY, Opcode.JUMP, Opcode.RAISE_ERROR):
                then_terminates = True

        jump_past_else = None if then_terminates else ctx.emit(Opcode.JUMP, 0)
        else_start = ctx.current_instruction_index()
        ctx.patch_jump(jump_to_else, else_start)
        self._generate_expr(plan.else_plan, ctx)

        if jump_past_else is not None:
            ctx.patch_jump(jump_past_else, ctx.current_instruction_index())

    def _generate_quote(self, plan: MenaiIRQuote, ctx: MenaiCodeGenContext) -> None:
        """Generate code for a quote expression."""
        const_index = ctx.add_constant(plan.quoted_value)
        ctx.emit(Opcode.LOAD_CONST, const_index)

    def _generate_error(self, plan: MenaiIRError, ctx: MenaiCodeGenContext) -> None:
        """Generate code for an error expression."""
        const_index = ctx.add_constant(plan.message)
        ctx.emit(Opcode.RAISE_ERROR, const_index)

    def _generate_let(self, plan: MenaiIRLet, ctx: MenaiCodeGenContext) -> None:
        """
        Generate code for a let expression.

        Binding tuples are (name, value_plan, slot) after MenaiIRAddresser.
        """
        for binding in plan.bindings:
            _, value_plan, var_index = binding  # type: ignore[misc]
            self._generate_expr(value_plan, ctx)
            ctx.emit(Opcode.STORE_VAR, var_index)
            ctx.max_locals = max(ctx.max_locals, var_index + 1)

        self._generate_expr(plan.body_plan, ctx)

    def _generate_letrec(self, plan: MenaiIRLetrec, ctx: MenaiCodeGenContext) -> None:
        """
        Generate code for a letrec expression.

        Binding tuples are (name, value_plan, slot) after MenaiIRAddresser.

        Two-phase approach for mutual recursion:
        Phase 1 — emit each closure skeleton with MAKE_CLOSURE(count=0) and
                   STORE_VAR.  Any lambda capturing a letrec sibling is deferred.
        Phase 2 — PATCH_CLOSURE fills in the captured values.
        """
        # Collect binding names for sibling-capture interception.
        binding_names = {name for name, *_ in plan.bindings}

        # Pre-scan all slot indices in this letrec (including inner let slots
        # embedded by the addresser) and prime ctx.max_locals to the high-water
        # mark before Phase 1 starts.  This ensures that when _generate_lambda
        # allocates a temp slot for a deferred closure (temp_slot = ctx.max_locals),
        # it lands above every slot the addresser has already assigned in this
        # frame — including helper-let slots inside binding value expressions that
        # haven't been emitted yet.
        max_slot = self._max_addressed_slot(plan)
        ctx.max_locals = max(ctx.max_locals, max_slot + 1)

        # Phase 1: create all closures / binding values.  Any lambda that
        # captures a letrec sibling is intercepted by _generate_lambda and
        # deferred to Phase 2 via PATCH_CLOSURE.
        ctx.letrec_sibling_names = binding_names
        ctx.letrec_deferred_patches = []

        for binding in plan.bindings:
            _, value_plan, var_index = binding  # type: ignore[misc]
            self._generate_expr(value_plan, ctx)
            ctx.emit(Opcode.STORE_VAR, var_index)
            ctx.max_locals = max(ctx.max_locals, var_index + 1)

        # Deactivate interception before Phase 2.
        deferred = ctx.letrec_deferred_patches
        ctx.letrec_sibling_names = None
        ctx.letrec_deferred_patches = None

        # Phase 2: patch all deferred closures.
        for closure_slot, lambda_plan in deferred:
            all_plans = lambda_plan.sibling_free_var_plans + lambda_plan.outer_free_var_plans
            for capture_slot, fvp in enumerate(all_plans):
                self._generate_expr(fvp, ctx)
                ctx.emit(Opcode.PATCH_CLOSURE, closure_slot, capture_slot)

        self._generate_expr(plan.body_plan, ctx)

    def _generate_lambda(self, plan: MenaiIRLambda, ctx: MenaiCodeGenContext) -> None:
        """Generate code for a lambda expression."""
        # Letrec interception: if we are currently generating a letrec binding
        # value AND this lambda captures any letrec sibling, defer ALL captures
        # to Phase 2 via PATCH_CLOSURE.
        #
        # We emit MAKE_CLOSURE with capture_count=0 (VM pre-allocates None slots),
        # store the closure in a fresh temp slot above max_locals, record it for
        # Phase 2, then load it back so the surrounding expression gets the value.
        if (ctx.letrec_sibling_names is not None
                and len(plan.sibling_free_vars) > 0):
            # Allocate a temp slot above all currently-known slots in this frame.
            temp_slot = ctx.max_locals
            ctx.max_locals = temp_slot + 1

            # Emit the closure skeleton; VM pre-allocates None capture slots.
            self._generate_letrec_lambda(plan, 0, ctx)

            ctx.emit(Opcode.STORE_VAR, temp_slot)
            assert ctx.letrec_deferred_patches is not None
            ctx.letrec_deferred_patches.append((temp_slot, plan))

            # Load back so the surrounding expression receives the closure.
            ctx.emit(Opcode.LOAD_VAR, temp_slot)
            return

        # Emit code to load each free variable value (for capture).
        # After copy propagation these may be any trivially-copyable IR node,
        # not necessarily a MenaiIRVariable, so we use the general dispatcher.
        for free_var_plan in plan.sibling_free_var_plans + plan.outer_free_var_plans:
            self._generate_expr(free_var_plan, ctx)

        # Create nested context for lambda body.
        lambda_ctx = MenaiCodeGenContext()
        lambda_ctx.current_lambda_name = plan.binding_name

        if plan.params:
            lambda_ctx.emit(Opcode.ENTER, len(plan.params))

        # max_locals is set by MenaiIRAddresser.
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
            free_vars=plan.sibling_free_vars + plan.outer_free_vars,
            param_names=plan.params,
            param_count=plan.param_count,
            local_count=lambda_ctx.max_locals,
            is_variadic=plan.is_variadic,
            name=lambda_name,
            source_line=plan.source_line,
            source_file=plan.source_file
        )

        code_index = ctx.add_code_object(lambda_code)

        # If we have free variables to capture, emit MAKE_CLOSURE with the code index and capture count.
        if len(plan.sibling_free_vars) + len(plan.outer_free_vars) != 0:
            ctx.emit(Opcode.MAKE_CLOSURE, code_index, len(plan.sibling_free_vars) + len(plan.outer_free_vars))
            return

        # No captures — pre-build MenaiFunction and store in constant pool.
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

        ctx.emit(Opcode.LOAD_CONST, ctx.constant_map[key])

    def _generate_letrec_lambda(
        self, plan: MenaiIRLambda, pre_captured_count: int, ctx: MenaiCodeGenContext
    ) -> None:
        """
        Emit the CodeObject and MAKE_CLOSURE instruction for a letrec lambda
        that has sibling free_vars requiring two-phase initialisation.

        pre_captured_count is always 0 in the current deferred path — all
        captures are deferred to Phase 2.  The VM pre-allocates None slots for
        the remaining free_vars entries, which PATCH_CLOSURE fills in.
        """
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
            free_vars=plan.sibling_free_vars + plan.outer_free_vars,
            param_names=plan.params,
            param_count=plan.param_count,
            local_count=lambda_ctx.max_locals,
            is_variadic=plan.is_variadic,
            name=lambda_name,
            source_line=plan.source_line,
            source_file=plan.source_file,
        )

        code_index = ctx.add_code_object(lambda_code)
        ctx.emit(Opcode.MAKE_CLOSURE, code_index, pre_captured_count)

    def _max_addressed_slot(self, plan: 'MenaiIRExpr') -> int:
        """
        Return the highest slot index found in any MenaiIRLet or MenaiIRLetrec
        binding within *plan*, recursing into all node types but NOT into
        MenaiIRLambda bodies (those are separate frames with their own slots).

        After MenaiIRAddresser, binding tuples are (name, value_plan, slot).
        This method reads the slot from index 2 of each tuple.

        Used by _generate_letrec to pre-prime ctx.max_locals before Phase 1
        so that temp slots for deferred closures never collide with slots
        already assigned by the addresser to inner let/letrec bindings.
        """
        if isinstance(plan, MenaiIRLet):
            hi = max((t[2] for t in plan.bindings), default=-1)  # type: ignore[index, misc]
            hi = max(hi, self._max_addressed_slot(plan.body_plan))
            for _, vp, *_ in plan.bindings:
                hi = max(hi, self._max_addressed_slot(vp))

            return hi

        if isinstance(plan, MenaiIRLetrec):
            hi = max((t[2] for t in plan.bindings), default=-1)  # type: ignore[index, misc]
            hi = max(hi, self._max_addressed_slot(plan.body_plan))
            for _, vp, *_ in plan.bindings:
                hi = max(hi, self._max_addressed_slot(vp))

            return hi

        if isinstance(plan, MenaiIRLambda):
            # Do NOT recurse into body — separate frame.
            # Do recurse into free_var_plans (evaluated in enclosing frame).
            all_fvp = plan.sibling_free_var_plans + plan.outer_free_var_plans
            return max((self._max_addressed_slot(p) for p in all_fvp), default=-1)

        if isinstance(plan, MenaiIRIf):
            return max(self._max_addressed_slot(plan.condition_plan),
                       self._max_addressed_slot(plan.then_plan),
                       self._max_addressed_slot(plan.else_plan))

        if isinstance(plan, MenaiIRCall):
            return max(self._max_addressed_slot(plan.func_plan),
                       max((self._max_addressed_slot(a) for a in plan.arg_plans), default=-1))

        if isinstance(plan, MenaiIRReturn):
            return self._max_addressed_slot(plan.value_plan)

        if isinstance(plan, MenaiIRTrace):
            return max(self._max_addressed_slot(plan.value_plan),
                       max((self._max_addressed_slot(m) for m in plan.message_plans), default=-1))

        # Leaf nodes: MenaiIRConstant, MenaiIRVariable, MenaiIRQuote, MenaiIREmptyList, MenaiIRError
        return -1

    def _generate_call(self, plan: MenaiIRCall, ctx: MenaiCodeGenContext) -> None:
        """Generate code for a function call."""
        # Detect direct self-recursive tail calls and emit JUMP 0.
        jump_to_self = (plan.is_tail_call  # type: ignore[too-many-boolean-expressions]
                        and not plan.is_builtin
                        and ctx.current_lambda_name is not None
                        and isinstance(plan.func_plan, MenaiIRVariable)
                        and plan.func_plan.var_type == 'local'
                        and plan.func_plan.name == ctx.current_lambda_name)
        if jump_to_self:
            for arg_plan in plan.arg_plans:
                self._generate_expr(arg_plan, ctx)

            ctx.emit(Opcode.JUMP, 0)
            return

        if plan.is_builtin:
            assert plan.builtin_name is not None
            builtin_name = plan.builtin_name

            if builtin_name == 'range':
                for arg_plan in plan.arg_plans:
                    self._generate_expr(arg_plan, ctx)

                if len(plan.arg_plans) == 2:
                    ctx.emit(Opcode.LOAD_CONST, ctx.add_constant(MenaiInteger(1)))

                ctx.emit(Opcode.RANGE)
                return

            if builtin_name == 'integer->complex':
                self._generate_expr(plan.arg_plans[0], ctx)
                if len(plan.arg_plans) == 1:
                    ctx.emit(Opcode.LOAD_CONST, ctx.add_constant(MenaiInteger(0)))

                else:
                    self._generate_expr(plan.arg_plans[1], ctx)

                ctx.emit(Opcode.INTEGER_TO_COMPLEX)
                return

            if builtin_name == 'integer->string':
                self._generate_expr(plan.arg_plans[0], ctx)
                if len(plan.arg_plans) == 1:
                    ctx.emit(Opcode.LOAD_CONST, ctx.add_constant(MenaiInteger(10)))

                else:
                    self._generate_expr(plan.arg_plans[1], ctx)

                ctx.emit(Opcode.INTEGER_TO_STRING)
                return

            if builtin_name == 'float->complex':
                self._generate_expr(plan.arg_plans[0], ctx)
                if len(plan.arg_plans) == 1:
                    ctx.emit(Opcode.LOAD_CONST, ctx.add_constant(MenaiFloat(0.0)))

                else:
                    self._generate_expr(plan.arg_plans[1], ctx)

                ctx.emit(Opcode.FLOAT_TO_COMPLEX)
                return

            if builtin_name == 'string->integer':
                self._generate_expr(plan.arg_plans[0], ctx)
                if len(plan.arg_plans) == 1:
                    ctx.emit(Opcode.LOAD_CONST, ctx.add_constant(MenaiInteger(10)))

                else:
                    self._generate_expr(plan.arg_plans[1], ctx)

                ctx.emit(Opcode.STRING_TO_INTEGER)
                return

            if builtin_name == 'string-slice':
                self._generate_expr(plan.arg_plans[0], ctx)
                self._generate_expr(plan.arg_plans[1], ctx)
                if len(plan.arg_plans) == 2:
                    self._generate_expr(plan.arg_plans[0], ctx)
                    ctx.emit(Opcode.STRING_LENGTH)

                else:
                    self._generate_expr(plan.arg_plans[2], ctx)

                ctx.emit(Opcode.STRING_SLICE)
                return

            if builtin_name == 'string->list':
                self._generate_expr(plan.arg_plans[0], ctx)
                if len(plan.arg_plans) == 1:
                    ctx.emit(Opcode.LOAD_CONST, ctx.add_constant(MenaiString("")))

                else:
                    self._generate_expr(plan.arg_plans[1], ctx)

                ctx.emit(Opcode.STRING_TO_LIST)
                return

            if builtin_name == 'list-slice':
                self._generate_expr(plan.arg_plans[0], ctx)
                self._generate_expr(plan.arg_plans[1], ctx)
                if len(plan.arg_plans) == 2:
                    self._generate_expr(plan.arg_plans[0], ctx)
                    ctx.emit(Opcode.LIST_LENGTH)

                else:
                    self._generate_expr(plan.arg_plans[2], ctx)

                ctx.emit(Opcode.LIST_SLICE)
                return

            if builtin_name == 'list->string':
                self._generate_expr(plan.arg_plans[0], ctx)
                if len(plan.arg_plans) == 1:
                    ctx.emit(Opcode.LOAD_CONST, ctx.add_constant(MenaiString("")))

                else:
                    self._generate_expr(plan.arg_plans[1], ctx)

                ctx.emit(Opcode.LIST_TO_STRING)
                return

            if builtin_name == 'dict-get':
                for arg_plan in plan.arg_plans:
                    self._generate_expr(arg_plan, ctx)

                if len(plan.arg_plans) == 2:
                    ctx.emit(Opcode.LOAD_NONE)

                ctx.emit(Opcode.DICT_GET)
                return

            for arg_plan in plan.arg_plans:
                self._generate_expr(arg_plan, ctx)

            if builtin_name == 'apply':
                ctx.emit(Opcode.TAIL_APPLY if plan.is_tail_call else Opcode.APPLY)
                return

            if builtin_name in BINARY_OPS:
                ctx.emit(BINARY_OPS[builtin_name])
                return

            if builtin_name in UNARY_OPS:
                ctx.emit(UNARY_OPS[builtin_name])
                return

            if builtin_name in TERNARY_OPS:
                ctx.emit(TERNARY_OPS[builtin_name])
                return

            assert builtin_name in BUILD_OPS, f"No opcode for '{builtin_name}' with {len(plan.arg_plans)} args"
            ctx.emit(BUILD_OPS[builtin_name], len(plan.arg_plans))
            return

        # Regular function call: args first, function on top.
        for arg_plan in plan.arg_plans:
            self._generate_expr(arg_plan, ctx)

        self._generate_expr(plan.func_plan, ctx)

        if plan.is_tail_call:
            ctx.emit(Opcode.TAIL_CALL, len(plan.arg_plans))

        else:
            ctx.emit(Opcode.CALL, len(plan.arg_plans))

    def _generate_empty_list(self, _plan: MenaiIREmptyList, ctx: MenaiCodeGenContext) -> None:
        """Generate code for an empty list literal."""
        ctx.emit(Opcode.LOAD_EMPTY_LIST)

    def _generate_return(self, plan: MenaiIRReturn, ctx: MenaiCodeGenContext) -> None:
        """Generate code for a return statement."""
        self._generate_expr(plan.value_plan, ctx)
        ctx.emit(Opcode.RETURN)

    def _generate_trace(self, plan: MenaiIRTrace, ctx: MenaiCodeGenContext) -> None:
        """Generate code for a trace expression."""
        for message_plan in plan.message_plans:
            self._generate_expr(message_plan, ctx)
            ctx.emit(Opcode.EMIT_TRACE)

        self._generate_expr(plan.value_plan, ctx)
