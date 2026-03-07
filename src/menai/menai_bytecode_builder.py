"""
VM code generator for the Menai compiler.

Translates a MenaiCFGFunction (SSA CFG) into a CodeObject ready for execution
by the Menai VM.
"""

from dataclasses import dataclass, field
from typing import Dict, List, Optional, Tuple

from menai.menai_bytecode import BUILTIN_OPCODE_MAP, CodeObject, Instruction, Opcode
from menai.menai_cfg import (
    MenaiCFGApplyInstr,
    MenaiCFGBlock,
    MenaiCFGBranchTerm,
    MenaiCFGBuiltinInstr,
    MenaiCFGCallInstr,
    MenaiCFGConstInstr,
    MenaiCFGFreeVarInstr,
    MenaiCFGFunction,
    MenaiCFGGlobalInstr,
    MenaiCFGInstr,
    MenaiCFGJumpTerm,
    MenaiCFGMakeClosureInstr,
    MenaiCFGPatchClosureInstr,
    MenaiCFGParamInstr,
    MenaiCFGPhiInstr,
    MenaiCFGRaiseTerm,
    MenaiCFGReturnTerm,
    MenaiCFGSelfLoopTerm,
    MenaiCFGTailApplyTerm,
    MenaiCFGTailCallTerm,
    MenaiCFGTerminator,
    MenaiCFGTraceInstr,
    MenaiCFGValue,
)
from menai.menai_value import (
    MenaiBoolean,
    MenaiComplex,
    MenaiDict,
    MenaiFloat,
    MenaiFunction,
    MenaiInteger,
    MenaiList,
    MenaiNone,
    MenaiString,
    MenaiValue,
)


# Derived opcode maps built from the single source of truth in BUILTIN_OPCODE_MAP.
UNARY_OPS  = {name: op for name, (op, arity) in BUILTIN_OPCODE_MAP.items() if arity == 1}
BINARY_OPS = {name: op for name, (op, arity) in BUILTIN_OPCODE_MAP.items() if arity == 2}
TERNARY_OPS = {name: op for name, (op, arity) in BUILTIN_OPCODE_MAP.items() if arity == 3}


@dataclass
class _EmitContext:
    """
    Mutable state for emitting one MenaiCFGFunction into a CodeObject.

    Tracks the instruction stream, constant/name pools, nested code objects,
    and the SSA-value-to-slot mapping.
    """
    instructions: List[Instruction] = field(default_factory=list)
    constants: List[MenaiValue] = field(default_factory=list)
    names: List[str] = field(default_factory=list)
    code_objects: List[CodeObject] = field(default_factory=list)
    constant_map: Dict[tuple, int] = field(default_factory=dict)
    name_map: Dict[str, int] = field(default_factory=dict)

    # SSA value id → local slot index
    slot_map: Dict[int, int] = field(default_factory=dict)
    next_slot: int = 0

    # Phi result id → slot, populated lazily on first store by a predecessor.
    # Kept separate so we can assert that a phi slot is allocated before the
    # join block tries to read it (via slot_of on the phi result).
    phi_slot_map: Dict[int, int] = field(default_factory=dict)

    # Value ids whose result is used only as a single phi incoming.  At
    # phi-store time the defining instruction is emitted directly into the phi
    # slot; during normal block emission the instruction is skipped so that no
    # intermediate slot is allocated.
    phi_sink: Dict[int, 'MenaiCFGInstr'] = field(default_factory=dict)

    # Name of the current function (for self-loop detection label)
    current_lambda_name: Optional[str] = None

    def alloc_slot(self, value: MenaiCFGValue) -> int:
        """Allocate the next free slot for `value` and record the mapping."""
        slot = self.next_slot
        self.next_slot += 1
        self.slot_map[value.id] = slot
        return slot

    def assign_slot(self, value: MenaiCFGValue, slot: int) -> None:
        """Assign a specific slot to `value` (used for params/free-vars)."""
        self.slot_map[value.id] = slot
        if slot >= self.next_slot:
            self.next_slot = slot + 1

    def slot_of(self, value: MenaiCFGValue) -> int:
        """Return the slot assigned to `value`.  Asserts it exists."""
        assert value.id in self.slot_map, (
            f"MenaiVMCodeGen: SSA value {value} has no assigned slot"
        )
        return self.slot_map[value.id]

    def ensure_slot(self, value: MenaiCFGValue) -> int:
        """
        Return a register slot for `value`, materialising it if necessary.

        For values with an existing slot: returns the existing slot.
        For values without a slot (should not occur in normal operation):
        allocates a fresh slot and emits MOVE from slot 0 as a safe fallback.
        """
        if value.id in self.slot_map:
            return self.slot_map[value.id]

        # Fallback: should not occur with register-based ops; copy from slot 0.
        slot = self.alloc_slot(value)
        self.emit(Opcode.MOVE, slot, dest=slot)
        return slot

    def emit(self, opcode: Opcode, src0: int = 0, src1: int = 0, dest: int = 0, src2: int = 0) -> int:
        """Emit an instruction, returning its index."""
        idx = len(self.instructions)
        self.instructions.append(Instruction(opcode, dest=dest, src0=src0, src1=src1, src2=src2))
        return idx

    def load_value(self, value: MenaiCFGValue) -> None:
        """
        Push `value` onto the call stack.

        All values have a slot; emit PUSH <slot>.
        """
        self.emit(Opcode.PUSH, self.slot_of(value))

    def emit_constant(self, value: MenaiValue, dest: int) -> None:
        """Emit the appropriate LOAD instruction writing directly to register dest."""
        if isinstance(value, MenaiNone):
            self.emit(Opcode.LOAD_NONE, dest=dest)
            return

        if isinstance(value, MenaiBoolean):
            self.emit(Opcode.LOAD_TRUE if value.value else Opcode.LOAD_FALSE, dest=dest)
            return

        if isinstance(value, MenaiList) and len(value.elements) == 0:
            self.emit(Opcode.LOAD_EMPTY_LIST, dest=dest)
            return

        if isinstance(value, MenaiDict) and len(value.pairs) == 0:
            self.emit(Opcode.LOAD_EMPTY_DICT, dest=dest)
            return

        const_idx = self.add_constant(value)
        self.emit(Opcode.LOAD_CONST, const_idx, dest=dest)

    def patch_jump(self, instr_index: int, target: int, src0: str = 'src0') -> None:
        """
        Back-patch a jump target into the given field of an already-emitted instruction.

        JUMP stores its target in src0.
        JUMP_IF_FALSE / JUMP_IF_TRUE store the condition in src0 and the target in src1.
        """
        setattr(self.instructions[instr_index], src0, target)

    def current_index(self) -> int:
        """Return the instruction index of the next instruction to be emitted."""
        return len(self.instructions)

    def add_constant(self, value: MenaiValue) -> int:
        """Add `value` to the constant pool if not already present, and return its index."""
        if isinstance(value, (MenaiInteger, MenaiFloat, MenaiComplex,
                               MenaiBoolean, MenaiString)):
            key: tuple = (type(value).__name__, value.value)

        else:
            key = (id(value),)

        if key in self.constant_map:
            return self.constant_map[key]

        idx = len(self.constants)
        self.constants.append(value)
        self.constant_map[key] = idx
        return idx

    def add_name(self, name: str) -> int:
        """Add `name` to the name pool if not already present, and return its index."""
        if name in self.name_map:
            return self.name_map[name]

        idx = len(self.names)
        self.names.append(name)
        self.name_map[name] = idx
        return idx

    def add_code_object(self, code_obj: CodeObject) -> int:
        """Add `code_obj` to the code object pool and return its index."""
        idx = len(self.code_objects)
        self.code_objects.append(code_obj)
        return idx


class MenaiBytecodeBuilder:
    """
    Generates a CodeObject from a MenaiCFGFunction.

    Usage::

        code_obj = MenaiVMCodeGen().generate(cfg_function, name="<module>")
    """

    def __init__(self) -> None:
        self._lambda_counter = 0

    def build(self, func: MenaiCFGFunction, name: str = "<module>") -> CodeObject:
        """
        Generate a top-level CodeObject from a MenaiCFGFunction.

        Args:
            func: The CFG function to compile (top-level module body).
            name: Name for the resulting CodeObject.

        Returns:
            A CodeObject ready for execution by the Menai VM.
        """
        ctx = _EmitContext(current_lambda_name=func.binding_name)
        self._emit_function_body(func, ctx)
        return CodeObject(
            instructions=ctx.instructions,
            constants=ctx.constants,
            names=ctx.names,
            code_objects=ctx.code_objects,
            param_count=0,
            local_count=ctx.next_slot,
            name=name,
        )

    def _emit_function_body(self, func: MenaiCFGFunction, ctx: _EmitContext) -> None:
        """
        Emit all blocks of `func` into `ctx` in reverse post-order (RPO).

        Phase 1: assign fixed slots to all params and free-var values.
        Phase 2: emit each block's instructions, patch_instrs, and terminator.
        Phase 3: back-patch all forward jump targets.

        RPO guarantees that all predecessors of a block are emitted before
        the block itself (for non-back-edges / self-loops).  This is required
        for the lazy phi-slot allocation scheme: when a predecessor emits its
        JumpTerm, it calls _emit_phi_stores_for_successor which allocates the
        phi slot; by the time the join block is emitted, the phi slot exists.
        """
        # Phase 1: assign fixed slots to params and free-vars only.
        # Phi slots are allocated lazily in _emit_phi_stores_for_successor.
        param_count = len(func.params)
        for block in func.blocks:
            for instr in block.instrs:
                if isinstance(instr, MenaiCFGParamInstr):
                    ctx.assign_slot(instr.result, instr.index)

                elif isinstance(instr, MenaiCFGFreeVarInstr):
                    ctx.assign_slot(instr.result, param_count + instr.index)

        # Phase 1b: identify phi-sink values — ConstInstr or GlobalInstr
        # results that are used only as a single phi incoming.  These will be
        # emitted directly into the phi slot at phi-store time, eliminating the
        # intermediate slot and the MOVE that would otherwise copy into it.
        ctx.phi_sink = _build_phi_sink(func)

        # Phase 2: emit blocks.
        # We need to back-patch jump targets, so we collect
        # (instr_index, target_block) pairs as we go.
        block_start: Dict[int, int] = {}   # block id → instruction index of first instr
        forward_jumps: List[Tuple[int, MenaiCFGBlock, str]] = []  # (instr_idx, target_block, patch_field)

        rpo = self._rpo(func)
        for i, block in enumerate(rpo):
            next_block = rpo[i + 1] if i + 1 < len(rpo) else None
            block_start[block.id] = ctx.current_index()
            self._emit_block(block, ctx, forward_jumps, next_block)

        # Phase 3: back-patch forward jumps.
        for instr_idx, target_block, patch_field in forward_jumps:
            ctx.patch_jump(instr_idx, block_start[target_block.id], patch_field)

    def _rpo(self, func: MenaiCFGFunction) -> List['MenaiCFGBlock']:
        """
        Return the reachable blocks of `func` in reverse post-order.

        RPO is computed by DFS from the entry block, collecting blocks in
        post-order (appended after all successors are visited), then reversing.
        Self-loop back-edges (SelfLoopTerm → entry) are not followed.
        Unreachable blocks (no path from entry) are excluded.
        """
        visited: set = set()
        post_order: List[MenaiCFGBlock] = []

        def dfs(block: MenaiCFGBlock) -> None:
            if block.id in visited:
                return

            visited.add(block.id)
            term = block.terminator
            if isinstance(term, MenaiCFGJumpTerm):
                dfs(term.target)

            elif isinstance(term, MenaiCFGBranchTerm):
                dfs(term.true_block)
                dfs(term.false_block)
            # ReturnTerm, TailCallTerm, TailApplyTerm, SelfLoopTerm, RaiseTerm
            # have no successors (SelfLoopTerm loops to entry but we don't
            # follow it — it's a back-edge handled by JUMP 0).
            post_order.append(block)

        dfs(func.entry)
        post_order.reverse()
        return post_order

    def _emit_block(
        self,
        block: MenaiCFGBlock,
        ctx: _EmitContext,
        forward_jumps: List[Tuple[int, MenaiCFGBlock, str]],
        next_block: Optional[MenaiCFGBlock],
    ) -> None:
        """Emit all instructions and the terminator for one block."""
        # Regular instructions.
        for instr in block.instrs:
            self._emit_instr(instr, ctx)

        # Patch instructions (letrec closure fixup).
        for patch in block.patch_instrs:
            self._emit_patch(patch, ctx)

        # Terminator.
        assert block.terminator is not None, (
            f"MenaiVMCodeGen: block {block.id} ({block.label}) has no terminator"
        )
        self._emit_terminator(block, block.terminator, ctx, forward_jumps, next_block)

    def _emit_instr(self, instr: MenaiCFGInstr, ctx: _EmitContext) -> None:
        """Emit a single non-terminator instruction."""
        # Phi-sink values are emitted directly into the phi slot at phi-store
        # time.  Skip them here so no intermediate slot is allocated.
        result = _result_of(instr)
        if result is not None and result.id in ctx.phi_sink:
            return

        if isinstance(instr, MenaiCFGParamInstr):
            # Params are already in their slots via ENTER; nothing to emit.
            return

        if isinstance(instr, MenaiCFGFreeVarInstr):
            # Free vars are in their slots after ENTER loads them; nothing to emit.
            return

        if isinstance(instr, MenaiCFGPhiInstr):
            # Phi nodes emit no instructions.  The phi result slot is
            # allocated lazily by _emit_phi_stores_for_successor when a
            # predecessor stores into it.  By the time the join block's
            # instructions run, the slot is guaranteed to exist in slot_map.
            if instr.result.id in ctx.phi_slot_map:
                ctx.slot_map[instr.result.id] = ctx.phi_slot_map[instr.result.id]
            return

        if isinstance(instr, MenaiCFGConstInstr):
            slot = ctx.alloc_slot(instr.result)
            ctx.emit_constant(instr.value, dest=slot)
            return

        if isinstance(instr, MenaiCFGGlobalInstr):
            name_idx = ctx.add_name(instr.name)
            slot = ctx.alloc_slot(instr.result)
            ctx.emit(Opcode.LOAD_NAME, name_idx, dest=slot)
            return

        if isinstance(instr, MenaiCFGBuiltinInstr):
            self._emit_builtin(instr, ctx)
            return

        if isinstance(instr, MenaiCFGCallInstr):
            for arg in instr.args:
                ctx.load_value(arg)
            ctx.load_value(instr.func)
            dest = ctx.alloc_slot(instr.result)
            ctx.emit(Opcode.CALL, len(instr.args), dest=dest)
            return

        if isinstance(instr, MenaiCFGApplyInstr):
            ctx.load_value(instr.func)
            ctx.load_value(instr.arg_list)
            dest = ctx.alloc_slot(instr.result)
            ctx.emit(Opcode.APPLY, dest=dest)
            return

        if isinstance(instr, MenaiCFGMakeClosureInstr):
            self._emit_make_closure(instr, ctx)
            return

        if isinstance(instr, MenaiCFGTraceInstr):
            for msg in instr.messages:
                msg_slot = ctx.ensure_slot(msg)
                ctx.emit(Opcode.EMIT_TRACE, msg_slot)

            result_slot = ctx.ensure_slot(instr.value)
            ctx.slot_map[instr.result.id] = result_slot
            return

        if isinstance(instr, MenaiCFGPatchClosureInstr):
            # Patch instructions may appear in block.instrs (letrec sibling
            # captures emitted before the body) as well as block.patch_instrs.
            # Delegate to the shared helper.
            self._emit_patch(instr, ctx)
            return

        raise TypeError(f"MenaiVMCodeGen: unhandled instruction {type(instr).__name__}")

    def _emit_phi_stores_for_successor(
        self,
        current_block: MenaiCFGBlock,
        successor: MenaiCFGBlock,
        ctx: _EmitContext,
    ) -> None:
        """
        Before jumping to `successor`, store each incoming phi value that
        this block contributes into the phi's pre-assigned slot.

        Called by jump/branch terminator emission immediately before the
        JUMP instruction so that the incoming value's slot is guaranteed
        to exist (it was defined earlier in this block's instruction stream).
        """
        for instr in successor.instrs:
            if not isinstance(instr, MenaiCFGPhiInstr):
                break

            for incoming_val, pred_block in instr.incoming:
                if pred_block.id == current_block.id:
                    # Allocate the phi slot lazily on first store.
                    if instr.result.id not in ctx.phi_slot_map:
                        phi_slot = ctx.alloc_slot(instr.result)
                        ctx.phi_slot_map[instr.result.id] = phi_slot

                    else:
                        phi_slot = ctx.phi_slot_map[instr.result.id]

                    if incoming_val.id in ctx.phi_sink:
                        # The incoming value is a phi-sink: its defining
                        # instruction was skipped during normal block emission.
                        # Emit the defining instruction directly into the phi
                        # slot, bypassing the phi-sink skip guard.
                        ctx.slot_map[incoming_val.id] = phi_slot
                        self._emit_sink_instr(ctx.phi_sink[incoming_val.id], phi_slot, ctx)
                    else:
                        src_slot = ctx.slot_of(incoming_val)
                        if src_slot != phi_slot:
                            ctx.emit(Opcode.MOVE, src_slot, dest=phi_slot)

    def _emit_sink_instr(
        self, instr: MenaiCFGInstr, dest: int, ctx: _EmitContext
    ) -> None:
        """
        Emit a phi-sink instruction directly into `dest`.  Called from
        _emit_phi_stores_for_successor; bypasses the phi-sink skip guard in
        _emit_instr.

        Covers ConstInstr, GlobalInstr, and BuiltinInstr — the only types
        admitted by _build_phi_sink.
        """
        if isinstance(instr, MenaiCFGConstInstr):
            ctx.emit_constant(instr.value, dest=dest)

        elif isinstance(instr, MenaiCFGGlobalInstr):
            name_idx = ctx.add_name(instr.name)
            ctx.emit(Opcode.LOAD_NAME, name_idx, dest=dest)

        elif isinstance(instr, MenaiCFGBuiltinInstr):
            opcode, _ = BUILTIN_OPCODE_MAP.get(instr.op, (None, None))
            if opcode is None:
                raise ValueError(f"_emit_sink_instr: unknown builtin {instr.op!r}")
            args = instr.args
            def slot(i: int) -> int:
                return ctx.slot_of(args[i])
            if instr.op in TERNARY_OPS:
                ctx.emit(opcode, slot(0), slot(1), dest=dest, src2=slot(2))
            elif instr.op in BINARY_OPS:
                ctx.emit(opcode, slot(0), slot(1), dest=dest)
            else:
                ctx.emit(opcode, slot(0), dest=dest)

        else:
            raise TypeError(
                f"_emit_sink_instr: unhandled instruction {type(instr).__name__}"
            )

    def _emit_patch(self, patch: MenaiCFGPatchClosureInstr, ctx: _EmitContext) -> None:
        """
        Emit a PATCH_CLOSURE instruction for a letrec fixup.

        PATCH_CLOSURE src0=closure_reg, src1=value_reg, src2=capture_idx
        All three operands are register indices — no stack involvement.
        """
        closure_slot = ctx.slot_of(patch.closure)
        value_slot = ctx.ensure_slot(patch.value)
        ctx.emit(Opcode.PATCH_CLOSURE, closure_slot, value_slot, src2=patch.capture_index)

    def _emit_terminator(
        self,
        block: MenaiCFGBlock,
        term: MenaiCFGTerminator,
        ctx: _EmitContext,
        forward_jumps: List[Tuple[int, MenaiCFGBlock, str]],
        next_block: Optional[MenaiCFGBlock],
    ) -> None:
        if isinstance(term, MenaiCFGReturnTerm):
            src0 = ctx.ensure_slot(term.value)
            ctx.emit(Opcode.RETURN, src0)
            return

        if isinstance(term, MenaiCFGJumpTerm):
            self._emit_phi_stores_for_successor(block, term.target, ctx)
            # Suppress the JUMP if the target is the immediately following block.
            if next_block is None or next_block.id != term.target.id:
                jump_idx = ctx.emit(Opcode.JUMP, 0)
                forward_jumps.append((jump_idx, term.target, 'src0'))

            return

        if isinstance(term, MenaiCFGBranchTerm):
            cond_slot = ctx.ensure_slot(term.cond)
            next_id = next_block.id if next_block is not None else -1
            if next_id == term.false_block.id:
                # False block is the fall-through: emit JUMP_IF_TRUE <true> only.
                # Phi stores happen in then/else blocks via their own JumpTerm.
                true_jump_idx = ctx.emit(Opcode.JUMP_IF_TRUE, cond_slot, 0)
                forward_jumps.append((true_jump_idx, term.true_block, 'src1'))

            elif next_id == term.true_block.id:
                # True block is the fall-through: emit JUMP_IF_FALSE <false> only.
                false_jump_idx = ctx.emit(Opcode.JUMP_IF_FALSE, cond_slot, 0)
                forward_jumps.append((false_jump_idx, term.false_block, 'src1'))

            else:
                # Neither successor is adjacent: emit JUMP_IF_FALSE <false> + JUMP <true>.
                false_jump_idx = ctx.emit(Opcode.JUMP_IF_FALSE, cond_slot, 0)
                true_jump_idx = ctx.emit(Opcode.JUMP, 0)
                forward_jumps.append((true_jump_idx, term.true_block, 'src0'))
                forward_jumps.append((false_jump_idx, term.false_block, 'src1'))

            return

        if isinstance(term, MenaiCFGTailCallTerm):
            for arg in term.args:
                ctx.load_value(arg)

            ctx.load_value(term.func)
            ctx.emit(Opcode.TAIL_CALL, len(term.args))
            return

        if isinstance(term, MenaiCFGTailApplyTerm):
            ctx.load_value(term.func)
            ctx.load_value(term.arg_list)
            ctx.emit(Opcode.TAIL_APPLY)
            return

        if isinstance(term, MenaiCFGSelfLoopTerm):
            # Push new arg values onto the stack in order, then JUMP 0.
            # ENTER at instruction 0 will pop them into param slots.
            for arg in term.args:
                ctx.load_value(arg)

            ctx.emit(Opcode.JUMP, 0)
            return

        if isinstance(term, MenaiCFGRaiseTerm):
            const_idx = ctx.add_constant(term.message)
            ctx.emit(Opcode.RAISE_ERROR, const_idx)
            return

        raise TypeError(f"MenaiVMCodeGen: unhandled terminator {type(term).__name__}")

    def _emit_builtin(self, instr: MenaiCFGBuiltinInstr, ctx: _EmitContext) -> None:
        """
        Emit a builtin operation.

        All optional arguments are synthesised by the desugarer, so every
        builtin arrives here with exactly the arity declared in
        BUILTIN_OPCODE_MAP.  The three generic branches below assert that
        invariant and dispatch to the correct opcode.
        """
        op = instr.op
        args = instr.args
        result = instr.result

        opcode, _ = BUILTIN_OPCODE_MAP.get(op, (None, None))
        if opcode is None:
            raise ValueError(f"MenaiVMCodeGen: unknown builtin op {op!r}")

        def slot(i: int) -> int:
            return ctx.ensure_slot(args[i])

        dest = ctx.alloc_slot(result)

        if op in TERNARY_OPS:
            assert len(args) == 3, f"_emit_builtin: {op!r} expects 3 args, got {len(args)}"
            ctx.emit(opcode, slot(0), slot(1), dest=dest, src2=slot(2))

        elif op in BINARY_OPS:
            assert len(args) == 2, f"_emit_builtin: {op!r} expects 2 args, got {len(args)}"
            ctx.emit(opcode, slot(0), slot(1), dest=dest)

        elif op in UNARY_OPS:
            assert len(args) == 1, f"_emit_builtin: {op!r} expects 1 arg, got {len(args)}"
            ctx.emit(opcode, slot(0), dest=dest)

        else:
            raise ValueError(f"MenaiVMCodeGen: unhandled register-based builtin op {op!r}")

    def _emit_make_closure(
        self, instr: MenaiCFGMakeClosureInstr, ctx: _EmitContext
    ) -> None:
        """
        Emit the code to construct a closure or a plain function constant.

        Recursively generates the child CodeObject for instr.function, then
        emits:
          - MAKE_CLOSURE code_idx 0 followed by PATCH_CLOSURE for each outer
            capture, when needs_patching is True.  All free-var slots are
            pre-allocated as None; sibling captures are filled later by the
            letrec phase-3 patch_instrs, and outer captures are filled here
            immediately after MAKE_CLOSURE.  This mirrors the old codegen's
            letrec two-phase approach and ensures outer captures land at the
            correct slot indices (after the sibling slots), not at slot 0.
          - MAKE_CLOSURE with all eagerly-loaded captures packed from slot 0,
            when needs_patching is False but there are captures.
          - LOAD_CONST with a pre-built MenaiFunction, only when there are
            no captures AND needs_patching is False.
        """
        child_func = instr.function
        child_code = self._generate_lambda_code_object(child_func)
        code_idx = ctx.add_code_object(child_code)

        capture_count = len(instr.captures)
        total_free_vars = len(child_func.free_vars)

        if instr.needs_patching or capture_count > 0:
            closure_slot = ctx.alloc_slot(instr.result)
            ctx.emit(Opcode.MAKE_CLOSURE, code_idx, 0, dest=closure_slot)
            outer_start = total_free_vars - capture_count
            for i, cap in enumerate(instr.captures):
                value_slot = ctx.ensure_slot(cap)
                ctx.emit(Opcode.PATCH_CLOSURE, closure_slot, value_slot, src2=outer_start + i)

            return

        # No captures — pre-build a MenaiFunction and store as a constant.
        func_val = MenaiFunction(
            parameters=tuple(child_code.param_names),
            name=child_code.name,
            bytecode=child_code,
            is_variadic=child_code.is_variadic,
        )
        key = ('function', id(child_code))
        if key not in ctx.constant_map:
            ctx.constant_map[key] = len(ctx.constants)
            ctx.constants.append(func_val)
        slot = ctx.alloc_slot(instr.result)
        ctx.emit(Opcode.LOAD_CONST, ctx.constant_map[key], dest=slot)
        return

    def _generate_lambda_code_object(self, func: MenaiCFGFunction) -> CodeObject:
        """Recursively generate a CodeObject for a nested lambda MenaiCFGFunction."""
        child_ctx = _EmitContext(current_lambda_name=func.binding_name)

        param_count = len(func.params)
        free_var_count = len(func.free_vars)

        # Emit ENTER to pop parameters into slots 0..P-1.
        if param_count > 0:
            child_ctx.emit(Opcode.ENTER, param_count)

        # Reserve slots for params and free vars so alloc_slot starts above them.
        child_ctx.next_slot = param_count + free_var_count

        self._emit_function_body(func, child_ctx)

        # Build the human-readable name.
        if func.binding_name:
            lambda_name = func.binding_name
        else:
            lambda_name = f"<lambda-{self._lambda_counter}>"
            self._lambda_counter += 1

        param_word = "param" if param_count == 1 else "params"
        lambda_name = f"{lambda_name}({param_count} {param_word})"

        return CodeObject(
            instructions=child_ctx.instructions,
            constants=child_ctx.constants,
            names=child_ctx.names,
            code_objects=child_ctx.code_objects,
            free_vars=func.free_vars,
            param_names=func.params,
            param_count=param_count,
            local_count=child_ctx.next_slot,
            is_variadic=func.is_variadic,
            name=lambda_name,
            source_line=func.source_line,
            source_file=func.source_file,
        )


def _result_of(instr: MenaiCFGInstr) -> 'MenaiCFGValue | None':
    """Return the SSA result value of `instr`, or None if it produces no result."""
    if isinstance(instr, (
        MenaiCFGConstInstr,
        MenaiCFGGlobalInstr,
        MenaiCFGParamInstr,
        MenaiCFGFreeVarInstr,
        MenaiCFGBuiltinInstr,
        MenaiCFGCallInstr,
        MenaiCFGApplyInstr,
        MenaiCFGMakeClosureInstr,
        MenaiCFGPhiInstr,
        MenaiCFGTraceInstr,
    )):
        return instr.result
    # MenaiCFGPatchClosureInstr has no result.
    return None


def _build_phi_sink(func: MenaiCFGFunction) -> Dict[int, MenaiCFGInstr]:
    """
    Identify values that are used only as a single phi incoming and can
    therefore be emitted directly into the phi slot, eliminating the
    intermediate slot and the MOVE.

    Returns a map from value id to the defining instruction.  The bytecode
    builder uses this map to:
      - skip the defining instruction during normal block emission, and
      - re-emit the defining instruction into the phi slot when the predecessor
        processes its phi store.

    A value qualifies as a phi-sink when:
      1. Its defining instruction produces a result (has a `result` field).
      2. It appears exactly once across all uses in the function — as an
         incoming value in exactly one phi node.
      3. Its defining instruction is in the predecessor block that contributes
         the phi incoming (guaranteeing all operands are live at emit time).
      4. It is not a ParamInstr, FreeVarInstr, PatchClosureInstr, or
         TraceInstr — these have special slot-assignment or emit logic that
         is incompatible with deferred emission.
    """
    from menai.menai_cfg_collapse_phi_chains import _value_ids_in_instr, _value_ids_in_term

    # Collect all candidate instruction results, keyed by value id, with the
    # block they are defined in.
    candidate_defs: Dict[int, tuple] = {}  # value_id -> (instr, block)
    for block in func.blocks:
        for instr in block.instrs:
            result = _result_of(instr)
            if result is not None and not isinstance(
                instr, (
                    MenaiCFGParamInstr, MenaiCFGFreeVarInstr, MenaiCFGTraceInstr,
                    MenaiCFGCallInstr, MenaiCFGApplyInstr,
                    MenaiCFGMakeClosureInstr, MenaiCFGPhiInstr,
                )
            ):
                candidate_defs[result.id] = (instr, block)

    if not candidate_defs:
        return {}

    # Count every use of each candidate value across the whole function,
    # distinguishing phi-incoming uses from all other uses.
    use_counts: Dict[int, int] = {vid: 0 for vid in candidate_defs}
    phi_use_counts: Dict[int, int] = {vid: 0 for vid in candidate_defs}
    # Also record which predecessor block each phi incoming comes from.
    phi_pred_block: Dict[int, MenaiCFGBlock] = {}

    for block in func.blocks:
        for instr in block.instrs:
            if isinstance(instr, MenaiCFGPhiInstr):
                for val, _ in instr.incoming:
                    if val.id in use_counts:
                        use_counts[val.id] += 1
                        phi_use_counts[val.id] += 1
            else:
                for vid in _value_ids_in_instr(instr):
                    if vid in use_counts:
                        use_counts[vid] += 1
        for patch in block.patch_instrs:
            for vid in (patch.closure.id, patch.value.id):
                if vid in use_counts:
                    use_counts[vid] += 1
        if block.terminator is not None:
            for vid in _value_ids_in_term(block.terminator):
                if vid in use_counts:
                    use_counts[vid] += 1

    # Build the predecessor-block map from phi incomings.
    for block in func.blocks:
        for instr in block.instrs:
            if isinstance(instr, MenaiCFGPhiInstr):
                for val, pred in instr.incoming:
                    if val.id in candidate_defs:
                        phi_pred_block[val.id] = pred

    # A value is a phi-sink if it has exactly one use (the phi incoming) and
    # its defining instruction is in the predecessor block for that incoming.
    result_map: Dict[int, MenaiCFGInstr] = {}
    for vid, (instr, def_block) in candidate_defs.items():
        if use_counts.get(vid, 0) == 1 and phi_use_counts.get(vid, 0) == 1:
            pred = phi_pred_block.get(vid)
            if pred is not None and pred.id == def_block.id:
                result_map[vid] = instr
    return result_map
