"""
VM code generator for the Menai compiler.

Translates a MenaiCFGFunction (SSA CFG) into a CodeObject ready for
execution by the Menai VM.

Position in the pipeline
------------------------
    MenaiCFGFunction  →  MenaiVMCodeGen  →  CodeObject

SSA-to-register mapping
-----------------------
The VM is a stack machine transitioning to register-based ops.  We bridge
the CFG's explicit SSA values by assigning each MenaiCFGValue a local slot
(register), then emitting POP after each instruction that produces a value
and PUSH wherever that value must be placed on the call stack for a CALL.

This is deliberately straightforward — it produces correct code and gives the
existing test suite something to run against.  Redundant PUSH/POP pairs will
be eliminated as ops are converted to read/write registers directly.

Slot layout
-----------
Within each lambda frame:
    0 .. P-1          parameters          (MenaiCFGParamInstr)
    P .. P+F-1        captured free vars  (MenaiCFGFreeVarInstr)
    P+F ..            all other SSA values, allocated in definition order

Phi nodes
---------
A MenaiCFGPhiInstr emits no instructions.  Both predecessor blocks store
their result into the phi's slot (via POP) before jumping to the join block.
The join block then simply pushes from that slot (via PUSH) when the phi
value is used downstream.

The mechanism: when we assign a slot to a phi value, we also record that
each predecessor block's "outgoing value" (the value it contributes to the
phi) must be stored into that same slot before its terminator jump.  We
handle this via a _phi_stores dict: block_id → list of (value, slot) pairs
to emit as POP before the block's terminator.

Block ordering
--------------
We emit blocks in the order they appear in MenaiCFGFunction.blocks (which
is construction order from the builder).  The builder always appends:
  entry, then_N, else_N, join_N for each if-expression, in nesting order.
This means the fall-through from a branch's true-block to its join is never
adjacent in the linear order (we always emit an explicit JUMP), which is
fine — the VM handles it correctly.

letrec patching
---------------
MenaiCFGPatchClosureInstr in block.patch_instrs is emitted after the block's
regular instructions and before its terminator.  The VM PATCH_CLOSURE opcode
takes (closure_slot, capture_index) and pops the value from the stack, so
we load the value, then emit PATCH_CLOSURE with the closure's slot and the
capture index.

self-loop (direct tail recursion)
----------------------------------
MenaiCFGSelfLoopTerm stores the new argument values into parameter slots
0..N-1 then emits JUMP 0 (back to the entry block).  The entry block's
ENTER instruction re-reads the slots, so we must NOT re-emit ENTER on
the loop-back path — JUMP 0 lands at instruction index 0 which is ENTER,
and ENTER pops from the *locals* array, not the stack.  Wait — actually the
VM ENTER opcode pops N values off the stack into locals.  On the loop-back
path (JUMP 0) the stack is empty, so we must push new arg values onto the
call stack (PUSH 0..N-1) and then JUMP 0 so ENTER re-pops them, or
store into the slots and jump to instruction 1.

Re-reading the VM design: JUMP 0 is the canonical self-tail-call target and
the VM's ENTER opcode is only executed on function entry (it pops from the
call stack frame set up by CALL/TAIL_CALL).  The correct pattern is:
  - Evaluate new args onto the stack
  - JUMP 0  (which re-executes ENTER, which pops the args back into slots)
But that only works if the args are on the stack in the right order when
ENTER fires.  Looking at the existing codegen: it pushes new arg values
onto the stack and then emits JUMP 0, so ENTER re-runs and re-pops them.
We follow the same convention.
"""

from __future__ import annotations

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
from menai.menai_cfg_stack_scheduler import MenaiCFGStackScheduler, StackSchedule
from menai.menai_value import (
    MenaiBoolean,
    MenaiComplex,
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

BUILD_OPS = {
    'list': Opcode.LIST,
    'dict': Opcode.DICT,
}


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

    # Stack schedule for this function — set before emission begins.
    schedule: StackSchedule = field(default_factory=StackSchedule)

    # Phi result id → slot, populated lazily on first store by a predecessor.
    # Kept separate so we can assert that a phi slot is allocated before the
    # join block tries to read it (via slot_of on the phi result).
    phi_slot_map: Dict[int, int] = field(default_factory=dict)

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
        """Return a register slot for `value`, materialising it if necessary.

        For slotted values: returns the existing slot.
        For rematerialisable constants: allocates a fresh slot, emits the
        constant load into it, and returns that slot.
        Transient values should not reach here — they have no slot by design.
        """
        if self.schedule.is_remat(value):
            slot = self.next_slot
            self.next_slot += 1
            self.emit_constant(self.schedule.remat_value_of(value), slot)
            return slot

        return self.slot_of(value)

    def emit(self, opcode: Opcode, src0: int = 0, src1: int = 0, dest: int = 0, src2: int = 0) -> int:
        """Emit an instruction, returning its index."""
        idx = len(self.instructions)
        self.instructions.append(Instruction(opcode, dest=dest, src0=src0, src1=src1, src2=src2))
        return idx

    def load_value(self, value: MenaiCFGValue) -> None:
        """
        Emit the instruction(s) needed to make `value` available on the stack.

        For stack-transient values that have a slot (produced by a register-based
        op such as LOAD_CONST): emit PUSH <slot>.
        For stack-transient values without a slot: emit nothing — the value is
        already on top of the stack from the immediately preceding unconverted op.
        For rematerialisable constants: allocate a temp slot, emit the register
        load into it, then PUSH it onto the call stack.
        For slotted values: emit PUSH <slot>.
        """
        if self.schedule.is_transient(value):
            if value.id in self.slot_map:
                self.emit(Opcode.PUSH, self.slot_of(value))
            return
        if self.schedule.is_remat(value):
            self.emit_const_push(self.schedule.remat_value_of(value))
            return
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
        const_idx = self.add_constant(value)
        self.emit(Opcode.LOAD_CONST, const_idx, dest=dest)

    def emit_const_push(self, value: MenaiValue) -> None:
        """Load a constant into a fresh temp register then PUSH it onto the call stack.

        Used for synthesised default arguments feeding unconverted stack-based ops,
        and for rematerialisable constants consumed by those same ops.
        """
        slot = self.next_slot
        self.next_slot += 1
        self.emit_constant(value, dest=slot)
        self.emit(Opcode.PUSH, slot)

    def store_result(self, value: MenaiCFGValue) -> int:
        """
        Emit the instruction(s) needed to store a freshly computed result.

        For stack-transient values: emit nothing — the value stays on the
        stack for its single consumer.  Returns -1 (no slot allocated).
        For rematerialisable constants: emit nothing — no slot needed, the
        load will be re-emitted at each use site.  Returns -1.
        For slotted values: allocate a slot, emit POP into <slot>, return slot.
        """
        if self.schedule.is_transient(value):
            return -1
        if self.schedule.is_remat(value):
            return -1
        slot = self.alloc_slot(value)
        self.emit(Opcode.POP, dest=slot)
        return slot

    def patch_jump(self, instr_index: int, target: int, src0: str = 'src0') -> None:
        """Back-patch a jump target into the given field of an already-emitted instruction.

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


class MenaiVMCodeGen:
    """
    Generates a CodeObject from a MenaiCFGFunction.

    Usage::

        code_obj = MenaiVMCodeGen().generate(cfg_function, name="<module>")
    """

    def __init__(self) -> None:
        self._lambda_counter = 0

    def generate(self, func: MenaiCFGFunction, name: str = "<module>") -> CodeObject:
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

        # Phase 0: classify SSA values as stack-transient or slotted.
        ctx.schedule = MenaiCFGStackScheduler().schedule(func)

        for block in func.blocks:
            for instr in block.instrs:
                if isinstance(instr, MenaiCFGParamInstr):
                    ctx.assign_slot(instr.result, instr.index)
                elif isinstance(instr, MenaiCFGFreeVarInstr):
                    ctx.assign_slot(instr.result, param_count + instr.index)

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
            # Rematerialisable constants: skip entirely at the definition site.
            # The load will be re-emitted (stack-push, dest=0) at each use site.
            if not ctx.schedule.is_remat(instr.result):
                # Always allocate a slot: LOAD_* is now register-based so it must
                # write to a register even if the stack scheduler marked it transient.
                slot = ctx.alloc_slot(instr.result)
                ctx.emit_constant(instr.value, dest=slot)
            return

        if isinstance(instr, MenaiCFGGlobalInstr):
            name_idx = ctx.add_name(instr.name)
            # LOAD_NAME is register-based: always allocate a slot regardless of
            # the stack scheduler's transient/slotted classification.
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
            ctx.emit(Opcode.CALL, len(instr.args))
            ctx.store_result(instr.result)
            return

        if isinstance(instr, MenaiCFGApplyInstr):
            ctx.load_value(instr.func)
            ctx.load_value(instr.arg_list)
            ctx.emit(Opcode.APPLY)
            ctx.store_result(instr.result)
            return

        if isinstance(instr, MenaiCFGMakeClosureInstr):
            self._emit_make_closure(instr, ctx)
            return

        if isinstance(instr, MenaiCFGTraceInstr):
            for msg in instr.messages:
                msg_slot = ctx.ensure_slot(msg)
                ctx.emit(Opcode.EMIT_TRACE, msg_slot)
            # instr.value may be stack-transient with no slot, meaning it is
            # sitting on top of the stack from the preceding stack-based op.
            # We must pop it into a fresh slot before recording the result slot.
            # A transient MenaiCFGConstInstr is given a register slot by the
            # const emitter even though the scheduler marks it transient, so we
            # check slot_map to distinguish "truly on stack" from "in register".
            if ctx.schedule.is_transient(instr.value) and instr.value.id not in ctx.slot_map:
                result_slot = ctx.next_slot
                ctx.next_slot += 1
                ctx.slot_map[instr.value.id] = result_slot
                ctx.emit(Opcode.POP, dest=result_slot)
            else:
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
                    ctx.load_value(incoming_val)
                    ctx.emit(Opcode.POP, dest=phi_slot)

    def _emit_patch(self, patch: MenaiCFGPatchClosureInstr, ctx: _EmitContext) -> None:
        """Emit a PATCH_CLOSURE instruction for a letrec fixup.

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
            ctx.load_value(term.value)
            ctx.emit(Opcode.RETURN)
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
        Emit a builtin operation, including special-case handling for
        optional-argument builtins (range, string-slice, dict-get, etc.).
        """
        op = instr.op
        args = instr.args

        def load_arg(i: int) -> None:
            ctx.load_value(args[i])

        def load_all() -> None:
            for a in args:
                ctx.load_value(a)

        # Special cases with optional / synthesised arguments.
        if op == 'range':
            load_all()
            if len(args) == 2:
                ctx.emit_const_push(MenaiInteger(1))
            ctx.emit(Opcode.RANGE)

        elif op == 'integer->complex':
            load_arg(0)
            if len(args) == 1:
                ctx.emit_const_push(MenaiInteger(0))
            else:
                load_arg(1)
            ctx.emit(Opcode.INTEGER_TO_COMPLEX)

        elif op == 'integer->string':
            load_arg(0)
            if len(args) == 1:
                ctx.emit_const_push(MenaiInteger(10))
            else:
                load_arg(1)
            ctx.emit(Opcode.INTEGER_TO_STRING)

        elif op == 'float->complex':
            load_arg(0)
            if len(args) == 1:
                ctx.emit_const_push(MenaiFloat(0.0))
            else:
                load_arg(1)
            ctx.emit(Opcode.FLOAT_TO_COMPLEX)

        elif op == 'string->integer':
            load_arg(0)
            if len(args) == 1:
                ctx.emit_const_push(MenaiInteger(10))
            else:
                load_arg(1)
            ctx.emit(Opcode.STRING_TO_INTEGER)

        elif op == 'string-slice':
            load_arg(0)
            load_arg(1)
            if len(args) == 2:
                # Default end = string-length(str)
                load_arg(0)
                ctx.emit(Opcode.STRING_LENGTH)
            else:
                load_arg(2)
            ctx.emit(Opcode.STRING_SLICE)

        elif op == 'string->list':
            load_arg(0)
            if len(args) == 1:
                ctx.emit_const_push(MenaiString(""))
            else:
                load_arg(1)
            ctx.emit(Opcode.STRING_TO_LIST)

        elif op == 'list-slice':
            load_arg(0)
            load_arg(1)
            if len(args) == 2:
                # Default end = list-length(lst)
                load_arg(0)
                ctx.emit(Opcode.LIST_LENGTH)
            else:
                load_arg(2)
            ctx.emit(Opcode.LIST_SLICE)

        elif op == 'list->string':
            load_arg(0)
            if len(args) == 1:
                ctx.emit_const_push(MenaiString(""))
            else:
                load_arg(1)
            ctx.emit(Opcode.LIST_TO_STRING)

        elif op == 'dict-get':
            load_all()
            if len(args) == 2:
                ctx.emit_const_push(MenaiNone())
            ctx.emit(Opcode.DICT_GET)

        elif op in BINARY_OPS:
            load_all()
            ctx.emit(BINARY_OPS[op])

        elif op in UNARY_OPS:
            load_all()
            ctx.emit(UNARY_OPS[op])

        elif op in TERNARY_OPS:
            load_all()
            ctx.emit(TERNARY_OPS[op])

        elif op in BUILD_OPS:
            load_all()
            ctx.emit(BUILD_OPS[op], len(args))

        else:
            raise ValueError(f"MenaiVMCodeGen: unknown builtin op {op!r}")

        # Store the result.
        ctx.store_result(instr.result)

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

        if instr.needs_patching:
            # Letrec case: emit MAKE_CLOSURE with 0 captures so the VM
            # pre-allocates None for all free-var slots.  Then immediately
            # PATCH_CLOSURE the outer (non-sibling) captures into their
            # correct slot indices.  Sibling captures are patched later by
            # the block's patch_instrs (phase 3 of letrec).
            closure_slot = ctx.alloc_slot(instr.result)
            ctx.emit(Opcode.MAKE_CLOSURE, code_idx, 0, dest=closure_slot)
            # Outer captures occupy the tail of free_vars: indices
            # [total_free_vars - capture_count .. total_free_vars - 1].
            outer_start = total_free_vars - capture_count
            for i, cap in enumerate(instr.captures):
                value_slot = ctx.ensure_slot(cap)
                ctx.emit(Opcode.PATCH_CLOSURE, closure_slot, value_slot, src2=outer_start + i)
            return
        elif capture_count > 0:
            # Non-letrec closure with captures: still passes captures via the
            # stack (PUSH each, then MAKE_CLOSURE pops them).  Result written
            # to dest register directly — no POP needed.
            for cap in instr.captures:
                ctx.load_value(cap)
            closure_slot = ctx.alloc_slot(instr.result)
            ctx.emit(Opcode.MAKE_CLOSURE, code_idx, capture_count, dest=closure_slot)
            return
        else:
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
        """
        Recursively generate a CodeObject for a nested lambda MenaiCFGFunction.
        """
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
