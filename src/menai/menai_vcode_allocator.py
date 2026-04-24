"""
Slot allocator for the Menai VM backend.

Takes a MenaiVCodeFunction with virtual registers (MenaiVCodeReg) and
produces a SlotMap — a mapping from register id to slot index — that the
bytecode emitter uses to assign concrete local variable slots.

Algorithm
---------
Params are pre-assigned to slots 0..P-1 and free vars to slots P..P+F-1 and
those slots are never reused — they remain live for the entire function body.
A single-pass linear scan is used for all other registers:

1. Scan the flat instruction list to find the last-use index for every
   register.

2. Walk the instruction list forward.  At each definition point assign the
   lowest slot not currently occupied by a live register.  At each use
   point, if this is the register's last use, free its slot so it can be
   reused by a subsequent definition.

Because VCode is phi-free and already linearised in RPO order, this simple
linear scan produces correct results without full dataflow liveness analysis.
Registers defined before a forward jump are still live at the jump target
if they are used there, and the last-use scan captures this correctly by
finding the actual last use index in the flat list regardless of labels.

MenaiVCodeMove instructions where src and dst are assigned the same slot
are no-ops and will be eliminated by the peephole pass.

Call argument registers whose last use is the call itself, and whose
definition has no intervening call barrier, are reassigned directly to the
outgoing zone (local_count + arg_index) in Phase 3.

Self-loop argument registers whose last use is the self-loop move itself,
and whose definition has no intervening call barrier, are reassigned directly
to the target param slot (arg_index) in Phase 3b, eliminating the MOVE.

Param/free-var register ids
----------------------------
The CFG builder assigns SSA value ids to params and free vars first, in
order: params get ids 0..P-1, free vars get ids P..P+F-1.  The VCode
builder preserves these ids directly (MenaiCFGValue.id → MenaiVCodeReg.id).
The allocator relies on this invariant to pre-assign fixed slots to those
register ids without needing an explicit mapping.
"""

from dataclasses import dataclass
from typing import Dict, List, Set, Tuple

from menai.menai_vcode import (
    MenaiVCodeApply,
    MenaiVCodeBuiltin,
    MenaiVCodeCall,
    MenaiVCodeFunction,
    MenaiVCodeInstr,
    MenaiVCodeJump,
    MenaiVCodeJumpIfFalse,
    MenaiVCodeJumpIfTrue,
    MenaiVCodeLabel,
    MenaiVCodeLoadConst,
    MenaiVCodeLoadName,
    MenaiVCodeMakeClosure,
    MenaiVCodeMove,
    MenaiVCodePatchClosure,
    MenaiVCodeMakeStruct,
    MenaiVCodeMakeList,
    MenaiVCodeMakeSet,
    MenaiVCodeMakeDict,
    MenaiVCodeReg,
    MenaiVCodeReturn,
    MenaiVCodeTailApply,
    MenaiVCodeTailCall,
    MenaiVCodeTrace,
)


@dataclass
class SlotMap:
    """Result of slot allocation for one MenaiVCodeFunction."""
    slots: Dict[int, int]   # register id → slot index
    slot_count: int         # total slots needed (= local_count in CodeObject)
    local_count: int = 0    # scratch + fixed slots before the outgoing zone
                            # (= slot_count when there are no outgoing slots)

    def slot_of(self, reg: MenaiVCodeReg) -> int:
        """Return the slot assigned to reg.  Asserts it exists."""
        assert reg.id in self.slots, (
            f"SlotMap: register {reg} has no assigned slot"
        )
        return self.slots[reg.id]


def allocate_slots(func: MenaiVCodeFunction) -> SlotMap:
    """
    Assign a slot index to every virtual register in func.

    Args:
        func: A MenaiVCodeFunction with virtual registers.

    Returns:
        A SlotMap mapping every register id to a slot index.
    """
    param_count = len(func.params)
    free_var_count = len(func.free_vars)
    fixed_count = param_count + free_var_count

    slots: Dict[int, int] = {}
    next_new_slot = 0

    # Pre-compute sets needed for Phase 3 safety checks.
    # closure_reg_ids: registers written by MAKE_CLOSURE — PATCH_CLOSURE
    # requires its closure operand within local_count.
    closure_reg_ids: Set[int] = {
        instr.dst.id for instr in func.instrs
        if isinstance(instr, MenaiVCodeMakeClosure)
    }

    # Phase 1: scan the flat instruction list to find the last-use index for
    # every register.
    last_use: Dict[int, int] = {}
    def_index: Dict[int, int] = {}

    for idx, instr in enumerate(func.instrs):
        defs, uses = _defs_uses(instr)
        for reg_id in defs:
            def_index[reg_id] = idx

        for reg_id in uses:
            last_use[reg_id] = idx

    # Pre-assign fixed slots for params (0..P-1) and free vars (P..P+F-1).
    # These register ids are guaranteed by the CFG builder's assignment order.
    fixed_reg_ids: List[int] = list(range(fixed_count))
    for reg_id in fixed_reg_ids:
        slots[reg_id] = reg_id

    # Phase 2: linear scan allocation for all other registers.  Fixed slots
    # (params and free vars) are permanently live and never released for reuse.
    live: Set[int] = set(fixed_reg_ids)

    def _free_slot() -> int:
        """Return the lowest slot index not currently occupied by a live register."""
        nonlocal next_new_slot
        occupied = {slots[rid] for rid in live if rid in slots}
        slot = 0
        while slot in occupied:
            slot += 1

        if slot >= next_new_slot:
            next_new_slot = slot + 1

        return slot

    def _kill_if_dead(reg_id: int, current_idx: int) -> None:
        """Remove reg_id from the live set if its last use is at or before current_idx."""
        if reg_id >= fixed_count and last_use.get(reg_id, -1) <= current_idx:
            live.discard(reg_id)

    for idx, instr in enumerate(func.instrs):
        if isinstance(instr, MenaiVCodeLabel):
            continue

        defs, uses = _defs_uses(instr)

        # MenaiVCodeMakeClosure: allocate the result first, then kill dead
        # inputs.  The bytecode emitter reads captures after writing the
        # closure slot (via PATCH_CLOSURE), so the closure slot must not
        # overlap with any capture register.
        if isinstance(instr, MenaiVCodeMakeClosure):
            dst_id = instr.dst.id
            if dst_id not in slots:
                slots[dst_id] = _free_slot()

            live.add(dst_id)
            for reg_id in uses:
                _kill_if_dead(reg_id, idx)

            _kill_if_dead(dst_id, idx)
            continue

        # MenaiVCodeTrace: result is an alias for the value input — assign
        # them the same slot so the MOVE emitted by the bytecode builder is
        # always a no-op and eliminated by the peephole pass.
        if isinstance(instr, MenaiVCodeTrace):
            for reg_id in [r.id for r in instr.messages]:
                _kill_if_dead(reg_id, idx)

            val_id = instr.value.id
            dst_id = instr.dst.id
            if val_id in slots:
                slots[dst_id] = slots[val_id]

            else:
                slots[dst_id] = _free_slot()
                slots[val_id] = slots[dst_id]

            live.add(dst_id)
            _kill_if_dead(dst_id, idx)
            continue

        # All other instructions: kill dead inputs first so the result can
        # reuse their slots, then allocate the result.
        for reg_id in uses:
            _kill_if_dead(reg_id, idx)

        for reg_id in defs:
            if reg_id not in slots:
                slots[reg_id] = _free_slot()

            live.add(reg_id)

        for reg_id in defs:
            _kill_if_dead(reg_id, idx)

    # Ensure slot_count covers all assigned slots.
    slot_count = next_new_slot
    for s in slots.values():
        if s >= slot_count:
            slot_count = s + 1

    local_count = slot_count

    # Phase 3: back-propagate outgoing slot assignments.  For each call/tail-call
    # argument that meets all safety conditions, reassign its scratch slot to
    # local_count + arg_index so the bytecode emitter needs no MOVE instruction.
    #
    # Safety conditions:
    #   1. Not a fixed register (param or free var) — those have fixed slots.
    #   2. Not a closure register — PATCH_CLOSURE requires its operands within
    #      local_count.
    #   3. The call is the last use of the register — the outgoing zone is
    #      clobbered when the call returns, so no later read is safe.
    #   4. No call or apply between the register's definition and this call —
    #      a prior call, apply, or struct construction would have already written
    #      local_count + arg_index.
    #      For call/apply result registers the defining call itself is not a
    #      barrier — the scan starts strictly after the definition index.
    max_outgoing_index = -1
    for call_idx, instr in enumerate(func.instrs):
        if not isinstance(instr, (MenaiVCodeCall, MenaiVCodeTailCall)):
            continue

        for arg_index, arg in enumerate(instr.args):
            reg_id = arg.id

            if reg_id < fixed_count:
                continue

            if reg_id in closure_reg_ids:
                continue

            if last_use.get(reg_id, -1) != call_idx:
                continue

            # Condition 4: scan for a call/apply barrier between def and use.
            reg_def = def_index.get(reg_id, 0)
            barrier = False
            for scan_idx in range(reg_def + 1, call_idx):
                scan_instr = func.instrs[scan_idx]
                if isinstance(scan_instr, (MenaiVCodeCall, MenaiVCodeApply,
                                           MenaiVCodeTailCall, MenaiVCodeTailApply,
                                           MenaiVCodeMakeStruct, MenaiVCodeMakeList,
                                           MenaiVCodeMakeSet, MenaiVCodeMakeDict)):
                    barrier = True
                    break

            if barrier:
                continue

            slots[reg_id] = local_count + arg_index
            max_outgoing_index = max(max_outgoing_index, arg_index)

    if max_outgoing_index >= 0:
        slot_count = local_count + max_outgoing_index + 1

    # Phase 3b: back-propagate param slot assignments for self-loop moves.
    # A self-loop is emitted as a group of MenaiVCodeMove instructions
    # (one per param) immediately followed by JUMP __entry__.  For each move
    # whose source register meets the same safety conditions as Phase 3, we
    # reassign the source register's slot directly to the target param slot
    # so the definition writes straight into the param slot and the MOVE
    # becomes a same-slot no-op, eliminated by the peephole pass.
    #
    # Safety conditions (parallel to Phase 3):
    #   1. Not a fixed register (param or free var).
    #   2. Not a closure register.
    #   3. The self-loop move is the last use of the register.
    #   4. No call or apply between the register's definition and this move.
    #   5. No instruction between the definition and this move reads from param_slot.
    for jump_idx, instr in enumerate(func.instrs):
        if not isinstance(instr, MenaiVCodeJump) or instr.label != "__entry__":
            continue

        # Collect the contiguous MOVE group immediately before this JUMP.
        move_start = jump_idx - 1
        while move_start >= 0 and isinstance(func.instrs[move_start], MenaiVCodeMove):
            move_start -= 1
        move_start += 1

        for move_idx in range(move_start, jump_idx):
            move = func.instrs[move_idx]
            assert isinstance(move, MenaiVCodeMove)
            reg_id = move.src.id
            param_slot = slots[move.dst.id]

            if reg_id < fixed_count:
                continue

            if reg_id in closure_reg_ids:
                continue

            if last_use.get(reg_id, -1) != move_idx:
                continue

            reg_def = def_index.get(reg_id, 0)
            barrier = False
            for scan_idx in range(reg_def + 1, move_idx):
                scan_instr = func.instrs[scan_idx]
                if isinstance(scan_instr, (MenaiVCodeCall, MenaiVCodeApply,
                                           MenaiVCodeTailCall, MenaiVCodeTailApply,
                                           MenaiVCodeMakeStruct, MenaiVCodeMakeList,
                                           MenaiVCodeMakeSet, MenaiVCodeMakeDict)):
                    barrier = True
                    break

                _, scan_uses = _defs_uses(scan_instr)
                if any(slots.get(u, -1) == param_slot for u in scan_uses):
                    barrier = True
                    break

            if barrier:
                continue

            slots[reg_id] = param_slot

    return SlotMap(slots=slots, slot_count=slot_count, local_count=local_count)


def _defs_uses(instr: MenaiVCodeInstr) -> Tuple[List[int], List[int]]:
    """
    Return (defs, uses) — lists of register ids defined and used by instr.

    Labels define and use nothing.
    Instructions with a dst define that register.
    Jump instructions use their condition register (if any).
    """
    if isinstance(instr, MenaiVCodeLabel):
        return [], []

    if isinstance(instr, MenaiVCodeMove):
        return [instr.dst.id], [instr.src.id]

    if isinstance(instr, MenaiVCodeLoadConst):
        return [instr.dst.id], []

    if isinstance(instr, MenaiVCodeLoadName):
        return [instr.dst.id], []

    if isinstance(instr, MenaiVCodeBuiltin):
        return [instr.dst.id], [r.id for r in instr.args]

    if isinstance(instr, MenaiVCodeCall):
        return [instr.dst.id], [instr.func.id] + [r.id for r in instr.args]

    if isinstance(instr, MenaiVCodeTailCall):
        return [], [instr.func.id] + [r.id for r in instr.args]

    if isinstance(instr, MenaiVCodeApply):
        return [instr.dst.id], [instr.func.id, instr.arg_list.id]

    if isinstance(instr, MenaiVCodeTailApply):
        return [], [instr.func.id, instr.arg_list.id]

    if isinstance(instr, MenaiVCodeMakeClosure):
        return [instr.dst.id], [r.id for r in instr.captures]

    if isinstance(instr, MenaiVCodePatchClosure):
        return [], [instr.closure.id, instr.value.id]

    if isinstance(instr, MenaiVCodeMakeStruct):
        return [instr.dst.id], [r.id for r in instr.args]

    if isinstance(instr, MenaiVCodeMakeList):
        return [instr.dst.id], [r.id for r in instr.args]

    if isinstance(instr, MenaiVCodeMakeSet):
        return [instr.dst.id], [r.id for r in instr.args]

    if isinstance(instr, MenaiVCodeMakeDict):
        return [instr.dst.id], [r.id for k, v in instr.pairs for r in (k, v)]

    if isinstance(instr, MenaiVCodeTrace):
        return [instr.dst.id], [r.id for r in instr.messages] + [instr.value.id]

    if isinstance(instr, MenaiVCodeJumpIfTrue):
        return [], [instr.cond.id]

    if isinstance(instr, MenaiVCodeJumpIfFalse):
        return [], [instr.cond.id]

    if isinstance(instr, MenaiVCodeReturn):
        return [], [instr.value.id]

    # MenaiVCodeJump, MenaiVCodeRaise: no register references.
    return [], []
