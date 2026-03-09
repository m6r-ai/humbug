"""
Peephole optimiser for MenaiVCodeFunction.

Runs a set of local pattern-matching optimisations over the flat VCode
instruction list.  Each sub-pass is applied repeatedly until no further
changes occur.

Sub-passes
----------
1. Redundant move elimination
   Removes MenaiVCodeMove instructions where src and dst have been assigned
   the same slot by the allocator.  These are no-ops produced by phi
   elimination when the incoming value and the phi result already share a
   slot.

   Requires a SlotMap — must run after slot allocation.

2. Jump-over-jump elimination
   Replaces the pattern:

       JUMP_IF_TRUE  r, @L1
       JUMP @L2

   with:

       JUMP_IF_FALSE r, @L2

   (and symmetrically for JUMP_IF_FALSE followed by JUMP).

   This eliminates the redundant unconditional jump that arises when the
   CFG has an empty arm block that was not bypassed (e.g. because it had a
   BranchTerm predecessor and a phi-bearing successor).  With the VCode
   pipeline this pattern can still arise when the false block falls through
   but the true block does not, and neither is the immediately next block
   in RPO order.

   Does not require a SlotMap — can run before or after slot allocation,
   but running after ensures the instruction indices used for jump targets
   are already resolved.  In practice this pass operates on label strings,
   not instruction indices, so it is independent of allocation.

The two sub-passes are composed and iterated to a joint fixed point.
"""

from typing import List, Tuple

from menai.menai_vcode import (
    MenaiVCodeFunction,
    MenaiVCodeInstr,
    MenaiVCodeJump,
    MenaiVCodeJumpIfFalse,
    MenaiVCodeJumpIfTrue,
    MenaiVCodeLabel,
    MenaiVCodeMove,
)
from menai.menai_vcode_allocator import SlotMap


def peephole(func: MenaiVCodeFunction, slot_map: SlotMap) -> MenaiVCodeFunction:
    """
    Apply peephole optimisations to func, iterating to a fixed point.

    Args:
        func:     The VCode function to optimise.
        slot_map: The slot allocation for func (needed for move elimination).

    Returns:
        A new MenaiVCodeFunction with redundant instructions removed.
        Returns func unchanged if no optimisations apply.
    """
    instrs = list(func.instrs)
    changed = True
    while changed:
        changed = False
        instrs, c = _eliminate_redundant_moves(instrs, slot_map)
        changed = changed or c
        instrs, c = _eliminate_jump_over_jump(instrs)
        changed = changed or c

    if instrs is func.instrs:
        return func

    return MenaiVCodeFunction(
        instrs=instrs,
        params=func.params,
        free_vars=func.free_vars,
        is_variadic=func.is_variadic,
        binding_name=func.binding_name,
        reg_count=func.reg_count,
        source_line=func.source_line,
        source_file=func.source_file,
    )


def _eliminate_redundant_moves(
    instrs: List[MenaiVCodeInstr],
    slot_map: SlotMap,
) -> Tuple[List[MenaiVCodeInstr], bool]:
    """
    Remove MenaiVCodeMove instructions where src and dst share a slot.

    These are produced by phi elimination when the incoming value and the
    phi result were assigned the same slot by the allocator.
    """
    result: List[MenaiVCodeInstr] = []
    changed = False
    for instr in instrs:
        if (
            isinstance(instr, MenaiVCodeMove)
            and slot_map.slots.get(instr.dst.id) == slot_map.slots.get(instr.src.id)
        ):
            changed = True

        else:
            result.append(instr)

    return result, changed


def _eliminate_jump_over_jump(
    instrs: List[MenaiVCodeInstr],
) -> Tuple[List[MenaiVCodeInstr], bool]:
    """
    Replace JUMP_IF_TRUE/FALSE @L1 immediately followed by JUMP @L2 with
    the inverted conditional JUMP_IF_FALSE/TRUE @L2, removing the JUMP.

    Skips over intervening labels when determining adjacency — a label
    between the conditional and the unconditional jump is just a marker and
    does not affect the pattern.

    After removing the unconditional jump, any labels that pointed to it
    are left in place (they now point to the next real instruction), which
    is correct.
    """
    result: List[MenaiVCodeInstr] = []
    changed = False
    i = 0
    while i < len(instrs):
        instr = instrs[i]
        if isinstance(instr, (MenaiVCodeJumpIfTrue, MenaiVCodeJumpIfFalse)):
            # Look ahead past any labels to find the next non-label instruction.
            j = i + 1
            while j < len(instrs) and isinstance(instrs[j], MenaiVCodeLabel):
                j += 1

            if j < len(instrs) and isinstance(instrs[j], MenaiVCodeJump):
                jump = instrs[j]
                assert isinstance(jump, MenaiVCodeJump)
                if jump.label == "__entry__":
                    result.append(instr)
                    i += 1
                    continue

                # Found the pattern — invert the conditional and drop the JUMP.
                if isinstance(instr, MenaiVCodeJumpIfTrue):
                    result.append(MenaiVCodeJumpIfFalse(
                        cond=instr.cond, label=jump.label
                    ))

                else:
                    result.append(MenaiVCodeJumpIfTrue(
                        cond=instr.cond, label=jump.label
                    ))

                # Emit any intervening labels (they now sit between the new
                # conditional and whatever follows the removed JUMP).
                for k in range(i + 1, j):
                    result.append(instrs[k])

                # Skip past the JUMP.
                i = j + 1
                changed = True
                continue

        result.append(instr)
        i += 1

    return result, changed
