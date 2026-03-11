"""
CFG pass: branch constant propagation.

When a phi node is used only as the condition of a MenaiCFGBranchTerm, and
some of its incoming values are statically-known boolean constants, those
arms do not need to flow through the phi join at all.  An incoming True
value means that predecessor always takes the true branch; an incoming False
value means it always takes the false branch.

For each such constant-valued incoming arm, this pass re-wires the *defining
block* of the constant (the block containing the MenaiCFGConstInstr) to jump
directly to the appropriate branch target, bypassing the phi join block
entirely.  Using the defining block rather than the phi's recorded predecessor
block handles the case where empty intermediate blocks (left behind by
MenaiCFGCollapsePhiChains) sit between the defining block and the join.

After all constant arms have been re-wired:

  - If the phi has no remaining incoming entries it is dead and the join
    block becomes unreachable.
  - If exactly one non-constant arm remains the phi is trivial (single
    predecessor) and the join block becomes an empty indirection that
    MenaiCFGSimplifyBlocks will eliminate on the next iteration.
  - If multiple non-constant arms remain the phi is retained with only
    those entries.

The pass runs to a fixed point within each function; a single round may
expose new candidates (e.g. after a join block is reduced to a single
incoming entry and then itself becomes a candidate).

This pass is designed to run after MenaiCFGCollapsePhiChains (which flattens
nested phi chains, producing the wide flat phis that are most amenable to
this optimisation) and before MenaiCFGSimplifyBlocks (which cleans up the
empty or trivial join blocks this pass leaves behind).

Example — the (or p1 (or p2 (or p3 p4))) pattern:

  Before (after CollapsePhiChains, with empty intermediate blocks still
  present):
    block3 (then for p1): const True  →  jump block9  (empty)
    block5 (then for p2): const True  →  jump block10 (empty)
    block7 (then for p3): const True  →  jump block11
    block8 (else for p3): %r = p4     →  jump block11
    block9:  jump block10  (empty)
    block10: jump block11  (empty)
    block11: %v = phi [True←3, True←5, True←7, %r←8]
             branch %v → loop_body / exit

  After (defining blocks re-wired to loop_body directly):
    block3: jump → loop_body
    block5: jump → loop_body
    block7: jump → loop_body
    block8: jump → block11   (non-constant, untouched)
    block9:  jump block10    (now unreachable — SimplifyBlocks cleans up)
    block10: jump block11    (now unreachable — SimplifyBlocks cleans up)
    block11: %v = phi [%r←8]
             branch %v → loop_body / exit
"""

from typing import Dict, List, Tuple

from menai.menai_cfg import (
    MenaiCFGBlock,
    MenaiCFGBranchTerm,
    MenaiCFGConstInstr,
    MenaiCFGFunction,
    MenaiCFGJumpTerm,
    MenaiCFGPhiInstr,
    MenaiCFGValue,
    relink_predecessors,
)
from menai.menai_cfg_optimization_pass import MenaiCFGOptimizationPass
from menai.menai_value import MenaiBoolean


class MenaiCFGBranchConstProp(MenaiCFGOptimizationPass):
    """
    Re-wire the defining blocks of phi incoming values that are statically-known
    boolean constants, when the phi is used only as the condition of a branch.

    For each phi P whose result is used only as the condition of a
    MenaiCFGBranchTerm in the same block, and for each incoming (val, pred)
    pair where val is a statically-known boolean constant:

      - Find the block that *defines* val (the block containing the
        MenaiCFGConstInstr for val).  This may differ from pred when empty
        intermediate blocks (left by an earlier CollapsePhiChains pass) sit
        between the defining block and the join.
      - Replace the defining block's jump-to-join with a direct jump to the
        branch's true_block (for True) or false_block (for False).
      - Remove the rewired entries from P's incoming list.
      - If P's incoming list becomes empty, remove P and the branch; the
        join block becomes dead (no predecessors will jump to it).
    """

    def _optimize_function(self, func: MenaiCFGFunction) -> tuple[MenaiCFGFunction, bool]:
        changed_overall = False

        while True:
            def_block_map = _build_def_block_map(func)
            round_changed = self._run_one_round(func, def_block_map)
            if not round_changed:
                break
            changed_overall = True

        if changed_overall:
            relink_predecessors(func)

        return func, changed_overall

    def _run_one_round(
        self,
        func: MenaiCFGFunction,
        def_block_map: Dict[int, 'MenaiCFGBlock'],
    ) -> bool:
        """
        Execute one round of branch constant propagation.

        Scans every block for a qualifying phi+branch pair and re-wires all
        constant-valued defining blocks.  Returns True if any change was made.
        """
        const_values: Dict[int, MenaiBoolean] = _collect_const_booleans(func)
        changed = False

        for block in func.blocks:
            result = _qualifying_phi_branch(block)
            if result is None:
                continue

            phi, branch = result

            # Partition incoming entries into constant-True, constant-False,
            # and non-constant.  For constants, track the defining block
            # (which may differ from the phi's recorded predecessor when empty
            # intermediate blocks sit between the definer and the join).
            true_def_blocks:  List[MenaiCFGBlock] = []
            false_def_blocks: List[MenaiCFGBlock] = []
            keep: List[Tuple[MenaiCFGValue, MenaiCFGBlock]] = []

            for val, pred in phi.incoming:
                bool_val = _resolve_bool(val, const_values)
                if bool_val is True:
                    true_def_blocks.append(def_block_map.get(val.id, pred))
                elif bool_val is False:
                    false_def_blocks.append(def_block_map.get(val.id, pred))
                else:
                    keep.append((val, pred))

            if not true_def_blocks and not false_def_blocks:
                continue

            # Re-wire each constant defining block to jump directly to the
            # appropriate branch target, bypassing this join block.
            for def_block in true_def_blocks:
                _rewire_predecessor(def_block, branch.true_block)

            for def_block in false_def_blocks:
                _rewire_predecessor(def_block, branch.false_block)

            # Update the phi's incoming list to only the non-constant arms.
            if keep:
                if len(keep) == 1:
                    # Single remaining entry — the phi is now trivial.  Replace
                    # the branch condition with the sole incoming value directly
                    # and remove the phi instruction entirely.
                    sole_val, _ = keep[0]
                    block.instrs.remove(phi)
                    block.terminator = MenaiCFGBranchTerm(
                        cond=sole_val,
                        true_block=branch.true_block,
                        false_block=branch.false_block,
                    )
                else:
                    block.instrs[block.instrs.index(phi)] = MenaiCFGPhiInstr(
                        result=phi.result,
                        incoming=keep,
                    )
            else:
                # All arms were constant — all predecessors have been re-wired
                # away from this block.  Remove the phi so the block is
                # instruction-free.  Replace the stale branch (which still
                # references the now-removed phi result) with an unconditional
                # jump to the true target so the block is structurally valid
                # until SimplifyBlocks removes it as unreachable.
                block.instrs.remove(phi)
                block.terminator = MenaiCFGJumpTerm(target=branch.true_block)

            changed = True

        return changed


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _build_def_block_map(func: MenaiCFGFunction) -> Dict[int, 'MenaiCFGBlock']:
    """
    Return a map from SSA value id to the block that defines it, for every
    instruction in func that produces a result.
    """
    result: Dict[int, MenaiCFGBlock] = {}
    for block in func.blocks:
        for instr in block.instrs:
            r = getattr(instr, 'result', None)
            if r is not None:
                result[r.id] = block
    return result


def _collect_const_booleans(func: MenaiCFGFunction) -> Dict[int, MenaiBoolean]:
    """
    Return a map from SSA value id to MenaiBoolean for every
    MenaiCFGConstInstr that produces a boolean constant anywhere in func.
    """
    result: Dict[int, MenaiBoolean] = {}
    for block in func.blocks:
        for instr in block.instrs:
            if (
                isinstance(instr, MenaiCFGConstInstr)
                and isinstance(instr.value, MenaiBoolean)
            ):
                result[instr.result.id] = instr.value
    return result


def _resolve_bool(
    val: MenaiCFGValue,
    const_values: Dict[int, MenaiBoolean],
) -> bool | None:
    """
    Return True/False if val is a statically-known boolean constant,
    or None if it is not.
    """
    bool_val = const_values.get(val.id)
    if bool_val is None:
        return None
    return bool_val.value


def _qualifying_phi_branch(
    block: MenaiCFGBlock,
) -> tuple[MenaiCFGPhiInstr, MenaiCFGBranchTerm] | None:
    """
    Return (phi, branch) if block qualifies for branch constant propagation:
      - block has no patch_instrs
      - block has exactly one instruction, which is a MenaiCFGPhiInstr
      - block's terminator is a MenaiCFGBranchTerm whose condition is the
        phi's result

    Returns None if the block does not qualify.
    """
    if block.patch_instrs:
        return None

    if not isinstance(block.terminator, MenaiCFGBranchTerm):
        return None

    if len(block.instrs) != 1:
        return None

    instr = block.instrs[0]
    if not isinstance(instr, MenaiCFGPhiInstr):
        return None

    branch = block.terminator
    if branch.cond.id != instr.result.id:
        return None

    return instr, branch


def _rewire_predecessor(
    def_block: MenaiCFGBlock,
    new_target: MenaiCFGBlock,
) -> None:
    """
    Replace def_block's unconditional jump terminator with a jump to
    new_target.

    Only MenaiCFGJumpTerm terminators are rewired.  If def_block reaches the
    join via a branch arm (which cannot happen for a well-formed phi join
    produced by MenaiCFGBuilder, but is guarded here for safety) the
    terminator is left unchanged.
    """
    if isinstance(def_block.terminator, MenaiCFGJumpTerm):
        def_block.terminator = MenaiCFGJumpTerm(target=new_target)
