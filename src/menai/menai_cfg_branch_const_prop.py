"""
CFG pass: branch and return constant propagation.

When a phi node is used only as the condition of a MenaiCFGBranchTerm, or
only as the value of a MenaiCFGReturnTerm, and some of its incoming values
are statically-known boolean constants, those arms do not need to flow
through the phi join at all.

For each such constant-valued incoming arm, this pass re-wires the *defining
block* of the constant (the block containing the MenaiCFGConstInstr) to jump
directly to the appropriate target, bypassing the phi join block entirely.
Using the defining block rather than the phi's recorded predecessor block
handles the case where empty intermediate blocks (left behind by
MenaiCFGCollapsePhiChains) sit between the defining block and the join.

Branch case (phi → BranchTerm):

  An incoming True value means that predecessor always takes the true branch;
  an incoming False value means it always takes the false branch.  The
  defining block is rewired to jump directly to true_block or false_block.

Return case (phi → ReturnTerm):

  An incoming constant value means that predecessor always returns that
  constant.  The defining block is rewired to return the constant's own SSA
  value directly instead of jumping to the join.

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

Example — branch case, the (or p1 (or p2 (or p3 p4))) pattern:

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
    block3: (empty) jump → loop_body
    block5: (empty) jump → loop_body
    block7: (empty) jump → loop_body
    block8: jump → block11   (non-constant, untouched)
    block9:  jump block10    (now unreachable — SimplifyBlocks cleans up)
    block10: jump block11    (now unreachable — SimplifyBlocks cleans up)
    block11: %v = phi [%r←8]
             branch %v → loop_body / exit

Example — return case, the (and A B) pattern:

  Before:
    then_block: %r = B  →  jump join
    else_block: %f = #f →  jump join
    join: %v = phi [%r←then, %f←else]
          return %v

  After (else_block rewired to return #f directly):
    then_block: %r = B  →  jump join
    else_block: return %f          (using %f, the SSA value defined in else_block)
    join: %v = phi [%r←then]   (trivial — SimplifyBlocks eliminates join)
          return %v
"""

from typing import Dict, List, Tuple

from menai.menai_cfg import (
    MenaiCFGBlock,
    MenaiCFGBranchTerm,
    MenaiCFGConstInstr,
    MenaiCFGFunction,
    MenaiCFGJumpTerm,
    MenaiCFGPhiInstr,
    MenaiCFGReturnTerm,
    MenaiCFGValue,
    relink_predecessors,
)
from menai.menai_cfg_optimization_pass import MenaiCFGOptimizationPass
from menai.menai_value import MenaiBoolean, MenaiValue


class MenaiCFGBranchConstProp(MenaiCFGOptimizationPass):
    """
    Re-wire the defining blocks of phi incoming values that are
    statically-known constants, when the phi feeds a branch or return.

    Branch case: for each phi P whose result is used only as the condition
    of a MenaiCFGBranchTerm in the same block, and for each incoming
    (val, pred) pair where val is a statically-known boolean constant:

      - Find the block that *defines* val (the block containing the
        MenaiCFGConstInstr for val).  This may differ from pred when empty
        intermediate blocks (left by an earlier CollapsePhiChains pass) sit
        between the defining block and the join.
      - Replace the defining block's jump-to-join with a direct jump to the
        branch's true_block (for True) or false_block (for False).
      - Remove the rewired entries from P's incoming list.

    Return case: for each phi P whose result is used only as the value of a
    MenaiCFGReturnTerm in the same block, and for each incoming (val, pred)
    pair where val is a statically-known constant (any type):

      - Find the defining block of val.
      - Replace the defining block's jump-to-join with a direct
        MenaiCFGReturnTerm returning val (the SSA value already defined in
        that block).
      - Remove the rewired entries from P's incoming list.

    In both cases, if P's incoming list becomes empty the phi and terminal
    are removed and the join block becomes dead (no predecessors will jump
    to it).
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
        Execute one round of constant propagation.

        Scans every block for a qualifying phi+terminal pair and re-wires all
        constant-valued defining blocks.  Returns True if any change was made.
        """
        const_values: Dict[int, MenaiValue] = _collect_const_values(func)
        changed = False

        for block in func.blocks:
            result = _qualifying_phi_terminal(block)
            if result is None:
                continue

            phi, terminal = result

            # Partition incoming entries into constant and non-constant.
            # For constants, track both the defining block and the SSA value
            # itself (needed to construct the return terminator).  The defining
            # block may differ from the phi's recorded predecessor when empty
            # intermediate blocks sit between the definer and the join.
            const_arms: List[Tuple[MenaiCFGBlock, MenaiCFGValue]] = []
            keep: List[Tuple[MenaiCFGValue, MenaiCFGBlock]] = []

            for val, pred in phi.incoming:
                if val.id in const_values:
                    def_block = def_block_map.get(val.id, pred)
                    if isinstance(def_block.terminator, MenaiCFGJumpTerm):
                        const_arms.append((def_block, val))

                    else:
                        keep.append((val, pred))

                else:
                    keep.append((val, pred))

            if not const_arms:
                continue

            # Re-wire each constant defining block to bypass this join block.
            for def_block, const_ssa_val in const_arms:
                _rewire_predecessor(def_block, terminal, const_ssa_val, const_values)

            # Update the phi's incoming list to only the non-constant arms.
            if keep:
                if len(keep) == 1:
                    # Single remaining entry — the phi is now trivial.  Replace
                    # the terminal's value/condition with the sole incoming value
                    # directly and remove the phi instruction entirely.
                    sole_val, _ = keep[0]
                    block.instrs.remove(phi)
                    if isinstance(terminal, MenaiCFGBranchTerm):
                        block.terminator = MenaiCFGBranchTerm(
                            cond=sole_val,
                            true_block=terminal.true_block,
                            false_block=terminal.false_block,
                        )

                    else:
                        assert isinstance(terminal, MenaiCFGReturnTerm)
                        block.terminator = MenaiCFGReturnTerm(value=sole_val)

                else:
                    block.instrs[block.instrs.index(phi)] = MenaiCFGPhiInstr(
                        result=phi.result,
                        incoming=keep,
                    )

            else:
                # All arms were constant — all predecessors have been re-wired
                # away from this block.  Remove the phi so the block is
                # instruction-free.  Replace the stale terminal with a
                # structurally valid one; the block is now unreachable and
                # SimplifyBlocks will drop it.
                block.instrs.remove(phi)
                if isinstance(terminal, MenaiCFGBranchTerm):
                    block.terminator = MenaiCFGJumpTerm(target=terminal.true_block)

                # For ReturnTerm the existing terminator is already valid as-is.

            changed = True

        return changed


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


def _collect_const_values(func: MenaiCFGFunction) -> Dict[int, MenaiValue]:
    """
    Return a map from SSA value id to MenaiValue for every
    MenaiCFGConstInstr anywhere in func.
    """
    result: Dict[int, MenaiValue] = {}
    for block in func.blocks:
        for instr in block.instrs:
            if isinstance(instr, MenaiCFGConstInstr):
                result[instr.result.id] = instr.value

    return result


def _qualifying_phi_terminal(
    block: MenaiCFGBlock,
) -> 'tuple[MenaiCFGPhiInstr, MenaiCFGBranchTerm | MenaiCFGReturnTerm] | None':
    """
    Return (phi, terminal) if block qualifies for constant propagation:
      - block has no patch_instrs
      - block has exactly one instruction, which is a MenaiCFGPhiInstr
      - block's terminator is a MenaiCFGBranchTerm whose condition is the
        phi's result, OR a MenaiCFGReturnTerm whose value is the phi's result

    Returns None if the block does not qualify.
    """
    if block.patch_instrs:
        return None

    if len(block.instrs) != 1:
        return None

    instr = block.instrs[0]
    if not isinstance(instr, MenaiCFGPhiInstr):
        return None

    term = block.terminator
    if isinstance(term, MenaiCFGBranchTerm) and term.cond.id == instr.result.id:
        return instr, term

    if isinstance(term, MenaiCFGReturnTerm) and term.value.id == instr.result.id:
        return instr, term

    return None


def _rewire_predecessor(
    def_block: MenaiCFGBlock,
    terminal: MenaiCFGBranchTerm | MenaiCFGReturnTerm,
    const_ssa_val: MenaiCFGValue,
    const_values: Dict[int, MenaiValue],
) -> None:
    """
    Replace def_block's unconditional jump terminator with the appropriate
    bypass terminator, determined by the join block's terminal type:

    - BranchTerm: jump directly to true_block (if the constant is truthy) or
      false_block (if falsy), bypassing the join.
    - ReturnTerm: return const_ssa_val directly (the SSA value is already
      defined in def_block, so the register allocator can resolve it).

    Only MenaiCFGJumpTerm terminators are rewired.  If def_block already has
    a non-jump terminator (which cannot happen for a well-formed phi join
    produced by MenaiCFGBuilder, but is guarded here for safety) the
    terminator is left unchanged.
    """
    if not isinstance(def_block.terminator, MenaiCFGJumpTerm):
        return

    if isinstance(terminal, MenaiCFGBranchTerm):
        menai_val = const_values[const_ssa_val.id]
        target = (
            terminal.true_block
            if (isinstance(menai_val, MenaiBoolean) and menai_val.value)
            else terminal.false_block
        )
        def_block.terminator = MenaiCFGJumpTerm(target=target)
        # The constant instruction is now dead — its result was only ever
        # consumed by the phi, which has been removed.  Drop it so the
        # vcode builder does not emit a spurious LOAD_* instruction.
        def_block.instrs = [
            i for i in def_block.instrs
            if not (isinstance(i, MenaiCFGConstInstr) and i.result.id == const_ssa_val.id)
        ]

    else:
        assert isinstance(terminal, MenaiCFGReturnTerm)
        def_block.terminator = MenaiCFGReturnTerm(value=const_ssa_val)
