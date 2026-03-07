"""
CFG pass: prune stale phi entries.

Removes incoming entries from phi nodes where the listed predecessor block
has no actual control-flow edge to the phi's containing block.
"""

from typing import Set, Tuple

from menai.menai_cfg import (
    MenaiCFGBranchTerm,
    MenaiCFGFunction,
    MenaiCFGJumpTerm,
    MenaiCFGPhiInstr,
    relink_predecessors,
)
from menai.menai_cfg_optimization_pass import MenaiCFGOptimizationPass


class MenaiCFGPruneStalePhiEntries(MenaiCFGOptimizationPass):
    """
    Remove incoming entries from phi nodes where the listed predecessor block
    has no actual control-flow edge to the phi's containing block.

    A control-flow edge exists from block P to block B iff P's terminator is:
    - MenaiCFGJumpTerm(B)
    - MenaiCFGBranchTerm(..., true_block=B, ...) or (..., false_block=B)

    Mutates block.instrs in place for blocks that contain stale phi entries.
    """

    def _optimize_function(self, func: MenaiCFGFunction) -> tuple[MenaiCFGFunction, bool]:
        edges: Set[Tuple[int, int]] = set()
        for block in func.blocks:
            term = block.terminator
            if isinstance(term, MenaiCFGJumpTerm):
                edges.add((block.id, term.target.id))

            elif isinstance(term, MenaiCFGBranchTerm):
                edges.add((block.id, term.true_block.id))
                edges.add((block.id, term.false_block.id))

        changed = False

        for block in func.blocks:
            for i, instr in enumerate(block.instrs):
                if not isinstance(instr, MenaiCFGPhiInstr):
                    continue

                pruned = [
                    (val, pred)
                    for val, pred in instr.incoming
                    if (pred.id, block.id) in edges
                ]
                if len(pruned) != len(instr.incoming):
                    block.instrs[i] = MenaiCFGPhiInstr(
                        result=instr.result,
                        incoming=pruned,
                    )
                    changed = True

        if changed:
            relink_predecessors(func)

        return func, changed
