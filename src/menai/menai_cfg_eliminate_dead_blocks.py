"""
CFG pass: eliminate dead blocks.

Removes blocks that are unreachable from the entry block.
"""

from typing import Set

from menai.menai_cfg import (
    MenaiCFGBlock,
    MenaiCFGBranchTerm,
    MenaiCFGFunction,
    MenaiCFGJumpTerm,
    MenaiCFGPhiInstr,
    relink_predecessors,
)
from menai.menai_cfg_optimization_pass import MenaiCFGOptimizationPass


class MenaiCFGEliminateDeadBlocks(MenaiCFGOptimizationPass):
    """
    Remove blocks that are unreachable from the entry block.

    Phi incoming entries that reference removed blocks are pruned in place.
    """

    def _optimize_function(self, func: MenaiCFGFunction) -> tuple[MenaiCFGFunction, bool]:
        reachable: Set[int] = set()

        def dfs(block: MenaiCFGBlock) -> None:
            if block.id in reachable:
                return

            reachable.add(block.id)
            term = block.terminator
            if isinstance(term, MenaiCFGJumpTerm):
                dfs(term.target)

            elif isinstance(term, MenaiCFGBranchTerm):
                dfs(term.true_block)
                dfs(term.false_block)

        if func.blocks:
            dfs(func.blocks[0])

        dead = {b.id for b in func.blocks if b.id not in reachable}
        if not dead:
            return func, False

        for block in func.blocks:
            if block.id in dead:
                continue

            for i, instr in enumerate(block.instrs):
                if not isinstance(instr, MenaiCFGPhiInstr):
                    continue

                pruned = [
                    (val, pred)
                    for val, pred in instr.incoming
                    if pred.id not in dead
                ]
                if len(pruned) != len(instr.incoming):
                    block.instrs[i] = MenaiCFGPhiInstr(
                        result=instr.result,
                        incoming=pruned,
                    )

        new_blocks = [b for b in func.blocks if b.id not in dead]
        new_func = MenaiCFGFunction(
            blocks=new_blocks,
            params=func.params,
            free_vars=func.free_vars,
            is_variadic=func.is_variadic,
            binding_name=func.binding_name,
            source_line=func.source_line,
            source_file=func.source_file,
        )
        relink_predecessors(new_func)
        return new_func, True
