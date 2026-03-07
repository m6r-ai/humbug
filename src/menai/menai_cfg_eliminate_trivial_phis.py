"""
CFG pass: eliminate trivial phi nodes.

Replaces each phi node that has exactly one incoming entry with a direct
reference to that incoming value, removing the phi instruction entirely.
"""

from typing import List, Set

from menai.menai_cfg import (
    MenaiCFGFunction,
    MenaiCFGInstr,
    MenaiCFGPhiInstr,
    MenaiCFGValue,
    CFGSubstMap,
    relink_predecessors,
    subst_instr,
    subst_patch,
    subst_term,
)
from menai.menai_cfg_optimization_pass import MenaiCFGOptimizationPass


class MenaiCFGEliminateTrivialPhis(MenaiCFGOptimizationPass):
    """
    Replace each phi node that has exactly one incoming entry with a direct
    reference to that incoming value, then remove the phi instruction.

    A single-incoming phi is just an alias: ``v_phi = phi [(v_x, pred)]``
    means v_phi is always v_x.  We substitute v_x for v_phi everywhere in
    the function and drop the phi instruction entirely.

    Zero-incoming phis (which can arise after stale-phi pruning when a join
    block is unreachable) are left alone — the block will be removed by
    dead-block elimination in the next pass.

    Mutates block.instrs, block.patch_instrs, and block.terminator in place.
    """

    def _optimize_function(self, func: MenaiCFGFunction) -> tuple[MenaiCFGFunction, bool]:
        subst: CFGSubstMap = {}

        for block in func.blocks:
            for instr in block.instrs:
                if isinstance(instr, MenaiCFGPhiInstr) and len(instr.incoming) == 1:
                    incoming_val, _ = instr.incoming[0]
                    subst[instr.result.id] = incoming_val

        if not subst:
            return func, False

        def resolve(v: MenaiCFGValue) -> MenaiCFGValue:
            seen: Set[int] = set()
            while v.id in subst and v.id not in seen:
                seen.add(v.id)
                v = subst[v.id]

            return v

        for block in func.blocks:
            new_instrs: List[MenaiCFGInstr] = []
            for instr in block.instrs:
                if isinstance(instr, MenaiCFGPhiInstr) and instr.result.id in subst:
                    continue

                new_instrs.append(subst_instr(instr, resolve))

            block.instrs = new_instrs
            block.patch_instrs = [subst_patch(p, resolve) for p in block.patch_instrs]
            if block.terminator is not None:
                block.terminator = subst_term(block.terminator, resolve)

        relink_predecessors(func)
        return func, True
