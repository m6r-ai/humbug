"""
CFG pass: bypass empty blocks.

Eliminates blocks that are pure indirections — no instructions, no
patch_instrs, and an unconditional jump terminator — by re-pointing their
predecessors directly at their target.
"""

from typing import Dict, List, Set, Tuple

from menai.menai_cfg import (
    MenaiCFGBlock,
    MenaiCFGBranchTerm,
    MenaiCFGFunction,
    MenaiCFGJumpTerm,
    MenaiCFGPhiInstr,
    MenaiCFGValue,
    relink_predecessors,
    remap_term,
)
from menai.menai_cfg_optimization_pass import MenaiCFGOptimizationPass


class MenaiCFGBypassEmptyBlocks(MenaiCFGOptimizationPass):
    """
    Bypass blocks that are pure indirections: no instructions, no patch_instrs,
    and an unconditional jump terminator.

    For each such empty block E with ``MenaiCFGJumpTerm(T)``, E can be bypassed
    only if the bypass is safe with respect to the VM codegen's phi store
    mechanism.

    Phi stores are emitted only when a block emits its own ``JumpTerm``.  If a
    BranchTerm block is re-pointed to jump directly to a phi-bearing block, no
    phi store is emitted for the branch's contribution.  Therefore:

    A bypass chain E1 -> E2 -> ... -> T is safe if and only if:
      - T has no phi instructions, OR
      - No block in the chain {E1, E2, ...} has a BranchTerm predecessor.

    The entry block (blocks[0]) is never bypassed even if empty.
    """

    def _optimize_function(self, func: MenaiCFGFunction) -> tuple[MenaiCFGFunction, bool]:
        entry_id = func.blocks[0].id
        block_map: Dict[int, MenaiCFGBlock] = {b.id: b for b in func.blocks}

        pred_map: Dict[int, List[MenaiCFGBlock]] = {b.id: [] for b in func.blocks}
        for block in func.blocks:
            term = block.terminator
            if isinstance(term, MenaiCFGJumpTerm):
                if term.target.id in pred_map:
                    pred_map[term.target.id].append(block)

            elif isinstance(term, MenaiCFGBranchTerm):
                if term.true_block.id in pred_map:
                    pred_map[term.true_block.id].append(block)

                if term.false_block.id in pred_map:
                    pred_map[term.false_block.id].append(block)

        def is_empty(block: MenaiCFGBlock) -> bool:
            return (
                block.id != entry_id
                and not block.instrs
                and not block.patch_instrs
                and isinstance(block.terminator, MenaiCFGJumpTerm)
            )

        def has_phi(block: MenaiCFGBlock) -> bool:
            return any(isinstance(i, MenaiCFGPhiInstr) for i in block.instrs)

        def chain_has_branch_predecessor(start: MenaiCFGBlock) -> bool:
            seen: Set[int] = set()
            block = start
            while is_empty(block) and block.id not in seen:
                if any(
                    isinstance(pred.terminator, MenaiCFGBranchTerm)
                    for pred in pred_map.get(block.id, [])
                ):
                    return True

                seen.add(block.id)
                assert isinstance(block.terminator, MenaiCFGJumpTerm)
                next_b = block_map.get(block.terminator.target.id)
                if next_b is None:
                    break

                block = next_b

            return False

        def ultimate_target(block: MenaiCFGBlock) -> MenaiCFGBlock:
            seen: Set[int] = set()
            while is_empty(block) and block.id not in seen:
                seen.add(block.id)
                assert isinstance(block.terminator, MenaiCFGJumpTerm)
                next_b = block_map.get(block.terminator.target.id)
                if next_b is None:
                    break

                block = next_b

            return block

        bypass: Dict[int, MenaiCFGBlock] = {}
        for block in func.blocks:
            if is_empty(block):
                target = ultimate_target(block)
                if target.id != block.id and not (
                    has_phi(target) and chain_has_branch_predecessor(block)
                ):
                    bypass[block.id] = target

        if not bypass:
            return func, False

        def remap_block(b: MenaiCFGBlock) -> MenaiCFGBlock:
            return bypass.get(b.id, b)

        for block in func.blocks:
            if block.id in bypass:
                continue

            for i, instr in enumerate(block.instrs):
                if not isinstance(instr, MenaiCFGPhiInstr):
                    continue

                new_incoming: List[Tuple[MenaiCFGValue, MenaiCFGBlock]] = []
                for val, pred in instr.incoming:
                    if pred.id in bypass:
                        actual_preds = pred_map.get(pred.id, [])
                        for actual_pred in actual_preds:
                            remapped = _find_non_empty_pred(actual_pred, bypass, pred_map)
                            new_incoming.append((val, remapped))

                    else:
                        new_incoming.append((val, pred))

                block.instrs[i] = MenaiCFGPhiInstr(
                    result=instr.result,
                    incoming=new_incoming,
                )

            if block.terminator is not None:
                block.terminator = remap_term(block.terminator, remap_block)

        new_blocks = [b for b in func.blocks if b.id not in bypass]
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


def _find_non_empty_pred(
    block: MenaiCFGBlock,
    bypass: Dict[int, MenaiCFGBlock],
    pred_map: Dict[int, List[MenaiCFGBlock]],
) -> MenaiCFGBlock:
    """Walk up the predecessor chain until we find a non-bypassed block."""
    seen: Set[int] = set()
    while block.id in bypass and block.id not in seen:
        seen.add(block.id)
        preds = pred_map.get(block.id, [])
        if not preds:
            break

        block = preds[0]

    return block
