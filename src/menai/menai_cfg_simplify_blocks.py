"""
CFG pass: simplify blocks.

Two sub-passes run to a joint fixed point:

1. Empty-block bypass
   Eliminates blocks that are pure indirections — no instructions, no
   patch_instrs, and an unconditional jump terminator — by re-pointing
   their predecessors directly at their target.

2. Trivial return inlining
   Inlines trivial terminal blocks directly into their jump predecessors,
   eliminating the jump entirely.  A trivial terminal block has no
   patch_instrs, a MenaiCFGReturnTerm, and either no instructions or a
   single MenaiCFGConstInstr.  For each predecessor that reaches such a
   block via an unconditional jump, the jump is replaced by a copy of the
   block's content (with a fresh SSA value when a const is involved).  If
   all jump predecessors are inlined the terminal block itself is removed.
"""

from typing import Dict, List, Optional, Set, Tuple

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
    remap_term,
    value_ids_in_instr,
    value_ids_in_term,
)
from menai.menai_cfg_optimization_pass import MenaiCFGOptimizationPass


class MenaiCFGSimplifyBlocks(MenaiCFGOptimizationPass):
    """
    Simplify the CFG by eliminating unnecessary block boundaries.

    Sub-pass 1 — empty-block bypass:
      For each non-entry block E with no instructions, no patch_instrs, and
      an unconditional jump terminator, re-point E's predecessors directly
      at E's target and remove E.

      Bypass is conditional on phi-store safety: if E's ultimate target has
      phi instructions, E may only be bypassed when no block in the bypass
      chain has a BranchTerm predecessor (otherwise the phi store emitted by
      the JumpTerm would be lost).

    Sub-pass 2 — trivial return inlining:
      For each block T with no patch_instrs, a MenaiCFGReturnTerm, and at
      most one MenaiCFGConstInstr or exactly one MenaiCFGPhiInstr (and no
      other instructions), inline T into every predecessor that reaches it
      via an unconditional jump.

      For the const case a fresh SSA value is allocated so that SSA
      single-definition is preserved.  For the phi case each predecessor
      already holds its contributing value, so that value becomes the return
      directly — no fresh allocation needed.  If all jump predecessors are
      inlined, T is removed from the function.
    """

    def _optimize_function(self, func: MenaiCFGFunction) -> tuple[MenaiCFGFunction, bool]:
        changed = False

        func, c = self._bypass_empty_blocks(func)
        changed = changed or c

        func, c = self._inline_trivial_returns(func)
        changed = changed or c

        return func, changed

    def _bypass_empty_blocks(self, func: MenaiCFGFunction) -> tuple[MenaiCFGFunction, bool]:
        """
        Bypass blocks that are pure indirections: no instructions, no
        patch_instrs, and an unconditional jump terminator.

        For each such empty block E with ``MenaiCFGJumpTerm(T)``, E can be
        bypassed only if the bypass is safe with respect to the VM codegen's
        phi store mechanism.

        Phi stores are emitted only when a block emits its own ``JumpTerm``.
        If a BranchTerm block is re-pointed to jump directly to a phi-bearing
        block, no phi store is emitted for the branch's contribution.
        Therefore:

        A bypass chain E1 -> E2 -> ... -> T is safe if and only if:
          - T has no phi instructions, OR
          - No block in the chain {E1, E2, ...} has a BranchTerm predecessor.

        The entry block (blocks[0]) is never bypassed even if empty.
        """
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

    def _inline_trivial_returns(self, func: MenaiCFGFunction) -> tuple[MenaiCFGFunction, bool]:
        """
        Inline trivial terminal blocks into their unconditional-jump predecessors.

        A trivial terminal block T qualifies when:
          - T has no patch_instrs
          - T has a MenaiCFGReturnTerm
          - T's instrs are one of:
              (a) empty
              (b) exactly one MenaiCFGConstInstr (no phi)
              (c) exactly one MenaiCFGPhiInstr whose result is the return value

        For each predecessor of T that reaches it via a MenaiCFGJumpTerm, the
        jump is replaced by an inlined copy of T's content.  When T contains a
        MenaiCFGConstInstr, a fresh SSA value is created for each copy to
        preserve the single-definition invariant of SSA form.  When T contains
        a MenaiCFGPhiInstr, each predecessor already holds its contributing
        value, which becomes the return value directly.

        If every predecessor that reaches T via a jump is inlined, T is removed
        from the function.  Predecessors that reach T via a branch are not
        inlined (a branch cannot be replaced by a return in place).
        """
        next_id = _max_value_id(func) + 1

        def fresh_value(hint: str) -> MenaiCFGValue:
            nonlocal next_id
            v = MenaiCFGValue(id=next_id, hint=hint)
            next_id += 1
            return v

        def trivial_return_content(
            block: MenaiCFGBlock,
        ) -> Optional[Tuple[object, MenaiCFGReturnTerm]]:
            """
            Return (content, return_term) if block is a trivial terminal block,
            or None if it does not qualify.

            content is one of:
              None                 — empty block (no instructions)
              MenaiCFGConstInstr   — single const instruction
              MenaiCFGPhiInstr     — single phi whose result is the return value
            """
            if block.patch_instrs:
                return None

            if not isinstance(block.terminator, MenaiCFGReturnTerm):
                return None

            if len(block.instrs) == 0:
                return (None, block.terminator)

            if len(block.instrs) == 1 and isinstance(block.instrs[0], MenaiCFGConstInstr):
                return (block.instrs[0], block.terminator)

            if (
                len(block.instrs) == 1
                and isinstance(block.instrs[0], MenaiCFGPhiInstr)
                and block.instrs[0].result.id == block.terminator.value.id
            ):
                return (block.instrs[0], block.terminator)

            return None

        changed = False
        inlined_block_ids: Set[int] = set()

        for target in list(func.blocks):
            content = trivial_return_content(target)
            if content is None:
                continue

            instr, return_term = content

            # Find predecessors that reach target via an unconditional jump.
            jump_preds = [
                b for b in target.predecessors
                if isinstance(b.terminator, MenaiCFGJumpTerm)
                and b.terminator.target.id == target.id
            ]

            if not jump_preds:
                continue

            actually_inlined: Set[int] = set()
            for pred in jump_preds:
                if isinstance(instr, MenaiCFGConstInstr):
                    # Duplicate the const with a fresh SSA value.
                    new_val = fresh_value(instr.result.hint)
                    pred.instrs.append(
                        MenaiCFGConstInstr(result=new_val, value=instr.value)
                    )
                    pred.terminator = MenaiCFGReturnTerm(value=new_val)

                elif isinstance(instr, MenaiCFGPhiInstr):
                    # Each predecessor already holds its contributing value.
                    # Look up the incoming entry for this predecessor.
                    contributing = next(
                        (val for val, blk in instr.incoming if blk.id == pred.id),
                        None,
                    )
                    if contributing is None:
                        # Predecessor not listed in phi incomings — skip.
                        continue
                    pred.terminator = MenaiCFGReturnTerm(value=contributing)

                else:
                    pred.terminator = MenaiCFGReturnTerm(value=return_term.value)

                actually_inlined.add(pred.id)
                changed = True

            # Remove target if every predecessor was a jump predecessor that
            # we just inlined, and no predecessor reaches target via a branch
            # or other non-jump edge.
            if (len(actually_inlined) == len(jump_preds) and len(jump_preds) == len(target.predecessors)):
                inlined_block_ids.add(target.id)

        if not changed:
            return func, False

        new_blocks = [b for b in func.blocks if b.id not in inlined_block_ids]
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


def _max_value_id(func: MenaiCFGFunction) -> int:
    """Return the highest SSA value id present anywhere in func."""
    max_id = -1

    def _check(vid: int) -> None:
        nonlocal max_id
        max_id = max(max_id, vid)

    for block in func.blocks:
        for instr in block.instrs:
            result = getattr(instr, 'result', None)
            if result is not None:
                _check(result.id)

            if isinstance(instr, MenaiCFGPhiInstr):
                for incoming_val, _ in instr.incoming:
                    _check(incoming_val.id)

            for vid in value_ids_in_instr(instr):
                _check(vid)

        for patch in block.patch_instrs:
            _check(patch.closure.id)
            _check(patch.value.id)

        if block.terminator is not None:
            for vid in value_ids_in_term(block.terminator):
                _check(vid)

    return max_id


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
