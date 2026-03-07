"""
CFG pass: collapse phi chains.

Eliminates phi-of-phi redundancy that arises from nested `if` expressions.
When the result of a phi node is used *only* as an incoming value in one or
more other phi nodes, the intermediate phi can be bypassed: each consuming
phi absorbs the intermediate's incoming entries in its place.

Example before:

    block A:  jump → join1
    block B:  jump → join1
    join1:    %v1 = phi [(%a, A), (%b, B)]
              jump → join2
    join2:    %v2 = phi [(%v1, join1), (%c, C)]

Example after:

    join2:    %v2 = phi [(%a, A), (%b, B), (%c, C)]

join1's phi is removed.  If join1 now has no instructions it becomes an
empty block, which MenaiCFGBypassEmptyBlocks will then eliminate.

Safety
------
The transformation is valid when:
  1. The intermediate phi result (%v1) is used *only* as a phi incoming
     value — never in a builtin, call, return, branch condition, etc.
  2. No consuming phi already has an entry from one of the intermediate
     phi's predecessor blocks (which would create a duplicate predecessor,
     violating the one-entry-per-predecessor invariant for phi nodes).

Condition 2 can arise when a consuming phi has multiple entries that would
expand to the same predecessor block.  The pass skips any collapse that
would produce such a conflict.

Menai is pure, so dead-code elimination is always safe (AGENTS.md).
"""

from typing import Dict, List, Set, Tuple

from menai.menai_cfg import (
    MenaiCFGBlock,
    MenaiCFGBuiltinInstr,
    MenaiCFGCallInstr,
    MenaiCFGApplyInstr,
    MenaiCFGBranchTerm,
    MenaiCFGFunction,
    MenaiCFGInstr,
    MenaiCFGMakeClosureInstr,
    MenaiCFGPatchClosureInstr,
    MenaiCFGPhiInstr,
    MenaiCFGReturnTerm,
    MenaiCFGSelfLoopTerm,
    MenaiCFGTailApplyTerm,
    MenaiCFGTailCallTerm,
    MenaiCFGTerminator,
    MenaiCFGTraceInstr,
    MenaiCFGValue,
    relink_predecessors,
)
from menai.menai_cfg_optimization_pass import MenaiCFGOptimizationPass


class MenaiCFGCollapsePhiChains(MenaiCFGOptimizationPass):
    """
    Replace phi-of-phi chains with a single flat phi, and remove phi nodes
    whose results are never used.

    For each phi P1 whose result is used *only* as an incoming value in
    other phi nodes (or not at all), expand each consuming phi by
    substituting P1's incoming entries for the P1 reference, then remove
    P1.

    After collapsing, blocks that contained only the now-removed phi (and
    an unconditional jump) become empty and will be eliminated by
    MenaiCFGBypassEmptyBlocks in the next pass.

    Mutates block.instrs in place.
    """

    def _optimize_function(self, func: MenaiCFGFunction) -> tuple[MenaiCFGFunction, bool]:
        changed_overall = False

        # Iterate to fixed point: each round may expose new candidates.
        while True:
            round_changed = self._run_one_round(func)
            if not round_changed:
                break
            changed_overall = True

        if changed_overall:
            relink_predecessors(func)

        return func, changed_overall

    def _run_one_round(self, func: MenaiCFGFunction) -> bool:
        """
        Execute one round of phi-chain collapsing.

        Returns True if any change was made.
        """
        # Build a map: value id → the phi instruction that defines it.
        phi_defs: Dict[int, MenaiCFGPhiInstr] = {}
        for block in func.blocks:
            for instr in block.instrs:
                if isinstance(instr, MenaiCFGPhiInstr):
                    phi_defs[instr.result.id] = instr

        if not phi_defs:
            return False

        # Count all uses of each phi result, distinguishing phi-incoming
        # uses from all other uses.
        total_uses: Dict[int, int] = {vid: 0 for vid in phi_defs}
        phi_uses: Dict[int, int] = {vid: 0 for vid in phi_defs}

        for block in func.blocks:
            for instr in block.instrs:
                if isinstance(instr, MenaiCFGPhiInstr):
                    for incoming_val, _ in instr.incoming:
                        if incoming_val.id in phi_defs:
                            total_uses[incoming_val.id] += 1
                            phi_uses[incoming_val.id] += 1
                else:
                    for vid in _value_ids_in_instr(instr):
                        if vid in phi_defs:
                            total_uses[vid] += 1

            for patch in block.patch_instrs:
                for vid in (patch.closure.id, patch.value.id):
                    if vid in phi_defs:
                        total_uses[vid] += 1

            term = block.terminator
            if term is not None:
                for vid in _value_ids_in_term(term):
                    if vid in phi_defs:
                        total_uses[vid] += 1

        # Candidates: phi results whose every use is as a phi incoming value
        # (including zero uses — those are simply dead).
        candidates: Set[int] = {
            vid
            for vid in phi_defs
            if total_uses[vid] == phi_uses[vid]
        }

        if not candidates:
            return False

        changed = False

        # Phase 1: expand candidate phi references in consuming phis.
        for block in func.blocks:
            new_instrs: List[MenaiCFGInstr] = []
            for instr in block.instrs:
                if not isinstance(instr, MenaiCFGPhiInstr):
                    new_instrs.append(instr)
                    continue

                # Build the full set of predecessor block ids that this phi
                # will have after expansion, for conflict detection.
                # We compute it upfront from all non-candidate entries plus
                # the expanded entries of each candidate entry.
                non_candidate_preds = {
                    pred.id
                    for val, pred in instr.incoming
                    if val.id not in candidates
                }

                expanded_incoming: List[Tuple[MenaiCFGValue, MenaiCFGBlock]] = []
                instr_changed = False

                for incoming_val, pred_block in instr.incoming:
                    if incoming_val.id not in candidates:
                        expanded_incoming.append((incoming_val, pred_block))
                        continue

                    src_phi = phi_defs[incoming_val.id]

                    # Conflict check: would any of src_phi's predecessor blocks
                    # already appear in the final phi (from non-candidate entries
                    # or from already-expanded candidate entries)?
                    already_present = {b.id for _, b in expanded_incoming}
                    src_pred_ids = {b.id for _, b in src_phi.incoming}
                    if already_present & src_pred_ids or non_candidate_preds & src_pred_ids:
                        # Conflict: keep this entry unexpanded.
                        expanded_incoming.append((incoming_val, pred_block))
                        continue

                    expanded_incoming.extend(src_phi.incoming)
                    instr_changed = True

                if instr_changed:
                    new_instrs.append(
                        MenaiCFGPhiInstr(result=instr.result, incoming=expanded_incoming)
                    )
                    changed = True
                else:
                    new_instrs.append(instr)

            block.instrs = new_instrs

        if not changed:
            # No expansions happened, but there may be zero-use candidates
            # to remove. Check for those.
            dead = {vid for vid in candidates if total_uses[vid] == 0}
            if dead:
                for block in func.blocks:
                    block.instrs = [
                        instr for instr in block.instrs
                        if not (isinstance(instr, MenaiCFGPhiInstr) and instr.result.id in dead)
                    ]
                return True
            return False

        # Phase 2: remove phi instructions that are now unreferenced.
        # Recount uses after the expansions to find newly-dead phis.
        new_phi_uses: Dict[int, int] = {vid: 0 for vid in phi_defs}
        for block in func.blocks:
            for instr in block.instrs:
                if isinstance(instr, MenaiCFGPhiInstr):
                    for incoming_val, _ in instr.incoming:
                        if incoming_val.id in new_phi_uses:
                            new_phi_uses[incoming_val.id] += 1

        dead_phis: Set[int] = {
            vid for vid in candidates if new_phi_uses[vid] == 0
        }

        if dead_phis:
            for block in func.blocks:
                block.instrs = [
                    instr for instr in block.instrs
                    if not (isinstance(instr, MenaiCFGPhiInstr) and instr.result.id in dead_phis)
                ]

        return True


def _value_ids_in_instr(instr: MenaiCFGInstr) -> List[int]:
    """Return all input value ids referenced by a non-phi instruction."""
    if isinstance(instr, MenaiCFGBuiltinInstr):
        return [a.id for a in instr.args]

    if isinstance(instr, MenaiCFGCallInstr):
        return [instr.func.id] + [a.id for a in instr.args]

    if isinstance(instr, MenaiCFGApplyInstr):
        return [instr.func.id, instr.arg_list.id]

    if isinstance(instr, MenaiCFGMakeClosureInstr):
        return [c.id for c in instr.captures]

    if isinstance(instr, MenaiCFGPatchClosureInstr):
        return [instr.closure.id, instr.value.id]

    if isinstance(instr, MenaiCFGTraceInstr):
        return [m.id for m in instr.messages] + [instr.value.id]

    # MenaiCFGConstInstr, MenaiCFGGlobalInstr, MenaiCFGParamInstr,
    # MenaiCFGFreeVarInstr: no input value references.
    return []


def _value_ids_in_term(term: MenaiCFGTerminator) -> List[int]:
    """Return all input value ids referenced by a terminator."""
    if isinstance(term, MenaiCFGReturnTerm):
        return [term.value.id]

    if isinstance(term, MenaiCFGBranchTerm):
        return [term.cond.id]

    if isinstance(term, MenaiCFGTailCallTerm):
        return [term.func.id] + [a.id for a in term.args]

    if isinstance(term, MenaiCFGTailApplyTerm):
        return [term.func.id, term.arg_list.id]

    if isinstance(term, MenaiCFGSelfLoopTerm):
        return [a.id for a in term.args]

    # MenaiCFGJumpTerm, MenaiCFGRaiseTerm: no value references.
    return []
