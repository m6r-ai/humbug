"""
CFG pass: collapse phi chains.

Two sub-passes run to a joint fixed point:

1. Phi-chain collapsing
   Eliminates phi-of-phi redundancy that arises from nested `if` expressions.
When the result of a phi node is used *only* as an incoming value in one or
more other phi nodes, the intermediate phi can be bypassed: each consuming
phi absorbs the intermediate's incoming entries in its place.

2. Constant phi folding
   When every incoming value of a phi node is a MenaiCFGConstInstr result
   carrying the same constant, the phi is replaced by a single
   MenaiCFGConstInstr.  All uses of the phi result are substituted with the
   new const value.  The phi block may then become an empty-jump block that
   MenaiCFGSimplifyBlocks will eliminate.

Example (phi-chain collapse) before:

    block A:  jump → join1
    block B:  jump → join1
    join1:    %v1 = phi [(%a, A), (%b, B)]
              jump → join2
    join2:    %v2 = phi [(%v1, join1), (%c, C)]

Example after:

    join2:    %v2 = phi [(%a, A), (%b, B), (%c, C)]

join1's phi is removed.  If join1 now has no instructions it becomes an
empty block, which MenaiCFGSimplifyBlocks will then eliminate.

Safety (phi-chain collapse)
---------------------------
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
    MenaiCFGConstInstr,
    MenaiCFGFunction,
    MenaiCFGInstr,
    MenaiCFGMakeClosureInstr,
    MenaiCFGPhiInstr,
    MenaiCFGValue,
    relink_predecessors,
    subst_instr,
    subst_patch,
    subst_term,
    value_ids_in_instr,
    value_ids_in_term,
)
from menai.menai_value import MenaiValue
from menai.menai_cfg_optimization_pass import MenaiCFGOptimizationPass


class MenaiCFGCollapsePhiChains(MenaiCFGOptimizationPass):
    """
    Replace phi-of-phi chains with a single flat phi, and remove phi nodes
    whose results are never used.

    Sub-pass 1 folds constant phis: when every incoming value of a phi is a
    const result carrying the same MenaiValue, the phi is replaced by a
    MenaiCFGConstInstr and all uses are substituted.

    Sub-pass 2 collapses phi chains: for each phi P1 whose result is used *only* as an incoming value in
    other phi nodes (or not at all), expand each consuming phi by
    substituting P1's incoming entries for the P1 reference, then remove
    P1.

    After collapsing, blocks that contained only the now-removed phi (and
    an unconditional jump) become empty and will be eliminated by
MenaiCFGSimplifyBlocks in the next pass.

    Mutates block.instrs in place.
    """

    def _optimize_function(self, func: MenaiCFGFunction) -> tuple[MenaiCFGFunction, bool]:
        changed_overall = False

        # Iterate to fixed point: each round may expose new candidates.
        while True:
            round_changed = self._fold_constant_phis(func)
            round_changed = self._run_one_round(func) or round_changed
            if not round_changed:
                break
            changed_overall = True

        if changed_overall:
            relink_predecessors(func)

        return func, changed_overall

    def _fold_constant_phis(self, func: MenaiCFGFunction) -> bool:
        """
        Fold phi nodes whose every incoming value is a const result carrying
        the same MenaiValue into a single MenaiCFGConstInstr.

        Builds a map from each phi result id to the folded const value, then
        substitutes all uses of those ids throughout the function.  The phi
        instructions themselves are then removed.

        Returns True if any phi was folded.
        """
        # Phase 1: identify foldable phis and the constant they fold to.
        # We need a map from SSA value id → MenaiCFGConstInstr that defines it,
        # so we can check whether every phi incoming is a same-valued const.
        const_defs: Dict[int, MenaiValue] = {}
        for block in func.blocks:
            for instr in block.instrs:
                if isinstance(instr, MenaiCFGConstInstr):
                    const_defs[instr.result.id] = instr.value

        foldable: Dict[int, MenaiValue] = {}  # phi result id → folded constant
        for block in func.blocks:
            for instr in block.instrs:
                if not isinstance(instr, MenaiCFGPhiInstr):
                    continue
                if not instr.incoming:
                    continue
                values = [const_defs.get(val.id) for val, _ in instr.incoming]
                if any(v is None for v in values):
                    continue
                first = values[0]
                if all(v == first for v in values):
                    foldable[instr.result.id] = first  # type: ignore[arg-type]

        if not foldable:
            return False

        # Phase 2: for each foldable phi, introduce a fresh MenaiCFGConstInstr
        # in the same block (replacing the phi), and build a substitution map
        # from the old phi result id to the new const result.
        subst: Dict[int, MenaiCFGValue] = {}
        for block in func.blocks:
            new_instrs: List[MenaiCFGInstr] = []
            for instr in block.instrs:
                if isinstance(instr, MenaiCFGPhiInstr) and instr.result.id in foldable:
                    const_val = foldable[instr.result.id]
                    new_result = MenaiCFGValue(id=instr.result.id, hint=instr.result.hint)
                    new_instrs.append(MenaiCFGConstInstr(result=new_result, value=const_val))
                    subst[instr.result.id] = new_result
                else:
                    new_instrs.append(instr)
            block.instrs = new_instrs

        # Phase 3: substitute uses of the old phi result ids throughout the
        # function.  Since we reused the same id for the new const result, the
        # subst map is an identity on those ids — but we still need to re-run
        # subst_instr / subst_term to handle any phi incoming entries that
        # reference the now-folded phi results, replacing them with the const.
        def resolve(v: MenaiCFGValue) -> MenaiCFGValue:
            return subst.get(v.id, v)

        for block in func.blocks:
            block.instrs = [subst_instr(i, resolve) for i in block.instrs]
            block.patch_instrs = [subst_patch(p, resolve) for p in block.patch_instrs]
            block.terminator = subst_term(block.terminator, resolve)

        return True

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
                    for vid in value_ids_in_instr(instr):
                        if vid in phi_defs:
                            total_uses[vid] += 1

            for patch in block.patch_instrs:
                for vid in (patch.closure.id, patch.value.id):
                    if vid in phi_defs:
                        total_uses[vid] += 1

            term = block.terminator
            if term is not None:
                for vid in value_ids_in_term(term):
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
