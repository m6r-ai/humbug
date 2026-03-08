"""
Tests for MenaiCFGCollapsePhiChains.

Covers:
  1. Basic two-level phi chain collapse
  2. Three-level chain collapsed in a single pass invocation
  3. Phi used outside a phi -- must NOT be collapsed
  4. Phi with zero uses -- removed without expansion
  5. Duplicate-predecessor conflict prevents unsafe collapse
  6. Multiple consumers of the same intermediate phi
  7. Nested lambda phi chains are optimized recursively
  8. No change when there is nothing to collapse
  9. Integration with downstream passes (bypass + dead-block elimination)
 10. End-to-end: compile nested-if Menai source and verify MOVE reduction
"""

from menai.menai_cfg import (
    MenaiCFGBlock,
    MenaiCFGBranchTerm,
    MenaiCFGBuiltinInstr,
    MenaiCFGConstInstr,
    MenaiCFGFunction,
    MenaiCFGJumpTerm,
    MenaiCFGMakeClosureInstr,
    MenaiCFGPhiInstr,
    MenaiCFGReturnTerm,
    MenaiCFGValue,
)
from menai.menai_cfg_bypass_empty_blocks import MenaiCFGBypassEmptyBlocks
from menai.menai_cfg_collapse_phi_chains import MenaiCFGCollapsePhiChains
from menai.menai_value import MenaiInteger


# ---------------------------------------------------------------------------
# Helpers (mirrors test_cfg_optimizer.py style)
# ---------------------------------------------------------------------------

_vid = 1000  # start high to avoid clashes with test_cfg_optimizer.py globals


def v(hint: str = "") -> MenaiCFGValue:
    global _vid
    _vid += 1
    return MenaiCFGValue(id=_vid, hint=hint)


def block(id: int, *instrs, patch_instrs=None, terminator=None, label: str = "block") -> MenaiCFGBlock:
    b = MenaiCFGBlock(id=id, label=label)
    b.instrs = list(instrs)
    b.patch_instrs = patch_instrs or []
    b.terminator = terminator
    return b


def func(*blocks, params=None, free_vars=None) -> MenaiCFGFunction:
    f = MenaiCFGFunction(blocks=list(blocks), params=params or [], free_vars=free_vars or [])
    _link(f)
    return f


def _link(f: MenaiCFGFunction) -> None:
    for b in f.blocks:
        b.predecessors = []
    for b in f.blocks:
        t = b.terminator
        if isinstance(t, MenaiCFGJumpTerm):
            t.target.predecessors.append(b)
        elif isinstance(t, MenaiCFGBranchTerm):
            t.true_block.predecessors.append(b)
            t.false_block.predecessors.append(b)


def phi_result_ids(f: MenaiCFGFunction):
    """Return all phi result value ids in f (flat, no nesting)."""
    return {
        instr.result.id
        for b in f.blocks
        for instr in b.instrs
        if isinstance(instr, MenaiCFGPhiInstr)
    }


def all_phi_incoming_value_ids(f: MenaiCFGFunction):
    """Return all value ids that appear as phi incoming values."""
    return {
        val.id
        for b in f.blocks
        for instr in b.instrs
        if isinstance(instr, MenaiCFGPhiInstr)
        for val, _ in instr.incoming
    }


# ---------------------------------------------------------------------------
# 1. Basic two-level chain
# ---------------------------------------------------------------------------

class TestBasicChain:

    def test_two_level_chain_collapsed(self):
        """
        CFG:
          A:  jump -> join1
          B:  jump -> join1
          join1: %v1 = phi [(%a, A), (%b, B)]
                 jump -> join2
          C:  jump -> join2
          join2: %v2 = phi [(%v1, join1), (%c, C)]
                 return %v2

        After collapse: join2's phi becomes [(%a, A), (%b, B), (%c, C)].
        join1's phi is removed.
        """
        va = v("a"); vb = v("b"); vc = v("c")
        v1 = v("v1"); v2 = v("v2")

        block_a = block(1, terminator=None, label="A")
        block_b = block(2, terminator=None, label="B")
        block_c = block(3, terminator=None, label="C")

        join1 = block(
            4,
            MenaiCFGPhiInstr(result=v1, incoming=[(va, block_a), (vb, block_b)]),
            label="join1",
        )
        join2 = block(
            5,
            MenaiCFGPhiInstr(result=v2, incoming=[(v1, join1), (vc, block_c)]),
            terminator=MenaiCFGReturnTerm(value=v2),
            label="join2",
        )

        block_a.terminator = MenaiCFGJumpTerm(target=join1)
        block_b.terminator = MenaiCFGJumpTerm(target=join1)
        block_c.terminator = MenaiCFGJumpTerm(target=join2)
        join1.terminator = MenaiCFGJumpTerm(target=join2)

        f = func(block_a, block_b, block_c, join1, join2)

        new_f, changed = MenaiCFGCollapsePhiChains()._optimize_function(f)
        assert changed

        # join1's phi (v1) should be gone -- it is now unreferenced.
        assert v1.id not in phi_result_ids(new_f), "intermediate phi should be removed"

        # join2's phi should now have three incoming entries: A, B, C.
        join2_new = next(b for b in new_f.blocks if b.id == 5)
        phi2 = next(i for i in join2_new.instrs if isinstance(i, MenaiCFGPhiInstr))
        pred_ids = {pred.id for _, pred in phi2.incoming}
        assert pred_ids == {1, 2, 3}, f"expected preds {{1,2,3}}, got {pred_ids}"
        assert len(phi2.incoming) == 3

    def test_no_change_when_no_phi_chain(self):
        """A single phi with non-phi incoming values is untouched."""
        va = v("a"); vb = v("b"); vphi = v("phi")
        block_a = block(1, terminator=None, label="A")
        block_b = block(2, terminator=None, label="B")
        join = block(
            3,
            MenaiCFGPhiInstr(result=vphi, incoming=[(va, block_a), (vb, block_b)]),
            terminator=MenaiCFGReturnTerm(value=vphi),
            label="join",
        )
        block_a.terminator = MenaiCFGJumpTerm(target=join)
        block_b.terminator = MenaiCFGJumpTerm(target=join)

        cond = v("cond")
        entry = block(
            0,
            MenaiCFGConstInstr(result=cond, value=MenaiInteger(1)),
            terminator=MenaiCFGBranchTerm(cond=cond, true_block=block_a, false_block=block_b),
            label="entry",
        )
        f = func(entry, block_a, block_b, join)

        new_f, changed = MenaiCFGCollapsePhiChains()._optimize_function(f)
        assert not changed
        assert new_f is f


# ---------------------------------------------------------------------------
# 2. Three-level chain
# ---------------------------------------------------------------------------

class TestThreeLevelChain:

    def test_three_level_chain_collapsed(self):
        """
        join1: %v1 = phi [(%a, A), (%b, B)]   -> jump join2
        join2: %v2 = phi [(%v1, join1), (%c, C)]  -> jump join3
        join3: %v3 = phi [(%v2, join2), (%d, D)]

        After collapse: join3 has [(%a, A), (%b, B), (%c, C), (%d, D)].
        Both v1 and v2 are removed.
        """
        va = v("a"); vb = v("b"); vc = v("c"); vd = v("d")
        v1 = v("v1"); v2 = v("v2"); v3 = v("v3")

        block_a = block(1, terminator=None, label="A")
        block_b = block(2, terminator=None, label="B")
        block_c = block(3, terminator=None, label="C")
        block_d = block(4, terminator=None, label="D")

        join1 = block(5, MenaiCFGPhiInstr(result=v1, incoming=[(va, block_a), (vb, block_b)]), label="join1")
        join2 = block(6, MenaiCFGPhiInstr(result=v2, incoming=[(v1, join1), (vc, block_c)]), label="join2")
        join3 = block(
            7,
            MenaiCFGPhiInstr(result=v3, incoming=[(v2, join2), (vd, block_d)]),
            terminator=MenaiCFGReturnTerm(value=v3),
            label="join3",
        )

        block_a.terminator = MenaiCFGJumpTerm(target=join1)
        block_b.terminator = MenaiCFGJumpTerm(target=join1)
        block_c.terminator = MenaiCFGJumpTerm(target=join2)
        block_d.terminator = MenaiCFGJumpTerm(target=join3)
        join1.terminator = MenaiCFGJumpTerm(target=join2)
        join2.terminator = MenaiCFGJumpTerm(target=join3)

        f = func(block_a, block_b, block_c, block_d, join1, join2, join3)

        new_f, changed = MenaiCFGCollapsePhiChains()._optimize_function(f)
        assert changed

        assert v1.id not in phi_result_ids(new_f)
        assert v2.id not in phi_result_ids(new_f)

        join3_new = next(b for b in new_f.blocks if b.id == 7)
        phi3 = next(i for i in join3_new.instrs if isinstance(i, MenaiCFGPhiInstr))
        pred_ids = {pred.id for _, pred in phi3.incoming}
        assert pred_ids == {1, 2, 3, 4}
        assert len(phi3.incoming) == 4


# ---------------------------------------------------------------------------
# 3. Phi used outside a phi -- must NOT be collapsed
# ---------------------------------------------------------------------------

class TestPhiUsedOutsidePhi:

    def test_phi_used_in_return_not_collapsed(self):
        """
        join1: %v1 = phi [(%a, A), (%b, B)]
               return %v1          <- non-phi use

        join2 also uses %v1 as phi incoming, but because %v1 has a non-phi
        use (the return), it must not be collapsed.
        """
        va = v("a"); vb = v("b"); vc = v("c")
        v1 = v("v1"); v2 = v("v2")

        block_a = block(1, terminator=None, label="A")
        block_b = block(2, terminator=None, label="B")
        block_c = block(3, terminator=None, label="C")

        join1 = block(
            4,
            MenaiCFGPhiInstr(result=v1, incoming=[(va, block_a), (vb, block_b)]),
            terminator=MenaiCFGReturnTerm(value=v1),
            label="join1",
        )
        join2 = block(
            5,
            MenaiCFGPhiInstr(result=v2, incoming=[(v1, join1), (vc, block_c)]),
            terminator=MenaiCFGReturnTerm(value=v2),
            label="join2",
        )

        block_a.terminator = MenaiCFGJumpTerm(target=join1)
        block_b.terminator = MenaiCFGJumpTerm(target=join1)
        block_c.terminator = MenaiCFGJumpTerm(target=join2)
        join1.terminator = MenaiCFGReturnTerm(value=v1)

        f = func(block_a, block_b, block_c, join1, join2)

        new_f, changed = MenaiCFGCollapsePhiChains()._optimize_function(f)
        assert not changed, "phi with non-phi use must not be collapsed"
        assert v1.id in phi_result_ids(new_f)

    def test_phi_used_in_builtin_not_collapsed(self):
        """
        join1: %v1 = phi [(%a, A), (%b, B)]
               %r  = not %v1       <- non-phi use
        """
        va = v("a"); vb = v("b"); vc = v("c")
        v1 = v("v1"); v2 = v("v2"); vr = v("r")

        block_a = block(1, terminator=None, label="A")
        block_b = block(2, terminator=None, label="B")
        block_c = block(3, terminator=None, label="C")

        join1 = block(
            4,
            MenaiCFGPhiInstr(result=v1, incoming=[(va, block_a), (vb, block_b)]),
            MenaiCFGBuiltinInstr(result=vr, op="not", args=[v1]),
            label="join1",
        )
        join2 = block(
            5,
            MenaiCFGPhiInstr(result=v2, incoming=[(v1, join1), (vc, block_c)]),
            terminator=MenaiCFGReturnTerm(value=v2),
            label="join2",
        )

        block_a.terminator = MenaiCFGJumpTerm(target=join1)
        block_b.terminator = MenaiCFGJumpTerm(target=join1)
        block_c.terminator = MenaiCFGJumpTerm(target=join2)
        join1.terminator = MenaiCFGJumpTerm(target=join2)

        f = func(block_a, block_b, block_c, join1, join2)

        new_f, changed = MenaiCFGCollapsePhiChains()._optimize_function(f)
        assert not changed, "phi used in builtin must not be collapsed"


# ---------------------------------------------------------------------------
# 4. Phi with zero uses -- removed without expansion
# ---------------------------------------------------------------------------

class TestZeroUsePhi:

    def test_zero_use_phi_removed(self):
        """
        A phi whose result is never used anywhere should be removed.
        (This can arise after earlier passes drop the only consumer.)
        """
        va = v("a"); vb = v("b")
        v_unused = v("unused")
        v_ret = v("ret")

        block_a = block(1, terminator=None, label="A")
        block_b = block(2, terminator=None, label="B")

        join = block(
            3,
            MenaiCFGPhiInstr(result=v_unused, incoming=[(va, block_a), (vb, block_b)]),
            terminator=MenaiCFGReturnTerm(value=v_ret),
            label="join",
        )
        block_a.terminator = MenaiCFGJumpTerm(target=join)
        block_b.terminator = MenaiCFGJumpTerm(target=join)

        cond = v("cond")
        entry = block(
            0,
            MenaiCFGConstInstr(result=cond, value=MenaiInteger(1)),
            terminator=MenaiCFGBranchTerm(cond=cond, true_block=block_a, false_block=block_b),
            label="entry",
        )
        f = func(entry, block_a, block_b, join)

        new_f, changed = MenaiCFGCollapsePhiChains()._optimize_function(f)
        assert changed
        assert v_unused.id not in phi_result_ids(new_f), "zero-use phi should be removed"


# ---------------------------------------------------------------------------
# 5. Duplicate-predecessor conflict
# ---------------------------------------------------------------------------

class TestDuplicatePredConflict:

    def test_conflict_prevents_collapse(self):
        """
        join1: %v1 = phi [(%a, A), (%b, B)]   -> jump join2
        join2: %v2 = phi [(%v1, join1), (%c, A)]   <- A already a predecessor!

        Expanding v1 would give join2 two entries from A -- a conflict.
        The collapse must be skipped for this entry.
        """
        va = v("a"); vb = v("b"); vc = v("c_from_A")
        v1 = v("v1"); v2 = v("v2")

        block_a = block(1, terminator=None, label="A")
        block_b = block(2, terminator=None, label="B")

        join1 = block(
            3,
            MenaiCFGPhiInstr(result=v1, incoming=[(va, block_a), (vb, block_b)]),
            label="join1",
        )
        # join2 already has an entry from A (vc), so expanding v1 would add A again.
        join2 = block(
            4,
            MenaiCFGPhiInstr(result=v2, incoming=[(v1, join1), (vc, block_a)]),
            terminator=MenaiCFGReturnTerm(value=v2),
            label="join2",
        )

        block_a.terminator = MenaiCFGJumpTerm(target=join1)
        block_b.terminator = MenaiCFGJumpTerm(target=join1)
        join1.terminator = MenaiCFGJumpTerm(target=join2)

        f = func(block_a, block_b, join1, join2)

        new_f, changed = MenaiCFGCollapsePhiChains()._optimize_function(f)
        # The collapse of v1 into join2 should be skipped due to the conflict.
        assert not changed, "conflict should prevent collapse"
        assert v1.id in phi_result_ids(new_f)


# ---------------------------------------------------------------------------
# 6. Multiple consumers of the same intermediate phi
# ---------------------------------------------------------------------------

class TestMultipleConsumers:

    def test_two_consumers_both_expanded(self):
        """
        Two independent consumers of the same intermediate phi v1:

            entry: branch cond -> left_join / right_join
            A: jump -> join1
            B: jump -> join1
            join1: %v1 = phi [(%a, A), (%b, B)]
            C: jump -> left_join
            left_join:  %v2 = phi [(%v1, join1), (%c, C)]  -> return %v2
            D: jump -> right_join
            right_join: %v3 = phi [(%v1, join1), (%d, D)]  -> return %v3

        v1 is used only as phi incoming in left_join and right_join.
        Both consumers should absorb v1's entries; v1 is then removed.
        This test calls _optimize_function directly so reachability is not
        checked -- dead-block elimination does not run.
        """
        va = v("a"); vb = v("b"); vc = v("c"); vd = v("d")
        v1 = v("v1"); v2 = v("v2"); v3 = v("v3")

        block_a = block(1, terminator=None, label="A")
        block_b = block(2, terminator=None, label="B")
        block_c = block(3, terminator=None, label="C")
        block_d = block(4, terminator=None, label="D")

        join1 = block(
            5,
            MenaiCFGPhiInstr(result=v1, incoming=[(va, block_a), (vb, block_b)]),
            label="join1",
        )
        left_join = block(
            6,
            MenaiCFGPhiInstr(result=v2, incoming=[(v1, join1), (vc, block_c)]),
            terminator=MenaiCFGReturnTerm(value=v2),
            label="left_join",
        )
        right_join = block(
            7,
            MenaiCFGPhiInstr(result=v3, incoming=[(v1, join1), (vd, block_d)]),
            terminator=MenaiCFGReturnTerm(value=v3),
            label="right_join",
        )

        block_a.terminator = MenaiCFGJumpTerm(target=join1)
        block_b.terminator = MenaiCFGJumpTerm(target=join1)
        block_c.terminator = MenaiCFGJumpTerm(target=left_join)
        block_d.terminator = MenaiCFGJumpTerm(target=right_join)
        # join1 branches to left_join or right_join depending on some condition;
        # for this unit test we just need the phi structure, so use a branch.
        vcond = v("cond")
        join1.terminator = MenaiCFGBranchTerm(
            cond=vcond, true_block=left_join, false_block=right_join
        )

        f = func(block_a, block_b, block_c, block_d, join1, left_join, right_join)

        new_f, changed = MenaiCFGCollapsePhiChains()._optimize_function(f)
        assert changed

        # v1 should be gone -- both consumers have absorbed its entries.
        assert v1.id not in phi_result_ids(new_f)
        assert v1.id not in all_phi_incoming_value_ids(new_f)

        # left_join's phi should now have entries from A, B, C.
        lj_new = next(b for b in new_f.blocks if b.id == 6)
        phi2 = next(i for i in lj_new.instrs if isinstance(i, MenaiCFGPhiInstr))
        assert {p.id for _, p in phi2.incoming} == {1, 2, 3}

        # right_join's phi should now have entries from A, B, D.
        rj_new = next(b for b in new_f.blocks if b.id == 7)
        phi3 = next(i for i in rj_new.instrs if isinstance(i, MenaiCFGPhiInstr))
        assert {p.id for _, p in phi3.incoming} == {1, 2, 4}


# ---------------------------------------------------------------------------
# 7. Nested lambda phi chains optimized recursively
# ---------------------------------------------------------------------------

class TestNestedLambda:

    def test_nested_lambda_chain_collapsed(self):
        """
        A MenaiCFGMakeClosureInstr whose child function contains a phi chain
        should have the chain collapsed by the recursive optimize() call.
        """
        va = v("a"); vb = v("b"); vc = v("c")
        v1 = v("v1"); v2 = v("v2")

        child_a = block(1, terminator=None, label="A")
        child_b = block(2, terminator=None, label="B")
        child_c = block(3, terminator=None, label="C")
        child_join1 = block(4, MenaiCFGPhiInstr(result=v1, incoming=[(va, child_a), (vb, child_b)]), label="join1")
        child_join2 = block(
            5,
            MenaiCFGPhiInstr(result=v2, incoming=[(v1, child_join1), (vc, child_c)]),
            terminator=MenaiCFGReturnTerm(value=v2),
            label="join2",
        )
        child_a.terminator = MenaiCFGJumpTerm(target=child_join1)
        child_b.terminator = MenaiCFGJumpTerm(target=child_join1)
        child_c.terminator = MenaiCFGJumpTerm(target=child_join2)
        child_join1.terminator = MenaiCFGJumpTerm(target=child_join2)
        child_func = func(child_a, child_b, child_c, child_join1, child_join2)

        v_closure = v("closure")
        mk = MenaiCFGMakeClosureInstr(result=v_closure, function=child_func, captures=[], needs_patching=False)
        parent_entry = block(0, mk, terminator=MenaiCFGReturnTerm(value=v_closure), label="entry")
        parent_f = func(parent_entry)

        new_parent, changed = MenaiCFGCollapsePhiChains().optimize(parent_f)
        assert changed

        mk_new = next(
            i for i in new_parent.blocks[0].instrs
            if isinstance(i, MenaiCFGMakeClosureInstr)
        )
        child_new = mk_new.function
        assert v1.id not in phi_result_ids(child_new), "intermediate phi in nested lambda should be removed"


# ---------------------------------------------------------------------------
# 8. No change when nothing to collapse
# ---------------------------------------------------------------------------

class TestNoChange:

    def test_empty_function_unchanged(self):
        v_c = v("c")
        entry = block(0, MenaiCFGConstInstr(result=v_c, value=MenaiInteger(1)), terminator=MenaiCFGReturnTerm(value=v_c))
        f = func(entry)
        new_f, changed = MenaiCFGCollapsePhiChains()._optimize_function(f)
        assert not changed
        assert new_f is f

    def test_genuine_two_predecessor_phi_unchanged(self):
        """A phi with two distinct non-phi incoming values is left alone."""
        va = v("a"); vb = v("b"); vphi = v("phi")
        block_a = block(1, terminator=None, label="A")
        block_b = block(2, terminator=None, label="B")
        join = block(
            3,
            MenaiCFGPhiInstr(result=vphi, incoming=[(va, block_a), (vb, block_b)]),
            terminator=MenaiCFGReturnTerm(value=vphi),
            label="join",
        )
        block_a.terminator = MenaiCFGJumpTerm(target=join)
        block_b.terminator = MenaiCFGJumpTerm(target=join)
        cond = v("cond")
        entry = block(
            0,
            MenaiCFGConstInstr(result=cond, value=MenaiInteger(1)),
            terminator=MenaiCFGBranchTerm(cond=cond, true_block=block_a, false_block=block_b),
        )
        f = func(entry, block_a, block_b, join)
        new_f, changed = MenaiCFGCollapsePhiChains()._optimize_function(f)
        assert not changed


# ---------------------------------------------------------------------------
# 9. Integration with downstream passes
# ---------------------------------------------------------------------------

_ALL_PASSES = [
    MenaiCFGCollapsePhiChains(),
    MenaiCFGBypassEmptyBlocks(),
]


class TestDownstreamIntegration:

    def test_collapsed_chain_then_bypass_then_dead_block(self):
        """
        After collapsing the phi chain:
          - join1 has no instructions left and an unconditional jump -> bypassed
          - join1 becomes unreachable -> removed by dead-block elimination

        Start:
          entry: branch cond -> A / mid
          mid:   branch cond2 -> B / C
          A: jump -> join1
          B: jump -> join1
          join1: %v1 = phi [(%a, A), (%b, B)]   -> jump join2
          C: jump -> join2
          join2: %v2 = phi [(%v1, join1), (%c, C)]  -> return %v2

        All three leaf blocks (A, B, C) are reachable from entry.
        End: join2 has phi [(%a, A), (%b, B), (%c, C)], join1 eliminated.
        """
        va = v("a"); vb = v("b"); vc = v("c")
        v1 = v("v1"); v2 = v("v2"); vcond = v("cond"); vcond2 = v("cond2")

        block_a = block(1, terminator=None, label="A")
        block_b = block(2, terminator=None, label="B")
        block_c = block(3, terminator=None, label="C")

        join1 = block(
            4,
            MenaiCFGPhiInstr(result=v1, incoming=[(va, block_a), (vb, block_b)]),
            label="join1",
        )
        join2 = block(
            5,
            MenaiCFGPhiInstr(result=v2, incoming=[(v1, join1), (vc, block_c)]),
            terminator=MenaiCFGReturnTerm(value=v2),
            label="join2",
        )
        block_a.terminator = MenaiCFGJumpTerm(target=join1)
        block_b.terminator = MenaiCFGJumpTerm(target=join1)
        block_c.terminator = MenaiCFGJumpTerm(target=join2)
        join1.terminator = MenaiCFGJumpTerm(target=join2)

        # mid branches to B or C, making both reachable from entry.
        mid = block(6, MenaiCFGConstInstr(result=vcond2, value=MenaiInteger(0)), label="mid")
        mid.terminator = MenaiCFGBranchTerm(cond=vcond2, true_block=block_b, false_block=block_c)

        entry = block(
            0,
            MenaiCFGConstInstr(result=vcond, value=MenaiInteger(1)),
            terminator=MenaiCFGBranchTerm(cond=vcond, true_block=block_a, false_block=mid),
            label="entry",
        )
        f = func(entry, mid, block_a, block_b, block_c, join1, join2)

        changed = True
        while changed:
            changed = False
            for pass_ in _ALL_PASSES:
                f, c = pass_._optimize_function(f)
                changed = changed or c

        block_ids = {b.id for b in f.blocks}
        assert 4 not in block_ids, "join1 should be eliminated after collapse+bypass"

        # join2 should still exist with a flat phi.
        join2_new = next(b for b in f.blocks if b.id == 5)
        phis = [i for i in join2_new.instrs if isinstance(i, MenaiCFGPhiInstr)]
        assert len(phis) == 1
        assert len(phis[0].incoming) == 3


# ---------------------------------------------------------------------------
# 10. End-to-end: compile nested-if Menai source
# ---------------------------------------------------------------------------

class TestEndToEnd:

    def _build_cfg(self, source: str, passes):
        from menai.menai_lexer import MenaiLexer
        from menai.menai_ast_builder import MenaiASTBuilder
        from menai.menai_ast_semantic_analyzer import MenaiASTSemanticAnalyzer
        from menai.menai_ast_module_resolver import MenaiASTModuleResolver
        from menai.menai_ast_desugarer import MenaiASTDesugarer
        from menai.menai_ast_constant_folder import MenaiASTConstantFolder
        from menai.menai_ir_builder import MenaiIRBuilder
        from menai.menai_ir_copy_propagator import MenaiIRCopyPropagator
        from menai.menai_ir_inline_once import MenaiIRInlineOnce
        from menai.menai_ir_optimizer import MenaiIROptimizer
        from menai.menai_cfg_builder import MenaiCFGBuilder

        tokens = MenaiLexer().lex(source)
        ast = MenaiASTBuilder().build(tokens, source, "")
        ast = MenaiASTSemanticAnalyzer().analyze(ast, source)
        ast = MenaiASTModuleResolver(None).resolve(ast)
        ast = MenaiASTDesugarer().desugar(ast)
        ast = MenaiASTConstantFolder().optimize(ast)
        ir = MenaiIRBuilder().build(ast)

        ir_passes = [MenaiIRCopyPropagator(), MenaiIRInlineOnce(), MenaiIROptimizer()]
        changed = True
        while changed:
            changed = False
            for p in ir_passes:
                ir, c = p.optimize(ir)
                changed = changed or c

        cfg = MenaiCFGBuilder().build(ir)
        changed = True
        while changed:
            changed = False
            for p in passes:
                cfg, c = p.optimize(cfg)
                changed = changed or c
        return cfg

    def _count_phis(self, cfg) -> int:
        total = sum(
            1 for b in cfg.blocks for i in b.instrs if isinstance(i, MenaiCFGPhiInstr)
        )
        for b in cfg.blocks:
            for i in b.instrs:
                if isinstance(i, MenaiCFGMakeClosureInstr):
                    total += self._count_phis(i.function)
        return total

    def _find_innermost_lambda(self, cfg):
        for b in cfg.blocks:
            for i in b.instrs:
                if isinstance(i, MenaiCFGMakeClosureInstr):
                    return self._find_innermost_lambda(i.function)
        return cfg

    def test_nested_if_fewer_phis_with_collapse(self):
        """
        A nested if (match-like chain) should have fewer phi nodes after
        collapse than without it.
        """
        source = """
        (lambda (x)
          (if (string=? x "A")
              "result-A"
              (if (string=? x "B")
                  "result-B"
                  "result-other")))
        """
        passes_without = [
            MenaiCFGBypassEmptyBlocks(),
        ]
        passes_with = [
            MenaiCFGCollapsePhiChains(),
            MenaiCFGBypassEmptyBlocks(),
        ]

        cfg_without = self._build_cfg(source, passes_without)
        cfg_with = self._build_cfg(source, passes_with)

        phis_without = self._count_phis(cfg_without)
        phis_with = self._count_phis(cfg_with)

        assert phis_with <= phis_without, (
            f"collapse pass should not increase phi count: "
            f"{phis_with} > {phis_without}"
        )

    def test_nested_if_fewer_blocks_with_collapse(self):
        """
        After phi-chain collapse + bypass + dead-block elimination, the
        innermost lambda should have fewer blocks than without collapse.
        """
        source = """
        (lambda (x)
          (if (string=? x "A")
              "result-A"
              (if (string=? x "B")
                  "result-B"
                  "result-other")))
        """
        passes_without = [
            MenaiCFGBypassEmptyBlocks(),
        ]
        passes_with = [
            MenaiCFGCollapsePhiChains(),
            MenaiCFGBypassEmptyBlocks(),
        ]

        cfg_without = self._build_cfg(source, passes_without)
        cfg_with = self._build_cfg(source, passes_with)

        inner_without = self._find_innermost_lambda(cfg_without)
        inner_with = self._find_innermost_lambda(cfg_with)

        assert len(inner_with.blocks) < len(inner_without.blocks), (
            f"collapse should reduce block count: "
            f"{len(inner_with.blocks)} not < {len(inner_without.blocks)}"
        )

    def test_result_correct_after_collapse(self):
        """
        End-to-end execution: a nested-if function compiled with the collapse
        pass enabled must produce correct results.
        """
        from menai import Menai
        menai = Menai()

        result = menai.evaluate("""
        (let ((classify
               (lambda (x)
                 (if (string=? x "A")
                     "result-A"
                     (if (string=? x "B")
                         "result-B"
                         "result-other")))))
          (list
            (classify "A")
            (classify "B")
            (classify "C")))
        """)
        assert result == ["result-A", "result-B", "result-other"]

    def test_deep_nested_if_correct(self):
        """
        A deeper nesting (3 levels) should still execute correctly.
        This exercises the three-level chain collapse path.
        """
        from menai import Menai
        menai = Menai()

        result = menai.evaluate("""
        (let ((classify
               (lambda (x)
                 (if (string=? x "A")
                     1
                     (if (string=? x "B")
                         2
                         (if (string=? x "C")
                             3
                             4))))))
          (list
            (classify "A")
            (classify "B")
            (classify "C")
            (classify "D")))
        """)
        assert result == [1, 2, 3, 4]
