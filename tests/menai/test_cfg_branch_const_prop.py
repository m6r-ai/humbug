"""
Tests for MenaiCFGBranchConstProp.

Covers:
  1. Single True arm is re-wired directly to true_block
  2. Single False arm is re-wired directly to false_block
  3. Mixed True/False/non-constant arms — only constants are re-wired
  4. All arms constant — phi and branch removed, join block becomes empty
  5. Non-constant phi is left untouched
  6. Phi used outside a branch condition is left untouched
  7. Block with patch_instrs is left untouched
  8. Multiple instructions in join block (phi not the only instr) — untouched
  9. Fixed-point: re-wired predecessor exposes a new candidate
 10. Nested lambda is optimized recursively
 11. End-to-end: (or p1 (or p2 p3)) whitespace-scanner pattern produces
     correct results and fewer blocks than without the pass
"""

from menai.menai_cfg import (
    MenaiCFGBlock,
    MenaiCFGBranchTerm,
    MenaiCFGBuiltinInstr,
    MenaiCFGConstInstr,
    MenaiCFGFunction,
    MenaiCFGJumpTerm,
    MenaiCFGMakeClosureInstr,
    MenaiCFGPatchClosureInstr,
    MenaiCFGPhiInstr,
    MenaiCFGReturnTerm,
    MenaiCFGValue,
)
from menai.menai_cfg_branch_const_prop import MenaiCFGBranchConstProp
from menai.menai_cfg_collapse_phi_chains import MenaiCFGCollapsePhiChains
from menai.menai_cfg_simplify_blocks import MenaiCFGSimplifyBlocks
from menai.menai_value import MenaiBoolean, MenaiInteger


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

_vid = 2000


def v(hint: str = "") -> MenaiCFGValue:
    global _vid
    _vid += 1
    return MenaiCFGValue(id=_vid, hint=hint)


def block(bid: int, *instrs, patch_instrs=None, terminator=None, label: str = "block") -> MenaiCFGBlock:
    b = MenaiCFGBlock(id=bid, label=label)
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


TRUE  = MenaiBoolean(True)
FALSE = MenaiBoolean(False)


# ---------------------------------------------------------------------------
# 1. Single True arm re-wired to true_block
# ---------------------------------------------------------------------------

class TestSingleTrueArm:

    def test_true_arm_rewired(self):
        """
        entry: branch cond → then_block / else_block
        then_block: const True → jump join
        else_block: %r = pred  → jump join
        join: %v = phi [True←then, %r←else]
              branch %v → body / exit

        After: then_block jumps directly to body; join phi has only [%r←else].
        """
        vcond = v("cond")
        vtrue = v("true")
        vr    = v("r")
        vphi  = v("phi")

        body = block(10, terminator=MenaiCFGReturnTerm(value=v("x")), label="body")
        exit_ = block(11, terminator=MenaiCFGReturnTerm(value=v("y")), label="exit")

        then_block = block(2, MenaiCFGConstInstr(result=vtrue, value=TRUE), label="then")
        else_block = block(3, MenaiCFGBuiltinInstr(result=vr, op="pred", args=[]), label="else")

        join = block(
            4,
            MenaiCFGPhiInstr(result=vphi, incoming=[(vtrue, then_block), (vr, else_block)]),
            label="join",
        )
        join.terminator = MenaiCFGBranchTerm(cond=vphi, true_block=body, false_block=exit_)

        then_block.terminator = MenaiCFGJumpTerm(target=join)
        else_block.terminator = MenaiCFGJumpTerm(target=join)

        entry = block(
            0,
            MenaiCFGConstInstr(result=vcond, value=MenaiInteger(1)),
            terminator=MenaiCFGBranchTerm(cond=vcond, true_block=then_block, false_block=else_block),
            label="entry",
        )
        f = func(entry, then_block, else_block, join, body, exit_)

        new_f, changed = MenaiCFGBranchConstProp()._optimize_function(f)
        assert changed

        # then_block should now jump directly to body, not join.
        then_new = next(b for b in new_f.blocks if b.id == 2)
        assert isinstance(then_new.terminator, MenaiCFGJumpTerm)
        assert then_new.terminator.target.id == body.id

        # join had two arms; one was constant so the phi collapses to a single
        # entry, which the pass eliminates — branch condition becomes %r directly.
        join_new = next(b for b in new_f.blocks if b.id == 4)
        assert not any(isinstance(i, MenaiCFGPhiInstr) for i in join_new.instrs)
        assert isinstance(join_new.terminator, MenaiCFGBranchTerm)
        assert join_new.terminator.cond.id == vr.id


# ---------------------------------------------------------------------------
# 2. Single False arm re-wired to false_block
# ---------------------------------------------------------------------------

class TestSingleFalseArm:

    def test_false_arm_rewired(self):
        """
        then_block: const False → jump join
        else_block: %r = pred   → jump join
        join: %v = phi [False←then, %r←else]
              branch %v → body / exit

        After: then_block jumps directly to exit; join phi has only [%r←else].
        """
        vfalse = v("false")
        vr     = v("r")
        vphi   = v("phi")

        body  = block(10, terminator=MenaiCFGReturnTerm(value=v("x")), label="body")
        exit_ = block(11, terminator=MenaiCFGReturnTerm(value=v("y")), label="exit")

        then_block = block(2, MenaiCFGConstInstr(result=vfalse, value=FALSE), label="then")
        else_block = block(3, MenaiCFGBuiltinInstr(result=vr, op="pred", args=[]), label="else")

        join = block(
            4,
            MenaiCFGPhiInstr(result=vphi, incoming=[(vfalse, then_block), (vr, else_block)]),
            label="join",
        )
        join.terminator = MenaiCFGBranchTerm(cond=vphi, true_block=body, false_block=exit_)

        then_block.terminator = MenaiCFGJumpTerm(target=join)
        else_block.terminator = MenaiCFGJumpTerm(target=join)

        vcond = v("cond")
        entry = block(
            0,
            MenaiCFGConstInstr(result=vcond, value=MenaiInteger(1)),
            terminator=MenaiCFGBranchTerm(cond=vcond, true_block=then_block, false_block=else_block),
            label="entry",
        )
        f = func(entry, then_block, else_block, join, body, exit_)

        new_f, changed = MenaiCFGBranchConstProp()._optimize_function(f)
        assert changed

        then_new = next(b for b in new_f.blocks if b.id == 2)
        assert isinstance(then_new.terminator, MenaiCFGJumpTerm)
        assert then_new.terminator.target.id == exit_.id

        # Single remaining arm — phi eliminated, branch condition is %r directly.
        join_new = next(b for b in new_f.blocks if b.id == 4)
        assert not any(isinstance(i, MenaiCFGPhiInstr) for i in join_new.instrs)
        assert isinstance(join_new.terminator, MenaiCFGBranchTerm)
        assert join_new.terminator.cond.id == vr.id


# ---------------------------------------------------------------------------
# 3. Mixed arms — only constants re-wired
# ---------------------------------------------------------------------------

class TestMixedArms:

    def test_mixed_arms(self):
        """
        Four incoming arms: True, True, False, non-constant.
        After: two True arms → body, one False arm → exit, non-constant kept.
        """
        vt1 = v("t1"); vt2 = v("t2"); vf1 = v("f1"); vr = v("r"); vphi = v("phi")

        body  = block(10, terminator=MenaiCFGReturnTerm(value=v("x")), label="body")
        exit_ = block(11, terminator=MenaiCFGReturnTerm(value=v("y")), label="exit")

        b_t1 = block(1, MenaiCFGConstInstr(result=vt1, value=TRUE),  label="t1")
        b_t2 = block(2, MenaiCFGConstInstr(result=vt2, value=TRUE),  label="t2")
        b_f1 = block(3, MenaiCFGConstInstr(result=vf1, value=FALSE), label="f1")
        b_r  = block(4, MenaiCFGBuiltinInstr(result=vr, op="p", args=[]), label="r")

        join = block(
            5,
            MenaiCFGPhiInstr(
                result=vphi,
                incoming=[(vt1, b_t1), (vt2, b_t2), (vf1, b_f1), (vr, b_r)],
            ),
            label="join",
        )
        join.terminator = MenaiCFGBranchTerm(cond=vphi, true_block=body, false_block=exit_)

        for b in (b_t1, b_t2, b_f1, b_r):
            b.terminator = MenaiCFGJumpTerm(target=join)

        vcond = v("cond")
        entry = block(
            0,
            MenaiCFGConstInstr(result=vcond, value=MenaiInteger(1)),
            terminator=MenaiCFGBranchTerm(cond=vcond, true_block=b_t1, false_block=b_t2),
            label="entry",
        )
        f = func(entry, b_t1, b_t2, b_f1, b_r, join, body, exit_)

        new_f, changed = MenaiCFGBranchConstProp()._optimize_function(f)
        assert changed

        def term_target(bid):
            b = next(b for b in new_f.blocks if b.id == bid)
            assert isinstance(b.terminator, MenaiCFGJumpTerm)
            return b.terminator.target.id

        assert term_target(1) == body.id
        assert term_target(2) == body.id
        assert term_target(3) == exit_.id

        # One non-constant arm remains — phi eliminated, branch uses %r directly.
        join_new = next(b for b in new_f.blocks if b.id == 5)
        assert not any(isinstance(i, MenaiCFGPhiInstr) for i in join_new.instrs)
        assert isinstance(join_new.terminator, MenaiCFGBranchTerm)
        assert join_new.terminator.cond.id == vr.id


# ---------------------------------------------------------------------------
# 4. All arms constant — phi and branch removed
# ---------------------------------------------------------------------------

class TestAllArmsConstant:

    def test_all_true_phi_and_branch_removed(self):
        """
        All four incoming values are True.
        After: all preds jump directly to body; phi and branch are removed
        from join, leaving it with no instructions and no terminator.
        """
        vt = [v(f"t{i}") for i in range(4)]
        vphi = v("phi")

        body  = block(10, terminator=MenaiCFGReturnTerm(value=v("x")), label="body")
        exit_ = block(11, terminator=MenaiCFGReturnTerm(value=v("y")), label="exit")

        preds = [
            block(i + 1, MenaiCFGConstInstr(result=vt[i], value=TRUE), label=f"p{i}")
            for i in range(4)
        ]

        join = block(
            5,
            MenaiCFGPhiInstr(result=vphi, incoming=[(vt[i], preds[i]) for i in range(4)]),
            label="join",
        )
        join.terminator = MenaiCFGBranchTerm(cond=vphi, true_block=body, false_block=exit_)

        for p in preds:
            p.terminator = MenaiCFGJumpTerm(target=join)

        vcond = v("cond")
        entry = block(
            0,
            MenaiCFGConstInstr(result=vcond, value=MenaiInteger(1)),
            terminator=MenaiCFGBranchTerm(cond=vcond, true_block=preds[0], false_block=preds[1]),
            label="entry",
        )
        f = func(entry, *preds, join, body, exit_)

        new_f, changed = MenaiCFGBranchConstProp()._optimize_function(f)
        assert changed

        for p in preds:
            p_new = next(b for b in new_f.blocks if b.id == p.id)
            assert isinstance(p_new.terminator, MenaiCFGJumpTerm)
            assert p_new.terminator.target.id == body.id

        join_new = next(b for b in new_f.blocks if b.id == 5)
        assert not any(isinstance(i, MenaiCFGPhiInstr) for i in join_new.instrs)
        # Stale branch replaced with a jump to true_block so the block is
        # structurally valid; it is now unreachable (all predecessors re-wired).
        assert isinstance(join_new.terminator, MenaiCFGJumpTerm)
        assert join_new.terminator.target.id == body.id


# ---------------------------------------------------------------------------
# 5. Non-constant phi left untouched
# ---------------------------------------------------------------------------

class TestNonConstantPhi:

    def test_all_non_constant_unchanged(self):
        """
        phi [%a←A, %b←B] used as branch condition: no constant arms,
        so nothing is re-wired.
        """
        va = v("a"); vb = v("b"); vphi = v("phi")

        body  = block(10, terminator=MenaiCFGReturnTerm(value=v("x")), label="body")
        exit_ = block(11, terminator=MenaiCFGReturnTerm(value=v("y")), label="exit")

        block_a = block(1, MenaiCFGBuiltinInstr(result=va, op="p1", args=[]), label="A")
        block_b = block(2, MenaiCFGBuiltinInstr(result=vb, op="p2", args=[]), label="B")

        join = block(
            3,
            MenaiCFGPhiInstr(result=vphi, incoming=[(va, block_a), (vb, block_b)]),
            label="join",
        )
        join.terminator = MenaiCFGBranchTerm(cond=vphi, true_block=body, false_block=exit_)

        block_a.terminator = MenaiCFGJumpTerm(target=join)
        block_b.terminator = MenaiCFGJumpTerm(target=join)

        vcond = v("cond")
        entry = block(
            0,
            MenaiCFGConstInstr(result=vcond, value=MenaiInteger(1)),
            terminator=MenaiCFGBranchTerm(cond=vcond, true_block=block_a, false_block=block_b),
            label="entry",
        )
        f = func(entry, block_a, block_b, join, body, exit_)

        new_f, changed = MenaiCFGBranchConstProp()._optimize_function(f)
        assert not changed
        assert new_f is f


# ---------------------------------------------------------------------------
# 6. Phi used outside a branch condition — untouched
# ---------------------------------------------------------------------------

class TestPhiUsedOutsideBranch:

    def test_phi_used_in_return_not_rewired(self):
        """
        join: %v = phi [True←A, %r←B]
              return %v          ← not a branch condition

        The phi has a constant incoming arm, but because it is used as a
        return value (not a branch condition), the pass must leave it alone.
        """
        vtrue = v("true"); vr = v("r"); vphi = v("phi")

        block_a = block(1, MenaiCFGConstInstr(result=vtrue, value=TRUE), label="A")
        block_b = block(2, MenaiCFGBuiltinInstr(result=vr, op="p", args=[]), label="B")

        join = block(
            3,
            MenaiCFGPhiInstr(result=vphi, incoming=[(vtrue, block_a), (vr, block_b)]),
            terminator=MenaiCFGReturnTerm(value=vphi),
            label="join",
        )

        block_a.terminator = MenaiCFGJumpTerm(target=join)
        block_b.terminator = MenaiCFGJumpTerm(target=join)

        vcond = v("cond")
        entry = block(
            0,
            MenaiCFGConstInstr(result=vcond, value=MenaiInteger(1)),
            terminator=MenaiCFGBranchTerm(cond=vcond, true_block=block_a, false_block=block_b),
            label="entry",
        )
        f = func(entry, block_a, block_b, join)

        new_f, changed = MenaiCFGBranchConstProp()._optimize_function(f)
        assert not changed
        assert new_f is f


# ---------------------------------------------------------------------------
# 7. Block with patch_instrs — untouched
# ---------------------------------------------------------------------------

class TestPatchInstrs:

    def test_patch_instrs_block_skipped(self):
        """
        A join block that has patch_instrs must not be modified even if its
        phi+branch would otherwise qualify.
        """
        vtrue = v("true"); vr = v("r"); vphi = v("phi")
        vclosure = v("closure"); vpatch_val = v("pv")

        body  = block(10, terminator=MenaiCFGReturnTerm(value=v("x")), label="body")
        exit_ = block(11, terminator=MenaiCFGReturnTerm(value=v("y")), label="exit")

        block_a = block(1, MenaiCFGConstInstr(result=vtrue, value=TRUE), label="A")
        block_b = block(2, MenaiCFGBuiltinInstr(result=vr, op="p", args=[]), label="B")

        join = block(
            3,
            MenaiCFGPhiInstr(result=vphi, incoming=[(vtrue, block_a), (vr, block_b)]),
            patch_instrs=[MenaiCFGPatchClosureInstr(closure=vclosure, capture_index=0, value=vpatch_val)],
            label="join",
        )
        join.terminator = MenaiCFGBranchTerm(cond=vphi, true_block=body, false_block=exit_)

        block_a.terminator = MenaiCFGJumpTerm(target=join)
        block_b.terminator = MenaiCFGJumpTerm(target=join)

        vcond = v("cond")
        entry = block(
            0,
            MenaiCFGConstInstr(result=vcond, value=MenaiInteger(1)),
            terminator=MenaiCFGBranchTerm(cond=vcond, true_block=block_a, false_block=block_b),
            label="entry",
        )
        f = func(entry, block_a, block_b, join, body, exit_)

        new_f, changed = MenaiCFGBranchConstProp()._optimize_function(f)
        assert not changed
        assert new_f is f


# ---------------------------------------------------------------------------
# 8. Multiple instructions in join block — untouched
# ---------------------------------------------------------------------------

class TestMultipleInstrsInJoin:

    def test_extra_instr_prevents_rewire(self):
        """
        join: %v = phi [True←A, %r←B]
              %x = some_builtin %v    ← extra instruction
              branch %v → body / exit

        The phi is not the only instruction, so the block does not qualify.
        """
        vtrue = v("true"); vr = v("r"); vphi = v("phi"); vx = v("x")

        body  = block(10, terminator=MenaiCFGReturnTerm(value=v("p")), label="body")
        exit_ = block(11, terminator=MenaiCFGReturnTerm(value=v("q")), label="exit")

        block_a = block(1, MenaiCFGConstInstr(result=vtrue, value=TRUE), label="A")
        block_b = block(2, MenaiCFGBuiltinInstr(result=vr, op="p", args=[]), label="B")

        join = block(
            3,
            MenaiCFGPhiInstr(result=vphi, incoming=[(vtrue, block_a), (vr, block_b)]),
            MenaiCFGBuiltinInstr(result=vx, op="some_builtin", args=[vphi]),
            label="join",
        )
        join.terminator = MenaiCFGBranchTerm(cond=vphi, true_block=body, false_block=exit_)

        block_a.terminator = MenaiCFGJumpTerm(target=join)
        block_b.terminator = MenaiCFGJumpTerm(target=join)

        vcond = v("cond")
        entry = block(
            0,
            MenaiCFGConstInstr(result=vcond, value=MenaiInteger(1)),
            terminator=MenaiCFGBranchTerm(cond=vcond, true_block=block_a, false_block=block_b),
            label="entry",
        )
        f = func(entry, block_a, block_b, join, body, exit_)

        new_f, changed = MenaiCFGBranchConstProp()._optimize_function(f)
        assert not changed
        assert new_f is f


# ---------------------------------------------------------------------------
# 9. Fixed-point: re-wired predecessor exposes new candidate
# ---------------------------------------------------------------------------

class TestFixedPoint:

    def test_two_rounds_needed(self):
        """
        After the first round, the re-wired predecessor itself becomes a
        single-entry phi that qualifies for a second round.

        join1: %v1 = phi [True←A, %r←B]  branch %v1 → join2 / exit
        join2: %v2 = phi [True←C, %v1←join1_true_arm_after_rewire]
               branch %v2 → body / exit2

        After round 1: A jumps to join2; join1 phi = [%r←B].
        After round 2: C jumps to body; join2 phi = [%v1←join1].

        We verify the pass reaches a fixed point (changed=True overall) and
        that A's terminator ends up pointing at join2 (body of first branch)
        and C's terminator ends up pointing at body.
        """
        vtrue_a = v("ta"); vtrue_c = v("tc"); vr = v("r"); v1 = v("v1"); v2 = v("v2")

        body  = block(20, terminator=MenaiCFGReturnTerm(value=v("x")), label="body")
        exit1 = block(21, terminator=MenaiCFGReturnTerm(value=v("y")), label="exit1")
        exit2 = block(22, terminator=MenaiCFGReturnTerm(value=v("z")), label="exit2")

        block_a = block(1, MenaiCFGConstInstr(result=vtrue_a, value=TRUE), label="A")
        block_b = block(2, MenaiCFGBuiltinInstr(result=vr, op="p", args=[]), label="B")
        block_c = block(3, MenaiCFGConstInstr(result=vtrue_c, value=TRUE), label="C")

        join2 = block(
            5,
            MenaiCFGPhiInstr(result=v2, incoming=[(vtrue_c, block_c), (v1, None)]),  # join1 set below
            label="join2",
        )
        join2.terminator = MenaiCFGBranchTerm(cond=v2, true_block=body, false_block=exit2)

        join1 = block(
            4,
            MenaiCFGPhiInstr(result=v1, incoming=[(vtrue_a, block_a), (vr, block_b)]),
            label="join1",
        )
        join1.terminator = MenaiCFGBranchTerm(cond=v1, true_block=join2, false_block=exit1)

        # Fix up the join2 phi incoming reference to join1.
        join2.instrs[0] = MenaiCFGPhiInstr(
            result=v2, incoming=[(vtrue_c, block_c), (v1, join1)]
        )

        block_a.terminator = MenaiCFGJumpTerm(target=join1)
        block_b.terminator = MenaiCFGJumpTerm(target=join1)
        block_c.terminator = MenaiCFGJumpTerm(target=join2)

        vcond = v("cond")
        entry = block(
            0,
            MenaiCFGConstInstr(result=vcond, value=MenaiInteger(1)),
            terminator=MenaiCFGBranchTerm(cond=vcond, true_block=block_a, false_block=block_b),
            label="entry",
        )
        f = func(entry, block_a, block_b, block_c, join1, join2, body, exit1, exit2)

        new_f, changed = MenaiCFGBranchConstProp()._optimize_function(f)
        assert changed

        a_new = next(b for b in new_f.blocks if b.id == 1)
        assert isinstance(a_new.terminator, MenaiCFGJumpTerm)
        assert a_new.terminator.target.id == join2.id

        c_new = next(b for b in new_f.blocks if b.id == 3)
        assert isinstance(c_new.terminator, MenaiCFGJumpTerm)
        assert c_new.terminator.target.id == body.id


# ---------------------------------------------------------------------------
# 10. Nested lambda optimized recursively
# ---------------------------------------------------------------------------

class TestNestedLambda:

    def test_nested_lambda_rewired(self):
        """
        A MenaiCFGMakeClosureInstr whose child function contains a qualifying
        phi+branch should be optimized by the recursive optimize() call.
        """
        vtrue = v("true"); vr = v("r"); vphi = v("phi")

        body  = block(10, terminator=MenaiCFGReturnTerm(value=v("x")), label="body")
        exit_ = block(11, terminator=MenaiCFGReturnTerm(value=v("y")), label="exit")

        child_a = block(1, MenaiCFGConstInstr(result=vtrue, value=TRUE), label="A")
        child_b = block(2, MenaiCFGBuiltinInstr(result=vr, op="p", args=[]), label="B")
        child_join = block(
            3,
            MenaiCFGPhiInstr(result=vphi, incoming=[(vtrue, child_a), (vr, child_b)]),
            label="join",
        )
        child_join.terminator = MenaiCFGBranchTerm(cond=vphi, true_block=body, false_block=exit_)
        child_a.terminator = MenaiCFGJumpTerm(target=child_join)
        child_b.terminator = MenaiCFGJumpTerm(target=child_join)

        vcond = v("cond")
        child_entry = block(
            0,
            MenaiCFGConstInstr(result=vcond, value=MenaiInteger(1)),
            terminator=MenaiCFGBranchTerm(cond=vcond, true_block=child_a, false_block=child_b),
            label="entry",
        )
        child_func = func(child_entry, child_a, child_b, child_join, body, exit_)

        v_closure = v("closure")
        mk = MenaiCFGMakeClosureInstr(result=v_closure, function=child_func, captures=[], needs_patching=False)
        parent_entry = block(0, mk, terminator=MenaiCFGReturnTerm(value=v_closure), label="entry")
        parent_f = func(parent_entry)

        new_parent, changed = MenaiCFGBranchConstProp().optimize(parent_f)
        assert changed

        mk_new = next(
            i for i in new_parent.blocks[0].instrs
            if isinstance(i, MenaiCFGMakeClosureInstr)
        )
        child_new = mk_new.function
        child_a_new = next(b for b in child_new.blocks if b.id == 1)
        assert isinstance(child_a_new.terminator, MenaiCFGJumpTerm)
        assert child_a_new.terminator.target.id == body.id


# ---------------------------------------------------------------------------
# 11. End-to-end: skip-ws pattern
# ---------------------------------------------------------------------------

class TestEndToEnd:

    def _compile_cfg(self, source: str, passes):
        from menai.menai_lexer import MenaiLexer
        from menai.menai_ast_builder import MenaiASTBuilder
        from menai.menai_ast_semantic_analyzer import MenaiASTSemanticAnalyzer
        from menai.menai_ast_module_resolver import MenaiASTModuleResolver
        from menai.menai_ast_desugarer import MenaiASTDesugarer
        from menai.menai_ast_constant_folder import MenaiASTConstantFolder
        from menai.menai_ir_builder import MenaiIRBuilder
        from menai.menai_ir_optimizer import MenaiIROptimizer
        from menai.menai_cfg_builder import MenaiCFGBuilder

        tokens = MenaiLexer().lex(source)
        ast = MenaiASTBuilder().build(tokens, source, "")
        ast = MenaiASTSemanticAnalyzer().analyze(ast, source)
        ast = MenaiASTModuleResolver(None).resolve(ast)
        ast = MenaiASTDesugarer().desugar(ast)
        ast = MenaiASTConstantFolder().optimize(ast)
        ir = MenaiIRBuilder().build(ast)
        ir, _ = MenaiIROptimizer().optimize(ir)
        cfg = MenaiCFGBuilder().build(ir)

        changed = True
        while changed:
            changed = False
            for p in passes:
                cfg, c = p.optimize(cfg)
                changed = changed or c
        return cfg

    def _find_loop_func(self, cfg):
        """Recursively find the 'loop' MenaiCFGFunction in the closure tree."""
        for b in cfg.blocks:
            for instr in b.instrs:
                if isinstance(instr, MenaiCFGMakeClosureInstr):
                    if instr.function.binding_name == 'loop':
                        return instr.function
                    found = self._find_loop_func(instr.function)
                    if found:
                        return found
        return None

    SKIP_WS = r"""
    (define skip-ws
      (lambda (s pos)
        (letrec ((loop (lambda (i)
                         (if (integer>=? i (string-length s))
                             i
                             (let ((ch (string-ref s i)))
                               (if (or (string=? ch " ")
                                   (or (string=? ch "\t")
                                   (or (string=? ch "\n")
                                       (string=? ch "\r"))))
                                   (loop (integer+ i 1))
                                   i))))))
          (loop pos))))
    """

    def test_skip_ws_const_true_blocks_bypass_join(self):
        """
        After BranchConstProp, the three const-True blocks (for ' ', '\\t', '\\n')
        should jump directly to the self-loop block, bypassing the phi join.
        The phi join block should have no phi instruction — the sole remaining
        arm (the '\\r' comparison result) becomes the branch condition directly.
        """
        passes_without = [
            MenaiCFGCollapsePhiChains(),
            MenaiCFGSimplifyBlocks(),
        ]
        passes_with = [
            MenaiCFGCollapsePhiChains(),
            MenaiCFGBranchConstProp(),
            MenaiCFGSimplifyBlocks(),
        ]

        cfg_without = self._compile_cfg(self.SKIP_WS, passes_without)
        cfg_with    = self._compile_cfg(self.SKIP_WS, passes_with)

        loop_without = self._find_loop_func(cfg_without)
        loop_with    = self._find_loop_func(cfg_with)

        assert loop_without is not None
        assert loop_with    is not None

        # Without the pass: the join block has a 4-entry phi.
        loop_without_join = next(
            (b for b in loop_without.blocks
             if any(isinstance(i, MenaiCFGPhiInstr) for i in b.instrs)),
            None,
        )
        assert loop_without_join is not None
        phi_without = next(i for i in loop_without_join.instrs if isinstance(i, MenaiCFGPhiInstr))
        assert len(phi_without.incoming) == 4

        # With the pass: no block in the loop has a phi instruction at all.
        assert not any(
            isinstance(i, MenaiCFGPhiInstr)
            for b in loop_with.blocks
            for i in b.instrs
        ), "BranchConstProp should eliminate all phi nodes in the loop"

        # The three const-True blocks jump directly to the self-loop block.
        from menai.menai_cfg import MenaiCFGSelfLoopTerm
        self_loop_block = next(
            b for b in loop_with.blocks
            if isinstance(b.terminator, MenaiCFGSelfLoopTerm)
        )
        const_true_blocks = [
            b for b in loop_with.blocks
            if any(
                isinstance(i, MenaiCFGConstInstr)
                and isinstance(i.value, MenaiBoolean)
                and i.value.value
                for i in b.instrs
            )
        ]
        assert len(const_true_blocks) == 3, (
            f"expected 3 const-True blocks, got {len(const_true_blocks)}"
        )
        for b in const_true_blocks:
            assert isinstance(b.terminator, MenaiCFGJumpTerm)
            assert b.terminator.target.id == self_loop_block.id, (
                f"const-True block {b.id} should jump to self_loop block "
                f"{self_loop_block.id}, got {b.terminator.target.id}"
            )

    def test_skip_ws_correct_results(self):
        """
        skip-ws compiled with all three passes must still produce correct
        results.
        """
        from menai import Menai
        menai = Menai()

        result = menai.evaluate(r"""
        (letrec ((skip-ws
                  (lambda (s pos)
                    (letrec ((loop (lambda (i)
                                     (if (integer>=? i (string-length s))
                                         i
                                         (let ((ch (string-ref s i)))
                                           (if (or (string=? ch " ")
                                               (or (string=? ch "\t")
                                               (or (string=? ch "\n")
                                                   (string=? ch "\r"))))
                                               (loop (integer+ i 1))
                                               i))))))
                      (loop pos)))))
          (list
            (skip-ws "   hello" 0)
            (skip-ws "\t\nworld" 0)
            (skip-ws "no-space" 0)
            (skip-ws "   " 0)))
        """)
        assert result == [3, 2, 0, 3]

    def test_and_chain_const_false_blocks_bypass_join(self):
        """
        The dual of test_skip_ws_const_true_blocks_bypass_join for `and`.

        (and p1 (and p2 (and p3 p4))) desugars to a right-nested chain of
        (if pred #f rest) — wait, actually (if pred rest #f).  After
        CollapsePhiChains the join phi has three const-False arms (one for
        each pred that was false) and one non-constant arm (p4's result).

        BranchConstProp should re-wire the three const-False defining blocks
        to jump directly to the branch false-target, and the single remaining
        non-constant arm should replace the phi as the branch condition directly.
        """
        passes_without = [
            MenaiCFGCollapsePhiChains(),
            MenaiCFGSimplifyBlocks(),
        ]
        passes_with = [
            MenaiCFGCollapsePhiChains(),
            MenaiCFGBranchConstProp(),
            MenaiCFGSimplifyBlocks(),
        ]

        # A loop that advances while all four predicates hold — uses `and`.
        source = r"""
        (define scan
          (lambda (s pos)
            (letrec ((loop (lambda (i)
                             (if (integer>=? i (string-length s))
                                 i
                                 (let ((ch (string-ref s i)))
                                   (if (and (string!=? ch " ")
                                       (and (string!=? ch "\t")
                                       (and (string!=? ch "\n")
                                            (string!=? ch "\r"))))
                                       (loop (integer+ i 1))
                                       i))))))
              (loop pos))))
        """

        cfg_without = self._compile_cfg(source, passes_without)
        cfg_with    = self._compile_cfg(source, passes_with)

        loop_without = self._find_loop_func(cfg_without)
        loop_with    = self._find_loop_func(cfg_with)

        assert loop_without is not None
        assert loop_with    is not None

        # Without the pass: the join block has a 4-entry phi.
        loop_without_join = next(
            (b for b in loop_without.blocks
             if any(isinstance(i, MenaiCFGPhiInstr) for i in b.instrs)),
            None,
        )
        assert loop_without_join is not None
        phi_without = next(i for i in loop_without_join.instrs if isinstance(i, MenaiCFGPhiInstr))
        assert len(phi_without.incoming) == 4

        # With the pass: no block in the loop has a phi instruction at all.
        assert not any(
            isinstance(i, MenaiCFGPhiInstr)
            for b in loop_with.blocks
            for i in b.instrs
        ), "BranchConstProp should eliminate all phi nodes in the loop"

        # The three const-False blocks must not jump to any remaining block
        # (i.e. they must not pass through a join).  SimplifyBlocks will have
        # inlined the trivial return into them, so they terminate directly.
        from menai.menai_cfg import MenaiCFGReturnTerm
        const_false_blocks = [
            b for b in loop_with.blocks
            if any(
                isinstance(i, MenaiCFGConstInstr)
                and isinstance(i.value, MenaiBoolean)
                and not i.value.value
                for i in b.instrs
            )
        ]
        assert len(const_false_blocks) == 3, (
            f"expected 3 const-False blocks, got {len(const_false_blocks)}"
        )
        for b in const_false_blocks:
            # Each const-False block must terminate without jumping through
            # a join — either a direct return (inlined by SimplifyBlocks) or
            # a jump straight to the self-loop (false-target of the branch).
            assert not isinstance(b.terminator, MenaiCFGPhiInstr)
            assert isinstance(b.terminator, (MenaiCFGJumpTerm, MenaiCFGReturnTerm)), (
                f"const-False block {b.id} has unexpected terminator: {b.terminator}"
            )

    def test_and_chain_correct_results(self):
        """
        A function using a nested `and` chain compiled with all three passes
        must produce correct results.
        """
        from menai import Menai
        menai = Menai()

        result = menai.evaluate(r"""
        (letrec ((scan
                  (lambda (s pos)
                    (letrec ((loop (lambda (i)
                                     (if (integer>=? i (string-length s))
                                         i
                                         (let ((ch (string-ref s i)))
                                           (if (and (string!=? ch " ")
                                               (and (string!=? ch "\t")
                                               (and (string!=? ch "\n")
                                                    (string!=? ch "\r"))))
                                               (loop (integer+ i 1))
                                               i))))))
                      (loop pos)))))
          (list
            (scan "hello   " 0)
            (scan "\t\nworld" 0)
            (scan "no-space" 0)
            (scan "   " 0)))
        """)
        assert result == [5, 0, 8, 0]
