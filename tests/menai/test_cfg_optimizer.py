"""
Tests for MenaiCFGOptimizer.

Each pass is tested in isolation using hand-built MenaiCFGFunction objects,
then integration tests compile real Menai source and verify the optimised
CFG has the expected block structure.

Pass coverage:
  1. Stale-phi pruning  (_prune_stale_phi_entries)
  2. Trivial-phi elimination  (_eliminate_trivial_phis)
  3. Empty-block bypass  (_bypass_empty_blocks)
  4. Dead-block elimination  (_eliminate_dead_blocks)
  5. Fixed-point iteration (cascading passes)
  6. Nested lambda optimization
  7. Integration: compile Menai source and inspect CFG
"""

import pytest

from menai.menai_cfg import (
    MenaiCFGBlock,
    MenaiCFGBranchTerm,
    MenaiCFGBuiltinInstr,
    MenaiCFGConstInstr,
    MenaiCFGFunction,
    MenaiCFGGlobalInstr,
    MenaiCFGJumpTerm,
    MenaiCFGMakeClosureInstr,
    MenaiCFGPatchClosureInstr,
    MenaiCFGPhiInstr,
    MenaiCFGReturnTerm,
    MenaiCFGTailCallTerm,
    MenaiCFGValue,
)
from menai.menai_cfg_optimizer import (
    MenaiCFGOptimizer,
    _bypass_empty_blocks,
    _eliminate_dead_blocks,
    _eliminate_trivial_phis,
    _prune_stale_phi_entries,
)
from menai.menai_value import MenaiInteger


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

_vid = 0
_bid = 0


def v(hint: str = "") -> MenaiCFGValue:
    """Return a fresh SSA value with a unique id."""
    global _vid
    _vid += 1
    return MenaiCFGValue(id=_vid, hint=hint)


def block(
    id: int,
    *instrs,
    patch_instrs=None,
    terminator=None,
    label: str = "block",
) -> MenaiCFGBlock:
    """Build a MenaiCFGBlock."""
    b = MenaiCFGBlock(id=id, label=label)
    b.instrs = list(instrs)
    b.patch_instrs = patch_instrs or []
    b.terminator = terminator
    return b


def func(*blocks, params=None, free_vars=None) -> MenaiCFGFunction:
    """Build a MenaiCFGFunction from blocks, linking predecessors."""
    f = MenaiCFGFunction(
        blocks=list(blocks),
        params=params or [],
        free_vars=free_vars or [],
    )
    _link(f)
    return f


def _link(f: MenaiCFGFunction) -> None:
    """Recompute predecessor lists from terminators."""
    for b in f.blocks:
        b.predecessors = []
    for b in f.blocks:
        t = b.terminator
        if isinstance(t, MenaiCFGJumpTerm):
            t.target.predecessors.append(b)
        elif isinstance(t, MenaiCFGBranchTerm):
            t.true_block.predecessors.append(b)
            t.false_block.predecessors.append(b)


def block_ids(f: MenaiCFGFunction) -> list:
    return [b.id for b in f.blocks]


def phi_incoming_pred_ids(phi: MenaiCFGPhiInstr) -> list:
    return [pred.id for _, pred in phi.incoming]


def first_phi(b: MenaiCFGBlock) -> MenaiCFGPhiInstr:
    for instr in b.instrs:
        if isinstance(instr, MenaiCFGPhiInstr):
            return instr
    raise AssertionError(f"No phi in block {b.id}")


# ---------------------------------------------------------------------------
# 1. Pass: Stale-phi pruning
# ---------------------------------------------------------------------------

class TestStalePhi:

    def test_no_change_when_all_entries_valid(self):
        """Phi with two valid predecessors — nothing to prune."""
        v_then = v("then")
        v_else = v("else")
        v_phi = v("phi")

        then_b = block(1, terminator=None, label="then")
        else_b = block(2, terminator=None, label="else")
        join_b = block(
            3,
            MenaiCFGPhiInstr(result=v_phi, incoming=[(v_then, then_b), (v_else, else_b)]),
            terminator=MenaiCFGReturnTerm(value=v_phi),
            label="join",
        )
        # Wire terminators so both branches jump to join.
        then_b.terminator = MenaiCFGJumpTerm(target=join_b)
        else_b.terminator = MenaiCFGJumpTerm(target=join_b)

        cond = v("cond")
        entry = block(
            0,
            MenaiCFGConstInstr(result=cond, value=MenaiInteger(1)),
            terminator=MenaiCFGBranchTerm(cond=cond, true_block=then_b, false_block=else_b),
            label="entry",
        )
        f = func(entry, then_b, else_b, join_b)

        new_f, changed = _prune_stale_phi_entries(f)
        assert not changed
        phi = first_phi(new_f.blocks[3])
        assert len(phi.incoming) == 2

    def test_prunes_entry_with_no_edge(self):
        """
        Phi lists a block as predecessor, but that block's terminator is a
        tail-call (no edge to the phi's block).  The entry must be pruned.
        """
        v_then = v("then")
        v_else = v("else_placeholder")
        v_phi = v("phi")

        # then_b jumps to join → valid edge
        then_b = block(1, label="then")
        # else_b has a tail-call → NO edge to join
        v_func = v("f")
        else_b = block(
            2,
            MenaiCFGGlobalInstr(result=v_func, name="f"),
            terminator=MenaiCFGTailCallTerm(func=v_func, args=[]),
            label="else",
        )
        join_b = block(
            3,
            MenaiCFGPhiInstr(
                result=v_phi,
                incoming=[(v_then, then_b), (v_else, else_b)],
            ),
            terminator=MenaiCFGReturnTerm(value=v_phi),
            label="join",
        )
        then_b.terminator = MenaiCFGJumpTerm(target=join_b)

        cond = v("cond")
        entry = block(
            0,
            MenaiCFGConstInstr(result=cond, value=MenaiInteger(1)),
            terminator=MenaiCFGBranchTerm(cond=cond, true_block=then_b, false_block=else_b),
            label="entry",
        )
        f = func(entry, then_b, else_b, join_b)

        new_f, changed = _prune_stale_phi_entries(f)
        assert changed
        phi = first_phi(new_f.blocks[3])
        assert len(phi.incoming) == 1
        assert phi.incoming[0][1].id == 1  # then_b

    def test_prunes_to_zero_incoming(self):
        """
        A phi whose only predecessor has no edge to it is pruned to zero
        incoming entries.  (Unreachable join block — dead-block elimination
        will remove it in a subsequent pass.)
        """
        v_val = v("val")
        v_phi = v("phi")

        # pred_b has a tail-call, never reaches join_b
        v_f = v("f")
        pred_b = block(
            1,
            MenaiCFGGlobalInstr(result=v_f, name="f"),
            terminator=MenaiCFGTailCallTerm(func=v_f, args=[]),
            label="pred",
        )
        join_b = block(
            2,
            MenaiCFGPhiInstr(result=v_phi, incoming=[(v_val, pred_b)]),
            terminator=MenaiCFGReturnTerm(value=v_phi),
            label="join",
        )
        entry = block(
            0,
            terminator=MenaiCFGJumpTerm(target=pred_b),
            label="entry",
        )
        f = func(entry, pred_b, join_b)

        new_f, changed = _prune_stale_phi_entries(f)
        assert changed
        phi = first_phi(new_f.blocks[2])
        assert len(phi.incoming) == 0

    def test_no_phi_no_change(self):
        """A function with no phi instructions is unchanged."""
        v_c = v("c")
        entry = block(
            0,
            MenaiCFGConstInstr(result=v_c, value=MenaiInteger(42)),
            terminator=MenaiCFGReturnTerm(value=v_c),
            label="entry",
        )
        f = func(entry)
        new_f, changed = _prune_stale_phi_entries(f)
        assert not changed
        assert new_f is f


# ---------------------------------------------------------------------------
# 2. Pass: Trivial-phi elimination
# ---------------------------------------------------------------------------

class TestTrivialPhi:

    def test_single_incoming_phi_eliminated(self):
        """
        join_b has phi(%v_then ← then_b).  After elimination, every use of
        v_phi should be replaced with v_then, and the phi instruction removed.
        """
        v_then = v("then_val")
        v_phi = v("phi")

        then_b = block(1, label="then")
        join_b = block(
            2,
            MenaiCFGPhiInstr(result=v_phi, incoming=[(v_then, then_b)]),
            terminator=MenaiCFGReturnTerm(value=v_phi),
            label="join",
        )
        then_b.terminator = MenaiCFGJumpTerm(target=join_b)

        cond = v("cond")
        entry = block(
            0,
            MenaiCFGConstInstr(result=cond, value=MenaiInteger(1)),
            terminator=MenaiCFGBranchTerm(
                cond=cond,
                true_block=then_b,
                false_block=join_b,  # else falls directly to join (unusual but valid)
            ),
            label="entry",
        )
        f = func(entry, then_b, join_b)

        new_f, changed = _eliminate_trivial_phis(f)
        assert changed

        # join_b should have no phi instruction remaining.
        join_new = new_f.blocks[2]
        for instr in join_new.instrs:
            assert not isinstance(instr, MenaiCFGPhiInstr), "phi should be removed"

        # The return terminator should now reference v_then, not v_phi.
        ret = join_new.terminator
        assert isinstance(ret, MenaiCFGReturnTerm)
        assert ret.value.id == v_then.id, "phi result should be substituted with incoming value"

    def test_chain_of_trivial_phis_resolved(self):
        """
        phi1 → phi2 → v_real: both phis should be eliminated, and uses of
        phi1 and phi2 should both resolve to v_real.
        """
        v_real = v("real")
        v_phi1 = v("phi1")
        v_phi2 = v("phi2")

        src_b = block(1, label="src")
        mid_b = block(
            2,
            MenaiCFGPhiInstr(result=v_phi2, incoming=[(v_real, src_b)]),
            label="mid",
        )
        join_b = block(
            3,
            MenaiCFGPhiInstr(result=v_phi1, incoming=[(v_phi2, mid_b)]),
            terminator=MenaiCFGReturnTerm(value=v_phi1),
            label="join",
        )
        src_b.terminator = MenaiCFGJumpTerm(target=mid_b)
        mid_b.terminator = MenaiCFGJumpTerm(target=join_b)

        entry = block(0, terminator=MenaiCFGJumpTerm(target=src_b), label="entry")
        f = func(entry, src_b, mid_b, join_b)

        new_f, changed = _eliminate_trivial_phis(f)
        assert changed

        # The return in join_b should reference v_real (chain resolved).
        join_new = new_f.blocks[3]
        ret = join_new.terminator
        assert isinstance(ret, MenaiCFGReturnTerm)
        assert ret.value.id == v_real.id

    def test_no_trivial_phi_no_change(self):
        """A phi with two incoming entries is not trivial — no change."""
        v_a = v("a")
        v_b = v("b")
        v_phi = v("phi")

        block_a = block(1, label="a")
        block_b = block(2, label="b")
        join_b = block(
            3,
            MenaiCFGPhiInstr(result=v_phi, incoming=[(v_a, block_a), (v_b, block_b)]),
            terminator=MenaiCFGReturnTerm(value=v_phi),
            label="join",
        )
        block_a.terminator = MenaiCFGJumpTerm(target=join_b)
        block_b.terminator = MenaiCFGJumpTerm(target=join_b)

        cond = v("cond")
        entry = block(
            0,
            MenaiCFGConstInstr(result=cond, value=MenaiInteger(1)),
            terminator=MenaiCFGBranchTerm(cond=cond, true_block=block_a, false_block=block_b),
            label="entry",
        )
        f = func(entry, block_a, block_b, join_b)

        new_f, changed = _eliminate_trivial_phis(f)
        assert not changed

    def test_phi_result_used_in_builtin(self):
        """Phi result used as a builtin operand is correctly substituted."""
        v_val = v("val")
        v_phi = v("phi")
        v_result = v("result")

        src_b = block(1, label="src")
        join_b = block(
            2,
            MenaiCFGPhiInstr(result=v_phi, incoming=[(v_val, src_b)]),
            MenaiCFGBuiltinInstr(result=v_result, op="not", args=[v_phi]),
            terminator=MenaiCFGReturnTerm(value=v_result),
            label="join",
        )
        src_b.terminator = MenaiCFGJumpTerm(target=join_b)

        entry = block(0, terminator=MenaiCFGJumpTerm(target=src_b), label="entry")
        f = func(entry, src_b, join_b)

        new_f, changed = _eliminate_trivial_phis(f)
        assert changed

        join_new = new_f.blocks[2]
        # Find the builtin instruction.
        builtins = [i for i in join_new.instrs if isinstance(i, MenaiCFGBuiltinInstr)]
        assert len(builtins) == 1
        assert builtins[0].args[0].id == v_val.id, "phi substituted in builtin arg"

    def test_phi_substituted_in_patch_closure(self):
        """Phi result used in a patch_closure instruction is substituted."""
        v_val = v("val")
        v_phi = v("phi")
        v_closure = v("closure")

        src_b = block(1, label="src")
        patch = MenaiCFGPatchClosureInstr(closure=v_closure, capture_index=0, value=v_phi)
        join_b = block(
            2,
            MenaiCFGPhiInstr(result=v_phi, incoming=[(v_val, src_b)]),
            patch_instrs=[patch],
            terminator=MenaiCFGReturnTerm(value=v_closure),
            label="join",
        )
        src_b.terminator = MenaiCFGJumpTerm(target=join_b)

        entry = block(0, terminator=MenaiCFGJumpTerm(target=src_b), label="entry")
        f = func(entry, src_b, join_b)

        new_f, changed = _eliminate_trivial_phis(f)
        assert changed

        join_new = new_f.blocks[2]
        assert join_new.patch_instrs[0].value.id == v_val.id


# ---------------------------------------------------------------------------
# 3. Pass: Empty-block bypass
# ---------------------------------------------------------------------------

class TestEmptyBlockBypass:

    def test_simple_empty_block_bypassed(self):
        """
        entry → empty → target
        After bypass: entry → target, empty removed.
        """
        v_c = v("c")
        target = block(2, terminator=MenaiCFGReturnTerm(value=v_c), label="target")
        empty = block(1, terminator=MenaiCFGJumpTerm(target=target), label="empty")
        entry = block(0, terminator=MenaiCFGJumpTerm(target=empty), label="entry")
        f = func(entry, empty, target)

        new_f, changed = _bypass_empty_blocks(f)
        assert changed
        assert 1 not in block_ids(new_f), "empty block should be removed"
        # entry's terminator should now point to target directly.
        assert isinstance(new_f.blocks[0].terminator, MenaiCFGJumpTerm)
        assert new_f.blocks[0].terminator.target.id == 2

    def test_entry_block_not_bypassed(self):
        """The entry block (blocks[0]) is never bypassed even if empty."""
        v_c = v("c")
        target = block(1, terminator=MenaiCFGReturnTerm(value=v_c), label="target")
        entry = block(0, terminator=MenaiCFGJumpTerm(target=target), label="entry")
        f = func(entry, target)

        new_f, changed = _bypass_empty_blocks(f)
        assert not changed, "entry block must not be bypassed"

    def test_chain_of_empty_blocks_collapsed(self):
        """
        entry → empty1 → empty2 → target
        Both empty blocks are bypassed in one pass; entry points to target.
        """
        v_c = v("c")
        target = block(3, terminator=MenaiCFGReturnTerm(value=v_c), label="target")
        empty2 = block(2, terminator=MenaiCFGJumpTerm(target=target), label="empty2")
        empty1 = block(1, terminator=MenaiCFGJumpTerm(target=empty2), label="empty1")
        entry = block(0, terminator=MenaiCFGJumpTerm(target=empty1), label="entry")
        f = func(entry, empty1, empty2, target)

        new_f, changed = _bypass_empty_blocks(f)
        assert changed
        assert 1 not in block_ids(new_f)
        assert 2 not in block_ids(new_f)
        assert new_f.blocks[0].terminator.target.id == 3

    def test_branch_targets_remapped(self):
        """
        entry branches to then_empty and else_real.
        then_empty is empty and jumps to join.
        join has no phi instructions.

        Even though then_empty has a branch predecessor (entry), bypass IS safe
        because join has no phis -- there are no phi stores to emit.  After
        bypass: entry branches directly to join and else_real.
        """
        v_cond = v("cond")
        v_result = v("result")

        join = block(3, terminator=MenaiCFGReturnTerm(value=v_result), label="join")
        then_empty = block(1, terminator=MenaiCFGJumpTerm(target=join), label="then_empty")
        else_real = block(
            2,
            MenaiCFGConstInstr(result=v_result, value=MenaiInteger(99)),
            terminator=MenaiCFGJumpTerm(target=join),
            label="else_real",
        )
        entry = block(
            0,
            MenaiCFGConstInstr(result=v_cond, value=MenaiInteger(1)),
            terminator=MenaiCFGBranchTerm(
                cond=v_cond, true_block=then_empty, false_block=else_real
            ),
            label="entry",
        )
        f = func(entry, then_empty, else_real, join)

        new_f, changed = _bypass_empty_blocks(f)
        assert changed, "empty block with phi-free target must be bypassed"
        assert 1 not in block_ids(new_f), "then_empty must be removed"
        branch = new_f.blocks[0].terminator
        assert isinstance(branch, MenaiCFGBranchTerm)
        assert branch.true_block.id == 3, "true_block should now point to join"

    def test_branch_target_with_phi_not_bypassed(self):
        """
        entry branches to then_empty and else_real.
        then_empty is empty and jumps to join.
        join has a phi instruction.

        Because then_empty has a branch predecessor (entry) AND join has a phi,
        bypass is UNSAFE -- the phi store would be lost.  The empty block must
        remain so it can emit its JumpTerm and trigger phi stores.
        """
        v_cond = v("cond")
        v_then = v("then_val")
        v_else = v("else_val")
        v_phi = v("phi")

        join = block(
            3,
            MenaiCFGPhiInstr(result=v_phi, incoming=[]),  # incoming set below
            terminator=MenaiCFGReturnTerm(value=v_phi),
            label="join",
        )
        then_empty = block(1, terminator=MenaiCFGJumpTerm(target=join), label="then_empty")
        else_real = block(
            2,
            MenaiCFGConstInstr(result=v_else, value=MenaiInteger(99)),
            terminator=MenaiCFGJumpTerm(target=join),
            label="else_real",
        )
        entry = block(
            0,
            MenaiCFGConstInstr(result=v_cond, value=MenaiInteger(1)),
            MenaiCFGConstInstr(result=v_then, value=MenaiInteger(0)),
            terminator=MenaiCFGBranchTerm(
                cond=v_cond, true_block=then_empty, false_block=else_real
            ),
            label="entry",
        )
        join.instrs[0] = MenaiCFGPhiInstr(
            result=v_phi, incoming=[(v_then, then_empty), (v_else, else_real)]
        )
        f = func(entry, then_empty, else_real, join)

        new_f, changed = _bypass_empty_blocks(f)
        assert not changed, (
            "empty block with branch predecessor and phi-bearing target "
            "must not be bypassed (phi store mechanism requires it)"
        )

    def test_phi_incoming_updated_when_empty_block_bypassed(self):
        """
        join has phi(v_then ← then_empty).  then_empty is bypassed to join.
        After bypass, the phi incoming should name entry (the predecessor of
        then_empty) instead of then_empty.
        """
        v_val = v("val")
        v_phi = v("phi")

        join = block(
            3,
            MenaiCFGPhiInstr(result=v_phi, incoming=[]),  # incoming filled below
            terminator=MenaiCFGReturnTerm(value=v_phi),
            label="join",
        )
        then_empty = block(1, terminator=MenaiCFGJumpTerm(target=join), label="then_empty")

        # entry jumps to then_empty; then_empty is empty and jumps to join.
        # The phi should list then_empty as predecessor, which we want to
        # re-label to entry after bypass.
        entry = block(
            0,
            MenaiCFGConstInstr(result=v_val, value=MenaiInteger(1)),
            terminator=MenaiCFGJumpTerm(target=then_empty),
            label="entry",
        )
        # Set up phi incoming to reference then_empty.
        join.instrs[0] = MenaiCFGPhiInstr(
            result=v_phi, incoming=[(v_val, then_empty)]
        )
        f = func(entry, then_empty, join)

        new_f, changed = _bypass_empty_blocks(f)
        assert changed

        join_new = new_f.blocks[1]  # join is now blocks[1] after empty removed
        phi = first_phi(join_new)
        assert len(phi.incoming) == 1
        # The incoming predecessor should now be entry (id=0), not then_empty (id=1).
        assert phi.incoming[0][1].id == 0, "phi predecessor should be re-labelled to entry"

    def test_non_empty_block_not_bypassed(self):
        """A block with instructions is not bypassed."""
        v_c = v("c")
        v_r = v("r")
        real = block(
            1,
            MenaiCFGConstInstr(result=v_c, value=MenaiInteger(1)),
            terminator=MenaiCFGReturnTerm(value=v_r),
            label="real",
        )
        entry = block(0, terminator=MenaiCFGJumpTerm(target=real), label="entry")
        f = func(entry, real)

        new_f, changed = _bypass_empty_blocks(f)
        assert not changed

    def test_block_with_patch_instrs_not_bypassed(self):
        """A block with patch_instrs is not empty — must not be bypassed."""
        v_c = v("closure")
        v_val = v("val")
        v_r = v("r")
        target = block(2, terminator=MenaiCFGReturnTerm(value=v_r), label="target")
        patched = block(
            1,
            patch_instrs=[MenaiCFGPatchClosureInstr(closure=v_c, capture_index=0, value=v_val)],
            terminator=MenaiCFGJumpTerm(target=target),
            label="patched",
        )
        entry = block(0, terminator=MenaiCFGJumpTerm(target=patched), label="entry")
        f = func(entry, patched, target)

        new_f, changed = _bypass_empty_blocks(f)
        assert not changed, "block with patch_instrs must not be bypassed"


# ---------------------------------------------------------------------------
# 4. Pass: Dead-block elimination
# ---------------------------------------------------------------------------

class TestDeadBlockElimination:

    def test_unreachable_block_removed(self):
        """A block with no predecessors (and not the entry) is removed."""
        v_c = v("c")
        v_d = v("d")
        dead = block(1, terminator=MenaiCFGReturnTerm(value=v_d), label="dead")
        entry = block(
            0,
            MenaiCFGConstInstr(result=v_c, value=MenaiInteger(1)),
            terminator=MenaiCFGReturnTerm(value=v_c),
            label="entry",
        )
        f = func(entry, dead)

        new_f, changed = _eliminate_dead_blocks(f)
        assert changed
        assert 1 not in block_ids(new_f)

    def test_reachable_blocks_kept(self):
        """All blocks reachable from entry are kept."""
        v_c = v("c")
        b1 = block(1, terminator=MenaiCFGReturnTerm(value=v_c), label="b1")
        entry = block(0, terminator=MenaiCFGJumpTerm(target=b1), label="entry")
        f = func(entry, b1)

        new_f, changed = _eliminate_dead_blocks(f)
        assert not changed

    def test_stale_phi_entry_pruned_for_dead_predecessor(self):
        """
        A phi that lists a dead block as predecessor has that entry pruned.
        """
        v_live = v("live")
        v_dead = v("dead_val")
        v_phi = v("phi")

        dead_b = block(1, label="dead")
        live_b = block(2, label="live")
        join_b = block(
            3,
            MenaiCFGPhiInstr(
                result=v_phi,
                incoming=[(v_live, live_b), (v_dead, dead_b)],
            ),
            terminator=MenaiCFGReturnTerm(value=v_phi),
            label="join",
        )
        live_b.terminator = MenaiCFGJumpTerm(target=join_b)
        # dead_b has no terminator pointing to it from entry → unreachable.
        dead_b.terminator = MenaiCFGJumpTerm(target=join_b)

        entry = block(0, terminator=MenaiCFGJumpTerm(target=live_b), label="entry")
        f = func(entry, dead_b, live_b, join_b)

        new_f, changed = _eliminate_dead_blocks(f)
        assert changed
        assert 1 not in block_ids(new_f)

        join_new = next(b for b in new_f.blocks if b.id == 3)
        phi = first_phi(join_new)
        assert len(phi.incoming) == 1
        assert phi.incoming[0][1].id == 2

    def test_entry_block_kept_even_if_empty(self):
        """The entry block is never removed, even if it has no predecessors."""
        v_c = v("c")
        entry = block(
            0,
            MenaiCFGConstInstr(result=v_c, value=MenaiInteger(1)),
            terminator=MenaiCFGReturnTerm(value=v_c),
            label="entry",
        )
        f = func(entry)

        new_f, changed = _eliminate_dead_blocks(f)
        assert not changed
        assert len(new_f.blocks) == 1

    def test_branch_both_targets_reachable(self):
        """Both sides of a branch are reachable and kept."""
        v_cond = v("cond")
        v_t = v("t")
        v_e = v("e")
        then_b = block(1, terminator=MenaiCFGReturnTerm(value=v_t), label="then")
        else_b = block(2, terminator=MenaiCFGReturnTerm(value=v_e), label="else")
        entry = block(
            0,
            MenaiCFGConstInstr(result=v_cond, value=MenaiInteger(1)),
            terminator=MenaiCFGBranchTerm(cond=v_cond, true_block=then_b, false_block=else_b),
            label="entry",
        )
        f = func(entry, then_b, else_b)

        new_f, changed = _eliminate_dead_blocks(f)
        assert not changed
        assert len(new_f.blocks) == 3


# ---------------------------------------------------------------------------
# 5. Fixed-point: cascading passes
# ---------------------------------------------------------------------------

class TestFixedPoint:

    def test_stale_phi_then_trivial_phi_then_dead_block(self):
        """
        The canonical `if` with one tail-call branch:

          entry: branch cond → then / else
          then:  jump → join
          else:  tail_call f()
          join:  phi(v_then ← then, v_else ← else) → return phi

        Pass 1 (stale phi): prune else entry from phi → phi(v_then ← then)
        Pass 2 (trivial phi): eliminate phi → join uses v_then directly
        Pass 3 (empty block): then_b has only a jump → bypass it
        Pass 4 (dead block): join_b may become unreachable if then_b is gone
          (but here join_b is still reachable from entry via then_b → join_b)

        After full optimization:
        - join_b has no phi
        - join_b returns v_then directly
        - 4 blocks → at most 3 (else is unreachable from join after phi elim?
          No — else is reachable from entry's branch, just doesn't reach join)
        - The else block (tail-call) remains reachable from entry.
        """
        v_then = v("then_val")
        v_else_placeholder = v("else_placeholder")
        v_phi = v("phi")
        v_f = v("f")
        v_cond = v("cond")

        then_b = block(1, label="then")
        else_b = block(
            2,
            MenaiCFGGlobalInstr(result=v_f, name="f"),
            terminator=MenaiCFGTailCallTerm(func=v_f, args=[]),
            label="else",
        )
        join_b = block(
            3,
            MenaiCFGPhiInstr(
                result=v_phi,
                incoming=[(v_then, then_b), (v_else_placeholder, else_b)],
            ),
            terminator=MenaiCFGReturnTerm(value=v_phi),
            label="join",
        )
        then_b.terminator = MenaiCFGJumpTerm(target=join_b)

        entry = block(
            0,
            MenaiCFGConstInstr(result=v_cond, value=MenaiInteger(1)),
            terminator=MenaiCFGBranchTerm(
                cond=v_cond, true_block=then_b, false_block=else_b
            ),
            label="entry",
        )
        f = func(entry, then_b, else_b, join_b)

        optimizer = MenaiCFGOptimizer()
        new_f = optimizer._optimize_function(f)

        # After full optimization:
        # - No phi instructions anywhere.
        for b in new_f.blocks:
            for instr in b.instrs:
                assert not isinstance(instr, MenaiCFGPhiInstr), \
                    f"No phi should remain, found one in block {b.id}"

        # - join_b (id=3) should still exist (reachable via then_b or directly)
        #   OR then_b was bypassed and join_b is reached directly from entry.
        # Either way, the return terminator somewhere should reference v_then.
        all_returns = [
            b.terminator for b in new_f.blocks
            if isinstance(b.terminator, MenaiCFGReturnTerm)
        ]
        assert any(r.value.id == v_then.id for r in all_returns), \
            "At least one return should reference v_then after phi elimination"

    def test_no_change_on_already_optimal_function(self):
        """
        A simple function with no degenerate patterns is unchanged.
        """
        v_c = v("c")
        entry = block(
            0,
            MenaiCFGConstInstr(result=v_c, value=MenaiInteger(1)),
            terminator=MenaiCFGReturnTerm(value=v_c),
            label="entry",
        )
        f = func(entry)

        optimizer = MenaiCFGOptimizer()
        new_f = optimizer._optimize_function(f)

        assert len(new_f.blocks) == 1
        assert isinstance(new_f.blocks[0].terminator, MenaiCFGReturnTerm)


# ---------------------------------------------------------------------------
# 6. Nested lambda optimization
# ---------------------------------------------------------------------------

class TestNestedLambdaOptimization:

    def test_nested_lambda_is_optimized(self):
        """
        A MenaiCFGMakeClosureInstr whose child function has a trivial phi
        should have that phi eliminated by the recursive optimization pass.
        """
        # Build a child function with a trivial phi.
        v_val = v("val")
        v_phi = v("phi_child")
        child_src = block(1, label="child_src")
        child_join = block(
            2,
            MenaiCFGPhiInstr(result=v_phi, incoming=[(v_val, child_src)]),
            terminator=MenaiCFGReturnTerm(value=v_phi),
            label="child_join",
        )
        child_src.terminator = MenaiCFGJumpTerm(target=child_join)
        child_entry = block(0, terminator=MenaiCFGJumpTerm(target=child_src), label="child_entry")
        child_func = func(child_entry, child_src, child_join)

        # Parent function contains a MakeClosureInstr wrapping the child.
        v_closure = v("closure")
        mk = MenaiCFGMakeClosureInstr(
            result=v_closure,
            function=child_func,
            captures=[],
            needs_patching=False,
        )
        parent_entry = block(
            0,
            mk,
            terminator=MenaiCFGReturnTerm(value=v_closure),
            label="entry",
        )
        parent_f = func(parent_entry)

        optimizer = MenaiCFGOptimizer()
        new_parent = optimizer.optimize(parent_f)

        # Find the MakeClosureInstr in the optimized parent.
        mk_new = None
        for instr in new_parent.blocks[0].instrs:
            if isinstance(instr, MenaiCFGMakeClosureInstr):
                mk_new = instr
                break
        assert mk_new is not None

        # The child function should have no phi instructions.
        for b in mk_new.function.blocks:
            for instr in b.instrs:
                assert not isinstance(instr, MenaiCFGPhiInstr), \
                    "Nested lambda phi should have been eliminated"


# ---------------------------------------------------------------------------
# 7. Integration: compile Menai source and inspect CFG
# ---------------------------------------------------------------------------

class TestIntegration:
    """
    Compile Menai source through the full pipeline up to the CFG stage and
    verify that the optimizer produces the expected block structure.

    We test by compiling with and without the optimizer and checking that the
    optimized CFG has fewer blocks / no trivial phis.
    """

    def _build_cfg(self, source: str, optimize: bool):
        """Compile source to CFG, with or without CFG optimization."""
        from menai.menai_lexer import MenaiLexer
        from menai.menai_ast_builder import MenaiASTBuilder
        from menai.menai_ast_semantic_analyzer import MenaiASTSemanticAnalyzer
        from menai.menai_module_resolver import MenaiModuleResolver
        from menai.menai_ast_desugarer import MenaiASTDesugarer
        from menai.menai_ast_constant_folder import MenaiASTConstantFolder
        from menai.menai_ir_builder import MenaiIRBuilder
        from menai.menai_ir_copy_propagator import MenaiIRCopyPropagator
        from menai.menai_ir_inline_once import MenaiIRInlineOnce
        from menai.menai_ir_optimizer import MenaiIROptimizer
        from menai.menai_cfg_builder import MenaiCFGBuilder
        from menai.menai_cfg_optimizer import MenaiCFGOptimizer

        lexer = MenaiLexer()
        ast_builder = MenaiASTBuilder()
        sem = MenaiASTSemanticAnalyzer()
        resolver = MenaiModuleResolver(None)
        desugarer = MenaiASTDesugarer()
        folder = MenaiASTConstantFolder()
        ir_builder = MenaiIRBuilder()
        cfg_builder = MenaiCFGBuilder()
        cfg_optimizer = MenaiCFGOptimizer()

        tokens = lexer.lex(source)
        ast = ast_builder.build(tokens, source, "")
        ast = sem.analyze(ast, source)
        ast = resolver.resolve(ast)
        ast = desugarer.desugar(ast)
        ast = folder.optimize(ast)

        ir = ir_builder.build(ast)
        ir_passes = [MenaiIRCopyPropagator(), MenaiIRInlineOnce(), MenaiIROptimizer()]
        changed = True
        while changed:
            changed = False
            for p in ir_passes:
                ir, c = p.optimize(ir)
                changed = changed or c

        cfg = cfg_builder.build(ir)
        if optimize:
            cfg = cfg_optimizer.optimize(cfg)
        return cfg

    def _find_lambda_cfg(self, cfg: MenaiCFGFunction, name: str) -> MenaiCFGFunction:
        """Find a nested MenaiCFGFunction by binding_name via BFS."""
        queue = [cfg]
        while queue:
            f = queue.pop(0)
            if f.binding_name == name:
                return f
            for b in f.blocks:
                for instr in b.instrs:
                    if isinstance(instr, MenaiCFGMakeClosureInstr):
                        queue.append(instr.function)
        raise AssertionError(f"Lambda {name!r} not found in CFG")

    def _count_phis(self, cfg: MenaiCFGFunction) -> int:
        """Count phi instructions in a CFG function and all nested lambdas."""
        total = sum(
            1
            for b in cfg.blocks
            for instr in b.instrs
            if isinstance(instr, MenaiCFGPhiInstr)
        )
        for b in cfg.blocks:
            for instr in b.instrs:
                if isinstance(instr, MenaiCFGMakeClosureInstr):
                    total += self._count_phis(instr.function)
        return total

    def _find_innermost_lambda(self, cfg: MenaiCFGFunction) -> MenaiCFGFunction:
        """Return the innermost (deepest) nested lambda CFG."""
        for b in cfg.blocks:
            for instr in b.instrs:
                if isinstance(instr, MenaiCFGMakeClosureInstr):
                    return self._find_innermost_lambda(instr.function)
        return cfg

    def test_simple_if_with_tail_call_branch(self):
        """
        (lambda (x) (if (integer=? x 0) x (integer+ x 1)))

        One branch returns x directly; the other returns integer+(x,1).
        Both branches reach the join block, so the phi is genuine (2 incoming).
        The optimizer should not eliminate it.
        After optimization: CFG should be valid and produce correct results.
        """
        source = "(lambda (x) (if (integer=? x 0) x (integer+ x 1)))"
        cfg_opt = self._build_cfg(source, optimize=True)
        cfg_raw = self._build_cfg(source, optimize=False)

        # Both produce the same logical function.
        # The optimized version should have no MORE blocks than the raw version.
        assert len(cfg_opt.blocks) <= len(cfg_raw.blocks)

    def test_if_with_one_tail_call_branch_eliminates_phi(self):
        """
        (lambda (start-date duration-days add-working-days)
          (if (integer=? duration-days 0)
              start-date
              (add-working-days start-date duration-days)))

        The else branch is a tail-call and never reaches the join block.
        The join phi is stale after pruning → trivial → eliminated.
        Optimized CFG should have no phi instructions.
        """
        source = """
        (lambda (start-date duration-days add-working-days)
          (if (integer=? duration-days 0)
              start-date
              (add-working-days start-date duration-days)))
        """
        cfg_opt = self._build_cfg(source, optimize=True)
        cfg_raw = self._build_cfg(source, optimize=False)

        # The lambda is nested inside a top-level MakeClosure wrapper.
        # Count phis recursively (including nested lambdas).
        raw_phis = self._count_phis(cfg_raw)
        assert raw_phis >= 1, f"raw CFG should have phi(s), got {raw_phis}"

        # Optimized CFG (including nested lambdas) should have no phis.
        opt_phis = self._count_phis(cfg_opt)
        assert opt_phis == 0, f"optimized CFG should have no phis, got {opt_phis}"

        # The innermost lambda (the actual function) should have fewer blocks.
        # After phi elimination the then-block has a phi-free target, so it
        # can be bypassed and the block count drops.
        raw_inner = self._find_innermost_lambda(cfg_raw)
        opt_inner = self._find_innermost_lambda(cfg_opt)
        assert len(opt_inner.blocks) < len(raw_inner.blocks), \
            f"optimized lambda should have fewer blocks: {len(opt_inner.blocks)} vs {len(raw_inner.blocks)}"

    def test_if_with_both_tail_call_branches(self):
        """
        (lambda (x f g)
          (if (integer=? x 0)
              (f x)
              (g x)))

        Both branches are tail-calls.  The join block is unreachable.
        After optimization: no phi, no join block.
        """
        source = """
        (lambda (x f g)
          (if (integer=? x 0)
              (f x)
              (g x)))
        """
        cfg_opt = self._build_cfg(source, optimize=True)

        assert self._count_phis(cfg_opt) == 0
        # No join block should remain in the innermost lambda (it was unreachable).
        inner = self._find_innermost_lambda(cfg_opt)
        join_blocks = [b for b in inner.blocks if b.label == "join"]
        assert len(join_blocks) == 0, "unreachable join block should be eliminated"

    def test_nested_if_optimization(self):
        """
        Nested ifs: multiple join blocks, each with a phi.
        When inner branches are tail-calls, inner phis become trivial.
        """
        source = """
        (lambda (x y f)
          (if (integer=? x 0)
              (if (integer=? y 0)
                  42
                  (f y))
              (f x)))
        """
        cfg_opt = self._build_cfg(source, optimize=True)
        cfg_raw = self._build_cfg(source, optimize=False)

        # Optimized should have fewer or equal blocks.
        assert len(cfg_opt.blocks) <= len(cfg_raw.blocks)
        # No stale phis should remain.
        # (The inner if has one tail-call branch, so its phi becomes trivial.)
        assert self._count_phis(cfg_opt) <= self._count_phis(cfg_raw)

    def test_compiled_result_correct_after_optimization(self):
        """
        End-to-end: compile and execute a function through the full pipeline
        (including CFG optimizer) and verify the result is correct.
        """
        from menai import Menai
        menai = Menai()

        # calculate-end-date pattern: if duration=0 return start, else call f
        result = menai.evaluate("""
        (let ((calc-end
               (lambda (start duration)
                 (if (integer=? duration 0)
                     start
                     (integer+ start duration)))))
          (list
            (calc-end 10 0)
            (calc-end 10 5)))
        """)
        # menai.evaluate() returns Python native values
        assert result == [10, 15]

    def test_deeply_recursive_function_correct(self):
        """
        A recursive function with a tail-call branch pattern should still
        execute correctly after optimization.
        """
        from menai import Menai
        menai = Menai()

        result = menai.evaluate("""
        (letrec ((sum (lambda (n acc)
                        (if (integer=? n 0)
                            acc
                            (sum (integer- n 1) (integer+ acc n))))))
          (sum 100 0))
        """)
        # menai.evaluate() returns Python native values
        assert result == 5050

    def test_multiple_ifs_in_letrec(self):
        """
        A letrec with multiple lambdas each containing an if-with-tail-call.
        All should be correctly optimized.
        """
        from menai import Menai
        menai = Menai()

        result = menai.evaluate("""
        (letrec ((f (lambda (x)
                      (if (integer=? x 0) 0 (integer+ x 1))))
                 (g (lambda (x)
                      (if (integer=? x 0) 1 (integer* x 2)))))
          (list (f 0) (f 3) (g 0) (g 4)))
        """)
        # menai.evaluate() returns Python native values
        assert result == [0, 4, 1, 8]
