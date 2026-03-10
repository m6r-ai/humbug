"""
Tests for CFG optimization passes.

Each pass is tested in isolation using hand-built MenaiCFGFunction objects,
then integration tests compile real Menai source and verify the optimised
CFG has the expected block structure.
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
from menai.menai_cfg_simplify_blocks import MenaiCFGSimplifyBlocks
from menai.menai_value import MenaiInteger



_pass = MenaiCFGSimplifyBlocks()
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

        new_f, changed = _pass._bypass_empty_blocks(f)
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

        new_f, changed = _pass._bypass_empty_blocks(f)
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

        new_f, changed = _pass._bypass_empty_blocks(f)
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

        new_f, changed = _pass._bypass_empty_blocks(f)
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

        new_f, changed = _pass._bypass_empty_blocks(f)
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

        new_f, changed = _pass._bypass_empty_blocks(f)
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

        new_f, changed = _pass._bypass_empty_blocks(f)
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

        new_f, changed = _pass._bypass_empty_blocks(f)
        assert not changed, "block with patch_instrs must not be bypassed"


_ALL_PASSES = [
    MenaiCFGSimplifyBlocks(),
]


class TestFixedPoint:

    def test_empty_then_block_bypassed_and_dead_else_block_removed(self):
        """
        The CFG builder no longer emits stale phi entries: when only one branch
        reaches the join block, no phi is emitted at all.  This test verifies
        the fixed-point loop over SimplifyBlocks + EliminateDeadBlocks
        handles the resulting shape:

          entry: branch cond → then / else
          then:  jump → join          (empty block — no instructions)
          else:  tail_call f()        (never reaches join)
          join:  return v_then        (no phi — builder emits value directly)

        Pass 1 (bypass empty blocks): then_b is empty → entry branches to join
        Pass 2 (dead block): else_b becomes unreachable from join's perspective
          but is still reachable from entry; then_b is now bypassed and removed.

        After full optimization:
        - No phi instructions anywhere (there were none to begin with).
        - then_b (id=1) is bypassed and removed.
        - join_b (id=3) is still reachable from entry directly.
        - The return in join_b references v_then.
        """
        v_then = v("then_val")
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
            terminator=MenaiCFGReturnTerm(value=v_then),
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

        changed = True
        while changed:
            changed = False
            for pass_ in _ALL_PASSES:
                f, c = pass_._optimize_function(f)
                changed = changed or c
        new_f = f

        # No phi instructions anywhere (there were none to begin with).
        for b in new_f.blocks:
            for instr in b.instrs:
                assert not isinstance(instr, MenaiCFGPhiInstr), \
                    f"No phi should remain, found one in block {b.id}"

        # then_b (empty) should have been bypassed and removed.
        assert 1 not in [b.id for b in new_f.blocks], "then_b should be bypassed and removed"

        # A return referencing v_then must still exist.
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

        changed = True
        while changed:
            changed = False
            for pass_ in _ALL_PASSES:
                f, c = pass_._optimize_function(f)
                changed = changed or c
        new_f = f

        assert len(new_f.blocks) == 1
        assert isinstance(new_f.blocks[0].terminator, MenaiCFGReturnTerm)


class TestTrivialReturnInlining:

    def test_jump_to_empty_return_block_inlined(self):
        """
        entry → return_block (no instrs, returns v_r)
        The jump in entry is replaced by a direct return.
        return_block is removed.
        """
        v_r = v("r")
        return_block = block(1, terminator=MenaiCFGReturnTerm(value=v_r), label="ret")
        entry = block(0, terminator=MenaiCFGJumpTerm(target=return_block), label="entry")
        f = func(entry, return_block)

        new_f, changed = _pass._inline_trivial_returns(f)
        assert changed
        assert 1 not in block_ids(new_f), "trivial return block should be removed"
        assert isinstance(new_f.blocks[0].terminator, MenaiCFGReturnTerm)
        assert new_f.blocks[0].terminator.value.id == v_r.id

    def test_jump_to_const_return_block_inlined(self):
        """
        entry → const_ret (LOAD_CONST v_c; return v_c)
        The jump is replaced by LOAD_CONST + return with a fresh SSA value.
        const_ret is removed.
        """
        from menai.menai_value import MenaiBoolean
        v_c = v("c")
        const_ret = block(
            1,
            MenaiCFGConstInstr(result=v_c, value=MenaiBoolean(False)),
            terminator=MenaiCFGReturnTerm(value=v_c),
            label="const_ret",
        )
        entry = block(0, terminator=MenaiCFGJumpTerm(target=const_ret), label="entry")
        f = func(entry, const_ret)

        new_f, changed = _pass._inline_trivial_returns(f)
        assert changed
        assert 1 not in block_ids(new_f), "const return block should be removed"
        term = new_f.blocks[0].terminator
        assert isinstance(term, MenaiCFGReturnTerm)
        # The return value must be a fresh SSA id (not the original v_c).
        assert term.value.id != v_c.id, "fresh SSA value must be used"
        # The const instruction must be present in entry.
        assert any(isinstance(i, MenaiCFGConstInstr) for i in new_f.blocks[0].instrs)

    def test_multiple_predecessors_each_get_fresh_copy(self):
        """
        Two predecessors both jump to the same const return block.
        Each gets its own inlined copy with a distinct fresh SSA value.
        The shared block is removed.
        """
        from menai.menai_value import MenaiBoolean
        v_cond = v("cond")
        v_c = v("c")
        shared_ret = block(
            3,
            MenaiCFGConstInstr(result=v_c, value=MenaiBoolean(False)),
            terminator=MenaiCFGReturnTerm(value=v_c),
            label="shared_ret",
        )
        pred_a = block(1, terminator=MenaiCFGJumpTerm(target=shared_ret), label="pred_a")
        pred_b = block(2, terminator=MenaiCFGJumpTerm(target=shared_ret), label="pred_b")
        entry = block(
            0,
            MenaiCFGConstInstr(result=v_cond, value=MenaiInteger(1)),
            terminator=MenaiCFGBranchTerm(cond=v_cond, true_block=pred_a, false_block=pred_b),
            label="entry",
        )
        f = func(entry, pred_a, pred_b, shared_ret)

        new_f, changed = _pass._inline_trivial_returns(f)
        assert changed
        assert 3 not in block_ids(new_f), "shared return block should be removed"

        ret_a = next(b for b in new_f.blocks if b.id == 1).terminator
        ret_b = next(b for b in new_f.blocks if b.id == 2).terminator
        assert isinstance(ret_a, MenaiCFGReturnTerm)
        assert isinstance(ret_b, MenaiCFGReturnTerm)
        # Each predecessor got a distinct fresh SSA value.
        assert ret_a.value.id != ret_b.value.id, "each predecessor must have a distinct fresh value"
        assert ret_a.value.id != v_c.id
        assert ret_b.value.id != v_c.id

    def test_branch_predecessor_not_inlined(self):
        """
        A block reached via a BranchTerm cannot have the return inlined —
        the trivial return block must remain when it has branch predecessors.
        """
        v_cond = v("cond")
        v_r = v("r")
        ret_block = block(1, terminator=MenaiCFGReturnTerm(value=v_r), label="ret")
        other = block(2, terminator=MenaiCFGReturnTerm(value=v_r), label="other")
        entry = block(
            0,
            MenaiCFGConstInstr(result=v_cond, value=MenaiInteger(1)),
            terminator=MenaiCFGBranchTerm(cond=v_cond, true_block=ret_block, false_block=other),
            label="entry",
        )
        f = func(entry, ret_block, other)

        new_f, changed = _pass._inline_trivial_returns(f)
        assert not changed, "branch predecessor must prevent inlining and removal"
        assert 1 in block_ids(new_f), "ret_block must remain"

    def test_trivial_return_block_with_patch_instrs_not_inlined(self):
        """A trivial return block with patch_instrs must not be inlined."""
        v_c = v("closure")
        v_val = v("val")
        v_r = v("r")
        ret_block = block(
            1,
            patch_instrs=[MenaiCFGPatchClosureInstr(closure=v_c, capture_index=0, value=v_val)],
            terminator=MenaiCFGReturnTerm(value=v_r),
            label="ret",
        )
        entry = block(0, terminator=MenaiCFGJumpTerm(target=ret_block), label="entry")
        f = func(entry, ret_block)

        new_f, changed = _pass._inline_trivial_returns(f)
        assert not changed, "block with patch_instrs must not be inlined"


    def test_phi_predecessor_missing_from_incoming_not_removed(self):
        """
        Bug #3: a phi-bearing trivial return block must not be removed when
        one of its jump predecessors is absent from the phi's incoming list.

        Setup:
          entry branches to pred_a and pred_b.
          pred_a jumps to ret_block.
          pred_b jumps to ret_block.
          ret_block: phi(%val_a <- pred_a); return phi_result

        pred_b is NOT listed in the phi's incoming entries, so it cannot be
        inlined (there is no contributing value to substitute).  The block
        must therefore remain — removing it would leave pred_b with a
        dangling jump.
        """
        v_cond = v("cond")
        v_val_a = v("val_a")
        v_phi = v("phi")

        ret_block = block(
            3,
            MenaiCFGPhiInstr(result=v_phi, incoming=[]),
            terminator=MenaiCFGReturnTerm(value=v_phi),
            label="ret",
        )
        pred_a = block(1, terminator=MenaiCFGJumpTerm(target=ret_block), label="pred_a")
        pred_b = block(2, terminator=MenaiCFGJumpTerm(target=ret_block), label="pred_b")
        entry = block(
            0,
            MenaiCFGConstInstr(result=v_cond, value=MenaiInteger(1)),
            terminator=MenaiCFGBranchTerm(cond=v_cond, true_block=pred_a, false_block=pred_b),
            label="entry",
        )
        # Only pred_a is listed in the phi — pred_b is intentionally absent.
        ret_block.instrs[0] = MenaiCFGPhiInstr(
            result=v_phi, incoming=[(v_val_a, pred_a)]
        )
        f = func(entry, pred_a, pred_b, ret_block)

        new_f, changed = _pass._inline_trivial_returns(f)

        # pred_a was inlined (it is in the phi incoming list).
        assert isinstance(next(b for b in new_f.blocks if b.id == 1).terminator,
                          MenaiCFGReturnTerm), "pred_a should be inlined"
        # ret_block must NOT be removed because pred_b was not inlined.
        assert 3 in block_ids(new_f), "ret_block must remain (pred_b still jumps to it)"

    def test_max_value_id_includes_phi_incoming_values(self):
        """
        Bug #5: _max_value_id must account for value ids that appear only as
        phi incoming values, not as instruction results or other operands.

        If the highest id in the function is a phi incoming value that is not
        defined in any instruction result, _max_value_id would previously
        return a lower bound, causing fresh_value() to reuse an existing id.
        """
        from menai.menai_cfg_simplify_blocks import _max_value_id

        # v_high appears only as a phi incoming value — it is not the result
        # of any instruction in this function.
        v_low = MenaiCFGValue(id=1, hint="low")
        v_high = MenaiCFGValue(id=999, hint="high")
        v_phi = MenaiCFGValue(id=2, hint="phi")

        phi_instr = MenaiCFGPhiInstr(result=v_phi, incoming=[(v_high, None)])  # type: ignore[arg-type]
        b = block(0, phi_instr, terminator=MenaiCFGReturnTerm(value=v_low), label="entry")
        f = func(b)

        assert _max_value_id(f) == 999, (
            "_max_value_id must include phi incoming value ids"
        )


class TestIntegration:
    """
    Compile Menai source through the full pipeline up to the CFG stage and
    verify that the optimizer produces the expected block structure.
    """

    def _build_cfg_raw(self, source: str):
        """Compile source to CFG without running CFG optimization passes."""
        from menai.menai_lexer import MenaiLexer
        from menai.menai_ast_builder import MenaiASTBuilder
        from menai.menai_ast_semantic_analyzer import MenaiASTSemanticAnalyzer
        from menai.menai_ast_module_resolver import MenaiASTModuleResolver
        from menai.menai_ast_desugarer import MenaiASTDesugarer
        from menai.menai_ast_constant_folder import MenaiASTConstantFolder
        from menai.menai_ir_builder import MenaiIRBuilder
        from menai.menai_ir_optimizer import MenaiIROptimizer
        from menai.menai_cfg_builder import MenaiCFGBuilder

        lexer = MenaiLexer()
        ast_builder = MenaiASTBuilder()
        sem = MenaiASTSemanticAnalyzer()
        resolver = MenaiASTModuleResolver(None)
        desugarer = MenaiASTDesugarer()
        folder = MenaiASTConstantFolder()
        ir_builder = MenaiIRBuilder()
        cfg_builder = MenaiCFGBuilder()

        tokens = lexer.lex(source)
        ast = ast_builder.build(tokens, source, "")
        ast = sem.analyze(ast, source)
        ast = resolver.resolve(ast)
        ast = desugarer.desugar(ast)
        ast = folder.optimize(ast)

        ir = ir_builder.build(ast)
        ir_passes = [MenaiIROptimizer()]
        changed = True
        while changed:
            changed = False
            for p in ir_passes:
                ir, c = p.optimize(ir)
                changed = changed or c

        cfg = cfg_builder.build(ir)
        return cfg

    def _build_cfg(self, source: str):
        """Compile source to CFG and run CFG optimization passes."""
        cfg = self._build_cfg_raw(source)
        changed = True
        while changed:
            changed = False
            for pass_ in _ALL_PASSES:
                cfg, c = pass_.optimize(cfg)
                changed = changed or c
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
        cfg_opt = self._build_cfg(source)
        cfg_raw = self._build_cfg_raw(source)

        # Both produce the same logical function.
        # The optimized version should have no MORE blocks than the raw version.
        assert len(cfg_opt.blocks) <= len(cfg_raw.blocks)

    def test_if_with_one_tail_call_branch_eliminates_phi(self):
        """
        (lambda (start-date duration-days add-working-days)
          (if (integer=? duration-days 0)
              start-date
              (add-working-days start-date duration-days)))

        The else branch is a tail-call and never reaches the join block.  The
        CFG builder recognises this and emits no phi at all — the join block
        is empty (just a jump target for the then branch).  The optimizer then
        bypasses the empty join block, reducing the block count.
        """
        source = """
        (lambda (start-date duration-days add-working-days)
          (if (integer=? duration-days 0)
              start-date
              (add-working-days start-date duration-days)))
        """
        cfg_opt = self._build_cfg(source)
        cfg_raw = self._build_cfg_raw(source)

        # The builder emits no phi when only one branch reaches join.
        raw_phis = self._count_phis(cfg_raw)
        assert raw_phis == 0, f"raw CFG should have no phis (builder fix), got {raw_phis}"

        # Optimized CFG should also have no phis.
        opt_phis = self._count_phis(cfg_opt)
        assert opt_phis == 0, f"optimized CFG should have no phis, got {opt_phis}"

        # The innermost lambda (the actual function) should have fewer blocks.
        # The join block is empty (no phi), so SimplifyBlocks eliminates it
        # and the block count drops.
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

        Both branches are tail-calls.  The CFG builder does not create a join
        block at all — it is a builder invariant, not an optimizer concern.
        """
        source = """
        (lambda (x f g)
          (if (integer=? x 0)
              (f x)
              (g x)))
        """
        cfg_raw = self._build_cfg_raw(source)
        assert self._count_phis(cfg_raw) == 0
        # The builder never creates a join block when both branches are tail-calls.
        inner = self._find_innermost_lambda(cfg_raw)
        join_blocks = [b for b in inner.blocks if b.label == "join"]
        assert len(join_blocks) == 0, "builder should not create a join block when both branches are tail-calls"
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
        cfg_opt = self._build_cfg(source)
        cfg_raw = self._build_cfg_raw(source)

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
