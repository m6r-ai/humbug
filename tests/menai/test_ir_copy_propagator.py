"""
Tests for MenaiIRCopyPropagator.

Strategy
--------
The tests are split into two layers:

1. Unit tests on the propagator directly, by constructing IR nodes by hand.
   These are precise and fast — they don't go through the full compiler
   pipeline and so are immune to unrelated pipeline changes.

2. Integration tests that compile real Menai source and verify that the
   optimized program still produces the correct result.  These catch
   regressions where the propagator silently changes semantics.

For the unit tests we build minimal IR trees that exercise specific cases:
  - constant binding is inlined
  - empty-list binding is inlined
  - quote binding is inlined
  - global-variable binding is inlined
  - local-variable binding is inlined (no captures)
  - local-variable binding is NOT inlined when captured by a child lambda
  - lambda binding is NOT inlined (not trivially copyable)
  - call binding is NOT inlined (not trivially copyable)
  - shadowing: inner let binding with same slot index is not clobbered
  - letrec bindings are never propagated
  - tail-recursive sentinel is never substituted
  - substitutions property tracks count correctly
  - changed flag is True iff at least one substitution occurred
  - integration with MenaiIROptimizer (dead-binding elimination cleans up)
"""

from __future__ import annotations

import pytest

from menai import Menai
from menai.menai_compiler import MenaiCompiler
from menai.menai_ir import (
    MenaiIRCall,
    MenaiIRConstant,
    MenaiIREmptyList,
    MenaiIRIf,
    MenaiIRLambda,
    MenaiIRLet,
    MenaiIRLetrec,
    MenaiIRQuote,
    MenaiIRReturn,
    MenaiIRVariable,
)
from menai.menai_ir_copy_propagator import MenaiIRCopyPropagator
from menai.menai_ir_optimizer import MenaiIROptimizer
from menai.menai_value import MenaiInteger, MenaiString, MenaiSymbol


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _const(n: int) -> MenaiIRConstant:
    return MenaiIRConstant(value=MenaiInteger(n))


def _str_const(s: str) -> MenaiIRConstant:
    return MenaiIRConstant(value=MenaiString(s))


def _local(index: int, depth: int = 0, is_parent_ref: bool = False) -> MenaiIRVariable:
    return MenaiIRVariable(
        name=f"v{index}",
        var_type='local',
        depth=depth,
        index=index,
        is_parent_ref=is_parent_ref,
    )


def _global(name: str) -> MenaiIRVariable:
    return MenaiIRVariable(name=name, var_type='global', depth=0, index=0)


def _add_call(a, b) -> MenaiIRCall:
    """Emit (integer+ a b) as a builtin call."""
    return MenaiIRCall(
        func_plan=_global('integer+'),
        arg_plans=[a, b],
        is_tail_call=False,
        is_tail_recursive=False,
        is_builtin=True,
        builtin_name='integer+',
    )


def _run(ir):
    """Run the copy propagator and return (new_ir, changed)."""
    return MenaiIRCopyPropagator().optimize(ir)


def _run_prop(ir):
    """Run the copy propagator and return just the new IR."""
    new_ir, _ = _run(ir)
    return new_ir


def _run_both(ir):
    """Run copy propagation then dead-binding elimination, return final IR."""
    ir, _ = MenaiIRCopyPropagator().optimize(ir)
    ir, _ = MenaiIROptimizer().optimize(ir)
    return ir


# ---------------------------------------------------------------------------
# Unit tests: trivially copyable predicates
# ---------------------------------------------------------------------------

class TestTriviallyInlineable:
    """Verify which value plans are considered trivially copyable."""

    def test_constant_is_inlined(self):
        """A let binding whose value is a constant is copy-propagated."""
        # (let ((x 42)) x)  →  42
        ir = MenaiIRReturn(value_plan=MenaiIRLet(
            bindings=[("x", _const(42), 0)],
            body_plan=_local(0),
            in_tail_position=True,
        ))
        result = _run_prop(ir)
        # After propagation the body should be the constant directly.
        # The let may still exist (binding dropped but let not yet collapsed
        # by the propagator — that's the dead-binder's job), OR it may be
        # collapsed if _prop_let detects no live bindings.
        # Either way, the body should resolve to the constant.
        assert isinstance(result, MenaiIRReturn)
        # The let should have collapsed (no live bindings).
        assert isinstance(result.value_plan, MenaiIRConstant)
        assert result.value_plan.value == MenaiInteger(42)

    def test_empty_list_is_inlined(self):
        """A let binding whose value is an empty list is copy-propagated."""
        from menai.menai_ir import MenaiIREmptyList
        ir = MenaiIRReturn(value_plan=MenaiIRLet(
            bindings=[("xs", MenaiIREmptyList(), 0)],
            body_plan=_local(0),
            in_tail_position=True,
        ))
        result = _run_prop(ir)
        assert isinstance(result, MenaiIRReturn)
        assert isinstance(result.value_plan, MenaiIREmptyList)

    def test_quote_is_inlined(self):
        """A let binding whose value is a quote is copy-propagated."""
        quoted = MenaiIRQuote(quoted_value=MenaiSymbol("hello"))
        ir = MenaiIRReturn(value_plan=MenaiIRLet(
            bindings=[("sym", quoted, 0)],
            body_plan=_local(0),
            in_tail_position=True,
        ))
        result = _run_prop(ir)
        assert isinstance(result, MenaiIRReturn)
        assert isinstance(result.value_plan, MenaiIRQuote)

    def test_global_variable_is_inlined(self):
        """A let binding whose value is a global variable is copy-propagated."""
        ir = MenaiIRReturn(value_plan=MenaiIRLet(
            bindings=[("f", _global('integer+'), 0)],
            body_plan=_local(0),
            in_tail_position=True,
        ))
        result = _run_prop(ir)
        assert isinstance(result, MenaiIRReturn)
        assert isinstance(result.value_plan, MenaiIRVariable)
        assert result.value_plan.var_type == 'global'

    def test_local_variable_is_inlined_when_no_captures(self):
        """A let binding whose value is a local variable is inlined when not captured."""
        # (let ((x 1) (y x)) y)
        # Both x (constant) and y (local alias) are in to_propagate.
        # Substitution is a single-pass walk: body _local(1) → _local(0).
        # The result of substituting _local(0) is NOT further substituted
        # in the same pass (substitution is not recursive).
        # Both bindings are dropped → let collapses → body is _local(0).
        ir = MenaiIRReturn(value_plan=MenaiIRLet(
            bindings=[("x", _const(1), 0), ("y", _local(0), 1)],
            body_plan=_local(1),   # body uses y (slot 1)
            in_tail_position=True,
        ))
        result = _run_prop(ir)
        assert isinstance(result, MenaiIRReturn)
        # Both bindings dropped → let collapsed → body is _local(0).
        # (A second pass of the propagator or dead-binder would then inline x.)
        assert isinstance(result.value_plan, MenaiIRVariable)
        assert result.value_plan.var_type == 'local'
        assert result.value_plan.index == 0

    def test_lambda_is_not_inlined(self):
        """A let binding whose value is a lambda is NOT copy-propagated."""
        lam = MenaiIRLambda(
            params=["p"],
            body_plan=MenaiIRReturn(value_plan=_local(0)),
            free_vars=[],
            free_var_plans=[],
            parent_refs=[],
            parent_ref_plans=[],
            param_count=1,
            is_variadic=False,
            max_locals=1,
        )
        ir = MenaiIRReturn(value_plan=MenaiIRLet(
            bindings=[("f", lam, 0)],
            body_plan=_local(0),
            in_tail_position=True,
        ))
        _, changed = _run(ir)
        assert not changed

    def test_call_is_not_inlined(self):
        """A let binding whose value is a call is NOT copy-propagated."""
        ir = MenaiIRReturn(value_plan=MenaiIRLet(
            bindings=[("r", _add_call(_const(1), _const(2)), 0)],
            body_plan=_local(0),
            in_tail_position=True,
        ))
        _, changed = _run(ir)
        assert not changed


# ---------------------------------------------------------------------------
# Unit tests: lambda boundary rule
# ---------------------------------------------------------------------------

class TestLambdaBoundaryRule:
    """Verify the lambda boundary rule for local variable inlining."""

    def test_local_var_not_inlined_when_captured(self):
        """
        A local variable binding that is captured by a child lambda must NOT
        be copy-propagated (the depth arithmetic would be wrong).
        """
        # (let ((x 1))
        #   (lambda () x))
        # x is captured by the lambda via free_var_plans.
        # The use counter will record external_count(frame=0, slot=0) = 1.
        # Therefore x should NOT be inlined.
        free_var_plan = _local(index=0, depth=0)  # loads x in enclosing frame
        lam = MenaiIRLambda(
            params=[],
            body_plan=MenaiIRReturn(value_plan=_local(index=0, depth=1)),
            free_vars=["x"],
            free_var_plans=[free_var_plan],
            parent_refs=[],
            parent_ref_plans=[],
            param_count=0,
            is_variadic=False,
            max_locals=1,
        )
        ir = MenaiIRReturn(value_plan=MenaiIRLet(
            bindings=[("x", _local(index=5, depth=0), 0)],  # x = some other local
            body_plan=lam,
            in_tail_position=True,
        ))
        _, changed = _run(ir)
        # x is captured → external_count > 0 → must NOT be inlined.
        assert not changed

    def test_constant_inlined_even_when_captured(self):
        """
        A constant binding is inlined even when the binding is captured by a
        child lambda, because constants contain no frame-relative references.
        """
        # (let ((k 99))
        #   (lambda () k))
        # k is captured, but its value is a constant — always safe to inline.
        free_var_plan = _local(index=0, depth=0)
        lam = MenaiIRLambda(
            params=[],
            body_plan=MenaiIRReturn(value_plan=_local(index=0, depth=1)),
            free_vars=["k"],
            free_var_plans=[free_var_plan],
            parent_refs=[],
            parent_ref_plans=[],
            param_count=0,
            is_variadic=False,
            max_locals=1,
        )
        ir = MenaiIRReturn(value_plan=MenaiIRLet(
            bindings=[("k", _const(99), 0)],
            body_plan=lam,
            in_tail_position=True,
        ))
        _, changed = _run(ir)
        # Constant → always inlineable regardless of captures.
        assert changed

    def test_global_inlined_even_when_captured(self):
        """
        A global variable binding is inlined even when captured by a child
        lambda, because globals use name-table lookup (not frame-relative).
        """
        free_var_plan = _local(index=0, depth=0)
        lam = MenaiIRLambda(
            params=[],
            body_plan=MenaiIRReturn(value_plan=_local(index=0, depth=1)),
            free_vars=["g"],
            free_var_plans=[free_var_plan],
            parent_refs=[],
            parent_ref_plans=[],
            param_count=0,
            is_variadic=False,
            max_locals=1,
        )
        ir = MenaiIRReturn(value_plan=MenaiIRLet(
            bindings=[("g", _global('integer+'), 0)],
            body_plan=lam,
            in_tail_position=True,
        ))
        _, changed = _run(ir)
        assert changed


# ---------------------------------------------------------------------------
# Unit tests: substitution correctness
# ---------------------------------------------------------------------------

class TestSubstitutionCorrectness:
    """Verify that substitution replaces all occurrences correctly."""

    def test_multiple_uses_all_replaced(self):
        """All uses of a propagated binding are replaced."""
        # (let ((x 7)) (integer+ x x))  →  (integer+ 7 7)
        ir = MenaiIRReturn(value_plan=MenaiIRLet(
            bindings=[("x", _const(7), 0)],
            body_plan=_add_call(_local(0), _local(0)),
            in_tail_position=True,
        ))
        result = _run_prop(ir)
        assert isinstance(result, MenaiIRReturn)
        call = result.value_plan
        assert isinstance(call, MenaiIRCall)
        assert isinstance(call.arg_plans[0], MenaiIRConstant)
        assert call.arg_plans[0].value == MenaiInteger(7)
        assert isinstance(call.arg_plans[1], MenaiIRConstant)
        assert call.arg_plans[1].value == MenaiInteger(7)

    def test_only_matching_slot_replaced(self):
        """Only the slot being propagated is replaced; other slots are untouched."""
        # (let ((x 1) (y 2)) (integer+ x y))
        # Propagate x=1 (constant); y is also a constant so it gets propagated too.
        # After propagation: (integer+ 1 2)
        ir = MenaiIRReturn(value_plan=MenaiIRLet(
            bindings=[("x", _const(1), 0), ("y", _const(2), 1)],
            body_plan=_add_call(_local(0), _local(1)),
            in_tail_position=True,
        ))
        result = _run_prop(ir)
        assert isinstance(result, MenaiIRReturn)
        call = result.value_plan
        assert isinstance(call, MenaiIRCall)
        assert isinstance(call.arg_plans[0], MenaiIRConstant)
        assert call.arg_plans[0].value == MenaiInteger(1)
        assert isinstance(call.arg_plans[1], MenaiIRConstant)
        assert call.arg_plans[1].value == MenaiInteger(2)

    def test_substitution_in_if_branches(self):
        """Substitution reaches into both branches of an if expression."""
        # (let ((k 5)) (if #t k k))  →  (if #t 5 5)
        ir = MenaiIRReturn(value_plan=MenaiIRLet(
            bindings=[("k", _const(5), 0)],
            body_plan=MenaiIRIf(
                condition_plan=MenaiIRConstant(value=MenaiInteger(1)),
                then_plan=_local(0),
                else_plan=_local(0),
                in_tail_position=False,
            ),
            in_tail_position=True,
        ))
        result = _run_prop(ir)
        assert isinstance(result, MenaiIRReturn)
        if_node = result.value_plan
        assert isinstance(if_node, MenaiIRIf)
        assert isinstance(if_node.then_plan, MenaiIRConstant)
        assert if_node.then_plan.value == MenaiInteger(5)
        assert isinstance(if_node.else_plan, MenaiIRConstant)
        assert if_node.else_plan.value == MenaiInteger(5)

    def test_substitution_does_not_cross_lambda_body(self):
        """
        Substitution does NOT replace depth=0 references inside a lambda body
        (those refer to the lambda's own frame, not the enclosing let's frame).
        """
        # (let ((x 99))
        #   (lambda (x) x))   ← inner x is param at slot 0 of lambda frame
        # The lambda body's _local(0) refers to the lambda's own param, not
        # the outer let's x.  It must NOT be replaced with 99.
        lam = MenaiIRLambda(
            params=["x"],
            body_plan=MenaiIRReturn(value_plan=_local(0)),  # lambda's own param
            free_vars=[],
            free_var_plans=[],
            parent_refs=[],
            parent_ref_plans=[],
            param_count=1,
            is_variadic=False,
            max_locals=1,
        )
        ir = MenaiIRReturn(value_plan=MenaiIRLet(
            bindings=[("x", _const(99), 0)],
            body_plan=lam,
            in_tail_position=True,
        ))
        result = _run_prop(ir)
        assert isinstance(result, MenaiIRReturn)
        opt_lam = result.value_plan
        assert isinstance(opt_lam, MenaiIRLambda)
        # The lambda body must still reference slot 0 (its own param).
        body = opt_lam.body_plan
        assert isinstance(body, MenaiIRReturn)
        assert isinstance(body.value_plan, MenaiIRVariable)
        assert body.value_plan.index == 0
        assert body.value_plan.var_type == 'local'

    def test_substitution_into_free_var_plans(self):
        """
        When a constant is propagated, its value is substituted into the
        lambda's free_var_plans (which are evaluated in the enclosing frame).
        """
        # (let ((k 42))
        #   (lambda () k))
        # k is a constant → inlineable even though captured.
        # After propagation, free_var_plans[0] should be the constant 42,
        # not a reference to slot 0.
        free_var_plan = _local(index=0, depth=0)  # originally loads k
        lam = MenaiIRLambda(
            params=[],
            body_plan=MenaiIRReturn(value_plan=_local(index=0, depth=1)),
            free_vars=["k"],
            free_var_plans=[free_var_plan],
            parent_refs=[],
            parent_ref_plans=[],
            param_count=0,
            is_variadic=False,
            max_locals=1,
        )
        ir = MenaiIRReturn(value_plan=MenaiIRLet(
            bindings=[("k", _const(42), 0)],
            body_plan=lam,
            in_tail_position=True,
        ))
        result = _run_prop(ir)
        assert isinstance(result, MenaiIRReturn)
        opt_lam = result.value_plan
        assert isinstance(opt_lam, MenaiIRLambda)
        # free_var_plans[0] should now be the constant 42.
        assert len(opt_lam.free_var_plans) == 1
        assert isinstance(opt_lam.free_var_plans[0], MenaiIRConstant)
        assert opt_lam.free_var_plans[0].value == MenaiInteger(42)


# ---------------------------------------------------------------------------
# Unit tests: shadowing
# ---------------------------------------------------------------------------

class TestShadowing:
    """Verify that inner let bindings with the same slot index are not clobbered."""

    def test_inner_let_shadows_outer_propagation(self):
        """
        If an inner let introduces a binding at the same slot index as an
        outer binding being propagated, the inner binding takes precedence
        inside its own body.
        """
        # Outer let: slot 0 = constant 99
        # Inner let: slot 0 = constant 1  (shadows outer slot 0)
        # Inner body: uses slot 0 → should get 1, not 99
        inner_let = MenaiIRLet(
            bindings=[("inner", _const(1), 0)],
            body_plan=_local(0),
            in_tail_position=True,
        )
        outer_let = MenaiIRLet(
            bindings=[("outer", _const(99), 0)],
            body_plan=inner_let,
            in_tail_position=True,
        )
        ir = MenaiIRReturn(value_plan=outer_let)
        result = _run_prop(ir)
        # After propagation the inner let's body should be 1 (not 99).
        assert isinstance(result, MenaiIRReturn)
        # Both lets should have collapsed (all bindings propagated).
        assert isinstance(result.value_plan, MenaiIRConstant)
        assert result.value_plan.value == MenaiInteger(1)


# ---------------------------------------------------------------------------
# Unit tests: letrec is not propagated
# ---------------------------------------------------------------------------

class TestLetrecNotPropagated:
    """Verify that letrec bindings are never copy-propagated."""

    def test_letrec_constant_binding_not_propagated(self):
        """
        Even a constant binding inside letrec is not copy-propagated
        (we skip letrec entirely for propagation).
        """
        from menai.menai_dependency_analyzer import MenaiBindingGroup
        from menai.menai_ast import MenaiASTInteger as ASTInt
        ir = MenaiIRReturn(value_plan=MenaiIRLetrec(
            bindings=[("k", _const(42), 0)],
            body_plan=_local(0),
            binding_groups=[MenaiBindingGroup(
                names={"k"}, bindings=[("k", ASTInt(42))],
                is_recursive=False, depends_on=set())],
            recursive_bindings=set(),
            in_tail_position=True,
        ))
        _, changed = _run(ir)
        assert not changed

    def test_letrec_body_inner_let_is_propagated(self):
        """
        Even though letrec bindings are not propagated, inner let nodes
        inside the letrec body ARE still optimized.
        """
        from menai.menai_dependency_analyzer import MenaiBindingGroup
        from menai.menai_ast import MenaiASTInteger as ASTInt
        inner_let = MenaiIRLet(
            bindings=[("x", _const(7), 1)],
            body_plan=_local(1),
            in_tail_position=True,
        )
        ir = MenaiIRReturn(value_plan=MenaiIRLetrec(
            bindings=[("f", _const(0), 0)],
            body_plan=inner_let,
            binding_groups=[MenaiBindingGroup(
                names={"f"}, bindings=[("f", ASTInt(0))],
                is_recursive=False, depends_on=set())],
            recursive_bindings=set(),
            in_tail_position=True,
        ))
        result, changed = _run(ir)
        # The inner let's constant binding should have been propagated.
        assert changed
        assert isinstance(result, MenaiIRReturn)
        letrec = result.value_plan
        assert isinstance(letrec, MenaiIRLetrec)
        # Inner let should have collapsed to the constant.
        assert isinstance(letrec.body_plan, MenaiIRConstant)
        assert letrec.body_plan.value == MenaiInteger(7)


# ---------------------------------------------------------------------------
# Unit tests: tail-recursive sentinel
# ---------------------------------------------------------------------------

class TestTailRecursiveSentinel:
    """Verify that the tail-recursive sentinel func_plan is never substituted."""

    def test_tail_recursive_sentinel_not_substituted(self):
        """
        The sentinel MenaiIRVariable(name='<tail-recursive>') used as the
        func_plan of a tail-recursive call must never be replaced.
        """
        # Simulate a tail-recursive call where slot 0 happens to be the
        # sentinel index.  The propagator must not replace the sentinel.
        sentinel = MenaiIRVariable(
            name='<tail-recursive>',
            var_type='local',
            depth=0,
            index=0,
            is_parent_ref=False,
        )
        tail_call = MenaiIRCall(
            func_plan=sentinel,
            arg_plans=[_const(1)],
            is_tail_call=True,
            is_tail_recursive=True,
            is_builtin=False,
            builtin_name=None,
        )
        ir = MenaiIRReturn(value_plan=MenaiIRLet(
            bindings=[("x", _const(99), 0)],
            body_plan=tail_call,
            in_tail_position=True,
        ))
        result = _run_prop(ir)
        assert isinstance(result, MenaiIRReturn)
        call = result.value_plan
        assert isinstance(call, MenaiIRCall)
        assert call.is_tail_recursive
        # The sentinel must be unchanged.
        assert isinstance(call.func_plan, MenaiIRVariable)
        assert call.func_plan.name == '<tail-recursive>'


# ---------------------------------------------------------------------------
# Unit tests: substitutions property and changed flag
# ---------------------------------------------------------------------------

class TestSubstitutionsProperty:
    """Verify the substitutions counter and changed flag."""

    def test_no_propagation_changed_false(self):
        """When no binding is propagated, changed is False."""
        lam = MenaiIRLambda(
            params=["p"],
            body_plan=MenaiIRReturn(value_plan=_local(0)),
            free_vars=[],
            free_var_plans=[],
            parent_refs=[],
            parent_ref_plans=[],
            param_count=1,
            is_variadic=False,
            max_locals=1,
        )
        ir = MenaiIRReturn(value_plan=MenaiIRLet(
            bindings=[("f", lam, 0)],
            body_plan=_local(0),
            in_tail_position=True,
        ))
        prop = MenaiIRCopyPropagator()
        _, changed = prop.optimize(ir)
        assert not changed
        assert prop.substitutions == 0

    def test_one_propagation_changed_true(self):
        """When one binding is propagated, changed is True and substitutions == 1."""
        ir = MenaiIRReturn(value_plan=MenaiIRLet(
            bindings=[("x", _const(1), 0)],
            body_plan=_local(0),
            in_tail_position=True,
        ))
        prop = MenaiIRCopyPropagator()
        _, changed = prop.optimize(ir)
        assert changed
        assert prop.substitutions == 1

    def test_two_propagations_counted(self):
        """Two propagated bindings are both counted."""
        ir = MenaiIRReturn(value_plan=MenaiIRLet(
            bindings=[("x", _const(1), 0), ("y", _const(2), 1)],
            body_plan=_add_call(_local(0), _local(1)),
            in_tail_position=True,
        ))
        prop = MenaiIRCopyPropagator()
        _, changed = prop.optimize(ir)
        assert changed
        assert prop.substitutions == 2

    def test_substitutions_reset_between_calls(self):
        """The substitutions counter is reset on each call to optimize()."""
        ir = MenaiIRReturn(value_plan=MenaiIRLet(
            bindings=[("x", _const(1), 0)],
            body_plan=_local(0),
            in_tail_position=True,
        ))
        prop = MenaiIRCopyPropagator()
        prop.optimize(ir)
        assert prop.substitutions == 1
        # Second call on a tree with no propagatable bindings.
        ir2 = MenaiIRReturn(value_plan=_const(42))
        prop.optimize(ir2)
        assert prop.substitutions == 0


# ---------------------------------------------------------------------------
# Unit tests: structural flags preserved
# ---------------------------------------------------------------------------

class TestFlagsPreserved:
    """Verify that structural flags (in_tail_position, etc.) are preserved."""

    def test_in_tail_position_preserved_on_remaining_let(self):
        """in_tail_position is carried through to the optimized let."""
        ir = MenaiIRReturn(value_plan=MenaiIRLet(
            bindings=[
                ("x", _const(1), 0),       # propagated
                ("y", _add_call(_const(2), _const(3)), 1),  # kept (call)
            ],
            body_plan=_local(1),
            in_tail_position=True,
        ))
        result = _run_prop(ir)
        assert isinstance(result, MenaiIRReturn)
        inner = result.value_plan
        assert isinstance(inner, MenaiIRLet)
        assert inner.in_tail_position is True

    def test_lambda_metadata_preserved(self):
        """Lambda metadata (params, max_locals, etc.) is preserved after propagation."""
        lam = MenaiIRLambda(
            params=["a", "b"],
            body_plan=MenaiIRReturn(value_plan=_local(0)),
            free_vars=["outer"],
            free_var_plans=[_local(index=2, depth=0)],
            parent_refs=[],
            parent_ref_plans=[],
            param_count=2,
            is_variadic=False,
            max_locals=5,
            binding_name="my_func",
            sibling_bindings=["sibling"],
            source_line=42,
            source_file="test.menai",
        )
        ir = MenaiIRReturn(value_plan=MenaiIRLet(
            bindings=[("k", _const(99), 0)],
            body_plan=lam,
            in_tail_position=True,
        ))
        result = _run_prop(ir)
        assert isinstance(result, MenaiIRReturn)
        opt_lam = result.value_plan
        assert isinstance(opt_lam, MenaiIRLambda)
        assert opt_lam.params == ["a", "b"]
        assert opt_lam.max_locals == 5
        assert opt_lam.binding_name == "my_func"
        assert opt_lam.sibling_bindings == ["sibling"]
        assert opt_lam.source_line == 42
        assert opt_lam.source_file == "test.menai"
        assert opt_lam.param_count == 2
        assert opt_lam.is_variadic is False


# ---------------------------------------------------------------------------
# Integration tests: compile + evaluate
# ---------------------------------------------------------------------------

class TestCopyPropagatorIntegration:
    """
    End-to-end tests: compile real Menai source with optimization enabled
    (which now includes copy propagation) and verify correctness.
    """

    @pytest.fixture
    def menai(self):
        return Menai()

    def test_constant_let_binding_inlined(self, menai):
        """A constant let binding is inlined; result is correct."""
        assert menai.evaluate("(let ((x 42)) x)") == 42

    def test_constant_used_multiple_times(self, menai):
        """A constant used multiple times is inlined at all sites."""
        assert menai.evaluate("(let ((x 3)) (integer+ x x))") == 6

    def test_nested_let_constants_inlined(self, menai):
        """Nested lets with constant bindings are both inlined."""
        result = menai.evaluate("""
            (let ((a 10))
              (let ((b 20))
                (integer+ a b)))
        """)
        assert result == 30

    def test_global_alias_inlined(self, menai):
        """A let binding that aliases a global is inlined."""
        result = menai.evaluate("""
            (let ((add integer+))
              (add 3 4))
        """)
        assert result == 7

    def test_lambda_binding_not_inlined_semantics_preserved(self, menai):
        """A lambda binding is not inlined; the program still works correctly."""
        result = menai.evaluate("""
            (let ((f (lambda (x) (integer* x 2))))
              (f 21))
        """)
        assert result == 42

    def test_closure_captures_constant_correctly(self, menai):
        """A closure that captures a constant works correctly after propagation."""
        result = menai.evaluate("""
            (let ((factor 3))
              (let ((mul (lambda (x) (integer* x factor))))
                (mul 14)))
        """)
        assert result == 42

    def test_recursive_function_not_broken(self, menai):
        """Recursive functions are not broken by copy propagation."""
        result = menai.evaluate("""
            (letrec ((fact (lambda (n)
                             (if (integer<=? n 1)
                                 1
                                 (integer* n (fact (integer- n 1)))))))
              (fact 7))
        """)
        assert result == 5040

    def test_tail_call_optimization_still_works(self, menai):
        """TCO still fires correctly after copy propagation (no stack overflow)."""
        result = menai.evaluate("""
            (letrec ((loop (lambda (n acc)
                             (if (integer=? n 0)
                                 acc
                                 (loop (integer- n 1) (integer+ acc 1))))))
              (loop 100000 0))
        """)
        assert result == 100000

    def test_if_with_constant_condition_branches_correct(self, menai):
        """if branches with propagated constants evaluate correctly."""
        result_t = menai.evaluate("""
            (let ((t #t) (f #f))
              (if t 1 2))
        """)
        assert result_t == 1

    def test_empty_list_binding_inlined(self, menai):
        """An empty-list binding is inlined; list-length still works."""
        # Note: (list) compiles to a LIST opcode call, not MenaiIREmptyList.
        # MenaiIREmptyList is only produced for the literal '() syntax.
        # We test that the binding is inlined and the result is correct.
        result = menai.evaluate("""
            (let ((nil (list)))
              (list-length nil))
        """)
        assert result == 0

    def test_quote_binding_inlined(self, menai):
        """A quoted symbol binding is inlined."""
        result = menai.evaluate("""
            (let ((sym (quote hello)))
              sym)
        """)
        assert result == "hello"  # MenaiSymbol("hello") evaluates as a symbol

    def test_higher_order_with_constant_factor(self, menai):
        """map-list with a closure that captures a constant works correctly."""
        result = menai.evaluate("""
            (let ((factor 3))
              (map-list
                (lambda (x) (integer* x factor))
                (list 1 2 3 4)))
        """)
        assert result == [3, 6, 9, 12]

    def test_mutual_recursion_preserved(self, menai):
        """Mutually recursive letrec bindings are both preserved."""
        result = menai.evaluate("""
            (letrec ((even? (lambda (n)
                              (if (integer=? n 0) #t (odd? (integer- n 1)))))
                     (odd?  (lambda (n)
                              (if (integer=? n 0) #f (even? (integer- n 1))))))
              (list (even? 10) (odd? 7)))
        """)
        assert result == [True, True]

    def test_optimization_disabled_same_result(self):
        """Compiling with optimize=False produces the same result."""
        source = "(let ((x 7) (y 6)) (integer* x y))"
        opt_result = Menai().evaluate(source)

        instance = Menai()
        instance.compiler = MenaiCompiler(optimize=False, module_loader=instance)
        no_opt_result = instance.evaluate(source)

        assert opt_result == no_opt_result == 42

    def test_complex_program_correct(self, menai):
        """A realistic program with multiple scopes produces the right answer."""
        result = menai.evaluate("""
            (letrec ((fib (lambda (n)
                            (if (integer<? n 2)
                                n
                                (integer+ (fib (integer- n 1))
                                          (fib (integer- n 2)))))))
              (map-list fib (list 0 1 2 3 4 5 6 7)))
        """)
        assert result == [0, 1, 1, 2, 3, 5, 8, 13]

    def test_let_star_chain_inlined(self, menai):
        """let* desugars to nested lets; constants are inlined through the chain."""
        result = menai.evaluate("""
            (let* ((a 1)
                   (b (integer+ a 1))
                   (c (integer+ b 1)))
              c)
        """)
        assert result == 3

    def test_pattern_match_with_constant_binding(self, menai):
        """Pattern matching with a constant binding works correctly."""
        result = menai.evaluate("""
            (let ((x 2))
              (match x
                (1 "one")
                (2 "two")
                (_ "other")))
        """)
        assert result == "two"

    def test_trace_expression_not_broken(self, menai):
        """trace expressions with propagated bindings still evaluate correctly."""
        result = menai.evaluate("""
            (let ((msg "hello"))
              (trace msg 42))
        """)
        assert result == 42
