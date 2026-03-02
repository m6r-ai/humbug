"""
Tests for MenaiIRInlineOnce.

Strategy
--------
The tests are split into two layers:

1. Unit tests on the pass directly, by constructing IR nodes by hand.
   These are precise and fast — they don't go through the full compiler
   pipeline and are immune to unrelated pipeline changes.

2. Integration tests that compile real Menai source and verify that the
   optimized program still produces the correct result.  These catch
   regressions where the pass silently changes semantics.

Key distinctions from MenaiIRCopyPropagator tests
--------------------------------------------------
- The central case that copy propagation CANNOT handle but inline-once CAN:
  a single-use binding whose value is a call expression.
- There is no lambda boundary rule: name-based substitution is always safe
  across lambda boundaries because variables are symbolic until the addresser
  runs.
- Multi-use bindings (total_count > 1) are NOT inlined, regardless of value type.
- Dead bindings (total_count == 0) are left for MenaiIROptimizer.
- letrec bindings are never inlined.

Notes on constructing test IR for lambda capture tests
------------------------------------------------------
The use counter works by name.  A MenaiIRVariable(name='x', var_type='local',
depth=-1, index=-1) in the lambda body is resolved to the enclosing let's
binding of 'x' and counted as one use.  Tests that want exactly one use
therefore place exactly one _named("x") reference in the tree.
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
from menai.menai_ir_inline_once import MenaiIRInlineOnce
from menai.menai_ir_optimizer import MenaiIROptimizer
from menai.menai_value import MenaiBoolean, MenaiInteger, MenaiString, MenaiSymbol


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


def _named(name: str) -> MenaiIRVariable:
    """Symbolic local variable reference (pre-addresser form)."""
    return MenaiIRVariable(name=name, var_type='local', depth=-1, index=-1)


def _global(name: str) -> MenaiIRVariable:
    return MenaiIRVariable(name=name, var_type='global', depth=0, index=0)


def _add_call(a, b) -> MenaiIRCall:
    """Emit (integer+ a b) as a builtin call."""
    return MenaiIRCall(
        func_plan=_global('integer+'),
        arg_plans=[a, b],
        is_tail_call=False,
        is_builtin=True,
        builtin_name='integer+',
    )


def _mul_call(a, b) -> MenaiIRCall:
    """Emit (integer* a b) as a builtin call."""
    return MenaiIRCall(
        func_plan=_global('integer*'),
        arg_plans=[a, b],
        is_tail_call=False,
        is_builtin=True,
        builtin_name='integer*',
    )


def _run(ir):
    """Run the inline-once pass and return (new_ir, changed)."""
    return MenaiIRInlineOnce().optimize(ir)


def _run_inline(ir):
    """Run the inline-once pass and return just the new IR."""
    new_ir, _ = _run(ir)
    return new_ir


def _run_both(ir):
    """Run inline-once then dead-binding elimination, return final IR."""
    ir, _ = MenaiIRInlineOnce().optimize(ir)
    ir, _ = MenaiIROptimizer().optimize(ir)
    return ir


# ---------------------------------------------------------------------------
# Unit tests: single-use call binding — the key case copy propagation misses
# ---------------------------------------------------------------------------

class TestSingleUseCallInlined:
    """Verify that single-use call bindings are inlined (the key new case)."""

    def test_single_use_call_is_inlined(self):
        """
        A let binding whose value is a call expression is inlined when
        total_count == 1.  This is the case copy propagation cannot handle.

        (let ((r (integer+ 3 4))) r)  →  (integer+ 3 4)
        """
        call = _add_call(_const(3), _const(4))
        ir = MenaiIRReturn(value_plan=MenaiIRLet(
            bindings=[("r", call)],
            body_plan=_named("r"),
            in_tail_position=True,
        ))
        result, changed = _run(ir)
        assert changed
        assert isinstance(result, MenaiIRReturn)
        # The let should have collapsed — body is the call directly.
        inlined = result.value_plan
        assert isinstance(inlined, MenaiIRCall)
        assert inlined.builtin_name == 'integer+'

    def test_single_use_if_expression_is_inlined(self):
        """
        A let binding whose value is an if-expression is inlined when
        total_count == 1.
        """
        if_expr = MenaiIRIf(
            condition_plan=MenaiIRConstant(value=MenaiBoolean(True)),
            then_plan=_const(1),
            else_plan=_const(2),
            in_tail_position=False,
        )
        ir = MenaiIRReturn(value_plan=MenaiIRLet(
            bindings=[("v", if_expr)],
            body_plan=_named("v"),
            in_tail_position=True,
        ))
        result, changed = _run(ir)
        assert changed
        assert isinstance(result, MenaiIRReturn)
        assert isinstance(result.value_plan, MenaiIRIf)

    def test_single_use_constant_is_inlined(self):
        """
        A single-use constant binding is inlined.
        (Copy propagation would also catch this, but inline-once handles it
        independently when tested in isolation.)
        """
        ir = MenaiIRReturn(value_plan=MenaiIRLet(
            bindings=[("k", _const(99))],
            body_plan=_named("k"),
            in_tail_position=True,
        ))
        result, changed = _run(ir)
        assert changed
        assert isinstance(result, MenaiIRReturn)
        assert isinstance(result.value_plan, MenaiIRConstant)
        assert result.value_plan.value == MenaiInteger(99)

    def test_chained_single_use_calls_inlined_in_one_pass(self):
        """
        A let* chain of single-use call bindings is fully inlined in a single
        optimize() call.  The substitution walk recurses into inner lets via
        _substitute_let → _inline, so both bindings collapse together.

        (let ((b (integer+ 1 5)))
          (let ((c (integer* b 2)))
            c))

        b has total_count=1 → inlined.  During substitution the inner let is
        reconstructed and _inline is called on it, which finds c also has
        total_count=1 → inlined.  Both collapse in one optimize() call.
        """
        inner_let = MenaiIRLet(
            bindings=[("c", _mul_call(_named("b"), _const(2)))],
            body_plan=_named("c"),
            in_tail_position=True,
        )
        outer_let = MenaiIRLet(
            bindings=[("b", _add_call(_const(1), _const(5)))],
            body_plan=inner_let,
            in_tail_position=True,
        )
        ir = MenaiIRReturn(value_plan=outer_let)

        result, changed = _run(ir)
        assert changed
        assert isinstance(result, MenaiIRReturn)
        final = result.value_plan
        assert isinstance(final, MenaiIRCall)
        assert final.builtin_name == 'integer*'
        # The first argument to integer* should be the inlined (integer+ 1 5) call.
        inner_arg = final.arg_plans[0]
        assert isinstance(inner_arg, MenaiIRCall)
        assert inner_arg.builtin_name == 'integer+'


# ---------------------------------------------------------------------------
# Unit tests: multi-use bindings are NOT inlined
# ---------------------------------------------------------------------------

class TestMultiUseNotInlined:
    """Verify that bindings with total_count > 1 are never inlined."""

    def test_two_use_call_not_inlined(self):
        """
        A call binding used twice is NOT inlined (would duplicate work).

        (let ((r (integer+ 3 4))) (integer+ r r))
        """
        call = _add_call(_const(3), _const(4))
        ir = MenaiIRReturn(value_plan=MenaiIRLet(
            bindings=[("r", call)],
            body_plan=_add_call(_named("r"), _named("r")),
            in_tail_position=True,
        ))
        _, changed = _run(ir)
        assert not changed

    def test_two_use_constant_not_inlined_by_inline_once(self):
        """
        A constant binding used twice is not inlined by inline-once
        (total_count == 2).  Copy propagation would handle this case, but
        inline-once must not, since its contract is total_count == 1.
        """
        ir = MenaiIRReturn(value_plan=MenaiIRLet(
            bindings=[("k", _const(7))],
            body_plan=_add_call(_named("k"), _named("k")),
            in_tail_position=True,
        ))
        _, changed = _run(ir)
        assert not changed


# ---------------------------------------------------------------------------
# Unit tests: dead bindings are left alone
# ---------------------------------------------------------------------------

class TestDeadBindingNotTouched:
    """Verify that dead bindings (total_count == 0) are left for MenaiIROptimizer."""

    def test_dead_call_binding_not_inlined(self):
        """
        A binding with total_count == 0 is not touched by inline-once.
        MenaiIROptimizer is responsible for dead-binding elimination.
        """
        call = _add_call(_const(1), _const(2))
        ir = MenaiIRReturn(value_plan=MenaiIRLet(
            bindings=[("unused", call)],
            body_plan=_const(42),
            in_tail_position=True,
        ))
        _, changed = _run(ir)
        assert not changed


# ---------------------------------------------------------------------------
# Unit tests: lambda boundary rule
# ---------------------------------------------------------------------------

class TestLambdaBoundaryRule:
    """Verify that inlining works correctly across lambda boundaries."""

    def test_local_var_value_inlined_when_captured(self):
        """
        A binding whose value is a local variable reference IS inlined even
        when captured by a child lambda.  With symbolic (name-based) variables,
        there is no lambda boundary restriction.

        (let ((x other)) (lambda () x))
        x is used once via outer_free_var_plans.  Its value is a symbolic
        local var ref _named("other").  The binding should be inlined.
        """
        lam = MenaiIRLambda(
            params=[],
            body_plan=MenaiIRReturn(value_plan=_const(0)),  # body irrelevant here
            sibling_free_vars=[],
            sibling_free_var_plans=[],
            outer_free_vars=["x"],
            outer_free_var_plans=[_named("x")],  # one use of "x" in enclosing frame
            param_count=0,
            is_variadic=False,
        )
        ir = MenaiIRReturn(value_plan=MenaiIRLet(
            bindings=[("x", _named("other"))],  # value is a symbolic local var ref
            body_plan=lam,
            in_tail_position=True,
        ))
        result, changed = _run(ir)
        # Symbolic local var ref + single use → IS inlined (no lambda boundary rule).
        assert changed
        assert isinstance(result, MenaiIRReturn)
        opt_lam = result.value_plan
        assert isinstance(opt_lam, MenaiIRLambda)
        # The let collapsed; outer_free_var_plans now holds the inlined value.
        assert len(opt_lam.outer_free_var_plans) == 1
        assert isinstance(opt_lam.outer_free_var_plans[0], MenaiIRVariable)
        assert opt_lam.outer_free_var_plans[0].name == "other"

    def test_call_value_inlined_even_when_captured(self):
        """
        A binding whose value is a call expression IS inlined even when
        captured by a child lambda.  With symbolic variables there is no
        lambda boundary restriction.

        (let ((result (integer+ x 1))) (lambda () result))
        result is used once via outer_free_var_plans.  Its value is a call.
        The binding should be inlined into outer_free_var_plans.
        """
        x_ref = _named("x")
        call = _add_call(x_ref, _const(1))

        lam = MenaiIRLambda(
            params=[],
            body_plan=MenaiIRReturn(value_plan=_const(0)),  # body irrelevant here
            sibling_free_vars=[],
            sibling_free_var_plans=[],
            outer_free_vars=["result"],
            outer_free_var_plans=[_named("result")],  # one use of "result"
            param_count=0,
            is_variadic=False,
        )
        ir = MenaiIRReturn(value_plan=MenaiIRLet(
            bindings=[("result", call)],
            body_plan=lam,
            in_tail_position=True,
        ))
        result, changed = _run(ir)
        # Call value + single use → inlined.
        assert changed
        assert isinstance(result, MenaiIRReturn)
        opt_lam = result.value_plan
        assert isinstance(opt_lam, MenaiIRLambda)
        # The let collapsed; outer_free_var_plans now holds the inlined call.
        assert len(opt_lam.outer_free_var_plans) == 1
        assert isinstance(opt_lam.outer_free_var_plans[0], MenaiIRCall)
        assert opt_lam.outer_free_var_plans[0].builtin_name == 'integer+'

    def test_constant_value_inlined_when_captured(self):
        """
        A single-use constant binding is inlined even when captured.
        With symbolic variables there is no lambda boundary restriction.

        (let ((k 42)) (lambda () k))
        k is used once via outer_free_var_plans.  Its value is a constant.
        The binding should be inlined into outer_free_var_plans.
        """
        lam = MenaiIRLambda(
            params=[],
            body_plan=MenaiIRReturn(value_plan=_const(0)),  # body irrelevant here
            sibling_free_vars=[],
            sibling_free_var_plans=[],
            outer_free_vars=["k"],
            outer_free_var_plans=[_named("k")],  # one use of "k"
            param_count=0,
            is_variadic=False,
        )
        ir = MenaiIRReturn(value_plan=MenaiIRLet(
            bindings=[("k", _const(42))],
            body_plan=lam,
            in_tail_position=True,
        ))
        result, changed = _run(ir)
        # Constant + single use → inlined.
        assert changed
        assert isinstance(result, MenaiIRReturn)
        opt_lam = result.value_plan
        assert isinstance(opt_lam, MenaiIRLambda)
        # Let collapsed; outer_free_var_plans now holds the inlined constant.
        assert len(opt_lam.outer_free_var_plans) == 1
        assert isinstance(opt_lam.outer_free_var_plans[0], MenaiIRConstant)
        assert opt_lam.outer_free_var_plans[0].value == MenaiInteger(42)

    def test_local_var_value_inlined_when_not_captured(self):
        """
        A single-use local variable binding is inlined (no lambda capture
        involved — the use is directly in the let body).
        """
        # (let ((y other)) y)  where other is some other binding, body uses y once.
        ir = MenaiIRReturn(value_plan=MenaiIRLet(
            bindings=[("y", _named("other"))],
            body_plan=_named("y"),
            in_tail_position=True,
        ))
        result, changed = _run(ir)
        assert changed
        assert isinstance(result, MenaiIRReturn)
        # Body should now be _named("other") directly.
        assert isinstance(result.value_plan, MenaiIRVariable)
        assert result.value_plan.name == "other"

    def test_substitution_does_not_cross_lambda_body(self):
        """
        Substitution does NOT replace a lambda param reference inside the
        lambda body, even if the param name matches a pending substitution
        in the enclosing scope.

        (let ((r (integer+ 1 2)))   ; r has no uses → dead
          (lambda (p) p))           ; lambda param p shadows any outer "p"

        The lambda has no outer_free_var_plans referencing "r" in the enclosing
        frame, so total_count(frame=0, "r") = 0.  The binding is dead —
        inline-once does not touch it (left for MenaiIROptimizer).  The
        lambda body's _named("p") refers to the lambda's own param.
        """
        lam = MenaiIRLambda(
            params=["p"],
            body_plan=MenaiIRReturn(value_plan=_named("p")),  # lambda's own param
            sibling_free_vars=[], sibling_free_var_plans=[], outer_free_vars=[],
            outer_free_var_plans=[],
            param_count=1,
            is_variadic=False,
        )
        ir = MenaiIRReturn(value_plan=MenaiIRLet(
            bindings=[("r", _add_call(_const(1), _const(2)))],  # dead: total_count=0
            body_plan=lam,
            in_tail_position=True,
        ))
        result, changed = _run(ir)
        # total_count=0 → dead binding, not touched by inline-once.
        assert not changed
        assert isinstance(result, MenaiIRReturn)
        let_node = result.value_plan
        assert isinstance(let_node, MenaiIRLet)
        opt_lam = let_node.body_plan
        assert isinstance(opt_lam, MenaiIRLambda)
        # The lambda body must still reference "p" (its own param).
        body = opt_lam.body_plan
        assert isinstance(body, MenaiIRReturn)
        assert isinstance(body.value_plan, MenaiIRVariable)
        assert body.value_plan.name == "p"
        assert body.value_plan.var_type == 'local'


# ---------------------------------------------------------------------------
# Unit tests: letrec bindings are never inlined
# ---------------------------------------------------------------------------

class TestLetrecNotInlined:
    """Verify that letrec bindings are never inlined."""

    def test_letrec_single_use_binding_not_inlined(self):
        """
        Even a single-use constant binding inside letrec is not inlined.
        """
        ir = MenaiIRReturn(value_plan=MenaiIRLetrec(
            bindings=[("k", _const(42))],
            body_plan=_named("k"),
            in_tail_position=True,
        ))
        _, changed = _run(ir)
        assert not changed

    def test_letrec_single_use_call_not_inlined(self):
        """
        A single-use call binding inside letrec is not inlined.
        """
        ir = MenaiIRReturn(value_plan=MenaiIRLetrec(
            bindings=[("r", _add_call(_const(1), _const(2)))],
            body_plan=_named("r"),
            in_tail_position=True,
        ))
        _, changed = _run(ir)
        assert not changed

    def test_letrec_body_inner_let_is_inlined(self):
        """
        Even though letrec bindings are not inlined, inner let nodes inside
        the letrec body ARE still optimized.
        """
        inner_let = MenaiIRLet(
            bindings=[("r", _add_call(_const(3), _const(4)))],
            body_plan=_named("r"),
            in_tail_position=True,
        )
        ir = MenaiIRReturn(value_plan=MenaiIRLetrec(
            bindings=[("f", _const(0))],
            body_plan=inner_let,
            in_tail_position=True,
        ))
        result, changed = _run(ir)
        assert changed
        assert isinstance(result, MenaiIRReturn)
        letrec = result.value_plan
        assert isinstance(letrec, MenaiIRLetrec)
        # Inner let should have collapsed to the inlined call.
        assert isinstance(letrec.body_plan, MenaiIRCall)
        assert letrec.body_plan.builtin_name == 'integer+'


# ---------------------------------------------------------------------------
# Unit tests: tail-recursive sentinel
# ---------------------------------------------------------------------------

class TestTailRecursiveSentinel:
    """Verify that single-use bindings in tail call args are inlined correctly."""

    def test_tail_call_arg_inlined(self):
        """
        A single-use let binding used as an argument to a tail call is inlined.
        The func_plan (a real variable reference) is also walked and substituted
        normally.
        """
        inner_call = _add_call(_const(1), _const(2))  # value for slot 1
        tail_call = MenaiIRCall(
            func_plan=_named("f"),               # callee reference by name
            arg_plans=[_named("x")],             # uses "x" once
            is_tail_call=True,
            is_builtin=False,
            builtin_name=None,
        )
        ir = MenaiIRReturn(value_plan=MenaiIRLet(
            bindings=[("x", inner_call)],        # single-use
            body_plan=tail_call,
            in_tail_position=True,
        ))
        result = _run_inline(ir)
        assert isinstance(result, MenaiIRReturn)
        # "x" inlined: let collapsed, body is the tail_call with "x" replaced.
        call = result.value_plan
        assert isinstance(call, MenaiIRCall)
        assert call.is_tail_call
        # The arg should now be the inlined call expression.
        assert isinstance(call.arg_plans[0], MenaiIRCall)
        assert call.arg_plans[0].builtin_name == 'integer+'


# ---------------------------------------------------------------------------
# Unit tests: shadowing
# ---------------------------------------------------------------------------

class TestShadowing:
    """Verify that inner let bindings with the same name shadow outer substitutions."""

    def test_inner_let_shadows_outer_inlining(self):
        """
        If an inner let introduces a binding with the same name as an outer
        binding being inlined, the inner binding shadows the outer substitution
        inside the inner body.

        Outer: "x" = (integer+ 3 4)  — used once in inner binding value
        Inner: "x" = (integer+ x 0)  — uses outer "x" in value, inner "x" in body
        Inner body: _named("x") → should get inner (integer+ x 0) value,
                    not outer (integer+ 3 4).

        After inlining outer "x" into inner value: inner value = (integer+ (integer+ 3 4) 0)
        After inlining inner "x" into inner body: result = (integer+ (integer+ 3 4) 0)
        """
        inner_let = MenaiIRLet(
            bindings=[("x", _add_call(_named("x"), _const(0)))],  # value uses outer "x"
            body_plan=_named("x"),                                 # body uses inner "x"
            in_tail_position=True,
        )
        outer_let = MenaiIRLet(
            bindings=[("x", _add_call(_const(3), _const(4)))],    # outer "x"
            body_plan=inner_let,
            in_tail_position=True,
        )
        ir = MenaiIRReturn(value_plan=outer_let)
        result, changed = _run(ir)
        assert changed
        assert isinstance(result, MenaiIRReturn)
        # Both lets collapsed; the final value is the inlined inner call.
        final = result.value_plan
        assert isinstance(final, MenaiIRCall)
        assert final.builtin_name == 'integer+'
        # First arg is the inlined outer "x" = (integer+ 3 4).
        assert isinstance(final.arg_plans[0], MenaiIRCall)
        assert final.arg_plans[0].builtin_name == 'integer+'
        assert isinstance(final.arg_plans[0].arg_plans[0], MenaiIRConstant)
        assert final.arg_plans[0].arg_plans[0].value == MenaiInteger(3)
        # Second arg is the constant 0.
        assert isinstance(final.arg_plans[1], MenaiIRConstant)
        assert final.arg_plans[1].value == MenaiInteger(0)


# ---------------------------------------------------------------------------
# Unit tests: inlinings property and changed flag
# ---------------------------------------------------------------------------

class TestInliningsProperty:
    """Verify the inlinings counter and changed flag."""

    def test_no_inlining_changed_false(self):
        """When no binding is inlined, changed is False."""
        # Two-use binding — not eligible.
        ir = MenaiIRReturn(value_plan=MenaiIRLet(
            bindings=[("r", _add_call(_const(1), _const(2)))],
            body_plan=_add_call(_named("r"), _named("r")),
            in_tail_position=True,
        ))
        inliner = MenaiIRInlineOnce()
        _, changed = inliner.optimize(ir)
        assert not changed
        assert inliner.inlinings == 0

    def test_one_inlining_changed_true(self):
        """When one binding is inlined, changed is True and inlinings == 1."""
        ir = MenaiIRReturn(value_plan=MenaiIRLet(
            bindings=[("r", _add_call(_const(1), _const(2)))],
            body_plan=_named("r"),
            in_tail_position=True,
        ))
        inliner = MenaiIRInlineOnce()
        _, changed = inliner.optimize(ir)
        assert changed
        assert inliner.inlinings == 1

    def test_inlinings_reset_between_calls(self):
        """The inlinings counter is reset on each call to optimize()."""
        ir = MenaiIRReturn(value_plan=MenaiIRLet(
            bindings=[("r", _add_call(_const(1), _const(2)))],
            body_plan=_named("r"),
            in_tail_position=True,
        ))
        inliner = MenaiIRInlineOnce()
        inliner.optimize(ir)
        assert inliner.inlinings == 1
        # Second call on a tree with no inlinable bindings.
        ir2 = MenaiIRReturn(value_plan=_const(42))
        inliner.optimize(ir2)
        assert inliner.inlinings == 0


# ---------------------------------------------------------------------------
# Unit tests: structural flags and metadata preserved
# ---------------------------------------------------------------------------

class TestFlagsPreserved:
    """Verify that structural flags and lambda metadata are preserved."""

    def test_in_tail_position_preserved_on_remaining_let(self):
        """in_tail_position is carried through to the optimized let."""
        # "r": single-use call (inlined), "s": two-use (kept).
        ir = MenaiIRReturn(value_plan=MenaiIRLet(
            bindings=[
                ("r", _add_call(_const(1), _const(2))),  # inlined (single-use)
                ("s", _const(5)),                         # kept (two uses in body)
            ],
            body_plan=_add_call(_named("s"), _named("s")),
            in_tail_position=True,
        ))
        result = _run_inline(ir)
        assert isinstance(result, MenaiIRReturn)
        inner = result.value_plan
        assert isinstance(inner, MenaiIRLet)
        assert inner.in_tail_position is True

    def test_lambda_metadata_preserved(self):
        """Lambda metadata (params, max_locals, etc.) is preserved after inlining."""
        # The lambda has no outer_free_var_plans referencing "r" in the enclosing
        # frame, so the binding is dead (total_count=0) — not inlined, let kept.
        # We verify the lambda's metadata passes through the optimization walk
        # unchanged.
        lam = MenaiIRLambda(
            params=["a", "b"],
            body_plan=MenaiIRReturn(value_plan=_named("a")),
            sibling_free_vars=["sibling"],
            sibling_free_var_plans=[],
            outer_free_vars=[],
            outer_free_var_plans=[],
            param_count=2,
            is_variadic=False,
            max_locals=0,
            binding_name="my_func",
            source_line=42,
            source_file="test.menai",
        )
        ir = MenaiIRReturn(value_plan=MenaiIRLet(
            bindings=[("r", _add_call(_const(1), _const(2)))],  # dead: total_count=0
            body_plan=lam,
            in_tail_position=True,
        ))
        result = _run_inline(ir)
        assert isinstance(result, MenaiIRReturn)
        # Binding is dead — let is preserved.
        let_node = result.value_plan
        assert isinstance(let_node, MenaiIRLet)
        opt_lam = let_node.body_plan
        assert isinstance(opt_lam, MenaiIRLambda)
        assert opt_lam.params == ["a", "b"]
        assert opt_lam.max_locals == 0
        assert opt_lam.binding_name == "my_func"
        assert opt_lam.sibling_free_vars == ["sibling"]
        assert opt_lam.source_line == 42
        assert opt_lam.source_file == "test.menai"
        assert opt_lam.param_count == 2
        assert opt_lam.is_variadic is False


# ---------------------------------------------------------------------------
# Integration tests: compile + evaluate
# ---------------------------------------------------------------------------

class TestInlineOnceIntegration:
    """
    End-to-end tests: compile real Menai source with optimization enabled
    and verify correctness.  These confirm that inline-once does not change
    program semantics.
    """

    @pytest.fixture
    def menai(self):
        return Menai()

    def test_let_star_chain_with_computed_values(self, menai):
        """
        let* chain with computed values is fully inlined and produces the
        correct result.

        (let* ((a 1) (b (integer+ a 5)) (c (integer* b 2))) c)
        a=1 → copy propagation inlines a.
        b=(integer+ 1 5) → inline-once inlines b (single-use call).
        c=(integer* 6 2) → inline-once inlines c (single-use call).
        Result: 12.
        """
        result = menai.evaluate("""
            (let* ((a 1)
                   (b (integer+ a 5))
                   (c (integer* b 2)))
              c)
        """)
        assert result == 12

    def test_let_star_chain_longer(self, menai):
        """Longer let* chain with mixed constants and calls."""
        result = menai.evaluate("""
            (let* ((a 2)
                   (b (integer+ a 3))
                   (c (integer* b b))
                   (d (integer- c 1)))
              d)
        """)
        assert result == 24  # a=2, b=5, c=25, d=24

    def test_match_with_non_constant_scrutinee(self, menai):
        """
        match with a non-constant scrutinee desugars to a single-use temp
        let binding.  inline-once should inline it correctly.
        """
        result = menai.evaluate("""
            (let ((x 3))
              (match (integer+ x 1)
                (1 "one")
                (2 "two")
                (4 "four")
                (_ "other")))
        """)
        assert result == "four"

    def test_match_with_computed_scrutinee_various(self, menai):
        """match with computed scrutinee — several arms."""
        result = menai.evaluate("""
            (letrec ((classify (lambda (n)
                                 (match (integer% n 3)
                                   (0 "div3")
                                   (1 "rem1")
                                   (_ "rem2")))))
              (list (classify 9) (classify 10) (classify 11)))
        """)
        assert result == ["div3", "rem1", "rem2"]

    def test_recursive_function_not_broken(self, menai):
        """Recursive functions are not broken by inline-once."""
        result = menai.evaluate("""
            (letrec ((fact (lambda (n)
                             (if (integer<=? n 1)
                                 1
                                 (integer* n (fact (integer- n 1)))))))
              (fact 8))
        """)
        assert result == 40320

    def test_tail_call_optimization_still_works(self, menai):
        """TCO still fires correctly after inline-once (no stack overflow)."""
        result = menai.evaluate("""
            (letrec ((loop (lambda (n acc)
                             (if (integer=? n 0)
                                 acc
                                 (loop (integer- n 1) (integer+ acc 1))))))
              (loop 100000 0))
        """)
        assert result == 100000

    def test_closure_with_computed_capture_not_broken(self, menai):
        """
        A closure that captures a computed (call) expression works correctly.
        This exercises the key inline-once path: the capture is inlined into
        free_var_plans.
        """
        result = menai.evaluate("""
            (let ((x 10))
              (let ((make-adder (lambda (n)
                                  (lambda (v) (integer+ v n)))))
                (let ((add-x (make-adder x)))
                  (add-x 32))))
        """)
        assert result == 42

    def test_higher_order_map_still_works(self, menai):
        """map-list with a closure works correctly after inline-once."""
        result = menai.evaluate("""
            (let ((factor 3))
              (map-list
                (lambda (x) (integer* x factor))
                (list 1 2 3 4 5)))
        """)
        assert result == [3, 6, 9, 12, 15]

    def test_mutual_recursion_preserved(self, menai):
        """Mutually recursive letrec bindings are not broken."""
        result = menai.evaluate("""
            (letrec ((even? (lambda (n)
                              (if (integer=? n 0) #t (odd? (integer- n 1)))))
                     (odd?  (lambda (n)
                              (if (integer=? n 0) #f (even? (integer- n 1))))))
              (list (even? 10) (odd? 7)))
        """)
        assert result == [True, True]

    def test_comparison_chain_desugared_temps_inlined(self, menai):
        """
        Variadic comparisons desugar to temp-var let* chains.  inline-once
        handles the single-use temp bindings.
        """
        result = menai.evaluate("""
            (let ((a 1) (b 2) (c 3))
              (if (integer<? a b c)
                  "ascending"
                  "not ascending"))
        """)
        assert result == "ascending"

    def test_let_star_with_string_ops(self, menai):
        """let* chain with string operations produces the correct result."""
        result = menai.evaluate("""
            (let* ((s "hello")
                   (n (string-length s))
                   (doubled (integer* n 2)))
              doubled)
        """)
        assert result == 10

    def test_nested_closures_not_broken(self, menai):
        """Nested closures work correctly after inline-once."""
        result = menai.evaluate("""
            (letrec ((make-counter
                       (lambda (start)
                         (lambda () start))))
              (let ((c1 (make-counter 10))
                    (c2 (make-counter 20)))
                (list (c1) (c2))))
        """)
        assert result == [10, 20]

    def test_fib_correct(self, menai):
        """Fibonacci is correct after inline-once."""
        result = menai.evaluate("""
            (letrec ((fib (lambda (n)
                            (if (integer<? n 2)
                                n
                                (integer+ (fib (integer- n 1))
                                          (fib (integer- n 2)))))))
              (map-list fib (list 0 1 2 3 4 5 6 7 8 9 10)))
        """)
        assert result == [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55]

    def test_optimize_false_same_result(self):
        """Compiling with optimize=False produces the same result."""
        source = """
            (let* ((a 3)
                   (b (integer+ a 4))
                   (c (integer* b 2)))
              c)
        """
        opt_result = Menai().evaluate(source)

        instance = Menai()
        instance.compiler = MenaiCompiler(optimize=False, module_loader=instance)
        no_opt_result = instance.evaluate(source)

        assert opt_result == no_opt_result == 14

    def test_optimize_false_match_same_result(self):
        """optimize=False and optimize=True agree on a match expression."""
        source = """
            (let ((x 5))
              (match (integer+ x 1)
                (6 "six")
                (_ "other")))
        """
        opt_result = Menai().evaluate(source)

        instance = Menai()
        instance.compiler = MenaiCompiler(optimize=False, module_loader=instance)
        no_opt_result = instance.evaluate(source)

        assert opt_result == no_opt_result == "six"

    def test_trace_expression_not_broken(self, menai):
        """trace expressions with inlined bindings still evaluate correctly."""
        result = menai.evaluate("""
            (let ((r (integer+ 20 22)))
              (trace "result" r))
        """)
        assert result == 42
