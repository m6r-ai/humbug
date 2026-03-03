"""
Tests for MenaiIRLambdaLifter.

Strategy
--------
The tests are split into two layers:

1. Unit tests on the lifter directly, by constructing IR nodes by hand.
   These are precise and fast — they verify the structural output of the
   transformation without going through the full compiler pipeline.

2. Integration tests that compile real Menai source and verify that the
   optimized program still produces the correct result.  These catch
   regressions where the lifter silently changes semantics.

Key structural properties under test
-------------------------------------
Standalone capturing lambda (inside a let or plain expression):
  - Produces (let ((helper ...)) wrapper).
  - Helper has no free vars; arity = original params + captured names.
  - Wrapper has original arity; body is a single tail-call to helper.
  - Already-closed lambdas are returned structurally unchanged.

Letrec group:
  - All helpers are hoisted into a single enclosing let (not buried inside
    individual binding values).
  - The letrec retains only the wrappers.
  - Already-closed letrec bindings stay in the letrec unchanged.
  - A letrec where no binding captures anything is returned as a plain
    letrec with no enclosing let.
  - Mixed letrec (some capturing, some closed) hoists only the helpers for
    the capturing bindings.
"""

from __future__ import annotations

import pytest

from menai import Menai
from menai.menai_ir import (
    MenaiIRCall,
    MenaiIRConstant,
    MenaiIRLambda,
    MenaiIRLet,
    MenaiIRLetrec,
    MenaiIRReturn,
    MenaiIRVariable,
)
from menai.menai_ir_lambda_lifter import MenaiIRLambdaLifter
from menai.menai_value import MenaiInteger


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _const(n: int) -> MenaiIRConstant:
    return MenaiIRConstant(value=MenaiInteger(n))


def _local(name: str) -> MenaiIRVariable:
    """Symbolic local variable reference (pre-addresser form)."""
    return MenaiIRVariable(name=name, var_type='local', depth=-1, index=-1)


def _closed_lambda(name: str, params: list[str]) -> MenaiIRLambda:
    """A fully-closed lambda with no free vars."""
    return MenaiIRLambda(
        params=params,
        body_plan=MenaiIRReturn(value_plan=_const(0)),
        sibling_free_vars=[],
        sibling_free_var_plans=[],
        outer_free_vars=[],
        outer_free_var_plans=[],
        param_count=len(params),
        is_variadic=False,
        binding_name=name,
    )


def _capturing_lambda(
    name: str,
    params: list[str],
    outer_free_vars: list[str],
    sibling_free_vars: list[str] | None = None,
) -> MenaiIRLambda:
    """A capturing lambda with the given outer and/or sibling free vars."""
    sfv = sibling_free_vars or []
    return MenaiIRLambda(
        params=params,
        body_plan=MenaiIRReturn(value_plan=_const(0)),
        sibling_free_vars=sfv,
        sibling_free_var_plans=[_local(v) for v in sfv],
        outer_free_vars=outer_free_vars,
        outer_free_var_plans=[_local(v) for v in outer_free_vars],
        param_count=len(params),
        is_variadic=False,
        binding_name=name,
    )


def _lift(ir):
    """Run the lifter and return the new IR."""
    return MenaiIRLambdaLifter().lift(ir)


# ---------------------------------------------------------------------------
# Unit tests: standalone capturing lambda
# ---------------------------------------------------------------------------

class TestStandaloneCapturingLambda:
    """Verify the standalone (non-letrec) worker/wrapper split."""

    def test_capturing_lambda_produces_let_wrapper(self):
        """
        A capturing lambda is replaced by (let ((helper ...)) wrapper).
        The result at the top level is a MenaiIRLet.
        """
        lam = _capturing_lambda('f', ['x'], outer_free_vars=['captured'])
        result = _lift(lam)
        assert isinstance(result, MenaiIRLet)

    def test_helper_is_fully_closed(self):
        """The helper lambda has no free vars."""
        lam = _capturing_lambda('f', ['x'], outer_free_vars=['captured'])
        result = _lift(lam)
        assert isinstance(result, MenaiIRLet)
        _, helper = result.bindings[0]
        assert isinstance(helper, MenaiIRLambda)
        assert helper.sibling_free_vars == []
        assert helper.outer_free_vars == []

    def test_helper_arity_is_params_plus_captures(self):
        """Helper params = original params + all captured names."""
        lam = _capturing_lambda('f', ['x', 'y'], outer_free_vars=['a', 'b'])
        result = _lift(lam)
        assert isinstance(result, MenaiIRLet)
        _, helper = result.bindings[0]
        assert isinstance(helper, MenaiIRLambda)
        assert helper.params == ['x', 'y', 'a', 'b']
        assert helper.param_count == 4

    def test_wrapper_has_original_arity(self):
        """Wrapper lambda has the original params unchanged."""
        lam = _capturing_lambda('f', ['x', 'y'], outer_free_vars=['a', 'b'])
        result = _lift(lam)
        assert isinstance(result, MenaiIRLet)
        wrapper = result.body_plan
        assert isinstance(wrapper, MenaiIRLambda)
        assert wrapper.params == ['x', 'y']
        assert wrapper.param_count == 2

    def test_wrapper_body_is_tail_call_to_helper(self):
        """Wrapper body is a single tail-call to the helper."""
        lam = _capturing_lambda('f', ['x'], outer_free_vars=['captured'])
        result = _lift(lam)
        assert isinstance(result, MenaiIRLet)
        helper_name, _ = result.bindings[0]
        wrapper = result.body_plan
        assert isinstance(wrapper, MenaiIRLambda)
        body = wrapper.body_plan
        assert isinstance(body, MenaiIRReturn)
        call = body.value_plan
        assert isinstance(call, MenaiIRCall)
        assert call.is_tail_call
        assert isinstance(call.func_plan, MenaiIRVariable)
        assert call.func_plan.name == helper_name

    def test_wrapper_tail_call_passes_params_then_captures(self):
        """Wrapper tail-call args are original params followed by captured names."""
        lam = _capturing_lambda('f', ['x', 'y'], outer_free_vars=['a', 'b'])
        result = _lift(lam)
        assert isinstance(result, MenaiIRLet)
        wrapper = result.body_plan
        assert isinstance(wrapper, MenaiIRLambda)
        call = wrapper.body_plan.value_plan  # type: ignore[union-attr]
        assert isinstance(call, MenaiIRCall)
        arg_names = [a.name for a in call.arg_plans]  # type: ignore[union-attr]
        assert arg_names == ['x', 'y', 'a', 'b']

    def test_helper_captures_helper_as_outer_free_var(self):
        """Wrapper captures the helper via outer_free_vars."""
        lam = _capturing_lambda('f', ['x'], outer_free_vars=['captured'])
        result = _lift(lam)
        assert isinstance(result, MenaiIRLet)
        helper_name, _ = result.bindings[0]
        wrapper = result.body_plan
        assert isinstance(wrapper, MenaiIRLambda)
        assert helper_name in wrapper.outer_free_vars

    def test_already_closed_lambda_unchanged(self):
        """A lambda with no free vars is returned structurally unchanged."""
        lam = _closed_lambda('f', ['x', 'y'])
        result = _lift(lam)
        # No let wrapper — the closed lambda is returned directly.
        assert isinstance(result, MenaiIRLambda)
        assert result.sibling_free_vars == []
        assert result.outer_free_vars == []
        assert result.params == ['x', 'y']

    def test_sibling_free_vars_included_in_helper_params(self):
        """Sibling free vars are also included in the helper params."""
        lam = _capturing_lambda('f', ['x'], outer_free_vars=['outer'], sibling_free_vars=['sibling'])
        result = _lift(lam)
        assert isinstance(result, MenaiIRLet)
        _, helper = result.bindings[0]
        assert isinstance(helper, MenaiIRLambda)
        # sibling comes first (sibling_free_vars before outer_free_vars)
        assert 'sibling' in helper.params
        assert 'outer' in helper.params
        assert helper.params == ['x', 'sibling', 'outer']

    def test_helper_binding_name_contains_original_name(self):
        """Helper binding_name is derived from the original lambda's binding_name."""
        lam = _capturing_lambda('my-fn', ['x'], outer_free_vars=['v'])
        result = _lift(lam)
        assert isinstance(result, MenaiIRLet)
        helper_name, _ = result.bindings[0]
        assert 'my-fn' in helper_name

    def test_helper_is_not_variadic(self):
        """Helper is never variadic, even when the original lambda was."""
        lam = MenaiIRLambda(
            params=['x', 'rest'],
            body_plan=MenaiIRReturn(value_plan=_const(0)),
            sibling_free_vars=[],
            sibling_free_var_plans=[],
            outer_free_vars=['captured'],
            outer_free_var_plans=[_local('captured')],
            param_count=2,
            is_variadic=True,
            binding_name='variadic-fn',
        )
        result = _lift(lam)
        assert isinstance(result, MenaiIRLet)
        _, helper = result.bindings[0]
        assert isinstance(helper, MenaiIRLambda)
        assert helper.is_variadic is False

    def test_wrapper_preserves_variadic_flag(self):
        """Wrapper preserves the original lambda's is_variadic flag."""
        lam = MenaiIRLambda(
            params=['x', 'rest'],
            body_plan=MenaiIRReturn(value_plan=_const(0)),
            sibling_free_vars=[],
            sibling_free_var_plans=[],
            outer_free_vars=['captured'],
            outer_free_var_plans=[_local('captured')],
            param_count=2,
            is_variadic=True,
            binding_name='variadic-fn',
        )
        result = _lift(lam)
        assert isinstance(result, MenaiIRLet)
        wrapper = result.body_plan
        assert isinstance(wrapper, MenaiIRLambda)
        assert wrapper.is_variadic is True


# ---------------------------------------------------------------------------
# Unit tests: letrec group — helper hoisting
# ---------------------------------------------------------------------------

class TestLetrecHelperHoisting:
    """Verify that helpers are hoisted into an enclosing let for letrec groups."""

    def _make_letrec(self, bindings, body=None):
        """Build a MenaiIRLetrec with the given bindings."""
        return MenaiIRLetrec(
            bindings=bindings,
            body_plan=body or MenaiIRReturn(value_plan=_const(0)),
            in_tail_position=True,
        )

    def test_capturing_letrec_produces_enclosing_let(self):
        """
        A letrec where at least one binding captures produces an enclosing let.
        """
        letrec = self._make_letrec([
            ('f', _capturing_lambda('f', ['x'], outer_free_vars=['v'])),
        ])
        result = _lift(letrec)
        assert isinstance(result, MenaiIRLet)

    def test_helpers_are_in_enclosing_let_not_inside_letrec(self):
        """
        Helpers appear as bindings of the enclosing let, not inside the letrec
        binding values.
        """
        letrec = self._make_letrec([
            ('f', _capturing_lambda('f', ['x'], outer_free_vars=['v'])),
        ])
        result = _lift(letrec)
        assert isinstance(result, MenaiIRLet)

        # The enclosing let binding should be the helper.
        assert len(result.bindings) == 1
        helper_name, helper = result.bindings[0]
        assert isinstance(helper, MenaiIRLambda)
        assert helper.sibling_free_vars == []
        assert helper.outer_free_vars == []

        # The letrec binding value should be the wrapper, not a let/helper pair.
        inner = result.body_plan
        assert isinstance(inner, MenaiIRLetrec)
        assert len(inner.bindings) == 1
        _, wrapper = inner.bindings[0]
        assert isinstance(wrapper, MenaiIRLambda)
        # Wrapper body is a tail-call to the helper.
        call = wrapper.body_plan.value_plan  # type: ignore[union-attr]
        assert isinstance(call, MenaiIRCall)
        assert call.is_tail_call
        assert call.func_plan.name == helper_name  # type: ignore[union-attr]

    def test_two_capturing_bindings_both_helpers_hoisted(self):
        """
        Both helpers from a two-binding letrec are hoisted into the same
        enclosing let.
        """
        letrec = self._make_letrec([
            ('f', _capturing_lambda('f', ['x'], sibling_free_vars=['g'], outer_free_vars=['v'])),
            ('g', _capturing_lambda('g', ['y'], sibling_free_vars=['f'], outer_free_vars=['v'])),
        ])
        result = _lift(letrec)
        assert isinstance(result, MenaiIRLet)

        # Two helpers in the enclosing let.
        assert len(result.bindings) == 2
        for _, helper in result.bindings:
            assert isinstance(helper, MenaiIRLambda)
            assert helper.sibling_free_vars == []
            assert helper.outer_free_vars == []

        # Two wrappers remain in the letrec.
        inner = result.body_plan
        assert isinstance(inner, MenaiIRLetrec)
        assert len(inner.bindings) == 2

    def test_no_capturing_letrec_returns_plain_letrec(self):
        """
        A letrec where no binding captures anything is returned as a plain
        letrec with no enclosing let.
        """
        letrec = self._make_letrec([
            ('f', _closed_lambda('f', ['x'])),
            ('g', _closed_lambda('g', ['y'])),
        ])
        result = _lift(letrec)
        # No enclosing let — result is the letrec directly.
        assert isinstance(result, MenaiIRLetrec)

    def test_mixed_letrec_only_capturing_bindings_hoisted(self):
        """
        In a mixed letrec (some capturing, some closed), only the capturing
        bindings produce hoisted helpers.  Closed bindings stay in the letrec.
        """
        letrec = self._make_letrec([
            ('f', _capturing_lambda('f', ['x'], outer_free_vars=['v'])),
            ('g', _closed_lambda('g', ['y'])),
        ])
        result = _lift(letrec)
        assert isinstance(result, MenaiIRLet)

        # One helper hoisted (for f).
        assert len(result.bindings) == 1
        helper_name, helper = result.bindings[0]
        assert isinstance(helper, MenaiIRLambda)
        assert 'f' in helper_name

        # Both f (wrapper) and g (closed) remain in the letrec.
        inner = result.body_plan
        assert isinstance(inner, MenaiIRLetrec)
        assert len(inner.bindings) == 2

        binding_names = [name for name, _ in inner.bindings]
        assert 'f' in binding_names
        assert 'g' in binding_names

    def test_helpers_are_co_visible_in_enclosing_let(self):
        """
        All helpers from a multi-binding letrec share the same enclosing let,
        so they are co-visible — each is in scope when the others are being
        compiled.  This is the key property that enables devirtualization.
        """
        letrec = self._make_letrec([
            ('f', _capturing_lambda('f', ['x'], sibling_free_vars=['g'], outer_free_vars=[])),
            ('g', _capturing_lambda('g', ['y'], sibling_free_vars=['f'], outer_free_vars=[])),
            ('h', _capturing_lambda('h', ['z'], sibling_free_vars=['f', 'g'], outer_free_vars=[])),
        ])
        result = _lift(letrec)
        assert isinstance(result, MenaiIRLet)

        # All three helpers in one let.
        assert len(result.bindings) == 3
        helper_names = [name for name, _ in result.bindings]

        # All helpers are fully closed.
        for _, helper in result.bindings:
            assert isinstance(helper, MenaiIRLambda)
            assert helper.sibling_free_vars == []
            assert helper.outer_free_vars == []

        # The letrec has three wrappers.
        inner = result.body_plan
        assert isinstance(inner, MenaiIRLetrec)
        assert len(inner.bindings) == 3

        # Each wrapper tail-calls one of the hoisted helpers.
        for _, wrapper in inner.bindings:
            assert isinstance(wrapper, MenaiIRLambda)
            call = wrapper.body_plan.value_plan  # type: ignore[union-attr]
            assert isinstance(call, MenaiIRCall)
            assert call.is_tail_call
            assert call.func_plan.name in helper_names  # type: ignore[union-attr]

    def test_letrec_body_is_walked(self):
        """The letrec body is recursively walked (nested capturing lambdas lifted)."""
        # Body contains a capturing lambda that should itself be lifted.
        inner_lam = _capturing_lambda('inner', ['a'], outer_free_vars=['x'])
        letrec = self._make_letrec(
            [('f', _closed_lambda('f', ['x']))],
            body=MenaiIRReturn(value_plan=inner_lam),
        )
        result = _lift(letrec)
        # The letrec body's capturing lambda should have been lifted.
        # The body of the letrec is now a MenaiIRReturn whose value_plan is a
        # MenaiIRLet (the standalone lifted inner lambda).
        letrec_node = result  # no helpers hoisted — closed binding only
        assert isinstance(letrec_node, MenaiIRLetrec)
        body = letrec_node.body_plan
        assert isinstance(body, MenaiIRReturn)
        assert isinstance(body.value_plan, MenaiIRLet)


# ---------------------------------------------------------------------------
# Integration tests: compile + evaluate
# ---------------------------------------------------------------------------

class TestLambdaLifterIntegration:
    """
    End-to-end tests: compile real Menai source and verify correctness.
    These confirm that the hoisting transformation does not change semantics.
    """

    @pytest.fixture
    def menai(self):
        return Menai()

    def test_simple_closure_correct(self, menai):
        """A simple closure over an outer binding still works."""
        result = menai.evaluate("""
            (let ((x 10))
              (let ((add-x (lambda (n) (integer+ n x))))
                (add-x 32)))
        """)
        assert result == 42

    def test_mutual_recursion_correct(self, menai):
        """Mutually recursive letrec still produces correct results."""
        result = menai.evaluate("""
            (letrec ((even? (lambda (n)
                              (if (integer=? n 0) #t (odd? (integer- n 1)))))
                     (odd?  (lambda (n)
                              (if (integer=? n 0) #f (even? (integer- n 1))))))
              (list (even? 10) (odd? 7) (even? 0) (odd? 1)))
        """)
        assert result == [True, True, True, True]

    def test_mutual_recursion_tco(self, menai):
        """Mutually recursive letrec handles large inputs without stack overflow."""
        result = menai.evaluate("""
            (letrec ((even? (lambda (n)
                              (if (integer=? n 0) #t (odd? (integer- n 1)))))
                     (odd?  (lambda (n)
                              (if (integer=? n 0) #f (even? (integer- n 1))))))
              (even? 100000))
        """)
        assert result is True

    def test_three_way_mutual_recursion_correct(self, menai):
        """Three-way mutual recursion with hoisted helpers still works."""
        result = menai.evaluate("""
            (letrec ((f (lambda (n) (if (integer<=? n 0) "f" (g (integer- n 1)))))
                     (g (lambda (n) (if (integer<=? n 0) "g" (h (integer- n 1)))))
                     (h (lambda (n) (if (integer<=? n 0) "h" (f (integer- n 1))))))
              (list (f 0) (f 1) (f 2) (f 3)))
        """)
        assert result == ["f", "g", "h", "f"]

    def test_letrec_capturing_outer_binding(self, menai):
        """
        Letrec bindings that capture an outer let binding work correctly
        after hoisting.
        """
        result = menai.evaluate("""
            (let ((base 100))
              (letrec ((f (lambda (n)
                            (if (integer=? n 0)
                                base
                                (g (integer- n 1)))))
                       (g (lambda (n)
                            (if (integer=? n 0)
                                (integer+ base 1)
                                (f (integer- n 1))))))
                (list (f 0) (f 1) (f 2) (f 3))))
        """)
        assert result == [100, 101, 100, 101]

    def test_mixed_letrec_closed_and_capturing(self, menai):
        """
        A letrec with both closed and capturing bindings works correctly.
        The closed binding is not hoisted; the capturing one is.
        """
        result = menai.evaluate("""
            (letrec ((helper (lambda (x) (integer* x 2)))
                     (process (lambda (lst)
                                (if (list-null? lst)
                                    (list)
                                    (list-prepend
                                      (process (list-rest lst))
                                      (helper (list-first lst)))))))
              (process (list 1 2 3 4 5)))
        """)
        assert result == [2, 4, 6, 8, 10]

    def test_closure_passed_to_higher_order_function(self, menai):
        """
        Wrappers produced by the lifter work correctly when passed as
        first-class values to higher-order functions.
        """
        result = menai.evaluate("""
            (let ((factor 3))
              (map-list
                (lambda (x) (integer* x factor))
                (list 1 2 3 4 5)))
        """)
        assert result == [3, 6, 9, 12, 15]

    def test_letrec_function_returned_as_value(self, menai):
        """
        A letrec-bound wrapper can be returned as a first-class value and
        called later.
        """
        result = menai.evaluate("""
            (let ((base 10))
              (letrec ((make-adder (lambda (n)
                                     (lambda (x) (integer+ x (integer+ n base))))))
                (let ((add5 (make-adder 5)))
                  (add5 27))))
        """)
        assert result == 42

    def test_nested_letrec_groups(self, menai):
        """Nested letrec groups are each independently hoisted correctly."""
        result = menai.evaluate("""
            (letrec ((outer (lambda (n)
                              (letrec ((inner (lambda (m)
                                               (integer+ n m))))
                                (inner (integer* n 2))))))
              (outer 7))
        """)
        assert result == 21

    def test_accumulator_style_mutual_recursion(self, menai):
        """Accumulator-style mutual recursion with captured outer values."""
        result = menai.evaluate("""
            (let ((step 1))
              (letrec ((count-a (lambda (n acc)
                                  (if (integer<=? n 0)
                                      acc
                                      (count-b (integer- n step) (integer+ acc 1)))))
                       (count-b (lambda (n acc)
                                  (if (integer<=? n 0)
                                      acc
                                      (count-a (integer- n step) (integer+ acc 1))))))
                (count-a 1000 0)))
        """)
        assert result == 1000
