"""Tests for float-expn and complex-expn prelude lambdas.

The underlying opcodes (FLOAT_EXPN, COMPLEX_EXPN) work correctly for direct
2-argument calls, which are inlined by the codegen.  The prelude lambdas are
exercised when:

  1. The function is passed as a first-class value (e.g. to apply/map/fold).
  2. The function is called with more than 2 arguments (variadic fold path).

A naming bug in _PRELUDE_SOURCE caused the lambdas to be registered under
'float-expt' / 'complex-expt' instead of 'float-expn' / 'complex-expn',
making the variadic/first-class paths fail at runtime.  These tests pin the
correct behaviour so the bug is visible before the fix and passes after it.
"""

import cmath
import math

import pytest

from menai import MenaiEvalError


class TestFloatExpnPrelude:
    """Tests for float-expn that exercise the prelude lambda path."""

    # ------------------------------------------------------------------
    # Direct 2-arg calls — these go through the opcode directly and must
    # always pass (they are here as a baseline / regression guard).
    # ------------------------------------------------------------------

    def test_direct_two_arg_basic(self, menai):
        """Direct 2-arg call: uses opcode inline, not the prelude lambda."""
        assert menai.evaluate("(float-expn 2.0 10.0)") == 1024.0

    def test_direct_two_arg_zero_exponent(self, menai):
        """x^0 == 1.0 for any x."""
        assert menai.evaluate("(float-expn 5.0 0.0)") == 1.0

    def test_direct_two_arg_zero_base(self, menai):
        """0^x == 0.0 for positive x."""
        assert menai.evaluate("(float-expn 0.0 3.0)") == 0.0

    # ------------------------------------------------------------------
    # First-class / apply path — exercises the prelude lambda.
    # These fail before the fix because the lambda is registered under
    # the wrong name ('float-expt') and therefore unreachable.
    # ------------------------------------------------------------------

    def test_apply_float_expn(self, menai):
        """float-expn passed to apply: goes through the prelude lambda."""
        result = menai.evaluate("(apply float-expn (list 2.0 8.0))")
        assert result == pytest.approx(256.0)

    def test_map_with_float_expn(self, menai):
        """float-expn used as a first-class value inside map (partial via lambda)."""
        # Square each element: map (lambda (x) (float-expn x 2.0)) over list
        result = menai.evaluate(
            "(map-list (lambda (x) (float-expn x 2.0)) (list 1.0 2.0 3.0 4.0))"
        )
        assert result == [1.0, 4.0, 9.0, 16.0]

    def test_fold_with_float_expn(self, menai):
        """float-expn used as a first-class value inside fold."""
        # fold float-expn 2.0 (list 3.0) → (float-expn 2.0 3.0) = 8.0
        result = menai.evaluate("(fold-list float-expn 2.0 (list 3.0))")
        assert result == pytest.approx(8.0)

    def test_function_predicate_on_float_expn(self, menai):
        """float-expn is a function? — verifies it resolves as a first-class value."""
        result = menai.evaluate("(function? float-expn)")
        assert result is True

    def test_function_min_arity_float_expn(self, menai):
        """float-expn is a variadic prelude lambda: min arity is 0 (lambda (. args) ...)."""
        result = menai.evaluate("(function-min-arity float-expn)")
        assert result == 0

    def test_variadic_three_args(self, menai):
        """float-expn with 3 args folds left: ((2.0 ^ 3.0) ^ 2.0) = 64.0."""
        result = menai.evaluate("(float-expn 2.0 3.0 2.0)")
        assert result == pytest.approx(64.0)


class TestComplexExpnPrelude:
    """Tests for complex-expn that exercise the prelude lambda path."""

    # ------------------------------------------------------------------
    # Direct 2-arg calls — baseline / regression guard.
    # ------------------------------------------------------------------

    def test_direct_two_arg_basic(self, menai):
        """Direct 2-arg call: uses opcode inline, not the prelude lambda."""
        result = menai.evaluate("(complex-expn 2+0j 10+0j)")
        assert abs(result - 1024 + 0j) < 1e-9

    def test_direct_two_arg_imaginary_exponent(self, menai):
        """e^(i*pi) ≈ -1 (Euler's identity via complex-expn)."""
        # complex-expn base exponent: base^exponent  →  e^(i*pi)
        # (complex-expn (float->complex e 0.0) (float->complex 0.0 pi))
        result = menai.evaluate(
            "(complex-expn (float->complex e 0.0) (float->complex 0.0 pi))"
        )
        expected = cmath.exp(1j * math.pi)
        assert abs(result - expected) < 1e-9

    # ------------------------------------------------------------------
    # First-class / apply path — exercises the prelude lambda.
    # These fail before the fix.
    # ------------------------------------------------------------------

    def test_apply_complex_expn(self, menai):
        """complex-expn passed to apply: goes through the prelude lambda."""
        result = menai.evaluate("(apply complex-expn (list 2+0j 8+0j))")
        assert abs(result - 256 + 0j) < 1e-9

    def test_map_with_complex_expn(self, menai):
        """complex-expn used as a first-class value inside map (partial via lambda)."""
        # Square each element: map (lambda (z) (complex-expn z 2+0j)) over list
        result = menai.evaluate(
            "(map-list (lambda (z) (complex-expn z 2+0j)) (list 1+0j 2+0j 3+0j))"
        )
        assert len(result) == 3
        assert abs(result[0] - 1 + 0j) < 1e-9
        assert abs(result[1] - 4 + 0j) < 1e-9
        assert abs(result[2] - 9 + 0j) < 1e-9

    def test_function_predicate_on_complex_expn(self, menai):
        """complex-expn is a function? — verifies it resolves as a first-class value."""
        result = menai.evaluate("(function? complex-expn)")
        assert result is True

    def test_function_min_arity_complex_expn(self, menai):
        """complex-expn is a variadic prelude lambda: min arity is 0 (lambda (. args) ...)."""
        result = menai.evaluate("(function-min-arity complex-expn)")
        assert result == 0

    def test_variadic_three_args(self, menai):
        """complex-expn with 3 args folds left: ((2+0j ^ 3+0j) ^ 2+0j) = 64+0j."""
        result = menai.evaluate("(complex-expn 2+0j 3+0j 2+0j)")
        assert abs(result - (64 + 0j)) < 1e-9
