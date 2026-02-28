"""Tests for complex+ and complex* prelude lambda correctness.

The prelude lambdas for complex+ and complex* incorrectly error on zero
arguments instead of returning the identity values (0+0j and 1+0j
respectively), unlike their integer and float counterparts.

These tests cover:
  1. Zero-arg identity — the bug that currently fails.
  2. Consistency: prelude path (via apply/first-class) vs direct opcode
     path, for 1-arg, 2-arg, and 3-arg calls.
"""

import pytest

from menai import MenaiEvalError


class TestComplexAddPrelude:
    """Tests for complex+ prelude lambda."""

    # ------------------------------------------------------------------
    # Zero-arg identity — currently broken in the prelude
    # ------------------------------------------------------------------

    def test_zero_arg_identity(self, menai):
        """(complex+) → 0+0j  (additive identity, matches integer+ and float+)."""
        result = menai.evaluate("(complex+)")
        assert result == 0+0j

    def test_zero_arg_via_apply(self, menai):
        """(apply complex+ (list)) → 0+0j  (same identity via first-class path)."""
        result = menai.evaluate("(apply complex+ (list))")
        assert result == 0+0j

    # ------------------------------------------------------------------
    # Consistency: prelude path must agree with direct opcode for 1 arg
    # ------------------------------------------------------------------

    def test_one_arg_consistency(self, menai):
        """(complex+ z) returns z — prelude and direct call agree."""
        # Direct call: codegen emits the opcode inline
        direct = menai.evaluate("(complex+ 3+4j 0+0j)")
        # Prelude path: via apply, forces the lambda
        via_apply = menai.evaluate("(apply complex+ (list 3+4j 0+0j))")
        assert direct == via_apply

    # ------------------------------------------------------------------
    # Consistency: prelude path must agree with direct opcode for 2 args
    # ------------------------------------------------------------------

    def test_two_arg_consistency(self, menai):
        """(complex+ a b) — prelude and direct call agree."""
        direct = menai.evaluate("(complex+ 1+2j 3+4j)")
        via_apply = menai.evaluate("(apply complex+ (list 1+2j 3+4j))")
        assert direct == via_apply
        assert direct == 4+6j

    # ------------------------------------------------------------------
    # Consistency: 3-arg variadic fold (prelude only path)
    # ------------------------------------------------------------------

    def test_three_arg_fold(self, menai):
        """(complex+ a b c) folds left: ((a+b)+c)."""
        result = menai.evaluate("(complex+ 1+0j 2+0j 3+0j)")
        assert result == 6+0j

    def test_three_arg_fold_via_apply(self, menai):
        """(apply complex+ (list a b c)) agrees with direct 3-arg call."""
        direct = menai.evaluate("(complex+ 1+2j 3+4j 5+6j)")
        via_apply = menai.evaluate("(apply complex+ (list 1+2j 3+4j 5+6j))")
        assert direct == via_apply
        assert direct == 9+12j


class TestComplexMulPrelude:
    """Tests for complex* prelude lambda."""

    # ------------------------------------------------------------------
    # Zero-arg identity — currently broken in the prelude
    # ------------------------------------------------------------------

    def test_zero_arg_identity(self, menai):
        """(complex*) → 1+0j  (multiplicative identity, matches integer* and float*)."""
        result = menai.evaluate("(complex*)")
        assert result == 1+0j

    def test_zero_arg_via_apply(self, menai):
        """(apply complex* (list)) → 1+0j  (same identity via first-class path)."""
        result = menai.evaluate("(apply complex* (list))")
        assert result == 1+0j

    # ------------------------------------------------------------------
    # Consistency: prelude path must agree with direct opcode for 2 args
    # ------------------------------------------------------------------

    def test_two_arg_consistency(self, menai):
        """(complex* a b) — prelude and direct call agree."""
        direct = menai.evaluate("(complex* 1+2j 3+4j)")
        via_apply = menai.evaluate("(apply complex* (list 1+2j 3+4j))")
        assert direct == via_apply
        assert direct == -5+10j

    # ------------------------------------------------------------------
    # Consistency: 3-arg variadic fold (prelude only path)
    # ------------------------------------------------------------------

    def test_three_arg_fold(self, menai):
        """(complex* a b c) folds left: ((a*b)*c)."""
        result = menai.evaluate("(complex* 1+0j 2+0j 3+0j)")
        assert result == 6+0j

    def test_three_arg_fold_via_apply(self, menai):
        """(apply complex* (list a b c)) agrees with direct 3-arg call."""
        direct = menai.evaluate("(complex* 2+0j 3+0j 4+0j)")
        via_apply = menai.evaluate("(apply complex* (list 2+0j 3+0j 4+0j))")
        assert direct == via_apply
        assert direct == 24+0j
