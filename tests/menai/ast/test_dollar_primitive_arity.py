"""Tests for arity checking of user-written $-prefixed primitive calls.

$-prefixed names are the explicit escape hatch for calling opcode-backed
primitives directly from source.  The semantic analyser validates both that
the base name is a known primitive and that the call supplies exactly the
opcode's arity — there is no optional-argument handling at this level.

Tests cover:
  - Valid calls at exact arity (unary, binary, ternary opcodes)
  - Too-few-arguments rejection
  - Too-many-arguments rejection
  - Unknown $-name rejection (regression guard)
"""

import pytest

from menai import MenaiEvalError


class TestDollarPrimitiveValidCalls:
    """Valid $-calls at exact arity evaluate correctly."""

    def test_unary_none_p(self, menai):
        """($none? #none) — unary opcode, 1 argument."""
        assert menai.evaluate('($none? #none)') is True

    def test_unary_integer_neg(self, menai):
        """($integer-neg 5) — unary opcode."""
        assert menai.evaluate('($integer-neg 5)') == -5

    def test_unary_string_length(self, menai):
        """($string-length "hello") — unary opcode."""
        assert menai.evaluate('($string-length "hello")') == 5

    def test_binary_integer_add(self, menai):
        """($integer+ 3 4) — binary opcode, 2 arguments."""
        assert menai.evaluate('($integer+ 3 4)') == 7

    def test_binary_string_eq(self, menai):
        """($string=? "a" "a") — binary opcode."""
        assert menai.evaluate('($string=? "a" "a")') is True

    def test_binary_list_prepend(self, menai):
        """($list-prepend (list 2 3) 1) — binary opcode."""
        assert menai.evaluate('($list-prepend (list 2 3) 1)') == [1, 2, 3]

    def test_ternary_dict_get(self, menai):
        """($dict-get d k default) — ternary opcode, 3 arguments."""
        assert menai.evaluate('($dict-get (dict "a" 1) "a" #none)') == 1

    def test_ternary_dict_get_missing_key(self, menai):
        """($dict-get d k default) returns default when key absent."""
        assert menai.evaluate('($dict-get (dict "a" 1) "z" 99)') == 99

    def test_ternary_range(self, menai):
        """($range 0 5 1) — ternary opcode, 3 arguments."""
        assert menai.evaluate('($range 0 5 1)') == [0, 1, 2, 3, 4]

    def test_ternary_string_slice(self, menai):
        """($string-slice s start end) — ternary opcode."""
        assert menai.evaluate('($string-slice "hello" 1 3)') == 'el'

    def test_ternary_list_slice(self, menai):
        """($list-slice l start end) — ternary opcode."""
        assert menai.evaluate('($list-slice (list 1 2 3 4 5) 1 3)') == [2, 3]


class TestDollarPrimitiveTooFewArgs:
    """$-calls with fewer than the opcode arity are rejected."""

    def test_unary_with_zero_args(self, menai):
        """($none?) — unary opcode called with 0 arguments."""
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('($none?)')

    def test_binary_with_zero_args(self, menai):
        """($integer+) — binary opcode called with 0 arguments."""
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('($integer+)')

    def test_binary_with_one_arg(self, menai):
        """($integer+ 3) — binary opcode called with 1 argument."""
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('($integer+ 3)')

    def test_ternary_with_one_arg(self, menai):
        """($dict-get d) — ternary opcode called with 1 argument."""
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('($dict-get (dict "a" 1))')

    def test_ternary_with_two_args(self, menai):
        """($dict-get d k) — ternary opcode called with 2 arguments."""
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('($dict-get (dict "a" 1) "a")')

    def test_ternary_range_with_two_args(self, menai):
        """($range 0 5) — ternary opcode called with 2 arguments."""
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('($range 0 5)')


class TestDollarPrimitiveTooManyArgs:
    """$-calls with more than the opcode arity are rejected."""

    def test_unary_with_two_args(self, menai):
        """($none? #none #none) — unary opcode called with 2 arguments."""
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('($none? #none #none)')

    def test_binary_with_three_args(self, menai):
        """($integer+ 1 2 3) — binary opcode called with 3 arguments."""
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('($integer+ 1 2 3)')

    def test_ternary_with_four_args(self, menai):
        """($dict-get d k v extra) — ternary opcode called with 4 arguments."""
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('($dict-get (dict "a" 1) "a" #none "extra")')

    def test_ternary_range_with_four_args(self, menai):
        """($range 0 5 1 99) — ternary opcode called with 4 arguments."""
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('($range 0 5 1 99)')


class TestDollarPrimitiveUnknownName:
    """$-calls with an unrecognised base name are rejected."""

    def test_unknown_dollar_name(self, menai):
        """($no-such-op 1) — base name not in BUILTIN_OPCODE_MAP."""
        with pytest.raises(MenaiEvalError, match="Unknown primitive"):
            menai.evaluate('($no-such-op 1)')

    def test_dollar_prelude_function_rejected(self, menai):
        """($map-list f l) — map-list is a prelude lambda, not an opcode primitive."""
        with pytest.raises(MenaiEvalError, match="Unknown primitive"):
            menai.evaluate('($map-list (lambda (x) x) (list 1 2 3))')
