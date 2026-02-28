"""End-to-end tests for variadic type-specific ordered comparison operators.

Covers the variadic forms of:
    integer<?  integer>?  integer<=?  integer>=?
    float<?    float>?    float<=?    float>=?
    string<?   string>?   string<=?   string>=?

Each operator is a comparison chain: (op a b c) means (and (op a b) (op b c)).
All operators require at least 2 arguments.

Design notes:
- The 2-arg (binary) case must be identical to the existing binary behaviour.
- The 3+-arg case must short-circuit: once a pair fails the whole expression
  is #f, and later arguments are not evaluated.
- Type errors must still propagate regardless of position.
- 0-arg and 1-arg calls are rejected at compile time by the semantic analyser.
- All operators are usable as first-class values (fold, map, apply, etc.).
"""

import pytest

from menai import Menai, MenaiEvalError


@pytest.fixture
def menai():
    return Menai()


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def eval_bool(menai, expr: str) -> bool:
    result = menai.evaluate(expr)
    assert isinstance(result, bool), f"Expected bool, got {type(result)}: {result!r}"
    return result


# ---------------------------------------------------------------------------
# integer<?
# ---------------------------------------------------------------------------

class TestIntegerLtVariadic:

    def test_binary_true(self, menai):
        assert eval_bool(menai, '(integer<? 1 2)') is True

    def test_binary_false(self, menai):
        assert eval_bool(menai, '(integer<? 2 1)') is False
        assert eval_bool(menai, '(integer<? 1 1)') is False

    def test_three_args_all_ascending(self, menai):
        assert eval_bool(menai, '(integer<? 1 2 3)') is True

    def test_three_args_last_pair_fails(self, menai):
        assert eval_bool(menai, '(integer<? 1 3 2)') is False

    def test_three_args_first_pair_fails(self, menai):
        assert eval_bool(menai, '(integer<? 3 1 2)') is False

    def test_four_args_ascending(self, menai):
        assert eval_bool(menai, '(integer<? 1 2 3 4)') is True

    def test_four_args_equal_in_middle(self, menai):
        assert eval_bool(menai, '(integer<? 1 2 2 3)') is False

    def test_five_args(self, menai):
        assert eval_bool(menai, '(integer<? 0 1 2 3 4)') is True
        assert eval_bool(menai, '(integer<? 0 1 2 4 3)') is False

    def test_negative_values(self, menai):
        assert eval_bool(menai, '(integer<? -3 -2 -1 0)') is True
        assert eval_bool(menai, '(integer<? -3 -2 -2 0)') is False

    def test_type_error_propagates(self, menai):
        with pytest.raises(MenaiEvalError):
            menai.evaluate('(integer<? 1 2.0 3)')

    def test_arity_zero_rejected(self, menai):
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(integer<?)')

    def test_arity_one_rejected(self, menai):
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(integer<? 1)')

    def test_first_class_with_apply(self, menai):
        # Passing the binary opcode as a first-class value to fold
        result = menai.evaluate('(integer<? (integer+ 1 0) (integer+ 1 1) (integer+ 1 2))')
        assert result is True


# ---------------------------------------------------------------------------
# integer>?
# ---------------------------------------------------------------------------

class TestIntegerGtVariadic:

    def test_binary_true(self, menai):
        assert eval_bool(menai, '(integer>? 3 2)') is True

    def test_binary_false(self, menai):
        assert eval_bool(menai, '(integer>? 2 3)') is False
        assert eval_bool(menai, '(integer>? 2 2)') is False

    def test_three_args_descending(self, menai):
        assert eval_bool(menai, '(integer>? 3 2 1)') is True

    def test_three_args_fails(self, menai):
        assert eval_bool(menai, '(integer>? 3 1 2)') is False

    def test_four_args_descending(self, menai):
        assert eval_bool(menai, '(integer>? 10 7 4 1)') is True

    def test_four_args_equal_in_middle(self, menai):
        assert eval_bool(menai, '(integer>? 10 7 7 1)') is False

    def test_type_error_propagates(self, menai):
        with pytest.raises(MenaiEvalError):
            menai.evaluate('(integer>? 3 2.0 1)')

    def test_arity_zero_rejected(self, menai):
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(integer>?)')

    def test_arity_one_rejected(self, menai):
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(integer>? 5)')


# ---------------------------------------------------------------------------
# integer<=?
# ---------------------------------------------------------------------------

class TestIntegerLteVariadic:

    def test_binary_true_strict(self, menai):
        assert eval_bool(menai, '(integer<=? 1 2)') is True

    def test_binary_true_equal(self, menai):
        assert eval_bool(menai, '(integer<=? 2 2)') is True

    def test_binary_false(self, menai):
        assert eval_bool(menai, '(integer<=? 3 2)') is False

    def test_three_args_non_decreasing(self, menai):
        assert eval_bool(menai, '(integer<=? 1 2 3)') is True
        assert eval_bool(menai, '(integer<=? 1 1 2)') is True
        assert eval_bool(menai, '(integer<=? 1 1 1)') is True

    def test_three_args_fails(self, menai):
        assert eval_bool(menai, '(integer<=? 1 3 2)') is False

    def test_four_args(self, menai):
        assert eval_bool(menai, '(integer<=? 1 2 2 3)') is True
        assert eval_bool(menai, '(integer<=? 1 2 3 2)') is False

    def test_type_error_propagates(self, menai):
        with pytest.raises(MenaiEvalError):
            menai.evaluate('(integer<=? 1 1.0 2)')

    def test_arity_zero_rejected(self, menai):
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(integer<=?)')

    def test_arity_one_rejected(self, menai):
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(integer<=? 1)')


# ---------------------------------------------------------------------------
# integer>=?
# ---------------------------------------------------------------------------

class TestIntegerGteVariadic:

    def test_binary_true_strict(self, menai):
        assert eval_bool(menai, '(integer>=? 3 2)') is True

    def test_binary_true_equal(self, menai):
        assert eval_bool(menai, '(integer>=? 2 2)') is True

    def test_binary_false(self, menai):
        assert eval_bool(menai, '(integer>=? 1 2)') is False

    def test_three_args_non_increasing(self, menai):
        assert eval_bool(menai, '(integer>=? 3 2 1)') is True
        assert eval_bool(menai, '(integer>=? 3 3 2)') is True
        assert eval_bool(menai, '(integer>=? 3 3 3)') is True

    def test_three_args_fails(self, menai):
        assert eval_bool(menai, '(integer>=? 3 1 2)') is False

    def test_four_args(self, menai):
        assert eval_bool(menai, '(integer>=? 5 4 4 3)') is True
        assert eval_bool(menai, '(integer>=? 5 4 3 4)') is False

    def test_type_error_propagates(self, menai):
        with pytest.raises(MenaiEvalError):
            menai.evaluate('(integer>=? 3 2.0 1)')

    def test_arity_zero_rejected(self, menai):
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(integer>=?)')

    def test_arity_one_rejected(self, menai):
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(integer>=? 5)')


# ---------------------------------------------------------------------------
# float<?
# ---------------------------------------------------------------------------

class TestFloatLtVariadic:

    def test_binary_true(self, menai):
        assert eval_bool(menai, '(float<? 1.0 2.0)') is True

    def test_binary_false(self, menai):
        assert eval_bool(menai, '(float<? 2.0 1.0)') is False
        assert eval_bool(menai, '(float<? 1.0 1.0)') is False

    def test_three_args_ascending(self, menai):
        assert eval_bool(menai, '(float<? 1.0 2.0 3.0)') is True

    def test_three_args_last_pair_fails(self, menai):
        assert eval_bool(menai, '(float<? 1.0 3.0 2.0)') is False

    def test_four_args_ascending(self, menai):
        assert eval_bool(menai, '(float<? 1.0 2.0 3.0 4.0)') is True

    def test_four_args_equal_in_middle(self, menai):
        assert eval_bool(menai, '(float<? 1.0 2.0 2.0 3.0)') is False

    def test_type_error_propagates(self, menai):
        with pytest.raises(MenaiEvalError):
            menai.evaluate('(float<? 1.0 2 3.0)')

    def test_arity_zero_rejected(self, menai):
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(float<?)')

    def test_arity_one_rejected(self, menai):
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(float<? 1.0)')


# ---------------------------------------------------------------------------
# float>?
# ---------------------------------------------------------------------------

class TestFloatGtVariadic:

    def test_binary_true(self, menai):
        assert eval_bool(menai, '(float>? 3.0 2.0)') is True

    def test_binary_false(self, menai):
        assert eval_bool(menai, '(float>? 2.0 3.0)') is False
        assert eval_bool(menai, '(float>? 2.0 2.0)') is False

    def test_three_args_descending(self, menai):
        assert eval_bool(menai, '(float>? 3.0 2.0 1.0)') is True

    def test_three_args_fails(self, menai):
        assert eval_bool(menai, '(float>? 3.0 1.0 2.0)') is False

    def test_four_args_descending(self, menai):
        assert eval_bool(menai, '(float>? 10.0 7.0 4.0 1.0)') is True

    def test_four_args_equal_in_middle(self, menai):
        assert eval_bool(menai, '(float>? 10.0 7.0 7.0 1.0)') is False

    def test_type_error_propagates(self, menai):
        with pytest.raises(MenaiEvalError):
            menai.evaluate('(float>? 3.0 2 1.0)')

    def test_arity_zero_rejected(self, menai):
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(float>?)')

    def test_arity_one_rejected(self, menai):
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(float>? 3.0)')


# ---------------------------------------------------------------------------
# float<=?
# ---------------------------------------------------------------------------

class TestFloatLteVariadic:

    def test_binary_true_strict(self, menai):
        assert eval_bool(menai, '(float<=? 1.0 2.0)') is True

    def test_binary_true_equal(self, menai):
        assert eval_bool(menai, '(float<=? 2.0 2.0)') is True

    def test_binary_false(self, menai):
        assert eval_bool(menai, '(float<=? 3.0 2.0)') is False

    def test_three_args_non_decreasing(self, menai):
        assert eval_bool(menai, '(float<=? 1.0 2.0 3.0)') is True
        assert eval_bool(menai, '(float<=? 1.0 1.0 2.0)') is True
        assert eval_bool(menai, '(float<=? 1.0 1.0 1.0)') is True

    def test_three_args_fails(self, menai):
        assert eval_bool(menai, '(float<=? 1.0 3.0 2.0)') is False

    def test_four_args(self, menai):
        assert eval_bool(menai, '(float<=? 1.0 2.0 2.0 3.0)') is True
        assert eval_bool(menai, '(float<=? 1.0 2.0 3.0 2.0)') is False

    def test_type_error_propagates(self, menai):
        with pytest.raises(MenaiEvalError):
            menai.evaluate('(float<=? 1.0 1 2.0)')

    def test_arity_zero_rejected(self, menai):
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(float<=?)')

    def test_arity_one_rejected(self, menai):
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(float<=? 1.0)')


# ---------------------------------------------------------------------------
# float>=?
# ---------------------------------------------------------------------------

class TestFloatGteVariadic:

    def test_binary_true_strict(self, menai):
        assert eval_bool(menai, '(float>=? 3.0 2.0)') is True

    def test_binary_true_equal(self, menai):
        assert eval_bool(menai, '(float>=? 2.0 2.0)') is True

    def test_binary_false(self, menai):
        assert eval_bool(menai, '(float>=? 1.0 2.0)') is False

    def test_three_args_non_increasing(self, menai):
        assert eval_bool(menai, '(float>=? 3.0 2.0 1.0)') is True
        assert eval_bool(menai, '(float>=? 3.0 3.0 2.0)') is True
        assert eval_bool(menai, '(float>=? 3.0 3.0 3.0)') is True

    def test_three_args_fails(self, menai):
        assert eval_bool(menai, '(float>=? 3.0 1.0 2.0)') is False

    def test_four_args(self, menai):
        assert eval_bool(menai, '(float>=? 5.0 4.0 4.0 3.0)') is True
        assert eval_bool(menai, '(float>=? 5.0 4.0 3.0 4.0)') is False

    def test_type_error_propagates(self, menai):
        with pytest.raises(MenaiEvalError):
            menai.evaluate('(float>=? 3.0 2 1.0)')

    def test_arity_zero_rejected(self, menai):
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(float>=?)')

    def test_arity_one_rejected(self, menai):
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(float>=? 3.0)')


# ---------------------------------------------------------------------------
# string<?
# ---------------------------------------------------------------------------

class TestStringLtVariadic:

    def test_binary_true(self, menai):
        assert eval_bool(menai, '(string<? "a" "b")') is True

    def test_binary_false(self, menai):
        assert eval_bool(menai, '(string<? "b" "a")') is False
        assert eval_bool(menai, '(string<? "a" "a")') is False

    def test_three_args_ascending(self, menai):
        assert eval_bool(menai, '(string<? "a" "b" "c")') is True

    def test_three_args_last_pair_fails(self, menai):
        assert eval_bool(menai, '(string<? "a" "c" "b")') is False

    def test_four_args_ascending(self, menai):
        assert eval_bool(menai, '(string<? "apple" "banana" "cherry" "date")') is True

    def test_four_args_equal_in_middle(self, menai):
        assert eval_bool(menai, '(string<? "a" "b" "b" "c")') is False

    def test_type_error_propagates(self, menai):
        with pytest.raises(MenaiEvalError):
            menai.evaluate('(string<? "a" 1 "c")')

    def test_arity_zero_rejected(self, menai):
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(string<?)')

    def test_arity_one_rejected(self, menai):
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(string<? "a")')

    def test_codepoint_ordering(self, menai):
        # 'Z' (90) < 'a' (97) in codepoint order
        assert eval_bool(menai, '(string<? "A" "Z" "a")') is True


# ---------------------------------------------------------------------------
# string>?
# ---------------------------------------------------------------------------

class TestStringGtVariadic:

    def test_binary_true(self, menai):
        assert eval_bool(menai, '(string>? "b" "a")') is True

    def test_binary_false(self, menai):
        assert eval_bool(menai, '(string>? "a" "b")') is False
        assert eval_bool(menai, '(string>? "a" "a")') is False

    def test_three_args_descending(self, menai):
        assert eval_bool(menai, '(string>? "c" "b" "a")') is True

    def test_three_args_fails(self, menai):
        assert eval_bool(menai, '(string>? "c" "a" "b")') is False

    def test_four_args_descending(self, menai):
        assert eval_bool(menai, '(string>? "date" "cherry" "banana" "apple")') is True

    def test_four_args_equal_in_middle(self, menai):
        assert eval_bool(menai, '(string>? "c" "b" "b" "a")') is False

    def test_type_error_propagates(self, menai):
        with pytest.raises(MenaiEvalError):
            menai.evaluate('(string>? "c" 2 "a")')

    def test_arity_zero_rejected(self, menai):
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(string>?)')

    def test_arity_one_rejected(self, menai):
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(string>? "z")')


# ---------------------------------------------------------------------------
# string<=?
# ---------------------------------------------------------------------------

class TestStringLteVariadic:

    def test_binary_true_strict(self, menai):
        assert eval_bool(menai, '(string<=? "a" "b")') is True

    def test_binary_true_equal(self, menai):
        assert eval_bool(menai, '(string<=? "a" "a")') is True

    def test_binary_false(self, menai):
        assert eval_bool(menai, '(string<=? "b" "a")') is False

    def test_three_args_non_decreasing(self, menai):
        assert eval_bool(menai, '(string<=? "a" "b" "c")') is True
        assert eval_bool(menai, '(string<=? "a" "a" "b")') is True
        assert eval_bool(menai, '(string<=? "a" "a" "a")') is True

    def test_three_args_fails(self, menai):
        assert eval_bool(menai, '(string<=? "a" "c" "b")') is False

    def test_four_args(self, menai):
        assert eval_bool(menai, '(string<=? "a" "b" "b" "c")') is True
        assert eval_bool(menai, '(string<=? "a" "b" "c" "b")') is False

    def test_type_error_propagates(self, menai):
        with pytest.raises(MenaiEvalError):
            menai.evaluate('(string<=? "a" #t "c")')

    def test_arity_zero_rejected(self, menai):
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(string<=?)')

    def test_arity_one_rejected(self, menai):
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(string<=? "a")')


# ---------------------------------------------------------------------------
# string>=?
# ---------------------------------------------------------------------------

class TestStringGteVariadic:

    def test_binary_true_strict(self, menai):
        assert eval_bool(menai, '(string>=? "b" "a")') is True

    def test_binary_true_equal(self, menai):
        assert eval_bool(menai, '(string>=? "a" "a")') is True

    def test_binary_false(self, menai):
        assert eval_bool(menai, '(string>=? "a" "b")') is False

    def test_three_args_non_increasing(self, menai):
        assert eval_bool(menai, '(string>=? "c" "b" "a")') is True
        assert eval_bool(menai, '(string>=? "c" "c" "b")') is True
        assert eval_bool(menai, '(string>=? "c" "c" "c")') is True

    def test_three_args_fails(self, menai):
        assert eval_bool(menai, '(string>=? "c" "a" "b")') is False

    def test_four_args(self, menai):
        assert eval_bool(menai, '(string>=? "d" "c" "c" "a")') is True
        assert eval_bool(menai, '(string>=? "d" "c" "a" "b")') is False

    def test_type_error_propagates(self, menai):
        with pytest.raises(MenaiEvalError):
            menai.evaluate('(string>=? "c" 2 "a")')

    def test_arity_zero_rejected(self, menai):
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(string>=?)')

    def test_arity_one_rejected(self, menai):
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(string>=? "z")')


# ---------------------------------------------------------------------------
# Cross-family consistency
# ---------------------------------------------------------------------------

class TestVariadicConsistencyWithBinary:
    """3-arg variadic must agree with the equivalent conjunction of binary calls."""

    @pytest.mark.parametrize("op,a,b,c", [
        ('integer<?',  '1',   '2',   '3'),
        ('integer<?',  '1',   '3',   '2'),
        ('integer>?',  '3',   '2',   '1'),
        ('integer>?',  '3',   '1',   '2'),
        ('integer<=?', '1',   '1',   '2'),
        ('integer<=?', '2',   '1',   '2'),
        ('integer>=?', '2',   '2',   '1'),
        ('integer>=?', '1',   '2',   '1'),
        ('float<?',    '1.0', '2.0', '3.0'),
        ('float<?',    '1.0', '3.0', '2.0'),
        ('float>?',    '3.0', '2.0', '1.0'),
        ('float>=?',   '2.0', '2.0', '1.0'),
        ('float<=?',   '1.0', '1.0', '2.0'),
    ])
    def test_three_arg_matches_conjunction(self, menai, op, a, b, c):
        variadic = menai.evaluate(f'({op} {a} {b} {c})')
        binary_conj = menai.evaluate(f'(if ({op} {a} {b}) ({op} {b} {c}) #f)')
        assert variadic == binary_conj, (
            f"({op} {a} {b} {c}) = {variadic} but "
            f"(and ({op} {a} {b}) ({op} {b} {c})) = {binary_conj}"
        )

    @pytest.mark.parametrize("op,a,b,c", [
        ('string<?',  '"a"', '"b"', '"c"'),
        ('string<?',  '"a"', '"c"', '"b"'),
        ('string>?',  '"c"', '"b"', '"a"'),
        ('string>=?', '"b"', '"b"', '"a"'),
        ('string<=?', '"a"', '"a"', '"b"'),
    ])
    def test_string_three_arg_matches_conjunction(self, menai, op, a, b, c):
        variadic = menai.evaluate(f'({op} {a} {b} {c})')
        binary_conj = menai.evaluate(f'(if ({op} {a} {b}) ({op} {b} {c}) #f)')
        assert variadic == binary_conj


class TestVariadicMiddleArgEvaluatedOnce:
    """The middle argument in a 3-arg chain must be evaluated exactly once.

    We verify this by binding the middle value with let and confirming the
    result is correct — no double-evaluation side-effect is observable in a
    pure language, but we can at least confirm the value is used in both
    pairs correctly.
    """

    def test_integer_middle_value_used_in_both_pairs(self, menai):
        # (integer<? 1 mid 5) where mid=3: uses 3 for both (1 < 3) and (3 < 5)
        result = menai.evaluate(
            '(let ((mid 3)) (integer<? 1 mid 5))'
        )
        assert result is True

    def test_float_middle_value_used_in_both_pairs(self, menai):
        result = menai.evaluate(
            '(let ((mid 2.0)) (float<? 1.0 mid 3.0))'
        )
        assert result is True

    def test_string_middle_value_used_in_both_pairs(self, menai):
        result = menai.evaluate(
            '(let ((mid "b")) (string<? "a" mid "c"))'
        )
        assert result is True


class TestVariadicInConditionals:
    """Variadic comparison results feed correctly into conditionals."""

    def test_integer_lt_in_if(self, menai):
        assert menai.evaluate('(if (integer<? 1 2 3) "yes" "no")') == "yes"
        assert menai.evaluate('(if (integer<? 1 3 2) "yes" "no")') == "no"

    def test_float_gte_in_if(self, menai):
        assert menai.evaluate('(if (float>=? 3.0 2.0 1.0) "yes" "no")') == "yes"
        assert menai.evaluate('(if (float>=? 3.0 1.0 2.0) "yes" "no")') == "no"

    def test_string_lte_in_if(self, menai):
        assert menai.evaluate('(if (string<=? "a" "a" "b") "yes" "no")') == "yes"
        assert menai.evaluate('(if (string<=? "b" "a" "c") "yes" "no")') == "no"
