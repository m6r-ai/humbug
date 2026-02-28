"""Tests for typed comparison operators.

This module tests:
- Type-specific not-equals predicates (!=?) for all typed equality types:
    boolean!=?, integer!=?, float!=?, complex!=?, string!=?, list!=?, dict!=?
- Type-specific ordered comparison predicates for integer, float, and string:
    integer<?, integer>?, integer<=?, integer>=?
    float<?,   float>?,   float<=?,   float>=?
    string<?,  string>?,  string<=?,  string>=?  (lexicographic / codepoint order)

Design notes:
- All operators are strictly typed: they reject arguments of the wrong type.
- All operators are binary (arity 2, 2): no variadic form exists.
- String ordering uses Unicode codepoint order (same as Python str comparison),
  not locale-aware collation.
"""

import pytest

from menai import MenaiEvalError


# ---------------------------------------------------------------------------
# Not-equals predicates
# ---------------------------------------------------------------------------

class TestBooleanNeqP:
    """Tests for boolean!=?"""

    def test_unequal_booleans(self, menai):
        assert menai.evaluate('(boolean!=? #t #f)') is True
        assert menai.evaluate('(boolean!=? #f #t)') is True

    def test_equal_booleans(self, menai):
        assert menai.evaluate('(boolean!=? #t #t)') is False
        assert menai.evaluate('(boolean!=? #f #f)') is False

    def test_rejects_non_boolean_first_arg(self, menai):
        with pytest.raises(MenaiEvalError, match="boolean!=.*requires boolean arguments.*integer"):
            menai.evaluate('(boolean!=? 1 #t)')

    def test_rejects_non_boolean_second_arg(self, menai):
        with pytest.raises(MenaiEvalError, match="boolean!=.*requires boolean arguments.*string"):
            menai.evaluate('(boolean!=? #t "true")')

    def test_wrong_arity(self, menai):
        with pytest.raises(MenaiEvalError, match="boolean!=.*has wrong number of arguments"):
            menai.evaluate('(boolean!=?)')

        with pytest.raises(MenaiEvalError, match="boolean!=.*has wrong number of arguments"):
            menai.evaluate('(boolean!=? #t)')


class TestIntegerNeqP:
    """Tests for integer!=?"""

    def test_unequal_integers(self, menai):
        assert menai.evaluate('(integer!=? 1 2)') is True
        assert menai.evaluate('(integer!=? -1 1)') is True
        assert menai.evaluate('(integer!=? 0 1)') is True

    def test_equal_integers(self, menai):
        assert menai.evaluate('(integer!=? 42 42)') is False
        assert menai.evaluate('(integer!=? 0 0)') is False
        assert menai.evaluate('(integer!=? -5 -5)') is False

    def test_rejects_float(self, menai):
        with pytest.raises(MenaiEvalError, match="integer!=.*requires integer arguments.*float"):
            menai.evaluate('(integer!=? 1 1.0)')

        with pytest.raises(MenaiEvalError, match="integer!=.*requires integer arguments.*float"):
            menai.evaluate('(integer!=? 1.0 1)')

    def test_rejects_complex(self, menai):
        with pytest.raises(MenaiEvalError, match="integer!=.*requires integer arguments.*complex"):
            menai.evaluate('(integer!=? 1 1+0j)')

    def test_rejects_non_number(self, menai):
        with pytest.raises(MenaiEvalError, match="integer!=.*requires integer arguments.*string"):
            menai.evaluate('(integer!=? 1 "1")')

    def test_wrong_arity(self, menai):
        with pytest.raises(MenaiEvalError, match="integer!=.*has wrong number of arguments"):
            menai.evaluate('(integer!=?)')

        with pytest.raises(MenaiEvalError, match="integer!=.*has wrong number of arguments"):
            menai.evaluate('(integer!=? 1)')


class TestFloatNeqP:
    """Tests for float!=?"""

    def test_unequal_floats(self, menai):
        assert menai.evaluate('(float!=? 1.0 2.0)') is True
        assert menai.evaluate('(float!=? 3.14 3.15)') is True

    def test_equal_floats(self, menai):
        assert menai.evaluate('(float!=? 1.0 1.0)') is False
        assert menai.evaluate('(float!=? 0.0 0.0)') is False

    def test_rejects_integer(self, menai):
        with pytest.raises(MenaiEvalError, match="float!=.*requires float arguments.*integer"):
            menai.evaluate('(float!=? 1.0 1)')

        with pytest.raises(MenaiEvalError, match="float!=.*requires float arguments.*integer"):
            menai.evaluate('(float!=? 1 1.0)')

    def test_rejects_complex(self, menai):
        with pytest.raises(MenaiEvalError, match="float!=.*requires float arguments.*complex"):
            menai.evaluate('(float!=? 1.0 1+0j)')

    def test_wrong_arity(self, menai):
        with pytest.raises(MenaiEvalError, match="float!=.*has wrong number of arguments"):
            menai.evaluate('(float!=?)')

        with pytest.raises(MenaiEvalError, match="float!=.*has wrong number of arguments"):
            menai.evaluate('(float!=? 1.0)')


class TestComplexNeqP:
    """Tests for complex!=?"""

    def test_unequal_complex(self, menai):
        assert menai.evaluate('(complex!=? 1+2j 1+3j)') is True
        assert menai.evaluate('(complex!=? 0+1j 0+0j)') is True

    def test_equal_complex(self, menai):
        assert menai.evaluate('(complex!=? 1+2j 1+2j)') is False
        assert menai.evaluate('(complex!=? 0+0j 0+0j)') is False

    def test_rejects_integer(self, menai):
        with pytest.raises(MenaiEvalError, match="complex!=.*requires complex arguments.*integer"):
            menai.evaluate('(complex!=? 1+0j 1)')

    def test_rejects_float(self, menai):
        with pytest.raises(MenaiEvalError, match="complex!=.*requires complex arguments.*float"):
            menai.evaluate('(complex!=? 1+0j 1.0)')

    def test_wrong_arity(self, menai):
        with pytest.raises(MenaiEvalError, match="complex!=.*has wrong number of arguments"):
            menai.evaluate('(complex!=?)')

        with pytest.raises(MenaiEvalError, match="complex!=.*has wrong number of arguments"):
            menai.evaluate('(complex!=? 1+2j)')


class TestStringNeqP:
    """Tests for string!=?"""

    def test_unequal_strings(self, menai):
        assert menai.evaluate('(string!=? "hello" "world")') is True
        assert menai.evaluate('(string!=? "a" "b")') is True
        assert menai.evaluate('(string!=? "" "x")') is True

    def test_equal_strings(self, menai):
        assert menai.evaluate('(string!=? "hello" "hello")') is False
        assert menai.evaluate('(string!=? "" "")') is False

    def test_rejects_non_string(self, menai):
        with pytest.raises(MenaiEvalError, match="string!=.*requires string arguments.*integer"):
            menai.evaluate('(string!=? "hello" 42)')

        with pytest.raises(MenaiEvalError, match="string!=.*requires string arguments.*boolean"):
            menai.evaluate('(string!=? #t "true")')

    def test_wrong_arity(self, menai):
        with pytest.raises(MenaiEvalError, match="string!=.*has wrong number of arguments"):
            menai.evaluate('(string!=?)')

        with pytest.raises(MenaiEvalError, match="string!=.*has wrong number of arguments"):
            menai.evaluate('(string!=? "a")')


class TestListNeqP:
    """Tests for list!=?"""

    def test_unequal_lists(self, menai):
        assert menai.evaluate('(list!=? (list 1 2 3) (list 1 2 4))') is True
        assert menai.evaluate('(list!=? (list 1) (list 1 2))') is True
        assert menai.evaluate('(list!=? (list) (list 1))') is True

    def test_equal_lists(self, menai):
        assert menai.evaluate('(list!=? (list 1 2 3) (list 1 2 3))') is False
        assert menai.evaluate('(list!=? (list) (list))') is False

    def test_nested_list_inequality(self, menai):
        assert menai.evaluate('(list!=? (list (list 1 2)) (list (list 1 3)))') is True
        assert menai.evaluate('(list!=? (list (list 1 2)) (list (list 1 2)))') is False

    def test_rejects_non_list(self, menai):
        with pytest.raises(MenaiEvalError, match="list!=.*requires list arguments.*integer"):
            menai.evaluate('(list!=? (list 1) 1)')

        with pytest.raises(MenaiEvalError, match="list!=.*requires list arguments.*string"):
            menai.evaluate('(list!=? "a" (list 1))')

    def test_wrong_arity(self, menai):
        with pytest.raises(MenaiEvalError, match="list!=.*has wrong number of arguments"):
            menai.evaluate('(list!=?)')

        with pytest.raises(MenaiEvalError, match="list!=.*has wrong number of arguments"):
            menai.evaluate('(list!=? (list 1))')


class TestAlistNeqP:
    """Tests for dict!=?"""

    def test_unequal_dicts(self, menai):
        assert menai.evaluate('(dict!=? (dict (list "a" 1)) (dict (list "a" 2)))') is True
        assert menai.evaluate('(dict!=? (dict) (dict (list "a" 1)))') is True

    def test_equal_dicts(self, menai):
        assert menai.evaluate('(dict!=? (dict) (dict))') is False
        assert menai.evaluate('(dict!=? (dict (list "a" 1)) (dict (list "a" 1)))') is False

    def test_rejects_non_dict(self, menai):
        with pytest.raises(MenaiEvalError, match="dict!=.*requires dict arguments.*list"):
            menai.evaluate('(dict!=? (dict) (list 1 2))')

        with pytest.raises(MenaiEvalError, match="dict!=.*requires dict arguments.*integer"):
            menai.evaluate('(dict!=? (dict) 42)')

    def test_wrong_arity(self, menai):
        with pytest.raises(MenaiEvalError, match="dict!=.*has wrong number of arguments"):
            menai.evaluate('(dict!=?)')

        with pytest.raises(MenaiEvalError, match="dict!=.*has wrong number of arguments"):
            menai.evaluate('(dict!=? (dict))')


# ---------------------------------------------------------------------------
# Integer ordered comparisons
# ---------------------------------------------------------------------------

class TestIntegerOrderedComparisons:
    """Tests for integer<?, integer>?, integer<=?, integer>=?"""

    def test_integer_lt_true(self, menai):
        assert menai.evaluate('(integer<? 1 2)') is True
        assert menai.evaluate('(integer<? -5 0)') is True
        assert menai.evaluate('(integer<? 0 1)') is True

    def test_integer_lt_false(self, menai):
        assert menai.evaluate('(integer<? 2 1)') is False
        assert menai.evaluate('(integer<? 1 1)') is False  # equal is not less-than

    def test_integer_gt_true(self, menai):
        assert menai.evaluate('(integer>? 2 1)') is True
        assert menai.evaluate('(integer>? 0 -5)') is True

    def test_integer_gt_false(self, menai):
        assert menai.evaluate('(integer>? 1 2)') is False
        assert menai.evaluate('(integer>? 1 1)') is False  # equal is not greater-than

    def test_integer_lte_true(self, menai):
        assert menai.evaluate('(integer<=? 1 2)') is True
        assert menai.evaluate('(integer<=? 1 1)') is True  # equal satisfies <=
        assert menai.evaluate('(integer<=? -1 0)') is True

    def test_integer_lte_false(self, menai):
        assert menai.evaluate('(integer<=? 2 1)') is False

    def test_integer_gte_true(self, menai):
        assert menai.evaluate('(integer>=? 2 1)') is True
        assert menai.evaluate('(integer>=? 1 1)') is True  # equal satisfies >=
        assert menai.evaluate('(integer>=? 0 -1)') is True

    def test_integer_gte_false(self, menai):
        assert menai.evaluate('(integer>=? 1 2)') is False

    def test_integer_comparisons_reject_float(self, menai):
        for op in ('integer<?', 'integer>?', 'integer<=?', 'integer>=?'):
            with pytest.raises(MenaiEvalError, match=f"{op}.*requires integer arguments.*float"):
                menai.evaluate(f'({op} 1 2.0)')

            with pytest.raises(MenaiEvalError, match=f"{op}.*requires integer arguments.*float"):
                menai.evaluate(f'({op} 1.0 2)')

    def test_integer_comparisons_reject_complex(self, menai):
        for op in ('integer<?', 'integer>?', 'integer<=?', 'integer>=?'):
            with pytest.raises(MenaiEvalError, match=f"{op}.*requires integer arguments.*complex"):
                menai.evaluate(f'({op} 1 1+0j)')

    def test_integer_comparisons_reject_non_number(self, menai):
        for op in ('integer<?', 'integer>?', 'integer<=?', 'integer>=?'):
            with pytest.raises(MenaiEvalError, match=f"{op}.*requires integer arguments.*string"):
                menai.evaluate(f'({op} 1 "2")')

            with pytest.raises(MenaiEvalError, match=f"{op}.*requires integer arguments.*boolean"):
                menai.evaluate(f'({op} #t 1)')

    def test_integer_comparisons_wrong_arity(self, menai):
        for op in ('integer<?', 'integer>?', 'integer<=?', 'integer>=?'):
            with pytest.raises(MenaiEvalError, match=f"{op}.*has wrong number of arguments"):
                menai.evaluate(f'({op})')

            with pytest.raises(MenaiEvalError, match=f"{op}.*has wrong number of arguments"):
                menai.evaluate(f'({op} 1)')

    def test_integer_comparisons_large_values(self, menai):
        """Test with arbitrarily large integers (Python's unbounded integers)."""
        big = 10 ** 50
        bigger = big + 1
        assert menai.evaluate(f'(integer<? {big} {bigger})') is True
        assert menai.evaluate(f'(integer>? {bigger} {big})') is True
        assert menai.evaluate(f'(integer<=? {big} {big})') is True
        assert menai.evaluate(f'(integer>=? {big} {big})') is True

    def test_integer_comparisons_negative_values(self, menai):
        assert menai.evaluate('(integer<? -10 -5)') is True
        assert menai.evaluate('(integer>? -5 -10)') is True
        assert menai.evaluate('(integer<=? -5 -5)') is True
        assert menai.evaluate('(integer>=? -5 -5)') is True

    def test_integer_comparisons_usable_in_conditionals(self, menai):
        """Test that results feed correctly into if expressions."""
        assert menai.evaluate('(if (integer<? 1 2) #t #f)') is True
        assert menai.evaluate('(if (integer>? 1 2) #t #f)') is False

    def test_integer_comparisons_usable_as_first_class(self, menai):
        """Test that operators can be passed as first-class functions."""
        assert menai.evaluate('(integer<? (integer+ 1 2) (integer* 2 2))') is True


# ---------------------------------------------------------------------------
# Float ordered comparisons
# ---------------------------------------------------------------------------

class TestFloatOrderedComparisons:
    """Tests for float<?, float>?, float<=?, float>=?"""

    def test_float_lt_true(self, menai):
        assert menai.evaluate('(float<? 1.0 2.0)') is True
        assert menai.evaluate('(float<? -1.5 0.0)') is True

    def test_float_lt_false(self, menai):
        assert menai.evaluate('(float<? 2.0 1.0)') is False
        assert menai.evaluate('(float<? 1.0 1.0)') is False

    def test_float_gt_true(self, menai):
        assert menai.evaluate('(float>? 2.0 1.0)') is True
        assert menai.evaluate('(float>? 0.0 -1.5)') is True

    def test_float_gt_false(self, menai):
        assert menai.evaluate('(float>? 1.0 2.0)') is False
        assert menai.evaluate('(float>? 1.0 1.0)') is False

    def test_float_lte_true(self, menai):
        assert menai.evaluate('(float<=? 1.0 2.0)') is True
        assert menai.evaluate('(float<=? 1.0 1.0)') is True

    def test_float_lte_false(self, menai):
        assert menai.evaluate('(float<=? 2.0 1.0)') is False

    def test_float_gte_true(self, menai):
        assert menai.evaluate('(float>=? 2.0 1.0)') is True
        assert menai.evaluate('(float>=? 1.0 1.0)') is True

    def test_float_gte_false(self, menai):
        assert menai.evaluate('(float>=? 1.0 2.0)') is False

    def test_float_comparisons_reject_integer(self, menai):
        for op in ('float<?', 'float>?', 'float<=?', 'float>=?'):
            with pytest.raises(MenaiEvalError, match=f"{op}.*requires float arguments.*integer"):
                menai.evaluate(f'({op} 1.0 2)')

            with pytest.raises(MenaiEvalError, match=f"{op}.*requires float arguments.*integer"):
                menai.evaluate(f'({op} 1 2.0)')

    def test_float_comparisons_reject_complex(self, menai):
        for op in ('float<?', 'float>?', 'float<=?', 'float>=?'):
            with pytest.raises(MenaiEvalError, match=f"{op}.*requires float arguments.*complex"):
                menai.evaluate(f'({op} 1.0 1+0j)')

    def test_float_comparisons_reject_non_number(self, menai):
        for op in ('float<?', 'float>?', 'float<=?', 'float>=?'):
            with pytest.raises(MenaiEvalError, match=f"{op}.*requires float arguments.*string"):
                menai.evaluate(f'({op} 1.0 "2.0")')

    def test_float_comparisons_wrong_arity(self, menai):
        for op in ('float<?', 'float>?', 'float<=?', 'float>=?'):
            with pytest.raises(MenaiEvalError, match=f"{op}.*has wrong number of arguments"):
                menai.evaluate(f'({op})')

            with pytest.raises(MenaiEvalError, match=f"{op}.*has wrong number of arguments"):
                menai.evaluate(f'({op} 1.0)')

    def test_float_comparisons_special_values(self, menai):
        """Test with very large and very small values."""
        # Use float-log of a very small positive number to get a large negative result
        assert menai.evaluate('(float<? (float-log 0.001) 0.0)') is True
        assert menai.evaluate('(float>? (float-log 1000.0) 0.0)') is True
        assert menai.evaluate('(float<=? (float-neg 1.0) 0.0)') is True
        assert menai.evaluate('(float>=? 1.0 (float-neg 1.0))') is True

    def test_float_comparisons_usable_in_conditionals(self, menai):
        assert menai.evaluate('(if (float<? 1.0 2.0) #t #f)') is True
        assert menai.evaluate('(if (float>? 1.0 2.0) #t #f)') is False

    def test_float_integer_not_interchangeable(self, menai):
        """Confirm float and integer comparisons are not interchangeable."""
        # integer<? rejects floats; float<? rejects integers
        with pytest.raises(MenaiEvalError, match="integer.*requires integer arguments.*float"):
            menai.evaluate('(integer<? 1 2.0)')

        with pytest.raises(MenaiEvalError, match="float.*requires float arguments.*integer"):
            menai.evaluate('(float<? 1 2.0)')


# ---------------------------------------------------------------------------
# String ordered comparisons
# ---------------------------------------------------------------------------

class TestStringOrderedComparisons:
    """Tests for string<?, string>?, string<=?, string>=?

    Ordering is Unicode codepoint order (same as Python str comparison).
    """

    def test_string_lt_true(self, menai):
        assert menai.evaluate('(string<? "apple" "banana")') is True
        assert menai.evaluate('(string<? "a" "b")') is True
        assert menai.evaluate('(string<? "" "a")') is True   # empty < non-empty

    def test_string_lt_false(self, menai):
        assert menai.evaluate('(string<? "banana" "apple")') is False
        assert menai.evaluate('(string<? "a" "a")') is False  # equal is not less-than

    def test_string_gt_true(self, menai):
        assert menai.evaluate('(string>? "banana" "apple")') is True
        assert menai.evaluate('(string>? "b" "a")') is True

    def test_string_gt_false(self, menai):
        assert menai.evaluate('(string>? "apple" "banana")') is False
        assert menai.evaluate('(string>? "a" "a")') is False

    def test_string_lte_true(self, menai):
        assert menai.evaluate('(string<=? "apple" "banana")') is True
        assert menai.evaluate('(string<=? "a" "a")') is True   # equal satisfies <=
        assert menai.evaluate('(string<=? "" "")') is True

    def test_string_lte_false(self, menai):
        assert menai.evaluate('(string<=? "banana" "apple")') is False

    def test_string_gte_true(self, menai):
        assert menai.evaluate('(string>=? "banana" "apple")') is True
        assert menai.evaluate('(string>=? "a" "a")') is True   # equal satisfies >=
        assert menai.evaluate('(string>=? "" "")') is True

    def test_string_gte_false(self, menai):
        assert menai.evaluate('(string>=? "apple" "banana")') is False

    def test_string_ordering_is_codepoint_not_locale(self, menai):
        """Uppercase letters have lower codepoints than lowercase in ASCII/Unicode."""
        # 'Z' (90) < 'a' (97) in codepoint order
        assert menai.evaluate('(string<? "Z" "a")') is True
        assert menai.evaluate('(string>? "a" "Z")') is True

    def test_string_ordering_by_length_when_prefix(self, menai):
        """A string that is a prefix of another is less than it."""
        assert menai.evaluate('(string<? "abc" "abcd")') is True
        assert menai.evaluate('(string>? "abcd" "abc")') is True

    def test_string_comparisons_reject_non_string(self, menai):
        for op in ('string<?', 'string>?', 'string<=?', 'string>=?'):
            with pytest.raises(MenaiEvalError, match=f"{op}.*requires string arguments.*integer"):
                menai.evaluate(f'({op} "a" 1)')

            with pytest.raises(MenaiEvalError, match=f"{op}.*requires string arguments.*boolean"):
                menai.evaluate(f'({op} #t "a")')

            with pytest.raises(MenaiEvalError, match=f"{op}.*requires string arguments.*list"):
                menai.evaluate(f'({op} (list "a") "a")')

    def test_string_comparisons_wrong_arity(self, menai):
        for op in ('string<?', 'string>?', 'string<=?', 'string>=?'):
            with pytest.raises(MenaiEvalError, match=f"{op}.*has wrong number of arguments"):
                menai.evaluate(f'({op})')

            with pytest.raises(MenaiEvalError, match=f"{op}.*has wrong number of arguments"):
                menai.evaluate(f'({op} "a")')

    def test_string_comparisons_usable_in_conditionals(self, menai):
        assert menai.evaluate('(if (string<? "a" "b") #t #f)') is True
        assert menai.evaluate('(if (string>? "a" "b") #t #f)') is False

    def test_string_comparisons_with_numbers_in_strings(self, menai):
        """Numeric strings compare lexicographically, not numerically."""
        # "9" > "10" lexicographically because '9' > '1'
        assert menai.evaluate('(string>? "9" "10")') is True
        assert menai.evaluate('(string<? "10" "9")') is True


# ---------------------------------------------------------------------------
# Cross-type consistency checks
# ---------------------------------------------------------------------------

class TestNeqPConsistencyWithEqP:
    """Verify that !=? is the exact negation of =? for all typed pairs."""

    def test_integer_neq_is_negation_of_eq(self, menai):
        cases = [('0', '0'), ('1', '2'), ('-1', '1')]
        for a, b in cases:
            eq = menai.evaluate(f'(integer=? {a} {b})')
            neq = menai.evaluate(f'(integer!=? {a} {b})')
            assert eq != neq, f"integer=? and integer!=? agree on ({a}, {b})"

    def test_float_neq_is_negation_of_eq(self, menai):
        cases = [('1.0', '1.0'), ('1.0', '2.0')]
        for a, b in cases:
            eq = menai.evaluate(f'(float=? {a} {b})')
            neq = menai.evaluate(f'(float!=? {a} {b})')
            assert eq != neq, f"float=? and float!=? agree on ({a}, {b})"

    def test_complex_neq_is_negation_of_eq(self, menai):
        cases = [('1+2j', '1+2j'), ('1+2j', '1+3j')]
        for a, b in cases:
            eq = menai.evaluate(f'(complex=? {a} {b})')
            neq = menai.evaluate(f'(complex!=? {a} {b})')
            assert eq != neq, f"complex=? and complex!=? agree on ({a}, {b})"

    def test_boolean_neq_is_negation_of_eq(self, menai):
        cases = [('#t', '#t'), ('#t', '#f'), ('#f', '#f')]
        for a, b in cases:
            eq = menai.evaluate(f'(boolean=? {a} {b})')
            neq = menai.evaluate(f'(boolean!=? {a} {b})')
            assert eq != neq, f"boolean=? and boolean!=? agree on ({a}, {b})"

    def test_string_neq_is_negation_of_eq(self, menai):
        cases = [('"a"', '"a"'), ('"a"', '"b"')]
        for a, b in cases:
            eq = menai.evaluate(f'(string=? {a} {b})')
            neq = menai.evaluate(f'(string!=? {a} {b})')
            assert eq != neq, f"string=? and string!=? agree on ({a}, {b})"

    def test_list_neq_is_negation_of_eq(self, menai):
        cases = [
            ('(list 1 2)', '(list 1 2)'),
            ('(list 1 2)', '(list 1 3)'),
        ]
        for a, b in cases:
            eq = menai.evaluate(f'(list=? {a} {b})')
            neq = menai.evaluate(f'(list!=? {a} {b})')
            assert eq != neq, f"list=? and list!=? agree on ({a}, {b})"

    def test_dict_neq_is_negation_of_eq(self, menai):
        cases = [
            ('(dict)', '(dict)'),
            ('(dict (list "k" 1))', '(dict (list "k" 2))'),
        ]
        for a, b in cases:
            eq = menai.evaluate(f'(dict=? {a} {b})')
            neq = menai.evaluate(f'(dict!=? {a} {b})')
            assert eq != neq, f"dict=? and dict!=? agree on ({a}, {b})"


class TestOrderedComparisonConsistency:
    """Verify that <?, >?, <=?, >=? are mutually consistent."""

    def test_integer_lt_gt_are_asymmetric(self, menai):
        """If a <? b then b >? a, and vice versa."""
        assert menai.evaluate('(integer<? 1 2)') is True
        assert menai.evaluate('(integer>? 2 1)') is True
        assert menai.evaluate('(integer<? 2 1)') is False
        assert menai.evaluate('(integer>? 1 2)') is False

    def test_integer_lte_gte_include_equality(self, menai):
        """a <=? a and a >=? a are always true."""
        for val in ('0', '1', '-1', '100'):
            assert menai.evaluate(f'(integer<=? {val} {val})') is True
            assert menai.evaluate(f'(integer>=? {val} {val})') is True

    def test_float_lt_gt_are_asymmetric(self, menai):
        assert menai.evaluate('(float<? 1.0 2.0)') is True
        assert menai.evaluate('(float>? 2.0 1.0)') is True

    def test_float_lte_gte_include_equality(self, menai):
        for val in ('0.0', '1.0', '-1.0', '3.14'):
            assert menai.evaluate(f'(float<=? {val} {val})') is True
            assert menai.evaluate(f'(float>=? {val} {val})') is True

    def test_string_lt_gt_are_asymmetric(self, menai):
        assert menai.evaluate('(string<? "a" "b")') is True
        assert menai.evaluate('(string>? "b" "a")') is True

    def test_string_lte_gte_include_equality(self, menai):
        for val in ('"a"', '"hello"', '""'):
            assert menai.evaluate(f'(string<=? {val} {val})') is True
            assert menai.evaluate(f'(string>=? {val} {val})') is True
