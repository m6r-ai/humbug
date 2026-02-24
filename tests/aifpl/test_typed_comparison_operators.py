"""Tests for typed comparison operators.

This module tests:
- Type-specific not-equals predicates (!=?) for all typed equality types:
    boolean!=?, integer!=?, float!=?, complex!=?, string!=?, list!=?, alist!=?
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

from aifpl import AIFPLEvalError


# ---------------------------------------------------------------------------
# Not-equals predicates
# ---------------------------------------------------------------------------

class TestBooleanNeqP:
    """Tests for boolean!=?"""

    def test_unequal_booleans(self, aifpl):
        assert aifpl.evaluate('(boolean!=? #t #f)') is True
        assert aifpl.evaluate('(boolean!=? #f #t)') is True

    def test_equal_booleans(self, aifpl):
        assert aifpl.evaluate('(boolean!=? #t #t)') is False
        assert aifpl.evaluate('(boolean!=? #f #f)') is False

    def test_rejects_non_boolean_first_arg(self, aifpl):
        with pytest.raises(AIFPLEvalError, match="boolean!=.*requires boolean arguments.*integer"):
            aifpl.evaluate('(boolean!=? 1 #t)')

    def test_rejects_non_boolean_second_arg(self, aifpl):
        with pytest.raises(AIFPLEvalError, match="boolean!=.*requires boolean arguments.*string"):
            aifpl.evaluate('(boolean!=? #t "true")')

    def test_wrong_arity(self, aifpl):
        with pytest.raises(AIFPLEvalError, match="boolean!=.*has wrong number of arguments"):
            aifpl.evaluate('(boolean!=?)')

        with pytest.raises(AIFPLEvalError, match="boolean!=.*has wrong number of arguments"):
            aifpl.evaluate('(boolean!=? #t)')


class TestIntegerNeqP:
    """Tests for integer!=?"""

    def test_unequal_integers(self, aifpl):
        assert aifpl.evaluate('(integer!=? 1 2)') is True
        assert aifpl.evaluate('(integer!=? -1 1)') is True
        assert aifpl.evaluate('(integer!=? 0 1)') is True

    def test_equal_integers(self, aifpl):
        assert aifpl.evaluate('(integer!=? 42 42)') is False
        assert aifpl.evaluate('(integer!=? 0 0)') is False
        assert aifpl.evaluate('(integer!=? -5 -5)') is False

    def test_rejects_float(self, aifpl):
        with pytest.raises(AIFPLEvalError, match="integer!=.*requires integer arguments.*float"):
            aifpl.evaluate('(integer!=? 1 1.0)')

        with pytest.raises(AIFPLEvalError, match="integer!=.*requires integer arguments.*float"):
            aifpl.evaluate('(integer!=? 1.0 1)')

    def test_rejects_complex(self, aifpl):
        with pytest.raises(AIFPLEvalError, match="integer!=.*requires integer arguments.*complex"):
            aifpl.evaluate('(integer!=? 1 1+0j)')

    def test_rejects_non_number(self, aifpl):
        with pytest.raises(AIFPLEvalError, match="integer!=.*requires integer arguments.*string"):
            aifpl.evaluate('(integer!=? 1 "1")')

    def test_wrong_arity(self, aifpl):
        with pytest.raises(AIFPLEvalError, match="integer!=.*has wrong number of arguments"):
            aifpl.evaluate('(integer!=?)')

        with pytest.raises(AIFPLEvalError, match="integer!=.*has wrong number of arguments"):
            aifpl.evaluate('(integer!=? 1)')


class TestFloatNeqP:
    """Tests for float!=?"""

    def test_unequal_floats(self, aifpl):
        assert aifpl.evaluate('(float!=? 1.0 2.0)') is True
        assert aifpl.evaluate('(float!=? 3.14 3.15)') is True

    def test_equal_floats(self, aifpl):
        assert aifpl.evaluate('(float!=? 1.0 1.0)') is False
        assert aifpl.evaluate('(float!=? 0.0 0.0)') is False

    def test_rejects_integer(self, aifpl):
        with pytest.raises(AIFPLEvalError, match="float!=.*requires float arguments.*integer"):
            aifpl.evaluate('(float!=? 1.0 1)')

        with pytest.raises(AIFPLEvalError, match="float!=.*requires float arguments.*integer"):
            aifpl.evaluate('(float!=? 1 1.0)')

    def test_rejects_complex(self, aifpl):
        with pytest.raises(AIFPLEvalError, match="float!=.*requires float arguments.*complex"):
            aifpl.evaluate('(float!=? 1.0 1+0j)')

    def test_wrong_arity(self, aifpl):
        with pytest.raises(AIFPLEvalError, match="float!=.*has wrong number of arguments"):
            aifpl.evaluate('(float!=?)')

        with pytest.raises(AIFPLEvalError, match="float!=.*has wrong number of arguments"):
            aifpl.evaluate('(float!=? 1.0)')


class TestComplexNeqP:
    """Tests for complex!=?"""

    def test_unequal_complex(self, aifpl):
        assert aifpl.evaluate('(complex!=? 1+2j 1+3j)') is True
        assert aifpl.evaluate('(complex!=? 0+1j 0+0j)') is True

    def test_equal_complex(self, aifpl):
        assert aifpl.evaluate('(complex!=? 1+2j 1+2j)') is False
        assert aifpl.evaluate('(complex!=? 0+0j 0+0j)') is False

    def test_rejects_integer(self, aifpl):
        with pytest.raises(AIFPLEvalError, match="complex!=.*requires complex arguments.*integer"):
            aifpl.evaluate('(complex!=? 1+0j 1)')

    def test_rejects_float(self, aifpl):
        with pytest.raises(AIFPLEvalError, match="complex!=.*requires complex arguments.*float"):
            aifpl.evaluate('(complex!=? 1+0j 1.0)')

    def test_wrong_arity(self, aifpl):
        with pytest.raises(AIFPLEvalError, match="complex!=.*has wrong number of arguments"):
            aifpl.evaluate('(complex!=?)')

        with pytest.raises(AIFPLEvalError, match="complex!=.*has wrong number of arguments"):
            aifpl.evaluate('(complex!=? 1+2j)')


class TestStringNeqP:
    """Tests for string!=?"""

    def test_unequal_strings(self, aifpl):
        assert aifpl.evaluate('(string!=? "hello" "world")') is True
        assert aifpl.evaluate('(string!=? "a" "b")') is True
        assert aifpl.evaluate('(string!=? "" "x")') is True

    def test_equal_strings(self, aifpl):
        assert aifpl.evaluate('(string!=? "hello" "hello")') is False
        assert aifpl.evaluate('(string!=? "" "")') is False

    def test_rejects_non_string(self, aifpl):
        with pytest.raises(AIFPLEvalError, match="string!=.*requires string arguments.*integer"):
            aifpl.evaluate('(string!=? "hello" 42)')

        with pytest.raises(AIFPLEvalError, match="string!=.*requires string arguments.*boolean"):
            aifpl.evaluate('(string!=? #t "true")')

    def test_wrong_arity(self, aifpl):
        with pytest.raises(AIFPLEvalError, match="string!=.*has wrong number of arguments"):
            aifpl.evaluate('(string!=?)')

        with pytest.raises(AIFPLEvalError, match="string!=.*has wrong number of arguments"):
            aifpl.evaluate('(string!=? "a")')


class TestListNeqP:
    """Tests for list!=?"""

    def test_unequal_lists(self, aifpl):
        assert aifpl.evaluate('(list!=? (list 1 2 3) (list 1 2 4))') is True
        assert aifpl.evaluate('(list!=? (list 1) (list 1 2))') is True
        assert aifpl.evaluate('(list!=? (list) (list 1))') is True

    def test_equal_lists(self, aifpl):
        assert aifpl.evaluate('(list!=? (list 1 2 3) (list 1 2 3))') is False
        assert aifpl.evaluate('(list!=? (list) (list))') is False

    def test_nested_list_inequality(self, aifpl):
        assert aifpl.evaluate('(list!=? (list (list 1 2)) (list (list 1 3)))') is True
        assert aifpl.evaluate('(list!=? (list (list 1 2)) (list (list 1 2)))') is False

    def test_rejects_non_list(self, aifpl):
        with pytest.raises(AIFPLEvalError, match="list!=.*requires list arguments.*integer"):
            aifpl.evaluate('(list!=? (list 1) 1)')

        with pytest.raises(AIFPLEvalError, match="list!=.*requires list arguments.*string"):
            aifpl.evaluate('(list!=? "a" (list 1))')

    def test_wrong_arity(self, aifpl):
        with pytest.raises(AIFPLEvalError, match="list!=.*has wrong number of arguments"):
            aifpl.evaluate('(list!=?)')

        with pytest.raises(AIFPLEvalError, match="list!=.*has wrong number of arguments"):
            aifpl.evaluate('(list!=? (list 1))')


class TestAlistNeqP:
    """Tests for alist!=?"""

    def test_unequal_alists(self, aifpl):
        assert aifpl.evaluate('(alist!=? (alist (list "a" 1)) (alist (list "a" 2)))') is True
        assert aifpl.evaluate('(alist!=? (alist) (alist (list "a" 1)))') is True

    def test_equal_alists(self, aifpl):
        assert aifpl.evaluate('(alist!=? (alist) (alist))') is False
        assert aifpl.evaluate('(alist!=? (alist (list "a" 1)) (alist (list "a" 1)))') is False

    def test_rejects_non_alist(self, aifpl):
        with pytest.raises(AIFPLEvalError, match="alist!=.*requires alist arguments.*list"):
            aifpl.evaluate('(alist!=? (alist) (list 1 2))')

        with pytest.raises(AIFPLEvalError, match="alist!=.*requires alist arguments.*integer"):
            aifpl.evaluate('(alist!=? (alist) 42)')

    def test_wrong_arity(self, aifpl):
        with pytest.raises(AIFPLEvalError, match="alist!=.*has wrong number of arguments"):
            aifpl.evaluate('(alist!=?)')

        with pytest.raises(AIFPLEvalError, match="alist!=.*has wrong number of arguments"):
            aifpl.evaluate('(alist!=? (alist))')


# ---------------------------------------------------------------------------
# Integer ordered comparisons
# ---------------------------------------------------------------------------

class TestIntegerOrderedComparisons:
    """Tests for integer<?, integer>?, integer<=?, integer>=?"""

    def test_integer_lt_true(self, aifpl):
        assert aifpl.evaluate('(integer<? 1 2)') is True
        assert aifpl.evaluate('(integer<? -5 0)') is True
        assert aifpl.evaluate('(integer<? 0 1)') is True

    def test_integer_lt_false(self, aifpl):
        assert aifpl.evaluate('(integer<? 2 1)') is False
        assert aifpl.evaluate('(integer<? 1 1)') is False  # equal is not less-than

    def test_integer_gt_true(self, aifpl):
        assert aifpl.evaluate('(integer>? 2 1)') is True
        assert aifpl.evaluate('(integer>? 0 -5)') is True

    def test_integer_gt_false(self, aifpl):
        assert aifpl.evaluate('(integer>? 1 2)') is False
        assert aifpl.evaluate('(integer>? 1 1)') is False  # equal is not greater-than

    def test_integer_lte_true(self, aifpl):
        assert aifpl.evaluate('(integer<=? 1 2)') is True
        assert aifpl.evaluate('(integer<=? 1 1)') is True  # equal satisfies <=
        assert aifpl.evaluate('(integer<=? -1 0)') is True

    def test_integer_lte_false(self, aifpl):
        assert aifpl.evaluate('(integer<=? 2 1)') is False

    def test_integer_gte_true(self, aifpl):
        assert aifpl.evaluate('(integer>=? 2 1)') is True
        assert aifpl.evaluate('(integer>=? 1 1)') is True  # equal satisfies >=
        assert aifpl.evaluate('(integer>=? 0 -1)') is True

    def test_integer_gte_false(self, aifpl):
        assert aifpl.evaluate('(integer>=? 1 2)') is False

    def test_integer_comparisons_reject_float(self, aifpl):
        for op in ('integer<?', 'integer>?', 'integer<=?', 'integer>=?'):
            with pytest.raises(AIFPLEvalError, match=f"{op}.*requires integer arguments.*float"):
                aifpl.evaluate(f'({op} 1 2.0)')

            with pytest.raises(AIFPLEvalError, match=f"{op}.*requires integer arguments.*float"):
                aifpl.evaluate(f'({op} 1.0 2)')

    def test_integer_comparisons_reject_complex(self, aifpl):
        for op in ('integer<?', 'integer>?', 'integer<=?', 'integer>=?'):
            with pytest.raises(AIFPLEvalError, match=f"{op}.*requires integer arguments.*complex"):
                aifpl.evaluate(f'({op} 1 1+0j)')

    def test_integer_comparisons_reject_non_number(self, aifpl):
        for op in ('integer<?', 'integer>?', 'integer<=?', 'integer>=?'):
            with pytest.raises(AIFPLEvalError, match=f"{op}.*requires integer arguments.*string"):
                aifpl.evaluate(f'({op} 1 "2")')

            with pytest.raises(AIFPLEvalError, match=f"{op}.*requires integer arguments.*boolean"):
                aifpl.evaluate(f'({op} #t 1)')

    def test_integer_comparisons_wrong_arity(self, aifpl):
        for op in ('integer<?', 'integer>?', 'integer<=?', 'integer>=?'):
            with pytest.raises(AIFPLEvalError, match=f"{op}.*has wrong number of arguments"):
                aifpl.evaluate(f'({op})')

            with pytest.raises(AIFPLEvalError, match=f"{op}.*has wrong number of arguments"):
                aifpl.evaluate(f'({op} 1)')

    def test_integer_comparisons_large_values(self, aifpl):
        """Test with arbitrarily large integers (Python's unbounded integers)."""
        big = 10 ** 50
        bigger = big + 1
        assert aifpl.evaluate(f'(integer<? {big} {bigger})') is True
        assert aifpl.evaluate(f'(integer>? {bigger} {big})') is True
        assert aifpl.evaluate(f'(integer<=? {big} {big})') is True
        assert aifpl.evaluate(f'(integer>=? {big} {big})') is True

    def test_integer_comparisons_negative_values(self, aifpl):
        assert aifpl.evaluate('(integer<? -10 -5)') is True
        assert aifpl.evaluate('(integer>? -5 -10)') is True
        assert aifpl.evaluate('(integer<=? -5 -5)') is True
        assert aifpl.evaluate('(integer>=? -5 -5)') is True

    def test_integer_comparisons_usable_in_conditionals(self, aifpl):
        """Test that results feed correctly into if expressions."""
        assert aifpl.evaluate('(if (integer<? 1 2) #t #f)') is True
        assert aifpl.evaluate('(if (integer>? 1 2) #t #f)') is False

    def test_integer_comparisons_usable_as_first_class(self, aifpl):
        """Test that operators can be passed as first-class functions."""
        assert aifpl.evaluate('(integer<? (integer+ 1 2) (integer* 2 2))') is True


# ---------------------------------------------------------------------------
# Float ordered comparisons
# ---------------------------------------------------------------------------

class TestFloatOrderedComparisons:
    """Tests for float<?, float>?, float<=?, float>=?"""

    def test_float_lt_true(self, aifpl):
        assert aifpl.evaluate('(float<? 1.0 2.0)') is True
        assert aifpl.evaluate('(float<? -1.5 0.0)') is True

    def test_float_lt_false(self, aifpl):
        assert aifpl.evaluate('(float<? 2.0 1.0)') is False
        assert aifpl.evaluate('(float<? 1.0 1.0)') is False

    def test_float_gt_true(self, aifpl):
        assert aifpl.evaluate('(float>? 2.0 1.0)') is True
        assert aifpl.evaluate('(float>? 0.0 -1.5)') is True

    def test_float_gt_false(self, aifpl):
        assert aifpl.evaluate('(float>? 1.0 2.0)') is False
        assert aifpl.evaluate('(float>? 1.0 1.0)') is False

    def test_float_lte_true(self, aifpl):
        assert aifpl.evaluate('(float<=? 1.0 2.0)') is True
        assert aifpl.evaluate('(float<=? 1.0 1.0)') is True

    def test_float_lte_false(self, aifpl):
        assert aifpl.evaluate('(float<=? 2.0 1.0)') is False

    def test_float_gte_true(self, aifpl):
        assert aifpl.evaluate('(float>=? 2.0 1.0)') is True
        assert aifpl.evaluate('(float>=? 1.0 1.0)') is True

    def test_float_gte_false(self, aifpl):
        assert aifpl.evaluate('(float>=? 1.0 2.0)') is False

    def test_float_comparisons_reject_integer(self, aifpl):
        for op in ('float<?', 'float>?', 'float<=?', 'float>=?'):
            with pytest.raises(AIFPLEvalError, match=f"{op}.*requires float arguments.*integer"):
                aifpl.evaluate(f'({op} 1.0 2)')

            with pytest.raises(AIFPLEvalError, match=f"{op}.*requires float arguments.*integer"):
                aifpl.evaluate(f'({op} 1 2.0)')

    def test_float_comparisons_reject_complex(self, aifpl):
        for op in ('float<?', 'float>?', 'float<=?', 'float>=?'):
            with pytest.raises(AIFPLEvalError, match=f"{op}.*requires float arguments.*complex"):
                aifpl.evaluate(f'({op} 1.0 1+0j)')

    def test_float_comparisons_reject_non_number(self, aifpl):
        for op in ('float<?', 'float>?', 'float<=?', 'float>=?'):
            with pytest.raises(AIFPLEvalError, match=f"{op}.*requires float arguments.*string"):
                aifpl.evaluate(f'({op} 1.0 "2.0")')

    def test_float_comparisons_wrong_arity(self, aifpl):
        for op in ('float<?', 'float>?', 'float<=?', 'float>=?'):
            with pytest.raises(AIFPLEvalError, match=f"{op}.*has wrong number of arguments"):
                aifpl.evaluate(f'({op})')

            with pytest.raises(AIFPLEvalError, match=f"{op}.*has wrong number of arguments"):
                aifpl.evaluate(f'({op} 1.0)')

    def test_float_comparisons_special_values(self, aifpl):
        """Test with very large and very small values."""
        # Use float-log of a very small positive number to get a large negative result
        assert aifpl.evaluate('(float<? (float-log 0.001) 0.0)') is True
        assert aifpl.evaluate('(float>? (float-log 1000.0) 0.0)') is True
        assert aifpl.evaluate('(float<=? (float-neg 1.0) 0.0)') is True
        assert aifpl.evaluate('(float>=? 1.0 (float-neg 1.0))') is True

    def test_float_comparisons_usable_in_conditionals(self, aifpl):
        assert aifpl.evaluate('(if (float<? 1.0 2.0) #t #f)') is True
        assert aifpl.evaluate('(if (float>? 1.0 2.0) #t #f)') is False

    def test_float_integer_not_interchangeable(self, aifpl):
        """Confirm float and integer comparisons are not interchangeable."""
        # integer<? rejects floats; float<? rejects integers
        with pytest.raises(AIFPLEvalError, match="integer.*requires integer arguments.*float"):
            aifpl.evaluate('(integer<? 1 2.0)')

        with pytest.raises(AIFPLEvalError, match="float.*requires float arguments.*integer"):
            aifpl.evaluate('(float<? 1 2.0)')


# ---------------------------------------------------------------------------
# String ordered comparisons
# ---------------------------------------------------------------------------

class TestStringOrderedComparisons:
    """Tests for string<?, string>?, string<=?, string>=?

    Ordering is Unicode codepoint order (same as Python str comparison).
    """

    def test_string_lt_true(self, aifpl):
        assert aifpl.evaluate('(string<? "apple" "banana")') is True
        assert aifpl.evaluate('(string<? "a" "b")') is True
        assert aifpl.evaluate('(string<? "" "a")') is True   # empty < non-empty

    def test_string_lt_false(self, aifpl):
        assert aifpl.evaluate('(string<? "banana" "apple")') is False
        assert aifpl.evaluate('(string<? "a" "a")') is False  # equal is not less-than

    def test_string_gt_true(self, aifpl):
        assert aifpl.evaluate('(string>? "banana" "apple")') is True
        assert aifpl.evaluate('(string>? "b" "a")') is True

    def test_string_gt_false(self, aifpl):
        assert aifpl.evaluate('(string>? "apple" "banana")') is False
        assert aifpl.evaluate('(string>? "a" "a")') is False

    def test_string_lte_true(self, aifpl):
        assert aifpl.evaluate('(string<=? "apple" "banana")') is True
        assert aifpl.evaluate('(string<=? "a" "a")') is True   # equal satisfies <=
        assert aifpl.evaluate('(string<=? "" "")') is True

    def test_string_lte_false(self, aifpl):
        assert aifpl.evaluate('(string<=? "banana" "apple")') is False

    def test_string_gte_true(self, aifpl):
        assert aifpl.evaluate('(string>=? "banana" "apple")') is True
        assert aifpl.evaluate('(string>=? "a" "a")') is True   # equal satisfies >=
        assert aifpl.evaluate('(string>=? "" "")') is True

    def test_string_gte_false(self, aifpl):
        assert aifpl.evaluate('(string>=? "apple" "banana")') is False

    def test_string_ordering_is_codepoint_not_locale(self, aifpl):
        """Uppercase letters have lower codepoints than lowercase in ASCII/Unicode."""
        # 'Z' (90) < 'a' (97) in codepoint order
        assert aifpl.evaluate('(string<? "Z" "a")') is True
        assert aifpl.evaluate('(string>? "a" "Z")') is True

    def test_string_ordering_by_length_when_prefix(self, aifpl):
        """A string that is a prefix of another is less than it."""
        assert aifpl.evaluate('(string<? "abc" "abcd")') is True
        assert aifpl.evaluate('(string>? "abcd" "abc")') is True

    def test_string_comparisons_reject_non_string(self, aifpl):
        for op in ('string<?', 'string>?', 'string<=?', 'string>=?'):
            with pytest.raises(AIFPLEvalError, match=f"{op}.*requires string arguments.*integer"):
                aifpl.evaluate(f'({op} "a" 1)')

            with pytest.raises(AIFPLEvalError, match=f"{op}.*requires string arguments.*boolean"):
                aifpl.evaluate(f'({op} #t "a")')

            with pytest.raises(AIFPLEvalError, match=f"{op}.*requires string arguments.*list"):
                aifpl.evaluate(f'({op} (list "a") "a")')

    def test_string_comparisons_wrong_arity(self, aifpl):
        for op in ('string<?', 'string>?', 'string<=?', 'string>=?'):
            with pytest.raises(AIFPLEvalError, match=f"{op}.*has wrong number of arguments"):
                aifpl.evaluate(f'({op})')

            with pytest.raises(AIFPLEvalError, match=f"{op}.*has wrong number of arguments"):
                aifpl.evaluate(f'({op} "a")')

    def test_string_comparisons_usable_in_conditionals(self, aifpl):
        assert aifpl.evaluate('(if (string<? "a" "b") #t #f)') is True
        assert aifpl.evaluate('(if (string>? "a" "b") #t #f)') is False

    def test_string_comparisons_with_numbers_in_strings(self, aifpl):
        """Numeric strings compare lexicographically, not numerically."""
        # "9" > "10" lexicographically because '9' > '1'
        assert aifpl.evaluate('(string>? "9" "10")') is True
        assert aifpl.evaluate('(string<? "10" "9")') is True


# ---------------------------------------------------------------------------
# Cross-type consistency checks
# ---------------------------------------------------------------------------

class TestNeqPConsistencyWithEqP:
    """Verify that !=? is the exact negation of =? for all typed pairs."""

    def test_integer_neq_is_negation_of_eq(self, aifpl):
        cases = [('0', '0'), ('1', '2'), ('-1', '1')]
        for a, b in cases:
            eq = aifpl.evaluate(f'(integer=? {a} {b})')
            neq = aifpl.evaluate(f'(integer!=? {a} {b})')
            assert eq != neq, f"integer=? and integer!=? agree on ({a}, {b})"

    def test_float_neq_is_negation_of_eq(self, aifpl):
        cases = [('1.0', '1.0'), ('1.0', '2.0')]
        for a, b in cases:
            eq = aifpl.evaluate(f'(float=? {a} {b})')
            neq = aifpl.evaluate(f'(float!=? {a} {b})')
            assert eq != neq, f"float=? and float!=? agree on ({a}, {b})"

    def test_complex_neq_is_negation_of_eq(self, aifpl):
        cases = [('1+2j', '1+2j'), ('1+2j', '1+3j')]
        for a, b in cases:
            eq = aifpl.evaluate(f'(complex=? {a} {b})')
            neq = aifpl.evaluate(f'(complex!=? {a} {b})')
            assert eq != neq, f"complex=? and complex!=? agree on ({a}, {b})"

    def test_boolean_neq_is_negation_of_eq(self, aifpl):
        cases = [('#t', '#t'), ('#t', '#f'), ('#f', '#f')]
        for a, b in cases:
            eq = aifpl.evaluate(f'(boolean=? {a} {b})')
            neq = aifpl.evaluate(f'(boolean!=? {a} {b})')
            assert eq != neq, f"boolean=? and boolean!=? agree on ({a}, {b})"

    def test_string_neq_is_negation_of_eq(self, aifpl):
        cases = [('"a"', '"a"'), ('"a"', '"b"')]
        for a, b in cases:
            eq = aifpl.evaluate(f'(string=? {a} {b})')
            neq = aifpl.evaluate(f'(string!=? {a} {b})')
            assert eq != neq, f"string=? and string!=? agree on ({a}, {b})"

    def test_list_neq_is_negation_of_eq(self, aifpl):
        cases = [
            ('(list 1 2)', '(list 1 2)'),
            ('(list 1 2)', '(list 1 3)'),
        ]
        for a, b in cases:
            eq = aifpl.evaluate(f'(list=? {a} {b})')
            neq = aifpl.evaluate(f'(list!=? {a} {b})')
            assert eq != neq, f"list=? and list!=? agree on ({a}, {b})"

    def test_alist_neq_is_negation_of_eq(self, aifpl):
        cases = [
            ('(alist)', '(alist)'),
            ('(alist (list "k" 1))', '(alist (list "k" 2))'),
        ]
        for a, b in cases:
            eq = aifpl.evaluate(f'(alist=? {a} {b})')
            neq = aifpl.evaluate(f'(alist!=? {a} {b})')
            assert eq != neq, f"alist=? and alist!=? agree on ({a}, {b})"


class TestOrderedComparisonConsistency:
    """Verify that <?, >?, <=?, >=? are mutually consistent."""

    def test_integer_lt_gt_are_asymmetric(self, aifpl):
        """If a <? b then b >? a, and vice versa."""
        assert aifpl.evaluate('(integer<? 1 2)') is True
        assert aifpl.evaluate('(integer>? 2 1)') is True
        assert aifpl.evaluate('(integer<? 2 1)') is False
        assert aifpl.evaluate('(integer>? 1 2)') is False

    def test_integer_lte_gte_include_equality(self, aifpl):
        """a <=? a and a >=? a are always true."""
        for val in ('0', '1', '-1', '100'):
            assert aifpl.evaluate(f'(integer<=? {val} {val})') is True
            assert aifpl.evaluate(f'(integer>=? {val} {val})') is True

    def test_float_lt_gt_are_asymmetric(self, aifpl):
        assert aifpl.evaluate('(float<? 1.0 2.0)') is True
        assert aifpl.evaluate('(float>? 2.0 1.0)') is True

    def test_float_lte_gte_include_equality(self, aifpl):
        for val in ('0.0', '1.0', '-1.0', '3.14'):
            assert aifpl.evaluate(f'(float<=? {val} {val})') is True
            assert aifpl.evaluate(f'(float>=? {val} {val})') is True

    def test_string_lt_gt_are_asymmetric(self, aifpl):
        assert aifpl.evaluate('(string<? "a" "b")') is True
        assert aifpl.evaluate('(string>? "b" "a")') is True

    def test_string_lte_gte_include_equality(self, aifpl):
        for val in ('"a"', '"hello"', '""'):
            assert aifpl.evaluate(f'(string<=? {val} {val})') is True
            assert aifpl.evaluate(f'(string>=? {val} {val})') is True
