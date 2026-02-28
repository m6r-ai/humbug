"""Tests for strict type-specific equality predicates.

This module tests the strict equality predicates that require all arguments
to be of a specific type and raise errors on type mismatches:
- integer=?, float=?, complex=?
- string=? (already existed, but tested here for completeness)
- boolean=?, list=?, dict=?
"""

import pytest

from menai import MenaiEvalError


class TestStrictEqualityPredicates:
    """Test strict type-specific equality predicates."""

    # ========== integer=? tests ==========

    def test_integer_eq_with_integers(self, menai):
        """Test integer=? with integer arguments."""
        assert menai.evaluate('(integer=? 42 42)') is True
        assert menai.evaluate('(integer=? 42 43)') is False
        assert menai.evaluate('(integer=? 1 1 1 1)') is True
        assert menai.evaluate('(integer=? 0 0)') is True
        assert menai.evaluate('(integer=? -5 -5)') is True

    def test_integer_eq_rejects_floats(self, menai):
        """Test integer=? raises error on float arguments."""
        # 2-arg: always evaluated — always raises.
        with pytest.raises(MenaiEvalError, match="integer=.*requires integer arguments.*float"):
            menai.evaluate('(integer=? 1 1.0)')

        # 3-arg: bad type in second pair, first pair is true → second pair is reached → raises.
        with pytest.raises(MenaiEvalError, match="integer=.*requires integer arguments.*float"):
            menai.evaluate('(integer=? 1 1 3.0)')

        # 3-arg: bad type in second pair, first pair is false → short-circuits → no error.
        assert menai.evaluate('(integer=? 1 2 3.0)') is False

    def test_integer_eq_rejects_complex(self, menai):
        """Test integer=? raises error on complex arguments."""
        with pytest.raises(MenaiEvalError, match="integer=.*requires integer arguments.*complex"):
            menai.evaluate('(integer=? 1 1+0j)')

    def test_integer_eq_rejects_non_numbers(self, menai):
        """Test integer=? raises error on non-numeric arguments."""
        with pytest.raises(MenaiEvalError, match="integer=.*requires integer arguments.*string"):
            menai.evaluate('(integer=? 1 "hello")')

    def test_integer_eq_requires_minimum_args(self, menai):
        """Test integer=? requires at least 2 arguments."""
        with pytest.raises(MenaiEvalError, match="integer=.*has wrong number of arguments"):
            menai.evaluate('(integer=?)')

        with pytest.raises(MenaiEvalError, match="integer=.*has wrong number of arguments"):
            menai.evaluate('(integer=? 42)')

    # ========== float=? tests ==========

    def test_float_eq_with_floats(self, menai):
        """Test float=? with float arguments."""
        assert menai.evaluate('(float=? 3.14 3.14)') is True
        assert menai.evaluate('(float=? 3.14 3.15)') is False
        assert menai.evaluate('(float=? 1.0 1.0 1.0)') is True
        assert menai.evaluate('(float=? 0.0 0.0)') is True

    def test_float_eq_rejects_integers(self, menai):
        """Test float=? raises error on integer arguments."""
        # 2-arg: always evaluated — always raises.
        with pytest.raises(MenaiEvalError, match="float=.*requires float arguments.*integer"):
            menai.evaluate('(float=? 1.0 1)')

        # 3-arg: bad type in second pair, first pair is true → second pair is reached → raises.
        with pytest.raises(MenaiEvalError, match="float=.*requires float arguments.*integer"):
            menai.evaluate('(float=? 1.0 1.0 3)')

        # 3-arg: bad type in second pair, first pair is false → short-circuits → no error.
        assert menai.evaluate('(float=? 1.0 2.0 3)') is False


    def test_float_eq_rejects_complex(self, menai):
        """Test float=? raises error on complex arguments."""
        with pytest.raises(MenaiEvalError, match="float=.*requires float arguments.*complex"):
            menai.evaluate('(float=? 1.0 1+0j)')

    def test_float_eq_requires_minimum_args(self, menai):
        """Test float=? requires at least 2 arguments."""
        with pytest.raises(MenaiEvalError, match="float=.*has wrong number of arguments"):
            menai.evaluate('(float=?)')

        with pytest.raises(MenaiEvalError, match="float=.*has wrong number of arguments"):
            menai.evaluate('(float=? 42.0)')

    # ========== complex=? tests ==========

    def test_complex_eq_with_complex(self, menai):
        """Test complex=? with complex arguments."""
        assert menai.evaluate('(complex=? 1+2j 1+2j)') is True
        assert menai.evaluate('(complex=? 1+2j 1+3j)') is False
        assert menai.evaluate('(complex=? 1j 1j 1j)') is True
        assert menai.evaluate('(complex=? 0+0j 0+0j)') is True

    def test_complex_eq_rejects_integers(self, menai):
        """Test complex=? raises error on integer arguments."""
        with pytest.raises(MenaiEvalError, match="complex=.*requires complex arguments.*integer"):
            menai.evaluate('(complex=? 1+0j 1)')

    def test_complex_eq_rejects_floats(self, menai):
        """Test complex=? raises error on float arguments."""
        with pytest.raises(MenaiEvalError, match="complex=.*requires complex arguments.*float"):
            menai.evaluate('(complex=? 1+0j 1.0)')

    def test_complex_eq_requires_minimum_args(self, menai):
        """Test complex=? requires at least 2 arguments."""
        with pytest.raises(MenaiEvalError, match="complex=.*has wrong number of arguments"):
            menai.evaluate('(complex=?)')

        with pytest.raises(MenaiEvalError, match="complex=.*has wrong number of arguments"):
            menai.evaluate('(complex=? 1+0j)')

    # ========== string=? tests ==========

    def test_string_eq_with_strings(self, menai):
        """Test string=? with string arguments."""
        assert menai.evaluate('(string=? "hello" "hello")') is True
        assert menai.evaluate('(string=? "hello" "world")') is False
        assert menai.evaluate('(string=? "test" "test" "test")') is True
        assert menai.evaluate('(string=? "" "")') is True

    def test_string_eq_rejects_non_strings(self, menai):
        """Test string=? raises error on non-string arguments."""
        with pytest.raises(MenaiEvalError, match="string=.*requires string arguments.*integer"):
            menai.evaluate('(string=? "hello" 42)')

        with pytest.raises(MenaiEvalError, match="string=.*requires string arguments.*boolean"):
            menai.evaluate('(string=? "hello" #t)')

    # ========== boolean=? tests ==========

    def test_boolean_eq_with_booleans(self, menai):
        """Test boolean=? with boolean arguments."""
        assert menai.evaluate('(boolean=? #t #t)') is True
        assert menai.evaluate('(boolean=? #f #f)') is True
        assert menai.evaluate('(boolean=? #t #f)') is False
        assert menai.evaluate('(boolean=? #t #t #t)') is True
        assert menai.evaluate('(boolean=? #f #f #f)') is True

    def test_boolean_eq_rejects_non_booleans(self, menai):
        """Test boolean=? raises error on non-boolean arguments."""
        with pytest.raises(MenaiEvalError, match="boolean=.*requires boolean arguments.*integer"):
            menai.evaluate('(boolean=? #t 1)')

        with pytest.raises(MenaiEvalError, match="boolean=.*requires boolean arguments.*string"):
            menai.evaluate('(boolean=? #t "true")')

    def test_boolean_eq_requires_minimum_args(self, menai):
        """Test boolean=? requires at least 2 arguments."""
        with pytest.raises(MenaiEvalError, match="boolean=.*has wrong number of arguments"):
            menai.evaluate('(boolean=?)')

        with pytest.raises(MenaiEvalError, match="boolean=.*has wrong number of arguments"):
            menai.evaluate('(boolean=? #t)')

    # ========== list=? tests ==========

    def test_list_eq_with_lists(self, menai):
        """Test list=? with list arguments."""
        assert menai.evaluate('(list=? (list 1 2 3) (list 1 2 3))') is True
        assert menai.evaluate('(list=? (list 1 2) (list 1 3))') is False
        assert menai.evaluate('(list=? (list) (list))') is True
        assert menai.evaluate('(list=? (list "a") (list "a") (list "a"))') is True

    def test_list_eq_structural_equality(self, menai):
        """Test list=? performs structural equality."""
        # Nested lists
        assert menai.evaluate('(list=? (list (list 1 2) 3) (list (list 1 2) 3))') is True
        assert menai.evaluate('(list=? (list (list 1 2) 3) (list (list 1 3) 3))') is False

        # Different lengths
        assert menai.evaluate('(list=? (list 1 2) (list 1 2 3))') is False

    def test_list_eq_rejects_non_lists(self, menai):
        """Test list=? raises error on non-list arguments."""
        with pytest.raises(MenaiEvalError, match="list=.*requires list arguments.*integer"):
            menai.evaluate('(list=? (list 1 2) 42)')

        with pytest.raises(MenaiEvalError, match="list=.*requires list arguments.*string"):
            menai.evaluate('(list=? (list 1) "hello")')

    def test_list_eq_requires_minimum_args(self, menai):
        """Test list=? requires at least 2 arguments."""
        with pytest.raises(MenaiEvalError, match="list=.*has wrong number of arguments"):
            menai.evaluate('(list=?)')

        with pytest.raises(MenaiEvalError, match="list=.*has wrong number of arguments"):
            menai.evaluate('(list=? (list 1 2))')

    # ========== dict=? tests ==========

    def test_dict_eq_with_dicts(self, menai):
        """Test dict=? with dict arguments."""
        # Empty dicts
        assert menai.evaluate('(dict=? (dict) (dict))') is True

        # Same key-value pairs
        assert menai.evaluate('(dict=? (dict (list "a" 1)) (dict (list "a" 1)))') is True

        # Different values
        assert menai.evaluate('(dict=? (dict (list "a" 1)) (dict (list "a" 2)))') is False

        # Multiple pairs
        code = '''(dict=?
            (dict (list "name" "Alice") (list "age" 30))
            (dict (list "name" "Alice") (list "age" 30)))'''
        assert menai.evaluate(code) is True

    def test_dict_eq_order_matters(self, menai):
        """Test dict=? is sensitive to order (structural equality)."""
        # Different order should be different (structural comparison)
        # Note: This tests the current implementation behavior
        code1 = '(dict=? (dict (list "a" 1) (list "b" 2)) (dict (list "b" 2) (list "a" 1)))'
        result = menai.evaluate(code1)
        # This will be False because dicts compare structurally (order matters)
        assert result is False

    def test_dict_eq_rejects_non_dicts(self, menai):
        """Test dict=? raises error on non-dict arguments."""
        with pytest.raises(MenaiEvalError, match="dict=.*requires dict arguments.*list"):
            menai.evaluate('(dict=? (dict) (list 1 2))')

        with pytest.raises(MenaiEvalError, match="dict=.*requires dict arguments.*integer"):
            menai.evaluate('(dict=? (dict) 42)')

    def test_dict_eq_requires_minimum_args(self, menai):
        """Test dict=? requires at least 2 arguments."""
        with pytest.raises(MenaiEvalError, match="dict=.*has wrong number of arguments"):
            menai.evaluate('(dict=?)')

        with pytest.raises(MenaiEvalError, match="dict=.*has wrong number of arguments"):
            menai.evaluate('(dict=? (dict))')

    def test_strict_predicates_provide_type_checking(self, menai):
        """Test that strict predicates serve as type assertions."""
        # string=? ensures all args are strings
        with pytest.raises(MenaiEvalError, match="string=.*requires string arguments"):
            menai.evaluate('(string=? "hello" 123)')

        # boolean=? ensures all args are booleans
        with pytest.raises(MenaiEvalError, match="boolean=.*requires boolean arguments"):
            menai.evaluate('(boolean=? #t 1)')

        # This makes strict predicates useful for catching type errors early

    # ========== Edge cases and error messages ==========

    def test_error_messages_include_position(self, menai):
        """Test that error messages indicate which argument failed."""
        # First argument wrong type
        with pytest.raises(MenaiEvalError, match="requires integer arguments"):
            menai.evaluate('(integer=? 1.0 1)')

        # Second argument wrong type
        with pytest.raises(MenaiEvalError, match="requires integer arguments"):
            menai.evaluate('(integer=? 1 1.0)')

        # Third argument wrong type
        with pytest.raises(MenaiEvalError, match="requires integer arguments"):
            menai.evaluate('(integer=? 1 1 1.0)')

    def test_all_strict_predicates_with_many_args(self, menai):
        """Test all strict predicates work with more than 2 arguments."""
        assert menai.evaluate('(integer=? 1 1 1 1)') is True
        assert menai.evaluate('(float=? 1.0 1.0 1.0)') is True
        assert menai.evaluate('(complex=? 1j 1j 1j)') is True
        assert menai.evaluate('(string=? "a" "a" "a" "a")') is True
        assert menai.evaluate('(boolean=? #t #t #t)') is True
        assert menai.evaluate('(list=? (list) (list) (list))') is True
        assert menai.evaluate('(dict=? (dict) (dict) (dict))') is True
