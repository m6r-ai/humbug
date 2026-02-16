"""Tests for strict type-specific equality predicates.

This module tests the strict equality predicates that require all arguments
to be of a specific type and raise errors on type mismatches:
- number=?, integer=?, float=?, complex=?
- string=? (already existed, but tested here for completeness)
- boolean=?, list=?, alist=?
"""

import pytest

from aifpl import AIFPLEvalError


class TestStrictEqualityPredicates:
    """Test strict type-specific equality predicates."""

    # ========== number=? tests ==========

    def test_number_eq_with_same_type(self, aifpl):
        """Test number=? with arguments of the same numeric type."""
        # Integers
        assert aifpl.evaluate('(number=? 42 42)') is True
        assert aifpl.evaluate('(number=? 42 43)') is False
        assert aifpl.evaluate('(number=? 1 1 1 1)') is True
        assert aifpl.evaluate('(number=? 1 1 2)') is False

        # Floats
        assert aifpl.evaluate('(number=? 3.14 3.14)') is True
        assert aifpl.evaluate('(number=? 3.14 3.15)') is False

        # Complex
        assert aifpl.evaluate('(number=? 1+2j 1+2j)') is True
        assert aifpl.evaluate('(number=? 1+2j 1+3j)') is False

    def test_number_eq_with_cross_type(self, aifpl):
        """Test number=? allows cross-type numeric comparison."""
        # Integer and float
        assert aifpl.evaluate('(number=? 1 1.0)') is True
        assert aifpl.evaluate('(number=? 2 2.0 2)') is True

        # Integer and complex
        assert aifpl.evaluate('(number=? 1 1+0j)') is True

        # Float and complex
        assert aifpl.evaluate('(number=? 1.0 1+0j)') is True

    def test_number_eq_rejects_non_numbers(self, aifpl):
        """Test number=? raises error on non-numeric arguments."""
        with pytest.raises(AIFPLEvalError, match="number=.*requires numeric arguments.*string"):
            aifpl.evaluate('(number=? 42 "hello")')

        with pytest.raises(AIFPLEvalError, match="number=.*requires numeric arguments.*boolean"):
            aifpl.evaluate('(number=? 1 #t)')

        with pytest.raises(AIFPLEvalError, match="number=.*requires numeric arguments.*list"):
            aifpl.evaluate('(number=? 1 (list 1 2))')

    def test_number_eq_requires_minimum_args(self, aifpl):
        """Test number=? requires at least 2 arguments."""
        with pytest.raises(AIFPLEvalError, match="number=.*requires at least 2 arguments"):
            aifpl.evaluate('(number=?)')

        with pytest.raises(AIFPLEvalError, match="number=.*requires at least 2 arguments"):
            aifpl.evaluate('(number=? 42)')

    # ========== integer=? tests ==========

    def test_integer_eq_with_integers(self, aifpl):
        """Test integer=? with integer arguments."""
        assert aifpl.evaluate('(integer=? 42 42)') is True
        assert aifpl.evaluate('(integer=? 42 43)') is False
        assert aifpl.evaluate('(integer=? 1 1 1 1)') is True
        assert aifpl.evaluate('(integer=? 0 0)') is True
        assert aifpl.evaluate('(integer=? -5 -5)') is True

    def test_integer_eq_rejects_floats(self, aifpl):
        """Test integer=? raises error on float arguments."""
        with pytest.raises(AIFPLEvalError, match="integer=.*requires integer arguments.*float"):
            aifpl.evaluate('(integer=? 1 1.0)')

        with pytest.raises(AIFPLEvalError, match="integer=.*requires integer arguments.*float"):
            aifpl.evaluate('(integer=? 1 2 3.0)')

    def test_integer_eq_rejects_complex(self, aifpl):
        """Test integer=? raises error on complex arguments."""
        with pytest.raises(AIFPLEvalError, match="integer=.*requires integer arguments.*complex"):
            aifpl.evaluate('(integer=? 1 1+0j)')

    def test_integer_eq_rejects_non_numbers(self, aifpl):
        """Test integer=? raises error on non-numeric arguments."""
        with pytest.raises(AIFPLEvalError, match="integer=.*requires integer arguments.*string"):
            aifpl.evaluate('(integer=? 1 "hello")')

    def test_integer_eq_requires_minimum_args(self, aifpl):
        """Test integer=? requires at least 2 arguments."""
        with pytest.raises(AIFPLEvalError, match="integer=.*requires at least 2 arguments"):
            aifpl.evaluate('(integer=?)')

        with pytest.raises(AIFPLEvalError, match="integer=.*requires at least 2 arguments"):
            aifpl.evaluate('(integer=? 42)')

    # ========== float=? tests ==========

    def test_float_eq_with_floats(self, aifpl):
        """Test float=? with float arguments."""
        assert aifpl.evaluate('(float=? 3.14 3.14)') is True
        assert aifpl.evaluate('(float=? 3.14 3.15)') is False
        assert aifpl.evaluate('(float=? 1.0 1.0 1.0)') is True
        assert aifpl.evaluate('(float=? 0.0 0.0)') is True

    def test_float_eq_rejects_integers(self, aifpl):
        """Test float=? raises error on integer arguments."""
        with pytest.raises(AIFPLEvalError, match="float=.*requires float arguments.*integer"):
            aifpl.evaluate('(float=? 1.0 1)')

        with pytest.raises(AIFPLEvalError, match="float=.*requires float arguments.*integer"):
            aifpl.evaluate('(float=? 1.0 2.0 3)')

    def test_float_eq_rejects_complex(self, aifpl):
        """Test float=? raises error on complex arguments."""
        with pytest.raises(AIFPLEvalError, match="float=.*requires float arguments.*complex"):
            aifpl.evaluate('(float=? 1.0 1+0j)')

    def test_float_eq_requires_minimum_args(self, aifpl):
        """Test float=? requires at least 2 arguments."""
        with pytest.raises(AIFPLEvalError, match="float=.*requires at least 2 arguments"):
            aifpl.evaluate('(float=?)')

        with pytest.raises(AIFPLEvalError, match="float=.*requires at least 2 arguments"):
            aifpl.evaluate('(float=? 42.0)')

    # ========== complex=? tests ==========

    def test_complex_eq_with_complex(self, aifpl):
        """Test complex=? with complex arguments."""
        assert aifpl.evaluate('(complex=? 1+2j 1+2j)') is True
        assert aifpl.evaluate('(complex=? 1+2j 1+3j)') is False
        assert aifpl.evaluate('(complex=? 1j 1j 1j)') is True
        assert aifpl.evaluate('(complex=? 0+0j 0+0j)') is True

    def test_complex_eq_rejects_integers(self, aifpl):
        """Test complex=? raises error on integer arguments."""
        with pytest.raises(AIFPLEvalError, match="complex=.*requires complex arguments.*integer"):
            aifpl.evaluate('(complex=? 1+0j 1)')

    def test_complex_eq_rejects_floats(self, aifpl):
        """Test complex=? raises error on float arguments."""
        with pytest.raises(AIFPLEvalError, match="complex=.*requires complex arguments.*float"):
            aifpl.evaluate('(complex=? 1+0j 1.0)')

    def test_complex_eq_requires_minimum_args(self, aifpl):
        """Test complex=? requires at least 2 arguments."""
        with pytest.raises(AIFPLEvalError, match="complex=.*requires at least 2 arguments"):
            aifpl.evaluate('(complex=?)')

        with pytest.raises(AIFPLEvalError, match="complex=.*requires at least 2 arguments"):
            aifpl.evaluate('(complex=? 1+0j)')

    # ========== string=? tests ==========

    def test_string_eq_with_strings(self, aifpl):
        """Test string=? with string arguments."""
        assert aifpl.evaluate('(string=? "hello" "hello")') is True
        assert aifpl.evaluate('(string=? "hello" "world")') is False
        assert aifpl.evaluate('(string=? "test" "test" "test")') is True
        assert aifpl.evaluate('(string=? "" "")') is True

    def test_string_eq_rejects_non_strings(self, aifpl):
        """Test string=? raises error on non-string arguments."""
        with pytest.raises(AIFPLEvalError, match="string=.*requires string arguments.*integer"):
            aifpl.evaluate('(string=? "hello" 42)')

        with pytest.raises(AIFPLEvalError, match="string=.*requires string arguments.*boolean"):
            aifpl.evaluate('(string=? "hello" #t)')

    # ========== boolean=? tests ==========

    def test_boolean_eq_with_booleans(self, aifpl):
        """Test boolean=? with boolean arguments."""
        assert aifpl.evaluate('(boolean=? #t #t)') is True
        assert aifpl.evaluate('(boolean=? #f #f)') is True
        assert aifpl.evaluate('(boolean=? #t #f)') is False
        assert aifpl.evaluate('(boolean=? #t #t #t)') is True
        assert aifpl.evaluate('(boolean=? #f #f #f)') is True

    def test_boolean_eq_rejects_non_booleans(self, aifpl):
        """Test boolean=? raises error on non-boolean arguments."""
        with pytest.raises(AIFPLEvalError, match="boolean=.*requires boolean arguments.*integer"):
            aifpl.evaluate('(boolean=? #t 1)')

        with pytest.raises(AIFPLEvalError, match="boolean=.*requires boolean arguments.*string"):
            aifpl.evaluate('(boolean=? #t "true")')

    def test_boolean_eq_requires_minimum_args(self, aifpl):
        """Test boolean=? requires at least 2 arguments."""
        with pytest.raises(AIFPLEvalError, match="boolean=.*requires at least 2 arguments"):
            aifpl.evaluate('(boolean=?)')

        with pytest.raises(AIFPLEvalError, match="boolean=.*requires at least 2 arguments"):
            aifpl.evaluate('(boolean=? #t)')

    # ========== list=? tests ==========

    def test_list_eq_with_lists(self, aifpl):
        """Test list=? with list arguments."""
        assert aifpl.evaluate('(list=? (list 1 2 3) (list 1 2 3))') is True
        assert aifpl.evaluate('(list=? (list 1 2) (list 1 3))') is False
        assert aifpl.evaluate('(list=? (list) (list))') is True
        assert aifpl.evaluate('(list=? (list "a") (list "a") (list "a"))') is True

    def test_list_eq_structural_equality(self, aifpl):
        """Test list=? performs structural equality."""
        # Nested lists
        assert aifpl.evaluate('(list=? (list (list 1 2) 3) (list (list 1 2) 3))') is True
        assert aifpl.evaluate('(list=? (list (list 1 2) 3) (list (list 1 3) 3))') is False

        # Different lengths
        assert aifpl.evaluate('(list=? (list 1 2) (list 1 2 3))') is False

    def test_list_eq_rejects_non_lists(self, aifpl):
        """Test list=? raises error on non-list arguments."""
        with pytest.raises(AIFPLEvalError, match="list=.*requires list arguments.*integer"):
            aifpl.evaluate('(list=? (list 1 2) 42)')

        with pytest.raises(AIFPLEvalError, match="list=.*requires list arguments.*string"):
            aifpl.evaluate('(list=? (list 1) "hello")')

    def test_list_eq_requires_minimum_args(self, aifpl):
        """Test list=? requires at least 2 arguments."""
        with pytest.raises(AIFPLEvalError, match="list=.*requires at least 2 arguments"):
            aifpl.evaluate('(list=?)')

        with pytest.raises(AIFPLEvalError, match="list=.*requires at least 2 arguments"):
            aifpl.evaluate('(list=? (list 1 2))')

    # ========== alist=? tests ==========

    def test_alist_eq_with_alists(self, aifpl):
        """Test alist=? with alist arguments."""
        # Empty alists
        assert aifpl.evaluate('(alist=? (alist) (alist))') is True

        # Same key-value pairs
        assert aifpl.evaluate('(alist=? (alist (list "a" 1)) (alist (list "a" 1)))') is True

        # Different values
        assert aifpl.evaluate('(alist=? (alist (list "a" 1)) (alist (list "a" 2)))') is False

        # Multiple pairs
        code = '''(alist=?
            (alist (list "name" "Alice") (list "age" 30))
            (alist (list "name" "Alice") (list "age" 30)))'''
        assert aifpl.evaluate(code) is True

    def test_alist_eq_order_matters(self, aifpl):
        """Test alist=? is sensitive to order (structural equality)."""
        # Different order should be different (structural comparison)
        # Note: This tests the current implementation behavior
        code1 = '(alist=? (alist (list "a" 1) (list "b" 2)) (alist (list "b" 2) (list "a" 1)))'
        result = aifpl.evaluate(code1)
        # This will be False because alists compare structurally (order matters)
        assert result is False

    def test_alist_eq_rejects_non_alists(self, aifpl):
        """Test alist=? raises error on non-alist arguments."""
        with pytest.raises(AIFPLEvalError, match="alist=.*requires alist arguments.*list"):
            aifpl.evaluate('(alist=? (alist) (list 1 2))')

        with pytest.raises(AIFPLEvalError, match="alist=.*requires alist arguments.*integer"):
            aifpl.evaluate('(alist=? (alist) 42)')

    def test_alist_eq_requires_minimum_args(self, aifpl):
        """Test alist=? requires at least 2 arguments."""
        with pytest.raises(AIFPLEvalError, match="alist=.*requires at least 2 arguments"):
            aifpl.evaluate('(alist=?)')

        with pytest.raises(AIFPLEvalError, match="alist=.*requires at least 2 arguments"):
            aifpl.evaluate('(alist=? (alist))')

    # ========== Comparison with universal = operator ==========

    def test_strict_predicates_vs_universal_equals(self, aifpl):
        """Test that strict predicates are more restrictive than =."""
        # Universal = works across types
        assert aifpl.evaluate('(= 1 1.0)') is True

        # But strict predicates reject cross-type
        with pytest.raises(AIFPLEvalError, match="integer=.*requires integer arguments"):
            aifpl.evaluate('(integer=? 1 1.0)')

        with pytest.raises(AIFPLEvalError, match="float=.*requires float arguments"):
            aifpl.evaluate('(float=? 1.0 1)')

    def test_strict_predicates_provide_type_checking(self, aifpl):
        """Test that strict predicates serve as type assertions."""
        # string=? ensures all args are strings
        with pytest.raises(AIFPLEvalError, match="string=.*requires string arguments"):
            aifpl.evaluate('(string=? "hello" 123)')

        # boolean=? ensures all args are booleans
        with pytest.raises(AIFPLEvalError, match="boolean=.*requires boolean arguments"):
            aifpl.evaluate('(boolean=? #t 1)')

        # This makes strict predicates useful for catching type errors early

    # ========== Edge cases and error messages ==========

    def test_error_messages_include_position(self, aifpl):
        """Test that error messages indicate which argument failed."""
        # First argument wrong type
        with pytest.raises(AIFPLEvalError, match="position 1"):
            aifpl.evaluate('(integer=? 1.0 1)')

        # Second argument wrong type
        with pytest.raises(AIFPLEvalError, match="position 2"):
            aifpl.evaluate('(integer=? 1 1.0)')

        # Third argument wrong type
        with pytest.raises(AIFPLEvalError, match="position 3"):
            aifpl.evaluate('(integer=? 1 1 1.0)')

    def test_all_strict_predicates_with_many_args(self, aifpl):
        """Test all strict predicates work with more than 2 arguments."""
        assert aifpl.evaluate('(number=? 1 1 1 1 1)') is True
        assert aifpl.evaluate('(integer=? 1 1 1 1)') is True
        assert aifpl.evaluate('(float=? 1.0 1.0 1.0)') is True
        assert aifpl.evaluate('(complex=? 1j 1j 1j)') is True
        assert aifpl.evaluate('(string=? "a" "a" "a" "a")') is True
        assert aifpl.evaluate('(boolean=? #t #t #t)') is True
        assert aifpl.evaluate('(list=? (list) (list) (list))') is True
        assert aifpl.evaluate('(alist=? (alist) (alist) (alist))') is True
