"""Tests for integer and float type conversion functions."""

import pytest
from aifpl.aifpl_error import AIFPLEvalError


class TestIntegerConversion:
    """Test the integer conversion function (FLOAT_TO_INTEGER: requires float argument)."""

    def test_integer_from_positive_float(self, aifpl):
        """Test integer conversion from positive float (truncates)."""
        result = aifpl.evaluate("(integer 3.7)")
        assert result == 3

    def test_integer_from_negative_float(self, aifpl):
        """Test integer conversion from negative float (truncates toward zero)."""
        result = aifpl.evaluate("(integer -3.7)")
        assert result == -3

    def test_integer_from_float_with_zero_decimal(self, aifpl):
        """Test integer conversion from float like 5.0."""
        result = aifpl.evaluate("(integer 5.0)")
        assert result == 5

    def test_integer_truncates_toward_zero(self, aifpl):
        """Test that integer truncates toward zero (not floor)."""
        # Positive: 3.7 -> 3 (same as floor)
        assert aifpl.evaluate("(integer 3.7)") == 3
        # Negative: -3.7 -> -3 (different from floor which would be -4)
        assert aifpl.evaluate("(integer -3.7)") == -3

    def test_integer_with_large_numbers(self, aifpl):
        """Test integer conversion with large numbers."""
        result = aifpl.evaluate("(integer 999999999.9)")
        assert result == 999999999

    def test_integer_with_zero(self, aifpl):
        """Test integer conversion with zero float."""
        assert aifpl.evaluate("(integer 0.0)") == 0

    def test_integer_from_integer(self, aifpl):
        """Test that integer conversion from integer raises error (requires float argument)."""
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate("(integer 5)")
        assert "requires float argument" in str(exc_info.value).lower()

    def test_integer_from_complex_error(self, aifpl):
        """Test that integer conversion from complex raises error (requires float argument)."""
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate("(integer (complex 3.0 0.0))")
        assert "requires float argument" in str(exc_info.value).lower()

    def test_integer_from_string_error(self, aifpl):
        """Test integer conversion from string raises error."""
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate('(integer "hello")')
        assert "requires float argument" in str(exc_info.value).lower()

    def test_integer_wrong_arg_count_zero(self, aifpl):
        """Test integer with no arguments raises error."""
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate("(integer)")
        assert "expected: exactly 1 argument" in str(exc_info.value).lower()
        assert "got 0" in str(exc_info.value).lower()

    def test_integer_wrong_arg_count_multiple(self, aifpl):
        """Test integer with multiple arguments raises error."""
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate("(integer 1 2)")
        assert "expected: exactly 1 argument" in str(exc_info.value).lower()
        assert "got 2" in str(exc_info.value).lower()


class TestFloatConversion:
    """Test the float conversion function (INTEGER_TO_FLOAT: requires integer argument)."""

    def test_float_from_integer(self, aifpl):
        """Test float conversion from integer."""
        result = aifpl.evaluate("(float 5)")
        # Verify it is recognised as a float type
        assert aifpl.evaluate("(float? (float 5))") is True
        assert result == 5

    def test_float_wrong_arg_count_zero(self, aifpl):
        """Test float with no arguments raises error."""
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate("(float)")
        assert "expected: exactly 1 argument" in str(exc_info.value).lower()
        assert "got 0" in str(exc_info.value).lower()

    def test_float_wrong_arg_count_multiple(self, aifpl):
        """Test float with multiple arguments raises error."""
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate("(float 1 2)")
        assert "expected: exactly 1 argument" in str(exc_info.value).lower()
        assert "got 2" in str(exc_info.value).lower()

    def test_float_with_negative_numbers(self, aifpl):
        """Test float conversion with negative integer."""
        result = aifpl.evaluate("(float -42)")
        assert result == -42

    def test_float_with_zero(self, aifpl):
        """Test float conversion with zero integer."""
        result = aifpl.evaluate("(float 0)")
        assert result == 0

    def test_float_with_large_numbers(self, aifpl):
        """Test float conversion with large integer."""
        result = aifpl.evaluate("(float 999999999)")
        assert result == 999999999

    def test_float_from_float(self, aifpl):
        """Test that float conversion from float raises error (requires integer argument)."""
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate("(float 3.14)")
        assert "requires integer argument" in str(exc_info.value).lower()

    def test_float_from_complex_error(self, aifpl):
        """Test that float conversion from complex raises error (requires integer argument)."""
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate("(float (complex 3 0))")
        assert "requires integer argument" in str(exc_info.value).lower()

    def test_float_from_string_error(self, aifpl):
        """Test float conversion from string raises error."""
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate('(float "hello")')
        assert "requires integer argument" in str(exc_info.value).lower()


class TestConversionRoundTrip:
    """Test round-trip conversions between integer and float."""

    def test_integer_to_float_to_integer(self, aifpl):
        """Test converting integer to float and back."""
        # (float 42) is integer->float, (integer ...) on the float result is float->integer
        result = aifpl.evaluate("(integer (float 42))")
        assert result == 42

    def test_float_to_integer_to_float(self, aifpl):
        """Test converting float to integer and back (loses precision)."""
        # (integer 3.7) is float->integer giving 3, (float 3) is integer->float
        # AIFPL returns 3 (integer representation of 3.0)
        result = aifpl.evaluate("(float (integer 3.7))")
        assert result == 3  # Not 3.7, precision is lost


class TestTypePredicatesWithConversions:
    """Test that type predicates work correctly with conversions."""

    def test_integer_result_is_integer_type(self, aifpl):
        """Test that integer conversion produces integer type."""
        result = aifpl.evaluate("(integer? (integer 3.7))")
        assert result is True

    def test_float_result_is_float_type(self, aifpl):
        """Test that float conversion produces float type."""
        result = aifpl.evaluate("(float? (float 5))")
        assert result is True

    def test_integer_from_float_is_not_float(self, aifpl):
        """Test that integer conversion from float is not a float."""
        result = aifpl.evaluate("(float? (integer 3.7))")
        assert result is False

    def test_float_from_integer_is_not_integer(self, aifpl):
        """Test that float conversion from integer is not an integer."""
        result = aifpl.evaluate("(integer? (float 5))")
        assert result is False

    def test_both_are_numbers(self, aifpl):
        """Test that both conversions produce numbers."""
        assert aifpl.evaluate("(number? (integer 3.7))") is True
        assert aifpl.evaluate("(number? (float 5))") is True
