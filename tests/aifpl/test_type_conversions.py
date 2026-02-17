"""Tests for integer and float type conversion functions."""

import pytest
from aifpl.aifpl_error import AIFPLEvalError


class TestIntegerConversion:
    """Test the integer conversion function."""

    def test_integer_from_integer(self, aifpl):
        """Test integer conversion from integer (no change)."""
        result = aifpl.evaluate("(integer 5)")
        assert result == 5
        # Verify it's actually an integer type
        assert isinstance(result, int)

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

    def test_integer_from_complex_with_zero_imaginary(self, aifpl):
        """Test integer conversion from complex with zero imaginary part."""
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate("(integer (complex 3 0))")
        assert "'integer' does not support complex numbers" in str(exc_info.value).lower()

    def test_integer_from_complex_with_tiny_imaginary(self, aifpl):
        """Test integer conversion from complex with tiny imaginary part."""
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate("(integer (complex 3.0 0.00000000001))")
        assert "'integer' does not support complex numbers" in str(exc_info.value).lower()

    def test_integer_from_complex_with_nonzero_imaginary_error(self, aifpl):
        """Test integer conversion from complex with non-zero imaginary part raises error."""
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate("(integer (complex 3 4))")
        assert "'integer' does not support complex numbers" in str(exc_info.value).lower()

    def test_integer_from_string_error(self, aifpl):
        """Test integer conversion from string raises error."""
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate('(integer "hello")')
        assert "requires real number arguments" in str(exc_info.value).lower()
        assert "string" in str(exc_info.value).lower()

    def test_integer_wrong_arg_count_zero(self, aifpl):
        """Test integer with no arguments raises error."""
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate("(integer)")
        assert "requires exactly 1 argument" in str(exc_info.value).lower()
        assert "got 0" in str(exc_info.value).lower()

    def test_integer_wrong_arg_count_multiple(self, aifpl):
        """Test integer with multiple arguments raises error."""
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate("(integer 1 2)")
        assert "requires exactly 1 argument" in str(exc_info.value).lower()
        assert "got 2" in str(exc_info.value).lower()

    def test_integer_truncates_toward_zero(self, aifpl):
        """Test that integer truncates toward zero (not floor)."""
        # Positive: 3.7 -> 3 (same as floor)
        assert aifpl.evaluate("(integer 3.7)") == 3
        # Negative: -3.7 -> -3 (different from floor which would be -4)
        assert aifpl.evaluate("(integer -3.7)") == -3
        assert aifpl.evaluate("(floor -3.7)") == -4

    def test_integer_with_large_numbers(self, aifpl):
        """Test integer conversion with large numbers."""
        result = aifpl.evaluate("(integer 999999999.9)")
        assert result == 999999999

    def test_integer_with_zero(self, aifpl):
        """Test integer conversion with zero."""
        assert aifpl.evaluate("(integer 0)") == 0
        assert aifpl.evaluate("(integer 0.0)") == 0


class TestFloatConversion:
    """Test the float conversion function."""

    def test_float_from_float(self, aifpl):
        """Test float conversion from float (no change)."""
        result = aifpl.evaluate("(float 3.14)")
        assert result == 3.14
        assert isinstance(result, float)

    def test_float_from_integer(self, aifpl):
        """Test float conversion from integer."""
        result = aifpl.evaluate("(float 5)")
        # Note: AIFPL currently converts 5.0 to 5 when returning Python values
        assert result == 5

    def test_float_from_float_with_zero_decimal(self, aifpl):
        """Test float conversion from float like 5.0 (stays as float)."""
        result = aifpl.evaluate("(float 5.0)")
        # Note: AIFPL currently converts 5.0 to 5 when returning Python values
        assert result == 5

    def test_float_from_complex_with_zero_imaginary(self, aifpl):
        """Test float conversion from complex with zero imaginary part."""
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate("(float (complex 3 0))")
        assert "'float' does not support complex numbers" in str(exc_info.value).lower()

    def test_float_from_complex_with_tiny_imaginary(self, aifpl):
        """Test float conversion from complex with tiny imaginary part."""
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate("(float (complex 3.14 0.00000000001))")
        assert "'float' does not support complex numbers" in str(exc_info.value).lower()

    def test_float_from_complex_with_nonzero_imaginary_error(self, aifpl):
        """Test float conversion from complex with non-zero imaginary part raises error."""
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate("(float (complex 3 4))")
        assert "'float' does not support complex numbers" in str(exc_info.value).lower()

    def test_float_from_string_error(self, aifpl):
        """Test float conversion from string raises error."""
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate('(float "hello")')
        assert "requires real number arguments" in str(exc_info.value).lower()
        assert "string" in str(exc_info.value).lower()

    def test_float_wrong_arg_count_zero(self, aifpl):
        """Test float with no arguments raises error."""
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate("(float)")
        assert "requires exactly 1 argument" in str(exc_info.value).lower()
        assert "got 0" in str(exc_info.value).lower()

    def test_float_wrong_arg_count_multiple(self, aifpl):
        """Test float with multiple arguments raises error."""
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate("(float 1 2)")
        assert "requires exactly 1 argument" in str(exc_info.value).lower()
        assert "got 2" in str(exc_info.value).lower()

    def test_float_with_negative_numbers(self, aifpl):
        """Test float conversion with negative numbers."""
        result = aifpl.evaluate("(float -42)")
        # Note: AIFPL currently converts -42.0 to -42 when returning Python values
        assert result == -42

    def test_float_with_zero(self, aifpl):
        """Test float conversion with zero."""
        result = aifpl.evaluate("(float 0)")
        # Note: AIFPL currently converts 0.0 to 0 when returning Python values
        assert result == 0

    def test_float_with_large_numbers(self, aifpl):
        """Test float conversion with large numbers."""
        result = aifpl.evaluate("(float 999999999)")
        # Note: AIFPL currently converts 999999999.0 to 999999999 when returning Python values
        assert result == 999999999


class TestConversionRoundTrip:
    """Test round-trip conversions between integer and float."""

    def test_integer_to_float_to_integer(self, aifpl):
        """Test converting integer to float and back."""
        result = aifpl.evaluate("(integer (float 42))")
        assert result == 42

    def test_float_to_integer_to_float(self, aifpl):
        """Test converting float to integer and back (loses precision)."""
        result = aifpl.evaluate("(float (integer 3.7))")
        # Note: AIFPL currently converts 3.0 to 3 when returning Python values
        assert result == 3  # Not 3.7, precision is lost

    def test_conversion_with_arithmetic(self, aifpl):
        """Test conversions combined with arithmetic."""
        # (integer (+ 2.5 2.5)) should be 5
        result = aifpl.evaluate("(integer (+ 2.5 2.5))")
        assert result == 5

        # (float (+ 2 3)) should be 5.0
        result = aifpl.evaluate("(float (+ 2 3))")
        # Note: AIFPL currently converts 5.0 to 5 when returning Python values
        assert result == 5


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
