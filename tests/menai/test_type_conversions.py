"""Tests for integer and float type conversion functions."""

import pytest
from menai.menai_error import MenaiEvalError


class TestIntegerConversion:
    """Test the integer conversion function (FLOAT_TO_INTEGER: requires float argument)."""

    def test_integer_from_positive_float(self, menai):
        """Test integer conversion from positive float (truncates)."""
        result = menai.evaluate("(float->integer 3.7)")
        assert result == 3

    def test_integer_from_negative_float(self, menai):
        """Test integer conversion from negative float (truncates toward zero)."""
        result = menai.evaluate("(float->integer -3.7)")
        assert result == -3

    def test_integer_from_float_with_zero_decimal(self, menai):
        """Test integer conversion from float like 5.0."""
        result = menai.evaluate("(float->integer 5.0)")
        assert result == 5

    def test_integer_truncates_toward_zero(self, menai):
        """Test that integer truncates toward zero (boolean-not floor)."""
        # Positive: 3.7 -> 3 (same as floor)
        assert menai.evaluate("(float->integer 3.7)") == 3
        # Negative: -3.7 -> -3 (different from floor which would be -4)
        assert menai.evaluate("(float->integer -3.7)") == -3

    def test_integer_with_large_numbers(self, menai):
        """Test integer conversion with large numbers."""
        result = menai.evaluate("(float->integer 999999999.9)")
        assert result == 999999999

    def test_integer_with_zero(self, menai):
        """Test integer conversion with zero float."""
        assert menai.evaluate("(float->integer 0.0)") == 0

    def test_integer_from_integer(self, menai):
        """Test that integer conversion from integer raises error (requires float argument)."""
        with pytest.raises(MenaiEvalError) as exc_info:
            menai.evaluate("(float->integer 5)")
        assert "requires float argument" in str(exc_info.value).lower()

    def test_integer_from_complex_error(self, menai):
        """Test that integer conversion from complex raises error (requires float argument)."""
        with pytest.raises(MenaiEvalError) as exc_info:
            menai.evaluate("(float->integer (float->complex 3.0 0.0))")
        assert "requires float argument" in str(exc_info.value).lower()

    def test_integer_from_string_error(self, menai):
        """Test integer conversion from string raises error."""
        with pytest.raises(MenaiEvalError) as exc_info:
            menai.evaluate('(float->integer "hello")')
        assert "requires float argument" in str(exc_info.value).lower()

    def test_integer_wrong_arg_count_zero(self, menai):
        """Test integer with no arguments raises error."""
        with pytest.raises(MenaiEvalError) as exc_info:
            menai.evaluate("(float->integer)")
        assert "expected: exactly 1 argument" in str(exc_info.value).lower()
        assert "got 0" in str(exc_info.value).lower()

    def test_integer_wrong_arg_count_multiple(self, menai):
        """Test integer with multiple arguments raises error."""
        with pytest.raises(MenaiEvalError) as exc_info:
            menai.evaluate("(float->integer 1 2)")
        assert "expected: exactly 1 argument" in str(exc_info.value).lower()
        assert "got 2" in str(exc_info.value).lower()


class TestFloatConversion:
    """Test the float conversion function (INTEGER_TO_FLOAT: requires integer argument)."""

    def test_float_from_integer(self, menai):
        """Test float conversion from integer."""
        result = menai.evaluate("(integer->float 5)")
        # Verify it is recognised as a float type
        assert menai.evaluate("(float? (integer->float 5))") is True
        assert result == 5

    def test_float_wrong_arg_count_zero(self, menai):
        """Test float with no arguments raises error."""
        with pytest.raises(MenaiEvalError) as exc_info:
            menai.evaluate("(integer->float)")
        assert "expected: exactly 1 argument" in str(exc_info.value).lower()
        assert "got 0" in str(exc_info.value).lower()

    def test_float_wrong_arg_count_multiple(self, menai):
        """Test float with multiple arguments raises error."""
        with pytest.raises(MenaiEvalError) as exc_info:
            menai.evaluate("(integer->float 1 2)")
        assert "expected: exactly 1 argument" in str(exc_info.value).lower()
        assert "got 2" in str(exc_info.value).lower()

    def test_float_with_negative_numbers(self, menai):
        """Test float conversion with negative integer."""
        result = menai.evaluate("(integer->float -42)")
        assert result == -42

    def test_float_with_zero(self, menai):
        """Test float conversion with zero integer."""
        result = menai.evaluate("(integer->float 0)")
        assert result == 0

    def test_float_with_large_numbers(self, menai):
        """Test float conversion with large integer."""
        result = menai.evaluate("(integer->float 999999999)")
        assert result == 999999999

    def test_float_from_float(self, menai):
        """Test that float conversion from float raises error (requires integer argument)."""
        with pytest.raises(MenaiEvalError) as exc_info:
            menai.evaluate("(integer->float 3.14)")
        assert "requires integer argument" in str(exc_info.value).lower()

    def test_float_from_complex_error(self, menai):
        """Test that float conversion from complex raises error (requires integer argument)."""
        with pytest.raises(MenaiEvalError) as exc_info:
            menai.evaluate("(integer->float (integer->complex 3 0))")
        assert "requires integer argument" in str(exc_info.value).lower()

    def test_float_from_string_error(self, menai):
        """Test float conversion from string raises error."""
        with pytest.raises(MenaiEvalError) as exc_info:
            menai.evaluate('(integer->float "hello")')
        assert "requires integer argument" in str(exc_info.value).lower()


class TestConversionRoundTrip:
    """Test round-trip conversions between integer and float."""

    def test_integer_to_float_to_integer(self, menai):
        """Test converting integer to float and back."""
        # (integer->float 42) is integer->float, (float->integer ...) on the float result is float->integer
        result = menai.evaluate("(float->integer (integer->float 42))")
        assert result == 42

    def test_float_to_integer_to_float(self, menai):
        """Test converting float to integer and back (loses precision)."""
        # (float->integer 3.7) is float->integer giving 3, (integer->float 3) is integer->float
        # Menai returns 3 (float->integer representation of 3.0)
        result = menai.evaluate("(integer->float (float->integer 3.7))")
        assert result == 3  # Not 3.7, precision is lost


class TestTypePredicatesWithConversions:
    """Test that type predicates work correctly with conversions."""

    def test_integer_result_is_integer_type(self, menai):
        """Test that integer conversion produces integer type."""
        result = menai.evaluate("(integer? (float->integer 3.7))")
        assert result is True

    def test_float_result_is_float_type(self, menai):
        """Test that float conversion produces float type."""
        result = menai.evaluate("(float? (integer->float 5))")
        assert result is True

    def test_integer_from_float_is_not_float(self, menai):
        """Test that integer conversion from float is not a float."""
        result = menai.evaluate("(float? (float->integer 3.7))")
        assert result is False

    def test_float_from_integer_is_not_integer(self, menai):
        """Test that float conversion from integer is not an integer."""
        result = menai.evaluate("(integer? (integer->float 5))")
        assert result is False


class TestIntegerToStringRadix:
    """Test integer->string with optional radix parameter."""

    @pytest.mark.parametrize("expression,expected", [
        # Default (decimal)
        ('(integer->string 255)', '"255"'),
        ('(integer->string 0)', '"0"'),
        ('(integer->string -255)', '"-255"'),
        # Explicit decimal
        ('(integer->string 255 10)', '"255"'),
        ('(integer->string -255 10)', '"-255"'),
        # Binary
        ('(integer->string 255 2)', '"11111111"'),
        ('(integer->string 0 2)', '"0"'),
        ('(integer->string 1 2)', '"1"'),
        ('(integer->string -255 2)', '"-11111111"'),
        # Octal
        ('(integer->string 255 8)', '"377"'),
        ('(integer->string 0 8)', '"0"'),
        ('(integer->string -255 8)', '"-377"'),
        # Hex
        ('(integer->string 255 16)', '"ff"'),
        ('(integer->string 256 16)', '"100"'),
        ('(integer->string 65535 16)', '"ffff"'),
        ('(integer->string 0 16)', '"0"'),
        ('(integer->string -255 16)', '"-ff"'),
    ])
    def test_integer_to_string_radix(self, menai, expression, expected):
        """Test integer->string with various radix values."""
        assert menai.evaluate_and_format(expression) == expected

    def test_integer_to_string_invalid_radix(self, menai):
        """Test integer->string with invalid radix raises error."""
        with pytest.raises(MenaiEvalError):
            menai.evaluate('(integer->string 255 3)')
        with pytest.raises(MenaiEvalError):
            menai.evaluate('(integer->string 255 0)')

    def test_integer_to_string_first_class(self, menai):
        """Test integer->string as a first-class function (uses prelude wrapper)."""
        # Passed to map with no radix — should default to decimal
        result = menai.evaluate_and_format('(list-map integer->string (list 1 2 3))')
        assert result == '("1" "2" "3")'
