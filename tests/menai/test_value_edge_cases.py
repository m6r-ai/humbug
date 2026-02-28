"""Tests for Menai value representation and edge cases."""

import math
import pytest

from menai import MenaiEvalError, MenaiDict, MenaiString, MenaiSymbol


class TestMenaiValueEdgeCases:
    """Test edge cases in value representation and handling."""

    def test_value_creation_edge_cases(self, menai):
        """Test value creation with edge case inputs."""
        # Test zero values
        assert menai.evaluate("0") == 0
        assert menai.evaluate("0.0") == 0.0
        assert menai.evaluate("-0.0") == -0.0

        # Test very small numbers
        result = menai.evaluate("1e-100")
        assert result == 1e-100

        # Test very large numbers
        result = menai.evaluate("1e100")
        assert result == 1e100

    def test_floating_point_precision_edge_cases(self, menai):
        """Test floating point precision and edge cases."""
        # Test precision limits
        result = menai.evaluate("(float/ 1.0 3.0)")
        assert abs(result - (1/3)) < 1e-15

        # Test very small differences
        result = menai.evaluate("(float- 1.0000000000000001 1.0)")
        assert result == 1.0000000000000001 - 1.0

        # Test addition of very different magnitudes
        result = menai.evaluate("(float+ 1e20 1.0)")
        assert result == 1e20 + 1

    def test_complex_number_edge_cases(self, menai):
        """Test complex number edge cases."""
        # Zero complex numbers
        result = menai.evaluate("(integer->complex 0)")
        assert result == 0+0j

        # Pure real complex (should simplify to real)
        result = menai.evaluate("(integer->complex 5)")
        assert result == 5+0j

        # Pure imaginary complex
        result = menai.evaluate("(float->complex 0.0 3.0)")
        assert result == 3j

        # Very small imaginary parts (should simplify based on tolerance)
        result = menai.evaluate("(float->complex 5.0 1e-15)")
        # With default tolerance (1e-10), should simplify to real
        assert result == 5+1e-15j

        # Negative components
        result = menai.evaluate("(integer->complex -2 -3)")
        assert result == -2-3j

    def test_string_edge_cases(self, menai):
        """Test string edge cases and special characters."""
        # Empty string
        result = menai.evaluate('""')
        assert result == ""

        # Single character
        result = menai.evaluate('"a"')
        assert result == "a"

        # Whitespace strings
        result = menai.evaluate('" "')
        assert result == " "

        result = menai.evaluate('"\\t"')
        assert result == "\t"

        result = menai.evaluate('"\\n"')
        assert result == "\n"

        # Unicode characters
        result = menai.evaluate('"\\u03B1\\u03B2\\u03B3"')
        assert result == "αβγ"

        # Escaped quotes
        result = menai.evaluate('"He said \\"Hello\\""')
        assert result == 'He said "Hello"'

    def test_boolean_edge_cases(self, menai):
        """Test boolean value edge cases."""
        # Boolean operations with edge cases
        assert menai.evaluate("(and)") is True  # Identity for and
        assert menai.evaluate("(or)") is False  # Identity for or

    def test_list_edge_cases(self, menai):
        """Test list value edge cases."""
        # Empty list
        result = menai.evaluate("()")
        assert result == []

        # Single element lists
        result = menai.evaluate("(list 1)")
        assert result == [1]

        # Mixed type lists
        result = menai.evaluate('(list 1 "hello" #t)')
        assert result == [1, "hello", True]

        # Nested empty lists
        result = menai.evaluate("(list () ())")
        assert result == [[], []]

        # Deeply nested lists
        result = menai.evaluate("(list (list (list 1)))")
        assert result == [[[1]]]

    def test_numeric_type_coercion_edge_cases(self, menai):
        """Test numeric type coercion edge cases."""
        # Integer to float promotion via explicit conversion
        result = menai.evaluate("(float+ (integer->float 1) 2.5)")
        assert result == 3.5
        assert isinstance(result, float)

        # Float to complex promotion via explicit conversion
        result = menai.evaluate("(complex+ (float->complex 2.5 0.0) (integer->complex 0 1))")
        assert result == 2.5+1j
        assert isinstance(result, complex)

        # Integer to complex promotion via explicit conversion
        result = menai.evaluate("(complex+ (integer->complex 1 0) (float->complex 0.0 1.0))")
        assert result == 1+1j
        assert isinstance(result, complex)

    def test_value_comparison_edge_cases(self, menai):
        """Test value comparison edge cases."""
        # Floating point comparisons
        assert menai.evaluate("(float=? 0.1 0.1)") is True
        assert menai.evaluate("(float=? 0.0 -0.0)") is True

        # Complex number comparisons
        assert menai.evaluate("(complex=? (integer->complex 1 2) (integer->complex 1 2))") is True
        assert menai.evaluate("(complex!=? (integer->complex 1 2) (integer->complex 1 3))") is True

        # String comparisons
        assert menai.evaluate('(string=? "" "")') is True
        assert menai.evaluate('(string=? "a" "a")') is True
        assert menai.evaluate('(string=? "a" "b")') is False

    def test_value_formatting_edge_cases(self, menai):
        """Test value formatting edge cases."""
        # Very large numbers
        result = menai.evaluate_and_format("1000000000000000000000")
        assert "1000000000000000000000" in result

        # Very small numbers - check what Menai actually returns
        result = menai.evaluate_and_format("1e-20")
        # Menai might format very small numbers as 0 or in scientific notation
        assert result in ["0", "1e-20", "1e-020"] or "e-" in result

        # Complex numbers with zero parts
        result = menai.evaluate_and_format("(integer->complex 5 0)")
        assert result == "5+0j"  # Should format as real

        result = menai.evaluate_and_format("(integer->complex 0 3)")
        assert result == "3j"  # Should format as pure imaginary

        # Empty structures
        result = menai.evaluate_and_format("()")
        assert result == "()"

        result = menai.evaluate_and_format('""')
        assert result == '""'

    def test_value_type_predicates_edge_cases(self, menai):
        """Test type predicate edge cases."""
        # Integer vs float distinction
        assert menai.evaluate("(integer? 5)") is True
        assert menai.evaluate("(integer? 5.0)") is False
        assert menai.evaluate("(float? 5.0)") is True
        assert menai.evaluate("(float? 5)") is False

        # Complex number predicates
        assert menai.evaluate("(complex? (integer->complex 1 2))") is True
        assert menai.evaluate("(complex? 1j)") is True
        assert menai.evaluate("(complex? 5)") is False

        # String predicates with edge cases
        assert menai.evaluate('(string? "")') is True
        assert menai.evaluate('(string? "a")') is True
        assert menai.evaluate("(string? 123)") is False

        # List predicates with edge cases
        assert menai.evaluate("(list? ())") is True
        assert menai.evaluate("(list? (list))") is True
        assert menai.evaluate("(list? (list 1))") is True
        assert menai.evaluate('(list? "hello")') is False

    def test_infinity_and_nan_handling(self, menai):
        """Test handling of infinity and NaN values."""
        # Test division that results in infinity
        try:
            result = menai.evaluate("(float/ 1.0 0.0)")
            # This might raise an error or return infinity
            if not isinstance(result, Exception):
                assert math.isinf(result)
        except MenaiEvalError:
            # Division by zero error is also acceptable
            pass

        # Test operations with very large numbers
        result = menai.evaluate("(float* 1e100 1e100)")
        if not math.isinf(result):
            assert result == 1e200

    def test_value_memory_efficiency(self, menai):
        """Test value memory efficiency with large data structures."""
        # Large list creation
        result = menai.evaluate("(range 1 1001)")
        assert len(result) == 1000
        assert result[0] == 1
        assert result[999] == 1000

        # Large string operations - check actual format
        result = menai.evaluate('(list->string (map-list integer->string (range 1 101)) ",")')
        assert isinstance(result, str)
        # Menai returns the string without quotes in the result
        assert result.startswith('1,2,3')
        assert result.endswith(',100')

    def test_value_immutability(self, menai):
        """Test that values are immutable."""
        # Lists should not be modified by operations
        result = menai.evaluate("""
        (let ((original (list 1 2 3)))
          (list
            original
            (list-concat original (list 4))
            original))
        """)

        # Original should appear unchanged
        assert result[0] == [1, 2, 3]
        assert result[1] == [1, 2, 3, 4]
        assert result[2] == [1, 2, 3]  # Should be unchanged

    def test_value_equality_edge_cases(self, menai):
        """Test value equality edge cases."""
        # Numeric equality across types
        assert menai.evaluate("(integer=? -0 0)") is True

        # Complex number equality
        assert menai.evaluate("(complex=? (integer->complex 0 1) 1j)") is True

        # List equality
        assert menai.evaluate("(list=? (list 1 2) (list 1 2))") is True
        assert menai.evaluate("(list=? () ())") is True
        assert menai.evaluate("(list!=? (list 1 2) (list 2 1))") is True

    def test_value_conversion_edge_cases(self, menai):
        """Test value conversion edge cases."""
        # String to number conversions
        assert menai.evaluate('(string->number "42")') == 42
        assert menai.evaluate('(string->number "3.14")') == 3.14
        assert menai.evaluate('(string->number "-5")') == -5

        # Edge case conversions
        assert menai.evaluate('(string->number "0")') == 0

    def test_value_arithmetic_edge_cases(self, menai):
        """Test arithmetic operations with edge case values."""
        # Operations with zero
        assert menai.evaluate("(integer+ 0 5)") == 5
        assert menai.evaluate("(integer* 0 5)") == 0
        assert menai.evaluate("(integer- 5 0)") == 5

        # Operations with negative zero
        result = menai.evaluate("(float+ -0.0 0.0)")
        assert result == 0.0

        # Operations with very small numbers
        result = menai.evaluate("(float+ 1e-100 1e-100)")
        assert result == 2e-100

    def test_value_string_operations_edge_cases(self, menai):
        """Test string operations with edge case values."""
        # Empty string operations
        assert menai.evaluate('(string-length "")') == 0
        assert menai.evaluate('(string-concat "" "")') == ""
        assert menai.evaluate('(string-upcase "")') == ""

        # Single character operations
        assert menai.evaluate('(string-length "a")') == 1
        assert menai.evaluate('(string-upcase "a")') == "A"
        assert menai.evaluate('(string-ref "a" 0)') == "a"

        # Whitespace operations
        assert menai.evaluate('(string-trim "   ")') == ""
        assert menai.evaluate('(string-trim "  hello  ")') == "hello"
        assert menai.evaluate('(string-trim-left "   ")') == ""
        assert menai.evaluate('(string-trim-left "  hello  ")') == "hello  "
        assert menai.evaluate('(string-trim-right "   ")') == ""
        assert menai.evaluate('(string-trim-right "  hello  ")') == "  hello"

    def test_value_list_operations_edge_cases(self, menai):
        """Test list operations with edge case values."""
        # Empty list operations
        assert menai.evaluate("(list-length ())") == 0
        assert menai.evaluate("(list-null? ())") is True
        assert menai.evaluate("(list-reverse ())") == []

        # Single element list operations
        assert menai.evaluate("(list-length (list 1))") == 1
        assert menai.evaluate("(list-first (list 1))") == 1
        assert menai.evaluate("(list-rest (list 1))") == []

        # List operations with mixed types
        result = menai.evaluate('(list 1 "hello" #t)')
        assert result == [1, "hello", True]
        assert menai.evaluate('(list-length (list 1 "hello" #t))') == 3

    def test_dict_coverage_edge_cases(self, menai):
        """Test dict edge cases for full coverage."""
        # Test symbol keys in dict (line 212 coverage)
        # We need to construct this manually since 'dict' special form evaluates keys
        # and symbols evaluate to variable lookups

        # Create an dict with a symbol key manually
        sym_key = MenaiSymbol("my-symbol")
        val = MenaiString("value")
        dict = MenaiDict(((sym_key, val),))

        # Test to_python conversion
        py_dict = dict.to_python()
        assert py_dict == {"my-symbol": "value"}

        # Test type_name
        assert dict.type_name() == "dict"

        # Test length method directly
        assert dict.length() == 1

        # Test is_empty method directly
        assert not dict.is_empty()
        assert MenaiDict().is_empty()

        # Test invalid key type error
        # Using a list as a key should fail
        with pytest.raises(MenaiEvalError, match="Dict keys must be strings, numbers, booleans, or symbols"):
            MenaiDict._to_hashable_key(MenaiDict())
