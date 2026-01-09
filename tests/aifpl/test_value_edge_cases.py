"""Tests for AIFPL value representation and edge cases."""

import math
import pytest

from aifpl import AIFPLEvalError, AIFPLAlist, AIFPLString, AIFPLNumber, AIFPLSymbol


class TestAIFPLValueEdgeCases:
    """Test edge cases in value representation and handling."""

    def test_value_creation_edge_cases(self, aifpl):
        """Test value creation with edge case inputs."""
        # Test zero values
        assert aifpl.evaluate("0") == 0
        assert aifpl.evaluate("0.0") == 0.0
        assert aifpl.evaluate("-0.0") == -0.0

        # Test very small numbers
        result = aifpl.evaluate("1e-100")
        assert result == 1e-100

        # Test very large numbers
        result = aifpl.evaluate("1e100")
        assert result == 1e100

    def test_floating_point_precision_edge_cases(self, aifpl):
        """Test floating point precision and edge cases."""
        # Test precision limits
        result = aifpl.evaluate("(/ 1 3)")
        assert abs(result - (1/3)) < 1e-15

        # Test very small differences
        result = aifpl.evaluate("(- 1.0000000000000001 1.0)")
        assert result == 1.0000000000000001 - 1.0

        # Test addition of very different magnitudes
        result = aifpl.evaluate("(+ 1e20 1)")
        assert result == 1e20 + 1

    def test_complex_number_edge_cases(self, aifpl):
        """Test complex number edge cases."""
        # Zero complex numbers
        result = aifpl.evaluate("(complex 0 0)")
        assert result == 0+0j

        # Pure real complex (should simplify to real)
        result = aifpl.evaluate("(complex 5 0)")
        assert result == 5
        assert isinstance(result, (int, float))

        # Pure imaginary complex
        result = aifpl.evaluate("(complex 0 3)")
        assert result == 3j

        # Very small imaginary parts (should simplify based on tolerance)
        result = aifpl.evaluate("(complex 5 1e-15)")
        # With default tolerance (1e-10), should simplify to real
        assert result == 5
        assert isinstance(result, (int, float))

        # Negative components
        result = aifpl.evaluate("(complex -2 -3)")
        assert result == -2-3j

    def test_string_edge_cases(self, aifpl):
        """Test string edge cases and special characters."""
        # Empty string
        result = aifpl.evaluate('""')
        assert result == ""

        # Single character
        result = aifpl.evaluate('"a"')
        assert result == "a"

        # Whitespace strings
        result = aifpl.evaluate('" "')
        assert result == " "

        result = aifpl.evaluate('"\\t"')
        assert result == "\t"

        result = aifpl.evaluate('"\\n"')
        assert result == "\n"

        # Unicode characters
        result = aifpl.evaluate('"\\u03B1\\u03B2\\u03B3"')
        assert result == "αβγ"

        # Escaped quotes
        result = aifpl.evaluate('"He said \\"Hello\\""')
        assert result == 'He said "Hello"'

    def test_boolean_edge_cases(self, aifpl):
        """Test boolean value edge cases."""
        # Standard booleans
        assert aifpl.evaluate("#t") is True
        assert aifpl.evaluate("#f") is False
        assert aifpl.evaluate("true") is True
        assert aifpl.evaluate("false") is False

        # Boolean operations with edge cases
        assert aifpl.evaluate("(and)") is True  # Identity for and
        assert aifpl.evaluate("(or)") is False  # Identity for or

    def test_list_edge_cases(self, aifpl):
        """Test list value edge cases."""
        # Empty list
        result = aifpl.evaluate("()")
        assert result == []

        # Single element lists
        result = aifpl.evaluate("(list 1)")
        assert result == [1]

        # Mixed type lists
        result = aifpl.evaluate('(list 1 "hello" #t)')
        assert result == [1, "hello", True]

        # Nested empty lists
        result = aifpl.evaluate("(list () ())")
        assert result == [[], []]

        # Deeply nested lists
        result = aifpl.evaluate("(list (list (list 1)))")
        assert result == [[[1]]]

    def test_numeric_type_coercion_edge_cases(self, aifpl):
        """Test numeric type coercion edge cases."""
        # Integer to float promotion
        result = aifpl.evaluate("(+ 1 2.5)")
        assert result == 3.5
        assert isinstance(result, float)

        # Float to complex promotion
        result = aifpl.evaluate("(+ 2.5 j)")
        assert result == 2.5+1j
        assert isinstance(result, complex)

        # Integer to complex promotion
        result = aifpl.evaluate("(+ 1 j)")
        assert result == 1+1j
        assert isinstance(result, complex)

    def test_value_comparison_edge_cases(self, aifpl):
        """Test value comparison edge cases."""
        # Floating point comparisons
        assert aifpl.evaluate("(= 0.1 0.1)") is True
        assert aifpl.evaluate("(= 0.0 -0.0)") is True

        # Complex number comparisons
        assert aifpl.evaluate("(= (complex 1 2) (complex 1 2))") is True
        assert aifpl.evaluate("(!= (complex 1 2) (complex 1 3))") is True

        # String comparisons
        assert aifpl.evaluate('(string=? "" "")') is True
        assert aifpl.evaluate('(string=? "a" "a")') is True
        assert aifpl.evaluate('(string=? "a" "b")') is False

    def test_value_formatting_edge_cases(self, aifpl):
        """Test value formatting edge cases."""
        # Very large numbers
        result = aifpl.evaluate_and_format("1000000000000000000000")
        assert "1000000000000000000000" in result

        # Very small numbers - check what AIFPL actually returns
        result = aifpl.evaluate_and_format("1e-20")
        # AIFPL might format very small numbers as 0 or in scientific notation
        assert result in ["0", "1e-20", "1e-020"] or "e-" in result

        # Complex numbers with zero parts
        result = aifpl.evaluate_and_format("(complex 5 0)")
        assert result == "5"  # Should format as real

        result = aifpl.evaluate_and_format("(complex 0 3)")
        assert result == "3j"  # Should format as pure imaginary

        # Empty structures
        result = aifpl.evaluate_and_format("()")
        assert result == "()"

        result = aifpl.evaluate_and_format('""')
        assert result == '""'

    def test_value_type_predicates_edge_cases(self, aifpl):
        """Test type predicate edge cases."""
        # Number type predicates with edge cases
        assert aifpl.evaluate("(number? 0)") is True
        assert aifpl.evaluate("(number? 0.0)") is True
        assert aifpl.evaluate("(number? (complex 0 0))") is True
        assert aifpl.evaluate("(number? #t)") is False

        # Integer vs float distinction
        assert aifpl.evaluate("(integer? 5)") is True
        assert aifpl.evaluate("(integer? 5.0)") is False
        assert aifpl.evaluate("(float? 5.0)") is True
        assert aifpl.evaluate("(float? 5)") is False

        # Complex number predicates
        assert aifpl.evaluate("(complex? (complex 1 2))") is True
        assert aifpl.evaluate("(complex? j)") is True
        assert aifpl.evaluate("(complex? 5)") is False

        # String predicates with edge cases
        assert aifpl.evaluate('(string? "")') is True
        assert aifpl.evaluate('(string? "a")') is True
        assert aifpl.evaluate("(string? 123)") is False

        # List predicates with edge cases
        assert aifpl.evaluate("(list? ())") is True
        assert aifpl.evaluate("(list? (list))") is True
        assert aifpl.evaluate("(list? (list 1))") is True
        assert aifpl.evaluate('(list? "hello")') is False

    def test_infinity_and_nan_handling(self, aifpl):
        """Test handling of infinity and NaN values."""
        # Test division that results in infinity
        try:
            result = aifpl.evaluate("(/ 1.0 0.0)")
            # This might raise an error or return infinity
            if not isinstance(result, Exception):
                assert math.isinf(result)
        except AIFPLEvalError:
            # Division by zero error is also acceptable
            pass

        # Test operations with very large numbers
        result = aifpl.evaluate("(* 1e100 1e100)")
        if not math.isinf(result):
            assert result == 1e200

    def test_value_memory_efficiency(self, aifpl):
        """Test value memory efficiency with large data structures."""
        # Large list creation
        result = aifpl.evaluate("(range 1 1001)")
        assert len(result) == 1000
        assert result[0] == 1
        assert result[999] == 1000

        # Large string operations - check actual format
        result = aifpl.evaluate('(string-join (map number->string (range 1 101)) ",")')
        assert isinstance(result, str)
        # AIFPL returns the string without quotes in the result
        assert result.startswith('1,2,3')
        assert result.endswith(',100')

    def test_value_immutability(self, aifpl):
        """Test that values are immutable."""
        # Lists should not be modified by operations
        result = aifpl.evaluate("""
        (let ((original (list 1 2 3)))
          (list
            original
            (append original (list 4))
            original))
        """)

        # Original should appear unchanged
        assert result[0] == [1, 2, 3]
        assert result[1] == [1, 2, 3, 4]
        assert result[2] == [1, 2, 3]  # Should be unchanged

    def test_value_equality_edge_cases(self, aifpl):
        """Test value equality edge cases."""
        # Numeric equality across types
        assert aifpl.evaluate("(= 5 5.0)") is True
        assert aifpl.evaluate("(= 0 0.0)") is True
        assert aifpl.evaluate("(= -0 0)") is True

        # Complex number equality
        assert aifpl.evaluate("(= (complex 5 0) 5)") is True
        assert aifpl.evaluate("(= (complex 0 1) j)") is True

        # List equality
        assert aifpl.evaluate("(= (list 1 2) (list 1 2))") is True
        assert aifpl.evaluate("(= () ())") is True
        assert aifpl.evaluate("(!= (list 1 2) (list 2 1))") is True

    def test_value_conversion_edge_cases(self, aifpl):
        """Test value conversion edge cases."""
        # String to number conversions
        assert aifpl.evaluate('(string->number "42")') == 42
        assert aifpl.evaluate('(string->number "3.14")') == 3.14
        assert aifpl.evaluate('(string->number "-5")') == -5

        # Number to string conversions
        assert aifpl.evaluate('(number->string 42)') == "42"
        assert aifpl.evaluate('(number->string 3.14)') == "3.14"
        assert aifpl.evaluate('(number->string -5)') == "-5"

        # Edge case conversions
        assert aifpl.evaluate('(string->number "0")') == 0
        assert aifpl.evaluate('(number->string 0)') == "0"

    def test_value_arithmetic_edge_cases(self, aifpl):
        """Test arithmetic operations with edge case values."""
        # Operations with zero
        assert aifpl.evaluate("(+ 0 5)") == 5
        assert aifpl.evaluate("(* 0 5)") == 0
        assert aifpl.evaluate("(- 5 0)") == 5

        # Operations with negative zero
        result = aifpl.evaluate("(+ -0.0 0.0)")
        assert result == 0.0

        # Operations with very small numbers
        result = aifpl.evaluate("(+ 1e-100 1e-100)")
        assert result == 2e-100

    def test_value_string_operations_edge_cases(self, aifpl):
        """Test string operations with edge case values."""
        # Empty string operations
        assert aifpl.evaluate('(string-length "")') == 0
        assert aifpl.evaluate('(string-append "" "")') == ""
        assert aifpl.evaluate('(string-upcase "")') == ""

        # Single character operations
        assert aifpl.evaluate('(string-length "a")') == 1
        assert aifpl.evaluate('(string-upcase "a")') == "A"
        assert aifpl.evaluate('(string-ref "a" 0)') == "a"

        # Whitespace operations
        assert aifpl.evaluate('(string-trim "   ")') == ""
        assert aifpl.evaluate('(string-trim "  hello  ")') == "hello"

    def test_value_list_operations_edge_cases(self, aifpl):
        """Test list operations with edge case values."""
        # Empty list operations
        assert aifpl.evaluate("(length ())") == 0
        assert aifpl.evaluate("(null? ())") is True
        assert aifpl.evaluate("(reverse ())") == []

        # Single element list operations
        assert aifpl.evaluate("(length (list 1))") == 1
        assert aifpl.evaluate("(first (list 1))") == 1
        assert aifpl.evaluate("(rest (list 1))") == []

        # List operations with mixed types
        result = aifpl.evaluate('(list 1 "hello" #t)')
        assert result == [1, "hello", True]
        assert aifpl.evaluate('(length (list 1 "hello" #t))') == 3

    def test_alist_coverage_edge_cases(self, aifpl):
        """Test alist edge cases for full coverage."""
        # Test symbol keys in alist (line 212 coverage)
        # We need to construct this manually since 'alist' special form evaluates keys
        # and symbols evaluate to variable lookups

        # Create an alist with a symbol key manually
        sym_key = AIFPLSymbol("my-symbol")
        val = AIFPLString("value")
        alist = AIFPLAlist(((sym_key, val),))

        # Test to_python conversion
        py_dict = alist.to_python()
        assert py_dict == {"my-symbol": "value"}

        # Test type_name
        assert alist.type_name() == "alist"

        # Test length method directly
        assert alist.length() == 1

        # Test is_empty method directly
        assert not alist.is_empty()
        assert AIFPLAlist().is_empty()

        # Test invalid key type error
        # Using a list as a key should fail
        with pytest.raises(AIFPLEvalError, match="Alist keys must be strings, numbers, booleans, or symbols"):
            AIFPLAlist._to_hashable_key(AIFPLAlist())
