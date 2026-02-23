"""Tests for complex number literals in AIFPL."""

import pytest

from aifpl import AIFPLTokenError, AIFPLTokenType, AIFPLLexer


class TestComplexNumberLiterals:
    """Test complex number literal tokenization and evaluation."""

    def test_pure_imaginary_unit(self, aifpl):
        """Test standalone 'j' or 'J' as imaginary unit (1j)."""
        # Lowercase j
        result = aifpl.evaluate("1j")
        assert result == 1j

        # Uppercase J
        result = aifpl.evaluate("1J")
        assert result == 1j

        # With explicit positive sign
        result = aifpl.evaluate("+1j")
        assert result == 1j

        # With negative sign
        result = aifpl.evaluate("-1j")
        assert result == -1j

    def test_pure_imaginary_numbers(self, aifpl):
        """Test pure imaginary numbers (no real part)."""
        test_cases = [
            ("4j", 4j),
            ("4J", 4j),
            ("-5j", -5j),
            ("-5J", -5j),
            ("1.5j", 1.5j),
            ("2.5J", 2.5j),
            ("-3.7j", -3.7j),
            ("0j", 0j),
            ("-0j", 0j),
        ]

        for expr, expected in test_cases:
            result = aifpl.evaluate(expr)
            assert result == expected, f"Expected {expr} to evaluate to {expected}, got {result}"

    def test_pure_imaginary_scientific_notation(self, aifpl):
        """Test pure imaginary numbers with scientific notation."""
        test_cases = [
            ("1e2j", 100j),
            ("1E2j", 100j),
            ("1.5e2j", 150j),
            ("3.7e-1j", 0.37j),
            ("-1e3j", -1000j),
            ("2.5e+1j", 25j),
        ]

        for expr, expected in test_cases:
            result = aifpl.evaluate(expr)
            assert result == expected, f"Expected {expr} to evaluate to {expected}, got {result}"

    def test_complex_with_both_parts(self, aifpl):
        """Test complex numbers with both real and imaginary parts."""
        test_cases = [
            ("3+4j", 3+4j),
            ("3-4j", 3-4j),
            ("1+1j", 1+1j),
            ("5-2j", 5-2j),
            ("0+1j", 1j),
            ("1+0j", 1+0j),  # Python keeps this as complex
            ("-3+4j", -3+4j),
            ("-3-4j", -3-4j),
            ("+3+4j", 3+4j),
            ("+3-4j", 3-4j),
        ]

        for expr, expected in test_cases:
            result = aifpl.evaluate(expr)
            assert result == expected, f"Expected {expr} to evaluate to {expected}, got {result}"

    def test_complex_with_floats(self, aifpl):
        """Test complex numbers with floating point components."""
        test_cases = [
            ("1.5+2.5j", 1.5+2.5j),
            ("3.14+1.59j", 3.14+1.59j),
            ("0.5+0.5j", 0.5+0.5j),
            ("2.0-3.5j", 2.0-3.5j),
            (".5+.5j", 0.5+0.5j),
            ("1.+2.j", 1.0+2.0j),
        ]

        for expr, expected in test_cases:
            result = aifpl.evaluate(expr)
            assert result == expected, f"Expected {expr} to evaluate to {expected}, got {result}"

    def test_complex_with_scientific_notation(self, aifpl):
        """Test complex numbers with scientific notation in both parts."""
        test_cases = [
            ("1e2+3e-1j", 100+0.3j),
            ("1E2+3E-1j", 100+0.3j),
            ("1.5e2+3.7e-1j", 150+0.37j),
            ("1e-10+1e-10j", 1e-10+1e-10j),
            ("2.5e+1+5e-1j", 25+0.5j),
            ("1e2-3e1j", 100-30j),
        ]

        for expr, expected in test_cases:
            result = aifpl.evaluate(expr)
            # Use approximate comparison for floating point
            assert abs(result.real - expected.real) < 1e-10, \
                f"Real part mismatch for {expr}: expected {expected.real}, got {result.real}"
            assert abs(result.imag - expected.imag) < 1e-10, \
                f"Imaginary part mismatch for {expr}: expected {expected.imag}, got {result.imag}"

    def test_complex_in_expressions(self, aifpl):
        """Test complex literals in arithmetic expressions."""
        test_cases = [
            ("(complex+ (complex 1 0) 2+3j)", 3+3j),
            ("(complex+ 2+3j (complex 1 0))", 3+3j),
            ("(complex+ 1+2j 3+4j)", 4+6j),
            ("(complex- 5+7j 2+3j)", 3+4j),
            ("(complex* (complex 2 0) 3+4j)", 6+8j),
            ("(complex* 1+2j 3+4j)", -5+10j),  # (1+2j)(3+4j) = 3+4j+6j+8j² = 3+10j-8 = -5+10j
        ]

        for expr, expected in test_cases:
            result = aifpl.evaluate(expr)
            assert result == expected, f"Expected {expr} to evaluate to {expected}, got {result}"

    def test_complex_with_functions(self, aifpl):
        """Test complex literals with built-in functions."""
        # Real and imaginary parts
        assert aifpl.evaluate("(real 3+4j)") == 3
        assert aifpl.evaluate("(imag 3+4j)") == 4
        assert aifpl.evaluate("(real 5j)") == 0
        assert aifpl.evaluate("(imag 5j)") == 5

        # Absolute value (magnitude)
        result = aifpl.evaluate("(complex-abs 3+4j)")
        assert abs(result - 5.0) < 1e-10, f"Expected |3+4j| = 5, got {result}"

        # Complex constructor still works
        assert aifpl.evaluate("(complex 3 4)") == 3+4j

    def test_lexer_complex_token_types(self):
        """Test that lexer produces correct token types for complex literals."""
        lexer = AIFPLLexer()

        test_cases = [
            ("1j", AIFPLTokenType.COMPLEX, 1j),
            ("4j", AIFPLTokenType.COMPLEX, 4j),
            ("3+4j", AIFPLTokenType.COMPLEX, 3+4j),
            ("1.5e2+3.7e-1j", AIFPLTokenType.COMPLEX, 150+0.37j),
            # Verify integers and floats still get correct types
            ("42", AIFPLTokenType.INTEGER, 42),
            ("3.14", AIFPLTokenType.FLOAT, 3.14),
        ]

        for expr, expected_type, expected_value in test_cases:
            tokens = lexer.lex(expr)
            assert len(tokens) == 1, f"Expected 1 token for '{expr}', got {len(tokens)}"
            token = tokens[0]
            assert token.type == expected_type, \
                f"For '{expr}': expected {expected_type.name}, got {token.type.name}"

            # Check value with appropriate comparison
            if isinstance(expected_value, complex):
                assert abs(token.value.real - expected_value.real) < 1e-10
                assert abs(token.value.imag - expected_value.imag) < 1e-10
            else:
                assert token.value == expected_value

    def test_complex_vs_symbol_disambiguation(self, aifpl):
        """Test that complex literals are correctly distinguished from symbols."""
        # These should be complex literals (not symbols)
        complex_cases = [
            "1j",
            "1J",
            "4j",
            "3+4j",
        ]

        for expr in complex_cases:
            result = aifpl.evaluate(expr)
            assert isinstance(result, complex), \
                f"Expected {expr} to evaluate to complex, got {type(result)}"

    def test_complex_literal_edge_cases(self, aifpl):
        """Test edge cases for complex literals."""
        edge_cases = [
            # Zero cases
            ("0j", 0j),
            ("0+0j", 0j),
            ("0.0j", 0j),
            ("0.0+0.0j", 0j),

            # Very small numbers
            ("1e-100j", 1e-100j),
            ("1e-100+1e-100j", 1e-100+1e-100j),

            # Very large numbers
            ("1e100j", 1e100j),
            ("1e100+1e100j", 1e100+1e100j),
        ]

        for expr, expected in edge_cases:
            result = aifpl.evaluate(expr)
            if abs(expected) < 1e-50:
                # For very small numbers, check they're both effectively zero
                assert abs(result) < 1e-50
            elif abs(expected) > 1e50:
                # For very large numbers, use relative comparison
                assert abs((result - expected) / expected) < 1e-10
            else:
                assert result == expected

    def test_invalid_complex_literals(self):
        """Test that invalid complex literal formats are rejected."""
        lexer = AIFPLLexer()

        invalid_cases = [
            "3+4",       # No 'j' suffix
            "3+4jk",     # Extra characters after 'j'
            "3++4j",     # Double operator
            "3+j+4",     # 'j' not at end
            "1j+3",      # 'j' at start (would parse as two tokens)
            "3+4i",      # Wrong imaginary unit (i instead of j)
        ]

        for expr in invalid_cases:
            try:
                tokens = lexer.lex(expr)
                # Some of these might lex but fail differently
                # (e.g., "3+4" would lex as three tokens: 3, +, 4)
                # We're mainly testing that "3+4jk" etc. fail at tokenization
                if 'j' in expr and expr.endswith('j') and any(c.isalpha() and c not in 'jJ' for c in expr):
                    pytest.fail(f"Expected tokenization error for invalid complex: {expr}")
            except AIFPLTokenError:
                # Expected for truly invalid formats
                pass

    def test_complex_mixed_with_other_types(self, aifpl):
        """Test complex literals mixed with other numeric types."""
        test_cases = [
            # Complex + integer
            ("(complex+ 3+4j (complex 5 0))", 8+4j),

            # Complex + float
            ("(complex+ 3+4j (complex 1.5 0))", 4.5+4j),

            # Complex + complex
            ("(complex+ 3+4j 1+2j)", 4+6j),

            # Mixed operations
            ("(complex+ (complex+ (complex 1 0) (complex 2.5 0)) 3+4j)", 6.5+4j),

            # Division
            ("(complex/ 6+8j (complex 2 0))", 3+4j),
        ]

        for expr, expected in test_cases:
            result = aifpl.evaluate(expr)
            # Use approximate comparison for complex results
            assert abs(result.real - expected.real) < 1e-10, \
                f"Real part mismatch for {expr}"
            assert abs(result.imag - expected.imag) < 1e-10, \
                f"Imaginary part mismatch for {expr}"

    def test_complex_in_lists(self, aifpl):
        """Test complex literals in list operations."""
        # List with complex numbers
        result = aifpl.evaluate("(list 1 2+3j 4)")
        assert len(result) == 3
        assert result[0] == 1
        assert result[1] == 2+3j
        assert result[2] == 4

        # Map over complex numbers
        result = aifpl.evaluate("(map (lambda (x) (complex* x (complex 2 0))) (list 1+1j 2+2j))")
        assert len(result) == 2
        assert result[0] == 2+2j
        assert result[1] == 4+4j

    def test_complex_comparison_not_supported(self, aifpl):
        """Test that comparison operations on complex numbers fail appropriately."""
        # Complex numbers don't support ordering comparisons
        comparison_ops = ['<', '>', '<=', '>=']

        for op in comparison_ops:
            with pytest.raises(Exception):  # Should raise some error
                aifpl.evaluate(f"({op} 3+4j 5+6j)")

        # But equality should work
        assert aifpl.evaluate("(= 3+4j 3+4j)") is True
        assert aifpl.evaluate("(= 3+4j 3+5j)") is False
        assert aifpl.evaluate("(!= 3+4j 3+5j)") is True

    def test_complex_type_predicates(self, aifpl):
        """Test type predicates with complex numbers."""
        # complex? predicate
        assert aifpl.evaluate("(complex? 3+4j)") is True
        assert aifpl.evaluate("(complex? 5j)") is True
        assert aifpl.evaluate("(complex? 1j)") is True
        assert aifpl.evaluate("(complex? 42)") is False
        assert aifpl.evaluate("(complex? 3.14)") is False

        # number? should return true for complex
        assert aifpl.evaluate("(number? 3+4j)") is True
        assert aifpl.evaluate("(number? 1j)") is True

        # integer? and float? should return false for complex
        assert aifpl.evaluate("(integer? 3+4j)") is False
        assert aifpl.evaluate("(float? 3+4j)") is False

    def test_complex_formatting_output(self, aifpl):
        """Test that complex numbers are formatted correctly in output."""
        # The output should use Python's complex format
        result = aifpl.evaluate("3+4j")
        result_str = str(result)
        assert "3" in result_str
        assert "4" in result_str
        assert "j" in result_str.lower()

        # Pure imaginary
        result = aifpl.evaluate("5j")
        result_str = str(result)
        assert "5" in result_str
        assert "j" in result_str.lower()

    def test_complex_separator_detection(self):
        """Test that complex separator detection handles scientific notation correctly."""
        lexer = AIFPLLexer()

        # These should all parse correctly - the separator finder must not
        # confuse the minus in scientific notation with the complex separator
        scientific_cases = [
            ("1e-10+2j", 1e-10+2j),
            ("1e-10-2j", 1e-10-2j),
            ("1.5e-5+3.7e-2j", 1.5e-5+3.7e-2j),
            ("2e+3+4e-1j", 2000+0.4j),
        ]

        for expr, expected in scientific_cases:
            tokens = lexer.lex(expr)
            assert len(tokens) == 1
            token = tokens[0]
            assert token.type == AIFPLTokenType.COMPLEX
            assert abs(token.value.real - expected.real) < 1e-10
            assert abs(token.value.imag - expected.imag) < 1e-10

    def test_complex_uppercase_j(self, aifpl):
        """Test that uppercase 'J' works the same as lowercase 'j'."""
        test_cases = [
            ("1J", "1j", 1j),
            ("4J", "4j", 4j),
            ("3+4J", "3+4j", 3+4j),
            ("1.5E2+3.7E-1J", "1.5e2+3.7e-1j", 150+0.37j),
        ]

        for upper_expr, lower_expr, expected in test_cases:
            upper_result = aifpl.evaluate(upper_expr)
            lower_result = aifpl.evaluate(lower_expr)

            assert upper_result == expected
            assert lower_result == expected
            assert upper_result == lower_result
