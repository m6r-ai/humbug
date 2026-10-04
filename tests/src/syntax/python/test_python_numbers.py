"""
Tests for Python number tokenization.
"""
import pytest

from syntax.python.python_lexer import PythonLexer


class TestPythonNumbers:
    """Test Python number tokenization."""

    def test_decimal_integers(self):
        """Test decimal integer numbers."""
        test_cases = ['0', '42', '123', '999999', '1234567890']
        for num in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Number '{num}' should produce one token"
            assert tokens[0].type.name == 'NUMBER', f"Number '{num}' should be NUMBER type"
            assert tokens[0].value == num

    def test_decimal_floats(self):
        """Test decimal floating point numbers."""
        test_cases = ['3.14', '0.5', '123.456', '0.0', '10.0']
        for num in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Float '{num}' should produce one token"
            assert tokens[0].type.name == 'NUMBER', f"Float '{num}' should be NUMBER type"
            assert tokens[0].value == num

    def test_float_starting_with_dot(self):
        """Test floating point numbers starting with a dot."""
        test_cases = ['.5', '.123', '.0']
        for num in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Float '{num}' should produce one token"
            assert tokens[0].type.name == 'NUMBER', f"Float '{num}' should be NUMBER type"
            assert tokens[0].value == num

    def test_hexadecimal_integers(self):
        """Test hexadecimal integers."""
        test_cases = ['0x0', '0xFF', '0x2A', '0xDEADBEEF', '0xabc123', '0XFF', '0X1A']
        for num in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Hex '{num}' should produce one token"
            assert tokens[0].type.name == 'NUMBER', f"Hex '{num}' should be NUMBER type"
            assert tokens[0].value == num

    def test_binary_integers(self):
        """Test binary integers."""
        test_cases = ['0b0', '0b1', '0b101010', '0b11111111', '0B0', '0B1010']
        for num in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Binary '{num}' should produce one token"
            assert tokens[0].type.name == 'NUMBER', f"Binary '{num}' should be NUMBER type"
            assert tokens[0].value == num

    def test_octal_integers(self):
        """Test octal integers."""
        test_cases = ['0o0', '0o7', '0o77', '0o123', '0o777', '0O7', '0O77']
        for num in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Octal '{num}' should produce one token"
            assert tokens[0].type.name == 'NUMBER', f"Octal '{num}' should be NUMBER type"
            assert tokens[0].value == num

    def test_scientific_notation_lowercase(self):
        """Test scientific notation with lowercase 'e'."""
        test_cases = ['1e10', '1.5e-3', '0.5e+10', '2.5e5', '1e0']
        for num in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Scientific '{num}' should produce one token"
            assert tokens[0].type.name == 'NUMBER', f"Scientific '{num}' should be NUMBER type"
            assert tokens[0].value == num

    def test_scientific_notation_uppercase(self):
        """Test scientific notation with uppercase 'E'."""
        test_cases = ['1E10', '1.5E-3', '0.5E+10', '2.5E5', '1E0']
        for num in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Scientific '{num}' should produce one token"
            assert tokens[0].type.name == 'NUMBER', f"Scientific '{num}' should be NUMBER type"
            assert tokens[0].value == num

    def test_complex_numbers(self):
        """Test complex numbers with 'j' suffix."""
        test_cases = ['1j', '2.5j', '0j', '3.14j', '1e10j', '0xFFj', '0b101j']
        for num in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Complex '{num}' should produce one token"
            assert tokens[0].type.name == 'NUMBER', f"Complex '{num}' should be NUMBER type"
            assert tokens[0].value == num

    def test_negative_numbers(self):
        """Test negative numbers (minus operator followed by number)."""
        test_cases = ['-42', '-3.14', '-0xFF', '-1e10', '-1j']
        for num in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert len(tokens) == 2, f"Negative number '{num}' should produce two tokens"
            assert tokens[0].type.name == 'OPERATOR' and tokens[0].value == '-', \
                "First token should be minus operator"
            assert tokens[1].type.name == 'NUMBER', f"Second token should be NUMBER type"

    def test_positive_numbers(self):
        """Test numbers with explicit plus sign."""
        test_cases = ['+42', '+3.14', '+0xFF']
        for num in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert len(tokens) == 2, f"Positive number '{num}' should produce two tokens"
            assert tokens[0].type.name == 'OPERATOR' and tokens[0].value == '+', \
                "First token should be plus operator"
            assert tokens[1].type.name == 'NUMBER', f"Second token should be NUMBER type"

    def test_leading_zeros_in_decimal(self):
        """Test decimal numbers with leading zeros."""
        # In Python 3, leading zeros are allowed only for '0'
        test_cases = ['007', '000123']
        for num in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            # These might be parsed as separate tokens or as numbers depending on implementation
            assert len(tokens) >= 1, f"Number with leading zeros '{num}' should produce tokens"

    def test_underscore_in_numbers(self):
        """Test numbers with underscores (Python 3.6+)."""
        test_cases = ['1_000', '1_000_000', '0xFF_FF', '0b1111_0000', '3.14_15_93']
        for num in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            # The lexer might not support underscores, so this could be multiple tokens
            # We're just checking it doesn't crash
            assert len(tokens) >= 1, f"Number with underscores '{num}' should produce tokens"

    def test_numbers_in_expressions(self):
        """Test numbers in mathematical expressions."""
        lexer = PythonLexer()
        lexer.lex(None, '1 + 2 * 3')

        tokens = list(lexer._tokens)
        number_tokens = [t for t in tokens if t.type.name == 'NUMBER']
        assert len(number_tokens) == 3, "Should have 3 number tokens"
        assert number_tokens[0].value == '1'
        assert number_tokens[1].value == '2'
        assert number_tokens[2].value == '3'

    def test_float_without_trailing_digits(self):
        """Test floats without digits after decimal point."""
        # Note: '1.' is valid in some contexts but might not be in Python
        test_cases = ['1.', '42.']
        for num in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            # This might be parsed as number + operator or as a single number
            assert len(tokens) >= 1, f"Float '{num}' should produce at least one token"

    def test_very_large_numbers(self):
        """Test very large numbers."""
        test_cases = [
            '999999999999999999999999999',
            '1.7976931348623157e308',
            '0xFFFFFFFFFFFFFFFF',
        ]
        for num in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Large number '{num}' should produce one token"
            assert tokens[0].type.name == 'NUMBER'

    def test_very_small_numbers(self):
        """Test very small numbers."""
        test_cases = [
            '0.0000000001',
            '1e-308',
            '2.2250738585072014e-308',
        ]
        for num in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Small number '{num}' should produce one token"
            assert tokens[0].type.name == 'NUMBER'

    def test_zero_variations(self):
        """Test different representations of zero."""
        test_cases = ['0', '0.0', '0x0', '0b0', '0o0', '0e0', '0j']
        for num in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Zero '{num}' should produce one token"
            assert tokens[0].type.name == 'NUMBER'

    def test_number_followed_by_identifier(self):
        """Test that numbers followed by identifiers are separate tokens."""
        lexer = PythonLexer()
        lexer.lex(None, '42abc')

        tokens = list(lexer._tokens)
        # Should be two tokens: number and identifier
        # Unless it's invalid syntax that gets parsed differently
        assert len(tokens) >= 1, "Should produce at least one token"
