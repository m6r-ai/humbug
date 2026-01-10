"""
Tests for AIFPL number tokenization.
"""
import pytest

from syntax.aifpl.aifpl_lexer import AIFPLLexer
from syntax.lexer import TokenType


class TestAIFPLNumbers:
    """Test AIFPL number tokenization."""

    def test_decimal_integers(self):
        """Test decimal integer numbers."""
        test_cases = ['0', '42', '123', '999999', '1234567890']
        for num in test_cases:
            lexer = AIFPLLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Number '{num}' should produce one token"
            assert tokens[0].type == TokenType.NUMBER, f"Number '{num}' should be NUMBER type"
            assert tokens[0].value == num

    def test_decimal_floats(self):
        """Test decimal floating point numbers."""
        test_cases = ['3.14', '0.5', '123.456', '0.0', '10.0']
        for num in test_cases:
            lexer = AIFPLLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Float '{num}' should produce one token"
            assert tokens[0].type == TokenType.NUMBER, f"Float '{num}' should be NUMBER type"
            assert tokens[0].value == num

    def test_float_starting_with_dot(self):
        """Test floating point numbers starting with a dot."""
        test_cases = ['.5', '.123', '.0']
        for num in test_cases:
            lexer = AIFPLLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Float '{num}' should produce one token"
            assert tokens[0].type == TokenType.NUMBER, f"Float '{num}' should be NUMBER type"
            assert tokens[0].value == num

    def test_signed_numbers(self):
        """Test numbers with explicit sign."""
        test_cases = ['+42', '-42', '+3.14', '-3.14', '+0', '-0']
        for num in test_cases:
            lexer = AIFPLLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Signed number '{num}' should produce one token"
            assert tokens[0].type == TokenType.NUMBER, f"Signed number '{num}' should be NUMBER type"
            assert tokens[0].value == num

    def test_scientific_notation(self):
        """Test scientific notation."""
        test_cases = ['1e10', '1.5e-3', '0.5e+10', '2.5e5', '1e0', '1E10', '1.5E-3']
        for num in test_cases:
            lexer = AIFPLLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Scientific '{num}' should produce one token"
            assert tokens[0].type == TokenType.NUMBER, f"Scientific '{num}' should be NUMBER type"
            assert tokens[0].value == num

    def test_complex_numbers(self):
        """Test complex numbers with 'j' suffix."""
        test_cases = ['1j', '2.5j', '0j', '3.14j', '1e10j', '-5j', '+3j']
        for num in test_cases:
            lexer = AIFPLLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Complex '{num}' should produce one token"
            assert tokens[0].type == TokenType.NUMBER, f"Complex '{num}' should be NUMBER type"
            assert tokens[0].value == num

    def test_hexadecimal_numbers(self):
        """Test hexadecimal numbers with #x prefix."""
        test_cases = ['#x0', '#xFF', '#x2A', '#xDEADBEEF', '#xabc123', '#xABC']
        for num in test_cases:
            lexer = AIFPLLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Hex '{num}' should produce one token"
            assert tokens[0].type == TokenType.NUMBER, f"Hex '{num}' should be NUMBER type"
            assert tokens[0].value == num

    def test_binary_numbers(self):
        """Test binary numbers with #b prefix."""
        test_cases = ['#b0', '#b1', '#b101010', '#b11111111', '#b1001']
        for num in test_cases:
            lexer = AIFPLLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Binary '{num}' should produce one token"
            assert tokens[0].type == TokenType.NUMBER, f"Binary '{num}' should be NUMBER type"
            assert tokens[0].value == num

    def test_octal_numbers(self):
        """Test octal numbers with #o prefix."""
        test_cases = ['#o0', '#o7', '#o77', '#o123', '#o777', '#o755']
        for num in test_cases:
            lexer = AIFPLLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Octal '{num}' should produce one token"
            assert tokens[0].type == TokenType.NUMBER, f"Octal '{num}' should be NUMBER type"
            assert tokens[0].value == num

    def test_decimal_numbers_with_d_prefix(self):
        """Test decimal numbers with #d prefix."""
        test_cases = ['#d0', '#d42', '#d123', '#d999']
        for num in test_cases:
            lexer = AIFPLLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Decimal '{num}' should produce one token"
            assert tokens[0].type == TokenType.NUMBER, f"Decimal '{num}' should be NUMBER type"
            assert tokens[0].value == num

    def test_numbers_in_expressions(self):
        """Test numbers in expressions."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(+ 1 2 3)')

        tokens = list(lexer._tokens)
        number_tokens = [t for t in tokens if t.type == TokenType.NUMBER]
        assert len(number_tokens) == 3, "Should have 3 number tokens"
        assert number_tokens[0].value == '1'
        assert number_tokens[1].value == '2'
        assert number_tokens[2].value == '3'

    def test_zero_variations(self):
        """Test different representations of zero."""
        test_cases = ['0', '0.0', '#x0', '#b0', '#o0', '0e0', '0j']
        for num in test_cases:
            lexer = AIFPLLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Zero '{num}' should produce one token"
            assert tokens[0].type == TokenType.NUMBER

    def test_very_large_numbers(self):
        """Test very large numbers."""
        test_cases = [
            '999999999999999999999999999',
            '1.7976931348623157e308',
            '#xFFFFFFFFFFFFFFFF',
        ]
        for num in test_cases:
            lexer = AIFPLLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Large number '{num}' should produce one token"
            assert tokens[0].type == TokenType.NUMBER

    def test_very_small_numbers(self):
        """Test very small numbers."""
        test_cases = [
            '0.0000000001',
            '1e-308',
            '2.2250738585072014e-308',
        ]
        for num in test_cases:
            lexer = AIFPLLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Small number '{num}' should produce one token"
            assert tokens[0].type == TokenType.NUMBER

    def test_numbers_with_whitespace(self):
        """Test that whitespace properly separates numbers."""
        lexer = AIFPLLexer()
        lexer.lex(None, '42 3.14 #xFF')

        tokens = list(lexer._tokens)
        number_tokens = [t for t in tokens if t.type == TokenType.NUMBER]
        assert len(number_tokens) == 3
        assert number_tokens[0].value == '42'
        assert number_tokens[1].value == '3.14'
        assert number_tokens[2].value == '#xFF'

    def test_numbers_in_list(self):
        """Test numbers in a list."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(list 1 2 3)')

        tokens = list(lexer._tokens)
        number_tokens = [t for t in tokens if t.type == TokenType.NUMBER]
        assert len(number_tokens) == 3
        assert [t.value for t in number_tokens] == ['1', '2', '3']

    def test_pi_and_e_constants(self):
        """Test that pi and e are identifiers, not numbers."""
        lexer = AIFPLLexer()
        lexer.lex(None, 'pi e')

        tokens = list(lexer._tokens)
        # Filter out whitespace
        # whitespace is skipped
        assert len(tokens) == 2
        assert tokens[0].type == TokenType.IDENTIFIER
        assert tokens[0].value == 'pi'
        assert tokens[1].type == TokenType.IDENTIFIER
        assert tokens[1].value == 'e'
        assert tokens[0].type == TokenType.IDENTIFIER
        assert tokens[0].value == 'pi'
        assert tokens[1].type == TokenType.IDENTIFIER
        assert tokens[1].value == 'e'
        assert tokens[0].type == TokenType.IDENTIFIER
        assert tokens[0].value == 'pi'
        assert tokens[1].type == TokenType.IDENTIFIER
        assert tokens[1].value == 'e'
        assert tokens[0].type == TokenType.IDENTIFIER
        assert tokens[0].value == 'pi'
        assert tokens[1].type == TokenType.IDENTIFIER
        assert tokens[1].value == 'e'

    def test_imaginary_unit_j(self):
        """Test that standalone 'j' is an identifier."""
        lexer = AIFPLLexer()
        lexer.lex(None, 'j')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.IDENTIFIER
        assert tokens[0].value == 'j'

    def test_complex_number_in_expression(self):
        """Test complex numbers in expressions."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(+ 1 (* 2 j))')

        tokens = list(lexer._tokens)
        # Should have parens, +, 1, parens, *, 2, j, parens
        identifier_tokens = [t for t in tokens if t.type == TokenType.IDENTIFIER]
        assert any(t.value == '+' for t in identifier_tokens)
        assert any(t.value == '*' for t in identifier_tokens)
        assert any(t.value == 'j' for t in identifier_tokens)

    def test_signed_numbers_as_separate_from_operators(self):
        """Test that +/- at start of numbers are part of the number."""
        lexer = AIFPLLexer()
        lexer.lex(None, '+42')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.NUMBER
        assert tokens[0].value == '+42'
