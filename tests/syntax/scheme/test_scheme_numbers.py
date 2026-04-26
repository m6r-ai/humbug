"""
Tests for Scheme number tokenization.
"""

from syntax.scheme.scheme_lexer import SchemeLexer
from syntax.lexer import TokenType


class TestSchemeNumbers:
    """Test Scheme number tokenization."""

    def _lex(self, source: str) -> list:
        lexer = SchemeLexer()
        lexer.lex(None, source)
        return list(lexer._tokens)

    def test_decimal_integers(self):
        """Test decimal integer literals."""
        for num in ['0', '42', '123', '999999', '1234567890']:
            tokens = self._lex(num)
            assert len(tokens) == 1, f"'{num}' should produce one token"
            assert tokens[0].type == TokenType.NUMBER, f"'{num}' should be NUMBER type"
            assert tokens[0].value == num

    def test_decimal_floats(self):
        """Test decimal floating point literals."""
        for num in ['3.14', '0.5', '123.456', '0.0', '10.0']:
            tokens = self._lex(num)
            assert len(tokens) == 1, f"'{num}' should produce one token"
            assert tokens[0].type == TokenType.NUMBER, f"'{num}' should be NUMBER type"
            assert tokens[0].value == num

    def test_float_starting_with_dot(self):
        """Test floating point literals starting with a dot."""
        for num in ['.5', '.123', '.0']:
            tokens = self._lex(num)
            assert len(tokens) == 1, f"'{num}' should produce one token"
            assert tokens[0].type == TokenType.NUMBER, f"'{num}' should be NUMBER type"
            assert tokens[0].value == num

    def test_signed_integers(self):
        """Test signed integer literals."""
        for num in ['+42', '-42', '+0', '-0']:
            tokens = self._lex(num)
            assert len(tokens) == 1, f"'{num}' should produce one token"
            assert tokens[0].type == TokenType.NUMBER, f"'{num}' should be NUMBER type"
            assert tokens[0].value == num

    def test_signed_floats(self):
        """Test signed floating point literals."""
        for num in ['+3.14', '-3.14', '+0.0', '-0.0']:
            tokens = self._lex(num)
            assert len(tokens) == 1, f"'{num}' should produce one token"
            assert tokens[0].type == TokenType.NUMBER, f"'{num}' should be NUMBER type"
            assert tokens[0].value == num

    def test_signed_dot_floats(self):
        """Test that +.5 and -.5 are recognised as single number tokens."""
        for num in ['+.5', '-.5', '+.123', '-.999']:
            tokens = self._lex(num)
            assert len(tokens) == 1, f"'{num}' should produce one token, got {len(tokens)}"
            assert tokens[0].type == TokenType.NUMBER, f"'{num}' should be NUMBER type"
            assert tokens[0].value == num

    def test_scientific_notation(self):
        """Test scientific notation literals."""
        for num in ['1e10', '1.5e-3', '0.5e+10', '2.5e5', '1e0', '1E10', '1.5E-3']:
            tokens = self._lex(num)
            assert len(tokens) == 1, f"'{num}' should produce one token"
            assert tokens[0].type == TokenType.NUMBER, f"'{num}' should be NUMBER type"
            assert tokens[0].value == num

    def test_exponent_at_end_of_input(self):
        """Test that a trailing 'e' does not crash the lexer."""
        tokens = self._lex('1e')
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.NUMBER
        assert tokens[0].value == '1e'

    def test_rational_numbers(self):
        """Test rational number literals."""
        for num in ['22/7', '1/2', '3/4', '355/113']:
            tokens = self._lex(num)
            assert len(tokens) == 1, f"'{num}' should produce one token"
            assert tokens[0].type == TokenType.NUMBER, f"'{num}' should be NUMBER type"
            assert tokens[0].value == num

    def test_signed_rational_numbers(self):
        """Test signed rational number literals."""
        for num in ['+22/7', '-1/2', '+3/4', '-355/113']:
            tokens = self._lex(num)
            assert len(tokens) == 1, f"'{num}' should produce one token"
            assert tokens[0].type == TokenType.NUMBER, f"'{num}' should be NUMBER type"
            assert tokens[0].value == num

    def test_pure_imaginary_numbers(self):
        """Test pure imaginary number literals (imaginary suffix 'i')."""
        for num in ['1i', '2.5i', '0i', '3.14i', '+5i', '-3i']:
            tokens = self._lex(num)
            assert len(tokens) == 1, f"'{num}' should produce one token"
            assert tokens[0].type == TokenType.NUMBER, f"'{num}' should be NUMBER type"
            assert tokens[0].value == num

    def test_complex_number_single_token(self):
        """Test that full rectangular complex literals are a single token."""
        cases = [
            '3+4i',
            '3-4i',
            '1.5e2+3.7e-1i',
            '0+1i',
            '+1+2i',
            '-1-2i',
        ]
        for num in cases:
            tokens = self._lex(num)
            assert len(tokens) == 1, f"Complex literal '{num}' should produce one token, got {len(tokens)}"
            assert tokens[0].type == TokenType.NUMBER, f"'{num}' should be NUMBER type"
            assert tokens[0].value == num

    def test_hexadecimal_numbers(self):
        """Test hexadecimal literals with #x prefix."""
        for num in ['#x0', '#xFF', '#x2A', '#xDEADBEEF', '#xabc123', '#xABC']:
            tokens = self._lex(num)
            assert len(tokens) == 1, f"'{num}' should produce one token"
            assert tokens[0].type == TokenType.NUMBER, f"'{num}' should be NUMBER type"
            assert tokens[0].value == num

    def test_binary_numbers(self):
        """Test binary literals with #b prefix."""
        for num in ['#b0', '#b1', '#b101010', '#b11111111', '#b1001']:
            tokens = self._lex(num)
            assert len(tokens) == 1, f"'{num}' should produce one token"
            assert tokens[0].type == TokenType.NUMBER, f"'{num}' should be NUMBER type"
            assert tokens[0].value == num

    def test_octal_numbers(self):
        """Test octal literals with #o prefix."""
        for num in ['#o0', '#o7', '#o77', '#o755', '#o644']:
            tokens = self._lex(num)
            assert len(tokens) == 1, f"'{num}' should produce one token"
            assert tokens[0].type == TokenType.NUMBER, f"'{num}' should be NUMBER type"
            assert tokens[0].value == num

    def test_decimal_numbers_with_d_prefix(self):
        """Test decimal literals with #d prefix (valid R5RS)."""
        for num in ['#d0', '#d42', '#d123', '#d999']:
            tokens = self._lex(num)
            assert len(tokens) == 1, f"'{num}' should produce one token"
            assert tokens[0].type == TokenType.NUMBER, f"'{num}' should be NUMBER type"
            assert tokens[0].value == num

    def test_uppercase_base_prefixes(self):
        """Test that uppercase base prefix letters are accepted."""
        cases = [
            ('#Xff', '#Xff'),
            ('#XFF', '#XFF'),
            ('#B1010', '#B1010'),
            ('#O755', '#O755'),
            ('#D42', '#D42'),
        ]
        for num, expected in cases:
            tokens = self._lex(num)
            assert len(tokens) == 1, f"'{num}' should produce one token"
            assert tokens[0].type == TokenType.NUMBER, f"'{num}' should be NUMBER type"
            assert tokens[0].value == expected

    def test_negative_based_numbers_are_two_tokens(self):
        """Test that -#xFF is two tokens — R5RS sign is not part of a prefixed literal."""
        tokens = self._lex('-#xFF')
        assert len(tokens) == 2, "'-#xFF' should produce two tokens in Scheme"
        assert tokens[0].type == TokenType.IDENTIFIER
        assert tokens[0].value == '-'
        assert tokens[1].type == TokenType.NUMBER
        assert tokens[1].value == '#xFF'

    def test_standalone_i_is_identifier(self):
        """Test that standalone 'i' is an identifier, not a number."""
        tokens = self._lex('i')
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.IDENTIFIER
        assert tokens[0].value == 'i'

    def test_numbers_in_expression(self):
        """Test numbers in a typical Scheme expression."""
        tokens = self._lex('(+ 1 2 3)')
        number_tokens = [t for t in tokens if t.type == TokenType.NUMBER]
        assert len(number_tokens) == 3
        assert [t.value for t in number_tokens] == ['1', '2', '3']

    def test_numbers_with_whitespace(self):
        """Test that whitespace properly separates numbers."""
        tokens = self._lex('42 3.14 #xFF')
        number_tokens = [t for t in tokens if t.type == TokenType.NUMBER]
        assert len(number_tokens) == 3
        assert number_tokens[0].value == '42'
        assert number_tokens[1].value == '3.14'
        assert number_tokens[2].value == '#xFF'

    def test_zero_variations(self):
        """Test different representations of zero."""
        for num in ['0', '0.0', '#x0', '#b0', '#o0', '#d0', '0e0', '0i']:
            tokens = self._lex(num)
            assert len(tokens) == 1, f"'{num}' should produce one token"
            assert tokens[0].type == TokenType.NUMBER

    def test_very_large_numbers(self):
        """Test very large number literals."""
        for num in ['999999999999999999999999999', '1.7976931348623157e308', '#xFFFFFFFFFFFFFFFF']:
            tokens = self._lex(num)
            assert len(tokens) == 1, f"'{num}' should produce one token"
            assert tokens[0].type == TokenType.NUMBER

    def test_very_small_numbers(self):
        """Test very small floating point literals."""
        for num in ['0.0000000001', '1e-308', '2.2250738585072014e-308']:
            tokens = self._lex(num)
            assert len(tokens) == 1, f"'{num}' should produce one token"
            assert tokens[0].type == TokenType.NUMBER
