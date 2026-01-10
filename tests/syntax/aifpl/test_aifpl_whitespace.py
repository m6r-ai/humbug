"""
Tests for AIFPL whitespace handling.
"""
import pytest

from syntax.aifpl.aifpl_lexer import AIFPLLexer
from syntax.lexer import TokenType


class TestAIFPLWhitespace:
    """Test AIFPL whitespace handling."""

    def test_single_space(self):
        """Test single space."""
        lexer = AIFPLLexer()
        lexer.lex(None, ' ')

        tokens = list(lexer._tokens)
        # Whitespace is skipped, should be empty
        assert len(tokens) == 0

    def test_multiple_spaces(self):
        """Test multiple spaces."""
        lexer = AIFPLLexer()
        lexer.lex(None, '     ')

        tokens = list(lexer._tokens)
        # Whitespace is skipped
        assert len(tokens) == 0

    def test_tab_character(self):
        """Test tab character."""
        lexer = AIFPLLexer()
        lexer.lex(None, '\t')

        tokens = list(lexer._tokens)
        # Whitespace is skipped
        assert len(tokens) == 0

    def test_mixed_spaces_and_tabs(self):
        """Test mixed spaces and tabs."""
        lexer = AIFPLLexer()
        lexer.lex(None, ' \t \t ')

        tokens = list(lexer._tokens)
        # Whitespace is skipped
        assert len(tokens) == 0

    def test_spaces_between_tokens(self):
        """Test spaces between tokens."""
        lexer = AIFPLLexer()
        lexer.lex(None, '1 2 3')

        tokens = list(lexer._tokens)
        number_tokens = [t for t in tokens if t.type == TokenType.NUMBER]
        assert len(number_tokens) == 3

    def test_no_space_between_parens_and_content(self):
        """Test no space between parens and content."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(+ 1 2)')

        tokens = list(lexer._tokens)
        # Should parse correctly even without spaces after (
        lparen_tokens = [t for t in tokens if t.type == TokenType.LPAREN]
        assert len(lparen_tokens) == 1

    def test_extra_spaces_in_expression(self):
        """Test extra spaces in expression."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(  +   1   2  )')

        tokens = list(lexer._tokens)
        number_tokens = [t for t in tokens if t.type == TokenType.NUMBER]
        assert len(number_tokens) == 2

    def test_leading_whitespace(self):
        """Test leading whitespace."""
        lexer = AIFPLLexer()
        lexer.lex(None, '    (+ 1 2)')

        tokens = list(lexer._tokens)
        # Should have tokens for the expression
        assert len(tokens) > 0

    def test_trailing_whitespace(self):
        """Test trailing whitespace."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(+ 1 2)    ')

        tokens = list(lexer._tokens)
        assert len(tokens) > 0

    def test_whitespace_around_operators(self):
        """Test whitespace around operators."""
        lexer = AIFPLLexer()
        lexer.lex(None, '( + 1 2 )')

        tokens = list(lexer._tokens)
        ident_tokens = [t for t in tokens if t.type == TokenType.IDENTIFIER]
        assert any(t.value == '+' for t in ident_tokens)

    def test_no_whitespace_in_string(self):
        """Test that whitespace in strings is preserved."""
        lexer = AIFPLLexer()
        lexer.lex(None, '"hello   world"')

        tokens = list(lexer._tokens)
        string_tokens = [t for t in tokens if t.type == TokenType.STRING]
        assert len(string_tokens) == 1
        # Whitespace should be part of the string
        assert '   ' in string_tokens[0].value

    def test_whitespace_separates_identifiers(self):
        """Test whitespace separates identifiers."""
        lexer = AIFPLLexer()
        lexer.lex(None, 'foo bar baz')

        tokens = list(lexer._tokens)
        ident_tokens = [t for t in tokens if t.type == TokenType.IDENTIFIER]
        assert len(ident_tokens) == 3
        assert [t.value for t in ident_tokens] == ['foo', 'bar', 'baz']

    def test_whitespace_separates_numbers(self):
        """Test whitespace separates numbers."""
        lexer = AIFPLLexer()
        lexer.lex(None, '1 2 3 4 5')

        tokens = list(lexer._tokens)
        number_tokens = [t for t in tokens if t.type == TokenType.NUMBER]
        assert len(number_tokens) == 5

    def test_whitespace_after_paren(self):
        """Test whitespace after opening paren."""
        lexer = AIFPLLexer()
        lexer.lex(None, '( + 1 2)')

        tokens = list(lexer._tokens)
        # Should parse correctly
        lparen_tokens = [t for t in tokens if t.type == TokenType.LPAREN]
        assert len(lparen_tokens) == 1

    def test_whitespace_before_paren(self):
        """Test whitespace before closing paren."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(+ 1 2 )')

        tokens = list(lexer._tokens)
        rparen_tokens = [t for t in tokens if t.type == TokenType.RPAREN]
        assert len(rparen_tokens) == 1

    def test_tabs_as_indentation(self):
        """Test tabs as indentation."""
        lexer = AIFPLLexer()
        lexer.lex(None, '\t\t(+ 1 2)')

        tokens = list(lexer._tokens)
        # Should parse the expression
        assert len(tokens) > 0

    def test_spaces_vs_tabs(self):
        """Test that spaces and tabs are both whitespace."""
        lexer1 = AIFPLLexer()
        lexer1.lex(None, '  x')
        tokens1 = list(lexer1._tokens)

        lexer2 = AIFPLLexer()
        lexer2.lex(None, '\tx')
        tokens2 = list(lexer2._tokens)

        # Both should have identifier token
        ident1 = [t for t in tokens1 if t.type == TokenType.IDENTIFIER]
        ident2 = [t for t in tokens2 if t.type == TokenType.IDENTIFIER]
        assert len(ident1) == 1
        assert len(ident2) == 1

    def test_whitespace_in_list(self):
        """Test whitespace in list."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(list 1  2   3    4)')

        tokens = list(lexer._tokens)
        number_tokens = [t for t in tokens if t.type == TokenType.NUMBER]
        assert len(number_tokens) == 4

    def test_whitespace_in_lambda(self):
        """Test whitespace in lambda."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(lambda  (x)  (*  x  x))')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type == TokenType.KEYWORD]
        assert len(keyword_tokens) == 1

    def test_whitespace_in_let(self):
        """Test whitespace in let."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(let  ((x  5))  x)')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type == TokenType.KEYWORD]
        assert len(keyword_tokens) == 1

    def test_no_whitespace_between_quote_and_expr(self):
        """Test no whitespace between quote and expression."""
        lexer = AIFPLLexer()
        lexer.lex(None, "'(+ 1 2)")

        tokens = list(lexer._tokens)
        quote_tokens = [t for t in tokens if t.type == TokenType.QUOTE]
        assert len(quote_tokens) == 1

    def test_whitespace_between_quote_and_expr(self):
        """Test whitespace between quote and expression."""
        lexer = AIFPLLexer()
        lexer.lex(None, "' (+ 1 2)")

        tokens = list(lexer._tokens)
        quote_tokens = [t for t in tokens if t.type == TokenType.QUOTE]
        assert len(quote_tokens) == 1

    def test_varying_whitespace_amounts(self):
        """Test varying amounts of whitespace."""
        test_cases = [
            '1 2',
            '1  2',
            '1   2',
            '1    2',
            '1     2',
        ]

        for expr in test_cases:
            lexer = AIFPLLexer()
            lexer.lex(None, expr)

            tokens = list(lexer._tokens)
            number_tokens = [t for t in tokens if t.type == TokenType.NUMBER]
            assert len(number_tokens) == 2, f"Expression '{expr}' should have 2 numbers"

    def test_whitespace_does_not_affect_tokenization(self):
        """Test that extra whitespace doesn't change token count."""
        lexer1 = AIFPLLexer()
        lexer1.lex(None, '(+ 1 2)')
        tokens1 = list(lexer1._tokens)

        lexer2 = AIFPLLexer()
        lexer2.lex(None, '(  +   1   2  )')
        tokens2 = list(lexer2._tokens)

        # Should have same tokens (whitespace is skipped)
        assert len(tokens1) == len(tokens2)

    def test_carriage_return_as_whitespace(self):
        """Test carriage return as whitespace."""
        lexer = AIFPLLexer()
        lexer.lex(None, '\r')

        tokens = list(lexer._tokens)
        # CR should be skipped
        assert len(tokens) == 0

    def test_form_feed_as_whitespace(self):
        """Test form feed as whitespace."""
        lexer = AIFPLLexer()
        lexer.lex(None, '\f')

        tokens = list(lexer._tokens)
        # FF should be skipped
        assert len(tokens) == 0

    def test_vertical_tab_as_whitespace(self):
        """Test vertical tab as whitespace."""
        lexer = AIFPLLexer()
        lexer.lex(None, '\v')

        tokens = list(lexer._tokens)
        # VT should be skipped
        assert len(tokens) == 0

    def test_whitespace_positions(self):
        """Test that token positions are tracked correctly with whitespace."""
        lexer = AIFPLLexer()
        lexer.lex(None, '1 2 3')

        tokens = list(lexer._tokens)
        # Positions should be in order
        for i in range(len(tokens) - 1):
            assert tokens[i].start <= tokens[i + 1].start

    def test_consecutive_whitespace_tokens(self):
        """Test consecutive whitespace."""
        lexer = AIFPLLexer()
        lexer.lex(None, '1     2')

        tokens = list(lexer._tokens)
        number_tokens = [t for t in tokens if t.type == TokenType.NUMBER]
        assert len(number_tokens) == 2

    def test_whitespace_in_nested_expressions(self):
        """Test whitespace in nested expressions."""
        lexer = AIFPLLexer()
        lexer.lex(None, '( ( + 1 2 ) )')

        tokens = list(lexer._tokens)
        lparen_count = sum(1 for t in tokens if t.type == TokenType.LPAREN)
        rparen_count = sum(1 for t in tokens if t.type == TokenType.RPAREN)
        assert lparen_count == 2
        assert rparen_count == 2
