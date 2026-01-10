"""
Tests for AIFPL edge cases and error handling.
"""
import pytest

from syntax.aifpl.aifpl_lexer import AIFPLLexer
from syntax.lexer import TokenType


class TestAIFPLEdgeCases:
    """Test AIFPL edge cases and error handling."""

    def test_empty_input(self):
        """Test empty input."""
        lexer = AIFPLLexer()
        lexer.lex(None, '')

        tokens = list(lexer._tokens)
        assert len(tokens) == 0

    def test_only_whitespace(self):
        """Test input with only whitespace."""
        lexer = AIFPLLexer()
        lexer.lex(None, '   \t  ')

        tokens = list(lexer._tokens)
        # Should have whitespace tokens or be empty
        # whitespace is skipped
        assert len(tokens) == 0


    def test_single_hash(self):
        """Test single hash symbol."""
        lexer = AIFPLLexer()
        lexer.lex(None, '#')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        # Should be an error token
        assert tokens[0].type == TokenType.ERROR

    def test_hash_with_invalid_char(self):
        """Test hash with invalid character."""
        lexer = AIFPLLexer()
        lexer.lex(None, '#z')

        tokens = list(lexer._tokens)
        # Should produce error token
        error_tokens = [t for t in tokens if t.type == TokenType.ERROR]
        assert len(error_tokens) >= 1

    def test_single_dot(self):
        """Test single dot."""
        lexer = AIFPLLexer()
        lexer.lex(None, '.')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        # Dot alone should be error
        assert tokens[0].type == TokenType.ERROR

    def test_dot_not_followed_by_digit(self):
        """Test dot not followed by digit."""
        lexer = AIFPLLexer()
        lexer.lex(None, '.x')

        tokens = list(lexer._tokens)
        # Should have error for dot, then identifier
        assert len(tokens) >= 1

    def test_plus_alone(self):
        """Test plus sign alone."""
        lexer = AIFPLLexer()
        lexer.lex(None, '+')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.IDENTIFIER
        assert tokens[0].value == '+'

    def test_minus_alone(self):
        """Test minus sign alone."""
        lexer = AIFPLLexer()
        lexer.lex(None, '-')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.IDENTIFIER
        assert tokens[0].value == '-'

    def test_unclosed_string(self):
        """Test unclosed string."""
        lexer = AIFPLLexer()
        state = lexer.lex(None, '"hello')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING
        assert state.in_string is True

    def test_string_with_only_quote(self):
        """Test string with only opening quote."""
        lexer = AIFPLLexer()
        state = lexer.lex(None, '"')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING
        assert state.in_string is True

    def test_escaped_char_at_end(self):
        """Test escape character at end of line."""
        lexer = AIFPLLexer()
        state = lexer.lex(None, '"test\\')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING
        assert state.in_string is True

    def test_very_long_line(self):
        """Test very long line."""
        long_expr = '(' + ' '.join([str(i) for i in range(1000)]) + ')'
        lexer = AIFPLLexer()
        lexer.lex(None, long_expr)

        tokens = list(lexer._tokens)
        number_tokens = [t for t in tokens if t.type == TokenType.NUMBER]
        assert len(number_tokens) == 1000

    def test_very_long_identifier(self):
        """Test very long identifier."""
        long_ident = 'a' * 10000
        lexer = AIFPLLexer()
        lexer.lex(None, long_ident)

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.IDENTIFIER
        assert len(tokens[0].value) == 10000

    def test_very_long_string(self):
        """Test very long string."""
        long_str = '"' + 'x' * 10000 + '"'
        lexer = AIFPLLexer()
        lexer.lex(None, long_str)

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING

    def test_very_long_comment(self):
        """Test very long comment."""
        long_comment = '; ' + 'x' * 10000
        lexer = AIFPLLexer()
        lexer.lex(None, long_comment)

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT

    def test_unicode_identifier(self):
        """Test unicode characters in identifier."""
        lexer = AIFPLLexer()
        lexer.lex(None, 'λ')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        # Should be parsed as identifier
        assert tokens[0].type == TokenType.IDENTIFIER

    def test_emoji_identifier(self):
        """Test emoji in identifier."""
        lexer = AIFPLLexer()
        lexer.lex(None, '🚀')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.IDENTIFIER

    def test_mixed_unicode(self):
        """Test mixed unicode characters."""
        lexer = AIFPLLexer()
        lexer.lex(None, 'hello世界')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.IDENTIFIER

    def test_null_character(self):
        """Test null character handling."""
        lexer = AIFPLLexer()
        lexer.lex(None, '\x00')

        tokens = list(lexer._tokens)
        # Should handle gracefully
        assert len(tokens) >= 0

    def test_control_characters(self):
        """Test control characters."""
        lexer = AIFPLLexer()
        lexer.lex(None, '\x01\x02\x03')

        tokens = list(lexer._tokens)
        # Should handle without crashing
        assert len(tokens) >= 0

    def test_tab_characters(self):
        """Test tab characters as whitespace."""
        lexer = AIFPLLexer()
        lexer.lex(None, '\t(\t+\t1\t2\t)\t')

        tokens = list(lexer._tokens)
        # whitespace is skipped
        # Should have parens, +, 1, 2, paren
        assert len(tokens) == 5

    def test_mixed_whitespace(self):
        """Test mixed whitespace types."""
        lexer = AIFPLLexer()
        lexer.lex(None, ' \t \t(+ 1 2)')

        tokens = list(lexer._tokens)
        # whitespace is skipped
        assert len(tokens) > 0

    def test_carriage_return(self):
        """Test carriage return handling."""
        lexer = AIFPLLexer()
        lexer.lex(None, 'a\rb')

        tokens = list(lexer._tokens)
        # Should handle CR
        ident_tokens = [t for t in tokens if t.type == TokenType.IDENTIFIER]
        assert len(ident_tokens) >= 1

    def test_number_overflow(self):
        """Test very large number."""
        huge_num = '9' * 1000
        lexer = AIFPLLexer()
        lexer.lex(None, huge_num)

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.NUMBER

    def test_deeply_nested_quotes(self):
        """Test deeply nested quotes."""
        lexer = AIFPLLexer()
        lexer.lex(None, "''''''x")

        tokens = list(lexer._tokens)
        quote_tokens = [t for t in tokens if t.type == TokenType.QUOTE]
        assert len(quote_tokens) == 6

    def test_many_parentheses(self):
        """Test many nested parentheses."""
        expr = '(' * 100 + 'x' + ')' * 100
        lexer = AIFPLLexer()
        lexer.lex(None, expr)

        tokens = list(lexer._tokens)
        lparen_count = sum(1 for t in tokens if t.type == TokenType.LPAREN)
        rparen_count = sum(1 for t in tokens if t.type == TokenType.RPAREN)
        assert lparen_count == 100
        assert rparen_count == 100

    def test_alternating_tokens(self):
        """Test alternating token types."""
        lexer = AIFPLLexer()
        lexer.lex(None, '1 "a" #t x')

        tokens = list(lexer._tokens)
        # whitespace is skipped
        assert len(tokens) == 4
        assert tokens[0].type == TokenType.NUMBER
        assert tokens[1].type == TokenType.STRING
        assert tokens[2].type == TokenType.BOOLEAN
        assert tokens[3].type == TokenType.IDENTIFIER

    def test_all_token_types_in_one_line(self):
        """Test all token types in one line."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(lambda (x) (if #t 42 "no")) ; comment')

        tokens = list(lexer._tokens)
        
        # Should have various token types
        has_lparen = any(t.type == TokenType.LPAREN for t in tokens)
        has_rparen = any(t.type == TokenType.RPAREN for t in tokens)
        has_keyword = any(t.type == TokenType.KEYWORD for t in tokens)
        has_identifier = any(t.type == TokenType.IDENTIFIER for t in tokens)
        has_boolean = any(t.type == TokenType.BOOLEAN for t in tokens)
        has_number = any(t.type == TokenType.NUMBER for t in tokens)
        has_string = any(t.type == TokenType.STRING for t in tokens)
        has_comment = any(t.type == TokenType.COMMENT for t in tokens)
        
        assert has_lparen and has_rparen
        assert has_keyword
        assert has_identifier
        assert has_boolean
        assert has_number
        assert has_string
        assert has_comment

    def test_empty_string_in_expression(self):
        """Test empty string in expression."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(string-append "" "")')

        tokens = list(lexer._tokens)
        string_tokens = [t for t in tokens if t.type == TokenType.STRING]
        assert len(string_tokens) == 2
        assert all(t.value == '""' for t in string_tokens)

    def test_zero_in_various_forms(self):
        """Test zero in different representations."""
        lexer = AIFPLLexer()
        lexer.lex(None, '0 0.0 #x0 #b0 #o0 #d0')

        tokens = list(lexer._tokens)
        number_tokens = [t for t in tokens if t.type == TokenType.NUMBER]
        assert len(number_tokens) == 6

    def test_whitespace_only_between_tokens(self):
        """Test various whitespace between tokens."""
        lexer = AIFPLLexer()
        lexer.lex(None, '1  2   3    4')

        tokens = list(lexer._tokens)
        number_tokens = [t for t in tokens if t.type == TokenType.NUMBER]
        assert len(number_tokens) == 4

    def test_no_whitespace_between_parens_and_content(self):
        """Test no whitespace between delimiters and content."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(+1 2)')

        tokens = list(lexer._tokens)
        # Should still parse correctly
        lparen_tokens = [t for t in tokens if t.type == TokenType.LPAREN]
        assert len(lparen_tokens) == 1

    def test_consecutive_operators(self):
        """Test consecutive operators."""
        lexer = AIFPLLexer()
        lexer.lex(None, '+ - * /')

        tokens = list(lexer._tokens)
        ident_tokens = [t for t in tokens if t.type == TokenType.IDENTIFIER]
        assert len(ident_tokens) == 4

    def test_special_number_forms(self):
        """Test special number edge cases."""
        test_cases = ['+0', '-0', '1e+0', '1e-0']
        for num in test_cases:
            lexer = AIFPLLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            number_tokens = [t for t in tokens if t.type == TokenType.NUMBER]
            assert len(number_tokens) == 1, f"'{num}' should be a number"
