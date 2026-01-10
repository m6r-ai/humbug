"""
Tests for AIFPL comment tokenization.
"""
import pytest

from syntax.aifpl.aifpl_lexer import AIFPLLexer
from syntax.lexer import TokenType


class TestAIFPLComments:
    """Test AIFPL comment tokenization."""

    def test_simple_comment(self):
        """Test simple comment."""
        lexer = AIFPLLexer()
        lexer.lex(None, '; this is a comment')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT
        assert tokens[0].value == '; this is a comment'

    def test_comment_at_end_of_line(self):
        """Test comment at end of line with code."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(+ 1 2) ; add numbers')

        tokens = list(lexer._tokens)
        comment_tokens = [t for t in tokens if t.type == TokenType.COMMENT]
        assert len(comment_tokens) == 1
        assert comment_tokens[0].value == '; add numbers'

    def test_comment_with_code_symbols(self):
        """Test comment containing code-like symbols."""
        lexer = AIFPLLexer()
        lexer.lex(None, '; (+ 1 2) this looks like code')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT

    def test_comment_with_special_chars(self):
        """Test comment with special characters."""
        lexer = AIFPLLexer()
        lexer.lex(None, '; !@#$%^&*(){}[]<>?')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT

    def test_comment_with_strings(self):
        """Test comment containing string-like content."""
        lexer = AIFPLLexer()
        lexer.lex(None, '; "this is not a string"')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT

    def test_comment_with_numbers(self):
        """Test comment containing numbers."""
        lexer = AIFPLLexer()
        lexer.lex(None, '; 123 456 789')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT

    def test_comment_only_semicolon(self):
        """Test comment that is only a semicolon."""
        lexer = AIFPLLexer()
        lexer.lex(None, ';')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT
        assert tokens[0].value == ';'

    def test_comment_with_leading_spaces(self):
        """Test comment with leading spaces."""
        lexer = AIFPLLexer()
        lexer.lex(None, '    ; indented comment')

        tokens = list(lexer._tokens)
        comment_tokens = [t for t in tokens if t.type == TokenType.COMMENT]
        assert len(comment_tokens) == 1
        assert '; indented comment' in comment_tokens[0].value

    def test_multiple_semicolons(self):
        """Test comment with multiple semicolons."""
        lexer = AIFPLLexer()
        lexer.lex(None, ';;; header comment')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT
        assert tokens[0].value == ';;; header comment'

    def test_comment_after_expression(self):
        """Test comment after complete expression."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(lambda (x) x) ; identity function')

        tokens = list(lexer._tokens)
        comment_tokens = [t for t in tokens if t.type == TokenType.COMMENT]
        assert len(comment_tokens) == 1
        assert 'identity function' in comment_tokens[0].value

    def test_comment_position(self):
        """Test that comment position is correctly tracked."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(+ 1 2) ; comment')

        tokens = list(lexer._tokens)
        comment_tokens = [t for t in tokens if t.type == TokenType.COMMENT]
        assert len(comment_tokens) == 1
        # Comment should start after the expression
        assert comment_tokens[0].start > 0

    def test_comment_with_unicode(self):
        """Test comment with unicode characters."""
        lexer = AIFPLLexer()
        lexer.lex(None, '; こんにちは world 🌍')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT

    def test_comment_with_url(self):
        """Test comment containing URL."""
        lexer = AIFPLLexer()
        lexer.lex(None, '; https://example.com')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT

    def test_comment_with_email(self):
        """Test comment containing email."""
        lexer = AIFPLLexer()
        lexer.lex(None, '; user@example.com')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT

    def test_comment_extends_to_end_of_line(self):
        """Test that comment extends to end of line."""
        lexer = AIFPLLexer()
        lexer.lex(None, '; comment text')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT
        # Should include everything from ; to end
        assert tokens[0].value == '; comment text'

    def test_code_before_comment(self):
        """Test various code patterns before comments."""
        test_cases = [
            '42 ; number',
            '"string" ; text',
            '#t ; boolean',
            'identifier ; name',
            '(+ 1 2) ; expression',
        ]

        for code in test_cases:
            lexer = AIFPLLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            comment_tokens = [t for t in tokens if t.type == TokenType.COMMENT]
            assert len(comment_tokens) == 1, f"Should have one comment in '{code}'"

    def test_comment_with_keywords(self):
        """Test comment containing keywords."""
        lexer = AIFPLLexer()
        lexer.lex(None, '; lambda if let quote')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT
        # Keywords in comments should not be tokenized as keywords

    def test_comment_with_operators(self):
        """Test comment containing operators."""
        lexer = AIFPLLexer()
        lexer.lex(None, '; + - * / = < >')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT

    def test_empty_line_after_comment(self):
        """Test that comment doesn't affect next line."""
        lexer1 = AIFPLLexer()
        state1 = lexer1.lex(None, '; comment')

        lexer2 = AIFPLLexer()
        state2 = lexer2.lex(state1, '(+ 1 2)')

        tokens2 = list(lexer2._tokens)
        # Should parse normally, no comment tokens
        comment_tokens = [t for t in tokens2 if t.type == TokenType.COMMENT]
        assert len(comment_tokens) == 0

    def test_comment_does_not_continue(self):
        """Test that comments don't continue to next line."""
        lexer1 = AIFPLLexer()
        state1 = lexer1.lex(None, '; line 1')

        lexer2 = AIFPLLexer()
        state2 = lexer2.lex(state1, '42')

        tokens2 = list(lexer2._tokens)
        # Should parse 42 as number, not comment
        assert any(t.type == TokenType.NUMBER for t in tokens2)

    def test_comment_in_nested_expression(self):
        """Test comment after nested expression."""
        lexer = AIFPLLexer()
        lexer.lex(None, '((lambda (x) x) 5) ; apply')

        tokens = list(lexer._tokens)
        comment_tokens = [t for t in tokens if t.type == TokenType.COMMENT]
        assert len(comment_tokens) == 1

    def test_comment_with_quote_char(self):
        """Test comment containing quote character."""
        lexer = AIFPLLexer()
        lexer.lex(None, "; it's a comment")

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT

    def test_comment_with_double_quote(self):
        """Test comment containing double quote."""
        lexer = AIFPLLexer()
        lexer.lex(None, '; "quoted" text')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT

    def test_comment_with_backslash(self):
        """Test comment containing backslash."""
        lexer = AIFPLLexer()
        lexer.lex(None, r'; path\to\file')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT

    def test_docstring_style_comment(self):
        """Test docstring-style comment."""
        lexer = AIFPLLexer()
        lexer.lex(None, '; Function: add two numbers')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT

    def test_todo_comment(self):
        """Test TODO-style comment."""
        lexer = AIFPLLexer()
        lexer.lex(None, '; TODO: implement this')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT

    def test_fixme_comment(self):
        """Test FIXME-style comment."""
        lexer = AIFPLLexer()
        lexer.lex(None, '; FIXME: bug here')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT
