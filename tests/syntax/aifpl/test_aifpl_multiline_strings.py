"""
Tests for AIFPL multiline string handling.
"""
import pytest

from syntax.aifpl.aifpl_lexer import AIFPLLexer
from syntax.lexer import TokenType


class TestAIFPLMultilineStrings:
    """Test AIFPL multiline string handling."""

    def test_multiline_string_with_escape_in_continuation(self):
        """Test multiline string with escape sequence in continuation line."""
        lexer1 = AIFPLLexer()
        state1 = lexer1.lex(None, '"hello')

        # Second line with escape sequence
        lexer2 = AIFPLLexer()
        state2 = lexer2.lex(state1, '\\nworld"')

        tokens2 = list(lexer2._tokens)
        assert len(tokens2) == 1
        assert tokens2[0].type == TokenType.STRING
        assert state2.in_string is False

    def test_multiline_string_with_escaped_quote_in_continuation(self):
        """Test multiline string with escaped quote in continuation line."""
        lexer1 = AIFPLLexer()
        state1 = lexer1.lex(None, '"hello')

        # Second line with escaped quote
        lexer2 = AIFPLLexer()
        state2 = lexer2.lex(state1, '\\"world"')

        tokens2 = list(lexer2._tokens)
        assert len(tokens2) == 1
        assert tokens2[0].type == TokenType.STRING
        assert state2.in_string is False

    def test_multiline_string_with_escaped_backslash_in_continuation(self):
        """Test multiline string with escaped backslash in continuation line."""
        lexer1 = AIFPLLexer()
        state1 = lexer1.lex(None, '"hello')

        # Second line with escaped backslash
        lexer2 = AIFPLLexer()
        state2 = lexer2.lex(state1, '\\\\world"')

        tokens2 = list(lexer2._tokens)
        assert len(tokens2) == 1
        assert tokens2[0].type == TokenType.STRING
        assert state2.in_string is False

    def test_multiline_string_with_escape_at_end_of_continuation(self):
        """Test multiline string with escape at end of continuation line."""
        lexer1 = AIFPLLexer()
        state1 = lexer1.lex(None, '"hello')

        # Second line ending with escape
        lexer2 = AIFPLLexer()
        state2 = lexer2.lex(state1, 'world\\')

        tokens2 = list(lexer2._tokens)
        assert len(tokens2) == 1
        assert tokens2[0].type == TokenType.STRING
        # Still in string because escape at end
        assert state2.in_string is True

    def test_multiline_string_with_multiple_escapes_in_continuation(self):
        """Test multiline string with multiple escape sequences in continuation."""
        lexer1 = AIFPLLexer()
        state1 = lexer1.lex(None, '"hello')

        # Second line with multiple escapes
        lexer2 = AIFPLLexer()
        state2 = lexer2.lex(state1, '\\n\\t\\r"')

        tokens2 = list(lexer2._tokens)
        assert len(tokens2) == 1
        assert tokens2[0].type == TokenType.STRING
        assert state2.in_string is False

    def test_multiline_string_continuation_with_unicode_escape(self):
        """Test multiline string continuation with unicode escape."""
        lexer1 = AIFPLLexer()
        state1 = lexer1.lex(None, '"hello')

        # Second line with unicode escape
        lexer2 = AIFPLLexer()
        state2 = lexer2.lex(state1, '\\u0041world"')

        tokens2 = list(lexer2._tokens)
        assert len(tokens2) == 1
        assert tokens2[0].type == TokenType.STRING
        assert state2.in_string is False

    def test_multiline_string_continuation_starts_with_escape(self):
        """Test multiline string continuation that starts with escape."""
        lexer1 = AIFPLLexer()
        state1 = lexer1.lex(None, '"test')

        # Second line starts with escape
        lexer2 = AIFPLLexer()
        state2 = lexer2.lex(state1, '\\nstart"')

        tokens2 = list(lexer2._tokens)
        assert len(tokens2) == 1
        assert tokens2[0].type == TokenType.STRING
        assert '\\nstart"' in tokens2[0].value

    def test_multiline_string_continuation_only_escape(self):
        """Test multiline string continuation with only escape sequence."""
        lexer1 = AIFPLLexer()
        state1 = lexer1.lex(None, '"test')

        # Second line is just escape and close
        lexer2 = AIFPLLexer()
        state2 = lexer2.lex(state1, '\\n"')

        tokens2 = list(lexer2._tokens)
        assert len(tokens2) == 1
        assert tokens2[0].type == TokenType.STRING
        assert state2.in_string is False

    def test_multiline_string_continuation_escape_before_quote(self):
        """Test multiline string continuation with escape right before closing quote."""
        lexer1 = AIFPLLexer()
        state1 = lexer1.lex(None, '"test')

        # Second line with escape before quote
        lexer2 = AIFPLLexer()
        state2 = lexer2.lex(state1, 'value\\"')

        tokens2 = list(lexer2._tokens)
        assert len(tokens2) == 1
        assert tokens2[0].type == TokenType.STRING
        # The escaped quote doesn't close the string
        assert state2.in_string is True

    def test_multiline_string_three_line_with_escapes(self):
        """Test multiline string across three lines with escapes."""
        lexer1 = AIFPLLexer()
        state1 = lexer1.lex(None, '"line1')

        lexer2 = AIFPLLexer()
        state2 = lexer2.lex(state1, '\\nline2')

        lexer3 = AIFPLLexer()
        state3 = lexer3.lex(state2, '\\nline3"')

        assert state1.in_string is True
        assert state2.in_string is True
        assert state3.in_string is False

    def test_multiline_string_continuation_with_tab_escape(self):
        """Test multiline string continuation with tab escape."""
        lexer1 = AIFPLLexer()
        state1 = lexer1.lex(None, '"hello')

        lexer2 = AIFPLLexer()
        state2 = lexer2.lex(state1, '\\tworld"')

        tokens2 = list(lexer2._tokens)
        assert len(tokens2) == 1
        assert tokens2[0].type == TokenType.STRING

    def test_multiline_string_continuation_with_carriage_return_escape(self):
        """Test multiline string continuation with carriage return escape."""
        lexer1 = AIFPLLexer()
        state1 = lexer1.lex(None, '"hello')

        lexer2 = AIFPLLexer()
        state2 = lexer2.lex(state1, '\\rworld"')

        tokens2 = list(lexer2._tokens)
        assert len(tokens2) == 1
        assert tokens2[0].type == TokenType.STRING

    def test_multiline_string_continuation_mixed_content_and_escapes(self):
        """Test multiline string continuation with mixed content and escapes."""
        lexer1 = AIFPLLexer()
        state1 = lexer1.lex(None, '"start')

        lexer2 = AIFPLLexer()
        state2 = lexer2.lex(state1, 'middle\\nmiddle2')

        lexer3 = AIFPLLexer()
        state3 = lexer3.lex(state2, 'end"')

        tokens3 = list(lexer3._tokens)
        assert len(tokens3) == 1
        assert tokens3[0].type == TokenType.STRING
        assert state3.in_string is False

    def test_multiline_string_continuation_empty_line(self):
        """Test multiline string continuation with empty line."""
        lexer1 = AIFPLLexer()
        state1 = lexer1.lex(None, '"start')

        lexer2 = AIFPLLexer()
        state2 = lexer2.lex(state1, '')

        lexer3 = AIFPLLexer()
        state3 = lexer3.lex(state2, 'end"')

        # Empty line should still be in string
        assert state2.in_string is True
        assert state3.in_string is False

    def test_multiline_string_continuation_whitespace_only(self):
        """Test multiline string continuation with whitespace only."""
        lexer1 = AIFPLLexer()
        state1 = lexer1.lex(None, '"start')

        lexer2 = AIFPLLexer()
        state2 = lexer2.lex(state1, '   ')

        tokens2 = list(lexer2._tokens)
        assert len(tokens2) == 1
        assert tokens2[0].type == TokenType.STRING
        assert state2.in_string is True
