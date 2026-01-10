"""
Tests for AIFPL string tokenization.
"""
import pytest

from syntax.aifpl.aifpl_lexer import AIFPLLexer, AIFPLLexerState
from syntax.lexer import TokenType


class TestAIFPLStrings:
    """Test AIFPL string tokenization."""

    def test_simple_string(self):
        """Test simple string literals."""
        test_cases = ['"hello"', '"world"', '"test"', '""']
        for string in test_cases:
            lexer = AIFPLLexer()
            lexer.lex(None, string)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"String '{string}' should produce one token"
            assert tokens[0].type == TokenType.STRING
            assert tokens[0].value == string

    def test_string_with_spaces(self):
        """Test strings containing spaces."""
        test_cases = ['"hello world"', '"  spaces  "', '" "']
        for string in test_cases:
            lexer = AIFPLLexer()
            lexer.lex(None, string)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.STRING
            assert tokens[0].value == string

    def test_string_with_escape_sequences(self):
        """Test strings with escape sequences."""
        test_cases = [
            r'"hello\nworld"',
            r'"tab\there"',
            r'"quote\"inside"',
            r'"backslash\\"',
            r'"unicode\u0041"',
        ]
        for string in test_cases:
            lexer = AIFPLLexer()
            lexer.lex(None, string)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"String with escapes '{string}' should produce one token"
            assert tokens[0].type == TokenType.STRING
            assert tokens[0].value == string

    def test_empty_string(self):
        """Test empty string."""
        lexer = AIFPLLexer()
        lexer.lex(None, '""')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING
        assert tokens[0].value == '""'

    def test_string_in_expression(self):
        """Test strings in expressions."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(string-append "hello" " " "world")')

        tokens = list(lexer._tokens)
        string_tokens = [t for t in tokens if t.type == TokenType.STRING]
        assert len(string_tokens) == 3
        assert string_tokens[0].value == '"hello"'
        assert string_tokens[1].value == '" "'
        assert string_tokens[2].value == '"world"'

    def test_multiline_string_unclosed(self):
        """Test that unclosed strings are handled."""
        lexer = AIFPLLexer()
        state = lexer.lex(None, '"hello')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING
        assert tokens[0].value == '"hello'
        assert state.in_string is True

    def test_multiline_string_continuation(self):
        """Test multiline string continuation."""
        lexer = AIFPLLexer()

        # First line - unclosed string
        state1 = lexer.lex(None, '"hello')
        tokens1 = list(lexer._tokens)
        assert len(tokens1) == 1
        assert tokens1[0].type == TokenType.STRING
        assert state1.in_string is True

        # Second line - continuation
        lexer2 = AIFPLLexer()
        state2 = lexer2.lex(state1, 'world"')
        tokens2 = list(lexer2._tokens)
        assert len(tokens2) == 1
        assert tokens2[0].type == TokenType.STRING
        assert tokens2[0].value == 'world"'
        assert state2.in_string is False

    def test_multiline_string_multiple_lines(self):
        """Test string spanning multiple lines."""
        lexer1 = AIFPLLexer()
        state1 = lexer1.lex(None, '"line1')

        lexer2 = AIFPLLexer()
        state2 = lexer2.lex(state1, 'line2')

        lexer3 = AIFPLLexer()
        state3 = lexer3.lex(state2, 'line3"')

        assert state1.in_string is True
        assert state2.in_string is True
        assert state3.in_string is False

    def test_string_with_escaped_quote(self):
        """Test string with escaped quote doesn't end the string."""
        lexer = AIFPLLexer()
        lexer.lex(None, r'"say \"hello\""')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING
        assert tokens[0].value == r'"say \"hello\""'

    def test_string_with_escaped_backslash(self):
        """Test string with escaped backslash."""
        lexer = AIFPLLexer()
        lexer.lex(None, r'"path\\to\\file"')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING

    def test_adjacent_strings(self):
        """Test multiple strings separated by whitespace."""
        lexer = AIFPLLexer()
        lexer.lex(None, '"hello" "world"')

        tokens = list(lexer._tokens)
        string_tokens = [t for t in tokens if t.type == TokenType.STRING]
        assert len(string_tokens) == 2
        assert string_tokens[0].value == '"hello"'
        assert string_tokens[1].value == '"world"'

    def test_string_with_parentheses(self):
        """Test strings containing parentheses."""
        lexer = AIFPLLexer()
        lexer.lex(None, '"(hello)"')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING
        assert tokens[0].value == '"(hello)"'

    def test_string_with_numbers(self):
        """Test strings containing numbers."""
        lexer = AIFPLLexer()
        lexer.lex(None, '"123"')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING
        assert tokens[0].value == '"123"'

    def test_string_with_special_chars(self):
        """Test strings with special characters."""
        test_cases = [
            '"!@#$%^&*()"',
            '"<>=+-*/"',
            '"[]{}|"',
        ]
        for string in test_cases:
            lexer = AIFPLLexer()
            lexer.lex(None, string)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.STRING

    def test_multiple_strings_in_list(self):
        """Test multiple strings in a list expression."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(list "a" "b" "c")')

        tokens = list(lexer._tokens)
        string_tokens = [t for t in tokens if t.type == TokenType.STRING]
        assert len(string_tokens) == 3
        assert [t.value for t in string_tokens] == ['"a"', '"b"', '"c"']

    def test_string_after_identifier(self):
        """Test string following an identifier."""
        lexer = AIFPLLexer()
        lexer.lex(None, 'name "value"')

        tokens = list(lexer._tokens)
        # whitespace is skipped
        assert len(tokens) == 2
        assert tokens[0].type == TokenType.IDENTIFIER
        assert tokens[1].type == TokenType.STRING

    def test_unclosed_string_at_end_of_line(self):
        """Test unclosed string at end of line maintains state."""
        lexer = AIFPLLexer()
        state = lexer.lex(None, '(print "hello')

        tokens = list(lexer._tokens)
        string_tokens = [t for t in tokens if t.type == TokenType.STRING]
        assert len(string_tokens) == 1
        assert state.in_string is True

    def test_string_continuation_after_code(self):
        """Test string continuation after other code."""
        lexer1 = AIFPLLexer()
        state1 = lexer1.lex(None, '(+ 1 "start')

        lexer2 = AIFPLLexer()
        state2 = lexer2.lex(state1, 'end")')

        assert state1.in_string is True
        assert state2.in_string is False

        tokens2 = list(lexer2._tokens)
        assert tokens2[0].type == TokenType.STRING

    def test_escaped_newline_in_string(self):
        """Test escaped newline in string."""
        lexer = AIFPLLexer()
        lexer.lex(None, r'"line1\nline2"')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING

    def test_escaped_tab_in_string(self):
        """Test escaped tab in string."""
        lexer = AIFPLLexer()
        lexer.lex(None, r'"col1\tcol2"')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING

    def test_unicode_escape_in_string(self):
        """Test unicode escape sequences."""
        lexer = AIFPLLexer()
        lexer.lex(None, r'"unicode \u0041 char"')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING

    def test_string_with_only_escapes(self):
        """Test string containing only escape sequences."""
        lexer = AIFPLLexer()
        lexer.lex(None, r'"\n\t\r"')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING
