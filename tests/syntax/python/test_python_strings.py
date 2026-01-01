"""
Tests for Python string tokenization.
"""
import pytest

from syntax.python.python_lexer import PythonLexer


class TestPythonStrings:
    """Test Python string tokenization."""

    def test_single_quoted_strings(self):
        """Test single-quoted strings."""
        test_cases = [
            "'hello'",
            "'world'",
            "'with spaces'",
            "'123'",
        ]
        for s in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"String '{s}' should produce one token"
            assert tokens[0].type.name == 'STRING', f"String '{s}' should be STRING type"

    def test_double_quoted_strings(self):
        """Test double-quoted strings."""
        test_cases = [
            '"hello"',
            '"world"',
            '"with spaces"',
            '"123"',
        ]
        for s in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"String '{s}' should produce one token"
            assert tokens[0].type.name == 'STRING', f"String '{s}' should be STRING type"

    def test_triple_single_quoted_strings(self):
        """Test triple single-quoted strings (docstrings)."""
        test_cases = [
            "'''hello'''",
            "'''multiline\nstring'''",
            "'''with spaces'''",
        ]
        for s in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Docstring '{s}' should produce one token"
            assert tokens[0].type.name == 'STRING', f"Docstring '{s}' should be STRING type"

    def test_triple_double_quoted_strings(self):
        """Test triple double-quoted strings (docstrings)."""
        test_cases = [
            '"""hello"""',
            '"""multiline\nstring"""',
            '"""with spaces"""',
        ]
        for s in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Docstring '{s}' should produce one token"
            assert tokens[0].type.name == 'STRING', f"Docstring '{s}' should be STRING type"

    def test_empty_strings(self):
        """Test empty strings."""
        test_cases = ["''", '""', "''''''", '""""""']
        for s in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Empty string '{s}' should produce one token"
            assert tokens[0].type.name == 'STRING', f"Empty string should be STRING type"

    def test_string_with_escaped_quotes(self):
        """Test strings containing escaped quote characters."""
        test_cases = [
            r"'She said \'hello\''",
            r'"He said \"hi\""',
            r"'It\'s'",
            r'"Say \"quote\""',
        ]
        for s in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"String with escaped quotes '{s}' should produce one token"
            assert tokens[0].type.name == 'STRING'

    def test_string_with_other_quote_type(self):
        """Test strings containing the other quote type."""
        test_cases = [
            "'She said \"hello\"'",
            '"He said \'hi\'"',
        ]
        for s in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"String '{s}' should produce one token"
            assert tokens[0].type.name == 'STRING'

    def test_string_with_escape_sequences(self):
        """Test strings with various escape sequences."""
        test_cases = [
            r"'line1\nline2'",
            r"'tab\there'",
            r"'backslash\\'",
            r"'carriage\rreturn'",
            r"'null\0byte'",
            r"'unicode\u0041'",
            r"'hex\x41'",
        ]
        for s in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"String with escapes '{s}' should produce one token"
            assert tokens[0].type.name == 'STRING'

    def test_raw_strings(self):
        """Test raw strings (r-prefix)."""
        test_cases = [
            r"r'raw string'",
            r'r"raw string"',
            r"r'with\nnewline'",
            r'r"with\ttab"',
        ]
        for s in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            # Raw string prefix 'r' is an identifier followed by string
            assert len(tokens) == 2, f"Raw string '{s}' should produce two tokens"
            assert tokens[0].type.name == 'IDENTIFIER', "First token should be identifier 'r'"
            assert tokens[1].type.name == 'STRING', "Second token should be STRING"

    def test_formatted_strings(self):
        """Test f-strings (f-prefix)."""
        test_cases = [
            "f'hello'",
            'f"world"',
            "f'value: {x}'",
        ]
        for s in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            # F-string prefix 'f' is an identifier followed by string
            assert len(tokens) >= 2, f"F-string '{s}' should produce at least two tokens"
            assert tokens[0].type.name == 'IDENTIFIER', "First token should be identifier 'f'"
            assert tokens[1].type.name == 'STRING', "Second token should be STRING"

    def test_byte_strings(self):
        """Test byte strings (b-prefix)."""
        test_cases = [
            "b'bytes'",
            'b"bytes"',
            "b'\\x00\\x01'",
        ]
        for s in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            # Byte string prefix 'b' is an identifier followed by string
            assert len(tokens) == 2, f"Byte string '{s}' should produce two tokens"
            assert tokens[0].type.name == 'IDENTIFIER', "First token should be identifier 'b'"
            assert tokens[1].type.name == 'STRING', "Second token should be STRING"

    def test_string_concatenation(self):
        """Test adjacent string concatenation."""
        lexer = PythonLexer()
        lexer.lex(None, '"hello" "world"')

        tokens = list(lexer._tokens)
        string_tokens = [t for t in tokens if t.type.name == 'STRING']
        assert len(string_tokens) == 2, "Should have 2 separate string tokens"

    def test_multiline_string_continuation(self):
        """Test multiline docstring continuation across lines."""
        lexer = PythonLexer()
        state = lexer.lex(None, '"""start')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Should have one token"
        assert tokens[0].type.name == 'STRING'
        assert state.in_docstring, "Should be in docstring state"

        # Continue on next line
        lexer2 = PythonLexer()
        state2 = lexer2.lex(state, 'middle')

        tokens2 = list(lexer2._tokens)
        assert len(tokens2) == 1, "Should have one token on second line"
        assert tokens2[0].type.name == 'STRING'
        assert state2.in_docstring, "Should still be in docstring state"

        # Close on third line
        lexer3 = PythonLexer()
        state3 = lexer3.lex(state2, 'end"""')

        tokens3 = list(lexer3._tokens)
        assert len(tokens3) == 1, "Should have one token on third line"
        assert tokens3[0].type.name == 'STRING'
        assert not state3.in_docstring, "Should no longer be in docstring state"

    def test_string_with_numbers(self):
        """Test strings containing numbers."""
        test_cases = [
            "'123'",
            '"456.789"',
            "'0x1234'",
        ]
        for s in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"String '{s}' should produce one token"
            assert tokens[0].type.name == 'STRING', "Should be STRING, not NUMBER"

    def test_unclosed_string(self):
        """Test unclosed strings."""
        test_cases = [
            "'unclosed",
            '"unclosed',
        ]
        for s in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            # Unclosed strings should still produce a token
            assert len(tokens) >= 1, f"Unclosed string '{s}' should produce at least one token"

    def test_string_with_backslash_at_end(self):
        """Test strings with backslash at the end."""
        test_cases = [
            r"'ends with\\'",
            r'"ends with\\"',
        ]
        for s in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 1, f"String '{s}' should produce at least one token"
