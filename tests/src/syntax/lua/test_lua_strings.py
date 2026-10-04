"""
Tests for Lua string tokenization.
"""
import pytest

from syntax.lua.lua_lexer import LuaLexer


class TestLuaStrings:
    """Test Lua string tokenization."""

    def test_single_quoted_strings(self):
        """Test single-quoted strings."""
        test_cases = [
            "'hello'",
            "'world'",
            "'escapes\\n\\t'",
            "'quoted \"quote\"'"
        ]
        for s in test_cases:
            lexer = LuaLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"String '{s}' should produce one token"
            assert tokens[0].type.name == 'STRING', f"String '{s}' should be STRING type"

    def test_double_quoted_strings(self):
        """Test double-quoted strings."""
        test_cases = [
            '"hello"',
            '"world"',
            '"escapes\n\t"',
            '"quoted \'quote\'"'
        ]
        for s in test_cases:
            lexer = LuaLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"String '{s}' should produce one token"
            assert tokens[0].type.name == 'STRING', f"String '{s}' should be STRING type"

    def test_multiline_bracket_strings(self):
        """Test multi-line bracket strings."""
        test_cases = [
            '[[hello]]',
            '[[world]]',
        ]
        for s in test_cases:
            lexer = LuaLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Multi-line string '{s}' should produce one token"
            assert tokens[0].type.name == 'STRING', f"Multi-line string '{s}' should be STRING type"

    def test_multiline_with_equals(self):
        """Test multi-line strings with equals signs."""
        test_cases = [
            '[=[equals=]=]',
            '[==[double==]==]',
            '[===[triple===]===]',
        ]
        for s in test_cases:
            lexer = LuaLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Bracket string '{s}' should produce one token"
            assert tokens[0].type.name == 'STRING', f"Bracket string '{s}' should be STRING type"

    def test_string_escapes(self):
        """Test escape sequences in strings."""
        test_cases = [
            r"'\n'",  # Newline escape
            r"'\t'",  # Tab escape
            r"'\''",  # Single quote escape
            r'"\""',  # Double quote escape
            r"'\u{41}'",  # Unicode escape (Lua 5.4+)
        ]
        for s in test_cases:
            lexer = LuaLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"String with escape '{s}' should produce one token"
            assert tokens[0].type.name == 'STRING', f"String with escape should be STRING type"

    def test_empty_strings(self):
        """Test empty strings."""
        test_cases = ["''", '""']
        for s in test_cases:
            lexer = LuaLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Empty string '{s}' should produce one token"
            assert tokens[0].type.name == 'STRING', f"Empty string should be STRING type"

    def test_string_concatenation(self):
        """Test string concatenation operator."""
        lexer = LuaLexer()
        lexer.lex(None, '"hello" .. "world"')

        tokens = list(lexer._tokens)
        assert len(tokens) == 3, "Concatenation should produce 3 tokens (2 strings + 1 operator)"
        assert tokens[0].type.name == 'STRING'
        assert tokens[1].type.name == 'OPERATOR'
        assert tokens[1].value == '..'
        assert tokens[2].type.name == 'STRING'

    def test_strings_with_quotes_inside(self):
        """Test strings containing quote characters."""
        test_cases = [
            '"She said \\"hello\\""',
            "'It\\'s \\'amazing\\''"
        ]
        for s in test_cases:
            lexer = LuaLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"String with internal quotes '{s}' should produce one token"
            assert tokens[0].type.name == 'STRING'
