"""
Tests for multi-line Lua constructs (strings and comments).
"""
import pytest

from syntax.lua.lua_lexer import LuaLexer
from syntax.lua.lua_parser import LuaParser


class TestLuaMultiline:
    """Test multi-line Lua constructs."""

    def test_multiline_string_across_lines(self):
        """Test multi-line string across multiple lines."""
        lexer = LuaLexer()

        # Line 1: Start of multi-line string
        state1 = lexer.lex(None, 'local s = [["Hello')
        assert state1.in_multiline_string == True
        assert state1.string_bracket_level == 0

        # Line 2: Content
        state2 = lexer.lex(state1, 'World]]')
        assert state2.in_multiline_string == False, "String should be closed"

    def test_unclosed_multiline_string(self):
        """Test unclosed multi-line string."""
        lexer = LuaLexer()

        # Line 1: Start of unclosed string
        state1 = lexer.lex(None, 'local s = [["Unclosed')
        assert state1.in_multiline_string == True

        # Line 2: Still in string
        state2 = lexer.lex(state1, 'more content]')
        assert state2.in_multiline_string == True, "String should still be open"

    def test_multiline_string_with_equals(self):
        """Test multi-line strings with equals signs close properly."""
        test_cases = [
            '[=[equals=]=]',
            '[==[double==]==]',
            '[===[triple===]===]',
        ]

        for code in test_cases:
            lexer = LuaLexer()
            state = lexer.lex(None, code)

            assert state.in_multiline_string == False, f"String should be closed for {code}"
            assert state.string_bracket_level == 0, f"Bracket level should be 0 when closed"

    def test_block_comment_across_lines(self):
        """Test multi-line block comment across multiple lines."""
        lexer = LuaLexer()

        # Line 1: Start of block comment
        state1 = lexer.lex(None, '--[[Start of comment')
        assert state1.in_block_comment == True

        # Line 2: Content
        state2 = lexer.lex(state1, 'Still in comment')
        assert state2.in_block_comment == True, "Should still be in comment"

        # Line 3: End of comment
        state3 = lexer.lex(state2, 'End of comment]]')
        assert state3.in_block_comment == False, "Comment should be closed"

    def test_unclosed_block_comment(self):
        """Test unclosed block comment."""
        lexer = LuaLexer()

        # Line 1: Start of unclosed comment
        state1 = lexer.lex(None, '--[[Never closed')
        assert state1.in_block_comment == True

        # Line 2: Still in comment
        state2 = lexer.lex(state1, 'more content')
        assert state2.in_block_comment == True, "Comment should still be open"

    def test_nested_bracket_strings(self):
        """Test bracket string with identifier after it."""
        lexer = LuaLexer()
        lexer.lex(None, 'outer = [=[inner]=] more')

        tokens = list(lexer._tokens)
        strings = [t for t in tokens if t.type.name == 'STRING']

        assert len(strings) == 1, "Should have 1 string token"

    def test_multiline_with_nested_brackets(self):
        """Test that multiline strings close at first ]]."""
        lexer = LuaLexer()
        state1 = lexer.lex(None, 'local s = [[hello]]')
        assert state1.in_multiline_string == False, "String should be closed"

        # Verify the string token was created
        tokens = list(lexer._tokens)
        strings = [t for t in tokens if t.type.name == 'STRING']
        assert len(strings) == 1, "Should have 1 string token"

    def test_string_in_table(self):
        """Test bracket string in table constructor."""
        lexer = LuaLexer()
        lexer.lex(None, '{[=[multistring]=] = 42}')

        tokens = list(lexer._tokens)
        assert len(tokens) == 5, "Should have 5 tokens"

        assert tokens[0].value == '{'
        assert tokens[1].type.name == 'STRING'
        assert tokens[2].value == '='
        assert tokens[3].type.name == 'NUMBER'
        assert tokens[4].value == '}'

    def test_mixed_multiline_constructs(self):
        """Test that block comment at end of line stays open."""
        lexer = LuaLexer()

        # Start block comment at end of line
        state1 = lexer.lex(None, 'local x = 42 --[[comment')
        assert state1.in_block_comment == True

        # Continue comment
        state2 = lexer.lex(state1, 'more comment')
        assert state2.in_block_comment == True, "Comment should still be open"

        # Close comment
        state3 = lexer.lex(state2, 'end]]')
        assert state3.in_block_comment == False, "Comment should be closed"
