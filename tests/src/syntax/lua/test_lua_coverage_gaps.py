"""
Tests for uncovered code paths in Lua lexer (based on coverage report).
"""
import pytest

from syntax.lua.lua_lexer import LuaLexer


class TestLuaCoverageGaps:
    """Test specific uncovered code paths in the Lua lexer."""

    def test_multiline_string_with_equals_continuation(self):
        """Test continuing a multiline string with equals signs across lines."""
        # Start a multiline string with equals: [=[
        lexer1 = LuaLexer()
        state1 = lexer1.lex(None, '[=[start string')

        assert state1.in_multiline_string, "Should be in multiline string"
        assert state1.string_bracket_level == 1, "Should have bracket level 1"

        # Continue on next line - this should hit _continue_multiline_string
        # with bracket_level > 0
        lexer2 = LuaLexer()
        state2 = lexer2.lex(state1, 'middle content')

        assert state2.in_multiline_string, "Should still be in multiline string"
        assert state2.string_bracket_level == 1, "Should maintain bracket level"

        # Close with matching bracket level: ]=]
        lexer3 = LuaLexer()
        state3 = lexer3.lex(state2, 'end]=]')

        assert not state3.in_multiline_string, "Should close multiline string"
        assert state3.string_bracket_level == 0, "Should reset bracket level"

    def test_multiline_string_with_double_equals_continuation(self):
        """Test continuing a multiline string with double equals."""
        lexer1 = LuaLexer()
        state1 = lexer1.lex(None, '[==[start')

        assert state1.in_multiline_string
        assert state1.string_bracket_level == 2

        lexer2 = LuaLexer()
        state2 = lexer2.lex(state1, 'middle')

        assert state2.in_multiline_string

        lexer3 = LuaLexer()
        state3 = lexer3.lex(state2, 'end]==]')

        assert not state3.in_multiline_string

    def test_close_bracket_while_in_multiline_string(self):
        """Test encountering ] while in a multiline string (should not close if levels don't match)."""
        # This tests the _read_close_bracket path when in_multiline_string is True
        # Start with [=[ (level 1)
        lexer1 = LuaLexer()
        state1 = lexer1.lex(None, '[=[text with ]')

        # The single ] shouldn't close it
        assert state1.in_multiline_string, "Single ] should not close [=["

        # Continue - encounter ]] which also shouldn't close [=[
        lexer2 = LuaLexer()
        state2 = lexer2.lex(state1, 'more ]] text')

        assert state2.in_multiline_string, "]] should not close [=["

        # Finally close with ]=]
        lexer3 = LuaLexer()
        state3 = lexer3.lex(state2, 'end ]=]')

        assert not state3.in_multiline_string, "]=] should close [=["

    def test_float_starting_with_dot_scientific_notation(self):
        """Test scientific notation for floats starting with dot: .5e10"""
        test_cases = [
            '.5e10',
            '.5e-3',
            '.5e+5',
            '.123e10',
            '.5E10',  # uppercase E
        ]

        for num in test_cases:
            lexer = LuaLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Scientific float '{num}' should be single token"
            assert tokens[0].type.name == 'NUMBER', f"'{num}' should be NUMBER"
            assert tokens[0].value == num, f"Value should match '{num}'"

    def test_bracket_string_not_followed_by_bracket(self):
        """Test [= not followed by [ (should be regular bracket operator)."""
        lexer = LuaLexer()
        lexer.lex(None, '[=')

        tokens = list(lexer._tokens)
        # Should be tokenized as [ operator followed by = operator
        assert len(tokens) == 2, "Should be two separate operators"
        assert tokens[0].type.name == 'OPERATOR'
        assert tokens[0].value == '['
        assert tokens[1].type.name == 'OPERATOR'
        assert tokens[1].value == '='

    def test_bracket_string_with_equals_not_closed(self):
        """Test [=[ string that's not closed on same line."""
        lexer = LuaLexer()
        state = lexer.lex(None, '[=[unclosed')

        assert state.in_multiline_string, "Should be in multiline string"
        assert state.string_bracket_level == 1, "Should track bracket level"

        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Should have one string token"
        assert tokens[0].type.name == 'STRING'

    def test_multiline_string_wrong_closing_bracket_level(self):
        """Test that wrong bracket level doesn't close multiline string."""
        lexer1 = LuaLexer()
        state1 = lexer1.lex(None, '[==[start')

        assert state1.string_bracket_level == 2

        # Try to close with wrong level: ]=] (level 1)
        lexer2 = LuaLexer()
        state2 = lexer2.lex(state1, 'text ]=] more')

        # Should still be open because levels don't match
        assert state2.in_multiline_string, "Wrong bracket level should not close"

        # Close with correct level: ]==]
        lexer3 = LuaLexer()
        state3 = lexer3.lex(state2, 'end ]==]')

        assert not state3.in_multiline_string, "Correct level should close"

    def test_close_bracket_in_normal_code(self):
        """Test ] operator in normal code (not in multiline string)."""
        lexer = LuaLexer()
        lexer.lex(None, 'arr[1]')

        tokens = list(lexer._tokens)
        close_bracket = [t for t in tokens if t.value == ']']
        assert len(close_bracket) == 1, "Should have closing bracket operator"
        assert close_bracket[0].type.name == 'OPERATOR'

    def test_multiline_string_with_triple_equals(self):
        """Test multiline string with three equals signs."""
        lexer1 = LuaLexer()
        state1 = lexer1.lex(None, '[===[content')

        assert state1.in_multiline_string
        assert state1.string_bracket_level == 3

        lexer2 = LuaLexer()
        state2 = lexer2.lex(state1, 'more]===]')

        assert not state2.in_multiline_string
        assert state2.string_bracket_level == 0
