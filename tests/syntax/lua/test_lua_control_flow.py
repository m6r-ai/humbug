"""
Tests for Lua control flow structures.
"""
import pytest

from syntax.lua.lua_lexer import LuaLexer
from syntax.lua.lua_parser import LuaParser


class TestLuaControlFlow:
    """Test Lua control flow structures."""

    def test_if_statement(self):
        """Test if statement."""
        lexer = LuaLexer()
        lexer.lex(None, 'if x > 10 then print("x is large") end')

        parser = LuaParser()
        parser.parse(None, 'if x > 10 then print("x is large") end')

        tokens = list(parser._tokens)
        keywords = [t for t in tokens if t.type.name == 'KEYWORD']

        assert len(keywords) == 3, "Should have 3 keywords: if, then, end"
        assert keywords[0].value == 'if'
        assert keywords[1].value == 'then'
        assert keywords[2].value == 'end'

    def test_if_elseif_else(self):
        """Test if-elseif-else chain."""
        lexer = LuaLexer()
        lexer.lex(None, 'if x == 1 then print("one") elseif x == 2 then print("two") else print("other") end')

        parser = LuaParser()
        parser.parse(None, 'if x == 1 then print("one") elseif x == 2 then print("two") else print("other") end')

        tokens = list(parser._tokens)
        keywords = [t for t in tokens if t.type.name == 'KEYWORD']

        assert len(keywords) == 6, "Should have 6 keywords: if, then, elseif, then, else, end"
        assert keywords[0].value == 'if'
        assert keywords[1].value == 'then'
        assert keywords[2].value == 'elseif'
        assert keywords[3].value == 'then'
        assert keywords[4].value == 'else'
        assert keywords[5].value == 'end'

    def test_for_loop(self):
        """Test for loop."""
        lexer = LuaLexer()
        lexer.lex(None, 'for i = 1, 10 do print(i) end')

        parser = LuaParser()
        parser.parse(None, 'for i = 1, 10 do print(i) end')

        tokens = list(parser._tokens)
        keywords = [t for t in tokens if t.type.name == 'KEYWORD']

        assert len(keywords) == 3, "Should have 3 keywords: for, do, end"

    def test_while_loop(self):
        """Test while loop."""
        lexer = LuaLexer()
        lexer.lex(None, 'while x > 0 do x = x - 1 end')

        parser = LuaParser()
        parser.parse(None, 'while x > 0 do x = x - 1 end')

        tokens = list(parser._tokens)
        keywords = [t for t in tokens if t.type.name == 'KEYWORD']

        assert len(keywords) == 3, "Should have 3 keywords: while, do, end"

    def test_repeat_until(self):
        """Test repeat-until loop."""
        lexer = LuaLexer()
        lexer.lex(None, 'repeat print("hello") until condition() end')

        parser = LuaParser()
        parser.parse(None, 'repeat print("hello") until condition() end')

        tokens = list(parser._tokens)
        keywords = [t for t in tokens if t.type.name == 'KEYWORD']

        assert len(keywords) == 3, "Should have 3 keywords: repeat, until, end"

    def test_nested_control_structures(self):
        """Test nested control structures."""
        lexer = LuaLexer()
        lexer.lex(None, 'if x then if y then z = 1 else z = 2 end end')

        parser = LuaParser()
        parser.parse(None, 'if x then if y then z = 1 else z = 2 end end')

        tokens = list(parser._tokens)
        keywords = [t for t in tokens if t.type.name == 'KEYWORD']

        assert len(keywords) == 7, "Should have 7 keywords: if, then, if, then, else, end, end"
        assert keywords[0].value == 'if'
        assert keywords[1].value == 'then'
        assert keywords[2].value == 'if'
        assert keywords[3].value == 'then'
        assert keywords[4].value == 'else'
        assert keywords[5].value == 'end'
        assert keywords[6].value == 'end'

    def test_break_continue(self):
        """Test break and continue."""
        lexer = LuaLexer()
        lexer.lex(None, 'for i = 1, 10 do if i > 5 then break end end')

        parser = LuaParser()
        parser.parse(None, 'for i = 1, 10 do if i > 5 then break end end')

        tokens = list(parser._tokens)
        keywords = [t for t in tokens if t.type.name == 'KEYWORD']

        assert len(keywords) == 7, "Should have 7 keywords: for, do, if, then, break, end, end"
        assert keywords[0].value == 'for'
        assert keywords[1].value == 'do'
        assert keywords[2].value == 'if'
        assert keywords[3].value == 'then'
        assert keywords[4].value == 'break'
        assert keywords[5].value == 'end'
        assert keywords[6].value == 'end'

    def test_return_statement(self):
        """Test return statement."""
        lexer = LuaLexer()
        lexer.lex(None, 'function add(a, b) return a + b end')

        parser = LuaParser()
        parser.parse(None, 'function add(a, b) return a + b end')

        tokens = list(parser._tokens)
        keywords = [t for t in tokens if t.type.name == 'KEYWORD']

        assert any(t.value == 'return' for t in keywords), "Should have return keyword"

    def test_local_declaration(self):
        """Test local variable declaration."""
        lexer = LuaLexer()
        lexer.lex(None, 'local x = 42')

        parser = LuaParser()
        parser.parse(None, 'local x = 42')

        tokens = list(parser._tokens)
        keywords = [t for t in tokens if t.type.name == 'KEYWORD']

        assert len(keywords) == 1, "Should have local keyword"
        assert keywords[0].value == 'local'
