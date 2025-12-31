"""
Tests for Lua function and table tokenization.
"""
import pytest

from syntax.lua.lua_lexer import LuaLexer
from syntax.lua.lua_parser import LuaParser


class TestLuaFunctions:
    """Test Lua function tokenization."""

    def test_function_declaration(self):
        """Test function declarations."""
        lexer = LuaLexer()
        lexer.lex(None, 'function add(a, b) return a + b end')

        parser = LuaParser()
        parser.parse(None, 'function add(a, b) return a + b end')

        tokens = list(parser._tokens)
        keywords = [t for t in tokens if t.type.name == 'KEYWORD']
        identifiers = [t for t in tokens if t.type.name == 'IDENTIFIER']

        assert len(keywords) == 3, "Should have 3 keywords: function, return, end"
        assert len(identifiers) == 4, "Should have 4 identifiers: add, a, b, a, b"

    def test_function_with_parameters(self):
        """Test function with parameters."""
        lexer = LuaLexer()
        lexer.lex(None, 'function greet(name) print("Hello, " .. name) end')

        parser = LuaParser()
        parser.parse(None, 'function greet(name) print("Hello, " .. name) end')

        tokens = list(parser._tokens)
        assert any(t.value == 'name' for t in tokens if t.type.name == 'IDENTIFIER'), "Should have 'name' parameter"

    def test_function_call(self):
        """Test function call detection."""
        lexer = LuaLexer()
        lexer.lex(None, 'print("hello")')

        parser = LuaParser()
        parser.parse(None, 'print("hello")')

        tokens = list(parser._tokens)
        assert len(tokens) >= 2, "Should have at least 2 tokens"
        assert tokens[0].type.name == 'FUNCTION_OR_METHOD', "Function name should be marked as function"
        assert tokens[1].value == '(', "Should have opening paren"

    def test_function_call_with_varargs(self):
        """Test function call with varargs."""
        lexer = LuaLexer()
        lexer.lex(None, 'print("hello", "world")')

        parser = LuaParser()
        parser.parse(None, 'print("hello", "world")')

        tokens = list(parser._tokens)
        function_token = [t for t in tokens if t.type.name == 'FUNCTION_OR_METHOD']
        assert len(function_token) == 1, "Function name should be marked as function"
        # This test doesn't actually have varargs (...), just multiple arguments
        assert len(function_token) >= 1, "Should have function call"

    def test_method_call(self):
        """Test method call."""
        lexer = LuaLexer()
        lexer.lex(None, 'string:upper()')

        parser = LuaParser()
        parser.parse(None, 'string:upper()')

        tokens = list(parser._tokens)
        identifiers = [t for t in tokens if t.type.name == 'IDENTIFIER']
        assert len(identifiers) == 1, "Should have one identifier 'string'"
        assert tokens[1].value == ':', "Should have colon operator"
        function_tokens = [t for t in tokens if t.type.name == 'FUNCTION_OR_METHOD']
        assert len(function_tokens) == 1, "Should have one function 'upper'"

    def test_anonymous_function(self):
        """Test anonymous function."""
        lexer = LuaLexer()
        lexer.lex(None, 'x:method(function() end)')

        parser = LuaParser()
        parser.parse(None, 'x:method(function() end)')

        tokens = list(parser._tokens)
        function_keywords = [t for t in tokens if t.value == 'function']
        assert any(function_keywords), "Should have function keyword"


class TestLuaTables:
    """Test Lua table tokenization."""

    def test_array_style_table(self):
        """Test array-style table."""
        lexer = LuaLexer()
        lexer.lex(None, '{1, 2, 3}')

        parser = LuaParser()
        parser.parse(None, '{1, 2, 3}')

        tokens = list(parser._tokens)
        assert len(tokens) == 7, "Should have 7 tokens: {, 1, comma, 2, comma, 3, }"
        assert tokens[0].value == '{'
        assert tokens[1].value == '1'
        assert tokens[2].value == ','
        assert tokens[3].value == '2'
        assert tokens[4].value == ','
        assert tokens[5].value == '3'
        assert tokens[6].value == '}'

    def test_record_style_table(self):
        """Test record-style table."""
        lexer = LuaLexer()
        lexer.lex(None, '{name = "John", age = 30}')

        parser = LuaParser()
        parser.parse(None, '{name = "John", age = 30}')

        tokens = list(parser._tokens)
        assert len(tokens) >= 6, "Should have at least 6 tokens"

    def test_empty_table(self):
        """Test empty table."""
        lexer = LuaLexer()
        lexer.lex(None, '{}')

        parser = LuaParser()
        parser.parse(None, '{}')

        tokens = list(parser._tokens)
        assert len(tokens) == 2, "Should have 2 tokens: { and }"

    def test_nested_tables(self):
        """Test nested tables."""
        lexer = LuaLexer()
        lexer.lex(None, 'outer = {inner = {a = 1}}')

        parser = LuaParser()
        parser.parse(None, 'outer = {inner = {a = 1}}')

        tokens = list(parser._tokens)
        braces = [t for t in tokens if t.value in ('{', '}')]
        assert len(braces) == 4, "Should have 4 braces"

    def test_table_field_access(self):
        """Test table field access."""
        lexer = LuaLexer()
        lexer.lex(None, 'person.name')

        parser = LuaParser()
        parser.parse(None, 'person.name')

        tokens = list(parser._tokens)
        identifiers = [t for t in tokens if t.type.name in ('IDENTIFIER', 'ELEMENT')]

        assert len(identifiers) == 2, "Should have 2 identifiers"
        assert tokens[0].value == 'person'
        assert tokens[1].value == '.'
        assert tokens[2].type.name == 'ELEMENT', "Field 'name' should be ELEMENT"

    def test_table_indexing(self):
        """Test table indexing."""
        lexer = LuaLexer()
        lexer.lex(None, 'table["key"]')

        parser = LuaParser()
        parser.parse(None, 'table["key"]')

        tokens = list(parser._tokens)
        assert len(tokens) >= 4, "Should have at least 4 tokens"
        assert tokens[0].value == 'table'
        assert tokens[1].value == '['
        assert tokens[2].value == '"key"'
        assert tokens[3].value == ']'

    def test_nested_field_access(self):
        """Test nested table field access."""
        lexer = LuaLexer()
        lexer.lex(None, 'config.database.host')

        parser = LuaParser()
        parser.parse(None, 'config.database.host')

        tokens = list(parser._tokens)
        identifiers = [t for t in tokens if t.type.name == 'IDENTIFIER']
        elements = [t for t in tokens if t.type.name == 'ELEMENT']

        assert len(identifiers) == 1, "Should have 1 identifier 'config'"
        assert len(elements) == 2, "Should have 2 ELEMENT tokens 'database' and 'host'"

    def test_table_mixed_access(self):
        """Test mixed table access patterns."""
        lexer = LuaLexer()
        lexer.lex(None, 't[1] + t.nested["key"]')

        parser = LuaParser()
        parser.parse(None, 't[1] + t.nested["key"]')

        tokens = list(parser._tokens)
        assert len(tokens) >= 10, "Should have many tokens"
