"""
Tests for Lua keyword and boolean literal tokenization.
"""
import pytest

from syntax.lua.lua_lexer import LuaLexer


class TestLuaKeywords:
    """Test Lua keyword tokenization."""

    def test_all_control_keywords(self):
        """Test all control flow keywords."""
        keywords = ['if', 'then', 'else', 'elseif', 'while', 'do', 'for', 'repeat', 'until']
        for keyword in keywords:
            lexer = LuaLexer()
            lexer.lex(None, keyword)

            tokens = list(lexer._tokens)
            token_types = [t.type.name for t in tokens]

            assert 'KEYWORD' in token_types, f"Keyword '{keyword}' not recognized"

    def test_other_keywords(self):
        """Test other Lua keywords."""
        keywords = ['and', 'break', 'function', 'in', 'local', 'not', 'or', 'return', 'end']
        for keyword in keywords:
            lexer = LuaLexer()
            lexer.lex(None, keyword)

            tokens = list(lexer._tokens)
            assert any('KEYWORD' in t.type.name for t in tokens), f"Keyword '{keyword}' not recognized"

    def test_boolean_literals(self):
        """Test boolean literal tokenization."""
        booleans = ['true', 'false', 'nil']
        for boolean in booleans:
            lexer = LuaLexer()
            lexer.lex(None, boolean)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Boolean '{boolean}' should produce one token"
            assert tokens[0].type.name == 'BOOLEAN', f"Boolean '{boolean}' should be BOOLEAN type"

    def test_keywords_case_sensitivity(self):
        """Test that keywords are case-sensitive."""
        lexer = LuaLexer()
        lexer.lex(None, 'True')

        tokens = list(lexer._tokens)
        assert all(t.type.name == 'IDENTIFIER' for t in tokens), "Keywords should be case-sensitive"

    def test_keywords_with_whitespace(self):
        """Test keywords surrounded by whitespace."""
        lexer = LuaLexer()
        lexer.lex(None, 'if x then else end')

        tokens = list(lexer._tokens)
        keywords = [t for t in tokens if t.type.name == 'KEYWORD']
        assert len(keywords) == 4, "Should find 4 keywords"

    def test_keyword_as_identifier_prefix(self):
        """Test identifiers that start with keywords."""
        lexer = LuaLexer()
        lexer.lex(None, 'localVar')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Should be a single identifier token"
        assert tokens[0].type.name == 'IDENTIFIER', "Should be an identifier, not split"
        assert tokens[0].value == 'localVar', "Value should be 'localVar'"
