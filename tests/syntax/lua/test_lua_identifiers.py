"""
Tests for Lua identifier tokenization and edge cases.
"""
import pytest

from syntax.lua.lua_lexer import LuaLexer


class TestLuaIdentifiers:
    """Test Lua identifier tokenization."""

    def test_simple_identifiers(self):
        """Test simple identifiers."""
        test_cases = [
            'myVar',
            '_private',
            'variableName',
            'CONSTANT',
        ]
        for ident in test_cases:
            lexer = LuaLexer()
            lexer.lex(None, ident)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Identifier '{ident}' should produce one token"
            assert tokens[0].type.name == 'IDENTIFIER', f"Identifier should be IDENTIFIER type"
            assert tokens[0].value == ident, f"Identifier value should match"

    def test_identifiers_with_numbers(self):
        """Test identifiers with numbers."""
        test_cases = [
            'var1',
            'x123',
            'player2',
            'name123abc',
        ]
        for ident in test_cases:
            lexer = LuaLexer()
            lexer.lex(None, ident)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Identifier '{ident}' should produce one token"
            assert tokens[0].type.name == 'IDENTIFIER'

    def test_identifiers_with_underscores(self):
        """Test identifiers with underscores."""
        test_cases = [
            '_private',
            '__init',
            '___triple',
            'my_var',
        ]
        for ident in test_cases:
            lexer = LuaLexer()
            lexer.lex(None, ident)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Identifier '{ident}' should produce one token"
            assert tokens[0].type.name == 'IDENTIFIER'

    def test_reserved_words_as_identifiers(self):
        """Test that reserved words can be part of identifiers in Lua."""
        # In Lua, keywords can be used as table fields: table.local, table.function
        test_cases = [
            'table.local',
            'table.function',
            'table.end',
        ]
        for ident in test_cases:
            lexer = LuaLexer()
            lexer.lex(None, ident)

            tokens = list(lexer._tokens)
            # 'table' should be IDENTIFIER, '.' should be OPERATOR, 'local'/'function'/'end' should be KEYWORD
            identifiers = [t for t in tokens if t.type.name == 'IDENTIFIER']
            operators = [t for t in tokens if t.type.name == 'OPERATOR']
            keywords = [t for t in tokens if t.type.name == 'KEYWORD']
            assert len(identifiers) == 1, f"Should have 1 identifier (table)"
            assert len(operators) == 1, f"Should have 1 operator (dot)"
            assert len(keywords) == 1, f"Should have 1 keyword (local/function/end)"

    def test_identifiers_case_sensitivity(self):
        """Test that identifiers are case-sensitive."""
        test_cases = [
            'MyVar',
            'myvar',
            'MYVAR',
        ]
        for ident in test_cases:
            lexer = LuaLexer()
            lexer.lex(None, ident)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Identifier '{ident}' should produce one token"

    def test_keyword_as_identifier_prefix(self):
        """Test identifiers that start with keywords."""
        test_cases = [
            'functionName',
            'endOfFile',
            'localVariable',
        ]
        for ident in test_cases:
            lexer = LuaLexer()
            lexer.lex(None, ident)

            tokens = list(lexer._tokens)
            # Should be a single identifier (keywords don't split mid-identifier)
            assert len(tokens) == 1, f"Identifier '{ident}' should be 1 token"
            assert tokens[0].type.name == 'IDENTIFIER', f"Should be identifier, not keyword"
            assert tokens[0].value == ident, f"Value should be '{ident}'"


class TestLuaEdgeCases:
    """Test Lua edge cases and unusual scenarios."""

    def test_empty_line(self):
        """Test empty line."""
        lexer = LuaLexer()
        lexer.lex(None, '')

        tokens = list(lexer._tokens)
        assert len(tokens) == 0, f"Empty line should produce no tokens"

    def test_whitespace_variations(self):
        """Test various whitespace."""
        test_cases = [
            '    x = 1',
            'x  =  2',
            '\tx\t= 3',
        ]
        for line in test_cases:
            lexer = LuaLexer()
            lexer.lex(None, line)

            tokens = list(lexer._tokens)
            # Should have identifiers and operators
            assert any(t.type.name == 'IDENTIFIER' for t in tokens), f"Should have identifier in '{line}'"

    def test_code_after_comment(self):
        """Test that comments consume rest of line."""
        # Lexer is called line-by-line, so this tests a single line with comment
        lexer = LuaLexer()
        lexer.lex(None, '-- comment')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Should produce one comment token"
        assert tokens[0].type.name == 'COMMENT', "Should be a comment"
        assert tokens[0].value == '-- comment', "Comment should include --"

    def test_comment_at_end(self):
        """Test comment at end of line."""
        lexer = LuaLexer()
        lexer.lex(None, 'x = 1 -- end comment')

        tokens = list(lexer._tokens)
        assert len(tokens) == 4, "Should have identifier, operator, number, and comment"
        assert tokens[0].type.name == 'IDENTIFIER'
        assert tokens[1].type.name == 'OPERATOR'
        assert tokens[2].type.name == 'NUMBER'
        assert tokens[3].type.name == 'COMMENT'

    def test_bracket_ambiguity(self):
        """Test bracket ambiguity (string vs table)."""
        # '[' could be start of multi-line string or table index
        lexer = LuaLexer()
        lexer.lex(None, 'x = [1, 2, 3]')

        tokens = list(lexer._tokens)
        # Without following '[', should be treated as operator
        assert tokens[0].value == 'x'
        assert tokens[1].value == '='
        assert tokens[2].value == '['
        assert tokens[2].type.name == 'OPERATOR'

    def test_dot_ambiguity(self):
        """Test dot ambiguity (field access vs decimal)."""
        lexer = LuaLexer()
        lexer.lex(None, 'x.y')

        tokens = list(lexer._tokens)
        # Should tokenize as identifier . identifier
        assert len(tokens) == 3, "Should have 3 tokens"
        assert tokens[0].type.name == 'IDENTIFIER'
        assert tokens[1].value == '.'
        assert tokens[2].type.name == 'IDENTIFIER'

    def test_string_after_minus(self):
        """Test that minus doesn't consume string."""
        lexer = LuaLexer()
        lexer.lex(None, 'x - "hello"')

        tokens = list(lexer._tokens)
        token_types = [t.type.name for t in tokens]

        assert 'STRING' in token_types, "Should have string token"
        assert token_types.count('OPERATOR') == 1, "Should have minus operator"
        assert token_types.count('IDENTIFIER') == 1, "Should have identifier"

    def test_multiple_statements(self):
        """Test multiple statements on one line."""
        lexer = LuaLexer()
        lexer.lex(None, 'local x = 1 local y = 2')

        tokens = list(lexer._tokens)
        keywords = [t for t in tokens if t.type.name == 'KEYWORD']
        identifiers = [t for t in tokens if t.type.name == 'IDENTIFIER']

        assert len(keywords) == 2, "Should have 2 local keywords"
        assert len(identifiers) == 2, "Should have 2 identifiers"

    def test_unusual_operators(self):
        """Test unusual operator patterns."""
        lexer = LuaLexer()
        lexer.lex(None, 'x ~= y ~= z')

        tokens = list(lexer._tokens)
        operators = [t for t in tokens if t.type.name == 'OPERATOR']
        identifiers = [t for t in tokens if t.type.name == 'IDENTIFIER']

        assert len(operators) == 2, "Should have 2 ~= operators"
        assert len(identifiers) == 3, "Should have 3 identifiers (x, y, z)"
