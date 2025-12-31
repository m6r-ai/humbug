"""
Tests for Lua operator tokenization.
"""
import pytest

from syntax.lua.lua_lexer import LuaLexer


class TestLuaOperators:
    """Test Lua operator tokenization."""

    def test_arithmetic_operators(self):
        """Test arithmetic operators."""
        operators = ['+', '-', '*', '/', '%', '^', '#']
        for op in operators:
            lexer = LuaLexer()
            lexer.lex(None, op)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Operator '{op}' should produce one token"
            assert tokens[0].type.name == 'OPERATOR', f"Operator '{op}' should be OPERATOR type"

    def test_comparison_operators(self):
        """Test comparison operators."""
        operators = ['==', '~=', '<=', '>=', '<', '>']
        for op in operators:
            lexer = LuaLexer()
            lexer.lex(None, op)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Comparison operator '{op}' should produce one token"
            assert tokens[0].type.name == 'OPERATOR', f"Operator '{op}' should be OPERATOR type"

    def test_logical_operators(self):
        """Test logical operators (as keywords)."""
        lexer = LuaLexer()
        lexer.lex(None, 'and or not')

        tokens = list(lexer._tokens)
        token_types = [t.type.name for t in tokens]

        assert 'KEYWORD' in token_types, "Should have KEYWORD tokens"
        assert token_types.count('KEYWORD') == 3, "Should have 3 keywords"

    def test_concatenation_operator(self):
        """Test concatenation operator."""
        lexer = LuaLexer()
        lexer.lex(None, '..')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1, f"Concatenation '..' should produce one token"
        assert tokens[0].type.name == 'OPERATOR'
        assert tokens[0].value == '..'

    def test_varargs_operator(self):
        """Test varargs operator."""
        lexer = LuaLexer()
        lexer.lex(None, '...')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1, f"Varargs '...' should produce one token"
        assert tokens[0].type.name == 'OPERATOR'
        assert tokens[0].value == '...'

    def test_method_call_operator(self):
        """Test method call operator."""
        lexer = LuaLexer()
        lexer.lex(None, ':')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1, f"Method operator ':' should produce one token"
        assert tokens[0].type.name == 'OPERATOR'
        assert tokens[0].value == ':'

    def test_field_access_operator(self):
        """Test field access operator."""
        lexer = LuaLexer()
        lexer.lex(None, '.')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1, f"Field access '.' should produce one token"
        assert tokens[0].type.name == 'OPERATOR'
        assert tokens[0].value == '.'

    def test_parentheses(self):
        """Test parentheses."""
        for op in ['(', ')']:
            lexer = LuaLexer()
            lexer.lex(None, op)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Operator '{op}' should produce one token"
            assert tokens[0].type.name == 'OPERATOR'
            assert tokens[0].value == op

    def test_braces(self):
        """Test curly braces."""
        for op in ['{', '}']:
            lexer = LuaLexer()
            lexer.lex(None, op)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Operator '{op}' should produce one token"
            assert tokens[0].type.name == 'OPERATOR'
            assert tokens[0].value == op

    def test_brackets(self):
        """Test square brackets."""
        for op in ['[', ']']:
            lexer = LuaLexer()
            lexer.lex(None, op)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Operator '{op}' should produce one token"
            assert tokens[0].type.name == 'OPERATOR'
            assert tokens[0].value == op

    def test_greedy_matching(self):
        """Test greedy operator matching."""
        test_cases = [
            ('...', '..'),  # ... should match before ..
            ('...', '...'),  # Multiple ...
            ('==', '='),  # == should match before =
            ('<=', '<'),  # <= should match before <
            ('>=', '>'),  # >= should match before >
        ]
        for longer, shorter in test_cases:
            lexer = LuaLexer()
            lexer.lex(None, longer + shorter)

            tokens = list(lexer._tokens)
            # First token should be the longer operator
            assert len(tokens) >= 2, f"Should produce at least 2 tokens"
            assert tokens[0].value == longer, f"Longer operator should match first"

    def test_operators_with_identifiers(self):
        """Test operators adjacent to identifiers."""
        test_cases = [
            'x + y',
            'a == b',
            'obj.method()',
            'table.field',
        ]
        for case in test_cases:
            lexer = LuaLexer()
            lexer.lex(None, case)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 3, f"Expression '{case}' should produce at least 3 tokens"
            # Check specific token types
            token_types = [t.type.name for t in tokens]
            assert 'IDENTIFIER' in token_types
            assert 'OPERATOR' in token_types

    def test_assignment_operator(self):
        """Test assignment operator."""
        lexer = LuaLexer()
        lexer.lex(None, 'x = 42')

        tokens = list(lexer._tokens)
        token_types = [t.type.name for t in tokens]

        assert len(tokens) == 3, "Assignment should produce 3 tokens (identifier, operator, number)"
        assert token_types == ['IDENTIFIER', 'OPERATOR', 'NUMBER']

    def test_comma_separator(self):
        """Test comma as separator."""
        lexer = LuaLexer()
        lexer.lex(None, 'a, b, c')

        tokens = list(lexer._tokens)
        token_types = [t.type.name for t in tokens]

        assert len(tokens) == 5, "Comma-separated list should produce 5 tokens"
        assert token_types.count('IDENTIFIER') == 3
        assert token_types.count('OPERATOR') == 2  # The commas

    def test_semicolon_statement_terminator(self):
        """Test semicolon as statement terminator."""
        lexer = LuaLexer()
        lexer.lex(None, 'x = 1; y = 2;')

        tokens = list(lexer._tokens)
        token_types = [t.type.name for t in tokens]

        assert len(tokens) >= 6, "Semicolons should be tokenized"
        assert token_types.count('OPERATOR') >= 2  # The semicolons
        assert ';' in [t.value for t in tokens]
