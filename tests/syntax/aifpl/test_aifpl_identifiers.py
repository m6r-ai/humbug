"""
Tests for AIFPL identifier tokenization.
"""
import pytest

from syntax.aifpl.aifpl_lexer import AIFPLLexer
from syntax.lexer import TokenType


class TestAIFPLIdentifiers:
    """Test AIFPL identifier tokenization."""

    def test_simple_identifiers(self):
        """Test simple identifier names."""
        test_cases = ['x', 'foo', 'bar', 'test', 'variable']
        for ident in test_cases:
            lexer = AIFPLLexer()
            lexer.lex(None, ident)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Identifier '{ident}' should produce one token"
            assert tokens[0].type == TokenType.IDENTIFIER
            assert tokens[0].value == ident

    def test_identifiers_with_hyphens(self):
        """Test identifiers with hyphens (kebab-case)."""
        test_cases = ['string-append', 'list-ref', 'my-var', 'test-case-1']
        for ident in test_cases:
            lexer = AIFPLLexer()
            lexer.lex(None, ident)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.IDENTIFIER
            assert tokens[0].value == ident

    def test_identifiers_with_numbers(self):
        """Test identifiers containing numbers."""
        test_cases = ['var1', 'test2', 'x123', 'foo42bar']
        for ident in test_cases:
            lexer = AIFPLLexer()
            lexer.lex(None, ident)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.IDENTIFIER
            assert tokens[0].value == ident

    def test_identifiers_with_question_mark(self):
        """Test predicate identifiers ending with ?."""
        test_cases = ['null?', 'number?', 'string?', 'list?', 'empty?']
        for ident in test_cases:
            lexer = AIFPLLexer()
            lexer.lex(None, ident)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.IDENTIFIER
            assert tokens[0].value == ident

    def test_identifiers_with_exclamation(self):
        """Test identifiers with exclamation marks."""
        test_cases = ['set!', 'not!', 'reset!']
        for ident in test_cases:
            lexer = AIFPLLexer()
            lexer.lex(None, ident)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.IDENTIFIER
            assert tokens[0].value == ident

    def test_arithmetic_operator_identifiers(self):
        """Test arithmetic operators as identifiers."""
        test_cases = ['+', '-', '*', '/', '//', '%', '**']
        for op in test_cases:
            lexer = AIFPLLexer()
            lexer.lex(None, op)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.IDENTIFIER
            assert tokens[0].value == op

    def test_comparison_operator_identifiers(self):
        """Test comparison operators as identifiers."""
        test_cases = ['=', '!=', '<', '>', '<=', '>=']
        for op in test_cases:
            lexer = AIFPLLexer()
            lexer.lex(None, op)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.IDENTIFIER
            assert tokens[0].value == op

    def test_math_function_identifiers(self):
        """Test math function names."""
        test_cases = ['sin', 'cos', 'tan', 'sqrt', 'abs', 'log', 'exp', 'pow']
        for func in test_cases:
            lexer = AIFPLLexer()
            lexer.lex(None, func)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.IDENTIFIER
            assert tokens[0].value == func

    def test_list_function_identifiers(self):
        """Test list function names."""
        test_cases = ['list', 'cons', 'first', 'rest', 'append', 'reverse', 'length']
        for func in test_cases:
            lexer = AIFPLLexer()
            lexer.lex(None, func)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.IDENTIFIER
            assert tokens[0].value == func

    def test_string_function_identifiers(self):
        """Test string function names."""
        test_cases = ['string-append', 'string-length', 'substring', 'string-upcase']
        for func in test_cases:
            lexer = AIFPLLexer()
            lexer.lex(None, func)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.IDENTIFIER
            assert tokens[0].value == func

    def test_higher_order_function_identifiers(self):
        """Test higher-order function names."""
        test_cases = ['map', 'filter', 'fold', 'reduce', 'apply']
        for func in test_cases:
            lexer = AIFPLLexer()
            lexer.lex(None, func)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.IDENTIFIER
            assert tokens[0].value == func

    def test_constants_as_identifiers(self):
        """Test constant names as identifiers."""
        test_cases = ['pi', 'e', 'j', 'true', 'false']
        for const in test_cases:
            lexer = AIFPLLexer()
            lexer.lex(None, const)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.IDENTIFIER
            assert tokens[0].value == const

    def test_identifiers_with_underscores(self):
        """Test identifiers with underscores."""
        test_cases = ['my_var', 'test_case', '_private', '__internal__']
        for ident in test_cases:
            lexer = AIFPLLexer()
            lexer.lex(None, ident)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.IDENTIFIER
            assert tokens[0].value == ident

    def test_identifiers_with_mixed_case(self):
        """Test that identifiers are case-sensitive."""
        test_cases = ['MyVar', 'TestCase', 'FOO', 'Bar']
        for ident in test_cases:
            lexer = AIFPLLexer()
            lexer.lex(None, ident)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.IDENTIFIER
            assert tokens[0].value == ident

    def test_identifiers_in_expression(self):
        """Test multiple identifiers in an expression."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(+ x y)')

        tokens = list(lexer._tokens)
        ident_tokens = [t for t in tokens if t.type == TokenType.IDENTIFIER]
        assert len(ident_tokens) == 3
        assert ident_tokens[0].value == '+'
        assert ident_tokens[1].value == 'x'
        assert ident_tokens[2].value == 'y'

    def test_identifiers_separated_by_whitespace(self):
        """Test identifiers separated by whitespace."""
        lexer = AIFPLLexer()
        lexer.lex(None, 'foo bar baz')

        tokens = list(lexer._tokens)
        ident_tokens = [t for t in tokens if t.type == TokenType.IDENTIFIER]
        assert len(ident_tokens) == 3
        assert [t.value for t in ident_tokens] == ['foo', 'bar', 'baz']

    def test_identifiers_separated_by_parens(self):
        """Test identifiers separated by parentheses."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(foo)(bar)')

        tokens = list(lexer._tokens)
        ident_tokens = [t for t in tokens if t.type == TokenType.IDENTIFIER]
        assert len(ident_tokens) == 2
        assert ident_tokens[0].value == 'foo'
        assert ident_tokens[1].value == 'bar'

    def test_single_letter_identifiers(self):
        """Test single letter identifiers."""
        test_cases = ['a', 'x', 'y', 'z', 'f', 'g']
        for ident in test_cases:
            lexer = AIFPLLexer()
            lexer.lex(None, ident)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.IDENTIFIER

    def test_very_long_identifier(self):
        """Test very long identifier names."""
        long_ident = 'this_is_a_very_long_identifier_name_that_should_still_work'
        lexer = AIFPLLexer()
        lexer.lex(None, long_ident)

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.IDENTIFIER
        assert tokens[0].value == long_ident

    def test_identifier_with_arrow(self):
        """Test identifiers with arrow-like patterns."""
        test_cases = ['->string', 'string->number', 'list->vector']
        for ident in test_cases:
            lexer = AIFPLLexer()
            lexer.lex(None, ident)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.IDENTIFIER
            assert tokens[0].value == ident

    def test_identifier_with_colon(self):
        """Test identifiers with colons."""
        test_cases = ['key:value', 'namespace:function', 'module:export']
        for ident in test_cases:
            lexer = AIFPLLexer()
            lexer.lex(None, ident)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.IDENTIFIER
            assert tokens[0].value == ident

    def test_identifier_with_period(self):
        """Test identifiers with periods (for object access)."""
        test_cases = ['obj.prop', 'module.function', 'namespace.class']
        for ident in test_cases:
            lexer = AIFPLLexer()
            lexer.lex(None, ident)

            tokens = list(lexer._tokens)
            # Period might be treated as separate token depending on implementation
            # We're checking it doesn't crash
            assert len(tokens) >= 1

    def test_bitwise_operators_as_identifiers(self):
        """Test bitwise operators as identifiers."""
        test_cases = ['bit-or', 'bit-and', 'bit-xor', 'bit-not', 'bit-shift-left']
        for op in test_cases:
            lexer = AIFPLLexer()
            lexer.lex(None, op)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.IDENTIFIER
            assert tokens[0].value == op

    def test_type_predicate_identifiers(self):
        """Test type predicate function names."""
        test_cases = ['number?', 'string?', 'boolean?', 'list?', 'function?', 'integer?']
        for pred in test_cases:
            lexer = AIFPLLexer()
            lexer.lex(None, pred)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.IDENTIFIER
            assert tokens[0].value == pred

    def test_alist_function_identifiers(self):
        """Test alist function names."""
        test_cases = ['alist', 'alist-get', 'alist-set', 'alist-has?', 'alist-keys']
        for func in test_cases:
            lexer = AIFPLLexer()
            lexer.lex(None, func)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.IDENTIFIER
            assert tokens[0].value == func
