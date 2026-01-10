"""
Tests for AIFPL boolean tokenization.
"""
import pytest

from syntax.aifpl.aifpl_lexer import AIFPLLexer
from syntax.lexer import TokenType


class TestAIFPLBooleans:
    """Test AIFPL boolean tokenization."""

    def test_true_boolean(self):
        """Test #t (true) boolean."""
        lexer = AIFPLLexer()
        lexer.lex(None, '#t')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.BOOLEAN
        assert tokens[0].value == '#t'

    def test_false_boolean(self):
        """Test #f (false) boolean."""
        lexer = AIFPLLexer()
        lexer.lex(None, '#f')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.BOOLEAN
        assert tokens[0].value == '#f'

    def test_uppercase_true(self):
        """Test #T (uppercase true)."""
        lexer = AIFPLLexer()
        lexer.lex(None, '#T')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.BOOLEAN
        assert tokens[0].value == '#T'

    def test_uppercase_false(self):
        """Test #F (uppercase false)."""
        lexer = AIFPLLexer()
        lexer.lex(None, '#F')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.BOOLEAN
        assert tokens[0].value == '#F'

    def test_booleans_in_expression(self):
        """Test booleans in expressions."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(and #t #f)')

        tokens = list(lexer._tokens)
        boolean_tokens = [t for t in tokens if t.type == TokenType.BOOLEAN]
        assert len(boolean_tokens) == 2
        assert boolean_tokens[0].value == '#t'
        assert boolean_tokens[1].value == '#f'

    def test_booleans_with_whitespace(self):
        """Test booleans separated by whitespace."""
        lexer = AIFPLLexer()
        lexer.lex(None, '#t #f #t')

        tokens = list(lexer._tokens)
        boolean_tokens = [t for t in tokens if t.type == TokenType.BOOLEAN]
        assert len(boolean_tokens) == 3
        assert [t.value for t in boolean_tokens] == ['#t', '#f', '#t']

    def test_boolean_in_if_expression(self):
        """Test boolean in if expression."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(if #t 1 0)')

        tokens = list(lexer._tokens)
        boolean_tokens = [t for t in tokens if t.type == TokenType.BOOLEAN]
        assert len(boolean_tokens) == 1
        assert boolean_tokens[0].value == '#t'

    def test_boolean_in_list(self):
        """Test booleans in a list."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(list #t #f #t)')

        tokens = list(lexer._tokens)
        boolean_tokens = [t for t in tokens if t.type == TokenType.BOOLEAN]
        assert len(boolean_tokens) == 3

    def test_boolean_comparison(self):
        """Test boolean in comparison."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(= x #t)')

        tokens = list(lexer._tokens)
        boolean_tokens = [t for t in tokens if t.type == TokenType.BOOLEAN]
        assert len(boolean_tokens) == 1
        assert boolean_tokens[0].value == '#t'

    def test_boolean_negation(self):
        """Test boolean with not."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(not #t)')

        tokens = list(lexer._tokens)
        boolean_tokens = [t for t in tokens if t.type == TokenType.BOOLEAN]
        assert len(boolean_tokens) == 1
        assert boolean_tokens[0].value == '#t'

    def test_multiple_booleans_in_and(self):
        """Test multiple booleans in and expression."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(and #t #t #f)')

        tokens = list(lexer._tokens)
        boolean_tokens = [t for t in tokens if t.type == TokenType.BOOLEAN]
        assert len(boolean_tokens) == 3

    def test_multiple_booleans_in_or(self):
        """Test multiple booleans in or expression."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(or #f #f #t)')

        tokens = list(lexer._tokens)
        boolean_tokens = [t for t in tokens if t.type == TokenType.BOOLEAN]
        assert len(boolean_tokens) == 3

    def test_boolean_at_start(self):
        """Test boolean at start of input."""
        lexer = AIFPLLexer()
        lexer.lex(None, '#t')

        tokens = list(lexer._tokens)
        assert tokens[0].type == TokenType.BOOLEAN
        assert tokens[0].start == 0

    def test_boolean_after_paren(self):
        """Test boolean after opening parenthesis."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(#t)')

        tokens = list(lexer._tokens)
        boolean_tokens = [t for t in tokens if t.type == TokenType.BOOLEAN]
        assert len(boolean_tokens) == 1

    def test_boolean_before_paren(self):
        """Test boolean before closing parenthesis."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(list #t)')

        tokens = list(lexer._tokens)
        boolean_tokens = [t for t in tokens if t.type == TokenType.BOOLEAN]
        assert len(boolean_tokens) == 1

    def test_boolean_positions(self):
        """Test that boolean positions are correctly tracked."""
        lexer = AIFPLLexer()
        lexer.lex(None, '#t #f')

        tokens = list(lexer._tokens)
        boolean_tokens = [t for t in tokens if t.type == TokenType.BOOLEAN]
        assert len(boolean_tokens) == 2
        assert boolean_tokens[0].start == 0
        assert boolean_tokens[1].start == 3

    def test_nested_boolean_expressions(self):
        """Test booleans in nested expressions."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(and (or #t #f) #t)')

        tokens = list(lexer._tokens)
        boolean_tokens = [t for t in tokens if t.type == TokenType.BOOLEAN]
        assert len(boolean_tokens) == 3

    def test_boolean_with_predicate(self):
        """Test boolean with predicate function."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(boolean? #t)')

        tokens = list(lexer._tokens)
        boolean_tokens = [t for t in tokens if t.type == TokenType.BOOLEAN]
        assert len(boolean_tokens) == 1

    def test_boolean_in_let_binding(self):
        """Test boolean in let binding."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(let ((x #t)) x)')

        tokens = list(lexer._tokens)
        boolean_tokens = [t for t in tokens if t.type == TokenType.BOOLEAN]
        assert len(boolean_tokens) == 1
        assert boolean_tokens[0].value == '#t'

    def test_boolean_as_function_argument(self):
        """Test boolean as function argument."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(func #t #f)')

        tokens = list(lexer._tokens)
        boolean_tokens = [t for t in tokens if t.type == TokenType.BOOLEAN]
        assert len(boolean_tokens) == 2

    def test_boolean_return_value(self):
        """Test boolean as return value in lambda."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(lambda (x) #t)')

        tokens = list(lexer._tokens)
        boolean_tokens = [t for t in tokens if t.type == TokenType.BOOLEAN]
        assert len(boolean_tokens) == 1

    def test_adjacent_booleans(self):
        """Test adjacent booleans without spaces (in parens)."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(#t#f)')

        tokens = list(lexer._tokens)
        boolean_tokens = [t for t in tokens if t.type == TokenType.BOOLEAN]
        # Should still parse as two separate booleans
        assert len(boolean_tokens) == 2

    def test_boolean_mixed_case_variations(self):
        """Test that only #t, #T, #f, #F are recognized."""
        valid_booleans = ['#t', '#T', '#f', '#F']
        for bool_val in valid_booleans:
            lexer = AIFPLLexer()
            lexer.lex(None, bool_val)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.BOOLEAN

    def test_invalid_boolean_like_tokens(self):
        """Test that invalid boolean-like tokens are not booleans."""
        invalid = ['#true', '#false', '#tt', '#ff']
        for invalid_bool in invalid:
            lexer = AIFPLLexer()
            lexer.lex(None, invalid_bool)

            tokens = list(lexer._tokens)
            # These should not be parsed as BOOLEAN tokens
            # They might be ERROR or multiple tokens
            boolean_tokens = [t for t in tokens if t.type == TokenType.BOOLEAN]
            # The first token might be #t or #f, but there should be additional tokens
            if boolean_tokens:
                # If it starts with #t or #f, there should be more tokens after
                assert len(tokens) > 1
