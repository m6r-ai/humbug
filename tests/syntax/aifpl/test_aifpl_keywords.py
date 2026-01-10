"""
Tests for AIFPL keyword tokenization.
"""
import pytest

from syntax.aifpl.aifpl_lexer import AIFPLLexer
from syntax.lexer import TokenType


class TestAIFPLKeywords:
    """Test AIFPL keyword (special form) tokenization."""

    def test_lambda_keyword(self):
        """Test lambda keyword."""
        lexer = AIFPLLexer()
        lexer.lex(None, 'lambda')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.KEYWORD
        assert tokens[0].value == 'lambda'

    def test_if_keyword(self):
        """Test if keyword."""
        lexer = AIFPLLexer()
        lexer.lex(None, 'if')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.KEYWORD
        assert tokens[0].value == 'if'

    def test_let_keyword(self):
        """Test let keyword."""
        lexer = AIFPLLexer()
        lexer.lex(None, 'let')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.KEYWORD
        assert tokens[0].value == 'let'

    def test_quote_keyword(self):
        """Test quote keyword."""
        lexer = AIFPLLexer()
        lexer.lex(None, 'quote')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.KEYWORD
        assert tokens[0].value == 'quote'

    def test_and_keyword(self):
        """Test and keyword."""
        lexer = AIFPLLexer()
        lexer.lex(None, 'and')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.KEYWORD
        assert tokens[0].value == 'and'

    def test_or_keyword(self):
        """Test or keyword."""
        lexer = AIFPLLexer()
        lexer.lex(None, 'or')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.KEYWORD
        assert tokens[0].value == 'or'

    def test_keywords_case_insensitive(self):
        """Test that keywords are case-insensitive."""
        test_cases = [
            ('lambda', 'LAMBDA', 'Lambda', 'LaMbDa'),
            ('if', 'IF', 'If', 'iF'),
            ('let', 'LET', 'Let', 'LeT'),
            ('quote', 'QUOTE', 'Quote', 'QuOtE'),
            ('and', 'AND', 'And', 'AnD'),
            ('or', 'OR', 'Or', 'oR'),
        ]
        
        for variations in test_cases:
            for keyword in variations:
                lexer = AIFPLLexer()
                lexer.lex(None, keyword)

                tokens = list(lexer._tokens)
                assert len(tokens) == 1
                assert tokens[0].type == TokenType.KEYWORD, \
                    f"'{keyword}' should be recognized as KEYWORD"

    def test_lambda_expression(self):
        """Test lambda in an expression."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(lambda (x) (* x x))')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type == TokenType.KEYWORD]
        assert len(keyword_tokens) == 1
        assert keyword_tokens[0].value == 'lambda'

    def test_if_expression(self):
        """Test if in an expression."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(if (> x 0) x (- x))')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type == TokenType.KEYWORD]
        assert len(keyword_tokens) == 1
        assert keyword_tokens[0].value == 'if'

    def test_let_expression(self):
        """Test let in an expression."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(let ((x 5)) x)')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type == TokenType.KEYWORD]
        assert len(keyword_tokens) == 1
        assert keyword_tokens[0].value == 'let'

    def test_quote_expression(self):
        """Test quote in an expression."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(quote (+ 1 2))')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type == TokenType.KEYWORD]
        assert len(keyword_tokens) == 1
        assert keyword_tokens[0].value == 'quote'

    def test_and_expression(self):
        """Test and in an expression."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(and #t #f)')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type == TokenType.KEYWORD]
        assert len(keyword_tokens) == 1
        assert keyword_tokens[0].value == 'and'

    def test_or_expression(self):
        """Test or in an expression."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(or #t #f)')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type == TokenType.KEYWORD]
        assert len(keyword_tokens) == 1
        assert keyword_tokens[0].value == 'or'

    def test_nested_keywords(self):
        """Test nested expressions with multiple keywords."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(if (and x y) (or a b) z)')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type == TokenType.KEYWORD]
        assert len(keyword_tokens) == 3
        assert keyword_tokens[0].value == 'if'
        assert keyword_tokens[1].value == 'and'
        assert keyword_tokens[2].value == 'or'

    def test_keyword_like_identifiers(self):
        """Test that similar-looking identifiers are not keywords."""
        non_keywords = ['lambda-x', 'if-then', 'letter', 'android', 'orbit']
        for ident in non_keywords:
            lexer = AIFPLLexer()
            lexer.lex(None, ident)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.IDENTIFIER, \
                f"'{ident}' should be IDENTIFIER, not KEYWORD"

    def test_keywords_with_whitespace(self):
        """Test keywords separated by whitespace."""
        lexer = AIFPLLexer()
        lexer.lex(None, 'lambda if let')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type == TokenType.KEYWORD]
        assert len(keyword_tokens) == 3
        assert [t.value for t in keyword_tokens] == ['lambda', 'if', 'let']

    def test_keywords_in_different_contexts(self):
        """Test keywords in various contexts."""
        test_cases = [
            '(lambda (x) x)',
            '(if #t 1 0)',
            '(let ((x 5)) x)',
            '(quote x)',
            '(and #t #t)',
            '(or #f #t)',
        ]
        
        for expr in test_cases:
            lexer = AIFPLLexer()
            lexer.lex(None, expr)

            tokens = list(lexer._tokens)
            keyword_tokens = [t for t in tokens if t.type == TokenType.KEYWORD]
            assert len(keyword_tokens) >= 1, f"Expression '{expr}' should contain keywords"

    def test_keyword_positions(self):
        """Test that keyword positions are correctly tracked."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(lambda (x) x)')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type == TokenType.KEYWORD]
        assert len(keyword_tokens) == 1
        assert keyword_tokens[0].start == 1  # Position after opening paren

    def test_multiple_keywords_positions(self):
        """Test positions of multiple keywords."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(if (and x y) a b)')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type == TokenType.KEYWORD]
        assert len(keyword_tokens) == 2
        assert keyword_tokens[0].value == 'if'
        assert keyword_tokens[1].value == 'and'
        # Positions should be in order
        assert keyword_tokens[0].start < keyword_tokens[1].start

    def test_keyword_at_start_of_line(self):
        """Test keyword at the beginning of a line."""
        lexer = AIFPLLexer()
        lexer.lex(None, 'lambda')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.KEYWORD
        assert tokens[0].start == 0

    def test_keyword_after_paren(self):
        """Test keyword immediately after opening parenthesis."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(lambda)')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type == TokenType.KEYWORD]
        assert len(keyword_tokens) == 1
        assert keyword_tokens[0].value == 'lambda'

    def test_all_keywords_in_one_expression(self):
        """Test all keywords in a single complex expression."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(let ((f (lambda (x) (if (and x #t) (or x #f) (quote nil))))) f)')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type == TokenType.KEYWORD]
        
        # Should have: let, lambda, if, and, or, quote
        assert len(keyword_tokens) == 6
        keyword_values = [t.value for t in keyword_tokens]
        assert 'let' in keyword_values
        assert 'lambda' in keyword_values
        assert 'if' in keyword_values
        assert 'and' in keyword_values
        assert 'or' in keyword_values
        assert 'quote' in keyword_values
