"""
Tests for Menai delimiter tokenization (parentheses and quote).
"""
import pytest

from syntax.menai.menai_lexer import MenaiLexer
from syntax.lexer import TokenType


class TestMenaiDelimiters:
    """Test Menai delimiter tokenization."""

    def test_left_paren(self):
        """Test left parenthesis."""
        lexer = MenaiLexer()
        lexer.lex(None, '(')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.LPAREN
        assert tokens[0].value == '('

    def test_right_paren(self):
        """Test right parenthesis."""
        lexer = MenaiLexer()
        lexer.lex(None, ')')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.RPAREN
        assert tokens[0].value == ')'

    def test_matching_parens(self):
        """Test matching parentheses."""
        lexer = MenaiLexer()
        lexer.lex(None, '()')

        tokens = list(lexer._tokens)
        assert len(tokens) == 2
        assert tokens[0].type == TokenType.LPAREN
        assert tokens[1].type == TokenType.RPAREN

    def test_nested_parens(self):
        """Test nested parentheses."""
        lexer = MenaiLexer()
        lexer.lex(None, '((()))')

        tokens = list(lexer._tokens)
        assert len(tokens) == 6
        lparen_count = sum(1 for t in tokens if t.type == TokenType.LPAREN)
        rparen_count = sum(1 for t in tokens if t.type == TokenType.RPAREN)
        assert lparen_count == 3
        assert rparen_count == 3

    def test_parens_with_expression(self):
        """Test parentheses around expression."""
        lexer = MenaiLexer()
        lexer.lex(None, '(+ 1 2)')

        tokens = list(lexer._tokens)
        paren_tokens = [t for t in tokens if t.type in (TokenType.LPAREN, TokenType.RPAREN)]
        assert len(paren_tokens) == 2
        assert paren_tokens[0].type == TokenType.LPAREN
        assert paren_tokens[1].type == TokenType.RPAREN

    def test_multiple_expressions(self):
        """Test multiple expressions with parentheses."""
        lexer = MenaiLexer()
        lexer.lex(None, '(+ 1 2) (* 3 4)')

        tokens = list(lexer._tokens)
        paren_tokens = [t for t in tokens if t.type in (TokenType.LPAREN, TokenType.RPAREN)]
        assert len(paren_tokens) == 4

    def test_quote_symbol(self):
        """Test quote symbol."""
        lexer = MenaiLexer()
        lexer.lex(None, "'")

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.QUOTE
        assert tokens[0].value == "'"

    def test_quote_before_expression(self):
        """Test quote before expression."""
        lexer = MenaiLexer()
        lexer.lex(None, "'(+ 1 2)")

        tokens = list(lexer._tokens)
        assert tokens[0].type == TokenType.QUOTE
        # Should have quote, then lparen, then expression, then rparen
        quote_tokens = [t for t in tokens if t.type == TokenType.QUOTE]
        assert len(quote_tokens) == 1

    def test_quote_before_identifier(self):
        """Test quote before identifier."""
        lexer = MenaiLexer()
        lexer.lex(None, "'x")

        tokens = list(lexer._tokens)
        assert len(tokens) == 2
        assert tokens[0].type == TokenType.QUOTE
        assert tokens[1].type == TokenType.IDENTIFIER

    def test_quote_before_list(self):
        """Test quote before list."""
        lexer = MenaiLexer()
        lexer.lex(None, "'(1 2 3)")

        tokens = list(lexer._tokens)
        quote_tokens = [t for t in tokens if t.type == TokenType.QUOTE]
        assert len(quote_tokens) == 1
        assert quote_tokens[0].start == 0

    def test_paren_positions(self):
        """Test that parenthesis positions are correct."""
        lexer = MenaiLexer()
        lexer.lex(None, '(+ 1 2)')

        tokens = list(lexer._tokens)
        lparen = [t for t in tokens if t.type == TokenType.LPAREN][0]
        rparen = [t for t in tokens if t.type == TokenType.RPAREN][0]

        assert lparen.start == 0
        assert rparen.start == 6

    def test_adjacent_parens(self):
        """Test adjacent parentheses."""
        lexer = MenaiLexer()
        lexer.lex(None, ')(')

        tokens = list(lexer._tokens)
        assert len(tokens) == 2
        assert tokens[0].type == TokenType.RPAREN
        assert tokens[1].type == TokenType.LPAREN

    def test_parens_without_spaces(self):
        """Test parentheses without spaces."""
        lexer = MenaiLexer()
        lexer.lex(None, '(+1 2)')

        tokens = list(lexer._tokens)
        paren_tokens = [t for t in tokens if t.type in (TokenType.LPAREN, TokenType.RPAREN)]
        assert len(paren_tokens) == 2

    def test_deeply_nested_parens(self):
        """Test deeply nested parentheses."""
        lexer = MenaiLexer()
        lexer.lex(None, '((((x))))')

        tokens = list(lexer._tokens)
        lparen_count = sum(1 for t in tokens if t.type == TokenType.LPAREN)
        rparen_count = sum(1 for t in tokens if t.type == TokenType.RPAREN)
        assert lparen_count == 4
        assert rparen_count == 4

    def test_unbalanced_left_paren(self):
        """Test unbalanced left parenthesis."""
        lexer = MenaiLexer()
        lexer.lex(None, '(+ 1 2')

        tokens = list(lexer._tokens)
        lparen_count = sum(1 for t in tokens if t.type == TokenType.LPAREN)
        rparen_count = sum(1 for t in tokens if t.type == TokenType.RPAREN)
        assert lparen_count == 1
        assert rparen_count == 0

    def test_unbalanced_right_paren(self):
        """Test unbalanced right parenthesis."""
        lexer = MenaiLexer()
        lexer.lex(None, '+ 1 2)')

        tokens = list(lexer._tokens)
        lparen_count = sum(1 for t in tokens if t.type == TokenType.LPAREN)
        rparen_count = sum(1 for t in tokens if t.type == TokenType.RPAREN)
        assert lparen_count == 0
        assert rparen_count == 1

    def test_empty_parens(self):
        """Test empty parentheses."""
        lexer = MenaiLexer()
        lexer.lex(None, '()')

        tokens = list(lexer._tokens)
        assert len(tokens) == 2
        assert tokens[0].type == TokenType.LPAREN
        assert tokens[1].type == TokenType.RPAREN

    def test_parens_in_string_ignored(self):
        """Test that parentheses in strings are part of string."""
        lexer = MenaiLexer()
        lexer.lex(None, '"(test)"')

        tokens = list(lexer._tokens)
        # Should only have one string token, no separate paren tokens
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING

    def test_quote_in_string_ignored(self):
        """Test that quote in string is part of string."""
        lexer = MenaiLexer()
        lexer.lex(None, r'"it\'s"')

        tokens = list(lexer._tokens)
        # Should be one string token
        string_tokens = [t for t in tokens if t.type == TokenType.STRING]
        assert len(string_tokens) == 1

    def test_parens_in_comment_ignored(self):
        """Test that parentheses in comments are part of comment."""
        lexer = MenaiLexer()
        lexer.lex(None, '; (+ 1 2)')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT

    def test_quote_in_comment_ignored(self):
        """Test that quote in comment is part of comment."""
        lexer = MenaiLexer()
        lexer.lex(None, "; 'quoted")

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT

    def test_lambda_expression_parens(self):
        """Test parentheses in lambda expression."""
        lexer = MenaiLexer()
        lexer.lex(None, '(lambda (x) (* x x))')

        tokens = list(lexer._tokens)
        lparen_count = sum(1 for t in tokens if t.type == TokenType.LPAREN)
        rparen_count = sum(1 for t in tokens if t.type == TokenType.RPAREN)
        assert lparen_count == 3
        assert rparen_count == 3

    def test_let_expression_parens(self):
        """Test parentheses in let expression."""
        lexer = MenaiLexer()
        lexer.lex(None, '(let ((x 5)) x)')

        tokens = list(lexer._tokens)
        lparen_count = sum(1 for t in tokens if t.type == TokenType.LPAREN)
        rparen_count = sum(1 for t in tokens if t.type == TokenType.RPAREN)
        assert lparen_count == 3
        assert rparen_count == 3

    def test_list_expression_parens(self):
        """Test parentheses in list expression."""
        lexer = MenaiLexer()
        lexer.lex(None, '(list 1 2 3)')

        tokens = list(lexer._tokens)
        lparen_count = sum(1 for t in tokens if t.type == TokenType.LPAREN)
        rparen_count = sum(1 for t in tokens if t.type == TokenType.RPAREN)
        assert lparen_count == 1
        assert rparen_count == 1

    def test_multiple_quotes(self):
        """Test multiple quote symbols."""
        lexer = MenaiLexer()
        lexer.lex(None, "''x")

        tokens = list(lexer._tokens)
        quote_tokens = [t for t in tokens if t.type == TokenType.QUOTE]
        assert len(quote_tokens) == 2

    def test_quote_list(self):
        """Test quoted list."""
        lexer = MenaiLexer()
        lexer.lex(None, "'(a b c)")

        tokens = list(lexer._tokens)
        assert tokens[0].type == TokenType.QUOTE
        assert tokens[1].type == TokenType.LPAREN

    def test_quote_empty_list(self):
        """Test quoted empty list."""
        lexer = MenaiLexer()
        lexer.lex(None, "'()")

        tokens = list(lexer._tokens)
        assert len(tokens) == 3
        assert tokens[0].type == TokenType.QUOTE
        assert tokens[1].type == TokenType.LPAREN
        assert tokens[2].type == TokenType.RPAREN

    def test_parens_with_numbers(self):
        """Test parentheses with numbers."""
        lexer = MenaiLexer()
        lexer.lex(None, '(1 2 3)')

        tokens = list(lexer._tokens)
        assert tokens[0].type == TokenType.LPAREN
        assert tokens[-1].type == TokenType.RPAREN

    def test_parens_with_strings(self):
        """Test parentheses with strings."""
        lexer = MenaiLexer()
        lexer.lex(None, '("a" "b")')

        tokens = list(lexer._tokens)
        assert tokens[0].type == TokenType.LPAREN
        assert tokens[-1].type == TokenType.RPAREN

    def test_complex_nesting(self):
        """Test complex nested structure."""
        lexer = MenaiLexer()
        lexer.lex(None, '(a (b (c d) e) f)')

        tokens = list(lexer._tokens)
        lparen_count = sum(1 for t in tokens if t.type == TokenType.LPAREN)
        rparen_count = sum(1 for t in tokens if t.type == TokenType.RPAREN)
        assert lparen_count == rparen_count
        assert lparen_count == 3
