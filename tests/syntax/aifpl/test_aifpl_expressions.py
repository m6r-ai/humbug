"""
Tests for AIFPL complex expression tokenization.
"""
import pytest

from syntax.aifpl.aifpl_lexer import AIFPLLexer
from syntax.lexer import TokenType


class TestAIFPLExpressions:
    """Test AIFPL complex expression tokenization."""

    def test_simple_arithmetic(self):
        """Test simple arithmetic expression."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(+ 1 2)')

        tokens = list(lexer._tokens)
        # whitespace is skipped
        assert len(tokens) == 5  # ( + 1 2 )

    def test_nested_arithmetic(self):
        """Test nested arithmetic expression."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(+ (* 2 3) (- 5 1))')

        tokens = list(lexer._tokens)
        number_tokens = [t for t in tokens if t.type == TokenType.NUMBER]
        assert len(number_tokens) == 4
        assert [t.value for t in number_tokens] == ['2', '3', '5', '1']

    def test_lambda_expression(self):
        """Test lambda expression."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(lambda (x) (* x x))')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type == TokenType.KEYWORD]
        assert len(keyword_tokens) == 1
        assert keyword_tokens[0].value == 'lambda'

    def test_lambda_with_multiple_params(self):
        """Test lambda with multiple parameters."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(lambda (x y z) (+ x y z))')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type == TokenType.KEYWORD]
        assert len(keyword_tokens) == 1

    def test_let_binding(self):
        """Test let binding expression."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(let ((x 5)) (* x 2))')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type == TokenType.KEYWORD]
        assert len(keyword_tokens) == 1
        assert keyword_tokens[0].value == 'let'

    def test_let_multiple_bindings(self):
        """Test let with multiple bindings."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(let ((x 5) (y 10)) (+ x y))')

        tokens = list(lexer._tokens)
        number_tokens = [t for t in tokens if t.type == TokenType.NUMBER]
        assert len(number_tokens) == 2

    def test_if_expression(self):
        """Test if expression."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(if (> x 0) x (- x))')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type == TokenType.KEYWORD]
        assert len(keyword_tokens) == 1
        assert keyword_tokens[0].value == 'if'

    def test_nested_if(self):
        """Test nested if expressions."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(if (> x 0) (if (< x 10) x 10) 0)')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type == TokenType.KEYWORD]
        assert len(keyword_tokens) == 2
        assert all(t.value == 'if' for t in keyword_tokens)

    def test_quoted_expression(self):
        """Test quoted expression."""
        lexer = AIFPLLexer()
        lexer.lex(None, "'(+ 1 2)")

        tokens = list(lexer._tokens)
        quote_tokens = [t for t in tokens if t.type == TokenType.QUOTE]
        assert len(quote_tokens) == 1

    def test_quote_keyword(self):
        """Test quote keyword form."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(quote (+ 1 2))')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type == TokenType.KEYWORD]
        assert len(keyword_tokens) == 1
        assert keyword_tokens[0].value == 'quote'

    def test_list_construction(self):
        """Test list construction."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(list 1 2 3 4 5)')

        tokens = list(lexer._tokens)
        number_tokens = [t for t in tokens if t.type == TokenType.NUMBER]
        assert len(number_tokens) == 5

    def test_cons_expression(self):
        """Test cons expression."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(cons 1 (list 2 3))')

        tokens = list(lexer._tokens)
        ident_tokens = [t for t in tokens if t.type == TokenType.IDENTIFIER]
        assert any(t.value == 'cons' for t in ident_tokens)
        assert any(t.value == 'list' for t in ident_tokens)

    def test_map_expression(self):
        """Test map expression."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(map (lambda (x) (* x 2)) (list 1 2 3))')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type == TokenType.KEYWORD]
        assert len(keyword_tokens) == 1
        assert keyword_tokens[0].value == 'lambda'

    def test_filter_expression(self):
        """Test filter expression."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(filter (lambda (x) (> x 0)) (list 1 2 3 4))')

        tokens = list(lexer._tokens)
        tokens = list(lexer._tokens)
        number_tokens = [t for t in tokens if t.type == TokenType.NUMBER]
        # Should have 0 (from > x 0), 1, 2, 3, 4
        assert len(number_tokens) == 5
        # Should have 0 (from > x 0), -1, 2, -3, 4
        assert len(number_tokens) == 5

    def test_fold_expression(self):
        """Test fold expression."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(fold + 0 (list 1 2 3 4))')

        tokens = list(lexer._tokens)
        ident_tokens = [t for t in tokens if t.type == TokenType.IDENTIFIER]
        assert any(t.value == 'fold' for t in ident_tokens)
        assert any(t.value == '+' for t in ident_tokens)

    def test_string_operations(self):
        """Test string operations."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(string-append "hello" " " "world")')

        tokens = list(lexer._tokens)
        string_tokens = [t for t in tokens if t.type == TokenType.STRING]
        assert len(string_tokens) == 3

    def test_string_predicate(self):
        """Test string predicate."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(string? "test")')

        tokens = list(lexer._tokens)
        ident_tokens = [t for t in tokens if t.type == TokenType.IDENTIFIER]
        assert any(t.value == 'string?' for t in ident_tokens)

    def test_boolean_logic(self):
        """Test boolean logic expressions."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(and (> x 0) (< x 10))')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type == TokenType.KEYWORD]
        assert len(keyword_tokens) == 1
        assert keyword_tokens[0].value == 'and'

    def test_complex_boolean(self):
        """Test complex boolean expression."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(or (and #t #f) (and #f #t))')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type == TokenType.KEYWORD]
        # Should have 'or' and two 'and's
        assert len(keyword_tokens) == 3

    def test_math_functions(self):
        """Test math functions."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(sin (* pi 0.5))')

        tokens = list(lexer._tokens)
        ident_tokens = [t for t in tokens if t.type == TokenType.IDENTIFIER]
        assert any(t.value == 'sin' for t in ident_tokens)
        assert any(t.value == 'pi' for t in ident_tokens)

    def test_complex_number_expression(self):
        """Test complex number expression."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(+ 1 (* 2 j))')

        tokens = list(lexer._tokens)
        ident_tokens = [t for t in tokens if t.type == TokenType.IDENTIFIER]
        assert any(t.value == 'j' for t in ident_tokens)

    def test_alist_construction(self):
        """Test alist construction."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(alist ("key1" "value1") ("key2" "value2"))')

        tokens = list(lexer._tokens)
        string_tokens = [t for t in tokens if t.type == TokenType.STRING]
        assert len(string_tokens) == 4

    def test_alist_access(self):
        """Test alist access."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(alist-get my-alist "key")')

        tokens = list(lexer._tokens)
        ident_tokens = [t for t in tokens if t.type == TokenType.IDENTIFIER]
        assert any(t.value == 'alist-get' for t in ident_tokens)

    def test_pattern_matching(self):
        """Test pattern matching expression."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(match x (42 "answer") (_ "other"))')

        tokens = list(lexer._tokens)
        number_tokens = [t for t in tokens if t.type == TokenType.NUMBER]
        assert len(number_tokens) == 1
        assert number_tokens[0].value == '42'

    def test_range_expression(self):
        """Test range expression."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(range 1 10)')

        tokens = list(lexer._tokens)
        ident_tokens = [t for t in tokens if t.type == TokenType.IDENTIFIER]
        assert any(t.value == 'range' for t in ident_tokens)

    def test_deeply_nested_expression(self):
        """Test deeply nested expression."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(+ (* (- 10 5) (/ 20 4)) (sqrt 16))')

        tokens = list(lexer._tokens)
        lparen_count = sum(1 for t in tokens if t.type == TokenType.LPAREN)
        rparen_count = sum(1 for t in tokens if t.type == TokenType.RPAREN)
        assert lparen_count == rparen_count
        assert lparen_count == 5

    def test_function_composition(self):
        """Test function composition."""
        lexer = AIFPLLexer()
        lexer.lex(None, '((lambda (f g) (lambda (x) (f (g x)))) func1 func2)')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type == TokenType.KEYWORD]
        assert len(keyword_tokens) == 2
        assert all(t.value == 'lambda' for t in keyword_tokens)

    def test_recursive_function(self):
        """Test recursive function definition."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(let ((fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))) (fact 5))')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type == TokenType.KEYWORD]
        # Should have let, lambda, if
        assert len(keyword_tokens) >= 3

    def test_type_predicates(self):
        """Test type predicate expressions."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(and (number? x) (> x 0))')

        tokens = list(lexer._tokens)
        ident_tokens = [t for t in tokens if t.type == TokenType.IDENTIFIER]
        assert any(t.value == 'number?' for t in ident_tokens)

    def test_list_operations_chain(self):
        """Test chained list operations."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(reverse (append (list 1 2) (list 3 4)))')

        tokens = list(lexer._tokens)
        ident_tokens = [t for t in tokens if t.type == TokenType.IDENTIFIER]
        assert any(t.value == 'reverse' for t in ident_tokens)
        assert any(t.value == 'append' for t in ident_tokens)

    def test_bitwise_operations(self):
        """Test bitwise operations."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(bit-or (bit-and #xFF #x0F) #x10)')

        tokens = list(lexer._tokens)
        ident_tokens = [t for t in tokens if t.type == TokenType.IDENTIFIER]
        assert any(t.value == 'bit-or' for t in ident_tokens)
        assert any(t.value == 'bit-and' for t in ident_tokens)

    def test_mixed_number_bases(self):
        """Test expression with mixed number bases."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(+ #xFF #b1111 #o77 42)')

        tokens = list(lexer._tokens)
        number_tokens = [t for t in tokens if t.type == TokenType.NUMBER]
        assert len(number_tokens) == 4

    def test_scientific_notation_in_expression(self):
        """Test scientific notation in expression."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(* 1.5e10 2.0e-5)')

        tokens = list(lexer._tokens)
        number_tokens = [t for t in tokens if t.type == TokenType.NUMBER]
        assert len(number_tokens) == 2
        assert '1.5e10' in [t.value for t in number_tokens]
        assert '2.0e-5' in [t.value for t in number_tokens]

    def test_expression_with_comment(self):
        """Test expression with inline comment."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(+ 1 2) ; adds two numbers')

        tokens = list(lexer._tokens)
        comment_tokens = [t for t in tokens if t.type == TokenType.COMMENT]
        assert len(comment_tokens) == 1
        number_tokens = [t for t in tokens if t.type == TokenType.NUMBER]
        assert len(number_tokens) == 2

    def test_multiline_expression_state(self):
        """Test multiline expression maintains state."""
        lexer1 = AIFPLLexer()
        state1 = lexer1.lex(None, '(let ((x 5)')

        lexer2 = AIFPLLexer()
        state2 = lexer2.lex(state1, '      (y 10))')

        lexer3 = AIFPLLexer()
        state3 = lexer3.lex(state2, '  (+ x y))')

        # Should parse without errors
        tokens3 = list(lexer3._tokens)
        assert len(tokens3) > 0

    def test_empty_list(self):
        """Test empty list expression."""
        lexer = AIFPLLexer()
        lexer.lex(None, '()')

        tokens = list(lexer._tokens)
        assert len(tokens) == 2
        assert tokens[0].type == TokenType.LPAREN
        assert tokens[1].type == TokenType.RPAREN

    def test_quoted_empty_list(self):
        """Test quoted empty list."""
        lexer = AIFPLLexer()
        lexer.lex(None, "'()")

        tokens = list(lexer._tokens)
        assert tokens[0].type == TokenType.QUOTE
        assert tokens[1].type == TokenType.LPAREN
        assert tokens[2].type == TokenType.RPAREN

    def test_nested_let_bindings(self):
        """Test nested let bindings."""
        lexer = AIFPLLexer()
        lexer.lex(None, '(let ((x 5)) (let ((y 10)) (+ x y)))')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type == TokenType.KEYWORD]
        assert len(keyword_tokens) == 2
        assert all(t.value == 'let' for t in keyword_tokens)

    def test_higher_order_function_application(self):
        """Test higher-order function application."""
        lexer = AIFPLLexer()
        lexer.lex(None, '((lambda (f) (f 5)) (lambda (x) (* x x)))')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type == TokenType.KEYWORD]
        assert len(keyword_tokens) == 2
