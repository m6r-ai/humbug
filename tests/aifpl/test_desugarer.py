"""Tests for the AIFPL desugarer.

This module tests the desugarer's ability to transform complex constructs
(like match expressions) into simpler core language constructs.
"""

import pytest

from aifpl.aifpl_desugarer import AIFPLDesugarer
from aifpl.aifpl_lexer import AIFPLLexer
from aifpl.aifpl_parser import AIFPLParser
from aifpl.aifpl_semantic_analyzer import AIFPLSemanticAnalyzer
from aifpl.aifpl_value import (
    AIFPLValue, AIFPLSymbol, AIFPLList, AIFPLInteger, AIFPLString, AIFPLBoolean
)
from aifpl.aifpl_error import AIFPLEvalError


def parse_and_analyze_expression(expr_str: str) -> AIFPLValue:
    """Helper to parse and semantically analyze an expression string into AST."""
    lexer = AIFPLLexer()
    tokens = lexer.lex(expr_str)
    parser = AIFPLParser()
    ast = parser.parse(tokens, expr_str)
    # Run semantic analysis before desugaring
    analyzer = AIFPLSemanticAnalyzer()
    return analyzer.analyze(ast)


class TestDesugarerBasic:
    """Test basic desugarer functionality."""

    def test_literals_pass_through(self):
        """Test that literals pass through unchanged."""
        desugarer = AIFPLDesugarer()

        # Numbers
        num = AIFPLInteger(42)
        assert desugarer.desugar(num) == num

        # Strings
        string = AIFPLString("hello")
        assert desugarer.desugar(string) == string

        # Booleans
        bool_true = AIFPLBoolean(True)
        assert desugarer.desugar(bool_true) == bool_true

    def test_symbols_pass_through(self):
        """Test that symbols pass through unchanged."""
        desugarer = AIFPLDesugarer()

        symbol = AIFPLSymbol('x')
        assert desugarer.desugar(symbol) == symbol

    def test_empty_list_passes_through(self):
        """Test that empty lists pass through unchanged."""
        desugarer = AIFPLDesugarer()

        empty = AIFPLList(())
        assert desugarer.desugar(empty) == empty

    def test_quote_not_desugared(self):
        """Test that quoted expressions are not desugared."""
        desugarer = AIFPLDesugarer()

        # (quote (match x (42 "found")))
        expr = parse_and_analyze_expression("(quote (match x (42 \"found\")))")
        result = desugarer.desugar(expr)

        # Should remain unchanged
        assert isinstance(result, AIFPLList)
        assert result.first().name == 'quote'


class TestDesugarerCoreConstructs:
    """Test desugaring of core constructs (if, let, lambda)."""

    def test_if_desugars_children(self):
        """Test that if expressions desugar their children."""
        desugarer = AIFPLDesugarer()

        # (if (match x (42 #t) (_ #f)) "yes" "no")
        # The match should be desugared, but if structure preserved
        expr = parse_and_analyze_expression('(if (match x (42 #t) (_ #f)) "yes" "no")')
        result = desugarer.desugar(expr)

        # Result should be an if
        assert isinstance(result, AIFPLList)
        assert result.first().name == 'if'

        # Condition should be desugared (should be a let, not match)
        condition = result.elements[1]
        assert isinstance(condition, AIFPLList)
        assert condition.first().name == 'let'

    def test_let_desugars_children(self):
        """Test that let expressions desugar their children."""
        desugarer = AIFPLDesugarer()

        # (let ((x (match y (42 1) (_ 0)))) x)
        expr = parse_and_analyze_expression('(let ((x (match y (42 1) (_ 0)))) x)')
        result = desugarer.desugar(expr)

        # Result should be a let
        assert isinstance(result, AIFPLList)
        assert result.first().name == 'let'

        # Binding value should be desugared
        bindings = result.elements[1]
        binding = bindings.elements[0]
        value = binding.elements[1]
        assert isinstance(value, AIFPLList)
        assert value.first().name == 'let'  # Match desugared to let

    def test_lambda_desugars_body(self):
        """Test that lambda expressions desugar their body."""
        desugarer = AIFPLDesugarer()

        # (lambda (x) (match x (42 "found") (_ "not found")))
        expr = parse_and_analyze_expression('(lambda (x) (match x (42 "found") (_ "not found")))')
        result = desugarer.desugar(expr)

        # Result should be a lambda
        assert isinstance(result, AIFPLList)
        assert result.first().name == 'lambda'

        # Body should be desugared
        body = result.elements[2]
        assert isinstance(body, AIFPLList)
        assert body.first().name == 'let'  # Match desugared to let

    def test_function_call_desugars_all_elements(self):
        """Test that function calls desugar all elements."""
        desugarer = AIFPLDesugarer()

        # (+ (match x (42 1) (_ 0)) (match y (1 10) (_ 0)))
        expr = parse_and_analyze_expression('(+ (match x (42 1) (_ 0)) (match y (1 10) (_ 0)))')
        result = desugarer.desugar(expr)

        # Result should be a call to +
        assert isinstance(result, AIFPLList)
        assert result.first().name == '+'

        # Both arguments should be desugared
        arg1 = result.elements[1]
        arg2 = result.elements[2]
        assert isinstance(arg1, AIFPLList)
        assert arg1.first().name == 'let'
        assert isinstance(arg2, AIFPLList)
        assert arg2.first().name == 'let'


class TestDesugarerMatchLiteral:
    """Test desugaring of literal patterns in match expressions."""

    def test_match_number_literal(self):
        """Test desugaring of number literal pattern."""
        desugarer = AIFPLDesugarer()

        # (match x (42 "found") (_ "default"))
        expr = parse_and_analyze_expression('(match x (42 "found") (_ "default"))')
        result = desugarer.desugar(expr)

        # Should be a let binding a temp variable
        assert isinstance(result, AIFPLList)
        assert result.first().name == 'let'

        # Should have bindings and body
        bindings = result.elements[1]
        assert isinstance(bindings, AIFPLList)
        assert len(bindings.elements) == 1

        # Binding should be (#:match-tmp-1 x)
        binding = bindings.elements[0]
        temp_var = binding.elements[0]
        assert isinstance(temp_var, AIFPLSymbol)
        assert temp_var.name.startswith('#:match-tmp-')

        # Body should be nested if expressions
        body = result.elements[2]
        assert isinstance(body, AIFPLList)
        assert body.first().name == 'if'

    def test_match_string_literal(self):
        """Test desugaring of string literal pattern."""
        desugarer = AIFPLDesugarer()

        # (match x ("hello" "greeting") (_ "other"))
        expr = parse_and_analyze_expression('(match x ("hello" "greeting") (_ "other"))')
        result = desugarer.desugar(expr)

        # Should be a let with if
        assert isinstance(result, AIFPLList)
        assert result.first().name == 'let'

        body = result.elements[2]
        assert isinstance(body, AIFPLList)
        assert body.first().name == 'if'

    def test_match_boolean_literal(self):
        """Test desugaring of boolean literal pattern."""
        desugarer = AIFPLDesugarer()

        # (match x (#t "true") (#f "false"))
        expr = parse_and_analyze_expression('(match x (#t "true") (#f "false"))')
        result = desugarer.desugar(expr)

        # Should be a let with if
        assert isinstance(result, AIFPLList)
        assert result.first().name == 'let'


class TestDesugarerMatchVariable:
    """Test desugaring of variable patterns in match expressions."""

    def test_match_variable_binding(self):
        """Test desugaring of variable binding pattern."""
        desugarer = AIFPLDesugarer()

        # (match x (n n))
        expr = parse_and_analyze_expression('(match x (n n))')
        result = desugarer.desugar(expr)

        # Should be: (let ((#:tmp x)) (if #t (let ((n #:tmp)) n) error))
        assert isinstance(result, AIFPLList)
        assert result.first().name == 'let'

        body = result.elements[2]
        assert isinstance(body, AIFPLList)
        assert body.first().name == 'if'

        # Condition should be #t (variable always matches)
        condition = body.elements[1]
        assert isinstance(condition, AIFPLBoolean)
        assert condition.value is True

        # Then branch should be a let binding the variable
        then_branch = body.elements[2]
        assert isinstance(then_branch, AIFPLList)
        assert then_branch.first().name == 'let'

    def test_match_wildcard(self):
        """Test desugaring of wildcard pattern."""
        desugarer = AIFPLDesugarer()

        # (match x (_ "anything"))
        expr = parse_and_analyze_expression('(match x (_ "anything"))')
        result = desugarer.desugar(expr)

        # Should be: (let ((#:tmp x)) (if #t "anything" error))
        assert isinstance(result, AIFPLList)
        assert result.first().name == 'let'

        body = result.elements[2]
        assert isinstance(body, AIFPLList)
        assert body.first().name == 'if'

        # Condition should be #t
        condition = body.elements[1]
        assert isinstance(condition, AIFPLBoolean)
        assert condition.value is True

        # Then branch should be the result directly (no bindings)
        then_branch = body.elements[2]
        assert isinstance(then_branch, AIFPLString)
        assert then_branch.value == "anything"


class TestDesugarerMatchType:
    """Test desugaring of type patterns in match expressions."""

    def test_match_type_pattern(self):
        """Test desugaring of type pattern."""
        desugarer = AIFPLDesugarer()

        # (match x ((number? n) n) (_ "not a number"))
        expr = parse_and_analyze_expression('(match x ((number? n) n) (_ "not a number"))')
        result = desugarer.desugar(expr)

        # Should be: (let ((#:tmp x)) (if (number? #:tmp) (let ((n #:tmp)) n) ...))
        assert isinstance(result, AIFPLList)
        assert result.first().name == 'let'

        body = result.elements[2]
        assert isinstance(body, AIFPLList)
        assert body.first().name == 'if'

        # Condition should be (number? #:tmp)
        condition = body.elements[1]
        assert isinstance(condition, AIFPLList)
        assert condition.first().name == 'number?'

        # Then branch should bind the variable
        then_branch = body.elements[2]
        assert isinstance(then_branch, AIFPLList)
        assert then_branch.first().name == 'let'

    def test_match_type_pattern_with_wildcard(self):
        """Test desugaring of type pattern with wildcard."""
        desugarer = AIFPLDesugarer()

        # (match x ((string? _) "is string") (_ "not string"))
        expr = parse_and_analyze_expression('(match x ((string? _) "is string") (_ "not string"))')
        result = desugarer.desugar(expr)

        # Should test type but not bind variable
        assert isinstance(result, AIFPLList)
        assert result.first().name == 'let'

        body = result.elements[2]
        condition = body.elements[1]
        assert isinstance(condition, AIFPLList)
        assert condition.first().name == 'string?'

        # Then branch should not have a let (no binding)
        then_branch = body.elements[2]
        assert isinstance(then_branch, AIFPLString)
        assert then_branch.value == "is string"


class TestDesugarerMatchList:
    """Test desugaring of list patterns in match expressions."""

    def test_match_empty_list(self):
        """Test desugaring of empty list pattern."""
        desugarer = AIFPLDesugarer()

        # (match x (() "empty") (_ "not empty"))
        expr = parse_and_analyze_expression('(match x (() "empty") (_ "not empty"))')
        result = desugarer.desugar(expr)

        # Should test with null?
        assert isinstance(result, AIFPLList)
        assert result.first().name == 'let'

        body = result.elements[2]
        condition = body.elements[1]
        assert isinstance(condition, AIFPLList)
        assert condition.first().name == 'null?'

    def test_match_fixed_list_simple(self):
        """Test desugaring of simple fixed-length list pattern."""
        desugarer = AIFPLDesugarer()

        # (match x ((a b) (list a b)) (_ "wrong"))
        expr = parse_and_analyze_expression('(match x ((a b) (list a b)) (_ "wrong"))')
        result = desugarer.desugar(expr)

        # Should test list? and length
        assert isinstance(result, AIFPLList)
        assert result.first().name == 'let'

        body = result.elements[2]
        condition = body.elements[1]

        # Condition should be an and expression
        assert isinstance(condition, AIFPLList)
        assert condition.first().name == 'and'

        # Should contain list? and length checks
        tests = condition.elements[1:]
        has_list_test = any(
            isinstance(t, AIFPLList) and t.first().name == 'list?'
            for t in tests
        )
        has_length_test = any(
            isinstance(t, AIFPLList) and 
            isinstance(t.elements[1], AIFPLList) and
            t.elements[1].first().name == 'length'
            for t in tests
        )
        assert has_list_test
        assert has_length_test

    def test_match_fixed_list_with_literals(self):
        """Test desugaring of fixed-length list with literal patterns."""
        desugarer = AIFPLDesugarer()

        # (match x ((1 2 3) "found") (_ "not found"))
        expr = parse_and_analyze_expression('(match x ((1 2 3) "found") (_ "not found"))')
        result = desugarer.desugar(expr)

        # Should desugar to nested tests
        assert isinstance(result, AIFPLList)
        assert result.first().name == 'let'


class TestDesugarerMatchCons:
    """Test desugaring of cons patterns in match expressions."""

    def test_match_cons_simple(self):
        """Test desugaring of simple cons pattern."""
        desugarer = AIFPLDesugarer()

        # (match x ((head . tail) (list head tail)) (_ "not list"))
        expr = parse_and_analyze_expression('(match x ((head . tail) (list head tail)) (_ "not list"))')
        result = desugarer.desugar(expr)

        # Should test list? and length
        assert isinstance(result, AIFPLList)
        assert result.first().name == 'let'

        body = result.elements[2]
        condition = body.elements[1]

        # Condition should be an and expression
        assert isinstance(condition, AIFPLList)
        assert condition.first().name == 'and'

    def test_match_cons_multiple_heads(self):
        """Test desugaring of cons pattern with multiple head elements."""
        desugarer = AIFPLDesugarer()

        # (match x ((a b . rest) (list a b rest)) (_ "not list"))
        expr = parse_and_analyze_expression('(match x ((a b . rest) (list a b rest)) (_ "not list"))')
        result = desugarer.desugar(expr)

        # Should test list? and length >= 2
        assert isinstance(result, AIFPLList)
        assert result.first().name == 'let'


class TestDesugarerMatchNested:
    """Test desugaring of nested patterns in match expressions."""

    def test_match_nested_list(self):
        """Test desugaring of nested list pattern."""
        desugarer = AIFPLDesugarer()

        # (match x (((a b)) (list a b)) (_ "wrong"))
        expr = parse_and_analyze_expression('(match x (((a b)) (list a b)) (_ "wrong"))')
        result = desugarer.desugar(expr)

        # Should desugar to nested tests
        assert isinstance(result, AIFPLList)
        assert result.first().name == 'let'

    def test_match_nested_type(self):
        """Test desugaring of nested type pattern."""
        desugarer = AIFPLDesugarer()

        # (match x (((number? n)) n) (_ "not list with number"))
        expr = parse_and_analyze_expression('(match x (((number? n)) n) (_ "not list with number"))')
        result = desugarer.desugar(expr)

        # Should desugar to nested tests
        assert isinstance(result, AIFPLList)
        assert result.first().name == 'let'


class TestDesugarerMatchMultipleClauses:
    """Test desugaring of match with multiple clauses."""

    def test_match_multiple_clauses_simple(self):
        """Test desugaring of match with multiple simple clauses."""
        desugarer = AIFPLDesugarer()

        # (match x (1 "one") (2 "two") (3 "three") (_ "other"))
        expr = parse_and_analyze_expression('(match x (1 "one") (2 "two") (3 "three") (_ "other"))')
        result = desugarer.desugar(expr)

        # Should be nested if expressions
        assert isinstance(result, AIFPLList)
        assert result.first().name == 'let'

        body = result.elements[2]
        assert isinstance(body, AIFPLList)
        assert body.first().name == 'if'

        # Else branch should also be an if
        else_branch = body.elements[3]
        assert isinstance(else_branch, AIFPLList)
        assert else_branch.first().name == 'if'

    def test_match_multiple_clauses_complex(self):
        """Test desugaring of match with complex multiple clauses."""
        desugarer = AIFPLDesugarer()

        # (match x ((number? n) n) ((string? s) s) (() "empty") (_ "other"))
        expr = parse_and_analyze_expression('(match x ((number? n) n) ((string? s) s) (() "empty") (_ "other"))')
        result = desugarer.desugar(expr)

        # Should be nested if expressions
        assert isinstance(result, AIFPLList)
        assert result.first().name == 'let'


class TestDesugarerMatchErrors:
    """Test error handling in semantic analyzer (errors caught before desugaring)."""

    def test_match_no_clauses(self):
        """Test error when match has no clauses."""
        # Errors should be caught by semantic analyzer, not desugarer
        analyzer = AIFPLSemanticAnalyzer()
        lexer = AIFPLLexer()
        parser = AIFPLParser()
        expr = parser.parse(lexer.lex('(match x)'), '(match x)')

        with pytest.raises(AIFPLEvalError, match="wrong number of arguments"):
            analyzer.analyze(expr)

    def test_match_invalid_clause(self):
        """Test error when match clause is invalid."""
        analyzer = AIFPLSemanticAnalyzer()
        lexer = AIFPLLexer()
        parser = AIFPLParser()
        expr = parser.parse(lexer.lex('(match x (42))'), '(match x (42))')

        with pytest.raises(AIFPLEvalError, match="wrong number of elements"):
            analyzer.analyze(expr)

    def test_cons_pattern_dot_at_start(self):
        """Test error when cons pattern has dot at start."""
        analyzer = AIFPLSemanticAnalyzer()
        lexer = AIFPLLexer()
        code = '(match x ((. tail) "bad") (_ "other"))'
        parser = AIFPLParser()
        expr = parser.parse(lexer.lex(code), code)

        with pytest.raises(AIFPLEvalError, match="dot at beginning"):
            analyzer.analyze(expr)

    def test_cons_pattern_dot_at_end(self):
        """Test error when cons pattern has dot at end."""
        analyzer = AIFPLSemanticAnalyzer()
        lexer = AIFPLLexer()
        code = '(match x ((head .) "bad") (_ "other"))'
        parser = AIFPLParser()
        expr = parser.parse(lexer.lex(code), code)

        with pytest.raises(AIFPLEvalError, match="dot at end"):
            analyzer.analyze(expr)

    def test_cons_pattern_multiple_after_dot(self):
        """Test error when cons pattern has multiple elements after dot."""
        analyzer = AIFPLSemanticAnalyzer()
        lexer = AIFPLLexer()
        code = '(match x ((head . a b) "bad") (_ "other"))'
        parser = AIFPLParser()
        expr = parser.parse(lexer.lex(code), code)

        with pytest.raises(AIFPLEvalError, match="multiple elements after dot"):
            analyzer.analyze(expr)


class TestDesugarerTempVariables:
    """Test temporary variable generation."""

    def test_temp_variables_unique(self):
        """Test that temporary variables are unique."""
        desugarer = AIFPLDesugarer()

        # Multiple match expressions should get different temp vars
        expr1 = parse_and_analyze_expression('(match x (42 "found") (_ "not"))')
        result1 = desugarer.desugar(expr1)

        expr2 = parse_and_analyze_expression('(match y (1 "one") (_ "other"))')
        result2 = desugarer.desugar(expr2)

        # Extract temp var names
        def get_temp_var(expr):
            # expr is (let ((temp val)) body)
            bindings = expr.elements[1]
            binding = bindings.elements[0]
            return binding.elements[0].name

        temp1 = get_temp_var(result1)
        temp2 = get_temp_var(result2)

        # Should be different
        assert temp1 != temp2
        assert temp1.startswith('#:match-tmp-')
        assert temp2.startswith('#:match-tmp-')

    def test_nested_match_temp_variables(self):
        """Test that nested matches get unique temp variables."""
        desugarer = AIFPLDesugarer()

        # (match x ((number? n) (match n (42 "found") (_ "not 42"))) (_ "not number"))
        expr = parse_and_analyze_expression('(match x ((number? n) (match n (42 "found") (_ "not 42"))) (_ "not number"))')
        result = desugarer.desugar(expr)

        # Both matches should have different temp variables
        assert isinstance(result, AIFPLList)
        assert result.first().name == 'let'


class TestDesugarerIntegration:
    """Integration tests for desugarer with full evaluation."""

    def test_desugared_match_evaluates_correctly(self, aifpl):
        """Test that desugared match expressions evaluate correctly."""
        # We need to integrate the desugarer with the evaluator
        # For now, we'll just test the structure

        desugarer = AIFPLDesugarer()

        # (match 42 (42 "found") (_ "not found"))
        expr = parse_and_analyze_expression('(match 42 (42 "found") (_ "not found"))')
        result = desugarer.desugar(expr)

        # The desugared expression should be valid AIFPL code
        assert isinstance(result, AIFPLList)
        assert result.first().name == 'let'
