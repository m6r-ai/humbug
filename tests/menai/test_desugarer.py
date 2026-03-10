"""Tests for the Menai desugarer.

This module tests the desugarer's ability to transform complex constructs
(like match expressions) into simpler core language constructs.
"""

import pytest

from menai.menai_ast_desugarer import MenaiASTDesugarer
from menai.menai_lexer import MenaiLexer
from menai.menai_ast_builder import MenaiASTBuilder
from menai.menai_ast_semantic_analyzer import MenaiASTSemanticAnalyzer
from menai.menai_ast import (
    MenaiASTNode, MenaiASTSymbol, MenaiASTList, MenaiASTInteger, MenaiASTString, MenaiASTBoolean
)
from menai.menai_error import MenaiEvalError


def parse_and_analyze_expression(expr_str: str) -> MenaiASTNode:
    """Helper to parse and semantically analyze an expression string into AST."""
    lexer = MenaiLexer()
    tokens = lexer.lex(expr_str)
    ast_builder = MenaiASTBuilder()
    ast = ast_builder.build(tokens, expr_str)

    # Run semantic analysis before desugaring
    analyzer = MenaiASTSemanticAnalyzer()
    return analyzer.analyze(ast)


class TestDesugarerBasic:
    """Test basic desugarer functionality."""

    def test_literals_pass_through(self):
        """Test that literals pass through unchanged."""
        desugarer = MenaiASTDesugarer()

        # Numbers
        num = MenaiASTInteger(42)
        assert desugarer.desugar(num) == num

        # Strings
        string = MenaiASTString("hello")
        assert desugarer.desugar(string) == string

        # Booleans
        bool_true = MenaiASTBoolean(True)
        assert desugarer.desugar(bool_true) == bool_true

    def test_symbols_pass_through(self):
        """Test that symbols pass through unchanged."""
        desugarer = MenaiASTDesugarer()

        symbol = MenaiASTSymbol('x')
        assert desugarer.desugar(symbol) == symbol

    def test_empty_list_passes_through(self):
        """Test that empty lists pass through unchanged."""
        desugarer = MenaiASTDesugarer()

        empty = MenaiASTList(())
        assert desugarer.desugar(empty) == empty

    def test_quote_not_desugared(self):
        """Test that quoted expressions are not desugared."""
        desugarer = MenaiASTDesugarer()

        # (quote (match x (42 "found")))
        expr = parse_and_analyze_expression("(quote (match x (42 \"found\")))")
        result = desugarer.desugar(expr)

        # Should remain unchanged
        assert isinstance(result, MenaiASTList)
        assert result.first().name == 'quote'


class TestDesugarerCoreConstructs:
    """Test desugaring of core constructs (if, let, lambda)."""

    def test_if_desugars_children(self):
        """Test that if expressions desugar their children."""
        desugarer = MenaiASTDesugarer()

        # (if (match x (42 #t) (_ #f)) "yes" "no")
        # The match on symbol x produces if directly (no let wrapper).
        expr = parse_and_analyze_expression('(if (match x (42 #t) (_ #f)) "yes" "no")')
        result = desugarer.desugar(expr)

        # Result should be an if
        assert isinstance(result, MenaiASTList)
        assert result.first().name == 'if'

        # Condition should be desugared — match on symbol produces if directly
        condition = result.elements[1]
        assert isinstance(condition, MenaiASTList)
        assert condition.first().name == 'if'

    def test_let_desugars_children(self):
        """Test that let expressions desugar their children."""
        desugarer = MenaiASTDesugarer()

        # (let ((x (match y (42 1) (_ 0)))) x)
        # Match on symbol y produces if directly (no let wrapper).
        expr = parse_and_analyze_expression('(let ((x (match y (42 1) (_ 0)))) x)')
        result = desugarer.desugar(expr)

        # Result should be a let
        assert isinstance(result, MenaiASTList)
        assert result.first().name == 'let'

        # Binding value should be desugared — match on symbol produces if directly
        bindings = result.elements[1]
        binding = bindings.elements[0]
        value = binding.elements[1]
        assert isinstance(value, MenaiASTList)
        assert value.first().name == 'if'

    def test_lambda_desugars_body(self):
        """Test that lambda expressions desugar their body."""
        desugarer = MenaiASTDesugarer()

        # (lambda (x) (match x (42 "found") (_ "not found")))
        # Match on symbol x produces if directly (no let wrapper).
        expr = parse_and_analyze_expression('(lambda (x) (match x (42 "found") (_ "not found")))')
        result = desugarer.desugar(expr)

        # Result should be a lambda
        assert isinstance(result, MenaiASTList)
        assert result.first().name == 'lambda'

        # Body should be desugared — match on symbol produces if directly
        body = result.elements[2]
        assert isinstance(body, MenaiASTList)
        assert body.first().name == 'if'

    def test_function_call_desugars_all_elements(self):
        """Test that function calls desugar all elements."""
        desugarer = MenaiASTDesugarer()

        # (+ (match x (42 1) (_ 0)) (match y (1 10) (_ 0)))
        # Both matches on symbols produce if directly (no let wrapper).
        expr = parse_and_analyze_expression('(+ (match x (42 1) (_ 0)) (match y (1 10) (_ 0)))')
        result = desugarer.desugar(expr)

        # Result should be a call to +
        assert isinstance(result, MenaiASTList)
        assert result.first().name == '+'

        # Both arguments should be desugared — match on symbol produces if directly
        arg1 = result.elements[1]
        arg2 = result.elements[2]
        assert isinstance(arg1, MenaiASTList)
        assert arg1.first().name == 'if'
        assert isinstance(arg2, MenaiASTList)
        assert arg2.first().name == 'if'


class TestDesugarerMatchLiteral:
    """Test desugaring of literal patterns in match expressions."""

    def test_match_number_literal(self):
        """Test desugaring of number literal pattern."""
        desugarer = MenaiASTDesugarer()

        # (match x (42 "found") (_ "default"))
        # Scrutinee is a symbol — no temp binding needed, result is if directly.
        expr = parse_and_analyze_expression('(match x (42 "found") (_ "default"))')
        result = desugarer.desugar(expr)

        assert isinstance(result, MenaiASTList)
        assert result.first().name == 'if'

    def test_match_string_literal(self):
        """Test desugaring of string literal pattern."""
        desugarer = MenaiASTDesugarer()

        # (match x ("hello" "greeting") (_ "other"))
        # Scrutinee is a symbol — result is if directly.
        expr = parse_and_analyze_expression('(match x ("hello" "greeting") (_ "other"))')
        result = desugarer.desugar(expr)

        assert isinstance(result, MenaiASTList)
        assert result.first().name == 'if'

    def test_match_boolean_literal(self):
        """Test desugaring of boolean literal pattern."""
        desugarer = MenaiASTDesugarer()

        # (match x (#t "true") (#f "false"))
        # Scrutinee is a symbol — result is if directly.
        expr = parse_and_analyze_expression('(match x (#t "true") (#f "false"))')
        result = desugarer.desugar(expr)

        assert isinstance(result, MenaiASTList)
        assert result.first().name == 'if'


class TestDesugarerMatchVariable:
    """Test desugaring of variable patterns in match expressions."""

    def test_match_variable_binding(self):
        """Test desugaring of variable binding pattern."""
        desugarer = MenaiASTDesugarer()

        # (match x (n n))
        # Scrutinee is a symbol. Variable pattern always matches so (if #t ...)
        # is eliminated. Result is directly: (let ((n x)) n).
        expr = parse_and_analyze_expression('(match x (n n))')
        result = desugarer.desugar(expr)

        assert isinstance(result, MenaiASTList)
        assert result.first().name == 'let'
        # The binding should bind n directly to x (no temp var)
        binding = result.elements[1].elements[0]
        assert isinstance(binding, MenaiASTList)
        assert isinstance(binding.elements[0], MenaiASTSymbol)
        assert binding.elements[0].name == 'n'
        assert isinstance(binding.elements[1], MenaiASTSymbol)
        assert binding.elements[1].name == 'x'

    def test_match_wildcard(self):
        """Test desugaring of wildcard pattern."""
        desugarer = MenaiASTDesugarer()

        # (match x (_ "anything"))
        # Scrutinee is a symbol, wildcard always matches — result is the
        # string directly with no wrapping let or if.
        expr = parse_and_analyze_expression('(match x (_ "anything"))')
        result = desugarer.desugar(expr)

        assert isinstance(result, MenaiASTString)
        assert result.value == "anything"


class TestDesugarerMatchType:
    """Test desugaring of type patterns in match expressions."""

    def test_match_type_pattern(self):
        """Test desugaring of type pattern."""
        desugarer = MenaiASTDesugarer()

        # (match x ((? number? n) n) (_ "not a number"))
        # Scrutinee is a symbol — result is if directly.
        expr = parse_and_analyze_expression('(match x ((? number? n) n) (_ "not a number"))')
        result = desugarer.desugar(expr)

        assert isinstance(result, MenaiASTList)
        assert result.first().name == 'if'

        # Condition should be (number? x)
        condition = result.elements[1]
        assert isinstance(condition, MenaiASTList)
        assert condition.first().name == 'number?'

        # Then branch should bind n to x
        then_branch = result.elements[2]
        assert isinstance(then_branch, MenaiASTList)
        assert then_branch.first().name == 'let'

    def test_match_type_pattern_with_wildcard(self):
        """Test desugaring of type pattern with wildcard."""
        desugarer = MenaiASTDesugarer()

        # (match x ((? string? _) "is string") (_ "not string"))
        # Scrutinee is a symbol — result is if directly.
        expr = parse_and_analyze_expression('(match x ((? string? _) "is string") (_ "not string"))')
        result = desugarer.desugar(expr)

        assert isinstance(result, MenaiASTList)
        assert result.first().name == 'if'

        condition = result.elements[1]
        assert isinstance(condition, MenaiASTList)
        assert condition.first().name == '$string?'

        # Then branch should not have a let (no binding for _)
        then_branch = result.elements[2]
        assert isinstance(then_branch, MenaiASTString)
        assert then_branch.value == "is string"


class TestDesugarerMatchList:
    """Test desugaring of list patterns in match expressions."""

    def test_match_empty_list(self):
        """Test desugaring of empty list pattern."""
        desugarer = MenaiASTDesugarer()

        # (match x (() "empty") (_ "not empty"))
        # Scrutinee is a symbol — result is if directly.
        expr = parse_and_analyze_expression('(match x (() "empty") (_ "not empty"))')
        result = desugarer.desugar(expr)

        assert isinstance(result, MenaiASTList)
        assert result.first().name == 'if'
        condition = result.elements[1]
        assert isinstance(condition, MenaiASTList)
        assert condition.first().name == '$list-null?'

    def test_match_fixed_list_simple(self):
        """Test desugaring of simple fixed-length list pattern."""
        desugarer = MenaiASTDesugarer()

        # (match x ((a b) (list a b)) (_ "wrong"))
        # Scrutinee is a symbol — result is if directly.
        expr = parse_and_analyze_expression('(match x ((a b) (list a b)) (_ "wrong"))')
        result = desugarer.desugar(expr)

        assert isinstance(result, MenaiASTList)
        assert result.first().name == 'if'

        condition = result.elements[1]
        assert isinstance(condition, MenaiASTList)
        assert condition.first().name == 'if'

        list_test = condition.elements[1]
        assert isinstance(list_test, MenaiASTList)
        assert list_test.first().name == '$list?'

        then_branch = condition.elements[2]
        assert isinstance(then_branch, MenaiASTList)
        assert isinstance(then_branch.elements[1], MenaiASTList)
        assert then_branch.elements[1].first().name == '$list-length'

    def test_match_fixed_list_with_literals(self):
        """Test desugaring of fixed-length list with literal patterns."""
        desugarer = MenaiASTDesugarer()

        # (match x ((1 2 3) "found") (_ "not found"))
        # Scrutinee is a symbol — result is if directly.
        expr = parse_and_analyze_expression('(match x ((1 2 3) "found") (_ "not found"))')
        result = desugarer.desugar(expr)

        assert isinstance(result, MenaiASTList)
        assert result.first().name == 'if'


class TestDesugarerMatchCons:
    """Test desugaring of cons patterns in match expressions."""

    def test_match_cons_simple(self):
        """Test desugaring of simple cons pattern."""
        desugarer = MenaiASTDesugarer()

        # (match x ((head . tail) (list head tail)) (_ "not list"))
        # Scrutinee is a symbol — result is if directly.
        expr = parse_and_analyze_expression('(match x ((head . tail) (list head tail)) (_ "not list"))')
        result = desugarer.desugar(expr)

        assert isinstance(result, MenaiASTList)
        assert result.first().name == 'if'

        condition = result.elements[1]
        assert isinstance(condition, MenaiASTList)
        assert condition.first().name == 'if'
        assert condition.elements[1].first().name == '$list?'

    def test_match_cons_multiple_heads(self):
        """Test desugaring of cons pattern with multiple head elements."""
        desugarer = MenaiASTDesugarer()

        # (match x ((a b . rest) (list a b rest)) (_ "not list"))
        # Scrutinee is a symbol — result is if directly.
        expr = parse_and_analyze_expression('(match x ((a b . rest) (list a b rest)) (_ "not list"))')
        result = desugarer.desugar(expr)

        assert isinstance(result, MenaiASTList)
        assert result.first().name == 'if'


class TestDesugarerMatchNested:
    """Test desugaring of nested patterns in match expressions."""

    def test_match_nested_list(self):
        """Test desugaring of nested list pattern."""
        desugarer = MenaiASTDesugarer()

        # (match x (((a b)) (list a b)) (_ "wrong"))
        # Scrutinee is a symbol — result is if directly.
        expr = parse_and_analyze_expression('(match x (((a b)) (list a b)) (_ "wrong"))')
        result = desugarer.desugar(expr)

        assert isinstance(result, MenaiASTList)
        assert result.first().name == 'if'

    def test_match_nested_type(self):
        """Test desugaring of nested type pattern."""
        desugarer = MenaiASTDesugarer()

        # (match x (((? number? n)) n) (_ "not list with number"))
        # Scrutinee is a symbol — result is if directly.
        expr = parse_and_analyze_expression('(match x (((? number? n)) n) (_ "not list with number"))')
        result = desugarer.desugar(expr)

        assert isinstance(result, MenaiASTList)
        assert result.first().name == 'if'


class TestDesugarerMatchMultipleClauses:
    """Test desugaring of match with multiple clauses."""

    def test_match_multiple_clauses_simple(self):
        """Test desugaring of match with multiple simple clauses."""
        desugarer = MenaiASTDesugarer()

        # (match x (1 "one") (2 "two") (3 "three") (_ "other"))
        # Scrutinee is a symbol — result is if directly.
        expr = parse_and_analyze_expression('(match x (1 "one") (2 "two") (3 "three") (_ "other"))')
        result = desugarer.desugar(expr)

        assert isinstance(result, MenaiASTList)
        assert result.first().name == 'if'

        # The else branch of the integer type guard is the wildcard result
        # "other" directly — (if #t "other" error) is eliminated by the desugarer.
        else_branch = result.elements[3]
        assert isinstance(else_branch, MenaiASTString)
        assert else_branch.value == "other"

    def test_match_multiple_clauses_complex(self):
        """Test desugaring of match with complex multiple clauses."""
        desugarer = MenaiASTDesugarer()

        # (match x ((? number? n) n) ((? string? s) s) (() "empty") (_ "other"))
        # Scrutinee is a symbol — result is if directly.
        expr = parse_and_analyze_expression('(match x ((? number? n) n) ((? string? s) s) (() "empty") (_ "other"))')
        result = desugarer.desugar(expr)

        assert isinstance(result, MenaiASTList)
        assert result.first().name == 'if'


class TestDesugarerMatchErrors:
    """Test error handling in semantic analyzer (errors caught before desugaring)."""

    def test_match_no_clauses(self):
        """Test error when match has no clauses."""
        # Errors should be caught by semantic analyzer, not desugarer
        analyzer = MenaiASTSemanticAnalyzer()
        lexer = MenaiLexer()
        ast_builder = MenaiASTBuilder()
        expr = ast_builder.build(lexer.lex('(match x)'), '(match x)')

        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            analyzer.analyze(expr)

    def test_match_invalid_clause(self):
        """Test error when match clause is invalid."""
        analyzer = MenaiASTSemanticAnalyzer()
        lexer = MenaiLexer()
        ast_builder = MenaiASTBuilder()
        expr = ast_builder.build(lexer.lex('(match x (42))'), '(match x (42))')

        with pytest.raises(MenaiEvalError, match="wrong number of elements"):
            analyzer.analyze(expr)

    def test_cons_pattern_dot_at_start(self):
        """Test error when cons pattern has dot at start."""
        analyzer = MenaiASTSemanticAnalyzer()
        lexer = MenaiLexer()
        code = '(match x ((. tail) "bad") (_ "other"))'
        ast_builder = MenaiASTBuilder()
        expr = ast_builder.build(lexer.lex(code), code)

        with pytest.raises(MenaiEvalError, match="dot at beginning"):
            analyzer.analyze(expr)

    def test_cons_pattern_dot_at_end(self):
        """Test error when cons pattern has dot at end."""
        analyzer = MenaiASTSemanticAnalyzer()
        lexer = MenaiLexer()
        code = '(match x ((head .) "bad") (_ "other"))'
        ast_builder = MenaiASTBuilder()
        expr = ast_builder.build(lexer.lex(code), code)

        with pytest.raises(MenaiEvalError, match="dot at end"):
            analyzer.analyze(expr)

    def test_cons_pattern_multiple_after_dot(self):
        """Test error when cons pattern has multiple elements after dot."""
        analyzer = MenaiASTSemanticAnalyzer()
        lexer = MenaiLexer()
        code = '(match x ((head . a b) "bad") (_ "other"))'
        ast_builder = MenaiASTBuilder()
        expr = ast_builder.build(lexer.lex(code), code)

        with pytest.raises(MenaiEvalError, match="multiple elements after dot"):
            analyzer.analyze(expr)


class TestDesugarerTempVariables:
    """Test temporary variable generation."""

    def test_temp_variables_unique(self):
        """Test that temporary variables are unique for compound scrutinees."""
        desugarer = MenaiASTDesugarer()

        # Match on compound expressions — these still generate temp vars.
        expr1 = parse_and_analyze_expression('(match (foo x) (42 "found") (_ "other"))')
        result1 = desugarer.desugar(expr1)

        expr2 = parse_and_analyze_expression('(match (bar y) (1 "one") (_ "other"))')
        result2 = desugarer.desugar(expr2)

        def get_temp_var(result):
            # result is (let ((temp val)) body)
            assert isinstance(result, MenaiASTList)
            assert result.first().name == 'let'
            bindings = result.elements[1]
            binding = bindings.elements[0]
            return binding.elements[0].name

        temp1 = get_temp_var(result1)
        temp2 = get_temp_var(result2)

        assert temp1 != temp2
        assert temp1.startswith('#:match-tmp-')
        assert temp2.startswith('#:match-tmp-')

    def test_nested_match_temp_variables(self):
        """Test that nested matches on symbols produce no temp variables."""
        desugarer = MenaiASTDesugarer()

        # Both outer match (on x) and inner match (on n) are symbol scrutinees.
        # Neither generates a temp binding — both produce if directly.
        expr = parse_and_analyze_expression('(match x ((? number? n) (match n (42 "found") (_ "not 42"))) (_ "not number"))')
        result = desugarer.desugar(expr)

        assert isinstance(result, MenaiASTList)
        assert result.first().name == 'if'


class TestDesugarerIntegration:
    """Integration tests for desugarer with full evaluation."""

    def test_desugared_match_evaluates_correctly(self, menai):
        """Test that desugared match expressions evaluate correctly."""
        desugarer = MenaiASTDesugarer()

        # (match 42 (42 "found") (_ "not found"))
        # Scrutinee is a literal integer — temp binding is generated.
        expr = parse_and_analyze_expression('(match 42 (42 "found") (_ "not found"))')
        result = desugarer.desugar(expr)

        assert isinstance(result, MenaiASTList)
        assert result.first().name == 'let'
