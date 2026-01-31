"""Tests for AIFPL static analyzer."""

import pytest
from aifpl.aifpl_lexer import AIFPLLexer
from aifpl.aifpl_parser import AIFPLParser
from aifpl.aifpl_semantic_analyzer import AIFPLSemanticAnalyzer
from aifpl.aifpl_desugarer import AIFPLDesugarer
from aifpl.aifpl_analyzer import AIFPLAnalyzer


@pytest.fixture
def analyzer():
    """Create analyzer with full pipeline."""
    lexer = AIFPLLexer()
    semantic = AIFPLSemanticAnalyzer()
    desugarer = AIFPLDesugarer()
    analyzer = AIFPLAnalyzer()

    def analyze(code):
        tokens = lexer.lex(code)
        ast = AIFPLParser(tokens, code).parse()
        ast = semantic.analyze(ast)
        ast = desugarer.desugar(ast)
        return analyzer.analyze(ast), ast

    return analyze


class TestLambdaAnalysis:
    """Test lambda expression analysis."""

    def test_lambda_no_free_vars(self, analyzer):
        """Lambda with no free variables."""
        result, ast = analyzer("(lambda (x) x)")

        # Should have one lambda
        assert len(result.lambda_info) == 1

        info = list(result.lambda_info.values())[0]
        assert info.params == ['x']
        assert info.free_vars == []
        assert not info.is_self_recursive
        assert len(info.mutually_recursive_with) == 0

    def test_lambda_with_free_vars(self, analyzer):
        """Lambda capturing free variables."""
        result, ast = analyzer("(lambda (x) (+ x y z))")

        info = list(result.lambda_info.values())[0]
        assert info.params == ['x']
        # y and z are free (+ is a builtin, handled differently)
        assert set(info.free_vars) == {'y', 'z'}

    def test_nested_lambda(self, analyzer):
        """Nested lambda expressions."""
        result, ast = analyzer("(lambda (x) (lambda (y) (+ x y z)))")

        # Should have two lambdas
        assert len(result.lambda_info) == 2

        # Find outer and inner lambdas
        outer = None
        inner = None
        for info in result.lambda_info.values():
            if info.params == ['x']:
                outer = info
            elif info.params == ['y']:
                inner = info

        assert outer is not None
        assert inner is not None

        # Outer lambda captures z
        assert set(outer.free_vars) == {'z'}

        # Inner lambda captures x (from outer) and z
        assert set(inner.free_vars) == {'x', 'z'}

    def test_lambda_in_let(self, analyzer):
        """Lambda bound in let (not recursive)."""
        result, ast = analyzer("(let ((f (lambda (x) (* x 2)))) (f 5))")

        info = list(result.lambda_info.values())[0]
        assert info.params == ['x']
        assert info.free_vars == []
        # Not in letrec, so no self-recursion tracking
        assert info.self_name is None
        assert not info.is_self_recursive


class TestLetrecAnalysis:
    """Test letrec expression analysis."""

    def test_simple_self_recursion(self, analyzer):
        """Self-recursive function in letrec."""
        code = """
        (letrec ((fact (lambda (n)
                         (if (<= n 1)
                             1
                             (* n (fact (- n 1)))))))
          (fact 5))
        """
        result, ast = analyzer(code)

        # Should have letrec info
        assert len(result.letrec_info) == 1

        letrec_info = list(result.letrec_info.values())[0]
        assert 'fact' in letrec_info.recursive_bindings
        assert len(letrec_info.mutual_recursion_groups) == 0  # Self-recursion, not mutual

        # Lambda should know it's self-recursive
        lambda_info = list(result.lambda_info.values())[0]
        assert lambda_info.self_name == 'fact'
        assert lambda_info.is_self_recursive
        assert len(lambda_info.mutually_recursive_with) == 0
        # Free vars should NOT include 'fact' (it's self-reference)
        assert 'fact' not in lambda_info.free_vars

    def test_mutual_recursion(self, analyzer):
        """Mutually recursive functions."""
        code = """
        (letrec ((even? (lambda (n)
                          (if (= n 0)
                              #t
                              (odd? (- n 1)))))
                 (odd? (lambda (n)
                         (if (= n 0)
                             #f
                             (even? (- n 1))))))
          (even? 10))
        """
        result, ast = analyzer(code)

        letrec_info = list(result.letrec_info.values())[0]

        # Both should be recursive
        assert 'even?' in letrec_info.recursive_bindings
        assert 'odd?' in letrec_info.recursive_bindings

        # Should detect mutual recursion
        assert len(letrec_info.mutual_recursion_groups) == 1
        assert letrec_info.mutual_recursion_groups[0] == {'even?', 'odd?'}

        # Check lambda info
        for lambda_info in result.lambda_info.values():
            if lambda_info.self_name == 'even?':
                assert 'odd?' in lambda_info.mutually_recursive_with
                assert 'odd?' not in lambda_info.free_vars  # Sibling, not free
            elif lambda_info.self_name == 'odd?':
                assert 'even?' in lambda_info.mutually_recursive_with
                assert 'even?' not in lambda_info.free_vars  # Sibling, not free

    def test_non_recursive_letrec(self, analyzer):
        """Letrec with non-recursive bindings."""
        code = "(letrec ((x 5) (y 10)) (+ x y))"
        result, ast = analyzer(code)

        letrec_info = list(result.letrec_info.values())[0]
        assert len(letrec_info.recursive_bindings) == 0
        assert len(letrec_info.mutual_recursion_groups) == 0

    def test_mixed_recursive_non_recursive(self, analyzer):
        """Letrec with both recursive and non-recursive bindings."""
        code = """
        (letrec ((x 5)
                 (fact (lambda (n)
                         (if (<= n 1)
                             x
                             (* n (fact (- n 1)))))))
          (fact 5))
        """
        result, ast = analyzer(code)

        letrec_info = list(result.letrec_info.values())[0]

        # Only fact is recursive
        assert 'fact' in letrec_info.recursive_bindings
        assert 'x' not in letrec_info.recursive_bindings

        # Lambda should capture x as free var
        # Note: x is in mutually_recursive_with because it's a sibling,
        # but it should ALSO be in free_vars since it's not a lambda
        lambda_info = list(result.lambda_info.values())[0]
        assert 'x' in lambda_info.free_vars
        assert lambda_info.is_self_recursive


class TestLetAnalysis:
    """Test regular let expression analysis."""

    def test_simple_let(self, analyzer):
        """Simple let binding."""
        code = "(let ((x 5) (y 10)) (+ x y))"
        result, ast = analyzer(code)

        # Let doesn't create letrec_info
        assert len(result.letrec_info) == 0

    def test_let_with_lambda(self, analyzer):
        """Let binding a lambda."""
        code = "(let ((f (lambda (x) (* x 2)))) (f 5))"
        result, ast = analyzer(code)

        lambda_info = list(result.lambda_info.values())[0]
        # In regular let, no self-recursion tracking
        assert lambda_info.self_name is None
        assert not lambda_info.is_self_recursive


class TestFreeVariableAnalysis:
    """Test free variable detection."""

    def test_nested_scopes(self, analyzer):
        """Free variables through nested scopes."""
        code = """
        (let ((a 1))
          (let ((b 2))
            (lambda (c) (+ a b c d))))
        """
        result, ast = analyzer(code)

        lambda_info = list(result.lambda_info.values())[0]
        # a, b, and d are all free variables that need to be captured
        # a and b are from outer let scopes, d is undefined (will error at runtime)
        # c is a parameter, so not free
        assert set(lambda_info.free_vars) == {'a', 'b', 'd'}

    def test_lambda_in_if(self, analyzer):
        """Lambda in if branch."""
        code = "(if test (lambda (x) (+ x y)) (lambda (x) (* x z)))"
        result, ast = analyzer(code)

        # Should have two lambdas
        assert len(result.lambda_info) == 2

        # Both should capture their respective free vars
        free_vars_sets = [set(info.free_vars) for info in result.lambda_info.values()]
        assert {'y'} in free_vars_sets or {'test', 'y'} in free_vars_sets
        assert {'z'} in free_vars_sets or {'test', 'z'} in free_vars_sets

    def test_closure_over_let(self, analyzer):
        """Lambda closing over let binding."""
        code = """
        (let ((x 10))
          (lambda (y) (+ x y)))
        """
        result, ast = analyzer(code)

        lambda_info = list(result.lambda_info.values())[0]
        assert lambda_info.free_vars == ['x']


class TestComplexCases:
    """Test complex analysis scenarios."""

    def test_three_way_mutual_recursion(self, analyzer):
        """Three mutually recursive functions."""
        code = """
        (letrec ((f (lambda (n) (if (= n 0) 0 (g (- n 1)))))
                 (g (lambda (n) (if (= n 0) 1 (h (- n 1)))))
                 (h (lambda (n) (if (= n 0) 2 (f (- n 1))))))
          (f 10))
        """
        result, ast = analyzer(code)

        letrec_info = list(result.letrec_info.values())[0]

        # All three should be recursive
        assert letrec_info.recursive_bindings == {'f', 'g', 'h'}

        # Should form one mutual recursion group
        assert len(letrec_info.mutual_recursion_groups) == 1
        assert letrec_info.mutual_recursion_groups[0] == {'f', 'g', 'h'}

    def test_nested_letrec(self, analyzer):
        """Nested letrec expressions."""
        code = """
        (letrec ((outer (lambda (n)
                          (letrec ((inner (lambda (m)
                                            (if (= m 0)
                                                n
                                                (inner (- m 1))))))
                            (inner n)))))
          (outer 5))
        """
        result, ast = analyzer(code)

        # Should have two letrec infos (outer and inner)
        assert len(result.letrec_info) == 2

        # Should have two lambdas
        assert len(result.lambda_info) == 2

        # Find the inner lambda
        inner_lambda = None
        for info in result.lambda_info.values():
            if info.self_name == 'inner':
                inner_lambda = info
                break

        assert inner_lambda is not None
        assert inner_lambda.is_self_recursive
        # Should capture 'n' from outer lambda
        assert 'n' in inner_lambda.free_vars
