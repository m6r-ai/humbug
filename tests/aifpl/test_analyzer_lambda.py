"""Tests for AIFPL Analyzer - Lambda expression analysis.

These tests verify that the analyzer correctly analyzes lambda expressions,
including closure analysis, free variable detection, and recursion handling.
"""

import pytest
from aifpl.aifpl_analyzer import AIFPLAnalyzer
from aifpl.aifpl_analysis_ir import (
    AnalyzedLambda,
    AnalyzedLiteral,
    AnalyzedVariable,
    AnalyzedCall,
    AnalyzedLet,
)
from aifpl.aifpl_value import (
    AIFPLNumber,
    AIFPLString,
    AIFPLBoolean,
    AIFPLSymbol,
    AIFPLList,
)
from aifpl.aifpl_error import AIFPLEvalError


class TestAnalyzeLambdaBasic:
    """Test basic lambda expression analysis."""
    
    def test_analyze_identity_lambda(self):
        """Test analyzing (lambda (x) x)."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("lambda"),
            AIFPLList((AIFPLSymbol("x"),)),
            AIFPLSymbol("x")
        ))
        
        result = analyzer.analyze(expr)
        
        assert isinstance(result, AnalyzedLambda)
        assert result.params == ["x"]
        assert isinstance(result.body, AnalyzedVariable)
        assert len(result.free_vars) == 0
        assert not result.is_recursive
    
    def test_lambda_with_multiple_params(self):
        """Test analyzing (lambda (x y z) (+ x y z))."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("lambda"),
            AIFPLList((
                AIFPLSymbol("x"),
                AIFPLSymbol("y"),
                AIFPLSymbol("z")
            )),
            AIFPLList((
                AIFPLSymbol("+"),
                AIFPLSymbol("x"),
                AIFPLSymbol("y"),
                AIFPLSymbol("z")
            ))
        ))
        
        result = analyzer.analyze(expr)
        
        assert result.params == ["x", "y", "z"]
        assert isinstance(result.body, AnalyzedCall)
        assert len(result.free_vars) == 0
    
    def test_lambda_with_no_params(self):
        """Test analyzing (lambda () 42)."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("lambda"),
            AIFPLList(()),
            AIFPLNumber(42)
        ))
        
        result = analyzer.analyze(expr)
        
        assert result.params == []
        assert isinstance(result.body, AnalyzedLiteral)
    
    def test_lambda_added_to_code_objects(self):
        """Test that lambda is added to code_objects list."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("lambda"),
            AIFPLList((AIFPLSymbol("x"),)),
            AIFPLSymbol("x")
        ))
        
        result = analyzer.analyze(expr)
        
        assert result.code_index >= 0
        assert len(analyzer.code_objects) > 0
        assert analyzer.code_objects[result.code_index] == result


class TestLambdaFreeVariables:
    """Test free variable detection in lambdas."""
    
    def test_lambda_with_no_free_vars(self):
        """Test lambda with no free variables."""
        analyzer = AIFPLAnalyzer()
        
        # (lambda (x) (+ x 1))
        expr = AIFPLList((
            AIFPLSymbol("lambda"),
            AIFPLList((AIFPLSymbol("x"),)),
            AIFPLList((
                AIFPLSymbol("+"),
                AIFPLSymbol("x"),
                AIFPLNumber(1)
            ))
        ))
        
        result = analyzer.analyze(expr)
        
        assert len(result.free_vars) == 0
    
    def test_lambda_with_free_variable(self):
        """Test lambda with free variable."""
        analyzer = AIFPLAnalyzer()
        
        # First create a let binding to make 'y' a local variable
        # (let ((y 10)) (lambda (x) (+ x y)))
        expr = AIFPLList((
            AIFPLSymbol("let"),
            AIFPLList((
                AIFPLList((AIFPLSymbol("y"), AIFPLNumber(10))),
            )),
            AIFPLList((
                AIFPLSymbol("lambda"),
                AIFPLList((AIFPLSymbol("x"),)),
                AIFPLList((
                    AIFPLSymbol("+"),
                    AIFPLSymbol("x"),
                    AIFPLSymbol("y")
                ))
            ))
        ))
        
        result = analyzer.analyze(expr)
        
        # Result is a let, lambda is in the body
        assert isinstance(result, AnalyzedLet)
        lambda_result = result.body
        assert isinstance(lambda_result, AnalyzedLambda)
        assert "y" in lambda_result.free_vars
    
    def test_lambda_with_multiple_free_vars(self):
        """Test lambda with multiple free variables."""
        analyzer = AIFPLAnalyzer()
        
        # (let ((a 1) (b 2)) (lambda (x) (+ x a b)))
        expr = AIFPLList((
            AIFPLSymbol("let"),
            AIFPLList((
                AIFPLList((AIFPLSymbol("a"), AIFPLNumber(1))),
                AIFPLList((AIFPLSymbol("b"), AIFPLNumber(2)))
            )),
            AIFPLList((
                AIFPLSymbol("lambda"),
                AIFPLList((AIFPLSymbol("x"),)),
                AIFPLList((
                    AIFPLSymbol("+"),
                    AIFPLSymbol("x"),
                    AIFPLSymbol("a"),
                    AIFPLSymbol("b")
                ))
            ))
        ))
        
        result = analyzer.analyze(expr)
        lambda_result = result.body
        
        assert "a" in lambda_result.free_vars
        assert "b" in lambda_result.free_vars
        assert "x" not in lambda_result.free_vars  # x is a parameter
    
    def test_nested_lambda_free_vars(self):
        """Test nested lambdas with free variables."""
        analyzer = AIFPLAnalyzer()
        
        # (let ((y 10)) (lambda (x) (lambda (z) (+ x y z))))
        inner_lambda = AIFPLList((
            AIFPLSymbol("lambda"),
            AIFPLList((AIFPLSymbol("z"),)),
            AIFPLList((
                AIFPLSymbol("+"),
                AIFPLSymbol("x"),
                AIFPLSymbol("y"),
                AIFPLSymbol("z")
            ))
        ))
        
        outer_lambda = AIFPLList((
            AIFPLSymbol("lambda"),
            AIFPLList((AIFPLSymbol("x"),)),
            inner_lambda
        ))
        
        expr = AIFPLList((
            AIFPLSymbol("let"),
            AIFPLList((
                AIFPLList((AIFPLSymbol("y"), AIFPLNumber(10))),
            )),
            outer_lambda
        ))
        
        result = analyzer.analyze(expr)
        outer = result.body
        inner = outer.body
        
        assert isinstance(outer, AnalyzedLambda)
        assert isinstance(inner, AnalyzedLambda)
        assert "y" in outer.free_vars  # y is free in outer
        assert "x" in inner.free_vars  # x is free in inner (from outer)
        assert "y" in inner.free_vars  # y is free in inner too


class TestLambdaRecursion:
    """Test recursion detection in lambdas."""
    
    def test_non_recursive_lambda(self):
        """Test that non-recursive lambda is not marked recursive."""
        analyzer = AIFPLAnalyzer()
        
        # (let ((f (lambda (x) (+ x 1)))) (f 5))
        expr = AIFPLList((
            AIFPLSymbol("let"),
            AIFPLList((
                AIFPLList((
                    AIFPLSymbol("f"),
                    AIFPLList((
                        AIFPLSymbol("lambda"),
                        AIFPLList((AIFPLSymbol("x"),)),
                        AIFPLList((
                            AIFPLSymbol("+"),
                            AIFPLSymbol("x"),
                            AIFPLNumber(1)
                        ))
                    ))
                )),
            )),
            AIFPLList((AIFPLSymbol("f"), AIFPLNumber(5)))
        ))
        
        result = analyzer.analyze(expr)
        lambda_result = result.bindings[0][1]
        
        assert isinstance(lambda_result, AnalyzedLambda)
        assert not lambda_result.is_recursive
    
    def test_recursive_lambda_detected(self):
        """Test that recursive lambda is detected."""
        analyzer = AIFPLAnalyzer()
        
        # (let ((fact (lambda (n) (fact (- n 1))))) (fact 5))
        expr = AIFPLList((
            AIFPLSymbol("let"),
            AIFPLList((
                AIFPLList((
                    AIFPLSymbol("fact"),
                    AIFPLList((
                        AIFPLSymbol("lambda"),
                        AIFPLList((AIFPLSymbol("n"),)),
                        AIFPLList((
                            AIFPLSymbol("fact"),
                            AIFPLList((
                                AIFPLSymbol("-"),
                                AIFPLSymbol("n"),
                                AIFPLNumber(1)
                            ))
                        ))
                    ))
                )),
            )),
            AIFPLList((AIFPLSymbol("fact"), AIFPLNumber(5)))
        ))
        
        result = analyzer.analyze(expr)
        lambda_result = result.bindings[0][1]
        
        assert isinstance(lambda_result, AnalyzedLambda)
        assert lambda_result.is_recursive
        assert "fact" in lambda_result.free_vars


class TestLambdaTailPosition:
    """Test tail position handling in lambdas."""
    
    def test_lambda_body_in_tail_position(self):
        """Test that lambda body is always in tail position."""
        analyzer = AIFPLAnalyzer()
        
        # (let ((f (lambda (n) (f (- n 1))))) (f 5))
        # The call to f in lambda body should be tail call
        expr = AIFPLList((
            AIFPLSymbol("let"),
            AIFPLList((
                AIFPLList((
                    AIFPLSymbol("f"),
                    AIFPLList((
                        AIFPLSymbol("lambda"),
                        AIFPLList((AIFPLSymbol("n"),)),
                        AIFPLList((
                            AIFPLSymbol("f"),
                            AIFPLList((
                                AIFPLSymbol("-"),
                                AIFPLSymbol("n"),
                                AIFPLNumber(1)
                            ))
                        ))
                    ))
                )),
            )),
            AIFPLList((AIFPLSymbol("f"), AIFPLNumber(5)))
        ))
        
        result = analyzer.analyze(expr)
        lambda_result = result.bindings[0][1]
        
        # Body should be a call in tail position
        assert isinstance(lambda_result.body, AnalyzedCall)
        assert lambda_result.body.is_tail_call


class TestLambdaScopeManagement:
    """Test scope management in lambdas."""
    
    def test_lambda_parameters_in_scope(self):
        """Test that lambda parameters are in scope in body."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("lambda"),
            AIFPLList((AIFPLSymbol("x"),)),
            AIFPLSymbol("x")
        ))
        
        result = analyzer.analyze(expr)
        
        # Body should reference x as a local variable (parameter)
        assert isinstance(result.body, AnalyzedVariable)
        assert result.body.var_type == "local"
    
    def test_lambda_scope_restored(self):
        """Test that scope is restored after lambda analysis."""
        analyzer = AIFPLAnalyzer()
        
        # Before lambda
        initial_depth = analyzer.symbol_table.get_current_depth()
        
        expr = AIFPLList((
            AIFPLSymbol("lambda"),
            AIFPLList((AIFPLSymbol("x"),)),
            AIFPLSymbol("x")
        ))
        
        result = analyzer.analyze(expr)
        
        # After lambda, should be back to initial depth
        assert analyzer.symbol_table.get_current_depth() == initial_depth


class TestLambdaInstructionCounting:
    """Test instruction count for lambdas."""
    
    def test_lambda_instruction_count(self):
        """Test that lambda has instruction count of 1."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("lambda"),
            AIFPLList((AIFPLSymbol("x"),)),
            AIFPLSymbol("x")
        ))
        
        result = analyzer.analyze(expr)
        
        # Lambda creation is just MAKE_CLOSURE
        assert result.instruction_count == 1


class TestLambdaErrorHandling:
    """Test error handling for lambda expressions."""
    
    def test_lambda_with_no_arguments(self):
        """Test (lambda) raises error."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((AIFPLSymbol("lambda"),))
        
        with pytest.raises(AIFPLEvalError, match="structure is incorrect"):
            analyzer.analyze(expr)
    
    def test_lambda_with_one_argument(self):
        """Test (lambda (x)) raises error."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("lambda"),
            AIFPLList((AIFPLSymbol("x"),))
        ))
        
        with pytest.raises(AIFPLEvalError, match="structure is incorrect"):
            analyzer.analyze(expr)
    
    def test_lambda_params_not_list(self):
        """Test lambda with non-list parameters raises error."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("lambda"),
            AIFPLNumber(42),  # Should be a list
            AIFPLNumber(1)
        ))
        
        with pytest.raises(AIFPLEvalError, match="parameters must be a list"):
            analyzer.analyze(expr)
    
    def test_lambda_param_not_symbol(self):
        """Test lambda with non-symbol parameter raises error."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("lambda"),
            AIFPLList((AIFPLNumber(42),)),  # Should be symbol
            AIFPLNumber(1)
        ))
        
        with pytest.raises(AIFPLEvalError, match="parameter .* must be a symbol"):
            analyzer.analyze(expr)
    
    def test_lambda_duplicate_params(self):
        """Test lambda with duplicate parameters raises error."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("lambda"),
            AIFPLList((
                AIFPLSymbol("x"),
                AIFPLSymbol("x")  # Duplicate
            )),
            AIFPLSymbol("x")
        ))
        
        with pytest.raises(AIFPLEvalError, match="must be unique"):
            analyzer.analyze(expr)


class TestLambdaProperties:
    """Test various properties of analyzed lambdas."""
    
    def test_expr_type_is_lambda(self):
        """Test that expr_type is 'lambda'."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("lambda"),
            AIFPLList((AIFPLSymbol("x"),)),
            AIFPLSymbol("x")
        ))
        
        result = analyzer.analyze(expr)
        
        assert result.expr_type == "lambda"
    
    def test_source_expr_preserved(self):
        """Test that source expression is preserved."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("lambda"),
            AIFPLList((AIFPLSymbol("x"),)),
            AIFPLSymbol("x")
        ))
        
        result = analyzer.analyze(expr)
        
        assert result.source_expr == expr
    
    def test_code_index_valid(self):
        """Test that code_index is valid."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("lambda"),
            AIFPLList((AIFPLSymbol("x"),)),
            AIFPLSymbol("x")
        ))
        
        result = analyzer.analyze(expr)
        
        assert result.code_index >= 0
        assert result.code_index < len(analyzer.code_objects)


class TestComplexLambdaScenarios:
    """Test complex lambda scenarios."""
    
    def test_lambda_in_call(self):
        """Test lambda used directly in call: ((lambda (x) (+ x 1)) 5)."""
        analyzer = AIFPLAnalyzer()
        
        lambda_expr = AIFPLList((
            AIFPLSymbol("lambda"),
            AIFPLList((AIFPLSymbol("x"),)),
            AIFPLList((
                AIFPLSymbol("+"),
                AIFPLSymbol("x"),
                AIFPLNumber(1)
            ))
        ))
        
        expr = AIFPLList((
            lambda_expr,
            AIFPLNumber(5)
        ))
        
        result = analyzer.analyze(expr)
        
        # Result is a call
        assert isinstance(result, AnalyzedCall)
        # Function is a lambda
        assert isinstance(result.func, AnalyzedLambda)
    
    def test_higher_order_lambda(self):
        """Test lambda that returns lambda."""
        analyzer = AIFPLAnalyzer()
        
        # (lambda (x) (lambda (y) (+ x y)))
        inner_lambda = AIFPLList((
            AIFPLSymbol("lambda"),
            AIFPLList((AIFPLSymbol("y"),)),
            AIFPLList((
                AIFPLSymbol("+"),
                AIFPLSymbol("x"),
                AIFPLSymbol("y")
            ))
        ))
        
        outer_lambda = AIFPLList((
            AIFPLSymbol("lambda"),
            AIFPLList((AIFPLSymbol("x"),)),
            inner_lambda
        ))
        
        result = analyzer.analyze(outer_lambda)
        
        assert isinstance(result, AnalyzedLambda)
        assert isinstance(result.body, AnalyzedLambda)
        # Inner lambda should have x as free variable
        assert "x" in result.body.free_vars
