"""Tests for AIFPL Analyzer - Let expression analysis.

These tests verify that the analyzer correctly analyzes let expressions,
including dependency analysis, recursion detection, and scope management.
"""

import pytest
from aifpl.aifpl_analyzer import AIFPLAnalyzer
from aifpl.aifpl_analysis_ir import (
    AnalyzedLet,
    AnalyzedLiteral,
    AnalyzedVariable,
    AnalyzedCall,
)
from aifpl.aifpl_value import (
    AIFPLNumber,
    AIFPLString,
    AIFPLBoolean,
    AIFPLSymbol,
    AIFPLList,
)
from aifpl.aifpl_error import AIFPLEvalError


class TestAnalyzeLetBasic:
    """Test basic let expression analysis."""
    
    def test_analyze_simple_let(self):
        """Test analyzing (let ((x 5)) x)."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("let"),
            AIFPLList((
                AIFPLList((
                    AIFPLSymbol("x"),
                    AIFPLNumber(5)
                )),
            )),
            AIFPLSymbol("x")
        ))
        
        result = analyzer.analyze(expr)
        
        assert isinstance(result, AnalyzedLet)
        assert len(result.bindings) == 1
        assert result.bindings[0][0] == "x"
        assert isinstance(result.bindings[0][1], AnalyzedLiteral)
        assert isinstance(result.body, AnalyzedVariable)
    
    def test_let_with_multiple_bindings(self):
        """Test analyzing (let ((x 5) (y 10)) (+ x y))."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("let"),
            AIFPLList((
                AIFPLList((AIFPLSymbol("x"), AIFPLNumber(5))),
                AIFPLList((AIFPLSymbol("y"), AIFPLNumber(10)))
            )),
            AIFPLList((
                AIFPLSymbol("+"),
                AIFPLSymbol("x"),
                AIFPLSymbol("y")
            ))
        ))
        
        result = analyzer.analyze(expr)
        
        assert len(result.bindings) == 2
        assert result.bindings[0][0] == "x"
        assert result.bindings[1][0] == "y"
        assert isinstance(result.body, AnalyzedCall)
    
    def test_let_with_empty_bindings(self):
        """Test analyzing (let () 42)."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("let"),
            AIFPLList(()),
            AIFPLNumber(42)
        ))
        
        result = analyzer.analyze(expr)
        
        assert len(result.bindings) == 0
        assert isinstance(result.body, AnalyzedLiteral)
    
    def test_let_bindings_have_indices(self):
        """Test that bindings have correct variable indices."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("let"),
            AIFPLList((
                AIFPLList((AIFPLSymbol("x"), AIFPLNumber(1))),
                AIFPLList((AIFPLSymbol("y"), AIFPLNumber(2))),
                AIFPLList((AIFPLSymbol("z"), AIFPLNumber(3)))
            )),
            AIFPLSymbol("x")
        ))
        
        result = analyzer.analyze(expr)
        
        # Bindings should have sequential indices
        assert result.bindings[0][2] == 0  # x at index 0
        assert result.bindings[1][2] == 1  # y at index 1
        assert result.bindings[2][2] == 2  # z at index 2


class TestLetScopeManagement:
    """Test scope management in let expressions."""
    
    def test_let_creates_new_scope(self):
        """Test that let creates a new scope."""
        analyzer = AIFPLAnalyzer()
        
        # Before let, depth should be 0
        assert analyzer.symbol_table.get_current_depth() == 0
        
        expr = AIFPLList((
            AIFPLSymbol("let"),
            AIFPLList((
                AIFPLList((AIFPLSymbol("x"), AIFPLNumber(5))),
            )),
            AIFPLSymbol("x")
        ))
        
        # Analyze (this will enter and exit scope)
        result = analyzer.analyze(expr)
        
        # After let, should be back to depth 0
        assert analyzer.symbol_table.get_current_depth() == 0
    
    def test_let_bindings_visible_in_body(self):
        """Test that bindings are visible in body."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("let"),
            AIFPLList((
                AIFPLList((AIFPLSymbol("x"), AIFPLNumber(5))),
            )),
            AIFPLSymbol("x")
        ))
        
        result = analyzer.analyze(expr)
        
        # Body should reference x as a local variable
        assert isinstance(result.body, AnalyzedVariable)
        assert result.body.var_type == "local"
    
    def test_nested_let_scopes(self):
        """Test nested let expressions."""
        analyzer = AIFPLAnalyzer()
        
        # (let ((x 1)) (let ((y 2)) (+ x y)))
        inner_let = AIFPLList((
            AIFPLSymbol("let"),
            AIFPLList((
                AIFPLList((AIFPLSymbol("y"), AIFPLNumber(2))),
            )),
            AIFPLList((
                AIFPLSymbol("+"),
                AIFPLSymbol("x"),
                AIFPLSymbol("y")
            ))
        ))
        
        outer_let = AIFPLList((
            AIFPLSymbol("let"),
            AIFPLList((
                AIFPLList((AIFPLSymbol("x"), AIFPLNumber(1))),
            )),
            inner_let
        ))
        
        result = analyzer.analyze(outer_let)
        
        assert isinstance(result, AnalyzedLet)
        assert isinstance(result.body, AnalyzedLet)


class TestLetRecursionDetection:
    """Test recursion detection in let bindings."""
    
    def test_non_recursive_binding(self):
        """Test that non-recursive bindings are not marked as recursive."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("let"),
            AIFPLList((
                AIFPLList((AIFPLSymbol("x"), AIFPLNumber(5))),
            )),
            AIFPLSymbol("x")
        ))
        
        result = analyzer.analyze(expr)
        
        assert len(result.recursive_bindings) == 0
    
    @pytest.mark.skip(reason="Requires lambda analysis (Step 2.5)")
    def test_recursive_lambda_detected(self):
        """Test that recursive lambda is detected."""
        analyzer = AIFPLAnalyzer()
        
        # (let ((fact (lambda (n) (fact (- n 1))))) (fact 5))
        # Note: We can't fully analyze lambda yet, but we can detect
        # that the lambda body references 'fact'
        lambda_body = AIFPLList((
            AIFPLSymbol("fact"),
            AIFPLList((
                AIFPLSymbol("-"),
                AIFPLSymbol("n"),
                AIFPLNumber(1)
            ))
        ))
        
        lambda_expr = AIFPLList((
            AIFPLSymbol("lambda"),
            AIFPLList((AIFPLSymbol("n"),)),
            lambda_body
        ))
        
        expr = AIFPLList((
            AIFPLSymbol("let"),
            AIFPLList((
                AIFPLList((AIFPLSymbol("fact"), lambda_expr)),
            )),
            AIFPLList((
                AIFPLSymbol("fact"),
                AIFPLNumber(5)
            ))
        ))
        
        result = analyzer.analyze(expr)
        
        # 'fact' should be marked as recursive
        assert "fact" in result.recursive_bindings
    
    @pytest.mark.skip(reason="Requires lambda analysis (Step 2.5)")
    def test_multiple_bindings_some_recursive(self):
        """Test mix of recursive and non-recursive bindings."""
        analyzer = AIFPLAnalyzer()
        
        # (let ((x 5) (f (lambda (n) (f n)))) (f x))
        lambda_expr = AIFPLList((
            AIFPLSymbol("lambda"),
            AIFPLList((AIFPLSymbol("n"),)),
            AIFPLList((
                AIFPLSymbol("f"),
                AIFPLSymbol("n")
            ))
        ))
        
        expr = AIFPLList((
            AIFPLSymbol("let"),
            AIFPLList((
                AIFPLList((AIFPLSymbol("x"), AIFPLNumber(5))),
                AIFPLList((AIFPLSymbol("f"), lambda_expr))
            )),
            AIFPLList((
                AIFPLSymbol("f"),
                AIFPLSymbol("x")
            ))
        ))
        
        result = analyzer.analyze(expr)
        
        assert "x" not in result.recursive_bindings
        assert "f" in result.recursive_bindings


class TestLetDependencyAnalysis:
    """Test dependency analysis for let bindings."""
    
    def test_binding_groups_present(self):
        """Test that binding_groups is populated."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("let"),
            AIFPLList((
                AIFPLList((AIFPLSymbol("x"), AIFPLNumber(5))),
            )),
            AIFPLSymbol("x")
        ))
        
        result = analyzer.analyze(expr)
        
        assert result.binding_groups is not None
        assert len(result.binding_groups) > 0
    
    @pytest.mark.skip(reason="Requires lambda analysis (Step 2.5)")
    def test_sibling_groups_for_mutual_recursion(self):
        """Test that sibling groups are identified."""
        analyzer = AIFPLAnalyzer()
        
        # (let ((f (lambda () (g))) (g (lambda () (f)))) (f))
        f_lambda = AIFPLList((
            AIFPLSymbol("lambda"),
            AIFPLList(()),
            AIFPLList((AIFPLSymbol("g"),))
        ))
        
        g_lambda = AIFPLList((
            AIFPLSymbol("lambda"),
            AIFPLList(()),
            AIFPLList((AIFPLSymbol("f"),))
        ))
        
        expr = AIFPLList((
            AIFPLSymbol("let"),
            AIFPLList((
                AIFPLList((AIFPLSymbol("f"), f_lambda)),
                AIFPLList((AIFPLSymbol("g"), g_lambda))
            )),
            AIFPLList((AIFPLSymbol("f"),))
        ))
        
        result = analyzer.analyze(expr)
        
        # Should identify f and g as mutually recursive
        assert len(result.sibling_groups) > 0


class TestLetInstructionCounting:
    """Test instruction count calculation for let expressions."""
    
    def test_simple_let_instruction_count(self):
        """Test instruction count for (let ((x 5)) x)."""
        analyzer = AIFPLAnalyzer()
        
        # x value: 1 (LOAD_CONST)
        # STORE_VAR: 1
        # body: 1 (LOAD_VAR)
        # Total: 3
        
        expr = AIFPLList((
            AIFPLSymbol("let"),
            AIFPLList((
                AIFPLList((AIFPLSymbol("x"), AIFPLNumber(5))),
            )),
            AIFPLSymbol("x")
        ))
        
        result = analyzer.analyze(expr)
        
        assert result.instruction_count == 3
    
    def test_multiple_bindings_instruction_count(self):
        """Test instruction count with multiple bindings."""
        analyzer = AIFPLAnalyzer()
        
        # (let ((x 5) (y 10)) (+ x y))
        # x: 1 + STORE: 1
        # y: 1 + STORE: 1
        # body: x(1) + y(1) + +(1) + CALL(1) = 4
        # Total: 8
        
        expr = AIFPLList((
            AIFPLSymbol("let"),
            AIFPLList((
                AIFPLList((AIFPLSymbol("x"), AIFPLNumber(5))),
                AIFPLList((AIFPLSymbol("y"), AIFPLNumber(10)))
            )),
            AIFPLList((
                AIFPLSymbol("+"),
                AIFPLSymbol("x"),
                AIFPLSymbol("y")
            ))
        ))
        
        result = analyzer.analyze(expr)
        
        assert result.instruction_count == 8


class TestLetTailPosition:
    """Test tail position handling in let expressions."""
    
    def test_body_inherits_tail_position(self):
        """Test that let body inherits tail position."""
        analyzer = AIFPLAnalyzer()
        
        # Set current function for tail call detection
        analyzer.current_function_name = "test"
        
        # (let ((x 5)) (test x))
        expr = AIFPLList((
            AIFPLSymbol("let"),
            AIFPLList((
                AIFPLList((AIFPLSymbol("x"), AIFPLNumber(5))),
            )),
            AIFPLList((
                AIFPLSymbol("test"),
                AIFPLSymbol("x")
            ))
        ))
        
        # Analyze in tail position
        result = analyzer._analyze_expression(expr, in_tail_position=True)
        
        # Body should be in tail position, so call should be tail call
        assert isinstance(result.body, AnalyzedCall)
        assert result.body.is_tail_call
    
    def test_binding_values_not_in_tail_position(self):
        """Test that binding values are never in tail position."""
        analyzer = AIFPLAnalyzer()
        
        analyzer.current_function_name = "test"
        
        # (let ((x (test 1))) x)
        expr = AIFPLList((
            AIFPLSymbol("let"),
            AIFPLList((
                AIFPLList((
                    AIFPLSymbol("x"),
                    AIFPLList((
                        AIFPLSymbol("test"),
                        AIFPLNumber(1)
                    ))
                )),
            )),
            AIFPLSymbol("x")
        ))
        
        # Even if let is in tail position, binding value is not
        result = analyzer._analyze_expression(expr, in_tail_position=True)
        
        binding_value = result.bindings[0][1]
        assert isinstance(binding_value, AnalyzedCall)
        assert not binding_value.is_tail_call


class TestLetErrorHandling:
    """Test error handling for let expressions."""
    
    def test_let_with_no_arguments(self):
        """Test (let) raises error."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((AIFPLSymbol("let"),))
        
        with pytest.raises(AIFPLEvalError, match="structure is incorrect"):
            analyzer.analyze(expr)
    
    def test_let_with_one_argument(self):
        """Test (let ()) raises error."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("let"),
            AIFPLList(())
        ))
        
        with pytest.raises(AIFPLEvalError, match="structure is incorrect"):
            analyzer.analyze(expr)
    
    def test_let_binding_list_not_list(self):
        """Test let with non-list binding raises error."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("let"),
            AIFPLNumber(42),  # Should be a list
            AIFPLNumber(1)
        ))
        
        with pytest.raises(AIFPLEvalError, match="binding list must be a list"):
            analyzer.analyze(expr)
    
    def test_let_binding_not_list(self):
        """Test let with non-list binding element raises error."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("let"),
            AIFPLList((
                AIFPLNumber(42),  # Should be (var val) list
            )),
            AIFPLNumber(1)
        ))
        
        with pytest.raises(AIFPLEvalError, match="binding .* must be a list"):
            analyzer.analyze(expr)
    
    def test_let_binding_wrong_length(self):
        """Test let binding with wrong number of elements."""
        analyzer = AIFPLAnalyzer()
        
        # Only one element
        expr = AIFPLList((
            AIFPLSymbol("let"),
            AIFPLList((
                AIFPLList((AIFPLSymbol("x"),)),  # Missing value
            )),
            AIFPLNumber(1)
        ))
        
        with pytest.raises(AIFPLEvalError, match="wrong number of elements"):
            analyzer.analyze(expr)
    
    def test_let_binding_variable_not_symbol(self):
        """Test let binding with non-symbol variable."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("let"),
            AIFPLList((
                AIFPLList((AIFPLNumber(42), AIFPLNumber(5))),  # Variable should be symbol
            )),
            AIFPLNumber(1)
        ))
        
        with pytest.raises(AIFPLEvalError, match="variable must be a symbol"):
            analyzer.analyze(expr)
    
    def test_let_duplicate_bindings(self):
        """Test let with duplicate binding names."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("let"),
            AIFPLList((
                AIFPLList((AIFPLSymbol("x"), AIFPLNumber(1))),
                AIFPLList((AIFPLSymbol("x"), AIFPLNumber(2)))  # Duplicate
            )),
            AIFPLSymbol("x")
        ))
        
        with pytest.raises(AIFPLEvalError, match="must be unique"):
            analyzer.analyze(expr)


class TestLetProperties:
    """Test various properties of analyzed let expressions."""
    
    def test_expr_type_is_let(self):
        """Test that expr_type is 'let'."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("let"),
            AIFPLList((
                AIFPLList((AIFPLSymbol("x"), AIFPLNumber(5))),
            )),
            AIFPLSymbol("x")
        ))
        
        result = analyzer.analyze(expr)
        
        assert result.expr_type == "let"
    
    def test_source_expr_preserved(self):
        """Test that source expression is preserved."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("let"),
            AIFPLList((
                AIFPLList((AIFPLSymbol("x"), AIFPLNumber(5))),
            )),
            AIFPLSymbol("x")
        ))
        
        result = analyzer.analyze(expr)
        
        assert result.source_expr == expr
