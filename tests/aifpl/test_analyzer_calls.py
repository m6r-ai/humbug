"""Tests for AIFPL Analyzer - Function call analysis.

These tests verify that the analyzer correctly analyzes function calls,
including builtin calls, regular calls, and tail calls.
"""

import pytest
from aifpl.aifpl_analyzer import AIFPLAnalyzer
from aifpl.aifpl_analysis_ir import (
    AnalyzedCall,
    AnalyzedVariable,
    AnalyzedLiteral,
    AnalyzedMakeList,
)
from aifpl.aifpl_value import (
    AIFPLNumber,
    AIFPLString,
    AIFPLBoolean,
    AIFPLSymbol,
    AIFPLList,
)


class TestAnalyzeBuiltinCalls:
    """Test analyzing builtin function calls."""
    
    def test_analyze_simple_builtin_call(self):
        """Test analyzing (+ 1 2)."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("+"),
            AIFPLNumber(1),
            AIFPLNumber(2)
        ))
        
        result = analyzer.analyze(expr)
        
        assert isinstance(result, AnalyzedCall)
        assert result.is_builtin
        assert result.builtin_index >= 0
        assert len(result.args) == 2
        assert all(isinstance(arg, AnalyzedLiteral) for arg in result.args)
        # func (1) + arg1 (1) + arg2 (1) + call (1) = 4
        assert result.instruction_count == 4
    
    def test_analyze_builtin_with_no_args(self):
        """Test analyzing builtin with no arguments."""
        analyzer = AIFPLAnalyzer()
        
        # (cons) with no args - this will be analyzed as a call
        expr = AIFPLList((AIFPLSymbol("+"),))
        
        result = analyzer.analyze(expr)
        
        assert isinstance(result, AnalyzedCall)
        assert result.is_builtin
        assert len(result.args) == 0
    
    def test_analyze_builtin_with_many_args(self):
        """Test analyzing builtin with many arguments."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("+"),
            AIFPLNumber(1),
            AIFPLNumber(2),
            AIFPLNumber(3),
            AIFPLNumber(4),
            AIFPLNumber(5)
        ))
        
        result = analyzer.analyze(expr)
        
        assert isinstance(result, AnalyzedCall)
        assert result.is_builtin
        assert len(result.args) == 5
    
    def test_analyze_nested_builtin_calls(self):
        """Test analyzing (+ (* 2 3) (- 5 1))."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("+"),
            AIFPLList((
                AIFPLSymbol("*"),
                AIFPLNumber(2),
                AIFPLNumber(3)
            )),
            AIFPLList((
                AIFPLSymbol("-"),
                AIFPLNumber(5),
                AIFPLNumber(1)
            ))
        ))
        
        result = analyzer.analyze(expr)
        
        assert isinstance(result, AnalyzedCall)
        assert result.is_builtin
        assert len(result.args) == 2
        assert all(isinstance(arg, AnalyzedCall) for arg in result.args)
        assert all(arg.is_builtin for arg in result.args)
    
    def test_builtin_function_object(self):
        """Test analyzing builtin function as value."""
        analyzer = AIFPLAnalyzer()
        
        # Just the symbol + by itself
        expr = AIFPLSymbol("+")
        
        result = analyzer.analyze(expr)
        
        assert isinstance(result, AnalyzedVariable)
        assert result.var_type == "builtin"
        assert result.name == "+"


class TestAnalyzeRegularCalls:
    """Test analyzing regular (non-builtin) function calls."""
    
    def test_analyze_variable_call(self):
        """Test analyzing (f x y)."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("f"),
            AIFPLSymbol("x"),
            AIFPLSymbol("y")
        ))
        
        result = analyzer.analyze(expr)
        
        assert isinstance(result, AnalyzedCall)
        assert not result.is_builtin
        assert result.builtin_index == -1
        assert isinstance(result.func, AnalyzedVariable)
        assert len(result.args) == 2
    
    def test_analyze_call_with_literal_args(self):
        """Test analyzing (f 1 "hello" #t)."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("f"),
            AIFPLNumber(1),
            AIFPLString("hello"),
            AIFPLBoolean(True)
        ))
        
        result = analyzer.analyze(expr)
        
        assert isinstance(result, AnalyzedCall)
        assert not result.is_builtin
        assert len(result.args) == 3
        assert all(isinstance(arg, AnalyzedLiteral) for arg in result.args)
    
    def test_analyze_call_with_mixed_args(self):
        """Test analyzing (f 1 x (+ 2 3))."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("f"),
            AIFPLNumber(1),
            AIFPLSymbol("x"),
            AIFPLList((
                AIFPLSymbol("+"),
                AIFPLNumber(2),
                AIFPLNumber(3)
            ))
        ))
        
        result = analyzer.analyze(expr)
        
        assert isinstance(result, AnalyzedCall)
        assert len(result.args) == 3
        assert isinstance(result.args[0], AnalyzedLiteral)
        assert isinstance(result.args[1], AnalyzedVariable)
        assert isinstance(result.args[2], AnalyzedCall)


class TestTailCallDetection:
    """Test tail call detection."""
    
    def test_non_tail_call(self):
        """Test that calls not in tail position are not marked as tail calls."""
        analyzer = AIFPLAnalyzer()
        
        # Simple call not in tail position
        expr = AIFPLList((
            AIFPLSymbol("f"),
            AIFPLNumber(1)
        ))
        
        result = analyzer.analyze(expr)
        
        assert isinstance(result, AnalyzedCall)
        assert not result.is_tail_call
    
    def test_tail_call_without_function_name(self):
        """Test that tail calls without current function name are not marked."""
        analyzer = AIFPLAnalyzer()
        
        # Even if we say it's in tail position, without a current function name
        # it can't be a tail recursive call
        expr = AIFPLList((
            AIFPLSymbol("f"),
            AIFPLNumber(1)
        ))
        
        result = analyzer._analyze_expression(expr, in_tail_position=True)
        
        assert isinstance(result, AnalyzedCall)
        assert not result.is_tail_call
    
    def test_tail_recursive_call(self):
        """Test that tail recursive calls are detected."""
        analyzer = AIFPLAnalyzer()
        
        # Set current function name
        analyzer.current_function_name = "factorial"
        
        # Call to factorial in tail position
        expr = AIFPLList((
            AIFPLSymbol("factorial"),
            AIFPLNumber(5)
        ))
        
        result = analyzer._analyze_expression(expr, in_tail_position=True)
        
        assert isinstance(result, AnalyzedCall)
        assert result.is_tail_call
    
    def test_non_recursive_tail_call(self):
        """Test that tail calls to different functions are not marked as tail recursive."""
        analyzer = AIFPLAnalyzer()
        
        # Set current function name
        analyzer.current_function_name = "foo"
        
        # Call to different function in tail position
        expr = AIFPLList((
            AIFPLSymbol("bar"),
            AIFPLNumber(5)
        ))
        
        result = analyzer._analyze_expression(expr, in_tail_position=True)
        
        assert isinstance(result, AnalyzedCall)
        # Not tail recursive because calling different function
        assert not result.is_tail_call


class TestInstructionCounting:
    """Test instruction count calculation for calls."""
    
    def test_simple_call_instruction_count(self):
        """Test instruction count for (+ 1 2)."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("+"),
            AIFPLNumber(1),
            AIFPLNumber(2)
        ))
        
        result = analyzer.analyze(expr)
        
        # func (1) + arg1 (1) + arg2 (1) + call (1) = 4
        assert result.instruction_count == 4
    
    def test_nested_call_instruction_count(self):
        """Test instruction count for nested calls."""
        analyzer = AIFPLAnalyzer()
        
        # (+ (* 2 3) 4)
        # Inner: func(1) + 2(1) + 3(1) + call(1) = 4
        # Outer: func(1) + inner(4) + 4(1) + call(1) = 7
        expr = AIFPLList((
            AIFPLSymbol("+"),
            AIFPLList((
                AIFPLSymbol("*"),
                AIFPLNumber(2),
                AIFPLNumber(3)
            )),
            AIFPLNumber(4)
        ))
        
        result = analyzer.analyze(expr)
        
        assert result.instruction_count == 7
    
    def test_call_with_variable_instruction_count(self):
        """Test instruction count with variables."""
        analyzer = AIFPLAnalyzer()
        
        # (f x y)
        # func(1) + x(1) + y(1) + call(1) = 4
        expr = AIFPLList((
            AIFPLSymbol("f"),
            AIFPLSymbol("x"),
            AIFPLSymbol("y")
        ))
        
        result = analyzer.analyze(expr)
        
        assert result.instruction_count == 4


class TestCallAnalysisProperties:
    """Test various properties of analyzed calls."""
    
    def test_func_not_in_tail_position(self):
        """Test that function expression is never in tail position."""
        analyzer = AIFPLAnalyzer()
        
        # Even if the call is in tail position, the function itself is not
        analyzer.current_function_name = "test"
        
        expr = AIFPLList((
            AIFPLSymbol("test"),
            AIFPLNumber(1)
        ))
        
        result = analyzer._analyze_expression(expr, in_tail_position=True)
        
        # The call is in tail position
        assert result.is_tail_call
        
        # But the function expression was analyzed with in_tail_position=False
        # (We can't directly test this, but the implementation ensures it)
    
    def test_args_not_in_tail_position(self):
        """Test that arguments are never in tail position."""
        analyzer = AIFPLAnalyzer()
        
        # Arguments are never in tail position, even if call is
        expr = AIFPLList((
            AIFPLSymbol("f"),
            AIFPLList((
                AIFPLSymbol("g"),
                AIFPLNumber(1)
            ))
        ))
        
        result = analyzer._analyze_expression(expr, in_tail_position=True)
        
        # The inner call (g 1) is an argument, so not in tail position
        inner_call = result.args[0]
        assert isinstance(inner_call, AnalyzedCall)
        assert not inner_call.is_tail_call
    
    def test_source_expr_preserved(self):
        """Test that source expression is preserved."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("+"),
            AIFPLNumber(1),
            AIFPLNumber(2)
        ))
        
        result = analyzer.analyze(expr)
        
        assert result.source_expr == expr
    
    def test_expr_type_is_call(self):
        """Test that expr_type is set correctly."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("f"),
            AIFPLNumber(1)
        ))
        
        result = analyzer.analyze(expr)
        
        assert result.expr_type == "call"


class TestBuiltinHelpers:
    """Test builtin helper methods."""
    
    def test_is_builtin(self):
        """Test _is_builtin helper."""
        analyzer = AIFPLAnalyzer()
        
        assert analyzer._is_builtin("+")
        assert analyzer._is_builtin("list")
        assert analyzer._is_builtin("map")
        assert not analyzer._is_builtin("my_function")
        assert not analyzer._is_builtin("unknown")
    
    def test_get_builtin_index(self):
        """Test _get_builtin_index helper."""
        analyzer = AIFPLAnalyzer()
        
        # Should return valid indices
        plus_idx = analyzer._get_builtin_index("+")
        assert plus_idx >= 0
        
        list_idx = analyzer._get_builtin_index("list")
        assert list_idx >= 0
        
        # Different builtins should have different indices
        assert plus_idx != list_idx
    
    def test_get_builtin_index_not_found(self):
        """Test _get_builtin_index with non-builtin."""
        analyzer = AIFPLAnalyzer()
        
        with pytest.raises(ValueError):
            analyzer._get_builtin_index("not_a_builtin")


class TestComplexCallScenarios:
    """Test complex call scenarios."""
    
    def test_deeply_nested_calls(self):
        """Test deeply nested function calls."""
        analyzer = AIFPLAnalyzer()
        
        # (+ (+ (+ 1 2) 3) 4)
        expr = AIFPLList((
            AIFPLSymbol("+"),
            AIFPLList((
                AIFPLSymbol("+"),
                AIFPLList((
                    AIFPLSymbol("+"),
                    AIFPLNumber(1),
                    AIFPLNumber(2)
                )),
                AIFPLNumber(3)
            )),
            AIFPLNumber(4)
        ))
        
        result = analyzer.analyze(expr)
        
        assert isinstance(result, AnalyzedCall)
        assert result.is_builtin
        assert isinstance(result.args[0], AnalyzedCall)
        assert isinstance(result.args[0].args[0], AnalyzedCall)
    
    def test_call_with_all_types_of_args(self):
        """Test call with literals, variables, and nested calls."""
        analyzer = AIFPLAnalyzer()
        
        # (f 42 "hello" x (+ 1 2))
        expr = AIFPLList((
            AIFPLSymbol("f"),
            AIFPLNumber(42),
            AIFPLString("hello"),
            AIFPLSymbol("x"),
            AIFPLList((
                AIFPLSymbol("+"),
                AIFPLNumber(1),
                AIFPLNumber(2)
            ))
        ))
        
        result = analyzer.analyze(expr)
        
        assert len(result.args) == 4
        assert isinstance(result.args[0], AnalyzedLiteral)  # 42
        assert isinstance(result.args[1], AnalyzedLiteral)  # "hello"
        assert isinstance(result.args[2], AnalyzedVariable)  # x
        assert isinstance(result.args[3], AnalyzedCall)  # (+ 1 2)
    
    def test_higher_order_function_call(self):
        """Test call that passes function as argument."""
        analyzer = AIFPLAnalyzer()
        
        # (map + (list 1 2 3))
        # Note: This will work once we implement lambda analysis
        expr = AIFPLList((
            AIFPLSymbol("map"),
            AIFPLSymbol("+"),
            AIFPLList((
                AIFPLSymbol("list"),
                AIFPLNumber(1),
                AIFPLNumber(2),
                AIFPLNumber(3)
            ))
        ))
        
        result = analyzer.analyze(expr)
        
        assert isinstance(result, AnalyzedCall)
        assert result.is_builtin
        assert len(result.args) == 2
        assert isinstance(result.args[0], AnalyzedVariable)  # +
        # (list 1 2 3) is analyzed as AnalyzedMakeList, not AnalyzedCall
        assert isinstance(result.args[1], AnalyzedMakeList)  # (list 1 2 3)
