"""Tests for AIFPL Analyzer - If expression analysis.

These tests verify that the analyzer correctly analyzes if expressions,
including jump offset calculation and tail position handling.
"""

import pytest
from aifpl.aifpl_analyzer import AIFPLAnalyzer
from aifpl.aifpl_analysis_ir import (
    AnalyzedIf,
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


class TestAnalyzeIfBasic:
    """Test basic if expression analysis."""
    
    def test_analyze_simple_if(self):
        """Test analyzing (if #t 1 2)."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("if"),
            AIFPLBoolean(True),
            AIFPLNumber(1),
            AIFPLNumber(2)
        ))
        
        result = analyzer.analyze(expr)
        
        assert isinstance(result, AnalyzedIf)
        assert isinstance(result.condition, AnalyzedLiteral)
        assert isinstance(result.then_branch, AnalyzedLiteral)
        assert isinstance(result.else_branch, AnalyzedLiteral)
    
    def test_if_with_variable_condition(self):
        """Test analyzing (if x 1 2)."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("if"),
            AIFPLSymbol("x"),
            AIFPLNumber(1),
            AIFPLNumber(2)
        ))
        
        result = analyzer.analyze(expr)
        
        assert isinstance(result, AnalyzedIf)
        assert isinstance(result.condition, AnalyzedVariable)
    
    def test_if_with_call_condition(self):
        """Test analyzing (if (> x 0) 1 2)."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("if"),
            AIFPLList((
                AIFPLSymbol(">"),
                AIFPLSymbol("x"),
                AIFPLNumber(0)
            )),
            AIFPLNumber(1),
            AIFPLNumber(2)
        ))
        
        result = analyzer.analyze(expr)
        
        assert isinstance(result, AnalyzedIf)
        assert isinstance(result.condition, AnalyzedCall)
    
    def test_if_with_complex_branches(self):
        """Test if with complex then/else branches."""
        analyzer = AIFPLAnalyzer()
        
        # (if x (+ 1 2) (+ 3 4))
        expr = AIFPLList((
            AIFPLSymbol("if"),
            AIFPLSymbol("x"),
            AIFPLList((
                AIFPLSymbol("+"),
                AIFPLNumber(1),
                AIFPLNumber(2)
            )),
            AIFPLList((
                AIFPLSymbol("+"),
                AIFPLNumber(3),
                AIFPLNumber(4)
            ))
        ))
        
        result = analyzer.analyze(expr)
        
        assert isinstance(result.then_branch, AnalyzedCall)
        assert isinstance(result.else_branch, AnalyzedCall)


class TestIfJumpOffsets:
    """Test jump offset calculation for if expressions."""
    
    def test_simple_if_jump_offsets(self):
        """Test jump offsets for (if #t 1 2)."""
        analyzer = AIFPLAnalyzer()
        
        # (if #t 1 2)
        # condition: 1 (LOAD_TRUE)
        # POP_JUMP_IF_FALSE: 1
        # then: 1 (LOAD_CONST)
        # JUMP: 1
        # else: 1 (LOAD_CONST)
        # Total: 5
        
        expr = AIFPLList((
            AIFPLSymbol("if"),
            AIFPLBoolean(True),
            AIFPLNumber(1),
            AIFPLNumber(2)
        ))
        
        result = analyzer.analyze(expr)
        
        # Jump to else: condition(1) + POP_JUMP(1) + then(1) + JUMP(1) = 4
        assert result.jump_to_else_offset == 4
        
        # Jump past else: jump_to_else(4) + else(1) = 5
        assert result.jump_past_else_offset == 5
        
        # Total instructions: 5
        assert result.instruction_count == 5
    
    def test_if_with_complex_condition(self):
        """Test jump offsets with complex condition."""
        analyzer = AIFPLAnalyzer()
        
        # (if (> x 0) 1 2)
        # condition: x(1) + 0(1) + >(1) + CALL(1) = 4
        # POP_JUMP_IF_FALSE: 1
        # then: 1
        # JUMP: 1
        # else: 1
        # Total: 8
        
        expr = AIFPLList((
            AIFPLSymbol("if"),
            AIFPLList((
                AIFPLSymbol(">"),
                AIFPLSymbol("x"),
                AIFPLNumber(0)
            )),
            AIFPLNumber(1),
            AIFPLNumber(2)
        ))
        
        result = analyzer.analyze(expr)
        
        # Jump to else: 4 + 1 + 1 + 1 = 7
        assert result.jump_to_else_offset == 7
        
        # Jump past else: 7 + 1 = 8
        assert result.jump_past_else_offset == 8
        
        # Total: 8
        assert result.instruction_count == 8
    
    def test_if_with_complex_branches(self):
        """Test jump offsets with complex branches."""
        analyzer = AIFPLAnalyzer()
        
        # (if x (+ 1 2) (+ 3 4))
        # condition: x(1)
        # POP_JUMP_IF_FALSE: 1
        # then: 1(1) + 2(1) + +(1) + CALL(1) = 4
        # JUMP: 1
        # else: 3(1) + 4(1) + +(1) + CALL(1) = 4
        # Total: 11
        
        expr = AIFPLList((
            AIFPLSymbol("if"),
            AIFPLSymbol("x"),
            AIFPLList((
                AIFPLSymbol("+"),
                AIFPLNumber(1),
                AIFPLNumber(2)
            )),
            AIFPLList((
                AIFPLSymbol("+"),
                AIFPLNumber(3),
                AIFPLNumber(4)
            ))
        ))
        
        result = analyzer.analyze(expr)
        
        # Jump to else: 1 + 1 + 4 + 1 = 7
        assert result.jump_to_else_offset == 7
        
        # Jump past else: 7 + 4 = 11
        assert result.jump_past_else_offset == 11
        
        # Total: 11
        assert result.instruction_count == 11


class TestNestedIf:
    """Test nested if expressions."""
    
    def test_nested_if_in_then_branch(self):
        """Test (if x (if y 1 2) 3)."""
        analyzer = AIFPLAnalyzer()
        
        inner_if = AIFPLList((
            AIFPLSymbol("if"),
            AIFPLSymbol("y"),
            AIFPLNumber(1),
            AIFPLNumber(2)
        ))
        
        expr = AIFPLList((
            AIFPLSymbol("if"),
            AIFPLSymbol("x"),
            inner_if,
            AIFPLNumber(3)
        ))
        
        result = analyzer.analyze(expr)
        
        assert isinstance(result, AnalyzedIf)
        assert isinstance(result.then_branch, AnalyzedIf)
        assert isinstance(result.else_branch, AnalyzedLiteral)
    
    def test_nested_if_in_else_branch(self):
        """Test (if x 1 (if y 2 3))."""
        analyzer = AIFPLAnalyzer()
        
        inner_if = AIFPLList((
            AIFPLSymbol("if"),
            AIFPLSymbol("y"),
            AIFPLNumber(2),
            AIFPLNumber(3)
        ))
        
        expr = AIFPLList((
            AIFPLSymbol("if"),
            AIFPLSymbol("x"),
            AIFPLNumber(1),
            inner_if
        ))
        
        result = analyzer.analyze(expr)
        
        assert isinstance(result, AnalyzedIf)
        assert isinstance(result.then_branch, AnalyzedLiteral)
        assert isinstance(result.else_branch, AnalyzedIf)
    
    def test_deeply_nested_if(self):
        """Test deeply nested if expressions."""
        analyzer = AIFPLAnalyzer()
        
        # (if a (if b (if c 1 2) 3) 4)
        innermost = AIFPLList((
            AIFPLSymbol("if"),
            AIFPLSymbol("c"),
            AIFPLNumber(1),
            AIFPLNumber(2)
        ))
        
        middle = AIFPLList((
            AIFPLSymbol("if"),
            AIFPLSymbol("b"),
            innermost,
            AIFPLNumber(3)
        ))
        
        outer = AIFPLList((
            AIFPLSymbol("if"),
            AIFPLSymbol("a"),
            middle,
            AIFPLNumber(4)
        ))
        
        result = analyzer.analyze(outer)
        
        assert isinstance(result, AnalyzedIf)
        assert isinstance(result.then_branch, AnalyzedIf)
        assert isinstance(result.then_branch.then_branch, AnalyzedIf)


class TestIfTailPosition:
    """Test tail position handling in if expressions."""
    
    def test_if_not_in_tail_position(self):
        """Test if not in tail position has JUMP instruction."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("if"),
            AIFPLBoolean(True),
            AIFPLNumber(1),
            AIFPLNumber(2)
        ))
        
        # Analyze not in tail position
        result = analyzer._analyze_expression(expr, in_tail_position=False)
        
        # Should have JUMP instruction, so instruction count includes it
        # condition(1) + POP_JUMP(1) + then(1) + JUMP(1) + else(1) = 5
        assert result.instruction_count == 5
    
    def test_if_in_tail_position(self):
        """Test if in tail position omits JUMP instruction."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("if"),
            AIFPLBoolean(True),
            AIFPLNumber(1),
            AIFPLNumber(2)
        ))
        
        # Analyze in tail position
        result = analyzer._analyze_expression(expr, in_tail_position=True)
        
        # Should NOT have JUMP instruction in tail position
        # condition(1) + POP_JUMP(1) + then(1) + else(1) = 4
        assert result.instruction_count == 4
    
    def test_branches_inherit_tail_position(self):
        """Test that branches inherit tail position from if."""
        analyzer = AIFPLAnalyzer()
        
        # Set current function for tail call detection
        analyzer.current_function_name = "test"
        
        # (if x (test 1) (test 2))
        expr = AIFPLList((
            AIFPLSymbol("if"),
            AIFPLSymbol("x"),
            AIFPLList((
                AIFPLSymbol("test"),
                AIFPLNumber(1)
            )),
            AIFPLList((
                AIFPLSymbol("test"),
                AIFPLNumber(2)
            ))
        ))
        
        # Analyze in tail position
        result = analyzer._analyze_expression(expr, in_tail_position=True)
        
        # Both branches should be in tail position, so calls should be tail calls
        assert isinstance(result.then_branch, AnalyzedCall)
        assert isinstance(result.else_branch, AnalyzedCall)
        assert result.then_branch.is_tail_call
        assert result.else_branch.is_tail_call
    
    def test_condition_not_in_tail_position(self):
        """Test that condition is never in tail position."""
        analyzer = AIFPLAnalyzer()
        
        # Set current function
        analyzer.current_function_name = "test"
        
        # (if (test 1) 2 3)
        expr = AIFPLList((
            AIFPLSymbol("if"),
            AIFPLList((
                AIFPLSymbol("test"),
                AIFPLNumber(1)
            )),
            AIFPLNumber(2),
            AIFPLNumber(3)
        ))
        
        # Even if if is in tail position, condition is not
        result = analyzer._analyze_expression(expr, in_tail_position=True)
        
        assert isinstance(result.condition, AnalyzedCall)
        assert not result.condition.is_tail_call


class TestIfErrorHandling:
    """Test error handling for if expressions."""
    
    def test_if_with_no_arguments(self):
        """Test (if) raises error."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((AIFPLSymbol("if"),))
        
        with pytest.raises(AIFPLEvalError, match="wrong number of arguments"):
            analyzer.analyze(expr)
    
    def test_if_with_one_argument(self):
        """Test (if x) raises error."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("if"),
            AIFPLSymbol("x")
        ))
        
        with pytest.raises(AIFPLEvalError, match="wrong number of arguments"):
            analyzer.analyze(expr)
    
    def test_if_with_two_arguments(self):
        """Test (if x y) raises error."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("if"),
            AIFPLSymbol("x"),
            AIFPLNumber(1)
        ))
        
        with pytest.raises(AIFPLEvalError, match="wrong number of arguments"):
            analyzer.analyze(expr)
    
    def test_if_with_too_many_arguments(self):
        """Test (if x y z w) raises error."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("if"),
            AIFPLSymbol("x"),
            AIFPLNumber(1),
            AIFPLNumber(2),
            AIFPLNumber(3)
        ))
        
        with pytest.raises(AIFPLEvalError, match="wrong number of arguments"):
            analyzer.analyze(expr)


class TestIfProperties:
    """Test various properties of analyzed if expressions."""
    
    def test_expr_type_is_if(self):
        """Test that expr_type is 'if'."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("if"),
            AIFPLBoolean(True),
            AIFPLNumber(1),
            AIFPLNumber(2)
        ))
        
        result = analyzer.analyze(expr)
        
        assert result.expr_type == "if"
    
    def test_source_expr_preserved(self):
        """Test that source expression is preserved."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("if"),
            AIFPLBoolean(True),
            AIFPLNumber(1),
            AIFPLNumber(2)
        ))
        
        result = analyzer.analyze(expr)
        
        assert result.source_expr == expr
    
    def test_jump_offsets_are_positive(self):
        """Test that jump offsets are positive integers."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("if"),
            AIFPLBoolean(True),
            AIFPLNumber(1),
            AIFPLNumber(2)
        ))
        
        result = analyzer.analyze(expr)
        
        assert result.jump_to_else_offset > 0
        assert result.jump_past_else_offset > 0
        assert result.jump_past_else_offset > result.jump_to_else_offset


class TestComplexIfScenarios:
    """Test complex if scenarios."""
    
    def test_if_with_all_expression_types(self):
        """Test if with literals, variables, and calls."""
        analyzer = AIFPLAnalyzer()
        
        # (if (> x 0) (+ x 1) "negative")
        expr = AIFPLList((
            AIFPLSymbol("if"),
            AIFPLList((
                AIFPLSymbol(">"),
                AIFPLSymbol("x"),
                AIFPLNumber(0)
            )),
            AIFPLList((
                AIFPLSymbol("+"),
                AIFPLSymbol("x"),
                AIFPLNumber(1)
            )),
            AIFPLString("negative")
        ))
        
        result = analyzer.analyze(expr)
        
        assert isinstance(result.condition, AnalyzedCall)
        assert isinstance(result.then_branch, AnalyzedCall)
        assert isinstance(result.else_branch, AnalyzedLiteral)
    
    def test_if_chain(self):
        """Test chain of if-else-if."""
        analyzer = AIFPLAnalyzer()
        
        # (if a 1 (if b 2 (if c 3 4)))
        innermost = AIFPLList((
            AIFPLSymbol("if"),
            AIFPLSymbol("c"),
            AIFPLNumber(3),
            AIFPLNumber(4)
        ))
        
        middle = AIFPLList((
            AIFPLSymbol("if"),
            AIFPLSymbol("b"),
            AIFPLNumber(2),
            innermost
        ))
        
        outer = AIFPLList((
            AIFPLSymbol("if"),
            AIFPLSymbol("a"),
            AIFPLNumber(1),
            middle
        ))
        
        result = analyzer.analyze(outer)
        
        assert isinstance(result, AnalyzedIf)
        assert isinstance(result.else_branch, AnalyzedIf)
        assert isinstance(result.else_branch.else_branch, AnalyzedIf)
