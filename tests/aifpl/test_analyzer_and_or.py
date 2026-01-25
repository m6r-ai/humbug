"""Tests for AIFPL Analyzer - And/Or expression analysis.

These tests verify that and/or expressions are analyzed correctly
with pre-calculated jump offsets for short-circuit evaluation.
"""

import pytest
from aifpl.aifpl_analyzer import AIFPLAnalyzer
from aifpl.aifpl_analysis_ir import AnalyzedAnd, AnalyzedOr
from aifpl.aifpl_value import (
    AIFPLBoolean,
    AIFPLSymbol,
    AIFPLList,
)


class TestAnalyzeAndBasic:
    """Test basic and expression analysis."""
    
    def test_analyze_empty_and(self):
        """Test analyzing (and) -> #t."""
        analyzer = AIFPLAnalyzer()
        
        # (and)
        expr = AIFPLList((AIFPLSymbol("and"),))
        
        analyzed = analyzer.analyze(expr)
        
        assert isinstance(analyzed, AnalyzedAnd)
        assert len(analyzed.args) == 0
        assert analyzed.instruction_count == 1  # Just LOAD_TRUE
    
    def test_analyze_and_single_arg(self):
        """Test analyzing (and #t)."""
        analyzer = AIFPLAnalyzer()
        
        # (and #t)
        expr = AIFPLList((
            AIFPLSymbol("and"),
            AIFPLBoolean(True)
        ))
        
        analyzed = analyzer.analyze(expr)
        
        assert isinstance(analyzed, AnalyzedAnd)
        assert len(analyzed.args) == 1
        # arg(1) + POP_JUMP(1) + LOAD_TRUE(1) + JUMP(1) + LOAD_FALSE(1) = 5
        assert analyzed.instruction_count == 5
    
    def test_analyze_and_two_args(self):
        """Test analyzing (and #t #f)."""
        analyzer = AIFPLAnalyzer()
        
        # (and #t #f)
        expr = AIFPLList((
            AIFPLSymbol("and"),
            AIFPLBoolean(True),
            AIFPLBoolean(False)
        ))
        
        analyzed = analyzer.analyze(expr)
        
        assert isinstance(analyzed, AnalyzedAnd)
        assert len(analyzed.args) == 2
        # arg1(1) + POP_JUMP(1) + arg2(1) + POP_JUMP(1) + LOAD_TRUE(1) + JUMP(1) + LOAD_FALSE(1) = 7
        assert analyzed.instruction_count == 7
    
    def test_analyze_and_three_args(self):
        """Test analyzing (and #t #t #f)."""
        analyzer = AIFPLAnalyzer()
        
        # (and #t #t #f)
        expr = AIFPLList((
            AIFPLSymbol("and"),
            AIFPLBoolean(True),
            AIFPLBoolean(True),
            AIFPLBoolean(False)
        ))
        
        analyzed = analyzer.analyze(expr)
        
        assert isinstance(analyzed, AnalyzedAnd)
        assert len(analyzed.args) == 3
        # 3 args, each with POP_JUMP + LOAD_TRUE + JUMP + LOAD_FALSE = 3*2 + 3 = 9
        assert analyzed.instruction_count == 9


class TestAnalyzeAndJumpOffsets:
    """Test that jump offsets are pre-calculated correctly."""
    
    def test_and_jump_offsets_not_zero(self):
        """Test that jump offsets are NOT zero placeholders."""
        analyzer = AIFPLAnalyzer()
        
        # (and #t #f)
        expr = AIFPLList((
            AIFPLSymbol("and"),
            AIFPLBoolean(True),
            AIFPLBoolean(False)
        ))
        
        analyzed = analyzer.analyze(expr)
        
        # Jump offsets should be pre-calculated, not zero
        assert len(analyzed.jump_to_false_offsets) == 2
        for offset in analyzed.jump_to_false_offsets:
            assert offset > 0  # Not a placeholder!
    
    def test_and_jump_to_false_offset_calculation(self):
        """Test that jump_to_false_offset is calculated correctly."""
        analyzer = AIFPLAnalyzer()
        
        # (and #t #f)
        expr = AIFPLList((
            AIFPLSymbol("and"),
            AIFPLBoolean(True),
            AIFPLBoolean(False)
        ))
        
        analyzed = analyzer.analyze(expr)
        
        # Structure:
        # 0: LOAD_TRUE (arg1)
        # 1: POP_JUMP_IF_FALSE 5
        # 2: LOAD_FALSE (arg2)
        # 3: POP_JUMP_IF_FALSE 5
        # 4: LOAD_TRUE
        # 5: JUMP 6
        # 6: LOAD_FALSE  <- false section
        
        # Both jumps should target position 6 (false section)
        assert analyzed.jump_to_false_offsets[0] == 6
        assert analyzed.jump_to_false_offsets[1] == 6


class TestAnalyzeOrBasic:
    """Test basic or expression analysis."""
    
    def test_analyze_empty_or(self):
        """Test analyzing (or) -> #f."""
        analyzer = AIFPLAnalyzer()
        
        # (or)
        expr = AIFPLList((AIFPLSymbol("or"),))
        
        analyzed = analyzer.analyze(expr)
        
        assert isinstance(analyzed, AnalyzedOr)
        assert len(analyzed.args) == 0
        assert analyzed.instruction_count == 1  # Just LOAD_FALSE
    
    def test_analyze_or_single_arg(self):
        """Test analyzing (or #f)."""
        analyzer = AIFPLAnalyzer()
        
        # (or #f)
        expr = AIFPLList((
            AIFPLSymbol("or"),
            AIFPLBoolean(False)
        ))
        
        analyzed = analyzer.analyze(expr)
        
        assert isinstance(analyzed, AnalyzedOr)
        assert len(analyzed.args) == 1
        # arg(1) + POP_JUMP(1) + LOAD_FALSE(1) + JUMP(1) + LOAD_TRUE(1) = 5
        assert analyzed.instruction_count == 5
    
    def test_analyze_or_two_args(self):
        """Test analyzing (or #f #t)."""
        analyzer = AIFPLAnalyzer()
        
        # (or #f #t)
        expr = AIFPLList((
            AIFPLSymbol("or"),
            AIFPLBoolean(False),
            AIFPLBoolean(True)
        ))
        
        analyzed = analyzer.analyze(expr)
        
        assert isinstance(analyzed, AnalyzedOr)
        assert len(analyzed.args) == 2
        # arg1(1) + POP_JUMP(1) + arg2(1) + POP_JUMP(1) + LOAD_FALSE(1) + JUMP(1) + LOAD_TRUE(1) = 7
        assert analyzed.instruction_count == 7


class TestAnalyzeOrJumpOffsets:
    """Test that jump offsets are pre-calculated correctly."""
    
    def test_or_jump_offsets_not_zero(self):
        """Test that jump offsets are NOT zero placeholders."""
        analyzer = AIFPLAnalyzer()
        
        # (or #f #t)
        expr = AIFPLList((
            AIFPLSymbol("or"),
            AIFPLBoolean(False),
            AIFPLBoolean(True)
        ))
        
        analyzed = analyzer.analyze(expr)
        
        # Jump offsets should be pre-calculated, not zero
        assert len(analyzed.jump_to_true_offsets) == 2
        for offset in analyzed.jump_to_true_offsets:
            assert offset > 0  # Not a placeholder!
    
    def test_or_jump_to_true_offset_calculation(self):
        """Test that jump_to_true_offset is calculated correctly."""
        analyzer = AIFPLAnalyzer()
        
        # (or #f #t)
        expr = AIFPLList((
            AIFPLSymbol("or"),
            AIFPLBoolean(False),
            AIFPLBoolean(True)
        ))
        
        analyzed = analyzer.analyze(expr)
        
        # Structure:
        # 0: LOAD_FALSE (arg1)
        # 1: POP_JUMP_IF_TRUE 5
        # 2: LOAD_TRUE (arg2)
        # 3: POP_JUMP_IF_TRUE 5
        # 4: LOAD_FALSE
        # 5: JUMP 6
        # 6: LOAD_TRUE  <- true section
        
        # Both jumps should target position 6 (true section)
        assert analyzed.jump_to_true_offsets[0] == 6
        assert analyzed.jump_to_true_offsets[1] == 6


class TestAnalyzeAndOrNested:
    """Test nested and/or expressions."""
    
    def test_analyze_nested_and_in_and(self):
        """Test analyzing (and (and #t #f) #t)."""
        analyzer = AIFPLAnalyzer()
        
        # (and (and #t #f) #t)
        expr = AIFPLList((
            AIFPLSymbol("and"),
            AIFPLList((
                AIFPLSymbol("and"),
                AIFPLBoolean(True),
                AIFPLBoolean(False)
            )),
            AIFPLBoolean(True)
        ))
        
        analyzed = analyzer.analyze(expr)
        
        assert isinstance(analyzed, AnalyzedAnd)
        assert len(analyzed.args) == 2
        # First arg is a nested and with instruction_count=7
        assert analyzed.args[0].instruction_count == 7
    
    def test_analyze_nested_or_in_or(self):
        """Test analyzing (or (or #f #t) #f)."""
        analyzer = AIFPLAnalyzer()
        
        # (or (or #f #t) #f)
        expr = AIFPLList((
            AIFPLSymbol("or"),
            AIFPLList((
                AIFPLSymbol("or"),
                AIFPLBoolean(False),
                AIFPLBoolean(True)
            )),
            AIFPLBoolean(False)
        ))
        
        analyzed = analyzer.analyze(expr)
        
        assert isinstance(analyzed, AnalyzedOr)
        assert len(analyzed.args) == 2
        # First arg is a nested or with instruction_count=7
        assert analyzed.args[0].instruction_count == 7
    
    def test_analyze_and_with_or(self):
        """Test analyzing (and #t (or #f #t))."""
        analyzer = AIFPLAnalyzer()
        
        # (and #t (or #f #t))
        expr = AIFPLList((
            AIFPLSymbol("and"),
            AIFPLBoolean(True),
            AIFPLList((
                AIFPLSymbol("or"),
                AIFPLBoolean(False),
                AIFPLBoolean(True)
            ))
        ))
        
        analyzed = analyzer.analyze(expr)
        
        assert isinstance(analyzed, AnalyzedAnd)
        assert len(analyzed.args) == 2
        # Second arg is an or expression
        assert isinstance(analyzed.args[1], AnalyzedOr)
