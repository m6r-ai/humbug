"""Tests for AIFPL Generator - If expression generation.

These tests verify the KEY INNOVATION of the two-pass approach:
jump offsets are pre-calculated during analysis, so code generation
emits correct bytecode from the start - NO PATCHING!
"""

import pytest
from aifpl.aifpl_generator import AIFPLGenerator
from aifpl.aifpl_analyzer import AIFPLAnalyzer
from aifpl.aifpl_bytecode import CodeObject, Opcode
from aifpl.aifpl_value import (
    AIFPLNumber,
    AIFPLBoolean,
    AIFPLSymbol,
    AIFPLList,
)


class TestGenerateIfBasic:
    """Test basic if expression generation."""
    
    def test_generate_simple_if(self):
        """Test generating code for simple if expression."""
        analyzer = AIFPLAnalyzer()
        
        # (if #t 1 2)
        expr = AIFPLList((
            AIFPLSymbol("if"),
            AIFPLBoolean(True),
            AIFPLNumber(1),
            AIFPLNumber(2)
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, [], [])
        code = generator.generate()
        
        # Expected bytecode:
        # 0: LOAD_TRUE              (condition)
        # 1: POP_JUMP_IF_FALSE 4    (jump to else)
        # 2: LOAD_CONST 0           (then: 1)
        # 3: JUMP 5                 (jump past else)
        # 4: LOAD_CONST 1           (else: 2)
        # 5: RETURN
        
        assert len(code.instructions) == 6
        assert code.instructions[0].opcode == Opcode.LOAD_TRUE
        assert code.instructions[1].opcode == Opcode.POP_JUMP_IF_FALSE
        assert code.instructions[1].arg1 == 4  # Jump to else at position 4
        assert code.instructions[2].opcode == Opcode.LOAD_CONST
        assert code.instructions[3].opcode == Opcode.JUMP
        assert code.instructions[3].arg1 == 5  # Jump past else to position 5
        assert code.instructions[4].opcode == Opcode.LOAD_CONST
        assert code.instructions[5].opcode == Opcode.RETURN
    
    def test_generate_if_with_false_condition(self):
        """Test generating code for if with false condition."""
        analyzer = AIFPLAnalyzer()
        
        # (if #f 1 2)
        expr = AIFPLList((
            AIFPLSymbol("if"),
            AIFPLBoolean(False),
            AIFPLNumber(1),
            AIFPLNumber(2)
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, [], [])
        code = generator.generate()
        
        # Structure should be same as above, just with LOAD_FALSE
        assert len(code.instructions) == 6
        assert code.instructions[0].opcode == Opcode.LOAD_FALSE
        assert code.instructions[1].opcode == Opcode.POP_JUMP_IF_FALSE


class TestGenerateIfJumpOffsets:
    """Test that jump offsets are correct (NO PATCHING!)."""
    
    def test_if_jump_offsets_not_zero(self):
        """Test that jump offsets are NOT zero placeholders."""
        analyzer = AIFPLAnalyzer()
        
        # (if #t 1 2)
        expr = AIFPLList((
            AIFPLSymbol("if"),
            AIFPLBoolean(True),
            AIFPLNumber(1),
            AIFPLNumber(2)
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, [], [])
        code = generator.generate()
        
        # KEY TEST: Jump offsets should be CORRECT, not 0 placeholders!
        pop_jump = code.instructions[1]
        assert pop_jump.opcode == Opcode.POP_JUMP_IF_FALSE
        assert pop_jump.arg1 != 0  # NOT a placeholder!
        assert pop_jump.arg1 == 4  # Correct target
        
        jump = code.instructions[3]
        assert jump.opcode == Opcode.JUMP
        assert jump.arg1 != 0  # NOT a placeholder!
        assert jump.arg1 == 5  # Correct target
    
    def test_if_jump_targets_match_analysis(self):
        """Test that generated jump targets match analyzed offsets."""
        analyzer = AIFPLAnalyzer()
        
        # (if #t 1 2)
        expr = AIFPLList((
            AIFPLSymbol("if"),
            AIFPLBoolean(True),
            AIFPLNumber(1),
            AIFPLNumber(2)
        ))
        
        analyzed = analyzer.analyze(expr)
        
        # Get analyzed offsets
        analyzed_else_offset = analyzed.jump_to_else_offset
        analyzed_end_offset = analyzed.jump_past_else_offset
        
        # Generate code
        generator = AIFPLGenerator(analyzed, analyzer.constants, [], [])
        code = generator.generate()
        
        # Verify generated jump targets match analysis
        # start_ip = 0 (beginning of if)
        pop_jump = code.instructions[1]
        assert pop_jump.arg1 == analyzed_else_offset
        
        jump = code.instructions[3]
        assert jump.arg1 == analyzed_end_offset


class TestGenerateIfNested:
    """Test nested if expressions."""
    
    def test_generate_nested_if_in_then(self):
        """Test generating code for nested if in then branch."""
        analyzer = AIFPLAnalyzer()
        
        # (if #t (if #f 1 2) 3)
        expr = AIFPLList((
            AIFPLSymbol("if"),
            AIFPLBoolean(True),
            AIFPLList((
                AIFPLSymbol("if"),
                AIFPLBoolean(False),
                AIFPLNumber(1),
                AIFPLNumber(2)
            )),
            AIFPLNumber(3)
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, [], [])
        code = generator.generate()
        
        # Outer if should have correct jump offsets
        # Inner if should also have correct jump offsets
        # All jumps should be correct from the start
        
        # Count POP_JUMP_IF_FALSE and JUMP instructions
        pop_jumps = [i for i in code.instructions if i.opcode == Opcode.POP_JUMP_IF_FALSE]
        jumps = [i for i in code.instructions if i.opcode == Opcode.JUMP]
        
        assert len(pop_jumps) == 2  # One for each if
        assert len(jumps) == 2      # One for each if
        
        # All jump targets should be non-zero
        for jump in pop_jumps + jumps:
            assert jump.arg1 > 0
    
    def test_generate_nested_if_in_else(self):
        """Test generating code for nested if in else branch."""
        analyzer = AIFPLAnalyzer()
        
        # (if #t 1 (if #f 2 3))
        expr = AIFPLList((
            AIFPLSymbol("if"),
            AIFPLBoolean(True),
            AIFPLNumber(1),
            AIFPLList((
                AIFPLSymbol("if"),
                AIFPLBoolean(False),
                AIFPLNumber(2),
                AIFPLNumber(3)
            ))
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, [], [])
        code = generator.generate()
        
        # Both ifs should have correct jumps
        pop_jumps = [i for i in code.instructions if i.opcode == Opcode.POP_JUMP_IF_FALSE]
        jumps = [i for i in code.instructions if i.opcode == Opcode.JUMP]
        
        assert len(pop_jumps) == 2
        assert len(jumps) == 2
        
        # All jump targets should be correct
        for jump in pop_jumps + jumps:
            assert jump.arg1 > 0


class TestGenerateIfWithExpressions:
    """Test if expressions with complex conditions and branches."""
    
    def test_generate_if_with_call_condition(self):
        """Test generating if with function call as condition."""
        analyzer = AIFPLAnalyzer()
        
        # (if (> x 0) 1 2)
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
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, [])
        code = generator.generate()
        
        # Condition should be: LOAD_NAME(x), LOAD_CONST(0), CALL_BUILTIN (> is builtin, not loaded)
        # Then: POP_JUMP_IF_FALSE
        assert code.instructions[0].opcode == Opcode.LOAD_NAME  # x
        assert code.instructions[1].opcode == Opcode.LOAD_CONST  # 0
        assert code.instructions[2].opcode == Opcode.CALL_BUILTIN
        assert code.instructions[3].opcode == Opcode.POP_JUMP_IF_FALSE
        
        # Jump target should be correct
        assert code.instructions[3].arg1 > 3
    
    def test_generate_if_with_call_branches(self):
        """Test generating if with function calls in branches."""
        analyzer = AIFPLAnalyzer()
        
        # (if #t (+ 1 2) (+ 3 4))
        expr = AIFPLList((
            AIFPLSymbol("if"),
            AIFPLBoolean(True),
            AIFPLList((AIFPLSymbol("+"), AIFPLNumber(1), AIFPLNumber(2))),
            AIFPLList((AIFPLSymbol("+"), AIFPLNumber(3), AIFPLNumber(4)))
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, [])
        code = generator.generate()
        
        # Should have both calls plus if structure
        builtin_calls = [i for i in code.instructions if i.opcode == Opcode.CALL_BUILTIN]
        assert len(builtin_calls) == 2
        
        # Jump offsets should account for call instructions
        pop_jump = code.instructions[1]
        assert pop_jump.opcode == Opcode.POP_JUMP_IF_FALSE
        # Should jump past: LOAD_CONST(1), LOAD_CONST(2), CALL_BUILTIN, JUMP
        assert pop_jump.arg1 == 6


class TestGenerateIfInstructionCounts:
    """Test that instruction counts match analysis."""
    
    def test_if_instruction_count_simple(self):
        """Test instruction count for simple if."""
        analyzer = AIFPLAnalyzer()
        
        # (if #t 1 2)
        expr = AIFPLList((
            AIFPLSymbol("if"),
            AIFPLBoolean(True),
            AIFPLNumber(1),
            AIFPLNumber(2)
        ))
        
        analyzed = analyzer.analyze(expr)
        
        # Analyzer predicted: condition(1) + POP_JUMP(1) + then(1) + JUMP(1) + else(1) = 5
        assert analyzed.instruction_count == 5
        
        generator = AIFPLGenerator(analyzed, analyzer.constants, [], [])
        code = generator.generate()
        
        # Generator should emit exactly 5 instructions (plus RETURN)
        assert len(code.instructions) == 6  # 5 + RETURN
    
    def test_if_instruction_count_complex(self):
        """Test instruction count for complex if."""
        analyzer = AIFPLAnalyzer()
        
        # (if (> x 0) (+ 1 2) (+ 3 4))
        expr = AIFPLList((
            AIFPLSymbol("if"),
            AIFPLList((
                AIFPLSymbol(">"),
                AIFPLSymbol("x"),
                AIFPLNumber(0)
            )),
            AIFPLList((AIFPLSymbol("+"), AIFPLNumber(1), AIFPLNumber(2))),
            AIFPLList((AIFPLSymbol("+"), AIFPLNumber(3), AIFPLNumber(4)))
        ))
        
        analyzed = analyzer.analyze(expr)
        predicted_count = analyzed.instruction_count
        
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, [])
        code = generator.generate()
        
        # Generator should emit predicted count (plus RETURN)
        assert len(code.instructions) == predicted_count + 1


class TestGenerateIfNoPatching:
    """Test that NO patching opcodes are emitted."""
    
    def test_no_patch_opcodes_in_if(self):
        """Test that if generation never emits PATCH opcodes."""
        analyzer = AIFPLAnalyzer()
        
        # (if #t 1 2)
        expr = AIFPLList((
            AIFPLSymbol("if"),
            AIFPLBoolean(True),
            AIFPLNumber(1),
            AIFPLNumber(2)
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, [], [])
        code = generator.generate()
        
        # NO PATCH opcodes should be present
        for instr in code.instructions:
            assert instr.opcode != Opcode.PATCH_CLOSURE_SELF
            assert instr.opcode != Opcode.PATCH_CLOSURE_SIBLING
    
    def test_no_patch_opcodes_in_nested_if(self):
        """Test that nested if generation never emits PATCH opcodes."""
        analyzer = AIFPLAnalyzer()
        
        # (if #t (if #f 1 2) (if #t 3 4))
        expr = AIFPLList((
            AIFPLSymbol("if"),
            AIFPLBoolean(True),
            AIFPLList((
                AIFPLSymbol("if"),
                AIFPLBoolean(False),
                AIFPLNumber(1),
                AIFPLNumber(2)
            )),
            AIFPLList((
                AIFPLSymbol("if"),
                AIFPLBoolean(True),
                AIFPLNumber(3),
                AIFPLNumber(4)
            ))
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, [], [])
        code = generator.generate()
        
        # NO PATCH opcodes anywhere
        for instr in code.instructions:
            assert instr.opcode != Opcode.PATCH_CLOSURE_SELF
            assert instr.opcode != Opcode.PATCH_CLOSURE_SIBLING
