"""Tests for AIFPL Generator - And/Or expression generation.

These tests verify that and/or expressions generate correct bytecode
with pre-calculated jump offsets (NO PATCHING!).
"""

import pytest
from aifpl.aifpl_generator import AIFPLGenerator
from aifpl.aifpl_analyzer import AIFPLAnalyzer
from aifpl.aifpl_bytecode import CodeObject, Opcode
from aifpl.aifpl_value import (
    AIFPLBoolean,
    AIFPLSymbol,
    AIFPLList,
)


class TestGenerateAndBasic:
    """Test basic and expression generation."""
    
    def test_generate_empty_and(self):
        """Test generating (and) -> #t."""
        analyzer = AIFPLAnalyzer()
        
        # (and)
        expr = AIFPLList((AIFPLSymbol("and"),))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, [], [])
        code = generator.generate()
        
        # Expected: LOAD_TRUE, RETURN
        assert len(code.instructions) == 2
        assert code.instructions[0].opcode == Opcode.LOAD_TRUE
        assert code.instructions[1].opcode == Opcode.RETURN
    
    def test_generate_and_single_arg(self):
        """Test generating (and #t)."""
        analyzer = AIFPLAnalyzer()
        
        # (and #t)
        expr = AIFPLList((
            AIFPLSymbol("and"),
            AIFPLBoolean(True)
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, [], [])
        code = generator.generate()
        
        # Expected bytecode:
        # 0: LOAD_TRUE              (arg)
        # 1: POP_JUMP_IF_FALSE 4    (jump to false section)
        # 2: LOAD_TRUE              (all true)
        # 3: JUMP 5                 (jump to end)
        # 4: LOAD_FALSE             (false section)
        # 5: RETURN
        
        assert len(code.instructions) == 6
        assert code.instructions[0].opcode == Opcode.LOAD_TRUE
        assert code.instructions[1].opcode == Opcode.POP_JUMP_IF_FALSE
        assert code.instructions[1].arg1 == 4
        assert code.instructions[2].opcode == Opcode.LOAD_TRUE
        assert code.instructions[3].opcode == Opcode.JUMP
        assert code.instructions[3].arg1 == 5
        assert code.instructions[4].opcode == Opcode.LOAD_FALSE
        assert code.instructions[5].opcode == Opcode.RETURN
    
    def test_generate_and_two_args(self):
        """Test generating (and #t #f)."""
        analyzer = AIFPLAnalyzer()
        
        # (and #t #f)
        expr = AIFPLList((
            AIFPLSymbol("and"),
            AIFPLBoolean(True),
            AIFPLBoolean(False)
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, [], [])
        code = generator.generate()
        
        # Expected bytecode:
        # 0: LOAD_TRUE              (arg1)
        # 1: POP_JUMP_IF_FALSE 6    (jump to false section)
        # 2: LOAD_FALSE             (arg2)
        # 3: POP_JUMP_IF_FALSE 6    (jump to false section)
        # 4: LOAD_TRUE              (all true)
        # 5: JUMP 7                 (jump to end)
        # 6: LOAD_FALSE             (false section)
        # 7: RETURN
        
        assert len(code.instructions) == 8
        assert code.instructions[0].opcode == Opcode.LOAD_TRUE
        assert code.instructions[1].opcode == Opcode.POP_JUMP_IF_FALSE
        assert code.instructions[1].arg1 == 6
        assert code.instructions[2].opcode == Opcode.LOAD_FALSE
        assert code.instructions[3].opcode == Opcode.POP_JUMP_IF_FALSE
        assert code.instructions[3].arg1 == 6
        assert code.instructions[4].opcode == Opcode.LOAD_TRUE
        assert code.instructions[5].opcode == Opcode.JUMP
        assert code.instructions[5].arg1 == 7
        assert code.instructions[6].opcode == Opcode.LOAD_FALSE
        assert code.instructions[7].opcode == Opcode.RETURN


class TestGenerateOrBasic:
    """Test basic or expression generation."""
    
    def test_generate_empty_or(self):
        """Test generating (or) -> #f."""
        analyzer = AIFPLAnalyzer()
        
        # (or)
        expr = AIFPLList((AIFPLSymbol("or"),))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, [], [])
        code = generator.generate()
        
        # Expected: LOAD_FALSE, RETURN
        assert len(code.instructions) == 2
        assert code.instructions[0].opcode == Opcode.LOAD_FALSE
        assert code.instructions[1].opcode == Opcode.RETURN
    
    def test_generate_or_single_arg(self):
        """Test generating (or #f)."""
        analyzer = AIFPLAnalyzer()
        
        # (or #f)
        expr = AIFPLList((
            AIFPLSymbol("or"),
            AIFPLBoolean(False)
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, [], [])
        code = generator.generate()
        
        # Expected bytecode:
        # 0: LOAD_FALSE             (arg)
        # 1: POP_JUMP_IF_TRUE 4     (jump to true section)
        # 2: LOAD_FALSE             (all false)
        # 3: JUMP 5                 (jump to end)
        # 4: LOAD_TRUE              (true section)
        # 5: RETURN
        
        assert len(code.instructions) == 6
        assert code.instructions[0].opcode == Opcode.LOAD_FALSE
        assert code.instructions[1].opcode == Opcode.POP_JUMP_IF_TRUE
        assert code.instructions[1].arg1 == 4
        assert code.instructions[2].opcode == Opcode.LOAD_FALSE
        assert code.instructions[3].opcode == Opcode.JUMP
        assert code.instructions[3].arg1 == 5
        assert code.instructions[4].opcode == Opcode.LOAD_TRUE
        assert code.instructions[5].opcode == Opcode.RETURN
    
    def test_generate_or_two_args(self):
        """Test generating (or #f #t)."""
        analyzer = AIFPLAnalyzer()
        
        # (or #f #t)
        expr = AIFPLList((
            AIFPLSymbol("or"),
            AIFPLBoolean(False),
            AIFPLBoolean(True)
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, [], [])
        code = generator.generate()
        
        # Expected bytecode:
        # 0: LOAD_FALSE             (arg1)
        # 1: POP_JUMP_IF_TRUE 6     (jump to true section)
        # 2: LOAD_TRUE              (arg2)
        # 3: POP_JUMP_IF_TRUE 6     (jump to true section)
        # 4: LOAD_FALSE             (all false)
        # 5: JUMP 7                 (jump to end)
        # 6: LOAD_TRUE              (true section)
        # 7: RETURN
        
        assert len(code.instructions) == 8
        assert code.instructions[0].opcode == Opcode.LOAD_FALSE
        assert code.instructions[1].opcode == Opcode.POP_JUMP_IF_TRUE
        assert code.instructions[1].arg1 == 6
        assert code.instructions[2].opcode == Opcode.LOAD_TRUE
        assert code.instructions[3].opcode == Opcode.POP_JUMP_IF_TRUE
        assert code.instructions[3].arg1 == 6
        assert code.instructions[4].opcode == Opcode.LOAD_FALSE
        assert code.instructions[5].opcode == Opcode.JUMP
        assert code.instructions[5].arg1 == 7
        assert code.instructions[6].opcode == Opcode.LOAD_TRUE
        assert code.instructions[7].opcode == Opcode.RETURN


class TestGenerateAndOrJumpOffsets:
    """Test that jump offsets are correct (NO PATCHING!)."""
    
    def test_and_jump_offsets_not_zero(self):
        """Test that and jump offsets are NOT zero placeholders."""
        analyzer = AIFPLAnalyzer()
        
        # (and #t #f)
        expr = AIFPLList((
            AIFPLSymbol("and"),
            AIFPLBoolean(True),
            AIFPLBoolean(False)
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, [], [])
        code = generator.generate()
        
        # All jump targets should be correct, not 0 placeholders
        pop_jump1 = code.instructions[1]
        assert pop_jump1.opcode == Opcode.POP_JUMP_IF_FALSE
        assert pop_jump1.arg1 != 0
        assert pop_jump1.arg1 == 6
        
        pop_jump2 = code.instructions[3]
        assert pop_jump2.opcode == Opcode.POP_JUMP_IF_FALSE
        assert pop_jump2.arg1 != 0
        assert pop_jump2.arg1 == 6
        
        jump = code.instructions[5]
        assert jump.opcode == Opcode.JUMP
        assert jump.arg1 != 0
        assert jump.arg1 == 7
    
    def test_or_jump_offsets_not_zero(self):
        """Test that or jump offsets are NOT zero placeholders."""
        analyzer = AIFPLAnalyzer()
        
        # (or #f #t)
        expr = AIFPLList((
            AIFPLSymbol("or"),
            AIFPLBoolean(False),
            AIFPLBoolean(True)
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, [], [])
        code = generator.generate()
        
        # All jump targets should be correct, not 0 placeholders
        pop_jump1 = code.instructions[1]
        assert pop_jump1.opcode == Opcode.POP_JUMP_IF_TRUE
        assert pop_jump1.arg1 != 0
        assert pop_jump1.arg1 == 6
        
        pop_jump2 = code.instructions[3]
        assert pop_jump2.opcode == Opcode.POP_JUMP_IF_TRUE
        assert pop_jump2.arg1 != 0
        assert pop_jump2.arg1 == 6
        
        jump = code.instructions[5]
        assert jump.opcode == Opcode.JUMP
        assert jump.arg1 != 0
        assert jump.arg1 == 7


class TestGenerateAndOrNested:
    """Test nested and/or expressions."""
    
    def test_generate_nested_and_in_and(self):
        """Test generating (and (and #t #f) #t)."""
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
        generator = AIFPLGenerator(analyzed, analyzer.constants, [], [])
        code = generator.generate()
        
        # Should have multiple POP_JUMP_IF_FALSE and JUMP instructions
        pop_jumps = [i for i in code.instructions if i.opcode == Opcode.POP_JUMP_IF_FALSE]
        jumps = [i for i in code.instructions if i.opcode == Opcode.JUMP]
        
        # Inner and has 2 POP_JUMP_IF_FALSE, outer and has 2 (one for nested result, one for #t)
        assert len(pop_jumps) == 4
        # Inner and has 1 JUMP, outer and has 1
        assert len(jumps) == 2
        
        # All jumps should be non-zero
        for jump in pop_jumps + jumps:
            assert jump.arg1 > 0
    
    def test_generate_nested_or_in_or(self):
        """Test generating (or (or #f #t) #f)."""
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
        generator = AIFPLGenerator(analyzed, analyzer.constants, [], [])
        code = generator.generate()
        
        # Should have multiple POP_JUMP_IF_TRUE and JUMP instructions
        pop_jumps = [i for i in code.instructions if i.opcode == Opcode.POP_JUMP_IF_TRUE]
        jumps = [i for i in code.instructions if i.opcode == Opcode.JUMP]
        
        # Inner or has 2 POP_JUMP_IF_TRUE, outer or has 2 (one for nested result, one for #f)
        assert len(pop_jumps) == 4
        # Inner or has 1 JUMP, outer or has 1
        assert len(jumps) == 2
        
        # All jumps should be non-zero
        for jump in pop_jumps + jumps:
            assert jump.arg1 > 0


class TestGenerateAndOrNoPatching:
    """Test that NO patching opcodes are emitted."""
    
    def test_no_patch_opcodes_in_and(self):
        """Test that and generation never emits PATCH opcodes."""
        analyzer = AIFPLAnalyzer()
        
        # (and #t #f #t)
        expr = AIFPLList((
            AIFPLSymbol("and"),
            AIFPLBoolean(True),
            AIFPLBoolean(False),
            AIFPLBoolean(True)
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, [], [])
        code = generator.generate()
        
        # NO PATCH opcodes should be present
        for instr in code.instructions:
            assert instr.opcode != Opcode.PATCH_CLOSURE_SELF
            assert instr.opcode != Opcode.PATCH_CLOSURE_SIBLING
    
    def test_no_patch_opcodes_in_or(self):
        """Test that or generation never emits PATCH opcodes."""
        analyzer = AIFPLAnalyzer()
        
        # (or #f #t #f)
        expr = AIFPLList((
            AIFPLSymbol("or"),
            AIFPLBoolean(False),
            AIFPLBoolean(True),
            AIFPLBoolean(False)
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, [], [])
        code = generator.generate()
        
        # NO PATCH opcodes should be present
        for instr in code.instructions:
            assert instr.opcode != Opcode.PATCH_CLOSURE_SELF
            assert instr.opcode != Opcode.PATCH_CLOSURE_SIBLING
