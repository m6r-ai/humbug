"""Tests for AIFPL Generator - Basic functionality.

These tests verify the generator skeleton and basic setup.
"""

import pytest
from aifpl.aifpl_generator import AIFPLGenerator
from aifpl.aifpl_analyzer import AIFPLAnalyzer
from aifpl.aifpl_analysis_ir import (
    AnalyzedLiteral,
    AnalyzedVariable,
)
from aifpl.aifpl_bytecode import CodeObject, Opcode
from aifpl.aifpl_value import (
    AIFPLNumber,
    AIFPLString,
    AIFPLBoolean,
    AIFPLSymbol,
)


class TestGeneratorCreation:
    """Test generator creation and initialization."""
    
    def test_create_generator(self):
        """Test creating a generator."""
        # Analyze a simple expression
        analyzer = AIFPLAnalyzer()
        analyzed = analyzer.analyze(AIFPLNumber(42))
        
        # Create generator
        generator = AIFPLGenerator(
            analyzed=analyzed,
            constants=analyzer.constants,
            names=analyzer.names,
            code_objects=analyzer.code_objects
        )
        
        assert generator.analyzed == analyzed
        assert generator.constants == analyzer.constants
        assert generator.names == analyzer.names
        assert len(generator.instructions) == 0
    
    def test_generator_with_empty_pools(self):
        """Test generator with empty constant/name pools."""
        analyzer = AIFPLAnalyzer()
        analyzed = analyzer.analyze(AIFPLBoolean(True))
        
        generator = AIFPLGenerator(
            analyzed=analyzed,
            constants=[],
            names=[],
            code_objects=[]
        )
        
        assert len(generator.constants) == 0
        assert len(generator.names) == 0


class TestGeneratorEmit:
    """Test instruction emission."""
    
    def test_emit_instruction(self):
        """Test emitting a single instruction."""
        analyzer = AIFPLAnalyzer()
        analyzed = analyzer.analyze(AIFPLNumber(1))
        
        generator = AIFPLGenerator(analyzed, [], [], [])
        
        # Emit an instruction
        index = generator._emit(Opcode.LOAD_TRUE)
        
        assert index == 0
        assert len(generator.instructions) == 1
        assert generator.instructions[0].opcode == Opcode.LOAD_TRUE
    
    def test_emit_instruction_with_args(self):
        """Test emitting instruction with arguments."""
        analyzer = AIFPLAnalyzer()
        analyzed = analyzer.analyze(AIFPLNumber(1))
        
        generator = AIFPLGenerator(analyzed, [], [], [])
        
        # Emit instruction with args
        index = generator._emit(Opcode.LOAD_VAR, 2, 3)
        
        assert index == 0
        assert len(generator.instructions) == 1
        assert generator.instructions[0].opcode == Opcode.LOAD_VAR
        assert generator.instructions[0].arg1 == 2
        assert generator.instructions[0].arg2 == 3
    
    def test_emit_multiple_instructions(self):
        """Test emitting multiple instructions."""
        analyzer = AIFPLAnalyzer()
        analyzed = analyzer.analyze(AIFPLNumber(1))
        
        generator = AIFPLGenerator(analyzed, [], [], [])
        
        # Emit several instructions
        idx1 = generator._emit(Opcode.LOAD_TRUE)
        idx2 = generator._emit(Opcode.LOAD_FALSE)
        idx3 = generator._emit(Opcode.LOAD_CONST, 5)
        
        assert idx1 == 0
        assert idx2 == 1
        assert idx3 == 2
        assert len(generator.instructions) == 3


class TestGeneratorCurrentIP:
    """Test current instruction pointer tracking."""
    
    def test_current_ip_empty(self):
        """Test current IP with no instructions."""
        analyzer = AIFPLAnalyzer()
        analyzed = analyzer.analyze(AIFPLNumber(1))
        
        generator = AIFPLGenerator(analyzed, [], [], [])
        
        assert generator._current_ip() == 0
    
    def test_current_ip_after_emit(self):
        """Test current IP after emitting instructions."""
        analyzer = AIFPLAnalyzer()
        analyzed = analyzer.analyze(AIFPLNumber(1))
        
        generator = AIFPLGenerator(analyzed, [], [], [])
        
        generator._emit(Opcode.LOAD_TRUE)
        assert generator._current_ip() == 1
        
        generator._emit(Opcode.LOAD_FALSE)
        assert generator._current_ip() == 2
        
        generator._emit(Opcode.RETURN)
        assert generator._current_ip() == 3


# Note: Stub tests removed since literal, variable, if, let, quote, call, and list 
# generation are now implemented. Lambda generation is the remaining major feature.


class TestGeneratorIntegration:
    """Test generator integration with analyzer."""
    
    def test_analyze_then_generate_setup(self):
        """Test that analyzer output can be fed to generator."""
        # Analyze an expression
        analyzer = AIFPLAnalyzer()
        expr = AIFPLNumber(42)
        analyzed = analyzer.analyze(expr)
        
        # Create generator with analyzer output
        generator = AIFPLGenerator(
            analyzed=analyzed,
            constants=analyzer.constants,
            names=analyzer.names,
            code_objects=analyzer.code_objects
        )
        
        # Verify setup
        assert generator.analyzed is not None
        assert isinstance(generator.analyzed, AnalyzedLiteral)
        assert len(generator.constants) == 1
        assert generator.constants[0] == expr
    
    def test_constant_pool_shared(self):
        """Test that constant pool is shared between analyzer and generator."""
        analyzer = AIFPLAnalyzer()
        
        # Analyze multiple literals
        num1 = AIFPLNumber(42)
        analyzer.analyze(num1)
        
        num2 = AIFPLNumber(100)
        analyzed = analyzer.analyze(num2)
        
        # Generator should have access to all constants
        generator = AIFPLGenerator(
            analyzed=analyzed,
            constants=analyzer.constants,
            names=analyzer.names,
            code_objects=analyzer.code_objects
        )
        
        # Both constants should be in the pool
        assert len(generator.constants) == 2
        assert num1 in generator.constants
        assert num2 in generator.constants
