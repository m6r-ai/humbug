"""Tests for AIFPL Generator - Expression generation.

These tests verify code generation for simple expressions:
literals, variables, calls, quote, and list construction.
"""

import pytest
from aifpl.aifpl_generator import AIFPLGenerator
from aifpl.aifpl_analyzer import AIFPLAnalyzer
from aifpl.aifpl_bytecode import CodeObject, Opcode
from aifpl.aifpl_value import (
    AIFPLNumber,
    AIFPLString,
    AIFPLBoolean,
    AIFPLSymbol,
    AIFPLList,
)


class TestGenerateLiterals:
    """Test literal code generation."""
    
    def test_generate_number(self):
        """Test generating code for a number literal."""
        analyzer = AIFPLAnalyzer()
        analyzed = analyzer.analyze(AIFPLNumber(42))
        
        generator = AIFPLGenerator(analyzed, analyzer.constants, [], [])
        code = generator.generate()
        
        # Should have: LOAD_CONST, RETURN
        assert len(code.instructions) == 2
        assert code.instructions[0].opcode == Opcode.LOAD_CONST
        assert code.instructions[0].arg1 == 0  # Index in constant pool
        assert code.instructions[1].opcode == Opcode.RETURN
    
    def test_generate_string(self):
        """Test generating code for a string literal."""
        analyzer = AIFPLAnalyzer()
        analyzed = analyzer.analyze(AIFPLString("hello"))
        
        generator = AIFPLGenerator(analyzed, analyzer.constants, [], [])
        code = generator.generate()
        
        # Should have: LOAD_CONST, RETURN
        assert len(code.instructions) == 2
        assert code.instructions[0].opcode == Opcode.LOAD_CONST
        assert code.instructions[0].arg1 == 0
        assert code.instructions[1].opcode == Opcode.RETURN
    
    def test_generate_boolean_true(self):
        """Test generating code for boolean true."""
        analyzer = AIFPLAnalyzer()
        analyzed = analyzer.analyze(AIFPLBoolean(True))
        
        generator = AIFPLGenerator(analyzed, analyzer.constants, [], [])
        code = generator.generate()
        
        # Should have: LOAD_TRUE, RETURN
        assert len(code.instructions) == 2
        assert code.instructions[0].opcode == Opcode.LOAD_TRUE
        assert code.instructions[1].opcode == Opcode.RETURN
    
    def test_generate_boolean_false(self):
        """Test generating code for boolean false."""
        analyzer = AIFPLAnalyzer()
        analyzed = analyzer.analyze(AIFPLBoolean(False))
        
        generator = AIFPLGenerator(analyzed, analyzer.constants, [], [])
        code = generator.generate()
        
        # Should have: LOAD_FALSE, RETURN
        assert len(code.instructions) == 2
        assert code.instructions[0].opcode == Opcode.LOAD_FALSE
        assert code.instructions[1].opcode == Opcode.RETURN


class TestGenerateVariables:
    """Test variable code generation."""
    
    def test_generate_global_variable(self):
        """Test generating code for a global variable."""
        analyzer = AIFPLAnalyzer()
        analyzed = analyzer.analyze(AIFPLSymbol("x"))
        
        generator = AIFPLGenerator(analyzed, [], analyzer.names, [])
        code = generator.generate()
        
        # Should have: LOAD_NAME, RETURN
        assert len(code.instructions) == 2
        assert code.instructions[0].opcode == Opcode.LOAD_NAME
        assert code.instructions[0].arg1 >= 0  # Index in name pool
        assert code.instructions[1].opcode == Opcode.RETURN
    
    def test_generate_builtin_variable(self):
        """Test generating code for a builtin reference."""
        analyzer = AIFPLAnalyzer()
        analyzed = analyzer.analyze(AIFPLSymbol("+"))
        
        generator = AIFPLGenerator(analyzed, [], analyzer.names, [])
        code = generator.generate()
        
        # Should have: LOAD_NAME, RETURN
        assert len(code.instructions) == 2
        assert code.instructions[0].opcode == Opcode.LOAD_NAME
        assert code.instructions[1].opcode == Opcode.RETURN


class TestGenerateCalls:
    """Test function call code generation."""
    
    def test_generate_builtin_call(self):
        """Test generating code for a builtin function call."""
        analyzer = AIFPLAnalyzer()
        
        # (+ 1 2)
        expr = AIFPLList((
            AIFPLSymbol("+"),
            AIFPLNumber(1),
            AIFPLNumber(2)
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, [])
        code = generator.generate()
        
        # Should have: LOAD_NAME(+), LOAD_CONST(1), LOAD_CONST(2), CALL_BUILTIN, RETURN
        assert len(code.instructions) == 5
        assert code.instructions[0].opcode == Opcode.LOAD_NAME  # +
        assert code.instructions[1].opcode == Opcode.LOAD_CONST  # 1
        assert code.instructions[2].opcode == Opcode.LOAD_CONST  # 2
        assert code.instructions[3].opcode == Opcode.CALL_BUILTIN
        assert code.instructions[3].arg2 == 2  # 2 arguments
        assert code.instructions[4].opcode == Opcode.RETURN
    
    def test_generate_builtin_call_no_args(self):
        """Test generating code for builtin with no arguments."""
        analyzer = AIFPLAnalyzer()
        
        # (list)
        expr = AIFPLList((AIFPLSymbol("list"),))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, [])
        code = generator.generate()
        
        # Should have: MAKE_LIST(0), RETURN
        assert len(code.instructions) == 2
        assert code.instructions[0].opcode == Opcode.MAKE_LIST
        assert code.instructions[0].arg1 == 0  # 0 elements
        assert code.instructions[1].opcode == Opcode.RETURN
    
    def test_generate_nested_calls(self):
        """Test generating code for nested function calls."""
        analyzer = AIFPLAnalyzer()
        
        # (+ 1 (+ 2 3))
        expr = AIFPLList((
            AIFPLSymbol("+"),
            AIFPLNumber(1),
            AIFPLList((
                AIFPLSymbol("+"),
                AIFPLNumber(2),
                AIFPLNumber(3)
            ))
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, [])
        code = generator.generate()
        
        # Outer: LOAD_NAME(+), LOAD_CONST(1), Inner: LOAD_NAME(+), LOAD_CONST(2), LOAD_CONST(3), CALL_BUILTIN, CALL_BUILTIN, RETURN
        assert len(code.instructions) == 8
        assert code.instructions[-1].opcode == Opcode.RETURN


class TestGenerateQuote:
    """Test quote code generation."""
    
    def test_generate_quote_number(self):
        """Test generating code for quoted number."""
        analyzer = AIFPLAnalyzer()
        
        # (quote 42)
        expr = AIFPLList((
            AIFPLSymbol("quote"),
            AIFPLNumber(42)
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, [], [])
        code = generator.generate()
        
        # Should have: LOAD_CONST, RETURN
        assert len(code.instructions) == 2
        assert code.instructions[0].opcode == Opcode.LOAD_CONST
        assert code.instructions[1].opcode == Opcode.RETURN
    
    def test_generate_quote_symbol(self):
        """Test generating code for quoted symbol."""
        analyzer = AIFPLAnalyzer()
        
        # (quote x)
        expr = AIFPLList((
            AIFPLSymbol("quote"),
            AIFPLSymbol("x")
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, [], [])
        code = generator.generate()
        
        # Should have: LOAD_CONST, RETURN
        assert len(code.instructions) == 2
        assert code.instructions[0].opcode == Opcode.LOAD_CONST
        assert code.instructions[1].opcode == Opcode.RETURN
    
    def test_generate_quote_list(self):
        """Test generating code for quoted list."""
        analyzer = AIFPLAnalyzer()
        
        # (quote (a b c))
        expr = AIFPLList((
            AIFPLSymbol("quote"),
            AIFPLList((
                AIFPLSymbol("a"),
                AIFPLSymbol("b"),
                AIFPLSymbol("c")
            ))
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, [], [])
        code = generator.generate()
        
        # Should have: LOAD_CONST, RETURN
        assert len(code.instructions) == 2
        assert code.instructions[0].opcode == Opcode.LOAD_CONST
        assert code.instructions[1].opcode == Opcode.RETURN


class TestGenerateMakeList:
    """Test list construction code generation."""
    
    def test_generate_empty_list(self):
        """Test generating code for empty list."""
        analyzer = AIFPLAnalyzer()
        
        # (list)
        expr = AIFPLList((AIFPLSymbol("list"),))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, [], [])
        code = generator.generate()
        
        # Should have: MAKE_LIST(0), RETURN
        assert len(code.instructions) == 2
        assert code.instructions[0].opcode == Opcode.MAKE_LIST
        assert code.instructions[0].arg1 == 0
        assert code.instructions[1].opcode == Opcode.RETURN
    
    def test_generate_list_with_literals(self):
        """Test generating code for list with literal elements."""
        analyzer = AIFPLAnalyzer()
        
        # (list 1 2 3)
        expr = AIFPLList((
            AIFPLSymbol("list"),
            AIFPLNumber(1),
            AIFPLNumber(2),
            AIFPLNumber(3)
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, [], [])
        code = generator.generate()
        
        # Should have: LOAD_CONST(1), LOAD_CONST(2), LOAD_CONST(3), MAKE_LIST(3), RETURN
        assert len(code.instructions) == 5
        assert code.instructions[0].opcode == Opcode.LOAD_CONST
        assert code.instructions[1].opcode == Opcode.LOAD_CONST
        assert code.instructions[2].opcode == Opcode.LOAD_CONST
        assert code.instructions[3].opcode == Opcode.MAKE_LIST
        assert code.instructions[3].arg1 == 3
        assert code.instructions[4].opcode == Opcode.RETURN
    
    def test_generate_list_with_expressions(self):
        """Test generating code for list with expression elements."""
        analyzer = AIFPLAnalyzer()
        
        # (list (+ 1 2) (+ 3 4))
        expr = AIFPLList((
            AIFPLSymbol("list"),
            AIFPLList((AIFPLSymbol("+"), AIFPLNumber(1), AIFPLNumber(2))),
            AIFPLList((AIFPLSymbol("+"), AIFPLNumber(3), AIFPLNumber(4)))
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, [])
        code = generator.generate()
        
        # First call: LOAD_NAME(+), LOAD_CONST(1), LOAD_CONST(2), CALL_BUILTIN
        # Second call: LOAD_NAME(+), LOAD_CONST(3), LOAD_CONST(4), CALL_BUILTIN
        # List: MAKE_LIST(2)
        # End: RETURN
        assert len(code.instructions) == 10
        assert code.instructions[-2].opcode == Opcode.MAKE_LIST
        assert code.instructions[-2].arg1 == 2
        assert code.instructions[-1].opcode == Opcode.RETURN


class TestInstructionCounts:
    """Test that instruction counts match analyzed predictions."""
    
    def test_literal_instruction_count(self):
        """Test that literal generates expected number of instructions."""
        analyzer = AIFPLAnalyzer()
        analyzed = analyzer.analyze(AIFPLNumber(42))
        
        # Analyzer predicted 1 instruction
        assert analyzed.instruction_count == 1
        
        generator = AIFPLGenerator(analyzed, analyzer.constants, [], [])
        code = generator.generate()
        
        # Generator should emit 1 instruction (plus RETURN)
        assert len(code.instructions) == 2  # LOAD_CONST + RETURN
    
    def test_call_instruction_count(self):
        """Test that call generates expected number of instructions."""
        analyzer = AIFPLAnalyzer()
        
        # (+ 1 2)
        expr = AIFPLList((
            AIFPLSymbol("+"),
            AIFPLNumber(1),
            AIFPLNumber(2)
        ))
        
        analyzed = analyzer.analyze(expr)
        
        # Analyzer predicted: func(1) + arg1(1) + arg2(1) + call(1) = 4
        assert analyzed.instruction_count == 4
        
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, [])
        code = generator.generate()
        
        # Generator should emit 4 instructions (plus RETURN)
        assert len(code.instructions) == 5  # 4 + RETURN
