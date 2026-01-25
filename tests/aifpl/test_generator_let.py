"""Tests for AIFPL Generator - Let expression generation.

These tests verify code generation for let expressions with:
- Simple bindings
- Multiple bindings
- Bindings with expressions
- Recursive bindings (when lambda support is added)
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


class TestGenerateLetBasic:
    """Test basic let expression generation."""
    
    def test_generate_simple_let(self):
        """Test generating code for simple let with one binding."""
        analyzer = AIFPLAnalyzer()
        
        # (let ((x 5)) x)
        expr = AIFPLList((
            AIFPLSymbol("let"),
            AIFPLList((
                AIFPLList((AIFPLSymbol("x"), AIFPLNumber(5))),
            )),
            AIFPLSymbol("x")
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, [])
        code = generator.generate()
        
        # Expected bytecode:
        # 0: LOAD_CONST 0      (5)
        # 1: STORE_VAR 0 0     (store to x)
        # 2: LOAD_VAR 0 0      (load x)
        # 3: RETURN
        
        assert len(code.instructions) == 4
        assert code.instructions[0].opcode == Opcode.LOAD_CONST
        assert code.instructions[1].opcode == Opcode.STORE_VAR
        assert code.instructions[1].arg1 == 0  # depth
        assert code.instructions[1].arg2 == 0  # index
        assert code.instructions[2].opcode == Opcode.LOAD_VAR
        assert code.instructions[2].arg1 == 0  # depth
        assert code.instructions[2].arg2 == 0  # index
        assert code.instructions[3].opcode == Opcode.RETURN
    
    def test_generate_let_multiple_bindings(self):
        """Test generating code for let with multiple bindings."""
        analyzer = AIFPLAnalyzer()
        
        # (let ((x 5) (y 10)) (+ x y))
        expr = AIFPLList((
            AIFPLSymbol("let"),
            AIFPLList((
                AIFPLList((AIFPLSymbol("x"), AIFPLNumber(5))),
                AIFPLList((AIFPLSymbol("y"), AIFPLNumber(10))),
            )),
            AIFPLList((AIFPLSymbol("+"), AIFPLSymbol("x"), AIFPLSymbol("y")))
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, [])
        code = generator.generate()
        
        # Expected bytecode:
        # 0: LOAD_CONST 0      (5)
        # 1: STORE_VAR 0 0     (store to x)
        # 2: LOAD_CONST 1      (10)
        # 3: STORE_VAR 0 1     (store to y)
        # 4: LOAD_VAR 0 0      (x)
        # 5: LOAD_VAR 0 1      (y)
        # 6: CALL_BUILTIN ...
        # 7: RETURN
        
        assert len(code.instructions) == 8
        assert code.instructions[0].opcode == Opcode.LOAD_CONST
        assert code.instructions[1].opcode == Opcode.STORE_VAR
        assert code.instructions[1].arg2 == 0  # x at index 0
        assert code.instructions[2].opcode == Opcode.LOAD_CONST
        assert code.instructions[3].opcode == Opcode.STORE_VAR
        assert code.instructions[3].arg2 == 1  # y at index 1


class TestGenerateLetWithExpressions:
    """Test let with expression values."""
    
    def test_generate_let_with_call_value(self):
        """Test let binding with function call as value."""
        analyzer = AIFPLAnalyzer()
        
        # (let ((x (+ 1 2))) x)
        expr = AIFPLList((
            AIFPLSymbol("let"),
            AIFPLList((
                AIFPLList((
                    AIFPLSymbol("x"),
                    AIFPLList((AIFPLSymbol("+"), AIFPLNumber(1), AIFPLNumber(2)))
                )),
            )),
            AIFPLSymbol("x")
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, [])
        code = generator.generate()
        
        # Binding value: LOAD_CONST(1), LOAD_CONST(2), CALL_BUILTIN
        # Store: STORE_VAR
        # Body: LOAD_VAR
        # End: RETURN
        
        assert code.instructions[0].opcode == Opcode.LOAD_CONST  # 1
        assert code.instructions[1].opcode == Opcode.LOAD_CONST  # 2
        assert code.instructions[2].opcode == Opcode.CALL_BUILTIN
        assert code.instructions[3].opcode == Opcode.STORE_VAR
        assert code.instructions[4].opcode == Opcode.LOAD_VAR
        assert code.instructions[5].opcode == Opcode.RETURN
    
    def test_generate_let_with_if_value(self):
        """Test let binding with if expression as value."""
        analyzer = AIFPLAnalyzer()
        
        # (let ((x (if #t 1 2))) x)
        expr = AIFPLList((
            AIFPLSymbol("let"),
            AIFPLList((
                AIFPLList((
                    AIFPLSymbol("x"),
                    AIFPLList((
                        AIFPLSymbol("if"),
                        AIFPLBoolean(True),
                        AIFPLNumber(1),
                        AIFPLNumber(2)
                    ))
                )),
            )),
            AIFPLSymbol("x")
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, [], [])
        code = generator.generate()
        
        # If expression, then STORE_VAR, then LOAD_VAR, then RETURN
        store_var_instr = None
        for i, instr in enumerate(code.instructions):
            if instr.opcode == Opcode.STORE_VAR:
                store_var_instr = (i, instr)
                break
        
        assert store_var_instr is not None
        idx, instr = store_var_instr
        assert code.instructions[idx + 1].opcode == Opcode.LOAD_VAR


class TestGenerateLetNested:
    """Test nested let expressions."""
    
    def test_generate_nested_let(self):
        """Test generating code for nested let expressions."""
        analyzer = AIFPLAnalyzer()
        
        # (let ((x 5)) (let ((y 10)) (+ x y)))
        expr = AIFPLList((
            AIFPLSymbol("let"),
            AIFPLList((
                AIFPLList((AIFPLSymbol("x"), AIFPLNumber(5))),
            )),
            AIFPLList((
                AIFPLSymbol("let"),
                AIFPLList((
                    AIFPLList((AIFPLSymbol("y"), AIFPLNumber(10))),
                )),
                AIFPLList((AIFPLSymbol("+"), AIFPLSymbol("x"), AIFPLSymbol("y")))
            ))
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, [])
        code = generator.generate()
        
        # Outer let: LOAD_CONST(5), STORE_VAR(0,0)
        # Inner let: LOAD_CONST(10), STORE_VAR(0,1)
        # Body: LOAD_NAME(+), LOAD_VAR(x), LOAD_VAR(y), CALL_BUILTIN
        # RETURN
        
        store_vars = [i for i in code.instructions if i.opcode == Opcode.STORE_VAR]
        assert len(store_vars) == 2
        
        load_vars = [i for i in code.instructions if i.opcode == Opcode.LOAD_VAR]
        assert len(load_vars) == 2


class TestGenerateLetInstructionCounts:
    """Test that instruction counts match analysis."""
    
    def test_let_instruction_count_simple(self):
        """Test instruction count for simple let."""
        analyzer = AIFPLAnalyzer()
        
        # (let ((x 5)) x)
        expr = AIFPLList((
            AIFPLSymbol("let"),
            AIFPLList((
                AIFPLList((AIFPLSymbol("x"), AIFPLNumber(5))),
            )),
            AIFPLSymbol("x")
        ))
        
        analyzed = analyzer.analyze(expr)
        predicted_count = analyzed.instruction_count
        
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, [])
        code = generator.generate()
        
        # Generator should emit predicted count (plus RETURN)
        assert len(code.instructions) == predicted_count + 1
    
    def test_let_instruction_count_multiple(self):
        """Test instruction count for let with multiple bindings."""
        analyzer = AIFPLAnalyzer()
        
        # (let ((x 5) (y 10) (z 15)) (+ x (+ y z)))
        expr = AIFPLList((
            AIFPLSymbol("let"),
            AIFPLList((
                AIFPLList((AIFPLSymbol("x"), AIFPLNumber(5))),
                AIFPLList((AIFPLSymbol("y"), AIFPLNumber(10))),
                AIFPLList((AIFPLSymbol("z"), AIFPLNumber(15))),
            )),
            AIFPLList((
                AIFPLSymbol("+"),
                AIFPLSymbol("x"),
                AIFPLList((AIFPLSymbol("+"), AIFPLSymbol("y"), AIFPLSymbol("z")))
            ))
        ))
        
        analyzed = analyzer.analyze(expr)
        predicted_count = analyzed.instruction_count
        
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, [])
        code = generator.generate()
        
        # Generator should emit predicted count (plus RETURN)
        assert len(code.instructions) == predicted_count + 1


class TestGenerateLetNoPatching:
    """Test that let generation never emits PATCH opcodes."""
    
    def test_no_patch_opcodes_in_let(self):
        """Test that simple let never emits PATCH opcodes."""
        analyzer = AIFPLAnalyzer()
        
        # (let ((x 5)) x)
        expr = AIFPLList((
            AIFPLSymbol("let"),
            AIFPLList((
                AIFPLList((AIFPLSymbol("x"), AIFPLNumber(5))),
            )),
            AIFPLSymbol("x")
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, [], [])
        code = generator.generate()
        
        # NO PATCH opcodes
        for instr in code.instructions:
            assert instr.opcode != Opcode.PATCH_CLOSURE_SELF
            assert instr.opcode != Opcode.PATCH_CLOSURE_SIBLING
    
    def test_no_patch_opcodes_in_nested_let(self):
        """Test that nested let never emits PATCH opcodes."""
        analyzer = AIFPLAnalyzer()
        
        # (let ((x 5)) (let ((y 10)) (let ((z 15)) (+ x (+ y z)))))
        expr = AIFPLList((
            AIFPLSymbol("let"),
            AIFPLList((
                AIFPLList((AIFPLSymbol("x"), AIFPLNumber(5))),
            )),
            AIFPLList((
                AIFPLSymbol("let"),
                AIFPLList((
                    AIFPLList((AIFPLSymbol("y"), AIFPLNumber(10))),
                )),
                AIFPLList((
                    AIFPLSymbol("let"),
                    AIFPLList((
                        AIFPLList((AIFPLSymbol("z"), AIFPLNumber(15))),
                    )),
                    AIFPLList((
                        AIFPLSymbol("+"),
                        AIFPLSymbol("x"),
                        AIFPLList((AIFPLSymbol("+"), AIFPLSymbol("y"), AIFPLSymbol("z")))
                    ))
                ))
            ))
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, [])
        code = generator.generate()
        
        # NO PATCH opcodes anywhere
        for instr in code.instructions:
            assert instr.opcode != Opcode.PATCH_CLOSURE_SELF
            assert instr.opcode != Opcode.PATCH_CLOSURE_SIBLING


class TestGenerateLetVariableScoping:
    """Test proper variable scoping in let expressions."""
    
    def test_let_variable_indices(self):
        """Test that variables get correct indices."""
        analyzer = AIFPLAnalyzer()
        
        # (let ((a 1) (b 2) (c 3)) c)
        expr = AIFPLList((
            AIFPLSymbol("let"),
            AIFPLList((
                AIFPLList((AIFPLSymbol("a"), AIFPLNumber(1))),
                AIFPLList((AIFPLSymbol("b"), AIFPLNumber(2))),
                AIFPLList((AIFPLSymbol("c"), AIFPLNumber(3))),
            )),
            AIFPLSymbol("c")
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, [], [])
        code = generator.generate()
        
        # Find STORE_VAR instructions
        store_vars = [i for i in code.instructions if i.opcode == Opcode.STORE_VAR]
        assert len(store_vars) == 3
        
        # Check indices: should be 0, 1, 2
        assert store_vars[0].arg2 == 0  # a
        assert store_vars[1].arg2 == 1  # b
        assert store_vars[2].arg2 == 2  # c
        
        # Body should load c (index 2)
        load_var = [i for i in code.instructions if i.opcode == Opcode.LOAD_VAR][0]
        assert load_var.arg2 == 2  # c
