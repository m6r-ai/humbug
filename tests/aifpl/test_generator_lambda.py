"""Tests for AIFPL Generator - Lambda expression generation.

These tests verify code generation for lambda expressions:
- Simple lambdas
- Lambdas with parameters
- Lambdas with closures (free variables)
- Recursive lambdas (when fully implemented)
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


class TestGenerateLambdaBasic:
    """Test basic lambda generation."""
    
    def test_generate_simple_lambda(self):
        """Test generating code for simple lambda."""
        analyzer = AIFPLAnalyzer()
        
        # (lambda (x) x)
        expr = AIFPLList((
            AIFPLSymbol("lambda"),
            AIFPLList((AIFPLSymbol("x"),)),
            AIFPLSymbol("x")
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, analyzer.code_objects)
        code = generator.generate()
        
        # Main code: MAKE_CLOSURE, RETURN
        assert len(code.instructions) == 2
        assert code.instructions[0].opcode == Opcode.MAKE_CLOSURE
        assert code.instructions[1].opcode == Opcode.RETURN
        
        # Should have generated a nested code object
        assert len(code.code_objects) == 1
        
        # Lambda body should be: STORE_VAR (store parameter), LOAD_VAR (x), RETURN
        lambda_code = code.code_objects[0]
        assert len(lambda_code.instructions) == 3
        assert lambda_code.instructions[0].opcode == Opcode.STORE_VAR  # Store parameter
        assert lambda_code.instructions[1].opcode == Opcode.LOAD_VAR
        assert lambda_code.instructions[2].opcode == Opcode.RETURN
    
    def test_generate_lambda_with_literal_body(self):
        """Test lambda with literal body."""
        analyzer = AIFPLAnalyzer()
        
        # (lambda (x) 42)
        expr = AIFPLList((
            AIFPLSymbol("lambda"),
            AIFPLList((AIFPLSymbol("x"),)),
            AIFPLNumber(42)
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, analyzer.code_objects)
        code = generator.generate()
        
        # Lambda body should be: STORE_VAR (parameter), LOAD_CONST, RETURN
        lambda_code = code.code_objects[0]
        assert lambda_code.instructions[0].opcode == Opcode.STORE_VAR
        assert lambda_code.instructions[1].opcode == Opcode.LOAD_CONST
        assert lambda_code.instructions[2].opcode == Opcode.RETURN
    
    def test_generate_lambda_multiple_params(self):
        """Test lambda with multiple parameters."""
        analyzer = AIFPLAnalyzer()
        
        # (lambda (x y) (+ x y))
        expr = AIFPLList((
            AIFPLSymbol("lambda"),
            AIFPLList((AIFPLSymbol("x"), AIFPLSymbol("y"))),
            AIFPLList((AIFPLSymbol("+"), AIFPLSymbol("x"), AIFPLSymbol("y")))
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, analyzer.code_objects)
        code = generator.generate()
        
        # Should create closure
        assert code.instructions[0].opcode == Opcode.MAKE_CLOSURE
        
        # Lambda body should call +
        lambda_code = code.code_objects[0]
        call_builtin = [i for i in lambda_code.instructions if i.opcode == Opcode.CALL_BUILTIN]
        assert len(call_builtin) == 1


class TestGenerateLambdaInLet:
    """Test lambda generation in let expressions."""
    
    def test_generate_let_with_lambda(self):
        """Test let binding with lambda."""
        analyzer = AIFPLAnalyzer()
        
        # (let ((f (lambda (x) x))) (f 5))
        expr = AIFPLList((
            AIFPLSymbol("let"),
            AIFPLList((
                AIFPLList((
                    AIFPLSymbol("f"),
                    AIFPLList((
                        AIFPLSymbol("lambda"),
                        AIFPLList((AIFPLSymbol("x"),)),
                        AIFPLSymbol("x")
                    ))
                )),
            )),
            AIFPLList((AIFPLSymbol("f"), AIFPLNumber(5)))
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, analyzer.code_objects)
        code = generator.generate()
        
        # Should have: MAKE_CLOSURE, STORE_VAR, LOAD_VAR, LOAD_CONST, CALL_FUNCTION, RETURN
        assert code.instructions[0].opcode == Opcode.MAKE_CLOSURE
        assert code.instructions[1].opcode == Opcode.STORE_VAR
        
        # Should have generated lambda code object
        assert len(code.code_objects) == 1


class TestGenerateLambdaNested:
    """Test nested lambda generation."""
    
    def test_generate_nested_lambda(self):
        """Test lambda returning lambda."""
        analyzer = AIFPLAnalyzer()
        
        # (lambda (x) (lambda (y) (+ x y)))
        expr = AIFPLList((
            AIFPLSymbol("lambda"),
            AIFPLList((AIFPLSymbol("x"),)),
            AIFPLList((
                AIFPLSymbol("lambda"),
                AIFPLList((AIFPLSymbol("y"),)),
                AIFPLList((AIFPLSymbol("+"), AIFPLSymbol("x"), AIFPLSymbol("y")))
            ))
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, analyzer.code_objects)
        code = generator.generate()
        
        # Outer lambda
        assert code.instructions[0].opcode == Opcode.MAKE_CLOSURE
        assert len(code.code_objects) >= 1
        
        # Outer lambda body should create inner lambda
        # But first it stores the parameter
        outer_lambda = code.code_objects[0]
        assert outer_lambda.instructions[0].opcode == Opcode.STORE_VAR  # Store x parameter
        assert outer_lambda.instructions[1].opcode == Opcode.MAKE_CLOSURE  # Create inner lambda


class TestGenerateLambdaCodeObjects:
    """Test code object generation for lambdas."""
    
    def test_lambda_code_object_structure(self):
        """Test that lambda code objects have correct structure."""
        analyzer = AIFPLAnalyzer()
        
        # (lambda (x y) (+ x y))
        expr = AIFPLList((
            AIFPLSymbol("lambda"),
            AIFPLList((AIFPLSymbol("x"), AIFPLSymbol("y"))),
            AIFPLList((AIFPLSymbol("+"), AIFPLSymbol("x"), AIFPLSymbol("y")))
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, analyzer.code_objects)
        code = generator.generate()
        
        # Check code object structure
        lambda_code = code.code_objects[0]
        assert isinstance(lambda_code, CodeObject)
        assert lambda_code.name == "<lambda>"
        assert len(lambda_code.instructions) > 0
        assert lambda_code.instructions[-1].opcode == Opcode.RETURN
    
    def test_multiple_lambdas_generate_multiple_code_objects(self):
        """Test that multiple lambdas generate separate code objects."""
        analyzer = AIFPLAnalyzer()
        
        # (let ((f (lambda (x) x)) (g (lambda (y) y))) (f 1))
        expr = AIFPLList((
            AIFPLSymbol("let"),
            AIFPLList((
                AIFPLList((
                    AIFPLSymbol("f"),
                    AIFPLList((
                        AIFPLSymbol("lambda"),
                        AIFPLList((AIFPLSymbol("x"),)),
                        AIFPLSymbol("x")
                    ))
                )),
                AIFPLList((
                    AIFPLSymbol("g"),
                    AIFPLList((
                        AIFPLSymbol("lambda"),
                        AIFPLList((AIFPLSymbol("y"),)),
                        AIFPLSymbol("y")
                    ))
                )),
            )),
            AIFPLList((AIFPLSymbol("f"), AIFPLNumber(1)))
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, analyzer.code_objects)
        code = generator.generate()
        
        # Should have two lambda code objects
        assert len(code.code_objects) == 2


class TestGenerateLambdaNoPatching:
    """Test that lambda generation never emits PATCH opcodes."""
    
    def test_no_patch_opcodes_in_simple_lambda(self):
        """Test that simple lambda doesn't emit PATCH opcodes."""
        analyzer = AIFPLAnalyzer()
        
        # (lambda (x) x)
        expr = AIFPLList((
            AIFPLSymbol("lambda"),
            AIFPLList((AIFPLSymbol("x"),)),
            AIFPLSymbol("x")
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, analyzer.code_objects)
        code = generator.generate()
        
        # Check main code
        for instr in code.instructions:
            assert instr.opcode != Opcode.PATCH_CLOSURE_SELF
            assert instr.opcode != Opcode.PATCH_CLOSURE_SIBLING
        
        # Check lambda body
        for lambda_code in code.code_objects:
            for instr in lambda_code.instructions:
                assert instr.opcode != Opcode.PATCH_CLOSURE_SELF
                assert instr.opcode != Opcode.PATCH_CLOSURE_SIBLING
    
    def test_no_patch_opcodes_in_nested_lambda(self):
        """Test that nested lambdas don't emit PATCH opcodes."""
        analyzer = AIFPLAnalyzer()
        
        # (lambda (x) (lambda (y) (+ x y)))
        expr = AIFPLList((
            AIFPLSymbol("lambda"),
            AIFPLList((AIFPLSymbol("x"),)),
            AIFPLList((
                AIFPLSymbol("lambda"),
                AIFPLList((AIFPLSymbol("y"),)),
                AIFPLList((AIFPLSymbol("+"), AIFPLSymbol("x"), AIFPLSymbol("y")))
            ))
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, analyzer.code_objects)
        code = generator.generate()
        
        # Check all code recursively
        def check_no_patch(code_obj):
            for instr in code_obj.instructions:
                assert instr.opcode != Opcode.PATCH_CLOSURE_SELF
                assert instr.opcode != Opcode.PATCH_CLOSURE_SIBLING
            for nested in code_obj.code_objects:
                check_no_patch(nested)
        
        check_no_patch(code)


class TestGenerateLambdaInstructionCounts:
    """Test that instruction counts match analysis."""
    
    def test_lambda_instruction_count(self):
        """Test instruction count for lambda."""
        analyzer = AIFPLAnalyzer()
        
        # (lambda (x) x)
        expr = AIFPLList((
            AIFPLSymbol("lambda"),
            AIFPLList((AIFPLSymbol("x"),)),
            AIFPLSymbol("x")
        ))
        
        analyzed = analyzer.analyze(expr)
        
        # Analyzer predicted 1 instruction (MAKE_CLOSURE)
        assert analyzed.instruction_count == 1
        
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, analyzer.code_objects)
        code = generator.generate()
        
        # Generator should emit 1 instruction (plus RETURN)
        assert len(code.instructions) == 2  # MAKE_CLOSURE + RETURN
