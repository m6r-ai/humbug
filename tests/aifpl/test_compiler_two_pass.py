"""Tests for two-pass compiler integration.

These tests verify that the two-pass compiler (analyzer + generator)
produces correct bytecode and works end-to-end.
"""

import pytest
from aifpl.aifpl_compiler import AIFPLCompiler
from aifpl.aifpl_tokenizer import AIFPLTokenizer
from aifpl.aifpl_parser import AIFPLParser
from aifpl.aifpl_evaluator import AIFPLEvaluator
from aifpl.aifpl_vm import AIFPLVM
from aifpl.aifpl_value import AIFPLNumber, AIFPLBoolean


def parse(code: str):
    """Helper to parse AIFPL code."""
    tokenizer = AIFPLTokenizer()
    tokens = tokenizer.tokenize(code)
    parser = AIFPLParser(tokens, code)
    return parser.parse()


def execute(code):
    """Helper to execute compiled code with proper VM setup."""
    vm = AIFPLVM()
    evaluator = AIFPLEvaluator()
    vm.set_globals(evaluator.CONSTANTS)
    return vm.execute(code)


class TestTwoPassCompilerBasic:
    """Test basic two-pass compilation."""
    
    def test_compile_literal(self):
        """Test compiling a literal."""
        compiler = AIFPLCompiler(use_two_pass=True)
        expr = parse("42")
        
        code = compiler.compile(expr)
        
        assert code is not None
        assert len(code.instructions) > 0
    
    def test_compile_arithmetic(self):
        """Test compiling arithmetic expression."""
        compiler = AIFPLCompiler(use_two_pass=True)
        expr = parse("(+ 1 2)")
        
        code = compiler.compile(expr)
        
        assert code is not None
        assert len(code.instructions) > 0
    
    def test_compile_if(self):
        """Test compiling if expression."""
        compiler = AIFPLCompiler(use_two_pass=True)
        expr = parse("(if #t 1 2)")
        
        code = compiler.compile(expr)
        
        assert code is not None
        assert len(code.instructions) > 0
    
    def test_compile_let(self):
        """Test compiling let expression."""
        compiler = AIFPLCompiler(use_two_pass=True)
        expr = parse("(let ((x 5)) x)")
        
        code = compiler.compile(expr)
        
        assert code is not None
        assert len(code.instructions) > 0
    
    def test_compile_lambda(self):
        """Test compiling lambda expression."""
        compiler = AIFPLCompiler(use_two_pass=True)
        expr = parse("(lambda (x) x)")
        
        code = compiler.compile(expr)
        
        assert code is not None
        assert len(code.instructions) > 0
        assert len(code.code_objects) == 1  # Lambda body


class TestTwoPassExecution:
    """Test that two-pass compiled code executes correctly."""
    
    def test_execute_literal(self):
        """Test executing literal."""
        compiler = AIFPLCompiler(use_two_pass=True)
        expr = parse("42")
        
        code = compiler.compile(expr)
        result = execute(code)
        
        assert isinstance(result, AIFPLNumber)
        assert result.value == 42
    
    def test_execute_arithmetic(self):
        """Test executing arithmetic."""
        compiler = AIFPLCompiler(use_two_pass=True)
        expr = parse("(+ 1 2)")
        
        code = compiler.compile(expr)
        result = execute(code)
        
        assert isinstance(result, AIFPLNumber)
        assert result.value == 3
    
    def test_execute_if_true(self):
        """Test executing if with true condition."""
        compiler = AIFPLCompiler(use_two_pass=True)
        expr = parse("(if #t 1 2)")
        
        code = compiler.compile(expr)
        result = execute(code)
        
        assert isinstance(result, AIFPLNumber)
        assert result.value == 1
    
    def test_execute_if_false(self):
        """Test executing if with false condition."""
        compiler = AIFPLCompiler(use_two_pass=True)
        expr = parse("(if #f 1 2)")
        
        code = compiler.compile(expr)
        result = execute(code)
        
        assert isinstance(result, AIFPLNumber)
        assert result.value == 2
    
    def test_execute_let(self):
        """Test executing let."""
        compiler = AIFPLCompiler(use_two_pass=True)
        expr = parse("(let ((x 5)) x)")
        
        code = compiler.compile(expr)
        result = execute(code)
        
        assert isinstance(result, AIFPLNumber)
        assert result.value == 5
    
    def test_execute_let_multiple_bindings(self):
        """Test executing let with multiple bindings."""
        compiler = AIFPLCompiler(use_two_pass=True)
        expr = parse("(let ((x 5) (y 10)) (+ x y))")
        
        code = compiler.compile(expr)
        result = execute(code)
        
        assert isinstance(result, AIFPLNumber)
        assert result.value == 15
    
    def test_execute_lambda_call(self):
        """Test executing lambda call."""
        compiler = AIFPLCompiler(use_two_pass=True)
        expr = parse("((lambda (x) x) 42)")
        
        code = compiler.compile(expr)
        result = execute(code)
        
        assert isinstance(result, AIFPLNumber)
        assert result.value == 42
    
    def test_execute_lambda_with_operation(self):
        """Test executing lambda with operation."""
        compiler = AIFPLCompiler(use_two_pass=True)
        expr = parse("((lambda (x y) (+ x y)) 3 4)")
        
        code = compiler.compile(expr)
        result = execute(code)
        
        assert isinstance(result, AIFPLNumber)
        assert result.value == 7


class TestTwoPassComplexExpressions:
    """Test complex expressions with two-pass compiler."""
    
    def test_nested_if(self):
        """Test nested if expressions."""
        compiler = AIFPLCompiler(use_two_pass=True)
        expr = parse("(if #t (if #f 1 2) 3)")
        
        code = compiler.compile(expr)
        result = execute(code)
        
        assert isinstance(result, AIFPLNumber)
        assert result.value == 2
    
    def test_nested_let(self):
        """Test nested let expressions."""
        compiler = AIFPLCompiler(use_two_pass=True)
        expr = parse("(let ((x 5)) (let ((y 10)) (+ x y)))")
        
        code = compiler.compile(expr)
        result = execute(code)
        
        assert isinstance(result, AIFPLNumber)
        assert result.value == 15
    
    def test_lambda_in_let(self):
        """Test lambda in let binding."""
        compiler = AIFPLCompiler(use_two_pass=True)
        expr = parse("(let ((f (lambda (x) (* x 2)))) (f 21))")
        
        code = compiler.compile(expr)
        result = execute(code)
        
        assert isinstance(result, AIFPLNumber)
        assert result.value == 42
    
    def test_complex_arithmetic(self):
        """Test complex arithmetic expression."""
        compiler = AIFPLCompiler(use_two_pass=True)
        expr = parse("(+ (* 2 3) (- 10 5))")
        
        code = compiler.compile(expr)
        result = execute(code)
        
        assert isinstance(result, AIFPLNumber)
        assert result.value == 11


class TestTwoPassCompilerMode:
    """Test compiler mode switching."""
    
    def test_single_pass_mode(self):
        """Test that single-pass mode still works."""
        compiler = AIFPLCompiler(use_two_pass=False)
        expr = parse("(+ 1 2)")
        
        code = compiler.compile(expr)
        result = execute(code)
        
        assert isinstance(result, AIFPLNumber)
        assert result.value == 3
    
    def test_two_pass_mode(self):
        """Test that two-pass mode works."""
        compiler = AIFPLCompiler(use_two_pass=True)
        expr = parse("(+ 1 2)")
        
        code = compiler.compile(expr)
        result = execute(code)
        
        assert isinstance(result, AIFPLNumber)
        assert result.value == 3
    
    def test_default_is_single_pass(self):
        """Test that default is single-pass."""
        compiler = AIFPLCompiler()
        assert compiler.use_two_pass == False


class TestTwoPassAndOr:
    """Test and/or compilation with two-pass compiler."""
    
    def test_and_empty(self):
        """Test (and) -> #t."""
        compiler = AIFPLCompiler(use_two_pass=True)
        expr = parse("(and)")
        
        code = compiler.compile(expr)
        result = execute(code)
        
        assert isinstance(result, AIFPLBoolean)
        assert result.value == True
    
    def test_and_single_true(self):
        """Test (and #t) -> #t."""
        compiler = AIFPLCompiler(use_two_pass=True)
        expr = parse("(and #t)")
        
        code = compiler.compile(expr)
        result = execute(code)
        
        assert isinstance(result, AIFPLBoolean)
        assert result.value == True
    
    def test_and_single_false(self):
        """Test (and #f) -> #f."""
        compiler = AIFPLCompiler(use_two_pass=True)
        expr = parse("(and #f)")
        
        code = compiler.compile(expr)
        result = execute(code)
        
        assert isinstance(result, AIFPLBoolean)
        assert result.value == False
    
    def test_and_all_true(self):
        """Test (and #t #t #t) -> #t."""
        compiler = AIFPLCompiler(use_two_pass=True)
        expr = parse("(and #t #t #t)")
        
        code = compiler.compile(expr)
        result = execute(code)
        
        assert isinstance(result, AIFPLBoolean)
        assert result.value == True
    
    def test_and_with_false(self):
        """Test (and #t #f #t) -> #f."""
        compiler = AIFPLCompiler(use_two_pass=True)
        expr = parse("(and #t #f #t)")
        
        code = compiler.compile(expr)
        result = execute(code)
        
        assert isinstance(result, AIFPLBoolean)
        assert result.value == False
    
    def test_or_empty(self):
        """Test (or) -> #f."""
        compiler = AIFPLCompiler(use_two_pass=True)
        expr = parse("(or)")
        
        code = compiler.compile(expr)
        result = execute(code)
        
        assert isinstance(result, AIFPLBoolean)
        assert result.value == False
    
    def test_or_with_true(self):
        """Test (or #f #t #f) -> #t."""
        compiler = AIFPLCompiler(use_two_pass=True)
        expr = parse("(or #f #t #f)")
        
        code = compiler.compile(expr)
        result = execute(code)
        
        assert isinstance(result, AIFPLBoolean)
        assert result.value == True
