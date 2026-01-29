"""Basic tests for bytecode compilation and execution."""

import pytest
from aifpl.aifpl import AIFPL
from aifpl.aifpl_parser import AIFPLParser
from aifpl.aifpl_tokenizer import AIFPLTokenizer
from aifpl.aifpl_compiler import AIFPLCompiler
from aifpl.aifpl_vm import AIFPLVM
from aifpl.aifpl_value import AIFPLBoolean, AIFPLInteger, AIFPLFloat, AIFPLComplex


def compile_and_run(expression: str) -> any:
    """Helper to compile and execute an expression."""
    # Parse
    tokenizer = AIFPLTokenizer()
    tokens = tokenizer.tokenize(expression)
    ast = AIFPLParser(tokens, expression).parse()

    # Compile
    compiler = AIFPLCompiler()
    code = compiler.compile(ast)

    # Setup VM with globals
    vm = AIFPLVM()
    result = vm.execute(code, AIFPL.CONSTANTS)
    return result


class TestBasicArithmetic:
    """Test basic arithmetic operations."""

    def test_simple_addition(self):
        result = compile_and_run("(+ 1 2)")
        assert isinstance(result, (AIFPLInteger, AIFPLFloat, AIFPLComplex))
        assert result.value == 3

    def test_multiple_addition(self):
        result = compile_and_run("(+ 1 2 3 4 5)")
        assert isinstance(result, (AIFPLInteger, AIFPLFloat, AIFPLComplex))
        assert result.value == 15

    def test_subtraction(self):
        result = compile_and_run("(- 10 3)")
        assert isinstance(result, (AIFPLInteger, AIFPLFloat, AIFPLComplex))
        assert result.value == 7

    def test_multiplication(self):
        result = compile_and_run("(* 2 3 4)")
        assert isinstance(result, (AIFPLInteger, AIFPLFloat, AIFPLComplex))
        assert result.value == 24

    def test_division(self):
        result = compile_and_run("(/ 12 3)")
        assert isinstance(result, (AIFPLInteger, AIFPLFloat, AIFPLComplex))
        assert result.value == 4.0

    def test_nested_arithmetic(self):
        result = compile_and_run("(+ (* 2 3) (- 10 5))")
        assert isinstance(result, (AIFPLInteger, AIFPLFloat, AIFPLComplex))
        assert result.value == 11


class TestConstants:
    """Test constant values."""

    def test_number(self):
        result = compile_and_run("42")
        assert isinstance(result, (AIFPLInteger, AIFPLFloat, AIFPLComplex))
        assert result.value == 42

    def test_boolean_true(self):
        result = compile_and_run("#t")
        assert isinstance(result, AIFPLBoolean)
        assert result.value == True

    def test_boolean_false(self):
        result = compile_and_run("#f")
        assert isinstance(result, AIFPLBoolean)
        assert result.value == False


class TestComparisons:
    """Test comparison operations."""

    def test_equality(self):
        result = compile_and_run("(= 5 5)")
        assert isinstance(result, AIFPLBoolean)
        assert result.value == True

    def test_inequality(self):
        result = compile_and_run("(= 5 6)")
        assert isinstance(result, AIFPLBoolean)
        assert result.value == False

    def test_less_than(self):
        result = compile_and_run("(< 3 5)")
        assert isinstance(result, AIFPLBoolean)
        assert result.value == True

    def test_greater_than(self):
        result = compile_and_run("(> 10 5)")
        assert isinstance(result, AIFPLBoolean)
        assert result.value == True


class TestConditionals:
    """Test if expressions."""

    def test_if_true(self):
        result = compile_and_run("(if #t 42 99)")
        assert isinstance(result, (AIFPLInteger, AIFPLFloat, AIFPLComplex))
        assert result.value == 42

    def test_if_false(self):
        result = compile_and_run("(if #f 42 99)")
        assert isinstance(result, (AIFPLInteger, AIFPLFloat, AIFPLComplex))
        assert result.value == 99

    def test_if_with_condition(self):
        result = compile_and_run("(if (> 10 5) 1 0)")
        assert isinstance(result, (AIFPLInteger, AIFPLFloat, AIFPLComplex))
        assert result.value == 1


class TestLet:
    """Test let bindings."""

    def test_simple_let(self):
        result = compile_and_run("(let ((x 5)) x)")
        assert isinstance(result, (AIFPLInteger, AIFPLFloat, AIFPLComplex))
        assert result.value == 5

    def test_let_with_operation(self):
        result = compile_and_run("(let ((x 5) (y 10)) (+ x y))")
        assert isinstance(result, (AIFPLInteger, AIFPLFloat, AIFPLComplex))
        assert result.value == 15

    def test_let_with_expression(self):
        result = compile_and_run("(let ((x (+ 2 3))) (* x 2))")
        assert isinstance(result, (AIFPLInteger, AIFPLFloat, AIFPLComplex))
        assert result.value == 10


class TestLambda:
    """Test lambda functions."""

    def test_simple_lambda(self):
        result = compile_and_run("((lambda (x) (* x x)) 5)")
        assert isinstance(result, (AIFPLInteger, AIFPLFloat, AIFPLComplex))
        assert result.value == 25

    def test_lambda_with_multiple_params(self):
        result = compile_and_run("((lambda (x y) (+ x y)) 3 4)")
        assert isinstance(result, (AIFPLInteger, AIFPLFloat, AIFPLComplex))
        assert result.value == 7

    def test_lambda_in_let(self):
        result = compile_and_run("""
            (let ((square (lambda (x) (* x x))))
              (square 6))
        """)
        assert isinstance(result, (AIFPLInteger, AIFPLFloat, AIFPLComplex))
        assert result.value == 36


class TestCodeObjectDisassembly:
    """Test code object disassembly for debugging."""

    def test_disassemble_simple(self):
        tokenizer = AIFPLTokenizer()
        compiler = AIFPLCompiler()

        tokens = tokenizer.tokenize("(+ 1 2)")
        ast = AIFPLParser(tokens, "(+ 1 2)").parse()
        code = compiler.compile(ast)

        # Should be able to disassemble
        disassembly = code.disassemble()
        assert "LOAD_CONST" in disassembly or "CALL_BUILTIN" in disassembly
        assert "+" in str(code.names) or code.constants  # Either name or constant


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
