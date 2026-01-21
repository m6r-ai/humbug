"""Tests for bytecode higher-order functions."""

import pytest
from aifpl.aifpl_parser import AIFPLParser
from aifpl.aifpl_tokenizer import AIFPLTokenizer
from aifpl.aifpl_compiler import AIFPLCompiler
from aifpl.aifpl_vm import AIFPLVM
from aifpl.aifpl_value import AIFPLNumber, AIFPLBoolean, AIFPLList
from aifpl.aifpl_evaluator import AIFPLEvaluator


def compile_and_run(expression: str) -> any:
    """Helper to compile and execute an expression."""
    tokenizer = AIFPLTokenizer()
    tokens = tokenizer.tokenize(expression)
    ast = AIFPLParser(tokens, expression).parse()

    compiler = AIFPLCompiler()
    code = compiler.compile(ast)

    evaluator = AIFPLEvaluator()
    vm = AIFPLVM(evaluator)

    # Set up globals (constants only - builtins are built into the VM)
    globals_dict = evaluator.CONSTANTS
    vm.set_globals(globals_dict)

    result = vm.execute(code)
    return result


class TestRange:
    """Test range function."""

    def test_range_basic(self):
        result = compile_and_run("(range 1 5)")
        assert isinstance(result, AIFPLList)
        assert len(result.elements) == 4
        assert result.elements[0].value == 1
        assert result.elements[3].value == 4

    def test_range_with_step(self):
        result = compile_and_run("(range 0 10 2)")
        assert isinstance(result, AIFPLList)
        assert len(result.elements) == 5
        assert result.elements[0].value == 0
        assert result.elements[1].value == 2
        assert result.elements[4].value == 8


class TestMap:
    """Test map function."""

    def test_map_simple(self):
        result = compile_and_run("(map (lambda (x) (* x x)) (list 1 2 3 4))")
        assert isinstance(result, AIFPLList)
        assert len(result.elements) == 4
        assert result.elements[0].value == 1
        assert result.elements[1].value == 4
        assert result.elements[2].value == 9
        assert result.elements[3].value == 16

    def test_map_with_range(self):
        result = compile_and_run("(map (lambda (x) (* x 2)) (range 1 6))")
        assert isinstance(result, AIFPLList)
        assert len(result.elements) == 5
        assert result.elements[0].value == 2
        assert result.elements[4].value == 10

    def test_map_addition(self):
        result = compile_and_run("(map (lambda (x) (+ x 10)) (list 1 2 3))")
        assert isinstance(result, AIFPLList)
        assert result.elements[0].value == 11
        assert result.elements[2].value == 13


class TestFilter:
    """Test filter function."""

    def test_filter_simple(self):
        result = compile_and_run("(filter (lambda (x) (> x 2)) (list 1 2 3 4 5))")
        assert isinstance(result, AIFPLList)
        assert len(result.elements) == 3
        assert result.elements[0].value == 3
        assert result.elements[2].value == 5

    def test_filter_even(self):
        # Filter even numbers (x % 2 == 0)
        # We don't have % yet, so use a different test
        result = compile_and_run("(filter (lambda (x) (< x 3)) (range 1 6))")
        assert isinstance(result, AIFPLList)
        assert len(result.elements) == 2
        assert result.elements[0].value == 1
        assert result.elements[1].value == 2

    def test_filter_empty_result(self):
        result = compile_and_run("(filter (lambda (x) (> x 10)) (list 1 2 3))")
        assert isinstance(result, AIFPLList)
        assert result.is_empty()


class TestFold:
    """Test fold function."""

    def test_fold_sum(self):
        result = compile_and_run("(fold + 0 (list 1 2 3 4 5))")
        assert isinstance(result, AIFPLNumber)
        assert result.value == 15

    def test_fold_product(self):
        result = compile_and_run("(fold * 1 (list 2 3 4))")
        assert isinstance(result, AIFPLNumber)
        assert result.value == 24

    def test_fold_with_lambda(self):
        result = compile_and_run("(fold (lambda (acc x) (+ acc (* x x))) 0 (list 1 2 3))")
        assert isinstance(result, AIFPLNumber)
        assert result.value == 14  # 1^2 + 2^2 + 3^2 = 1 + 4 + 9

    def test_fold_with_range(self):
        result = compile_and_run("(fold + 0 (range 1 11))")
        assert isinstance(result, AIFPLNumber)
        assert result.value == 55  # Sum of 1..10


class TestCombinations:
    """Test combinations of higher-order functions."""

    def test_map_then_fold(self):
        result = compile_and_run("(fold + 0 (map (lambda (x) (* x x)) (range 1 6)))")
        assert isinstance(result, AIFPLNumber)
        assert result.value == 55  # 1 + 4 + 9 + 16 + 25

    def test_filter_then_map(self):
        result = compile_and_run("""
            (map (lambda (x) (* x 2))
                 (filter (lambda (x) (> x 2)) (list 1 2 3 4 5)))
        """)
        assert isinstance(result, AIFPLList)
        assert len(result.elements) == 3
        assert result.elements[0].value == 6  # 3 * 2
        assert result.elements[2].value == 10  # 5 * 2

    def test_complex_pipeline(self):
        # Sum of squares of numbers > 2
        result = compile_and_run("""
            (fold + 0
                  (map (lambda (x) (* x x))
                       (filter (lambda (x) (> x 2))
                               (range 1 6))))
        """)
        assert isinstance(result, AIFPLNumber)
        assert result.value == 50  # 3^2 + 4^2 + 5^2 = 9 + 16 + 25


class TestBenchmarkPatterns:
    """Test patterns used in benchmarks."""

    def test_map_100_elements(self):
        result = compile_and_run("(map (lambda (x) (* x x)) (range 1 101))")
        assert isinstance(result, AIFPLList)
        assert len(result.elements) == 100
        assert result.elements[0].value == 1
        assert result.elements[99].value == 10000

    def test_filter_100_elements(self):
        result = compile_and_run("(filter (lambda (x) (> x 50)) (range 1 101))")
        assert isinstance(result, AIFPLList)
        assert len(result.elements) == 50

    def test_fold_100_elements(self):
        result = compile_and_run("(fold + 0 (range 1 101))")
        assert isinstance(result, AIFPLNumber)
        assert result.value == 5050  # Sum of 1..100


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
