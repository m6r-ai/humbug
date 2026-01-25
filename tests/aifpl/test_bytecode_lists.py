"""Tests for bytecode list operations."""

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

    vm = AIFPLVM()

    evaluator = AIFPLEvaluator()
    vm.set_globals(evaluator.CONSTANTS)

    result = vm.execute(code)
    return result


class TestListCreation:
    """Test list creation operations."""

    def test_empty_list(self):
        result = compile_and_run("(list)")
        assert isinstance(result, AIFPLList)
        assert result.is_empty()

    def test_list_with_elements(self):
        result = compile_and_run("(list 1 2 3)")
        assert isinstance(result, AIFPLList)
        assert len(result.elements) == 3
        assert result.elements[0].value == 1
        assert result.elements[1].value == 2
        assert result.elements[2].value == 3

    def test_cons(self):
        result = compile_and_run("(cons 1 (list 2 3))")
        assert isinstance(result, AIFPLList)
        assert len(result.elements) == 3
        assert result.elements[0].value == 1


class TestListAccess:
    """Test list access operations."""

    def test_first(self):
        result = compile_and_run("(first (list 1 2 3))")
        assert isinstance(result, AIFPLNumber)
        assert result.value == 1

    def test_rest(self):
        result = compile_and_run("(rest (list 1 2 3))")
        assert isinstance(result, AIFPLList)
        assert len(result.elements) == 2
        assert result.elements[0].value == 2

    def test_last(self):
        result = compile_and_run("(last (list 1 2 3))")
        assert isinstance(result, AIFPLNumber)
        assert result.value == 3

    def test_length(self):
        result = compile_and_run("(length (list 1 2 3 4 5))")
        assert isinstance(result, AIFPLNumber)
        assert result.value == 5


class TestListManipulation:
    """Test list manipulation operations."""

    def test_append(self):
        result = compile_and_run("(append (list 1 2) (list 3 4))")
        assert isinstance(result, AIFPLList)
        assert len(result.elements) == 4
        assert result.elements[0].value == 1
        assert result.elements[3].value == 4

    def test_reverse(self):
        result = compile_and_run("(reverse (list 1 2 3))")
        assert isinstance(result, AIFPLList)
        assert result.elements[0].value == 3
        assert result.elements[2].value == 1

    def test_take(self):
        result = compile_and_run("(take 2 (list 1 2 3 4 5))")
        assert isinstance(result, AIFPLList)
        assert len(result.elements) == 2
        assert result.elements[0].value == 1
        assert result.elements[1].value == 2

    def test_drop(self):
        result = compile_and_run("(drop 2 (list 1 2 3 4 5))")
        assert isinstance(result, AIFPLList)
        assert len(result.elements) == 3
        assert result.elements[0].value == 3

    def test_remove(self):
        result = compile_and_run("(remove 2 (list 1 2 3 2 4))")
        assert isinstance(result, AIFPLList)
        assert len(result.elements) == 3
        # Should have removed all 2s
        assert all(elem.value != 2 for elem in result.elements)


class TestListPredicates:
    """Test list predicate operations."""

    def test_null_true(self):
        result = compile_and_run("(null? (list))")
        assert isinstance(result, AIFPLBoolean)
        assert result.value == True

    def test_null_false(self):
        result = compile_and_run("(null? (list 1))")
        assert isinstance(result, AIFPLBoolean)
        assert result.value == False

    def test_member_true(self):
        result = compile_and_run("(member? 2 (list 1 2 3))")
        assert isinstance(result, AIFPLBoolean)
        assert result.value == True

    def test_member_false(self):
        result = compile_and_run("(member? 5 (list 1 2 3))")
        assert isinstance(result, AIFPLBoolean)
        assert result.value == False

    def test_position_found(self):
        result = compile_and_run("(position 3 (list 1 2 3 4))")
        assert isinstance(result, AIFPLNumber)
        assert result.value == 2

    def test_position_not_found(self):
        result = compile_and_run("(position 5 (list 1 2 3))")
        assert isinstance(result, AIFPLBoolean)
        assert result.value == False


class TestMathFunctions:
    """Test additional math functions."""

    def test_sqrt(self):
        result = compile_and_run("(sqrt 16)")
        assert isinstance(result, AIFPLNumber)
        assert result.value == 4.0

    def test_abs_positive(self):
        result = compile_and_run("(abs 5)")
        assert isinstance(result, AIFPLNumber)
        assert result.value == 5

    def test_abs_negative(self):
        result = compile_and_run("(abs -5)")
        assert isinstance(result, AIFPLNumber)
        assert result.value == 5

    def test_min(self):
        result = compile_and_run("(min 3 1 4 1 5)")
        assert isinstance(result, AIFPLNumber)
        assert result.value == 1

    def test_max(self):
        result = compile_and_run("(max 3 1 4 1 5)")
        assert isinstance(result, AIFPLNumber)
        assert result.value == 5

    def test_pow(self):
        result = compile_and_run("(pow 2 3)")
        assert isinstance(result, AIFPLNumber)
        assert result.value == 8


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
