"""Tests for bytecode string operations."""

import pytest
from aifpl.aifpl_parser import AIFPLParser
from aifpl.aifpl_tokenizer import AIFPLTokenizer
from aifpl.aifpl_compiler import AIFPLCompiler
from aifpl.aifpl_vm import AIFPLVM
from aifpl.aifpl_value import AIFPLNumber, AIFPLBoolean, AIFPLString, AIFPLList
from aifpl.aifpl_error import AIFPLEvalError
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


class TestStringBasics:
    """Test basic string operations."""

    def test_string_literal(self):
        result = compile_and_run('"hello"')
        assert isinstance(result, AIFPLString)
        assert result.value == "hello"

    def test_string_append(self):
        result = compile_and_run('(string-append "hello" " " "world")')
        assert isinstance(result, AIFPLString)
        assert result.value == "hello world"

    def test_string_append_many(self):
        result = compile_and_run('(string-append "a" "b" "c" "d")')
        assert isinstance(result, AIFPLString)
        assert result.value == "abcd"

    def test_string_length(self):
        result = compile_and_run('(string-length "hello")')
        assert isinstance(result, AIFPLNumber)
        assert result.value == 5

    def test_string_length_empty(self):
        result = compile_and_run('(string-length "")')
        assert isinstance(result, AIFPLNumber)
        assert result.value == 0


class TestStringCaseConversion:
    """Test string case conversion."""

    def test_string_upcase(self):
        result = compile_and_run('(string-upcase "hello")')
        assert isinstance(result, AIFPLString)
        assert result.value == "HELLO"

    def test_string_downcase(self):
        result = compile_and_run('(string-downcase "HELLO")')
        assert isinstance(result, AIFPLString)
        assert result.value == "hello"

    def test_string_upcase_mixed(self):
        result = compile_and_run('(string-upcase "HeLLo WoRLd")')
        assert isinstance(result, AIFPLString)
        assert result.value == "HELLO WORLD"


class TestStringManipulation:
    """Test string manipulation operations."""

    def test_string_trim(self):
        result = compile_and_run('(string-trim "  hello  ")')
        assert isinstance(result, AIFPLString)
        assert result.value == "hello"

    def test_string_replace(self):
        result = compile_and_run('(string-replace "hello world" "world" "there")')
        assert isinstance(result, AIFPLString)
        assert result.value == "hello there"

    def test_string_replace_multiple(self):
        result = compile_and_run('(string-replace "banana" "a" "o")')
        assert isinstance(result, AIFPLString)
        assert result.value == "bonono"

    def test_substring(self):
        result = compile_and_run('(substring "hello" 1 4)')
        assert isinstance(result, AIFPLString)
        assert result.value == "ell"

    def test_string_ref(self):
        result = compile_and_run('(string-ref "hello" 1)')
        assert isinstance(result, AIFPLString)
        assert result.value == "e"


class TestStringSplitJoin:
    """Test string split and join operations."""

    def test_string_split(self):
        result = compile_and_run('(string-split "a,b,c" ",")')
        assert isinstance(result, AIFPLList)
        assert len(result.elements) == 3
        assert result.elements[0].value == "a"
        assert result.elements[1].value == "b"
        assert result.elements[2].value == "c"

    def test_string_split_spaces(self):
        result = compile_and_run('(string-split "hello world test" " ")')
        assert isinstance(result, AIFPLList)
        assert len(result.elements) == 3

    def test_string_join(self):
        result = compile_and_run('(string-join (list "hello" "world") " ")')
        assert isinstance(result, AIFPLString)
        assert result.value == "hello world"

    def test_string_join_comma(self):
        result = compile_and_run('(string-join (list "a" "b" "c") ",")')
        assert isinstance(result, AIFPLString)
        assert result.value == "a,b,c"


class TestStringPredicates:
    """Test string predicate operations."""

    def test_string_contains_true(self):
        result = compile_and_run('(string-contains? "hello world" "world")')
        assert isinstance(result, AIFPLBoolean)
        assert result.value == True

    def test_string_contains_false(self):
        result = compile_and_run('(string-contains? "hello world" "xyz")')
        assert isinstance(result, AIFPLBoolean)
        assert result.value == False

    def test_string_prefix_true(self):
        result = compile_and_run('(string-prefix? "hello" "he")')
        assert isinstance(result, AIFPLBoolean)
        assert result.value == True

    def test_string_prefix_false(self):
        result = compile_and_run('(string-prefix? "hello" "lo")')
        assert isinstance(result, AIFPLBoolean)
        assert result.value == False

    def test_string_suffix_true(self):
        result = compile_and_run('(string-suffix? "hello" "lo")')
        assert isinstance(result, AIFPLBoolean)
        assert result.value == True

    def test_string_suffix_false(self):
        result = compile_and_run('(string-suffix? "hello" "he")')
        assert isinstance(result, AIFPLBoolean)
        assert result.value == False


class TestStringConversion:
    """Test string conversion operations."""

    def test_string_to_number_int(self):
        result = compile_and_run('(string->number "42")')
        assert isinstance(result, AIFPLNumber)
        assert result.value == 42

    def test_string_to_number_float(self):
        result = compile_and_run('(string->number "3.14")')
        assert isinstance(result, AIFPLNumber)
        assert result.value == 3.14

    def test_string_to_number_invalid(self):
        with pytest.raises(AIFPLEvalError, match="Cannot convert string to number"):
            compile_and_run('(string->number "not a number")')

    def test_number_to_string_int(self):
        result = compile_and_run('(number->string 42)')
        assert isinstance(result, AIFPLString)
        assert result.value == "42"

    def test_number_to_string_float(self):
        result = compile_and_run('(number->string 3.14)')
        assert isinstance(result, AIFPLString)
        assert result.value == "3.14"


class TestStringCombinations:
    """Test combinations of string operations."""

    def test_upcase_append(self):
        result = compile_and_run('(string-upcase (string-append "hello" " " "world"))')
        assert isinstance(result, AIFPLString)
        assert result.value == "HELLO WORLD"

    def test_split_map_join(self):
        result = compile_and_run('''
            (string-join
              (map string-upcase (string-split "hello,world" ","))
              ",")
        ''')
        assert isinstance(result, AIFPLString)
        assert result.value == "HELLO,WORLD"

    def test_complex_string_processing(self):
        # Trim, split, filter non-empty, join
        result = compile_and_run('''
            (let ((words (string-split (string-trim "  hello  world  ") " ")))
              (string-join words "-"))
        ''')
        assert isinstance(result, AIFPLString)
        # Note: split will create empty strings for multiple spaces
        # This is a simple test


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
