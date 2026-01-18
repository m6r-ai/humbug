"""Tests for bytecode alist operations."""

import pytest
from aifpl.aifpl_parser import AIFPLParser
from aifpl.aifpl_tokenizer import AIFPLTokenizer
from aifpl.aifpl_compiler import AIFPLCompiler
from aifpl.aifpl_vm import AIFPLVM
from aifpl.aifpl_value import AIFPLNumber, AIFPLBoolean, AIFPLString, AIFPLList, AIFPLAlist
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
    globals_dict = {**evaluator.CONSTANTS, **evaluator._builtin_functions}
    vm.set_globals(globals_dict)
    
    result = vm.execute(code)
    return result


class TestAlistCreation:
    """Test alist creation."""
    
    def test_empty_alist(self):
        result = compile_and_run('(alist)')
        assert isinstance(result, AIFPLAlist)
        assert len(result.pairs) == 0
    
    def test_alist_with_pairs(self):
        result = compile_and_run('(alist ("name" "Alice") ("age" 30))')
        assert isinstance(result, AIFPLAlist)
        assert len(result.pairs) == 2
    
    def test_alist_string_keys(self):
        result = compile_and_run('(alist ("a" 1) ("b" 2) ("c" 3))')
        assert isinstance(result, AIFPLAlist)
        assert len(result.pairs) == 3


class TestAlistAccess:
    """Test alist access operations."""
    
    def test_alist_get(self):
        result = compile_and_run('(alist-get (alist ("name" "Alice") ("age" 30)) "name")')
        assert isinstance(result, AIFPLString)
        assert result.value == "Alice"
    
    def test_alist_get_number(self):
        result = compile_and_run('(alist-get (alist ("age" 30)) "age")')
        assert isinstance(result, AIFPLNumber)
        assert result.value == 30
    
    def test_alist_get_with_default(self):
        result = compile_and_run('(alist-get (alist ("a" 1)) "b" 99)')
        assert isinstance(result, AIFPLNumber)
        assert result.value == 99
    
    def test_alist_has_true(self):
        result = compile_and_run('(alist-has? (alist ("name" "Alice")) "name")')
        assert isinstance(result, AIFPLBoolean)
        assert result.value == True
    
    def test_alist_has_false(self):
        result = compile_and_run('(alist-has? (alist ("name" "Alice")) "age")')
        assert isinstance(result, AIFPLBoolean)
        assert result.value == False


class TestAlistManipulation:
    """Test alist manipulation operations."""
    
    def test_alist_set_new_key(self):
        result = compile_and_run('(alist-set (alist ("a" 1)) "b" 2)')
        assert isinstance(result, AIFPLAlist)
        assert len(result.pairs) == 2
        assert result.get(AIFPLString("b")).value == 2
    
    def test_alist_set_existing_key(self):
        result = compile_and_run('(alist-set (alist ("a" 1)) "a" 99)')
        assert isinstance(result, AIFPLAlist)
        assert len(result.pairs) == 1
        assert result.get(AIFPLString("a")).value == 99
    
    def test_alist_remove(self):
        result = compile_and_run('(alist-remove (alist ("a" 1) ("b" 2)) "a")')
        assert isinstance(result, AIFPLAlist)
        assert len(result.pairs) == 1
        assert not result.has_key(AIFPLString("a"))
    
    def test_alist_merge(self):
        result = compile_and_run('(alist-merge (alist ("a" 1) ("b" 2)) (alist ("c" 3) ("d" 4)))')
        assert isinstance(result, AIFPLAlist)
        assert len(result.pairs) == 4
    
    def test_alist_merge_override(self):
        result = compile_and_run('(alist-merge (alist ("a" 1)) (alist ("a" 99)))')
        assert isinstance(result, AIFPLAlist)
        assert len(result.pairs) == 1
        assert result.get(AIFPLString("a")).value == 99


class TestAlistKeysValues:
    """Test alist keys and values extraction."""
    
    def test_alist_keys(self):
        result = compile_and_run('(alist-keys (alist ("a" 1) ("b" 2) ("c" 3)))')
        assert isinstance(result, AIFPLList)
        assert len(result.elements) == 3
        assert result.elements[0].value == "a"
        assert result.elements[1].value == "b"
        assert result.elements[2].value == "c"
    
    def test_alist_values(self):
        result = compile_and_run('(alist-values (alist ("a" 1) ("b" 2) ("c" 3)))')
        assert isinstance(result, AIFPLList)
        assert len(result.elements) == 3
        assert result.elements[0].value == 1
        assert result.elements[1].value == 2
        assert result.elements[2].value == 3
    
    def test_alist_keys_empty(self):
        result = compile_and_run('(alist-keys (alist))')
        assert isinstance(result, AIFPLList)
        assert result.is_empty()


class TestAlistPredicates:
    """Test alist type predicates."""
    
    def test_alist_predicate_true(self):
        result = compile_and_run('(alist? (alist ("a" 1)))')
        assert isinstance(result, AIFPLBoolean)
        assert result.value == True
    
    def test_alist_predicate_false_list(self):
        result = compile_and_run('(alist? (list 1 2 3))')
        assert isinstance(result, AIFPLBoolean)
        assert result.value == False
    
    def test_alist_predicate_false_number(self):
        result = compile_and_run('(alist? 42)')
        assert isinstance(result, AIFPLBoolean)
        assert result.value == False


class TestAlistCombinations:
    """Test combinations of alist operations."""
    
    def test_nested_alist(self):
        result = compile_and_run('''
            (alist ("user" (alist ("name" "Alice") ("age" 30))))
        ''')
        assert isinstance(result, AIFPLAlist)
        user = result.get(AIFPLString("user"))
        assert isinstance(user, AIFPLAlist)
    
    def test_alist_with_let(self):
        result = compile_and_run('''
            (let ((data (alist ("x" 10) ("y" 20))))
              (+ (alist-get data "x") (alist-get data "y")))
        ''')
        assert isinstance(result, AIFPLNumber)
        assert result.value == 30
    
    def test_alist_map_keys(self):
        result = compile_and_run('''
            (map (lambda (k) (alist-get (alist ("a" 1) ("b" 2) ("c" 3)) k))
                 (list "a" "b" "c"))
        ''')
        assert isinstance(result, AIFPLList)
        assert len(result.elements) == 3
        assert result.elements[0].value == 1
        assert result.elements[1].value == 2
        assert result.elements[2].value == 3
    
    def test_data_processing_pipeline(self):
        # Create alist, extract values, sum them
        result = compile_and_run('''
            (let ((data (alist ("id" 1) ("value" 100))))
              (alist-get data "value"))
        ''')
        assert isinstance(result, AIFPLNumber)
        assert result.value == 100
    
    def test_alist_update_chain(self):
        result = compile_and_run('''
            (alist-set
              (alist-set (alist ("a" 1)) "b" 2)
              "c" 3)
        ''')
        assert isinstance(result, AIFPLAlist)
        assert len(result.pairs) == 3


class TestBenchmarkPatterns:
    """Test patterns used in benchmarks."""
    
    def test_alist_creation_many_pairs(self):
        result = compile_and_run('''
            (alist
              ("a" 1) ("b" 2) ("c" 3) ("d" 4) ("e" 5)
              ("f" 6) ("g" 7) ("h" 8) ("i" 9) ("j" 10))
        ''')
        assert isinstance(result, AIFPLAlist)
        assert len(result.pairs) == 10
    
    def test_alist_in_map(self):
        # Create list of alists
        result = compile_and_run('''
            (map (lambda (i) (alist ("id" i) ("value" (* i i))))
                 (range 1 6))
        ''')
        assert isinstance(result, AIFPLList)
        assert len(result.elements) == 5
        # Each element should be an alist
        for elem in result.elements:
            assert isinstance(elem, AIFPLAlist)


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
