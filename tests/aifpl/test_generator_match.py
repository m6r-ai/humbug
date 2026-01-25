"""Tests for AIFPL Generator - Match expression generation.

These tests verify that match expressions generate correct bytecode
with pattern matching logic.
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
from aifpl.aifpl_vm import AIFPLVM
from aifpl.aifpl_evaluator import AIFPLEvaluator


def execute_code(code: CodeObject):
    """Helper to execute compiled code."""
    vm = AIFPLVM()
    evaluator = AIFPLEvaluator()
    vm.set_globals(evaluator.CONSTANTS)
    return vm.execute(code)


class TestGenerateMatchLiteral:
    """Test literal pattern matching."""
    
    def test_match_literal_number(self):
        """Test matching a literal number."""
        analyzer = AIFPLAnalyzer()
        
        # (match 42 (42 "found") (_ "not found"))
        expr = AIFPLList((
            AIFPLSymbol("match"),
            AIFPLNumber(42),
            AIFPLList((AIFPLNumber(42), AIFPLString("found"))),
            AIFPLList((AIFPLSymbol("_"), AIFPLString("not found")))
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, [])
        code = generator.generate()
        
        result = execute_code(code)
        assert result.value == "found"
    
    def test_match_literal_string(self):
        """Test matching a literal string."""
        analyzer = AIFPLAnalyzer()
        
        # (match "hello" ("hello" "found") (_ "not found"))
        expr = AIFPLList((
            AIFPLSymbol("match"),
            AIFPLString("hello"),
            AIFPLList((AIFPLString("hello"), AIFPLString("found"))),
            AIFPLList((AIFPLSymbol("_"), AIFPLString("not found")))
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, [])
        code = generator.generate()
        
        result = execute_code(code)
        assert result.value == "found"
    
    def test_match_literal_no_match(self):
        """Test when literal doesn't match."""
        analyzer = AIFPLAnalyzer()
        
        # (match 99 (42 "found") (_ "not found"))
        expr = AIFPLList((
            AIFPLSymbol("match"),
            AIFPLNumber(99),
            AIFPLList((AIFPLNumber(42), AIFPLString("found"))),
            AIFPLList((AIFPLSymbol("_"), AIFPLString("not found")))
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, [])
        code = generator.generate()
        
        result = execute_code(code)
        assert result.value == "not found"


class TestGenerateMatchVariable:
    """Test variable pattern matching."""
    
    def test_match_variable_binds(self):
        """Test that variable pattern binds the value."""
        analyzer = AIFPLAnalyzer()
        
        # (match 42 (x x))
        expr = AIFPLList((
            AIFPLSymbol("match"),
            AIFPLNumber(42),
            AIFPLList((AIFPLSymbol("x"), AIFPLSymbol("x")))
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, [])
        code = generator.generate()
        
        result = execute_code(code)
        assert result.value == 42
    
    def test_match_wildcard(self):
        """Test wildcard pattern always matches."""
        analyzer = AIFPLAnalyzer()
        
        # (match 42 (_ "matched"))
        expr = AIFPLList((
            AIFPLSymbol("match"),
            AIFPLNumber(42),
            AIFPLList((AIFPLSymbol("_"), AIFPLString("matched")))
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, [])
        code = generator.generate()
        
        result = execute_code(code)
        assert result.value == "matched"


class TestGenerateMatchType:
    """Test type pattern matching."""
    
    def test_match_number_type(self):
        """Test matching number type."""
        analyzer = AIFPLAnalyzer()
        
        # (match 42 ((number? n) n) (_ "not a number"))
        expr = AIFPLList((
            AIFPLSymbol("match"),
            AIFPLNumber(42),
            AIFPLList((
                AIFPLList((AIFPLSymbol("number?"), AIFPLSymbol("n"))),
                AIFPLSymbol("n")
            )),
            AIFPLList((AIFPLSymbol("_"), AIFPLString("not a number")))
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, [])
        code = generator.generate()
        
        result = execute_code(code)
        assert result.value == 42
    
    def test_match_string_type(self):
        """Test matching string type."""
        analyzer = AIFPLAnalyzer()
        
        # (match "hello" ((string? s) s) (_ "not a string"))
        expr = AIFPLList((
            AIFPLSymbol("match"),
            AIFPLString("hello"),
            AIFPLList((
                AIFPLList((AIFPLSymbol("string?"), AIFPLSymbol("s"))),
                AIFPLSymbol("s")
            )),
            AIFPLList((AIFPLSymbol("_"), AIFPLString("not a string")))
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, [])
        code = generator.generate()
        
        result = execute_code(code)
        assert result.value == "hello"
    
    def test_match_type_no_match(self):
        """Test when type doesn't match."""
        analyzer = AIFPLAnalyzer()
        
        # (match 42 ((string? s) s) (_ "not a string"))
        expr = AIFPLList((
            AIFPLSymbol("match"),
            AIFPLNumber(42),
            AIFPLList((
                AIFPLList((AIFPLSymbol("string?"), AIFPLSymbol("s"))),
                AIFPLSymbol("s")
            )),
            AIFPLList((AIFPLSymbol("_"), AIFPLString("not a string")))
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, [])
        code = generator.generate()
        
        result = execute_code(code)
        assert result.value == "not a string"


class TestGenerateMatchEmptyList:
    """Test empty list pattern matching."""
    
    def test_match_empty_list(self):
        """Test matching empty list."""
        analyzer = AIFPLAnalyzer()
        
        # (match (list) (() "empty") (_ "not empty"))
        expr = AIFPLList((
            AIFPLSymbol("match"),
            AIFPLList((AIFPLSymbol("list"),)),
            AIFPLList((AIFPLList(()), AIFPLString("empty"))),
            AIFPLList((AIFPLSymbol("_"), AIFPLString("not empty")))
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, [])
        code = generator.generate()
        
        result = execute_code(code)
        assert result.value == "empty"
    
    def test_match_non_empty_list(self):
        """Test when list is not empty."""
        analyzer = AIFPLAnalyzer()
        
        # (match (list 1) (() "empty") (_ "not empty"))
        expr = AIFPLList((
            AIFPLSymbol("match"),
            AIFPLList((AIFPLSymbol("list"), AIFPLNumber(1))),
            AIFPLList((AIFPLList(()), AIFPLString("empty"))),
            AIFPLList((AIFPLSymbol("_"), AIFPLString("not empty")))
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, [])
        code = generator.generate()
        
        result = execute_code(code)
        assert result.value == "not empty"


class TestGenerateMatchFixedList:
    """Test fixed-length list pattern matching."""
    
    def test_match_single_element_list(self):
        """Test matching single element list."""
        analyzer = AIFPLAnalyzer()
        
        # (match (list 42) ((x) x) (_ "no match"))
        expr = AIFPLList((
            AIFPLSymbol("match"),
            AIFPLList((AIFPLSymbol("list"), AIFPLNumber(42))),
            AIFPLList((AIFPLList((AIFPLSymbol("x"),)), AIFPLSymbol("x"))),
            AIFPLList((AIFPLSymbol("_"), AIFPLString("no match")))
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, [])
        code = generator.generate()
        
        result = execute_code(code)
        assert result.value == 42
    
    def test_match_two_element_list(self):
        """Test matching two element list."""
        analyzer = AIFPLAnalyzer()
        
        # (match (list 1 2) ((x y) (+ x y)) (_ "no match"))
        expr = AIFPLList((
            AIFPLSymbol("match"),
            AIFPLList((AIFPLSymbol("list"), AIFPLNumber(1), AIFPLNumber(2))),
            AIFPLList((
                AIFPLList((AIFPLSymbol("x"), AIFPLSymbol("y"))),
                AIFPLList((AIFPLSymbol("+"), AIFPLSymbol("x"), AIFPLSymbol("y")))
            )),
            AIFPLList((AIFPLSymbol("_"), AIFPLString("no match")))
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, [])
        code = generator.generate()
        
        result = execute_code(code)
        assert result.value == 3
    
    def test_match_wrong_length(self):
        """Test when list has wrong length."""
        analyzer = AIFPLAnalyzer()
        
        # (match (list 1 2 3) ((x y) "two") (_ "other"))
        expr = AIFPLList((
            AIFPLSymbol("match"),
            AIFPLList((AIFPLSymbol("list"), AIFPLNumber(1), AIFPLNumber(2), AIFPLNumber(3))),
            AIFPLList((AIFPLList((AIFPLSymbol("x"), AIFPLSymbol("y"))), AIFPLString("two"))),
            AIFPLList((AIFPLSymbol("_"), AIFPLString("other")))
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, [])
        code = generator.generate()
        
        result = execute_code(code)
        assert result.value == "other"


class TestGenerateMatchNested:
    """Test nested pattern matching."""
    
    def test_match_nested_list(self):
        """Test matching nested list pattern."""
        analyzer = AIFPLAnalyzer()
        
        # (match (list 1 (list 2 3)) ((x (y z)) (+ x y z)) (_ "no match"))
        expr = AIFPLList((
            AIFPLSymbol("match"),
            AIFPLList((
                AIFPLSymbol("list"),
                AIFPLNumber(1),
                AIFPLList((AIFPLSymbol("list"), AIFPLNumber(2), AIFPLNumber(3)))
            )),
            AIFPLList((
                AIFPLList((
                    AIFPLSymbol("x"),
                    AIFPLList((AIFPLSymbol("y"), AIFPLSymbol("z")))
                )),
                AIFPLList((AIFPLSymbol("+"), AIFPLSymbol("x"), AIFPLSymbol("y"), AIFPLSymbol("z")))
            )),
            AIFPLList((AIFPLSymbol("_"), AIFPLString("no match")))
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, [])
        code = generator.generate()
        
        result = execute_code(code)
        assert result.value == 6
    
    def test_match_type_in_list(self):
        """Test type pattern inside list pattern."""
        analyzer = AIFPLAnalyzer()
        
        # (match (list 42 "hello") ((number? n) (string? s)) (+ n 1)) (_ "no match"))
        expr = AIFPLList((
            AIFPLSymbol("match"),
            AIFPLList((AIFPLSymbol("list"), AIFPLNumber(42), AIFPLString("hello"))),
            AIFPLList((
                AIFPLList((
                    AIFPLList((AIFPLSymbol("number?"), AIFPLSymbol("n"))),
                    AIFPLList((AIFPLSymbol("string?"), AIFPLSymbol("s")))
                )),
                AIFPLList((AIFPLSymbol("+"), AIFPLSymbol("n"), AIFPLNumber(1)))
            )),
            AIFPLList((AIFPLSymbol("_"), AIFPLString("no match")))
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, [])
        code = generator.generate()
        
        result = execute_code(code)
        assert result.value == 43


class TestGenerateMatchMultipleClauses:
    """Test match with multiple clauses."""
    
    def test_match_first_clause(self):
        """Test when first clause matches."""
        analyzer = AIFPLAnalyzer()
        
        # (match 1 (1 "one") (2 "two") (_ "other"))
        expr = AIFPLList((
            AIFPLSymbol("match"),
            AIFPLNumber(1),
            AIFPLList((AIFPLNumber(1), AIFPLString("one"))),
            AIFPLList((AIFPLNumber(2), AIFPLString("two"))),
            AIFPLList((AIFPLSymbol("_"), AIFPLString("other")))
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, [])
        code = generator.generate()
        
        result = execute_code(code)
        assert result.value == "one"
    
    def test_match_second_clause(self):
        """Test when second clause matches."""
        analyzer = AIFPLAnalyzer()
        
        # (match 2 (1 "one") (2 "two") (_ "other"))
        expr = AIFPLList((
            AIFPLSymbol("match"),
            AIFPLNumber(2),
            AIFPLList((AIFPLNumber(1), AIFPLString("one"))),
            AIFPLList((AIFPLNumber(2), AIFPLString("two"))),
            AIFPLList((AIFPLSymbol("_"), AIFPLString("other")))
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, [])
        code = generator.generate()
        
        result = execute_code(code)
        assert result.value == "two"
    
    def test_match_fallthrough_to_wildcard(self):
        """Test when no clause matches and falls through to wildcard."""
        analyzer = AIFPLAnalyzer()
        
        # (match 99 (1 "one") (2 "two") (_ "other"))
        expr = AIFPLList((
            AIFPLSymbol("match"),
            AIFPLNumber(99),
            AIFPLList((AIFPLNumber(1), AIFPLString("one"))),
            AIFPLList((AIFPLNumber(2), AIFPLString("two"))),
            AIFPLList((AIFPLSymbol("_"), AIFPLString("other")))
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, [])
        code = generator.generate()
        
        result = execute_code(code)
        assert result.value == "other"


class TestGenerateMatchCons:
    """Test cons pattern matching."""
    
    def test_match_cons_simple(self):
        """Test simple cons pattern (head . tail)."""
        analyzer = AIFPLAnalyzer()
        
        # (match (list 1 2 3) ((head . tail) head) (_ "no match"))
        expr = AIFPLList((
            AIFPLSymbol("match"),
            AIFPLList((AIFPLSymbol("list"), AIFPLNumber(1), AIFPLNumber(2), AIFPLNumber(3))),
            AIFPLList((
                AIFPLList((AIFPLSymbol("head"), AIFPLSymbol("."), AIFPLSymbol("tail"))),
                AIFPLSymbol("head")
            )),
            AIFPLList((AIFPLSymbol("_"), AIFPLString("no match")))
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, [])
        code = generator.generate()
        
        result = execute_code(code)
        assert result.value == 1
    
    def test_match_cons_tail(self):
        """Test cons pattern accessing tail."""
        analyzer = AIFPLAnalyzer()
        
        # (match (list 1 2 3) ((head . tail) tail) (_ "no match"))
        expr = AIFPLList((
            AIFPLSymbol("match"),
            AIFPLList((AIFPLSymbol("list"), AIFPLNumber(1), AIFPLNumber(2), AIFPLNumber(3))),
            AIFPLList((
                AIFPLList((AIFPLSymbol("head"), AIFPLSymbol("."), AIFPLSymbol("tail"))),
                AIFPLSymbol("tail")
            )),
            AIFPLList((AIFPLSymbol("_"), AIFPLString("no match")))
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, [])
        code = generator.generate()
        
        result = execute_code(code)
        # tail should be (list 2 3)
        assert isinstance(result, AIFPLList)
        assert len(result.elements) == 2
        assert result.elements[0].value == 2
        assert result.elements[1].value == 3
    
    def test_match_cons_multiple_heads(self):
        """Test cons pattern with multiple head elements (a b . rest)."""
        analyzer = AIFPLAnalyzer()
        
        # (match (list 1 2 3 4) ((a b . rest) (+ a b)) (_ "no match"))
        expr = AIFPLList((
            AIFPLSymbol("match"),
            AIFPLList((AIFPLSymbol("list"), AIFPLNumber(1), AIFPLNumber(2), AIFPLNumber(3), AIFPLNumber(4))),
            AIFPLList((
                AIFPLList((AIFPLSymbol("a"), AIFPLSymbol("b"), AIFPLSymbol("."), AIFPLSymbol("rest"))),
                AIFPLList((AIFPLSymbol("+"), AIFPLSymbol("a"), AIFPLSymbol("b")))
            )),
            AIFPLList((AIFPLSymbol("_"), AIFPLString("no match")))
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, [])
        code = generator.generate()
        
        result = execute_code(code)
        assert result.value == 3
    
    def test_match_cons_empty_list_fails(self):
        """Test cons pattern fails on empty list."""
        analyzer = AIFPLAnalyzer()
        
        # (match (list) ((head . tail) "matched") (_ "no match"))
        expr = AIFPLList((
            AIFPLSymbol("match"),
            AIFPLList((AIFPLSymbol("list"),)),
            AIFPLList((
                AIFPLList((AIFPLSymbol("head"), AIFPLSymbol("."), AIFPLSymbol("tail"))),
                AIFPLString("matched")
            )),
            AIFPLList((AIFPLSymbol("_"), AIFPLString("no match")))
        ))
        
        analyzed = analyzer.analyze(expr)
        generator = AIFPLGenerator(analyzed, analyzer.constants, analyzer.names, [])
        code = generator.generate()
        
        result = execute_code(code)
        assert result.value == "no match"
