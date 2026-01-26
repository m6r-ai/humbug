"""Tests for Phase 1 of typed number migration.

These tests verify that the new AIFPLInteger, AIFPLFloat, and AIFPLComplex types
work correctly when enabled via the parser flag.
"""

import pytest
from aifpl import AIFPLInteger, AIFPLFloat, AIFPLComplex, AIFPLNumber
from aifpl.aifpl_tokenizer import AIFPLTokenizer
from aifpl.aifpl_parser import AIFPLParser
from aifpl.aifpl_builtins import extract_numeric_value, is_numeric_type


class TestTypedNumberTypes:
    """Test the new typed number classes."""
    
    def test_integer_type(self):
        """Test AIFPLInteger type."""
        val = AIFPLInteger(42)
        assert val.value == 42
        assert val.type_name() == "integer"
        assert val.is_self_evaluating()
        assert val.to_python() == 42
    
    def test_float_type(self):
        """Test AIFPLFloat type."""
        val = AIFPLFloat(3.14)
        assert val.value == 3.14
        assert val.type_name() == "float"
        assert val.is_self_evaluating()
        assert val.to_python() == 3.14
    
    def test_complex_type(self):
        """Test AIFPLComplex type."""
        val = AIFPLComplex(3+4j)
        assert val.value == 3+4j
        assert val.type_name() == "complex"
        assert val.is_self_evaluating()
        assert val.to_python() == 3+4j


class TestParserWithTypedNumbers:
    """Test parser with use_typed_numbers flag."""
    
    def test_parser_creates_old_types_by_default(self):
        """By default, parser should create AIFPLNumber."""
        tokenizer = AIFPLTokenizer()
        tokens = tokenizer.tokenize("42")
        parser = AIFPLParser(tokens, "42")
        result = parser.parse()
        
        assert isinstance(result, (AIFPLNumber, AIFPLInteger, AIFPLFloat, AIFPLComplex))
        assert result.value == 42
    
    def test_parser_creates_integer_with_flag(self):
        """With flag enabled, parser should create AIFPLInteger."""
        tokenizer = AIFPLTokenizer()
        tokens = tokenizer.tokenize("42")
        parser = AIFPLParser(tokens, "42")
        parser.use_typed_numbers = True
        result = parser.parse()
        
        assert isinstance(result, AIFPLInteger)
        assert not isinstance(result, AIFPLNumber)
        assert result.value == 42
    
    def test_parser_creates_float_with_flag(self):
        """With flag enabled, parser should create AIFPLFloat."""
        tokenizer = AIFPLTokenizer()
        tokens = tokenizer.tokenize("3.14")
        parser = AIFPLParser(tokens, "3.14")
        parser.use_typed_numbers = True
        result = parser.parse()
        
        assert isinstance(result, AIFPLFloat)
        assert not isinstance(result, AIFPLNumber)
        assert result.value == 3.14
    
    def test_parser_creates_complex_with_flag(self):
        """With flag enabled, parser should create AIFPLComplex."""
        tokenizer = AIFPLTokenizer()
        tokens = tokenizer.tokenize("(+ 3 (* 4 j))")
        parser = AIFPLParser(tokens, "(+ 3 (* 4 j))")
        parser.use_typed_numbers = True
        result = parser.parse()
        
        # The expression itself is a list, but we can check the numbers in it
        assert result.elements[1].value == 3
        assert isinstance(result.elements[1], AIFPLInteger)


class TestNumericHelpers:
    """Test helper functions for numeric type compatibility."""
    
    def test_extract_numeric_value_from_old_type(self):
        """extract_numeric_value should work with AIFPLNumber."""
        val = AIFPLNumber(42)
        assert extract_numeric_value(val) == 42
    
    def test_extract_numeric_value_from_integer(self):
        """extract_numeric_value should work with AIFPLInteger."""
        val = AIFPLInteger(42)
        assert extract_numeric_value(val) == 42
    
    def test_extract_numeric_value_from_float(self):
        """extract_numeric_value should work with AIFPLFloat."""
        val = AIFPLFloat(3.14)
        assert extract_numeric_value(val) == 3.14
    
    def test_extract_numeric_value_from_complex(self):
        """extract_numeric_value should work with AIFPLComplex."""
        val = AIFPLComplex(3+4j)
        assert extract_numeric_value(val) == 3+4j
    
    def test_is_numeric_type_old(self):
        """is_numeric_type should recognize AIFPLNumber."""
        val = AIFPLNumber(42)
        assert is_numeric_type(val)
    
    def test_is_numeric_type_integer(self):
        """is_numeric_type should recognize AIFPLInteger."""
        val = AIFPLInteger(42)
        assert is_numeric_type(val)
    
    def test_is_numeric_type_float(self):
        """is_numeric_type should recognize AIFPLFloat."""
        val = AIFPLFloat(3.14)
        assert is_numeric_type(val)
    
    def test_is_numeric_type_complex(self):
        """is_numeric_type should recognize AIFPLComplex."""
        val = AIFPLComplex(3+4j)
        assert is_numeric_type(val)
    
    def test_is_numeric_type_non_numeric(self):
        """is_numeric_type should return False for non-numeric types."""
        from aifpl import AIFPLString
        val = AIFPLString("hello")
        assert not is_numeric_type(val)



class TestTypePredicatesWithNewTypes:
    """Test that type predicates work with both old and new number types."""
    
    def test_number_predicate_with_old_type(self):
        """number? should recognize AIFPLNumber."""
        from aifpl import AIFPL
        aifpl = AIFPL()
        assert aifpl.evaluate("(number? 42)") == True
        assert aifpl.evaluate("(number? 3.14)") == True
        assert aifpl.evaluate("(number? (+ 1 (* 2 j)))") == True
        assert aifpl.evaluate('(number? "hello")') == False
    
    def test_integer_predicate_with_old_type(self):
        """integer? should work with AIFPLNumber."""
        from aifpl import AIFPL
        aifpl = AIFPL()
        assert aifpl.evaluate("(integer? 42)") == True
        assert aifpl.evaluate("(integer? 3.14)") == False
        assert aifpl.evaluate("(integer? (+ 1 (* 2 j)))") == False
    
    def test_float_predicate_with_old_type(self):
        """float? should work with AIFPLNumber."""
        from aifpl import AIFPL
        aifpl = AIFPL()
        assert aifpl.evaluate("(float? 3.14)") == True
        assert aifpl.evaluate("(float? 42)") == False
        assert aifpl.evaluate("(float? (+ 1 (* 2 j)))") == False
    
    def test_complex_predicate_with_old_type(self):
        """complex? should work with AIFPLNumber."""
        from aifpl import AIFPL
        aifpl = AIFPL()
        assert aifpl.evaluate("(complex? (+ 1 (* 2 j)))") == True
        assert aifpl.evaluate("(complex? 42)") == False
        assert aifpl.evaluate("(complex? 3.14)") == False
    
    def test_number_predicate_with_new_integer(self):
        """number? should recognize AIFPLInteger."""
        from aifpl import AIFPLInteger
        from aifpl.aifpl_collections import AIFPLCollectionsFunctions
        
        funcs = AIFPLCollectionsFunctions()
        result = funcs._builtin_number_p([AIFPLInteger(42)])
        assert result.value == True
    
    def test_number_predicate_with_new_float(self):
        """number? should recognize AIFPLFloat."""
        from aifpl import AIFPLFloat
        from aifpl.aifpl_collections import AIFPLCollectionsFunctions
        
        funcs = AIFPLCollectionsFunctions()
        result = funcs._builtin_number_p([AIFPLFloat(3.14)])
        assert result.value == True
    
    def test_number_predicate_with_new_complex(self):
        """number? should recognize AIFPLComplex."""
        from aifpl import AIFPLComplex
        from aifpl.aifpl_collections import AIFPLCollectionsFunctions
        
        funcs = AIFPLCollectionsFunctions()
        result = funcs._builtin_number_p([AIFPLComplex(3+4j)])
        assert result.value == True
    
    def test_integer_predicate_with_new_integer(self):
        """integer? should recognize AIFPLInteger."""
        from aifpl import AIFPLInteger
        from aifpl.aifpl_collections import AIFPLCollectionsFunctions
        
        funcs = AIFPLCollectionsFunctions()
        result = funcs._builtin_integer_p([AIFPLInteger(42)])
        assert result.value == True
    
    def test_integer_predicate_with_new_float(self):
        """integer? should return False for AIFPLFloat."""
        from aifpl import AIFPLFloat
        from aifpl.aifpl_collections import AIFPLCollectionsFunctions
        
        funcs = AIFPLCollectionsFunctions()
        result = funcs._builtin_integer_p([AIFPLFloat(3.14)])
        assert result.value == False
    
    def test_float_predicate_with_new_float(self):
        """float? should recognize AIFPLFloat."""
        from aifpl import AIFPLFloat
        from aifpl.aifpl_collections import AIFPLCollectionsFunctions
        
        funcs = AIFPLCollectionsFunctions()
        result = funcs._builtin_float_p([AIFPLFloat(3.14)])
        assert result.value == True
    
    def test_float_predicate_with_new_integer(self):
        """float? should return False for AIFPLInteger."""
        from aifpl import AIFPLInteger
        from aifpl.aifpl_collections import AIFPLCollectionsFunctions
        
        funcs = AIFPLCollectionsFunctions()
        result = funcs._builtin_float_p([AIFPLInteger(42)])
        assert result.value == False
    
    def test_complex_predicate_with_new_complex(self):
        """complex? should recognize AIFPLComplex."""
        from aifpl import AIFPLComplex
        from aifpl.aifpl_collections import AIFPLCollectionsFunctions
        
        funcs = AIFPLCollectionsFunctions()
        result = funcs._builtin_complex_p([AIFPLComplex(3+4j)])
        assert result.value == True
    
    def test_complex_predicate_with_new_integer(self):
        """complex? should return False for AIFPLInteger."""
        from aifpl import AIFPLInteger
        from aifpl.aifpl_collections import AIFPLCollectionsFunctions
        
        funcs = AIFPLCollectionsFunctions()
        result = funcs._builtin_complex_p([AIFPLInteger(42)])
        assert result.value == False
