"""Tests for Phase 1 of typed number migration.

These tests verify that the new AIFPLInteger, AIFPLFloat, and AIFPLComplex types
work correctly when enabled via the parser flag.
"""

import pytest
from aifpl import AIFPLInteger, AIFPLFloat, AIFPLComplex
from aifpl.aifpl_lexer import AIFPLLexer
from aifpl.aifpl_parser import AIFPLParser
from aifpl.aifpl_builtins import is_numeric_type


class TestParserWithTypedNumbers:
    """Test parser with use_typed_numbers flag."""
    
    def test_parser_creates_old_types_by_default(self):
        """Parser should create typed numbers."""
        lexer = AIFPLLexer()
        tokens = lexer.lex("42")
        parser = AIFPLParser()
        result = parser.parse(tokens, "42")
        
        assert isinstance(result, (AIFPLInteger, AIFPLFloat, AIFPLComplex))
        assert result.value == 42
    
    def test_parser_creates_integer_with_flag(self):
        """With flag enabled, parser should create AIFPLInteger."""
        lexer = AIFPLLexer()
        tokens = lexer.lex("42")
        parser = AIFPLParser()
        parser.use_typed_numbers = True
        result = parser.parse(tokens, "42")
        
        assert isinstance(result, AIFPLInteger)
        assert result.value == 42
    
    def test_parser_creates_float_with_flag(self):
        """With flag enabled, parser should create AIFPLFloat."""
        lexer = AIFPLLexer()
        tokens = lexer.lex("3.14")
        parser = AIFPLParser()
        parser.use_typed_numbers = True
        result = parser.parse(tokens, "3.14")
        
        assert isinstance(result, AIFPLFloat)
        assert result.value == 3.14
    
    def test_parser_creates_complex_with_flag(self):
        """With flag enabled, parser should create AIFPLComplex."""
        lexer = AIFPLLexer()
        tokens = lexer.lex("(+ 3 (* 4 1j))")
        parser = AIFPLParser()
        parser.use_typed_numbers = True
        result = parser.parse(tokens, "(+ 3 (* 4 1j))")
        
        # The expression itself is a list, but we can check the numbers in it
        assert result.elements[1].value == 3
        assert isinstance(result.elements[1], AIFPLInteger)


class TestNumericHelpers:
    """Test helper functions for numeric type compatibility."""
    
    def test_is_numeric_type_old(self):
        """is_numeric_type should recognize typed numbers."""
        val = AIFPLInteger(42)
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
    """Test that type predicates work with typed numbers."""
    
    def test_number_predicate_with_old_type(self):
        """number? should recognize typed numbers."""
        from aifpl import AIFPL
        aifpl = AIFPL()
        assert aifpl.evaluate("(number? 42)") == True
        assert aifpl.evaluate("(number? 3.14)") == True
        assert aifpl.evaluate("(number? (+ 1 (* 2 1j)))") == True
        assert aifpl.evaluate('(number? "hello")') == False
    
    def test_integer_predicate_with_old_type(self):
        """integer? should work with typed numbers."""
        from aifpl import AIFPL
        aifpl = AIFPL()
        assert aifpl.evaluate("(integer? 42)") == True
        assert aifpl.evaluate("(integer? 3.14)") == False
        assert aifpl.evaluate("(integer? (+ 1 (* 2 1j)))") == False
    
    def test_float_predicate_with_old_type(self):
        """float? should work with typed numbers."""
        from aifpl import AIFPL
        aifpl = AIFPL()
        assert aifpl.evaluate("(float? 3.14)") == True
        assert aifpl.evaluate("(float? 42)") == False
        assert aifpl.evaluate("(float? (+ 1 (* 2 1j)))") == False
    
    def test_complex_predicate_with_old_type(self):
        """complex? should work with typed numbers."""
        from aifpl import AIFPL
        aifpl = AIFPL()
        assert aifpl.evaluate("(complex? (+ 1 (* 2 1j)))") == True
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
