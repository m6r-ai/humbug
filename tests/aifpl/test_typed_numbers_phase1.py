"""Tests for Phase 1 of typed number migration.

These tests verify that the new AIFPLASTInteger, AIFPLASTFloat, and AIFPLASTComplex types
work correctly when enabled via the parser flag.
"""

import pytest
from aifpl import AIFPLASTInteger, AIFPLASTFloat, AIFPLASTComplex
from aifpl.aifpl_lexer import AIFPLLexer
from aifpl.aifpl_parser import AIFPLParser


class TestParserWithTypedNumbers:
    """Test parser with use_typed_numbers flag."""

    def test_parser_creates_old_types_by_default(self):
        """Parser should create typed numbers."""
        lexer = AIFPLLexer()
        tokens = lexer.lex("42")
        parser = AIFPLParser()
        result = parser.parse(tokens, "42")

        assert isinstance(result, (AIFPLASTInteger, AIFPLASTFloat, AIFPLASTComplex))
        assert result.value == 42

    def test_parser_creates_integer_with_flag(self):
        """With flag enabled, parser should create AIFPLASTInteger."""
        lexer = AIFPLLexer()
        tokens = lexer.lex("42")
        parser = AIFPLParser()
        parser.use_typed_numbers = True
        result = parser.parse(tokens, "42")

        assert isinstance(result, AIFPLASTInteger)
        assert result.value == 42

    def test_parser_creates_float_with_flag(self):
        """With flag enabled, parser should create AIFPLASTFloat."""
        lexer = AIFPLLexer()
        tokens = lexer.lex("3.14")
        parser = AIFPLParser()
        parser.use_typed_numbers = True
        result = parser.parse(tokens, "3.14")

        assert isinstance(result, AIFPLASTFloat)
        assert result.value == 3.14

    def test_parser_creates_complex_with_flag(self):
        """With flag enabled, parser should create AIFPLASTComplex."""
        lexer = AIFPLLexer()
        tokens = lexer.lex("(+ 3 (* 4 1j))")
        parser = AIFPLParser()
        parser.use_typed_numbers = True
        result = parser.parse(tokens, "(+ 3 (* 4 1j))")

        # The expression itself is a list, but we can check the numbers in it
        assert result.elements[1].value == 3
        assert isinstance(result.elements[1], AIFPLASTInteger)


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
        """number? should recognize AIFPLASTInteger."""
        from aifpl import AIFPLASTInteger
        from aifpl.aifpl_collections import AIFPLCollectionsFunctions

        funcs = AIFPLCollectionsFunctions()
        result = funcs._builtin_number_p([AIFPLASTInteger(42)])
        assert result.value == True

    def test_number_predicate_with_new_float(self):
        """number? should recognize AIFPLASTFloat."""
        from aifpl import AIFPLASTFloat
        from aifpl.aifpl_collections import AIFPLCollectionsFunctions

        funcs = AIFPLCollectionsFunctions()
        result = funcs._builtin_number_p([AIFPLASTFloat(3.14)])
        assert result.value == True

    def test_number_predicate_with_new_complex(self):
        """number? should recognize AIFPLASTComplex."""
        from aifpl import AIFPLASTComplex
        from aifpl.aifpl_collections import AIFPLCollectionsFunctions

        funcs = AIFPLCollectionsFunctions()
        result = funcs._builtin_number_p([AIFPLASTComplex(3+4j)])
        assert result.value == True

    def test_integer_predicate_with_new_integer(self):
        """integer? should recognize AIFPLASTInteger."""
        from aifpl import AIFPLASTInteger
        from aifpl.aifpl_collections import AIFPLCollectionsFunctions

        funcs = AIFPLCollectionsFunctions()
        result = funcs._builtin_integer_p([AIFPLASTInteger(42)])
        assert result.value == True

    def test_integer_predicate_with_new_float(self):
        """integer? should return False for AIFPLASTFloat."""
        from aifpl import AIFPLASTFloat
        from aifpl.aifpl_collections import AIFPLCollectionsFunctions

        funcs = AIFPLCollectionsFunctions()
        result = funcs._builtin_integer_p([AIFPLASTFloat(3.14)])
        assert result.value == False

    def test_float_predicate_with_new_float(self):
        """float? should recognize AIFPLASTFloat."""
        from aifpl import AIFPLASTFloat
        from aifpl.aifpl_collections import AIFPLCollectionsFunctions

        funcs = AIFPLCollectionsFunctions()
        result = funcs._builtin_float_p([AIFPLASTFloat(3.14)])
        assert result.value == True

    def test_float_predicate_with_new_integer(self):
        """float? should return False for AIFPLASTInteger."""
        from aifpl import AIFPLASTInteger
        from aifpl.aifpl_collections import AIFPLCollectionsFunctions

        funcs = AIFPLCollectionsFunctions()
        result = funcs._builtin_float_p([AIFPLASTInteger(42)])
        assert result.value == False

    def test_complex_predicate_with_new_complex(self):
        """complex? should recognize AIFPLASTComplex."""
        from aifpl import AIFPLASTComplex
        from aifpl.aifpl_collections import AIFPLCollectionsFunctions

        funcs = AIFPLCollectionsFunctions()
        result = funcs._builtin_complex_p([AIFPLASTComplex(3+4j)])
        assert result.value == True

    def test_complex_predicate_with_new_integer(self):
        """complex? should return False for AIFPLASTInteger."""
        from aifpl import AIFPLASTInteger
        from aifpl.aifpl_collections import AIFPLCollectionsFunctions

        funcs = AIFPLCollectionsFunctions()
        result = funcs._builtin_complex_p([AIFPLASTInteger(42)])
        assert result.value == False
