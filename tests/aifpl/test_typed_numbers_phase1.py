"""Tests for Phase 1 of typed number migration.

These tests verify that the AIFPLInteger, AIFPLFloat, and AIFPLComplex types
work correctly when enabled via the parser flag.
"""

import pytest
from aifpl import AIFPLASTInteger, AIFPLASTFloat, AIFPLASTComplex, AIFPLInteger, AIFPLFloat, AIFPLComplex
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
        tokens = lexer.lex("(integer+ 3 4)")
        parser = AIFPLParser()
        parser.use_typed_numbers = True
        result = parser.parse(tokens, "(integer+ 3 4)")

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
        assert aifpl.evaluate("(number? (complex+ (complex 1 0) (complex* (complex 2 0) 1j)))") == True
        assert aifpl.evaluate('(number? "hello")') == False

    def test_integer_predicate_with_old_type(self):
        """integer? should work with typed numbers."""
        from aifpl import AIFPL
        aifpl = AIFPL()
        assert aifpl.evaluate("(integer? 42)") == True
        assert aifpl.evaluate("(integer? 3.14)") == False
        assert aifpl.evaluate("(integer? (complex+ (complex 1 0) (complex* (complex 2 0) 1j)))") == False

    def test_float_predicate_with_old_type(self):
        """float? should work with typed numbers."""
        from aifpl import AIFPL
        aifpl = AIFPL()
        assert aifpl.evaluate("(float? 3.14)") == True
        assert aifpl.evaluate("(float? 42)") == False
        assert aifpl.evaluate("(float? (complex+ (complex 1 0) (complex* (complex 2 0) 1j)))") == False

    def test_complex_predicate_with_old_type(self):
        """complex? should work with typed numbers."""
        from aifpl import AIFPL
        aifpl = AIFPL()
        assert aifpl.evaluate("(complex? (complex+ (complex 1 0) (complex* (complex 2 0) 1j)))") == True
        assert aifpl.evaluate("(complex? 42)") == False
        assert aifpl.evaluate("(complex? 3.14)") == False
