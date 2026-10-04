"""
Tests for XML DOCTYPE edge cases to achieve 100% coverage.
"""
import pytest

from syntax.xml.xml_lexer import XMLLexer
from syntax.lexer import TokenType


class TestXMLDoctypeEdgeCases:
    """Test XML DOCTYPE edge cases for complete coverage."""

    def test_doctype_without_closing_bracket(self):
        """Test DOCTYPE that reaches end of line without closing >."""
        lexer = XMLLexer()
        lexer.lex(None, '<!DOCTYPE root SYSTEM "root.dtd"')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.DOCTYPE
        # Should consume everything to end of line
        assert 'root.dtd' in tokens[0].value

    def test_doctype_with_nested_brackets_unclosed(self):
        """Test DOCTYPE with unclosed nested brackets."""
        lexer = XMLLexer()
        lexer.lex(None, '<!DOCTYPE root [<!ELEMENT test (#PCDATA)')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.DOCTYPE
        # Should consume to end even with unclosed brackets

    def test_doctype_ends_at_bracket_depth_zero(self):
        """Test DOCTYPE that ends when bracket depth returns to 0."""
        lexer = XMLLexer()
        lexer.lex(None, '<!DOCTYPE root [<!ELEMENT a (#PCDATA)>]>')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.DOCTYPE
        # Should properly close at the final >
        assert tokens[0].value.endswith('>')
