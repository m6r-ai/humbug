"""
Tests for XML DOCTYPE declaration tokenization.
"""
import pytest

from syntax.xml.xml_lexer import XMLLexer
from syntax.lexer import TokenType


class TestXMLDoctype:
    """Test XML DOCTYPE declaration tokenization."""

    def test_simple_doctype(self):
        """Test simple DOCTYPE declaration."""
        lexer = XMLLexer()
        lexer.lex(None, '<!DOCTYPE root>')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.DOCTYPE
        assert tokens[0].value == '<!DOCTYPE root>'

    def test_doctype_with_system(self):
        """Test DOCTYPE with SYSTEM identifier."""
        lexer = XMLLexer()
        lexer.lex(None, '<!DOCTYPE root SYSTEM "root.dtd">')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.DOCTYPE

    def test_doctype_with_public(self):
        """Test DOCTYPE with PUBLIC identifier."""
        lexer = XMLLexer()
        lexer.lex(None, '<!DOCTYPE root PUBLIC "-//Example//DTD Example 1.0//EN" "example.dtd">')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.DOCTYPE

    def test_doctype_with_internal_subset(self):
        """Test DOCTYPE with internal DTD subset."""
        lexer = XMLLexer()
        lexer.lex(None, '<!DOCTYPE root [<!ELEMENT root (#PCDATA)>]>')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.DOCTYPE

    def test_doctype_html_style(self):
        """Test HTML-style DOCTYPE (also valid in XML contexts)."""
        lexer = XMLLexer()
        lexer.lex(None, '<!DOCTYPE html>')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.DOCTYPE

    def test_doctype_with_url(self):
        """Test DOCTYPE with URL."""
        lexer = XMLLexer()
        lexer.lex(None, '<!DOCTYPE root SYSTEM "http://example.com/root.dtd">')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.DOCTYPE

    def test_doctype_case_sensitive(self):
        """Test that DOCTYPE is case-sensitive."""
        lexer = XMLLexer()
        lexer.lex(None, '<!DOCTYPE root>')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.DOCTYPE

    def test_doctype_before_root_element(self):
        """Test DOCTYPE before root element."""
        lexer = XMLLexer()
        lexer.lex(None, '<!DOCTYPE root><root />')

        tokens = list(lexer._tokens)
        doctype_tokens = [t for t in tokens if t.type == TokenType.DOCTYPE]
        assert len(doctype_tokens) == 1

        tag_tokens = [t for t in tokens if t.type == TokenType.HTML_TAG]
        assert len(tag_tokens) == 1

    def test_doctype_after_xml_declaration(self):
        """Test DOCTYPE after XML declaration."""
        lines = [
            '<?xml version="1.0"?>',
            '<!DOCTYPE root>',
        ]

        for line in lines:
            lexer = XMLLexer()
            lexer.lex(None, line)
            tokens = list(lexer._tokens)
            assert len(tokens) >= 1

    def test_doctype_with_complex_internal_subset(self):
        """Test DOCTYPE with complex internal subset."""
        lexer = XMLLexer()
        lexer.lex(None, '<!DOCTYPE root [<!ELEMENT a (#PCDATA)><!ELEMENT b (#PCDATA)>]>')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.DOCTYPE

    def test_doctype_with_entity_declarations(self):
        """Test DOCTYPE with entity declarations."""
        lexer = XMLLexer()
        lexer.lex(None, '<!DOCTYPE root [<!ENTITY copy "&#169;">]>')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.DOCTYPE

    def test_doctype_with_whitespace(self):
        """Test DOCTYPE with various whitespace."""
        lexer = XMLLexer()
        lexer.lex(None, '<!DOCTYPE  root  SYSTEM  "root.dtd"  >')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.DOCTYPE

    def test_doctype_xhtml_transitional(self):
        """Test XHTML Transitional DOCTYPE."""
        lexer = XMLLexer()
        lexer.lex(None, '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.DOCTYPE

    def test_doctype_xhtml_strict(self):
        """Test XHTML Strict DOCTYPE."""
        lexer = XMLLexer()
        lexer.lex(None, '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.DOCTYPE

    def test_doctype_svg(self):
        """Test SVG DOCTYPE."""
        lexer = XMLLexer()
        lexer.lex(None, '<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.DOCTYPE

    def test_doctype_mathml(self):
        """Test MathML DOCTYPE."""
        lexer = XMLLexer()
        lexer.lex(None, '<!DOCTYPE math PUBLIC "-//W3C//DTD MathML 2.0//EN" "http://www.w3.org/Math/DTD/mathml2/mathml2.dtd">')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.DOCTYPE

    def test_doctype_not_confused_with_comment(self):
        """Test that DOCTYPE is not confused with comment."""
        lexer = XMLLexer()
        lexer.lex(None, '<!DOCTYPE root><!-- comment -->')

        tokens = list(lexer._tokens)
        doctype_tokens = [t for t in tokens if t.type == TokenType.DOCTYPE]
        comment_tokens = [t for t in tokens if t.type == TokenType.COMMENT]
        assert len(doctype_tokens) == 1
        assert len(comment_tokens) == 1

    def test_doctype_with_nested_brackets(self):
        """Test DOCTYPE with nested brackets in internal subset."""
        lexer = XMLLexer()
        lexer.lex(None, '<!DOCTYPE root [<!ELEMENT root (a|b)*>]>')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.DOCTYPE
