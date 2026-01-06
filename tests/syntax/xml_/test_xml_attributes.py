"""
Tests for XML attribute tokenization.
"""
import pytest

from syntax.xml.xml_lexer import XMLLexer
from syntax.lexer import TokenType


class TestXMLAttributes:
    """Test XML attribute tokenization."""

    def test_simple_attribute(self):
        """Test simple attribute with quoted value."""
        lexer = XMLLexer()
        lexer.lex(None, '<tag attr="value">')

        tokens = list(lexer._tokens)
        attr_tokens = [t for t in tokens if t.type == TokenType.HTML_ATTRIBUTE]
        assert len(attr_tokens) == 1
        assert attr_tokens[0].value == 'attr'

        string_tokens = [t for t in tokens if t.type == TokenType.STRING]
        assert len(string_tokens) == 1
        assert string_tokens[0].value == '"value"'

    def test_attribute_with_single_quotes(self):
        """Test attribute with single-quoted value."""
        lexer = XMLLexer()
        lexer.lex(None, "<tag attr='value'>")

        tokens = list(lexer._tokens)
        attr_tokens = [t for t in tokens if t.type == TokenType.HTML_ATTRIBUTE]
        assert len(attr_tokens) == 1

        string_tokens = [t for t in tokens if t.type == TokenType.STRING]
        assert len(string_tokens) == 1
        assert string_tokens[0].value == "'value'"

    def test_multiple_attributes(self):
        """Test multiple attributes."""
        lexer = XMLLexer()
        lexer.lex(None, '<tag attr1="value1" attr2="value2">')

        tokens = list(lexer._tokens)
        attr_tokens = [t for t in tokens if t.type == TokenType.HTML_ATTRIBUTE]
        assert len(attr_tokens) == 2
        assert attr_tokens[0].value == 'attr1'
        assert attr_tokens[1].value == 'attr2'

    def test_attribute_with_hyphen(self):
        """Test attribute name with hyphen."""
        lexer = XMLLexer()
        lexer.lex(None, '<tag my-attr="value">')

        tokens = list(lexer._tokens)
        attr_tokens = [t for t in tokens if t.type == TokenType.HTML_ATTRIBUTE]
        assert len(attr_tokens) == 1
        assert attr_tokens[0].value == 'my-attr'

    def test_attribute_with_underscore(self):
        """Test attribute name with underscore."""
        lexer = XMLLexer()
        lexer.lex(None, '<tag my_attr="value">')

        tokens = list(lexer._tokens)
        attr_tokens = [t for t in tokens if t.type == TokenType.HTML_ATTRIBUTE]
        assert len(attr_tokens) == 1
        assert attr_tokens[0].value == 'my_attr'

    def test_attribute_with_namespace(self):
        """Test attribute with namespace prefix."""
        lexer = XMLLexer()
        lexer.lex(None, '<tag xmlns:ns="http://example.com">')

        tokens = list(lexer._tokens)
        attr_tokens = [t for t in tokens if t.type == TokenType.HTML_ATTRIBUTE]
        assert len(attr_tokens) == 1
        assert attr_tokens[0].value == 'xmlns:ns'

    def test_attribute_with_numbers(self):
        """Test attribute name with numbers."""
        lexer = XMLLexer()
        lexer.lex(None, '<tag attr123="value">')

        tokens = list(lexer._tokens)
        attr_tokens = [t for t in tokens if t.type == TokenType.HTML_ATTRIBUTE]
        assert len(attr_tokens) == 1
        assert attr_tokens[0].value == 'attr123'

    def test_attribute_empty_value(self):
        """Test attribute with empty value."""
        lexer = XMLLexer()
        lexer.lex(None, '<tag attr="">')

        tokens = list(lexer._tokens)
        attr_tokens = [t for t in tokens if t.type == TokenType.HTML_ATTRIBUTE]
        assert len(attr_tokens) == 1

        string_tokens = [t for t in tokens if t.type == TokenType.STRING]
        assert len(string_tokens) == 1
        assert string_tokens[0].value == '""'

    def test_attribute_with_special_chars_in_value(self):
        """Test attribute value with special characters."""
        lexer = XMLLexer()
        lexer.lex(None, '<tag attr="value with spaces and &amp;">')

        tokens = list(lexer._tokens)
        string_tokens = [t for t in tokens if t.type == TokenType.STRING]
        assert len(string_tokens) == 1
        assert 'value with spaces and &amp;' in string_tokens[0].value

    def test_attribute_with_url_value(self):
        """Test attribute with URL value."""
        lexer = XMLLexer()
        lexer.lex(None, '<tag href="https://example.com/path?query=value">')

        tokens = list(lexer._tokens)
        string_tokens = [t for t in tokens if t.type == TokenType.STRING]
        assert len(string_tokens) == 1
        assert 'https://example.com/path?query=value' in string_tokens[0].value

    def test_attribute_case_sensitivity(self):
        """Test that attribute names preserve case."""
        lexer = XMLLexer()
        lexer.lex(None, '<tag MyAttr="value">')

        tokens = list(lexer._tokens)
        attr_tokens = [t for t in tokens if t.type == TokenType.HTML_ATTRIBUTE]
        assert len(attr_tokens) == 1
        assert attr_tokens[0].value == 'MyAttr'

    def test_attributes_with_whitespace(self):
        """Test attributes with various whitespace."""
        lexer = XMLLexer()
        lexer.lex(None, '<tag  attr1="value1"   attr2="value2"  >')

        tokens = list(lexer._tokens)
        attr_tokens = [t for t in tokens if t.type == TokenType.HTML_ATTRIBUTE]
        assert len(attr_tokens) == 2

    def test_attribute_equals_sign(self):
        """Test that equals sign is tokenized as operator."""
        lexer = XMLLexer()
        lexer.lex(None, '<tag attr="value">')

        tokens = list(lexer._tokens)
        equals_tokens = [t for t in tokens if t.type == TokenType.OPERATOR and t.value == '=']
        assert len(equals_tokens) == 1

    def test_self_closing_tag_with_attributes(self):
        """Test self-closing tag with attributes."""
        lexer = XMLLexer()
        lexer.lex(None, '<tag attr="value" />')

        tokens = list(lexer._tokens)
        attr_tokens = [t for t in tokens if t.type == TokenType.HTML_ATTRIBUTE]
        assert len(attr_tokens) == 1

    def test_attribute_with_numeric_value(self):
        """Test attribute with numeric value."""
        lexer = XMLLexer()
        lexer.lex(None, '<tag count="42">')

        tokens = list(lexer._tokens)
        string_tokens = [t for t in tokens if t.type == TokenType.STRING]
        assert len(string_tokens) == 1
        assert '42' in string_tokens[0].value

    def test_boolean_style_attribute(self):
        """Test boolean-style attribute (common in XHTML)."""
        lexer = XMLLexer()
        lexer.lex(None, '<tag checked="checked">')

        tokens = list(lexer._tokens)
        attr_tokens = [t for t in tokens if t.type == TokenType.HTML_ATTRIBUTE]
        assert len(attr_tokens) == 1
        assert attr_tokens[0].value == 'checked'

    def test_attribute_with_quotes_in_value(self):
        """Test attribute value containing opposite quote type."""
        lexer = XMLLexer()
        lexer.lex(None, '''<tag attr="value with 'single' quotes">''')

        tokens = list(lexer._tokens)
        string_tokens = [t for t in tokens if t.type == TokenType.STRING]
        assert len(string_tokens) == 1

    def test_multiple_namespace_attributes(self):
        """Test multiple namespace declarations."""
        lexer = XMLLexer()
        lexer.lex(None, '<tag xmlns:ns1="uri1" xmlns:ns2="uri2">')

        tokens = list(lexer._tokens)
        attr_tokens = [t for t in tokens if t.type == TokenType.HTML_ATTRIBUTE]
        assert len(attr_tokens) == 2
        assert 'xmlns:ns1' in [t.value for t in attr_tokens]
        assert 'xmlns:ns2' in [t.value for t in attr_tokens]

    def test_attribute_without_quotes(self):
        """Test unquoted attribute value (technically valid in some contexts)."""
        lexer = XMLLexer()
        lexer.lex(None, '<tag attr=value>')

        tokens = list(lexer._tokens)
        # The lexer should handle this, though it's not standard XML
        attr_tokens = [t for t in tokens if t.type == TokenType.HTML_ATTRIBUTE]
        assert len(attr_tokens) >= 1

    def test_xml_standard_attributes(self):
        """Test XML standard attributes like xml:lang."""
        lexer = XMLLexer()
        lexer.lex(None, '<tag xml:lang="en" xml:space="preserve">')

        tokens = list(lexer._tokens)
        attr_tokens = [t for t in tokens if t.type == TokenType.HTML_ATTRIBUTE]
        assert len(attr_tokens) == 2
        assert 'xml:lang' in [t.value for t in attr_tokens]
        assert 'xml:space' in [t.value for t in attr_tokens]
