"""
Tests for HTML self-closing tags.
"""
import pytest

from syntax.html.html_lexer import HTMLLexer
from syntax.lexer import TokenType


class TestHTMLSelfClosing:
    """Test HTML self-closing tag handling."""

    def test_simple_self_closing_tag(self):
        """Test simple self-closing tag."""
        lexer = HTMLLexer()
        lexer.lex(None, '<br />')

        tokens = list(lexer._tokens)
        tag_tokens = [t for t in tokens if t.type == TokenType.HTML_TAG]
        assert len(tag_tokens) == 1
        assert tag_tokens[0].value == 'br'

    def test_self_closing_with_attribute(self):
        """Test self-closing tag with attribute."""
        lexer = HTMLLexer()
        lexer.lex(None, '<img src="test.png" />')

        tokens = list(lexer._tokens)
        tag_tokens = [t for t in tokens if t.type == TokenType.HTML_TAG]
        assert len(tag_tokens) == 1

        attr_tokens = [t for t in tokens if t.type == TokenType.HTML_ATTRIBUTE]
        assert len(attr_tokens) == 1
        assert attr_tokens[0].value == 'src'

    def test_self_closing_with_multiple_attributes(self):
        """Test self-closing tag with multiple attributes."""
        lexer = HTMLLexer()
        lexer.lex(None, '<input type="text" name="username" />')

        tokens = list(lexer._tokens)
        attr_tokens = [t for t in tokens if t.type == TokenType.HTML_ATTRIBUTE]
        assert len(attr_tokens) == 2
        assert attr_tokens[0].value == 'type'
        assert attr_tokens[1].value == 'name'

    def test_self_closing_no_space_before_slash(self):
        """Test self-closing tag with no space before slash."""
        lexer = HTMLLexer()
        lexer.lex(None, '<br/>')

        tokens = list(lexer._tokens)
        tag_tokens = [t for t in tokens if t.type == TokenType.HTML_TAG]
        assert len(tag_tokens) == 1

    def test_self_closing_with_spaces(self):
        """Test self-closing tag with extra spaces."""
        lexer = HTMLLexer()
        lexer.lex(None, '<hr   />')

        tokens = list(lexer._tokens)
        tag_tokens = [t for t in tokens if t.type == TokenType.HTML_TAG]
        assert len(tag_tokens) == 1
        assert tag_tokens[0].value == 'hr'

    def test_void_elements(self):
        """Test common HTML void elements."""
        void_elements = ['br', 'hr', 'img', 'input', 'meta', 'link']

        for element in void_elements:
            lexer = HTMLLexer()
            lexer.lex(None, f'<{element} />')

            tokens = list(lexer._tokens)
            tag_tokens = [t for t in tokens if t.type == TokenType.HTML_TAG]
            assert len(tag_tokens) == 1
            assert tag_tokens[0].value == element

    def test_self_closing_with_data_attributes(self):
        """Test self-closing tag with data attributes."""
        lexer = HTMLLexer()
        lexer.lex(None, '<div data-value="test" />')

        tokens = list(lexer._tokens)
        attr_tokens = [t for t in tokens if t.type == TokenType.HTML_ATTRIBUTE]
        assert len(attr_tokens) == 1
        assert attr_tokens[0].value == 'data-value'

    def test_slash_not_treated_as_attribute(self):
        """Test that standalone slash is not treated as an attribute."""
        lexer = HTMLLexer()
        lexer.lex(None, '<img src="test.png" />')

        tokens = list(lexer._tokens)
        attr_tokens = [t for t in tokens if t.type == TokenType.HTML_ATTRIBUTE]
        # Should only have 'src', not '/'
        assert len(attr_tokens) == 1
        assert all(t.value != '/' for t in attr_tokens)
