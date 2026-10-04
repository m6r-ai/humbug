"""
Tests for XML tag tokenization.
"""
import pytest

from syntax.xml.xml_lexer import XMLLexer
from syntax.lexer import TokenType


class TestXMLTags:
    """Test XML tag tokenization."""

    def test_simple_tag(self):
        """Test simple opening and closing tags."""
        lexer = XMLLexer()
        lexer.lex(None, '<tag>')

        tokens = list(lexer._tokens)
        assert len(tokens) == 3, "Should have <, tag name, and >"
        assert tokens[0].type == TokenType.OPERATOR
        assert tokens[0].value == '<'
        assert tokens[1].type == TokenType.HTML_TAG
        assert tokens[1].value == 'tag'
        assert tokens[2].type == TokenType.OPERATOR
        assert tokens[2].value == '>'

    def test_closing_tag(self):
        """Test closing tag."""
        lexer = XMLLexer()
        lexer.lex(None, '</tag>')

        tokens = list(lexer._tokens)
        assert len(tokens) == 3
        assert tokens[0].type == TokenType.OPERATOR
        assert tokens[0].value == '<'
        assert tokens[1].type == TokenType.HTML_TAG
        assert tokens[1].value == '/tag'

    def test_self_closing_tag(self):
        """Test self-closing tag."""
        lexer = XMLLexer()
        lexer.lex(None, '<tag />')

        tokens = list(lexer._tokens)
        tag_tokens = [t for t in tokens if t.type == TokenType.HTML_TAG]
        assert len(tag_tokens) == 1
        assert tag_tokens[0].value == 'tag'

    def test_tag_with_hyphen(self):
        """Test tag name with hyphen."""
        lexer = XMLLexer()
        lexer.lex(None, '<my-tag>')

        tokens = list(lexer._tokens)
        tag_tokens = [t for t in tokens if t.type == TokenType.HTML_TAG]
        assert len(tag_tokens) == 1
        assert tag_tokens[0].value == 'my-tag'

    def test_tag_with_underscore(self):
        """Test tag name with underscore."""
        lexer = XMLLexer()
        lexer.lex(None, '<my_tag>')

        tokens = list(lexer._tokens)
        tag_tokens = [t for t in tokens if t.type == TokenType.HTML_TAG]
        assert len(tag_tokens) == 1
        assert tag_tokens[0].value == 'my_tag'

    def test_tag_with_numbers(self):
        """Test tag name with numbers."""
        lexer = XMLLexer()
        lexer.lex(None, '<tag123>')

        tokens = list(lexer._tokens)
        tag_tokens = [t for t in tokens if t.type == TokenType.HTML_TAG]
        assert len(tag_tokens) == 1
        assert tag_tokens[0].value == 'tag123'

    def test_tag_with_namespace(self):
        """Test tag with namespace prefix."""
        lexer = XMLLexer()
        lexer.lex(None, '<ns:tag>')

        tokens = list(lexer._tokens)
        tag_tokens = [t for t in tokens if t.type == TokenType.HTML_TAG]
        assert len(tag_tokens) == 1
        assert tag_tokens[0].value == 'ns:tag'

    def test_nested_tags(self):
        """Test nested tags on same line."""
        lexer = XMLLexer()
        lexer.lex(None, '<outer><inner></inner></outer>')

        tokens = list(lexer._tokens)
        tag_tokens = [t for t in tokens if t.type == TokenType.HTML_TAG]
        assert len(tag_tokens) == 4
        assert tag_tokens[0].value == 'outer'
        assert tag_tokens[1].value == 'inner'
        assert tag_tokens[2].value == '/inner'
        assert tag_tokens[3].value == '/outer'

    def test_tag_with_text_content(self):
        """Test tag with text content."""
        lexer = XMLLexer()
        lexer.lex(None, '<tag>content</tag>')

        tokens = list(lexer._tokens)
        text_tokens = [t for t in tokens if t.type == TokenType.TEXT]
        assert len(text_tokens) == 1
        assert text_tokens[0].value == 'content'

    def test_tag_with_whitespace_content(self):
        """Test tag with whitespace content."""
        lexer = XMLLexer()
        lexer.lex(None, '<tag>  content  </tag>')

        tokens = list(lexer._tokens)
        text_tokens = [t for t in tokens if t.type == TokenType.TEXT]
        assert len(text_tokens) == 1
        assert text_tokens[0].value == '  content  '

    def test_empty_tag(self):
        """Test empty tag with no content."""
        lexer = XMLLexer()
        lexer.lex(None, '<tag></tag>')

        tokens = list(lexer._tokens)
        tag_tokens = [t for t in tokens if t.type == TokenType.HTML_TAG]
        assert len(tag_tokens) == 2
        text_tokens = [t for t in tokens if t.type == TokenType.TEXT]
        assert len(text_tokens) == 0

    def test_tag_case_sensitivity(self):
        """Test that tag names preserve case."""
        lexer = XMLLexer()
        lexer.lex(None, '<MyTag>')

        tokens = list(lexer._tokens)
        tag_tokens = [t for t in tokens if t.type == TokenType.HTML_TAG]
        assert len(tag_tokens) == 1
        assert tag_tokens[0].value == 'MyTag'

    def test_multiple_namespace_levels(self):
        """Test tag with multiple namespace levels."""
        lexer = XMLLexer()
        lexer.lex(None, '<ns1:ns2:tag>')

        tokens = list(lexer._tokens)
        tag_tokens = [t for t in tokens if t.type == TokenType.HTML_TAG]
        assert len(tag_tokens) == 1
        assert tag_tokens[0].value == 'ns1:ns2:tag'

    def test_tag_with_special_content(self):
        """Test tag with special characters in content."""
        lexer = XMLLexer()
        lexer.lex(None, '<tag>content with & and more</tag>')

        tokens = list(lexer._tokens)
        text_tokens = [t for t in tokens if t.type == TokenType.TEXT]
        assert len(text_tokens) == 1
        assert 'content with & and more' in text_tokens[0].value

    def test_unclosed_tag(self):
        """Test unclosed tag (just opening bracket and name)."""
        lexer = XMLLexer()
        state = lexer.lex(None, '<tag')

        tokens = list(lexer._tokens)
        assert state.in_tag, "Should be in tag state"
        assert len(tokens) == 2  # < and tag name

    def test_tag_with_newline_in_content(self):
        """Test that text content stops at tag boundary."""
        lexer = XMLLexer()
        lexer.lex(None, '<tag>text')

        tokens = list(lexer._tokens)
        text_tokens = [t for t in tokens if t.type == TokenType.TEXT]
        assert len(text_tokens) == 1
        assert text_tokens[0].value == 'text'
