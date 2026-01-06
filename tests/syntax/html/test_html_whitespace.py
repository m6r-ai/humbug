"""
Tests for HTML whitespace handling.
"""
import pytest

from syntax.html.html_lexer import HTMLLexer
from syntax.lexer import TokenType


class TestHTMLWhitespace:
    """Test HTML whitespace tokenization and preservation."""

    def test_whitespace_in_text_content(self):
        """Test that whitespace in text content is preserved."""
        lexer = HTMLLexer()
        lexer.lex(None, '<p>  content  </p>')

        tokens = list(lexer._tokens)
        text_tokens = [t for t in tokens if t.type == TokenType.TEXT]
        assert len(text_tokens) == 1
        assert text_tokens[0].value == '  content  '

    def test_leading_whitespace_preserved(self):
        """Test that leading whitespace in text is preserved."""
        lexer = HTMLLexer()
        lexer.lex(None, '<div>   text</div>')

        tokens = list(lexer._tokens)
        text_tokens = [t for t in tokens if t.type == TokenType.TEXT]
        assert len(text_tokens) == 1
        assert text_tokens[0].value == '   text'

    def test_trailing_whitespace_preserved(self):
        """Test that trailing whitespace in text is preserved."""
        lexer = HTMLLexer()
        lexer.lex(None, '<div>text   </div>')

        tokens = list(lexer._tokens)
        text_tokens = [t for t in tokens if t.type == TokenType.TEXT]
        assert len(text_tokens) == 1
        assert text_tokens[0].value == 'text   '

    def test_whitespace_between_attributes_skipped(self):
        """Test that whitespace between attributes is properly skipped."""
        lexer = HTMLLexer()
        lexer.lex(None, '<div   class="test"   id="main"   >')

        tokens = list(lexer._tokens)
        attr_tokens = [t for t in tokens if t.type == TokenType.HTML_ATTRIBUTE]
        assert len(attr_tokens) == 2
        assert attr_tokens[0].value == 'class'
        assert attr_tokens[1].value == 'id'

    def test_newlines_in_text_preserved(self):
        """Test that newlines in text content are preserved."""
        lexer = HTMLLexer()
        lexer.lex(None, '<pre>line1\nline2</pre>')

        tokens = list(lexer._tokens)
        text_tokens = [t for t in tokens if t.type == TokenType.TEXT]
        assert len(text_tokens) == 1
        assert '\n' in text_tokens[0].value

    def test_tabs_in_text_preserved(self):
        """Test that tabs in text content are preserved."""
        lexer = HTMLLexer()
        lexer.lex(None, '<code>\ttabbed</code>')

        tokens = list(lexer._tokens)
        text_tokens = [t for t in tokens if t.type == TokenType.TEXT]
        assert len(text_tokens) == 1
        assert '\t' in text_tokens[0].value

    def test_multiple_spaces_preserved(self):
        """Test that multiple consecutive spaces are preserved."""
        lexer = HTMLLexer()
        lexer.lex(None, '<span>word     word</span>')

        tokens = list(lexer._tokens)
        text_tokens = [t for t in tokens if t.type == TokenType.TEXT]
        assert len(text_tokens) == 1
        assert 'word     word' in text_tokens[0].value

    def test_whitespace_only_text(self):
        """Test text content that is only whitespace."""
        lexer = HTMLLexer()
        lexer.lex(None, '<div>     </div>')

        tokens = list(lexer._tokens)
        text_tokens = [t for t in tokens if t.type == TokenType.TEXT]
        assert len(text_tokens) == 1
        assert text_tokens[0].value == '     '

    def test_whitespace_after_tag_name(self):
        """Test whitespace after tag name before attributes."""
        lexer = HTMLLexer()
        lexer.lex(None, '<div     class="test">')

        tokens = list(lexer._tokens)
        tag_tokens = [t for t in tokens if t.type == TokenType.HTML_TAG]
        assert len(tag_tokens) == 1
        assert tag_tokens[0].value == 'div'

    def test_no_whitespace_between_tags(self):
        """Test tags with no whitespace between them."""
        lexer = HTMLLexer()
        lexer.lex(None, '<span>text</span><span>more</span>')

        tokens = list(lexer._tokens)
        text_tokens = [t for t in tokens if t.type == TokenType.TEXT]
        assert len(text_tokens) == 2
        assert text_tokens[0].value == 'text'
        assert text_tokens[1].value == 'more'
