"""
Tests for HTML Unicode support.
"""
import pytest

from syntax.html.html_lexer import HTMLLexer
from syntax.lexer import TokenType


class TestHTMLUnicode:
    """Test HTML Unicode character handling."""

    def test_unicode_in_text_content(self):
        """Test Unicode characters in text content."""
        lexer = HTMLLexer()
        lexer.lex(None, '<p>Hello 世界 🌍</p>')

        tokens = list(lexer._tokens)
        text_tokens = [t for t in tokens if t.type == TokenType.TEXT]
        assert len(text_tokens) == 1
        assert '世界' in text_tokens[0].value
        assert '🌍' in text_tokens[0].value

    def test_unicode_in_attribute_value(self):
        """Test Unicode characters in attribute values."""
        lexer = HTMLLexer()
        lexer.lex(None, '<div title="Hello 世界">content</div>')

        tokens = list(lexer._tokens)
        string_tokens = [t for t in tokens if t.type == TokenType.STRING]
        assert len(string_tokens) >= 1
        # Find the title attribute value
        title_value = None
        for token in string_tokens:
            if '世界' in token.value:
                title_value = token.value
                break
        assert title_value is not None

    def test_unicode_tag_names(self):
        """Test that Unicode characters work in tag names (even if not standard HTML)."""
        lexer = HTMLLexer()
        lexer.lex(None, '<div_中文>')

        tokens = list(lexer._tokens)
        tag_tokens = [t for t in tokens if t.type == TokenType.HTML_TAG]
        assert len(tag_tokens) == 1
        assert '中文' in tag_tokens[0].value

    def test_unicode_attribute_names(self):
        """Test Unicode characters in attribute names."""
        lexer = HTMLLexer()
        lexer.lex(None, '<div attr_中文="value">')

        tokens = list(lexer._tokens)
        attr_tokens = [t for t in tokens if t.type == TokenType.HTML_ATTRIBUTE]
        assert len(attr_tokens) == 1
        assert '中文' in attr_tokens[0].value

    def test_emoji_in_content(self):
        """Test emoji in text content."""
        lexer = HTMLLexer()
        lexer.lex(None, '<p>😀 😃 😄 😁</p>')

        tokens = list(lexer._tokens)
        text_tokens = [t for t in tokens if t.type == TokenType.TEXT]
        assert len(text_tokens) == 1
        assert '😀' in text_tokens[0].value

    def test_mixed_scripts(self):
        """Test mixed writing systems."""
        lexer = HTMLLexer()
        lexer.lex(None, '<p>English العربية 中文 日本語</p>')

        tokens = list(lexer._tokens)
        text_tokens = [t for t in tokens if t.type == TokenType.TEXT]
        assert len(text_tokens) == 1
        assert 'العربية' in text_tokens[0].value
        assert '日本語' in text_tokens[0].value

    def test_unicode_in_comments(self):
        """Test Unicode in HTML comments."""
        lexer = HTMLLexer()
        lexer.lex(None, '<!-- Comment with 中文 -->')

        tokens = list(lexer._tokens)
        comment_tokens = [t for t in tokens if t.type == TokenType.COMMENT]
        assert len(comment_tokens) == 1
        assert '中文' in comment_tokens[0].value

    def test_rtl_text(self):
        """Test right-to-left text."""
        lexer = HTMLLexer()
        lexer.lex(None, '<p dir="rtl">مرحبا</p>')

        tokens = list(lexer._tokens)
        text_tokens = [t for t in tokens if t.type == TokenType.TEXT]
        assert len(text_tokens) == 1
        assert 'مرحبا' in text_tokens[0].value

    def test_combining_characters(self):
        """Test combining diacritical marks."""
        lexer = HTMLLexer()
        lexer.lex(None, '<p>café naïve</p>')

        tokens = list(lexer._tokens)
        text_tokens = [t for t in tokens if t.type == TokenType.TEXT]
        assert len(text_tokens) == 1
        assert 'café' in text_tokens[0].value

    def test_zero_width_characters(self):
        """Test zero-width characters (if present)."""
        lexer = HTMLLexer()
        # Zero-width space
        lexer.lex(None, '<p>word\u200bword</p>')

        tokens = list(lexer._tokens)
        text_tokens = [t for t in tokens if t.type == TokenType.TEXT]
        assert len(text_tokens) == 1
