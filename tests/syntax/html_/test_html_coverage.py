"""
Tests for HTML coverage gaps to achieve 100% coverage.
"""
import pytest

from syntax.html.html_lexer import HTMLLexer
from syntax.lexer import TokenType


class TestHTMLCoverageGaps:
    """Test HTML features that weren't covered by basic tests."""

    def test_doctype_declaration(self):
        """Test DOCTYPE declaration."""
        lexer = HTMLLexer()
        lexer.lex(None, '<!DOCTYPE html>')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.DOCTYPE
        assert 'DOCTYPE' in tokens[0].value

    def test_script_tag_with_content(self):
        """Test script tag with JavaScript content."""
        lexer = HTMLLexer()
        lexer.lex(None, '<script>var x = 10;</script>')

        tokens = list(lexer._tokens)
        script_tokens = [t for t in tokens if t.type == TokenType.SCRIPT]
        assert len(script_tokens) == 1
        assert 'var x = 10;' in script_tokens[0].value

    def test_script_tag_multiline_start(self):
        """Test script tag that starts and continues on next line."""
        lexer1 = HTMLLexer()
        state1 = lexer1.lex(None, '<script>')

        lexer2 = HTMLLexer()
        state2 = lexer2.lex(state1, 'var x = 10;')

        tokens = list(lexer2._tokens)
        script_tokens = [t for t in tokens if t.type == TokenType.SCRIPT]
        assert len(script_tokens) == 1

    def test_script_tag_without_closing(self):
        """Test script content that doesn't close on same line."""
        lexer = HTMLLexer()
        lexer.lex(None, '<script>var x = 10; var y = 20;')

        tokens = list(lexer._tokens)
        script_tokens = [t for t in tokens if t.type == TokenType.SCRIPT]
        assert len(script_tokens) == 1

    def test_style_tag_with_content(self):
        """Test style tag with CSS content."""
        lexer = HTMLLexer()
        lexer.lex(None, '<style>body { margin: 0; }</style>')

        tokens = list(lexer._tokens)
        style_tokens = [t for t in tokens if t.type == TokenType.STYLE]
        assert len(style_tokens) == 1
        assert 'margin' in style_tokens[0].value

    def test_style_tag_multiline_start(self):
        """Test style tag that starts and continues on next line."""
        lexer1 = HTMLLexer()
        state1 = lexer1.lex(None, '<style>')

        lexer2 = HTMLLexer()
        state2 = lexer2.lex(state1, 'body { margin: 0; }')

        tokens = list(lexer2._tokens)
        style_tokens = [t for t in tokens if t.type == TokenType.STYLE]
        assert len(style_tokens) == 1

    def test_style_tag_without_closing(self):
        """Test style content that doesn't close on same line."""
        lexer = HTMLLexer()
        lexer.lex(None, '<style>body { margin: 0; } p { padding: 10px; }')

        tokens = list(lexer._tokens)
        style_tokens = [t for t in tokens if t.type == TokenType.STYLE]
        assert len(style_tokens) == 1

    def test_multiline_comment_continuation(self):
        """Test comment that spans multiple lines."""
        lexer1 = HTMLLexer()
        state1 = lexer1.lex(None, '<!-- Start of comment')

        assert state1.in_comment, "Should be in comment state"

        lexer2 = HTMLLexer()
        state2 = lexer2.lex(state1, 'Middle of comment')

        tokens = list(lexer2._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT

    def test_multiline_comment_end(self):
        """Test ending a multiline comment."""
        lexer1 = HTMLLexer()
        state1 = lexer1.lex(None, '<!-- Start')

        lexer2 = HTMLLexer()
        state2 = lexer2.lex(state1, 'End -->')

        tokens = list(lexer2._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT
        assert not state2.in_comment

    def test_multiline_tag_continuation(self):
        """Test tag that continues across lines."""
        lexer1 = HTMLLexer()
        state1 = lexer1.lex(None, '<div class="test"')

        assert state1.in_tag, "Should be in tag state"
        assert state1.tag_name == 'div'

        lexer2 = HTMLLexer()
        state2 = lexer2.lex(state1, 'id="main">')

        tokens = list(lexer2._tokens)
        attr_tokens = [t for t in tokens if t.type == TokenType.HTML_ATTRIBUTE]
        assert len(attr_tokens) == 1
        assert attr_tokens[0].value == 'id'

    def test_multiline_script_continuation(self):
        """Test script block continuing from previous line."""
        lexer1 = HTMLLexer()
        state1 = lexer1.lex(None, '<script>')

        assert state1.in_script, "Should be in script state"

        lexer2 = HTMLLexer()
        state2 = lexer2.lex(state1, 'var x = 10;</script>')

        tokens = list(lexer2._tokens)
        script_tokens = [t for t in tokens if t.type == TokenType.SCRIPT]
        assert len(script_tokens) == 1
        assert not state2.in_script

    def test_multiline_style_continuation(self):
        """Test style block continuing from previous line."""
        lexer1 = HTMLLexer()
        state1 = lexer1.lex(None, '<style>')

        assert state1.in_style, "Should be in style state"

        lexer2 = HTMLLexer()
        state2 = lexer2.lex(state1, 'body { margin: 0; }</style>')

        tokens = list(lexer2._tokens)
        style_tokens = [t for t in tokens if t.type == TokenType.STYLE]
        assert len(style_tokens) == 1
        assert not state2.in_style

    def test_doctype_with_system(self):
        """Test DOCTYPE with SYSTEM identifier."""
        lexer = HTMLLexer()
        lexer.lex(None, '<!DOCTYPE html SYSTEM "about:legacy-compat">')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.DOCTYPE

    def test_doctype_xhtml(self):
        """Test XHTML DOCTYPE."""
        lexer = HTMLLexer()
        lexer.lex(None, '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.DOCTYPE


    def test_doctype_without_closing(self):
        """Test DOCTYPE that reaches end of line without closing >."""
        lexer = HTMLLexer()
        lexer.lex(None, '<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN"')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.DOCTYPE
        # Should consume everything to end of line even without >
