"""
Fixed tests for HTML parser with correct continuation state values.
"""
import pytest

from syntax.html.html_parser import HTMLParser, HTMLParserState


class TestHTMLParserFixed:
    """Test HTML parser with correct continuation state expectations."""

    def test_html_script_multiline_continuation_state(self):
        """Test script tag continuation state (offset applied)."""
        parser = HTMLParser()
        state = parser.parse(None, '<script>')

        assert isinstance(state, HTMLParserState)
        # Continuation state should be non-zero when in script
        assert state.continuation_state != 0
        assert state.lexer_state.in_script

    def test_html_style_multiline_continuation_state(self):
        """Test style tag continuation state (offset applied)."""
        parser = HTMLParser()
        state = parser.parse(None, '<style>')

        assert isinstance(state, HTMLParserState)
        # Continuation state should be non-zero when in style
        assert state.continuation_state != 0
        assert state.lexer_state.in_style

    def test_html_script_closes_properly(self):
        """Test that script tag closes and resets state."""
        parser1 = HTMLParser()
        state1 = parser1.parse(None, '<script>')

        parser2 = HTMLParser()
        state2 = parser2.parse(state1, 'var x = 1;')

        parser3 = HTMLParser()
        state3 = parser3.parse(state2, '</script>')

        assert isinstance(state3, HTMLParserState)
        # After closing script, should not be in script mode
        assert not state3.lexer_state.in_script

    def test_html_style_closes_properly(self):
        """Test that style tag closes and resets state."""
        parser1 = HTMLParser()
        state1 = parser1.parse(None, '<style>')

        parser2 = HTMLParser()
        state2 = parser2.parse(state1, 'body { margin: 0; }')

        parser3 = HTMLParser()
        state3 = parser3.parse(state2, '</style>')

        assert isinstance(state3, HTMLParserState)
        # After closing style, should not be in style mode
        assert not state3.lexer_state.in_style
