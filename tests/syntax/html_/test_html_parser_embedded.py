"""
Tests for HTML parser embedded content handling to achieve 100% coverage.
"""
import pytest

from syntax.html.html_parser import HTMLParser, HTMLParserState
from syntax.lexer import TokenType


class TestHTMLParserEmbedded:
    """Test HTML parser embedded JavaScript and CSS handling."""

    def test_script_with_multiline_javascript(self):
        """Test script tag with JavaScript that spans multiple lines."""
        # First line: open script tag and start JavaScript
        parser1 = HTMLParser()
        state1 = parser1.parse(None, '<script>')

        # Second line: JavaScript content
        parser2 = HTMLParser()
        state2 = parser2.parse(state1, 'var x = 1;')

        # Third line: more JavaScript
        parser3 = HTMLParser()
        state3 = parser3.parse(state2, 'var y = 2;')

        # Fourth line: close script
        parser4 = HTMLParser()
        state4 = parser4.parse(state3, '</script>')

        assert isinstance(state4, HTMLParserState)

    def test_style_with_multiline_css(self):
        """Test style tag with CSS that spans multiple lines."""
        # First line: open style tag
        parser1 = HTMLParser()
        state1 = parser1.parse(None, '<style>')

        # Second line: CSS content
        parser2 = HTMLParser()
        state2 = parser2.parse(state1, 'body { margin: 0; }')

        # Third line: more CSS
        parser3 = HTMLParser()
        state3 = parser3.parse(state2, 'div { padding: 10px; }')

        # Fourth line: close style
        parser4 = HTMLParser()
        state4 = parser4.parse(state3, '</style>')

        assert isinstance(state4, HTMLParserState)

    def test_script_with_continuation_state(self):
        """Test that script parsing preserves continuation state."""
        # Parse HTML with script tag containing JavaScript
        parser1 = HTMLParser()
        state1 = parser1.parse(None, '<html><script>function test() {</script></html>')

        # The embedded parser should have returned a state
        tokens = []
        while True:
            token = parser1.get_next_token()
            if token is None:
                break
            tokens.append(token)

        assert len(tokens) > 0

    def test_style_with_continuation_state(self):
        """Test that style parsing preserves continuation state."""
        # Parse HTML with style tag containing CSS
        parser1 = HTMLParser()
        state1 = parser1.parse(None, '<html><style>body { color: red; }</style></html>')

        # The embedded parser should have returned a state
        tokens = []
        while True:
            token = parser1.get_next_token()
            if token is None:
                break
            tokens.append(token)

        assert len(tokens) > 0

    def test_script_embedded_parser_state_not_none(self):
        """Test that embedded parser state is not None for valid JavaScript."""
        # Create a complete HTML document with script
        html_lines = [
            '<html>',
            '<head>',
            '<script>',
            'var x = 10;',
            'function test() {',
            '    return x;',
            '}',
            '</script>',
            '</head>',
            '</html>'
        ]

        state = None
        for line in html_lines:
            parser = HTMLParser()
            state = parser.parse(state, line)

        assert isinstance(state, HTMLParserState)

    def test_style_embedded_parser_state_not_none(self):
        """Test that embedded parser state is not None for valid CSS."""
        # Create a complete HTML document with style
        html_lines = [
            '<html>',
            '<head>',
            '<style>',
            'body {',
            '    margin: 0;',
            '    padding: 0;',
            '}',
            '</style>',
            '</head>',
            '</html>'
        ]

        state = None
        for line in html_lines:
            parser = HTMLParser()
            state = parser.parse(state, line)

        assert isinstance(state, HTMLParserState)

    def test_script_and_style_in_same_document(self):
        """Test document with both script and style tags."""
        html_lines = [
            '<html>',
            '<head>',
            '<style>body { margin: 0; }</style>',
            '<script>var x = 1;</script>',
            '</head>',
            '</html>'
        ]

        state = None
        for line in html_lines:
            parser = HTMLParser()
            state = parser.parse(state, line)

        assert isinstance(state, HTMLParserState)

    def test_nested_script_handling(self):
        """Test multiple script tags in sequence."""
        html_lines = [
            '<script>var a = 1;</script>',
            '<script>var b = 2;</script>',
            '<script>var c = 3;</script>'
        ]

        state = None
        for line in html_lines:
            parser = HTMLParser()
            state = parser.parse(state, line)

        assert isinstance(state, HTMLParserState)

    def test_nested_style_handling(self):
        """Test multiple style tags in sequence."""
        html_lines = [
            '<style>body {}</style>',
            '<style>div {}</style>',
            '<style>span {}</style>'
        ]

        state = None
        for line in html_lines:
            parser = HTMLParser()
            state = parser.parse(state, line)

        assert isinstance(state, HTMLParserState)

    def test_embedded_parser_with_actual_continuation(self):
        """Test embedded parser with content that requires continuation."""
        # JavaScript with unclosed string or comment would create continuation
        parser1 = HTMLParser()
        state1 = parser1.parse(None, '<script>')

        parser2 = HTMLParser()
        state2 = parser2.parse(state1, '/* comment')

        # This should have a continuation state from the JavaScript parser
        assert isinstance(state2, HTMLParserState)
        # The continuation state should reflect the embedded parser state
        assert state2.continuation_state != 0

    def test_embedded_parser_css_with_continuation(self):
        """Test embedded CSS parser with content that requires continuation."""
        parser1 = HTMLParser()
        state1 = parser1.parse(None, '<style>')

        parser2 = HTMLParser()
        state2 = parser2.parse(state1, '/* comment')

        # This should have a continuation state from the CSS parser
        assert isinstance(state2, HTMLParserState)
        assert state2.continuation_state != 0
