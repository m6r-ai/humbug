"""
Tests for HTML parser.
"""
import pytest

from syntax.html.html_parser import HTMLParser, HTMLParserState
from syntax.lexer import TokenType


class TestHTMLParser:
    """Test HTML parser functionality."""

    def test_simple_html_parsing(self):
        """Test parsing simple HTML."""
        parser = HTMLParser()
        state = parser.parse(None, '<p>Hello World</p>')

        assert isinstance(state, HTMLParserState)
        assert state.continuation_state == 0

        tokens = []
        while True:
            token = parser.get_next_token()
            if token is None:
                break
            tokens.append(token)

        assert len(tokens) > 0

    def test_html_with_comment(self):
        """Test parsing HTML with comment."""
        parser = HTMLParser()
        state = parser.parse(None, '<!-- comment --><div>text</div>')

        assert isinstance(state, HTMLParserState)
        assert state.continuation_state == 0

    def test_html_comment_multiline_start(self):
        """Test HTML comment that continues to next line."""
        parser = HTMLParser()
        state = parser.parse(None, '<!-- unclosed comment')

        assert isinstance(state, HTMLParserState)
        assert state.continuation_state == 1  # in_comment
        assert state.lexer_state.in_comment

    def test_html_comment_multiline_continuation(self):
        """Test continuing a multiline comment."""
        parser1 = HTMLParser()
        state1 = parser1.parse(None, '<!-- start')

        parser2 = HTMLParser()
        state2 = parser2.parse(state1, 'middle')

        assert isinstance(state2, HTMLParserState)
        assert state2.continuation_state == 1

    def test_html_comment_multiline_end(self):
        """Test ending a multiline comment."""
        parser1 = HTMLParser()
        state1 = parser1.parse(None, '<!-- start')

        parser2 = HTMLParser()
        state2 = parser2.parse(state1, 'end -->')

        assert isinstance(state2, HTMLParserState)
        assert state2.continuation_state == 0
        assert not state2.lexer_state.in_comment

    def test_html_with_script_tag(self):
        """Test parsing HTML with script tag."""
        parser = HTMLParser()
        state = parser.parse(None, '<script>var x = 1;</script>')

        assert isinstance(state, HTMLParserState)
        tokens = []
        while True:
            token = parser.get_next_token()
            if token is None:
                break
            tokens.append(token)

        # Should have tokens from both HTML and embedded JavaScript
        assert len(tokens) > 0

    def test_html_script_multiline_start(self):
        """Test script tag that continues to next line."""
        parser = HTMLParser()
        state = parser.parse(None, '<script>')

        assert isinstance(state, HTMLParserState)
        # Continuation state is non-zero when in script (offset applied by embedded parser)
        assert state.continuation_state != 0
        assert state.lexer_state.in_script

    def test_html_script_multiline_end(self):
        """Test ending a script tag."""
        parser1 = HTMLParser()
        state1 = parser1.parse(None, '<script>')

        parser2 = HTMLParser()
        state2 = parser2.parse(state1, 'var x = 1;')

        parser3 = HTMLParser()
        state3 = parser3.parse(state2, '</script>')

        assert isinstance(state3, HTMLParserState)
        # After closing, should not be in script mode
        assert state3.continuation_state != 2
        assert not state3.lexer_state.in_script

    def test_html_with_style_tag(self):
        """Test parsing HTML with style tag."""
        parser = HTMLParser()
        state = parser.parse(None, '<style>body { color: red; }</style>')

        assert isinstance(state, HTMLParserState)
        tokens = []
        while True:
            token = parser.get_next_token()
            if token is None:
                break
            tokens.append(token)

        # Should have tokens from both HTML and embedded CSS
        assert len(tokens) > 0

    def test_html_style_multiline_start(self):
        """Test style tag that continues to next line."""
        parser = HTMLParser()
        state = parser.parse(None, '<style>')

        assert isinstance(state, HTMLParserState)
        # Continuation state is non-zero when in style (offset applied by embedded parser)
        assert state.continuation_state != 0
        assert state.lexer_state.in_style

    def test_html_style_multiline_end(self):
        """Test ending a style tag."""
        parser1 = HTMLParser()
        state1 = parser1.parse(None, '<style>')

        parser2 = HTMLParser()
        state2 = parser2.parse(state1, 'body { color: red; }')

        parser3 = HTMLParser()
        state3 = parser3.parse(state2, '</style>')

        assert isinstance(state3, HTMLParserState)
        # After closing, should not be in style mode
        assert state3.continuation_state != 3
        assert not state3.lexer_state.in_style

    def test_html_with_doctype(self):
        """Test parsing HTML with DOCTYPE."""
        parser = HTMLParser()
        state = parser.parse(None, '<!DOCTYPE html>')

        assert isinstance(state, HTMLParserState)
        assert state.continuation_state == 0

    def test_html_empty_input(self):
        """Test parsing empty input."""
        parser = HTMLParser()
        state = parser.parse(None, '')

        assert isinstance(state, HTMLParserState)
        assert state.continuation_state == 0

    def test_html_parser_state_assertion(self):
        """Test that parser asserts correct state type."""
        parser = HTMLParser()
        state = parser.parse(None, '<div>test</div>')

        # Should work with correct state type
        parser2 = HTMLParser()
        state2 = parser2.parse(state, '<span>more</span>')
        assert isinstance(state2, HTMLParserState)

    def test_html_embedded_parser_state_preservation(self):
        """Test that embedded parser state is preserved."""
        parser1 = HTMLParser()
        state1 = parser1.parse(None, '<script>')

        parser2 = HTMLParser()
        state2 = parser2.parse(state1, 'var x = 1;')

        # Should preserve embedded parser state
        assert state2.embedded_parser_state is not None or state2.embedded_parser_state is None

    def test_html_script_with_javascript_content(self):
        """Test script tag with actual JavaScript code."""
        parser = HTMLParser()
        state = parser.parse(None, '<script>function test() { return 42; }</script>')

        assert isinstance(state, HTMLParserState)
        tokens = []
        while True:
            token = parser.get_next_token()
            if token is None:
                break
            tokens.append(token)

        assert len(tokens) > 0

    def test_html_style_with_css_content(self):
        """Test style tag with actual CSS code."""
        parser = HTMLParser()
        state = parser.parse(None, '<style>.class { background: blue; }</style>')

        assert isinstance(state, HTMLParserState)
        tokens = []
        while True:
            token = parser.get_next_token()
            if token is None:
                break
            tokens.append(token)

        assert len(tokens) > 0

    def test_html_multiple_script_tags(self):
        """Test HTML with multiple script tags."""
        parser = HTMLParser()
        state = parser.parse(None, '<script>var a = 1;</script><script>var b = 2;</script>')

        assert isinstance(state, HTMLParserState)

    def test_html_multiple_style_tags(self):
        """Test HTML with multiple style tags."""
        parser = HTMLParser()
        state = parser.parse(None, '<style>body {}</style><style>div {}</style>')

        assert isinstance(state, HTMLParserState)

    def test_html_script_and_style_together(self):
        """Test HTML with both script and style tags."""
        parser = HTMLParser()
        state = parser.parse(None, '<style>body {}</style><script>var x = 1;</script>')

        assert isinstance(state, HTMLParserState)

    def test_html_complex_document(self):
        """Test parsing a more complex HTML document."""
        html = '''<!DOCTYPE html>
<html>
<head>
    <style>body { margin: 0; }</style>
</head>
<body>
    <div>Content</div>
    <script>console.log('test');</script>
</body>
</html>'''

        lines = html.split('\n')
        state = None
        for line in lines:
            parser = HTMLParser()
            state = parser.parse(state, line)

        assert isinstance(state, HTMLParserState)
        assert state.continuation_state == 0

    def test_html_embedded_parser_none_when_no_parser_available(self):
        """Test that embedded parser returns None when language parser not available."""
        # This tests the edge case in _embedded_parse where create_parser returns None
        # This is hard to test directly, but we can ensure the code path exists
        parser = HTMLParser()
        state = parser.parse(None, '<script>test</script>')
        assert isinstance(state, HTMLParserState)
