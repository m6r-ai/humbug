"""
Tests for HTML parser edge cases to achieve 100% coverage.
"""
import pytest
from unittest.mock import patch

from syntax.html.html_parser import HTMLParser, HTMLParserState
from syntax import ParserRegistry, ProgrammingLanguage


class TestHTMLParserEdgeCases:
    """Test HTML parser edge cases for complete coverage."""

    def test_embedded_parser_not_available_for_script(self):
        """Test when JavaScript parser is not available."""
        # Mock ParserRegistry.create_parser to return None for JavaScript
        with patch.object(ParserRegistry, 'create_parser', return_value=None):
            parser = HTMLParser()
            state = parser.parse(None, '<script>var x = 1;</script>')

            assert isinstance(state, HTMLParserState)
            # When embedded parser is None, embedded_parser_state should be None
            assert state.embedded_parser_state is None

    def test_embedded_parser_not_available_for_style(self):
        """Test when CSS parser is not available."""
        # Mock ParserRegistry.create_parser to return None for CSS
        with patch.object(ParserRegistry, 'create_parser', return_value=None):
            parser = HTMLParser()
            state = parser.parse(None, '<style>body { margin: 0; }</style>')

            assert isinstance(state, HTMLParserState)
            # When embedded parser is None, embedded_parser_state should be None
            assert state.embedded_parser_state is None

    def test_embedded_parser_returns_none_state_for_script(self):
        """Test when JavaScript parser returns None state."""
        # Create a mock parser that returns None
        class MockParser:
            def parse(self, prev_state, input_str):
                return None
            def get_next_token(self):
                return None

        with patch.object(ParserRegistry, 'create_parser', return_value=MockParser()):
            parser = HTMLParser()
            state = parser.parse(None, '<script>var x = 1;</script>')

            assert isinstance(state, HTMLParserState)
            # When embedded parser returns None state, continuation should still work
            # The embedded_parser_state will be None

    def test_embedded_parser_returns_none_state_for_style(self):
        """Test when CSS parser returns None state."""
        # Create a mock parser that returns None
        class MockParser:
            def parse(self, prev_state, input_str):
                return None
            def get_next_token(self):
                return None

        with patch.object(ParserRegistry, 'create_parser', return_value=MockParser()):
            parser = HTMLParser()
            state = parser.parse(None, '<style>body { margin: 0; }</style>')

            assert isinstance(state, HTMLParserState)
            # When embedded parser returns None state, continuation should still work

    def test_script_with_no_embedded_parser_multiline(self):
        """Test script tag spanning multiple lines when no parser available."""
        with patch.object(ParserRegistry, 'create_parser', return_value=None):
            parser1 = HTMLParser()
            state1 = parser1.parse(None, '<script>')

            parser2 = HTMLParser()
            state2 = parser2.parse(state1, 'var x = 1;')

            parser3 = HTMLParser()
            state3 = parser3.parse(state2, '</script>')

            assert isinstance(state3, HTMLParserState)

    def test_style_with_no_embedded_parser_multiline(self):
        """Test style tag spanning multiple lines when no parser available."""
        with patch.object(ParserRegistry, 'create_parser', return_value=None):
            parser1 = HTMLParser()
            state1 = parser1.parse(None, '<style>')

            parser2 = HTMLParser()
            state2 = parser2.parse(state1, 'body { margin: 0; }')

            parser3 = HTMLParser()
            state3 = parser3.parse(state2, '</style>')

            assert isinstance(state3, HTMLParserState)
