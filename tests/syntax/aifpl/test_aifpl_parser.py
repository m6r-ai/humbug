"""
Tests for AIFPL parser.
"""
import pytest

from syntax.aifpl.aifpl_parser import AIFPLParser, AIFPLParserState
from syntax.lexer import TokenType
from syntax.programming_language import ProgrammingLanguage


class TestAIFPLParser:
    """Test AIFPL parser."""

    def test_parse_simple_expression(self):
        """Test parsing a simple expression."""
        parser = AIFPLParser()
        state = parser.parse(None, '(+ 1 2)')

        assert isinstance(state, AIFPLParserState)
        assert state.parsing_continuation is False
        assert state.continuation_state == 0

    def test_parse_multiline_string_start(self):
        """Test parsing start of multiline string."""
        parser = AIFPLParser()
        state = parser.parse(None, '"hello')

        assert isinstance(state, AIFPLParserState)
        assert state.parsing_continuation is True
        assert state.continuation_state == 1
        assert state.lexer_state.in_string is True

    def test_parse_multiline_string_continuation(self):
        """Test parsing continuation of multiline string."""
        parser1 = AIFPLParser()
        state1 = parser1.parse(None, '"hello')

        parser2 = AIFPLParser()
        state2 = parser2.parse(state1, 'world"')

        assert isinstance(state2, AIFPLParserState)
        assert state2.parsing_continuation is False
        assert state2.continuation_state == 0
        assert state2.lexer_state.in_string is False

    def test_parse_multiline_string_with_escape(self):
        """Test parsing multiline string with escape sequence."""
        parser1 = AIFPLParser()
        state1 = parser1.parse(None, '"hello\\')

        parser2 = AIFPLParser()
        state2 = parser2.parse(state1, 'nworld"')

        assert state2.parsing_continuation is False

    def test_parse_empty_input(self):
        """Test parsing empty input."""
        parser = AIFPLParser()
        state = parser.parse(None, '')

        assert isinstance(state, AIFPLParserState)
        assert state.parsing_continuation is False

    def test_parse_comment(self):
        """Test parsing comment."""
        parser = AIFPLParser()
        state = parser.parse(None, '; this is a comment')

        tokens = list(parser._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT

    def test_parse_numbers(self):
        """Test parsing numbers."""
        parser = AIFPLParser()
        state = parser.parse(None, '42 3.14 #xFF')

        tokens = list(parser._tokens)
        number_tokens = [t for t in tokens if t.type == TokenType.NUMBER]
        assert len(number_tokens) == 3

    def test_parse_strings(self):
        """Test parsing strings."""
        parser = AIFPLParser()
        state = parser.parse(None, '"hello" "world"')

        tokens = list(parser._tokens)
        string_tokens = [t for t in tokens if t.type == TokenType.STRING]
        assert len(string_tokens) == 2

    def test_parse_identifiers(self):
        """Test parsing identifiers."""
        parser = AIFPLParser()
        state = parser.parse(None, 'foo bar baz')

        tokens = list(parser._tokens)
        ident_tokens = [t for t in tokens if t.type == TokenType.IDENTIFIER]
        assert len(ident_tokens) == 3

    def test_parse_keywords(self):
        """Test parsing keywords."""
        parser = AIFPLParser()
        state = parser.parse(None, '(lambda (x) x)')

        tokens = list(parser._tokens)
        keyword_tokens = [t for t in tokens if t.type == TokenType.KEYWORD]
        assert len(keyword_tokens) == 1
        assert keyword_tokens[0].value == 'lambda'

    def test_parse_booleans(self):
        """Test parsing booleans."""
        parser = AIFPLParser()
        state = parser.parse(None, '#t #f')

        tokens = list(parser._tokens)
        bool_tokens = [t for t in tokens if t.type == TokenType.BOOLEAN]
        assert len(bool_tokens) == 2

    def test_parse_delimiters(self):
        """Test parsing delimiters."""
        parser = AIFPLParser()
        state = parser.parse(None, '()')

        tokens = list(parser._tokens)
        assert len(tokens) == 2
        assert tokens[0].type == TokenType.LPAREN
        assert tokens[1].type == TokenType.RPAREN

    def test_parse_quote(self):
        """Test parsing quote."""
        parser = AIFPLParser()
        state = parser.parse(None, "'x")

        tokens = list(parser._tokens)
        assert tokens[0].type == TokenType.QUOTE

    def test_parse_complex_expression(self):
        """Test parsing complex expression."""
        parser = AIFPLParser()
        state = parser.parse(None, '(let ((x 5)) (* x x))')

        tokens = list(parser._tokens)
        assert len(tokens) > 0

    def test_parser_state_type_assertion(self):
        """Test that parser asserts correct state type."""
        parser1 = AIFPLParser()
        state1 = parser1.parse(None, '(+ 1 2)')

        # Create a parser with the correct state type
        parser2 = AIFPLParser()
        state2 = parser2.parse(state1, '(* 3 4)')

        assert isinstance(state2, AIFPLParserState)

    def test_get_next_token(self):
        """Test getting tokens from parser."""
        parser = AIFPLParser()
        parser.parse(None, '(+ 1 2)')

        # Tokens should be accessible
        tokens = list(parser._tokens)
        assert len(tokens) == 5  # ( + 1 2 )

    def test_parse_with_all_token_types(self):
        """Test parsing with all token types in one line."""
        parser = AIFPLParser()
        state = parser.parse(None, '(lambda (x) (if #t 42 "no")) ; comment')

        tokens = list(parser._tokens)

        # Check we have various token types
        has_lparen = any(t.type == TokenType.LPAREN for t in tokens)
        has_rparen = any(t.type == TokenType.RPAREN for t in tokens)
        has_keyword = any(t.type == TokenType.KEYWORD for t in tokens)
        has_identifier = any(t.type == TokenType.IDENTIFIER for t in tokens)
        has_boolean = any(t.type == TokenType.BOOLEAN for t in tokens)
        has_number = any(t.type == TokenType.NUMBER for t in tokens)
        has_string = any(t.type == TokenType.STRING for t in tokens)
        has_comment = any(t.type == TokenType.COMMENT for t in tokens)

        assert all([has_lparen, has_rparen, has_keyword, has_identifier, 
                   has_boolean, has_number, has_string, has_comment])

    def test_multiline_string_multiple_continuations(self):
        """Test multiline string across multiple lines."""
        parser1 = AIFPLParser()
        state1 = parser1.parse(None, '"line1')
        assert state1.parsing_continuation is True

        parser2 = AIFPLParser()
        state2 = parser2.parse(state1, 'line2')
        assert state2.parsing_continuation is True

        parser3 = AIFPLParser()
        state3 = parser3.parse(state2, 'line3"')
        assert state3.parsing_continuation is False

    def test_parse_preserves_lexer_state(self):
        """Test that parser preserves lexer state."""
        parser1 = AIFPLParser()
        state1 = parser1.parse(None, '"unclosed')

        assert state1.lexer_state is not None
        assert state1.lexer_state.in_string is True

        parser2 = AIFPLParser()
        state2 = parser2.parse(state1, 'closed"')

        assert state2.lexer_state is not None
        assert state2.lexer_state.in_string is False

    def test_parse_multiple_lines_independently(self):
        """Test parsing multiple independent lines."""
        lines = [
            '(+ 1 2)',
            '(* 3 4)',
            '(- 5 6)'
        ]

        for line in lines:
            parser = AIFPLParser()
            state = parser.parse(None, line)
            assert state.parsing_continuation is False

    def test_parse_whitespace_only(self):
        """Test parsing whitespace only."""
        parser = AIFPLParser()
        state = parser.parse(None, '   \t  ')

        tokens = list(parser._tokens)
        assert len(tokens) == 0

    def test_parse_mixed_content(self):
        """Test parsing mixed content."""
        parser = AIFPLParser()
        state = parser.parse(None, '  (+ 1 2)  ; add  ')

        tokens = list(parser._tokens)
        # Should have tokens for expression and comment
        assert len(tokens) > 0
