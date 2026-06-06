"""
Tests for markdown strikethrough syntax highlighting.
"""
import pytest
import sys
sys.path.insert(0, 'src')

import syntax.parser_imports
from syntax.markdown.markdown_parser import MarkdownParser
from syntax.lexer import TokenType


def _parse_line(line: str, parser_state=None):
    """Helper: parse a single line and return (tokens, new_state)."""
    parser = MarkdownParser()
    new_state = parser.parse(parser_state, line)
    tokens = []
    while True:
        token = parser.get_next_token()
        if token is None:
            break
        tokens.append(token)
    return tokens, new_state


def test_simple_strikethrough():
    """Basic ~~text~~ produces correct token sequence."""
    tokens, _ = _parse_line("~~hello~~")

    types = [t.type for t in tokens]
    values = [t.value for t in tokens]

    assert TokenType.STRIKETHROUGH_START in types
    assert TokenType.STRIKETHROUGH in types
    assert TokenType.STRIKETHROUGH_END in types

    assert values[types.index(TokenType.STRIKETHROUGH_START)] == '~~'
    assert values[types.index(TokenType.STRIKETHROUGH)] == 'hello'
    assert values[types.index(TokenType.STRIKETHROUGH_END)] == '~~'


def test_strikethrough_in_sentence():
    """Strikethrough embedded in surrounding plain text."""
    tokens, _ = _parse_line("before ~~struck~~ after")

    types = [t.type for t in tokens]
    values = [t.value for t in tokens]

    assert TokenType.STRIKETHROUGH_START in types
    assert TokenType.STRIKETHROUGH in types
    assert TokenType.STRIKETHROUGH_END in types
    assert values[types.index(TokenType.STRIKETHROUGH)] == 'struck'

    # Text before and after should be present
    text_values = [t.value for t in tokens if t.type == TokenType.TEXT]
    combined = ''.join(text_values)
    assert 'before' in combined
    assert 'after' in combined


def test_strikethrough_with_no_closing_marker():
    """Unclosed ~~ should not produce strikethrough tokens."""
    tokens, _ = _parse_line("~~no close")

    types = [t.type for t in tokens]
    assert TokenType.STRIKETHROUGH_START not in types
    assert TokenType.STRIKETHROUGH not in types
    assert TokenType.STRIKETHROUGH_END not in types


def test_strikethrough_in_heading():
    """Strikethrough inside a heading is highlighted correctly."""
    tokens, _ = _parse_line("## Title with ~~struck~~ word")

    types = [t.type for t in tokens]
    assert TokenType.STRIKETHROUGH_START in types
    assert TokenType.STRIKETHROUGH in types
    assert TokenType.STRIKETHROUGH_END in types


def test_strikethrough_in_blockquote():
    """Strikethrough inside a blockquote is highlighted correctly."""
    tokens, _ = _parse_line("> quoted ~~struck~~ text")

    types = [t.type for t in tokens]
    assert TokenType.STRIKETHROUGH_START in types
    assert TokenType.STRIKETHROUGH in types
    assert TokenType.STRIKETHROUGH_END in types


def test_strikethrough_with_nested_bold():
    """Strikethrough content may contain bold formatting."""
    tokens, _ = _parse_line("~~**bold struck**~~")

    types = [t.type for t in tokens]
    assert TokenType.STRIKETHROUGH_START in types
    assert TokenType.STRIKETHROUGH_END in types
    assert TokenType.BOLD_START in types
    assert TokenType.BOLD in types
    assert TokenType.BOLD_END in types


def test_strikethrough_markers_have_correct_positions():
    """Token start positions are correct."""
    tokens, _ = _parse_line("aa ~~bb~~ cc")

    strike_start = next(t for t in tokens if t.type == TokenType.STRIKETHROUGH_START)
    strike_content = next(t for t in tokens if t.type == TokenType.STRIKETHROUGH)
    strike_end = next(t for t in tokens if t.type == TokenType.STRIKETHROUGH_END)

    assert strike_start.start == 3   # position of first ~
    assert strike_content.start == 5  # position of 'b'
    assert strike_end.start == 7     # position of closing ~~


def test_multiple_strikethroughs_on_one_line():
    """Multiple strikethrough spans on a single line are all recognised."""
    tokens, _ = _parse_line("~~one~~ and ~~two~~")

    strike_contents = [t.value for t in tokens if t.type == TokenType.STRIKETHROUGH]
    assert strike_contents == ['one', 'two']

    strike_starts = [t for t in tokens if t.type == TokenType.STRIKETHROUGH_START]
    strike_ends = [t for t in tokens if t.type == TokenType.STRIKETHROUGH_END]
    assert len(strike_starts) == 2
    assert len(strike_ends) == 2


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
