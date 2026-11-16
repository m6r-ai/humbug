"""
Tests for markdown inline code syntax highlighting, in particular spans that
run across a line break.
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


def _parse_lines(lines: list[str]):
    """Helper: parse lines incrementally and return (list_of_token_lists, state)."""
    parser = MarkdownParser()
    state = None
    all_tokens = []
    for line in lines:
        state = parser.parse(state, line)
        tokens = []
        while True:
            token = parser.get_next_token()
            if token is None:
                break
            tokens.append(token)
        all_tokens.append(tokens)
    return all_tokens, state


def _types(tokens):
    """Return the token types for a list of tokens."""
    return [t.type for t in tokens]


def test_simple_inline_code():
    """`code` produces INLINE_CODE_START, INLINE_CODE and INLINE_CODE_END."""
    tokens, state = _parse_line("a `b` c")

    types = _types(tokens)
    assert TokenType.INLINE_CODE_START in types
    assert TokenType.INLINE_CODE in types
    assert TokenType.INLINE_CODE_END in types
    assert state.inline_formatting_stack == []


def test_unclosed_inline_code_opens_span():
    """An unclosed backtick opens an inline code span that continues to the next line."""
    tokens, state = _parse_line("text `code starts")

    types = _types(tokens)
    assert TokenType.INLINE_CODE_START in types
    assert TokenType.INLINE_CODE in types
    assert TokenType.INLINE_CODE_END not in types

    content = next(t for t in tokens if t.type == TokenType.INLINE_CODE)
    assert content.value == "code starts"
    assert state.inline_formatting_stack == ["`"]


def test_inline_code_closed_on_next_line():
    """A span opened on one line is closed by a backtick on the next line."""
    all_tokens, state = _parse_lines(["text `code", "continues` after"])

    assert TokenType.INLINE_CODE_START in _types(all_tokens[0])
    assert TokenType.INLINE_CODE_END in _types(all_tokens[1])
    assert state.inline_formatting_stack == []

    continued = next(t for t in all_tokens[1] if t.type == TokenType.INLINE_CODE)
    assert continued.value == "continues"


def test_text_after_closing_backtick_is_parsed_normally():
    """Text following the closing backtick is processed for inline formatting."""
    all_tokens, _ = _parse_lines(["`one", "two` and `three`"])

    second_line = all_tokens[1]
    assert TokenType.INLINE_CODE_END in _types(second_line)

    code_values = [t.value for t in second_line if t.type == TokenType.INLINE_CODE]
    assert code_values == ["two", "three"]


def test_inline_code_across_lines_in_list_item():
    """A code span broken across a line break inside a list item stays styled."""
    lines = [
        "- A type guard now names the type it required, e.g. `type mismatch: expects a",
        "  bytes value`, rather than a bare `type mismatch`.",
    ]
    all_tokens, state = _parse_lines(lines)

    assert TokenType.INLINE_CODE_START in _types(all_tokens[0])
    assert TokenType.INLINE_CODE in _types(all_tokens[0])

    second_line = all_tokens[1]
    assert TokenType.LIST_MARKER in _types(second_line)
    assert TokenType.INLINE_CODE_END in _types(second_line)

    code_values = [t.value for t in second_line if t.type == TokenType.INLINE_CODE]
    assert code_values == ["bytes value", "type mismatch"]
    assert state.inline_formatting_stack == []


def test_inline_code_span_does_not_leak_onto_next_paragraph():
    """A blank line ends an open inline code span."""
    _, state = _parse_lines(["text `unclosed", ""])

    assert state.inline_formatting_stack == []


def test_inline_code_positions_across_lines():
    """Token start positions are relative to each line."""
    all_tokens, _ = _parse_lines(["ab `cd", "ef` gh"])

    start = next(t for t in all_tokens[0] if t.type == TokenType.INLINE_CODE_START)
    content = next(t for t in all_tokens[1] if t.type == TokenType.INLINE_CODE)
    end = next(t for t in all_tokens[1] if t.type == TokenType.INLINE_CODE_END)

    assert start.start == 3
    assert content.start == 0
    assert end.start == 2


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
