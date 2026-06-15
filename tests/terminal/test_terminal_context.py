"""Tests for TerminalContext.get_buffer_content."""

from unittest.mock import MagicMock
import pytest

from terminal.terminal_state import TerminalState
from terminal_context.terminal_context import TerminalContext


def _make_context(rows: int = 24, cols: int = 80) -> TerminalContext:
    """Return a TerminalContext backed by a real TerminalState and a mock process."""
    state = TerminalState(rows, cols)
    state.set_default_colors(0xFFFFFF, 0x000000)
    state.set_ansi_colors({i: i * 0x111111 for i in range(16)})
    process = MagicMock()
    return TerminalContext("test-id", process, state)


def _write(context: TerminalContext, text: str) -> None:
    """Feed raw bytes into the terminal state."""
    context._state.put_data(text.encode())  # pylint: disable=protected-access


class TestGetBufferContentBoundary:
    """get_buffer_content excludes unvisited blank rows below the cursor."""

    def test_read_all_excludes_blank_rows_below_cursor(self):
        """Reading all lines returns only lines up to and including the cursor row."""
        context = _make_context(rows=24, cols=80)
        _write(context, "hello\r\nworld")
        result = context.get_buffer_content()
        lines = result.split('\n')
        assert lines == ["hello", "world"]

    def test_read_n_excludes_blank_rows_below_cursor(self):
        """Reading N lines does not count blank rows below the cursor against the limit."""
        context = _make_context(rows=24, cols=80)
        _write(context, "hello\r\nworld")
        result = context.get_buffer_content(max_lines=10)
        lines = result.split('\n')
        assert lines == ["hello", "world"]

    def test_read_all_empty_terminal_returns_single_blank_line(self):
        """An untouched terminal returns a single empty line (cursor is on row 0)."""
        context = _make_context(rows=24, cols=80)
        result = context.get_buffer_content()
        assert result == ""

    def test_read_all_single_line(self):
        """A single line of output is returned without trailing blank lines."""
        context = _make_context(rows=24, cols=80)
        _write(context, "only one line")
        result = context.get_buffer_content()
        assert result == "only one line"

    def test_read_n_fewer_than_content_lines(self):
        """Requesting fewer lines than exist returns the most recent N lines."""
        context = _make_context(rows=24, cols=80)
        _write(context, "line1\r\nline2\r\nline3\r\nline4\r\nline5")
        result = context.get_buffer_content(max_lines=3)
        lines = result.split('\n')
        assert lines == ["line3", "line4", "line5"]

    def test_read_all_and_read_n_agree_on_content(self):
        """read_all and read_n=large both return the same content."""
        context = _make_context(rows=24, cols=80)
        _write(context, "alpha\r\nbeta\r\ngamma")
        all_content = context.get_buffer_content()
        large_n_content = context.get_buffer_content(max_lines=1000)
        assert all_content == large_n_content

    def test_trailing_spaces_stripped_per_line(self):
        """Each line has trailing spaces stripped."""
        context = _make_context(rows=24, cols=80)
        _write(context, "hi")
        result = context.get_buffer_content()
        assert result == "hi"
        assert not result.endswith(" ")

    def test_cursor_below_content_excludes_blank_rows_between_content_and_cursor(self):
        """Blank rows between content and a cursor moved down by escape sequences are excluded."""
        context = _make_context(rows=24, cols=80)
        _write(context, "hello")
        # Simulate a shell moving the cursor to row 10 via absolute positioning,
        # as cmd.exe does on Windows startup.
        _write(context, "\x1b[11;1H")
        result = context.get_buffer_content()
        lines = result.split('\n')
        # Should include hello on row 0 and the blank rows up to cursor row 10,
        # but nothing below row 10.
        assert len(lines) == 11
        assert lines[0] == "hello"
        assert all(line == "" for line in lines[1:])

    def test_cursor_below_content_read_n_anchors_to_cursor(self):
        """Reading N lines anchors to the cursor row, not to the last non-blank line."""
        context = _make_context(rows=24, cols=80)
        _write(context, "hello")
        # Move cursor to row 10.
        _write(context, "\x1b[11;1H")
        result = context.get_buffer_content(max_lines=3)
        lines = result.split('\n')
        # The last 3 lines ending at the cursor (row 10) are all blank.
        assert lines == ["", "", ""]
