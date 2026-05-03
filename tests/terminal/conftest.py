"""Shared fixtures for terminal tests."""

import pytest

from terminal.terminal_buffer import TerminalBuffer
from terminal.terminal_state import TerminalState


@pytest.fixture
def make_buffer():
    """Factory for TerminalBuffer instances."""
    def _make(rows=24, cols=80, history_scrollback=True, scrollback_limit=None):
        return TerminalBuffer(rows, cols, history_scrollback, scrollback_limit)
    return _make


@pytest.fixture
def make_state():
    """Factory for TerminalState instances."""
    def _make(rows=24, cols=80, scrollback_limit=None):
        state = TerminalState(rows, cols, scrollback_limit)
        # Provide minimal colour tables so SGR sequences don't crash
        state.set_default_colors(0xFFFFFF, 0x000000)
        state.set_ansi_colors({i: i * 0x111111 for i in range(16)})
        return state
    return _make


def get_line_text(buffer: TerminalBuffer, line_index: int) -> str:
    """Extract the text content of a buffer line as a string, stripped of trailing spaces."""
    line = buffer.lines()[line_index]
    return ''.join(line.get_character(col)[0] for col in range(line.width)).rstrip()


def get_screen_text(buffer: TerminalBuffer) -> list[str]:
    """Return the visible screen rows as a list of stripped strings."""
    rows = buffer.rows()
    total = len(buffer.lines())
    return [get_line_text(buffer, total - rows + r) for r in range(rows)]
