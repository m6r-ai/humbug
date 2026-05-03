"""Tests for soft-wrap reflow on resize.

These tests define the *desired* behaviour after the reflow feature is
implemented. They are marked with pytest.mark.xfail so the suite passes
cleanly today, but each will be promoted to a normal (non-xfail) test once
the implementation is in place.

A test should be un-marked (xfail removed) only when the implementation
actually makes it pass — never patch the implementation to force a pass.
"""

import pytest

from terminal.terminal_buffer import TerminalBuffer
from terminal.terminal_line import TerminalCharacterAttributes

from tests.terminal.conftest import get_line_text, get_screen_text


def _write(buf: TerminalBuffer, text: str) -> None:
    for ch in text:
        buf.write_char(ch)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _logical_lines(buf: TerminalBuffer) -> list[str]:
    """Return the visible screen collapsed so that soft-wrap continuations
    are joined back into a single string per logical line."""
    rows = get_screen_text(buf)
    result = []
    lines = buf.lines()
    total = len(lines)
    screen_start = total - buf.rows()
    for r, text in enumerate(rows):
        line = lines[screen_start + r]
        if not line.continuation:
            result.append(text)
        else:
            result[-1] += text
    return result


# ---------------------------------------------------------------------------
# continuation flag — set correctly during writing
# ---------------------------------------------------------------------------

class TestContinuationFlag:
    """The continuation flag must be set on soft-wrap lines and cleared on
    hard-newline lines."""

    def test_no_continuation_on_fresh_line(self, make_buffer):
        buf = make_buffer(rows=5, cols=10)
        lines = buf.lines()
        for line in lines:
            assert not line.continuation

    def test_soft_wrap_sets_continuation(self, make_buffer):
        buf = make_buffer(rows=5, cols=5)
        _write(buf, 'ABCDEFGH')
        lines = buf.lines()
        total = len(lines)
        # Row 1 (the wrapped continuation) should have continuation=True
        assert lines[total - buf.rows() + 1].continuation is True

    def test_hard_newline_does_not_set_continuation(self, make_buffer):
        buf = make_buffer(rows=5, cols=10)
        _write(buf, 'Hello\nWorld')
        lines = buf.lines()
        total = len(lines)
        # Row 1 started with a hard newline — should NOT be a continuation
        assert lines[total - buf.rows() + 1].continuation is False

    def test_first_line_of_soft_wrap_group_not_continuation(self, make_buffer):
        buf = make_buffer(rows=5, cols=5)
        _write(buf, 'ABCDEFGH')
        lines = buf.lines()
        total = len(lines)
        assert lines[total - buf.rows()].continuation is False

    def test_three_line_wrap_flags(self, make_buffer):
        buf = make_buffer(rows=5, cols=5)
        _write(buf, 'ABCDEFGHIJKLM')
        lines = buf.lines()
        total = len(lines)
        assert lines[total - buf.rows() + 0].continuation is False
        assert lines[total - buf.rows() + 1].continuation is True
        assert lines[total - buf.rows() + 2].continuation is True

    def test_mixed_hard_and_soft(self, make_buffer):
        buf = make_buffer(rows=10, cols=5)
        _write(buf, 'ABCDEFGH\nIJ')
        lines = buf.lines()
        total = len(lines)
        # Row 0: 'ABCDE' — not a continuation
        assert lines[total - buf.rows() + 0].continuation is False
        # Row 1: 'FGH' — continuation of row 0
        assert lines[total - buf.rows() + 1].continuation is True
        # Row 2: 'IJ' — started by hard newline, NOT a continuation
        assert lines[total - buf.rows() + 2].continuation is False


# ---------------------------------------------------------------------------
# Reflow on widen
# ---------------------------------------------------------------------------

class TestReflowWiden:
    """When the terminal is widened, soft-wrapped lines should merge back."""

    def test_widen_merges_soft_wrap(self, make_buffer):
        buf = make_buffer(rows=5, cols=5)
        _write(buf, 'ABCDEFGH')
        buf.resize(5, 10)
        rows = get_screen_text(buf)
        assert rows[0] == 'ABCDEFGH'
        assert rows[1] == ''

    def test_widen_merges_three_line_wrap(self, make_buffer):
        buf = make_buffer(rows=5, cols=5)
        _write(buf, 'ABCDEFGHIJKLM')
        buf.resize(5, 20)
        rows = get_screen_text(buf)
        assert rows[0] == 'ABCDEFGHIJKLM'
        assert rows[1] == ''
        assert rows[2] == ''

    def test_widen_partial_merge(self, make_buffer):
        """Three physical lines that fit into two after widening."""
        buf = make_buffer(rows=5, cols=5)
        _write(buf, 'ABCDEFGHIJ')  # 10 chars -> 2 lines at width 5
        buf.resize(5, 7)           # At width 7: 'ABCDEFG' + 'HIJ'
        rows = get_screen_text(buf)
        assert rows[0] == 'ABCDEFG'
        assert rows[1] == 'HIJ'
        assert rows[2] == ''

    def test_widen_does_not_merge_hard_newlines(self, make_buffer):
        buf = make_buffer(rows=5, cols=10)
        _write(buf, 'Hello\r\nWorld')
        buf.resize(5, 20)
        rows = get_screen_text(buf)
        assert rows[0] == 'Hello'
        assert rows[1] == 'World'

    def test_widen_cursor_position_after_reflow(self, make_buffer):
        buf = make_buffer(rows=5, cols=5)
        _write(buf, 'ABCDEFGH')
        # Cursor is at row=1, col=3 before resize
        buf.resize(5, 10)
        # After reflow the logical line is 'ABCDEFGH' on row 0, cursor at col 8
        assert buf.cursor().row == 0
        assert buf.cursor().col == 8

    def test_widen_continuation_flags_updated(self, make_buffer):
        buf = make_buffer(rows=5, cols=5)
        _write(buf, 'ABCDEFGH')
        buf.resize(5, 10)
        lines = buf.lines()
        total = len(lines)
        # After reflow there is only one physical line for this logical line
        assert lines[total - buf.rows()].continuation is False
        assert lines[total - buf.rows() + 1].continuation is False


# ---------------------------------------------------------------------------
# Reflow on narrow
# ---------------------------------------------------------------------------

class TestReflowNarrow:
    """When the terminal is narrowed, lines that exceed the new width should
    wrap and produce continuation lines."""

    def test_narrow_wraps_long_line(self, make_buffer):
        buf = make_buffer(rows=5, cols=10)
        _write(buf, 'ABCDEFGH')   # 8 chars, fits on one line at width 10
        buf.resize(5, 5)
        rows = get_screen_text(buf)
        assert rows[0] == 'ABCDE'
        assert rows[1] == 'FGH'

    def test_narrow_sets_continuation_on_new_wrap(self, make_buffer):
        buf = make_buffer(rows=5, cols=10)
        _write(buf, 'ABCDEFGH')
        buf.resize(5, 5)
        lines = buf.lines()
        total = len(lines)
        assert lines[total - buf.rows() + 0].continuation is False
        assert lines[total - buf.rows() + 1].continuation is True

    def test_narrow_does_not_wrap_hard_newline_lines(self, make_buffer):
        buf = make_buffer(rows=5, cols=10)
        _write(buf, 'Hi\r\nThere')
        buf.resize(5, 5)
        rows = get_screen_text(buf)
        assert rows[0] == 'Hi'
        assert rows[1] == 'There'

    def test_narrow_cursor_position_after_reflow(self, make_buffer):
        buf = make_buffer(rows=5, cols=10)
        _write(buf, 'ABCDEFGH')
        buf.resize(5, 5)
        # Cursor was at col 8 on row 0; after reflow it's at col 3 on row 1
        assert buf.cursor().row == 1
        assert buf.cursor().col == 3


# ---------------------------------------------------------------------------
# Round-trip: narrow then widen
# ---------------------------------------------------------------------------

class TestReflowRoundTrip:
    """Narrowing then widening should recover the original layout."""

    def test_round_trip_single_soft_wrap(self, make_buffer):
        buf = make_buffer(rows=5, cols=10)
        _write(buf, 'ABCDEFGH')
        buf.resize(5, 5)
        # Widen beyond original — requires genuine reflow to merge the lines
        buf.resize(5, 12)
        rows = get_screen_text(buf)
        assert rows[0] == 'ABCDEFGH'
        assert rows[1] == ''

    def test_round_trip_multiple_wraps(self, make_buffer):
        buf = make_buffer(rows=5, cols=20)
        _write(buf, 'ABCDEFGHIJKLMNOPQRST')  # Exactly 20 chars
        buf.resize(5, 5)
        # Widen beyond original — requires genuine reflow to merge the lines
        buf.resize(5, 25)
        rows = get_screen_text(buf)
        assert rows[0] == 'ABCDEFGHIJKLMNOPQRST'

    def test_round_trip_mixed_content(self, make_buffer):
        buf = make_buffer(rows=10, cols=10)
        _write(buf, 'ABCDEFGHIJ\r\nShort')   # Soft-wrap + hard-newline
        buf.resize(10, 5)
        buf.resize(10, 10)
        rows = get_screen_text(buf)
        assert rows[0] == 'ABCDEFGHIJ'
        assert rows[1] == 'Short'


# ---------------------------------------------------------------------------
# Scrollback reflow
# ---------------------------------------------------------------------------

class TestReflowScrollback:
    """Reflow must also apply to lines that have scrolled into the scrollback."""

    def test_scrollback_soft_wrap_reflows_on_widen(self, make_buffer):
        buf = make_buffer(rows=3, cols=5, history_scrollback=True)
        # ABCDEFGH soft-wraps into 2 physical lines at width 5, then both
        # scroll into history as subsequent lines push them off screen
        _write(buf, 'ABCDEFGH\r\n')
        _write(buf, 'Next\r\n')
        _write(buf, 'Last\r\n')
        buf.resize(3, 10)
        # The scrollback line 'ABCDEFGH' should now be a single logical line
        scrollback_line = get_line_text(buf, 0)
        assert scrollback_line == 'ABCDEFGH'

    def test_scrollback_logical_line_count_correct(self, make_buffer):
        buf = make_buffer(rows=3, cols=5, history_scrollback=True)
        for i in range(5):
            _write(buf, f'L{i}\r\n')   # Short lines — no wrapping
        initial_history = buf.history_lines()
        buf.resize(3, 10)
        # No soft-wraps existed, so reflow should not change the line count
        assert buf.history_lines() == initial_history
        buf.resize(3, 20)
        assert buf.history_lines() == initial_history


# ---------------------------------------------------------------------------
# Attribute preservation through reflow
# ---------------------------------------------------------------------------

class TestReflowAttributes:
    """SGR attributes must survive reflow correctly."""

    def test_bold_attribute_preserved_across_wrap_boundary(self, make_buffer):
        buf = make_buffer(rows=5, cols=5)
        # Write bold characters that will wrap
        buf.attributes().current |= TerminalCharacterAttributes.BOLD
        _write(buf, 'ABCDEFGH')
        buf.resize(5, 10)
        lines = buf.lines()
        total = len(lines)
        line = lines[total - buf.rows()]
        for col in range(8):
            _, attrs, _, _ = line.get_character(col)
            assert attrs & TerminalCharacterAttributes.BOLD

    def test_mixed_attributes_preserved_across_wrap_boundary(self, make_buffer):
        buf = make_buffer(rows=5, cols=5)
        # First 3 chars bold, next 5 normal — wrap occurs mid-sequence
        buf.attributes().current |= TerminalCharacterAttributes.BOLD
        _write(buf, 'ABC')
        buf.attributes().current = TerminalCharacterAttributes.NONE
        _write(buf, 'DEFGH')
        buf.resize(5, 10)
        lines = buf.lines()
        total = len(lines)
        line = lines[total - buf.rows()]
        for col in range(3):
            _, attrs, _, _ = line.get_character(col)
            assert attrs & TerminalCharacterAttributes.BOLD
        for col in range(3, 8):
            _, attrs, _, _ = line.get_character(col)
            assert not (attrs & TerminalCharacterAttributes.BOLD)
