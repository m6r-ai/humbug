"""Tests for TerminalBuffer resize behaviour.

These tests document the *current* resize behaviour precisely. When reflow is
implemented, the soft-wrap tests will need to be updated to reflect the new
expected behaviour. The hard-newline and cursor-addressed tests should
continue to pass unchanged.
"""

import pytest

from terminal.terminal_buffer import TerminalBuffer
from terminal.terminal_line import TerminalCharacterAttributes

from tests.terminal.conftest import get_line_text, get_screen_text


def _write(buf: TerminalBuffer, text: str) -> None:
    """Write a string into the buffer one character at a time."""
    for ch in text:
        buf.write_char(ch)


class TestResizeSimple:
    """Basic resize without any content."""

    def test_widen_updates_cols(self, make_buffer):
        buf = make_buffer(rows=5, cols=10)
        buf.resize(5, 20)
        assert buf.cols() == 20

    def test_narrow_updates_cols(self, make_buffer):
        buf = make_buffer(rows=5, cols=20)
        buf.resize(5, 10)
        assert buf.cols() == 10

    def test_grow_rows_updates_rows(self, make_buffer):
        buf = make_buffer(rows=5, cols=10)
        buf.resize(10, 10)
        assert buf.rows() == 10

    def test_shrink_rows_updates_rows(self, make_buffer):
        buf = make_buffer(rows=10, cols=10)
        buf.resize(5, 10)
        assert buf.rows() == 5

    def test_cursor_col_clamped_on_narrow(self, make_buffer):
        buf = make_buffer(rows=5, cols=20)
        buf.move_cursor_forward(15)
        buf.resize(5, 10)
        assert buf.cursor().col <= 9

    def test_cursor_row_clamped_on_shrink(self, make_buffer):
        buf = make_buffer(rows=10, cols=10)
        buf.move_cursor_down(8)
        buf.resize(5, 10)
        assert buf.cursor().row <= 4

    def test_resize_to_same_size_is_noop(self, make_buffer):
        buf = make_buffer(rows=5, cols=10)
        _write(buf, 'Hello')
        buf.resize(5, 10)
        assert get_screen_text(buf)[0] == 'Hello'


class TestResizeContentPreservation:
    """Content written with hard newlines should survive resize."""

    def test_hard_newline_content_preserved_on_widen(self, make_buffer):
        buf = make_buffer(rows=5, cols=10)
        _write(buf, 'Hello\r\nWorld')
        buf.resize(5, 20)
        rows = get_screen_text(buf)
        assert rows[0] == 'Hello'
        assert rows[1] == 'World'

    def test_hard_newline_content_preserved_on_narrow(self, make_buffer):
        buf = make_buffer(rows=5, cols=20)
        _write(buf, 'Hi\r\nThere')
        buf.resize(5, 10)
        rows = get_screen_text(buf)
        assert rows[0] == 'Hi'
        assert rows[1] == 'There'

    def test_multiple_lines_preserved_on_widen(self, make_buffer):
        buf = make_buffer(rows=10, cols=10)
        for i in range(5):
            _write(buf, f'Line{i}\r\n')
        buf.resize(10, 20)
        rows = get_screen_text(buf)
        for i in range(5):
            assert rows[i] == f'Line{i}'

    def test_content_at_exact_width_preserved(self, make_buffer):
        buf = make_buffer(rows=5, cols=5)
        _write(buf, 'ABCDE')
        buf.resize(5, 10)
        line_idx = len(buf.lines()) - buf.rows()
        text = ''.join(buf.lines()[line_idx].get_character(c)[0] for c in range(5))
        assert text == 'ABCDE'


class TestResizeSoftWrap:
    """Soft-wrap lines (written without explicit newlines).

    Currently these are treated the same as hard-newline lines on resize.
    After reflow is implemented, widening should merge soft-wrapped continuations
    back into the logical line. These tests document current behaviour; the
    reflow-specific tests live in test_terminal_buffer_reflow.py.
    """

    def test_soft_wrap_produces_two_lines(self, make_buffer):
        buf = make_buffer(rows=5, cols=5)
        _write(buf, 'ABCDEFGH')
        # With cols=5, 'ABCDE' fills row 0 and 'FGH' starts row 1
        rows = get_screen_text(buf)
        assert rows[0] == 'ABCDE'
        assert rows[1] == 'FGH'

    def test_soft_wrap_cursor_position(self, make_buffer):
        buf = make_buffer(rows=5, cols=5)
        _write(buf, 'ABCDEFGH')
        assert buf.cursor().row == 1
        assert buf.cursor().col == 3

    def test_soft_wrap_across_three_lines(self, make_buffer):
        buf = make_buffer(rows=5, cols=5)
        _write(buf, 'ABCDEFGHIJKLM')
        rows = get_screen_text(buf)
        assert rows[0] == 'ABCDE'
        assert rows[1] == 'FGHIJ'
        assert rows[2] == 'KLM'


class TestResizeCursorTracking:
    """Cursor position after resize."""

    def test_cursor_stays_on_same_logical_row_after_widen(self, make_buffer):
        buf = make_buffer(rows=5, cols=10)
        _write(buf, 'Hello\nWorld')
        row_before = buf.cursor().row
        buf.resize(5, 20)
        assert buf.cursor().row == row_before

    def test_cursor_col_unchanged_when_within_new_width(self, make_buffer):
        buf = make_buffer(rows=5, cols=20)
        _write(buf, 'Hello')
        buf.resize(5, 15)
        assert buf.cursor().col == 5

    def test_cursor_row_stays_in_bounds_after_shrink(self, make_buffer):
        buf = make_buffer(rows=10, cols=10)
        buf.move_cursor_down(9)
        buf.resize(5, 10)
        assert 0 <= buf.cursor().row < 5


class TestResizeScrollRegion:
    """Scroll region adjustment on resize."""

    def test_scroll_region_bottom_adjusts_on_grow(self, make_buffer):
        buf = make_buffer(rows=5, cols=10)
        buf.resize(10, 10)
        assert buf.scroll_region().bottom == 10

    def test_scroll_region_bottom_adjusts_on_shrink(self, make_buffer):
        buf = make_buffer(rows=10, cols=10)
        buf.resize(5, 10)
        assert buf.scroll_region().bottom == 5

    def test_custom_scroll_region_clipped_on_shrink(self, make_buffer):
        buf = make_buffer(rows=10, cols=10)
        buf.set_top_and_bottom_margins(3, 8)
        buf.resize(5, 10)
        assert buf.scroll_region().bottom <= 5


class TestResizeScrollback:
    """Scrollback buffer behaviour during resize."""

    def test_scrollback_grows_as_content_scrolls(self, make_buffer):
        buf = make_buffer(rows=3, cols=10, history_scrollback=True)
        for i in range(6):
            _write(buf, f'Line{i}\n')
        assert buf.history_lines() > buf.rows()

    def test_scrollback_limit_respected(self, make_buffer):
        buf = make_buffer(rows=3, cols=10, history_scrollback=True, scrollback_limit=10)
        for i in range(20):
            _write(buf, f'Line{i}\n')
        assert buf.history_lines() <= 10

    def test_scrollback_preserved_on_widen(self, make_buffer):
        buf = make_buffer(rows=3, cols=10, history_scrollback=True)
        for i in range(6):
            _write(buf, f'Row{i}\n')
        history_before = buf.history_lines()
        buf.resize(3, 20)
        # History should not shrink
        assert buf.history_lines() >= buf.rows()

    def test_alternate_buffer_no_history(self, make_buffer):
        buf = make_buffer(rows=3, cols=10, history_scrollback=False)
        for i in range(6):
            _write(buf, f'Line{i}\n')
        # Alternate buffer should never grow beyond rows
        assert buf.history_lines() == buf.rows()


class TestResizeGrowShrinkScrollbar:
    """Regression tests for spurious scrollback after grow-then-shrink."""

    def test_grow_then_shrink_no_spurious_scrollback(self, make_buffer):
        """Growing rows then shrinking back must not create spurious scrollback.

        Bug: max_cursor_row was bumped to new_rows-1 on grow, marking newly-added
        blank lines as visited.  On subsequent shrink those blank lines were not
        pruned and became scrollback, causing a spurious scrollbar.
        """
        buf = make_buffer(rows=3, cols=10, history_scrollback=True)
        # Fill to bottom so max_cursor_row == old_rows - 1
        _write(buf, 'A\r\nB\r\nC')
        assert buf.cursor().row == 2

        buf.resize(6, 10)   # grow
        buf.resize(3, 10)   # shrink back

        assert buf.history_lines() == buf.rows()

    def test_grow_then_shrink_with_scrollback_no_extra_lines(self, make_buffer):
        """Same as above but with pre-existing scrollback."""
        buf = make_buffer(rows=3, cols=10, history_scrollback=True)
        # Push some content into scrollback
        for i in range(5):
            _write(buf, f'Line{i}\n')

        history_before = buf.history_lines()
        buf.resize(6, 10)
        buf.resize(3, 10)

        assert buf.history_lines() == history_before

    def test_repeated_grow_shrink_stable(self, make_buffer):
        """Multiple grow/shrink cycles must not accumulate scrollback."""
        buf = make_buffer(rows=3, cols=10, history_scrollback=True)
        _write(buf, 'A\r\nB\r\nC')

        buf.resize(6, 10)
        buf.resize(3, 10)
        history_after_first = buf.history_lines()

        buf.resize(6, 10)
        buf.resize(3, 10)

        assert buf.history_lines() == history_after_first

    def test_grow_does_not_mark_blank_lines_as_visited(self, make_buffer):
        """After growing, the newly-added blank rows must remain unvisited.

        Verified by checking that a subsequent shrink back to the original size
        leaves history_lines() == rows (no scrollback inflation).
        """
        buf = make_buffer(rows=5, cols=10, history_scrollback=True)
        _write(buf, 'Hello\nWorld')
        # cursor at row 1; rows 2-4 are unvisited
        assert buf.max_cursor_row() == 1

        buf.resize(10, 10)
        # max_cursor_row must NOT have jumped to 9
        assert buf.max_cursor_row() == 1

        buf.resize(5, 10)
        assert buf.history_lines() == buf.rows()


class TestResizeShrinkExpandBlankLines:
    """Regression tests for blank lines appearing between prompts after shrink+expand."""

    def test_shrink_expand_visible_content_intact(self, make_buffer):
        """Shrinking then expanding must not push content into scrollback.

        Bug: after expand, max_cursor_row was set to new_rows-1, so a second
        shrink would not prune the blank lines, leaving the visible screen blank
        and content stranded in scrollback.
        """
        buf = make_buffer(rows=10, cols=80, history_scrollback=True)
        _write(buf, 'prompt1\r\nprompt2\r\nprompt3')
        # cursor at row 2; rows 3-9 unvisited

        buf.resize(5, 80)   # shrink
        buf.resize(10, 80)  # expand back

        # Content should still be on-screen, not in scrollback
        assert buf.history_lines() == buf.rows()
        rows = get_screen_text(buf)
        assert rows[0] == 'prompt1'
        assert rows[1] == 'prompt2'
        assert rows[2] == 'prompt3'

    def test_shrink_expand_shrink_no_blank_screen(self, make_buffer):
        """Shrink → expand → shrink must not produce a blank visible screen."""
        buf = make_buffer(rows=10, cols=80, history_scrollback=True)
        _write(buf, 'line1\r\nline2\r\nline3')

        buf.resize(5, 80)
        buf.resize(10, 80)
        buf.resize(5, 80)

        rows = get_screen_text(buf)
        # The first three rows must still contain the written content
        assert rows[0] == 'line1'
        assert rows[1] == 'line2'
        assert rows[2] == 'line3'

    def test_shrink_expand_cursor_row_correct(self, make_buffer):
        """After shrink+expand the cursor must be on the correct row."""
        buf = make_buffer(rows=10, cols=80, history_scrollback=True)
        _write(buf, 'A\r\nB\r\nC')
        assert buf.cursor().row == 2

        buf.resize(5, 80)
        buf.resize(10, 80)

        assert buf.cursor().row == 2
