"""Tests for TerminalBuffer character writing and cursor movement."""

import pytest

from terminal.terminal_buffer import TerminalBuffer
from terminal.terminal_line import TerminalCharacterAttributes

from tests.terminal.conftest import get_line_text, get_screen_text


class TestWriteChar:
    """Tests for write_char behaviour."""

    def test_write_printable_advances_cursor(self, make_buffer):
        buf = make_buffer(rows=5, cols=10)
        buf.write_char('A')
        assert buf.cursor().col == 1

    def test_write_fills_characters(self, make_buffer):
        buf = make_buffer(rows=5, cols=10)
        for ch in 'Hello':
            buf.write_char(ch)
        line_idx = len(buf.lines()) - buf.rows()
        line = buf.lines()[line_idx]
        text = ''.join(line.get_character(c)[0] for c in range(5))
        assert text == 'Hello'

    def test_carriage_return_moves_to_col_zero(self, make_buffer):
        buf = make_buffer(rows=5, cols=10)
        for ch in 'Hello':
            buf.write_char(ch)
        buf.write_char('\r')
        assert buf.cursor().col == 0

    def test_newline_moves_cursor_down(self, make_buffer):
        buf = make_buffer(rows=5, cols=10)
        buf.write_char('\n')
        assert buf.cursor().row == 1

    def test_backspace_moves_cursor_back(self, make_buffer):
        buf = make_buffer(rows=5, cols=10)
        for ch in 'AB':
            buf.write_char(ch)
        buf.write_char('\b')
        assert buf.cursor().col == 1

    def test_backspace_does_not_go_below_zero(self, make_buffer):
        buf = make_buffer(rows=5, cols=10)
        buf.write_char('\b')
        assert buf.cursor().col == 0

    def test_write_at_last_column_sets_delayed_wrap(self, make_buffer):
        buf = make_buffer(rows=5, cols=5)
        for ch in 'ABCDE':
            buf.write_char(ch)
        assert buf.cursor().col == 4
        assert buf.cursor().delayed_wrap is True

    def test_delayed_wrap_fires_on_next_char(self, make_buffer):
        buf = make_buffer(rows=5, cols=5)
        for ch in 'ABCDEF':
            buf.write_char(ch)
        # Cursor should have wrapped to row 1, col 1
        assert buf.cursor().row == 1
        assert buf.cursor().col == 1

    def test_newline_at_bottom_scrolls(self, make_buffer):
        buf = make_buffer(rows=3, cols=10)
        # Fill all rows then add one more newline
        for _ in range(3):
            buf.write_char('\n')
        # Cursor should still be at row 2 (bottom)
        assert buf.cursor().row == 2

    def test_tab_moves_to_next_tab_stop(self, make_buffer):
        buf = make_buffer(rows=5, cols=40)
        buf.write_char('\t')
        assert buf.cursor().col == 8

    def test_tab_from_mid_position(self, make_buffer):
        buf = make_buffer(rows=5, cols=40)
        buf.move_cursor_forward(3)
        buf.write_char('\t')
        assert buf.cursor().col == 8

    def test_tab_at_last_stop_goes_to_end(self, make_buffer):
        buf = make_buffer(rows=5, cols=10)
        # Move to col 8 (last tab stop before end)
        buf.move_cursor_forward(8)
        buf.write_char('\t')
        assert buf.cursor().col == 9  # Clipped to last column


class TestCursorMovement:
    """Tests for explicit cursor movement methods."""

    def test_move_cursor_up(self, make_buffer):
        buf = make_buffer(rows=10, cols=10)
        buf.move_cursor_down(5)
        buf.move_cursor_up(2)
        assert buf.cursor().row == 3

    def test_move_cursor_up_clamps_at_zero(self, make_buffer):
        buf = make_buffer(rows=10, cols=10)
        buf.move_cursor_up(100)
        assert buf.cursor().row == 0

    def test_move_cursor_down(self, make_buffer):
        buf = make_buffer(rows=10, cols=10)
        buf.move_cursor_down(3)
        assert buf.cursor().row == 3

    def test_move_cursor_down_clamps_at_bottom(self, make_buffer):
        buf = make_buffer(rows=10, cols=10)
        buf.move_cursor_down(100)
        assert buf.cursor().row == 9

    def test_move_cursor_forward(self, make_buffer):
        buf = make_buffer(rows=5, cols=10)
        buf.move_cursor_forward(4)
        assert buf.cursor().col == 4

    def test_move_cursor_forward_clamps(self, make_buffer):
        buf = make_buffer(rows=5, cols=10)
        buf.move_cursor_forward(100)
        assert buf.cursor().col == 9

    def test_move_cursor_back(self, make_buffer):
        buf = make_buffer(rows=5, cols=10)
        buf.move_cursor_forward(5)
        buf.move_cursor_back(2)
        assert buf.cursor().col == 3

    def test_move_cursor_back_clamps(self, make_buffer):
        buf = make_buffer(rows=5, cols=10)
        buf.move_cursor_back(100)
        assert buf.cursor().col == 0

    def test_set_cursor_position(self, make_buffer):
        buf = make_buffer(rows=10, cols=20)
        buf.set_cursor_position(5, 10)  # 1-based
        assert buf.cursor().row == 4
        assert buf.cursor().col == 9

    def test_set_cursor_position_clamps(self, make_buffer):
        buf = make_buffer(rows=10, cols=20)
        buf.set_cursor_position(100, 100)
        assert buf.cursor().row == 9
        assert buf.cursor().col == 19

    def test_save_and_restore_cursor(self, make_buffer):
        buf = make_buffer(rows=10, cols=20)
        buf.set_cursor_position(3, 7)
        buf.save_cursor()
        buf.set_cursor_position(1, 1)
        buf.restore_cursor()
        assert buf.cursor().row == 2
        assert buf.cursor().col == 6

    def test_cursor_movement_clears_delayed_wrap(self, make_buffer):
        buf = make_buffer(rows=5, cols=5)
        for ch in 'ABCDE':
            buf.write_char(ch)
        assert buf.cursor().delayed_wrap is True
        buf.move_cursor_back(1)
        assert buf.cursor().delayed_wrap is False


class TestEraseOperations:
    """Tests for erase in display and erase in line."""

    def _write_row(self, buf, text):
        for ch in text:
            buf.write_char(ch)

    def test_erase_in_line_to_end(self, make_buffer):
        buf = make_buffer(rows=5, cols=10)
        self._write_row(buf, 'Hello')
        buf.set_cursor_position(1, 3)  # col 2 (0-based)
        buf.erase_in_line(0)
        line_idx = len(buf.lines()) - buf.rows()
        text = ''.join(buf.lines()[line_idx].get_character(c)[0] for c in range(10))
        assert text[:2] == 'He'
        assert text[2:].strip() == ''

    def test_erase_in_line_from_start(self, make_buffer):
        buf = make_buffer(rows=5, cols=10)
        self._write_row(buf, 'Hello')
        buf.set_cursor_position(1, 4)  # col 3 (0-based)
        buf.erase_in_line(1)
        line_idx = len(buf.lines()) - buf.rows()
        text = ''.join(buf.lines()[line_idx].get_character(c)[0] for c in range(10))
        assert text[:4].strip() == ''

    def test_erase_entire_line(self, make_buffer):
        buf = make_buffer(rows=5, cols=10)
        self._write_row(buf, 'Hello')
        buf.set_cursor_position(1, 1)
        buf.erase_in_line(2)
        text = get_screen_text(buf)[0]
        assert text == ''

    def test_erase_in_display_to_end(self, make_buffer):
        buf = make_buffer(rows=3, cols=10)
        for ch in 'ABC':
            buf.write_char(ch)
        buf.write_char('\n')
        for ch in 'DEF':
            buf.write_char(ch)
        buf.set_cursor_position(1, 2)
        buf.erase_in_display(0)
        rows = get_screen_text(buf)
        assert rows[0][0] == 'A'
        assert rows[0][1:].strip() == ''
        assert rows[1].strip() == ''


class TestScrolling:
    """Tests for scroll operations."""

    def test_scroll_up_adds_blank_line_at_bottom(self, make_buffer):
        buf = make_buffer(rows=3, cols=10)
        for ch in 'ABC':
            buf.write_char(ch)
        buf.scroll_up(1)
        rows = get_screen_text(buf)
        assert rows[2] == ''

    def test_scroll_down_adds_blank_line_at_top(self, make_buffer):
        buf = make_buffer(rows=3, cols=10)
        buf.move_cursor_down(2)
        for ch in 'ABC':
            buf.write_char(ch)
        buf.scroll_down(1)
        rows = get_screen_text(buf)
        assert rows[0] == ''

    def test_history_grows_on_scroll_up(self, make_buffer):
        buf = make_buffer(rows=3, cols=10, history_scrollback=True)
        initial = buf.history_lines()
        buf.scroll_up(1)
        assert buf.history_lines() == initial + 1

    def test_no_history_growth_when_disabled(self, make_buffer):
        buf = make_buffer(rows=3, cols=10, history_scrollback=False)
        initial = buf.history_lines()
        buf.scroll_up(1)
        assert buf.history_lines() == initial


class TestInsertDelete:
    """Tests for insert/delete lines and characters."""

    def test_insert_lines(self, make_buffer):
        buf = make_buffer(rows=5, cols=10)
        buf.write_char('A')
        buf.write_char('\r')
        buf.write_char('\n')
        buf.write_char('B')
        buf.set_cursor_position(1, 1)
        buf.insert_lines(1)
        rows = get_screen_text(buf)
        assert rows[0] == ''
        assert rows[1] == 'A'

    def test_delete_lines(self, make_buffer):
        buf = make_buffer(rows=5, cols=10)
        buf.write_char('A')
        buf.write_char('\r')
        buf.write_char('\n')
        buf.write_char('B')
        buf.set_cursor_position(1, 1)
        buf.delete_lines(1)
        rows = get_screen_text(buf)
        assert rows[0] == 'B'

    def test_insert_chars(self, make_buffer):
        buf = make_buffer(rows=5, cols=10)
        for ch in 'ABCD':
            buf.write_char(ch)
        buf.set_cursor_position(1, 2)  # col 1
        buf.insert_chars(2)
        line_idx = len(buf.lines()) - buf.rows()
        text = ''.join(buf.lines()[line_idx].get_character(c)[0] for c in range(8))
        assert text[0] == 'A'
        assert text[1] == ' '
        assert text[2] == ' '
        assert text[3] == 'B'

    def test_delete_chars(self, make_buffer):
        buf = make_buffer(rows=5, cols=10)
        for ch in 'ABCDE':
            buf.write_char(ch)
        buf.set_cursor_position(1, 2)  # col 1
        buf.delete_chars(2)
        line_idx = len(buf.lines()) - buf.rows()
        text = ''.join(buf.lines()[line_idx].get_character(c)[0] for c in range(5))
        assert text[0] == 'A'
        assert text[1] == 'D'
        assert text[2] == 'E'
