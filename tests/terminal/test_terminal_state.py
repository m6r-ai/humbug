"""Tests for TerminalState ANSI/VT sequence processing."""

import pytest

from terminal.terminal_state import TerminalState
from terminal.terminal_buffer import TerminalBuffer

from tests.terminal.conftest import get_screen_text


def _put(state: TerminalState, text: str) -> None:
    state.put_data(text.encode())


def _screen(state: TerminalState) -> list[str]:
    return get_screen_text(state.current_buffer())


class TestPlainText:
    """Plain text without escape sequences."""

    def test_simple_text(self, make_state):
        state = make_state(rows=5, cols=20)
        _put(state, 'Hello')
        assert _screen(state)[0] == 'Hello'

    def test_newline(self, make_state):
        state = make_state(rows=5, cols=20)
        _put(state, 'Line1\r\nLine2')
        rows = _screen(state)
        assert rows[0] == 'Line1'
        assert rows[1] == 'Line2'

    def test_crlf(self, make_state):
        state = make_state(rows=5, cols=20)
        _put(state, 'Line1\r\nLine2')
        rows = _screen(state)
        assert rows[0] == 'Line1'
        assert rows[1] == 'Line2'

    def test_carriage_return_overwrites(self, make_state):
        state = make_state(rows=5, cols=20)
        _put(state, 'Hello\rWorld')
        assert _screen(state)[0] == 'World'


class TestCursorMovementSequences:
    """CSI cursor movement sequences."""

    def test_cursor_up(self, make_state):
        state = make_state(rows=10, cols=20)
        _put(state, '\n\n\n')  # Move to row 3
        _put(state, '\x1b[2A')  # CUU 2
        assert state.current_buffer().cursor().row == 1

    def test_cursor_down(self, make_state):
        state = make_state(rows=10, cols=20)
        _put(state, '\x1b[3B')  # CUD 3
        assert state.current_buffer().cursor().row == 3

    def test_cursor_forward(self, make_state):
        state = make_state(rows=5, cols=20)
        _put(state, '\x1b[5C')  # CUF 5
        assert state.current_buffer().cursor().col == 5

    def test_cursor_back(self, make_state):
        state = make_state(rows=5, cols=20)
        _put(state, '\x1b[10C\x1b[3D')  # CUF 10, CUB 3
        assert state.current_buffer().cursor().col == 7

    def test_cursor_position(self, make_state):
        state = make_state(rows=10, cols=20)
        _put(state, '\x1b[5;10H')  # CUP row=5, col=10
        buf = state.current_buffer()
        assert buf.cursor().row == 4
        assert buf.cursor().col == 9

    def test_cursor_horizontal_absolute(self, make_state):
        state = make_state(rows=5, cols=20)
        _put(state, '\x1b[8G')  # CHA col=8
        assert state.current_buffer().cursor().col == 7


class TestEraseSequences:
    """CSI erase sequences."""

    def test_erase_in_line_to_end(self, make_state):
        state = make_state(rows=5, cols=20)
        _put(state, 'Hello World')
        _put(state, '\x1b[5G')   # Move to col 5
        _put(state, '\x1b[0K')   # EL 0 - erase to end
        row = _screen(state)[0]
        assert row[:4] == 'Hell'
        assert row[4:].strip() == ''

    def test_erase_entire_line(self, make_state):
        state = make_state(rows=5, cols=20)
        _put(state, 'Hello World')
        _put(state, '\x1b[2K')
        assert _screen(state)[0] == ''

    def test_erase_display_to_end(self, make_state):
        state = make_state(rows=3, cols=20)
        _put(state, 'Line1\nLine2\nLine3')
        _put(state, '\x1b[1;1H')   # Move to top-left
        _put(state, '\x1b[0J')     # ED 0 - erase to end
        rows = _screen(state)
        for row in rows:
            assert row == ''


class TestSGRSequences:
    """SGR colour and attribute sequences."""

    def test_bold_attribute(self, make_state):
        state = make_state(rows=5, cols=20)
        _put(state, '\x1b[1mBold\x1b[0m')
        buf = state.current_buffer()
        line_idx = len(buf.lines()) - buf.rows()
        _, attrs, _, _ = buf.lines()[line_idx].get_character(0)
        from terminal.terminal_line import TerminalCharacterAttributes
        assert attrs & TerminalCharacterAttributes.BOLD

    def test_reset_clears_attributes(self, make_state):
        state = make_state(rows=5, cols=20)
        _put(state, '\x1b[1;3mAB\x1b[0mCD')
        buf = state.current_buffer()
        line_idx = len(buf.lines()) - buf.rows()
        from terminal.terminal_line import TerminalCharacterAttributes
        _, attrs_cd, _, _ = buf.lines()[line_idx].get_character(2)
        assert attrs_cd == TerminalCharacterAttributes.NONE

    def test_foreground_color(self, make_state):
        state = make_state(rows=5, cols=20)
        _put(state, '\x1b[31mRed\x1b[0m')  # Standard red
        buf = state.current_buffer()
        line_idx = len(buf.lines()) - buf.rows()
        from terminal.terminal_line import TerminalCharacterAttributes
        _, attrs, fg, _ = buf.lines()[line_idx].get_character(0)
        assert attrs & TerminalCharacterAttributes.CUSTOM_FG
        assert fg is not None

    def test_256_color_foreground(self, make_state):
        state = make_state(rows=5, cols=20)
        _put(state, '\x1b[38;5;196mX\x1b[0m')
        buf = state.current_buffer()
        line_idx = len(buf.lines()) - buf.rows()
        from terminal.terminal_line import TerminalCharacterAttributes
        _, attrs, fg, _ = buf.lines()[line_idx].get_character(0)
        assert attrs & TerminalCharacterAttributes.CUSTOM_FG
        assert fg is not None

    def test_rgb_color_foreground(self, make_state):
        state = make_state(rows=5, cols=20)
        _put(state, '\x1b[38;2;255;128;0mX\x1b[0m')
        buf = state.current_buffer()
        line_idx = len(buf.lines()) - buf.rows()
        from terminal.terminal_line import TerminalCharacterAttributes
        _, attrs, fg, _ = buf.lines()[line_idx].get_character(0)
        assert attrs & TerminalCharacterAttributes.CUSTOM_FG
        assert fg == (255 << 16) | (128 << 8) | 0


class TestPrivateModes:
    """DEC private mode sequences."""

    def test_alternate_screen_switch(self, make_state):
        state = make_state(rows=5, cols=20)
        _put(state, 'Main screen')
        main_buf = state.current_buffer()
        _put(state, '\x1b[?1049h')  # Enter alternate screen
        assert state.current_buffer() is not main_buf
        _put(state, '\x1b[?1049l')  # Leave alternate screen
        assert state.current_buffer() is main_buf

    def test_alternate_screen_content_isolated(self, make_state):
        state = make_state(rows=5, cols=20)
        _put(state, 'Main content')
        _put(state, '\x1b[?1049h')
        # Alternate screen should be blank
        assert _screen(state)[0] == ''
        _put(state, '\x1b[?1049l')
        assert _screen(state)[0] == 'Main content'

    def test_auto_wrap_mode_off(self, make_state):
        state = make_state(rows=5, cols=5)
        _put(state, '\x1b[?7l')  # Disable auto-wrap
        _put(state, 'ABCDEFGH')
        buf = state.current_buffer()
        # With auto-wrap off, cursor should stay at last column
        assert buf.cursor().col == 4
        assert buf.cursor().row == 0

    def test_cursor_visibility(self, make_state):
        state = make_state(rows=5, cols=20)
        _put(state, '\x1b[?25l')  # Hide cursor
        assert not state.current_buffer().cursor().visible
        _put(state, '\x1b[?25h')  # Show cursor
        assert state.current_buffer().cursor().visible

    def test_origin_mode(self, make_state):
        state = make_state(rows=10, cols=20)
        _put(state, '\x1b[3;8r')   # Set scroll region rows 3-8
        _put(state, '\x1b[?6h')    # Enable origin mode
        # In origin mode, cursor position is relative to scroll region
        assert state.current_buffer().cursor().row == 0
        _put(state, '\x1b[?6l')    # Disable origin mode


class TestOSCSequences:
    """OSC (Operating System Command) sequences."""

    def test_set_window_title(self, make_state):
        state = make_state(rows=5, cols=20)
        _put(state, '\x1b]0;My Terminal\x07')
        assert state.terminal_title() == 'My Terminal'

    def test_set_window_title_st_terminator(self, make_state):
        state = make_state(rows=5, cols=20)
        _put(state, '\x1b]2;Another Title\x1b\\')
        assert state.terminal_title() == 'Another Title'

    def test_set_working_directory(self, make_state):
        state = make_state(rows=5, cols=20)
        _put(state, '\x1b]7;/home/user/projects\x07')
        assert state.current_directory() == '/home/user/projects'


class TestScrollMargins:
    """Scroll region and margin behaviour."""

    def test_set_top_bottom_margins(self, make_state):
        state = make_state(rows=10, cols=20)
        _put(state, '\x1b[3;8r')
        buf = state.current_buffer()
        assert buf.scroll_region().top == 2
        assert buf.scroll_region().bottom == 8

    def test_scroll_up_within_region(self, make_state):
        state = make_state(rows=5, cols=10)
        # Write 4 rows so nothing scrolls into history yet
        _put(state, 'AAA\r\nBBB\r\nCCC\r\nDDD\r\n')
        # Set scroll region to rows 2-4 (1-based), scroll up 1 within it
        _put(state, '\x1b[2;4r')
        _put(state, '\x1b[2;1H')
        _put(state, '\x1b[S')
        rows = _screen(state)
        assert rows[0] == 'AAA'   # Row 1 - outside region, unchanged
        assert rows[1] == 'CCC'   # Row 2 - BBB scrolled out, CCC moved up
        assert rows[2] == 'DDD'   # Row 3 - moved up
        assert rows[3] == ''      # Row 4 - blank inserted at bottom of region
        assert rows[4] == ''      # Row 5 - outside region, was already blank


class TestTabStops:
    """Tab stop behaviour."""

    def test_default_tab_stops(self, make_state):
        state = make_state(rows=5, cols=40)
        _put(state, '\t')
        assert state.current_buffer().cursor().col == 8

    def test_set_tab_stop(self, make_state):
        state = make_state(rows=5, cols=40)
        _put(state, '\x1b[5G')   # Move to col 5
        _put(state, '\x1bH')     # Set tab stop here
        _put(state, '\x1b[1G')   # Back to col 1
        _put(state, '\t')
        assert state.current_buffer().cursor().col == 4  # 0-based col 4

    def test_clear_all_tab_stops(self, make_state):
        state = make_state(rows=5, cols=40)
        _put(state, '\x1b[3g')   # Clear all tab stops
        _put(state, '\t')
        # With no tab stops, cursor should go to last column
        assert state.current_buffer().cursor().col == 39


class TestStateMetadata:
    """State serialisation and restoration."""

    def test_round_trip_preserves_content(self, make_state):
        state = make_state(rows=5, cols=20)
        _put(state, 'Hello\r\nWorld')
        metadata = state.create_state_metadata()

        state2 = make_state(rows=5, cols=20)
        state2.restore_from_metadata(metadata)
        rows = get_screen_text(state2.current_buffer())
        assert rows[0] == 'Hello'
        assert rows[1] == 'World'

    def test_round_trip_preserves_cursor(self, make_state):
        state = make_state(rows=5, cols=20)
        _put(state, 'Hello\r\nWorld')
        metadata = state.create_state_metadata()

        state2 = make_state(rows=5, cols=20)
        state2.restore_from_metadata(metadata)
        buf = state2.current_buffer()
        assert buf.cursor().row == state.current_buffer().cursor().row
        assert buf.cursor().col == state.current_buffer().cursor().col

    def test_round_trip_preserves_title(self, make_state):
        state = make_state(rows=5, cols=20)
        _put(state, '\x1b]0;Test Title\x07')
        metadata = state.create_state_metadata()

        state2 = make_state(rows=5, cols=20)
        state2.restore_from_metadata(metadata)
        assert state2.terminal_title() == 'Test Title'


class TestResizeViaState:
    """Resize through TerminalState (covers both buffers)."""

    def test_resize_updates_dimensions(self, make_state):
        state = make_state(rows=5, cols=20)
        state.resize(10, 40)
        assert state.terminal_rows() == 10
        assert state.terminal_columns() == 40

    def test_resize_with_alternate_buffer(self, make_state):
        state = make_state(rows=5, cols=20)
        _put(state, '\x1b[?1049h')   # Enter alternate screen
        state.resize(10, 40)
        assert state.terminal_rows() == 10
        assert state.terminal_columns() == 40
        _put(state, '\x1b[?1049l')   # Leave alternate screen
        assert state.terminal_rows() == 10
        assert state.terminal_columns() == 40
