"""Terminal state management."""

import base64
import logging
from dataclasses import dataclass
from typing import Dict, Tuple

from humbug.gui.terminal.terminal_buffer import (
    TerminalBuffer, CharacterAttributes,
    TerminalBufferSnapshot
)


@dataclass
class MouseTrackingState:
    """Mouse tracking configuration."""
    enabled: bool = False
    mode: int = 0  # 0=off, 1000=normal, 1002=button, 1003=any
    utf8_mode: bool = False
    sgr_mode: bool = False


class TerminalState:
    """Manages terminal emulator state and processing."""

    def __init__(self, rows: int, cols: int):
        """
        Initialize terminal state.

        Args:
            rows: Initial number of rows
            cols: Initial number of columns
        """
        self._logger = logging.getLogger("TerminalState")

        # Initialize buffers
        self._main_buffer = TerminalBuffer(rows, cols, True)
        self._alternate_buffer = None
        self._current_buffer = self._main_buffer

        # Terminal state
        self._terminal_title = ""
        self._current_directory = None
        self._escape_seq_buffer = ""
        self._in_escape_seq = False
        self._screen_reverse_mode = False
        self._mouse_tracking = MouseTrackingState()

        self.clipboard_data = None

        # Default colors (will be set by widget)
        self._default_fg = 0
        self._default_bg = 0

        # ANSI color mapping - will be populated by widget
        self._ansi_colors = {}

    @property
    def current_buffer(self) -> TerminalBuffer:
        """Get current terminal buffer."""
        return self._current_buffer

    @property
    def mouse_tracking(self) -> MouseTrackingState:
        """Get mouse tracking state."""
        return self._mouse_tracking

    @property
    def screen_reverse_mode(self) -> bool:
        """Get screen reverse mode state."""
        return self._screen_reverse_mode

    @property
    def terminal_title(self) -> str:
        """Get terminal title."""
        return self._terminal_title

    def set_default_colors(self, fg: int, bg: int) -> None:
        """Set default foreground and background colors."""
        self._default_fg = fg
        self._default_bg = bg

    def set_ansi_colors(self, color_map: Dict[int, int]) -> None:
        """Set ANSI color mapping."""
        self._ansi_colors = color_map

    def resize(self, new_rows: int, new_cols: int) -> None:
        """
        Resize terminal buffers.

        Args:
            new_rows: New number of rows
            new_cols: New number of columns
        """
        self._main_buffer.resize(new_rows, new_cols)
        if self._alternate_buffer:
            self._alternate_buffer.resize(new_rows, new_cols)

    def _xterm_to_rgb(self, color_index: int) -> int:
        """
        Convert xterm-256 color index to RGB value.

        Args:
            color_index: Color index from 0-255

        Returns:
            32-bit RGB color value (0x00RRGGBB format)
        """
        # Use color table for standard colors (0-15)
        if color_index < 16:
            return self._ansi_colors[color_index]

        # Handle 6x6x6 color cube (indices 16-231)
        if color_index < 232:
            # Remove the 16 basic colors offset
            color_index -= 16

            # Extract RGB components
            # Each component has 6 possible values: 0, 95, 135, 175, 215, 255
            blue = color_index % 6
            green = (color_index // 6) % 6
            red = color_index // 36

            # Convert to actual RGB values
            def component_to_value(component: int) -> int:
                if component == 0:
                    return 0
                return 40 * component + 55

            r = component_to_value(red)
            g = component_to_value(green)
            b = component_to_value(blue)

            return (r << 16) | (g << 8) | b

        # Handle grayscale colors (indices 232-255)
        gray_value = 8 + (color_index - 232) * 10
        return (gray_value << 16) | (gray_value << 8) | gray_value

    def put_data(self, data: bytes) -> None:
        """
        Process received terminal data.

        Args:
            data: Raw bytes from terminal

        Raises:
            UnicodeDecodeError: If data cannot be decoded
        """
        text = data.decode(errors='replace')

        i = 0
        while i < len(text):
            char = text[i]
            i += 1

            if self._in_escape_seq:
                # Handle escape sequence processing
                if char in '\r\n\b\f\t\v':
                    self._current_buffer.write_char(char)
                    continue

                if char == '\x1b':
                    self._logger.warning(
                        f"Unknown escape sequence - discarding: {repr(self._escape_seq_buffer)}"
                    )
                    self._escape_seq_buffer = ""

                self._escape_seq_buffer += char

                if self._is_escape_sequence_complete(self._escape_seq_buffer):
                    self._process_escape_sequence(self._escape_seq_buffer)
                    self._escape_seq_buffer = ""
                    self._in_escape_seq = False
                elif len(self._escape_seq_buffer) > 128:  # Safety limit
                    self._logger.warning(
                        f"Escape sequence too long, discarding: {repr(self._escape_seq_buffer)}"
                    )
                    self._escape_seq_buffer = ""
                    self._in_escape_seq = False

            elif char == '\x1b':  # Start of new escape sequence
                self._in_escape_seq = True
                self._escape_seq_buffer = char

            else:
                self._current_buffer.write_char(char)

    def _is_escape_sequence_complete(self, sequence: str) -> bool:
        """
        Check if an escape sequence is complete.

        Args:
            sequence: The escape sequence to check

        Returns:
            bool: True if the sequence is complete
        """
        if len(sequence) < 2:
            return False

        if sequence.startswith('\x1b]'):  # OSC sequence
            return sequence.endswith('\x07') or sequence.endswith('\x1b\\')

        if sequence.startswith('\x1b['):  # CSI sequence
            return sequence[-1].isalpha() or sequence[-1] in '@`~'

        if sequence.startswith('\x1bP'):  # DCS sequence
            return sequence.endswith('\x1b\\')

        if sequence.startswith('\x1b#'):  # line attributes sequence
            return len(sequence) == 3

        # Simple ESC sequences
        return len(sequence) == 2 and sequence[1] in '78DEHM'

    def _process_dec_special(self, sequence: str) -> None:
        """Process DEC special sequences."""
        buffer = self._current_buffer
        code = sequence[-1]

        if code == '8':  # DECALN - Screen Alignment Pattern
            for r in range(buffer.rows):
                line_index = len(buffer.lines) - buffer.rows + r
                line = buffer.lines[line_index]
                for c in range(buffer.cols):
                    line.set_character(c, 'E')
        else:
            self._logger.warning(f"Unknown DEC special sequence {repr(sequence)}")

    def _process_escape_sequence(self, sequence: str) -> None:
        """Process ANSI escape sequence."""
        # OSC sequences
        if sequence.startswith('\x1b]'):
            self._process_osc(sequence)
            return

        # Private mode sequences
        if sequence.startswith('\x1b[?'):
            code = sequence[-1]
            params = sequence[3:-1]  # Remove ESC[? and final character
            if code == 'h':
                self._process_private_mode(params, True)
            elif code == 'l':
                self._process_private_mode(params, False)
            return

        # CSI sequences
        if sequence.startswith('\x1b['):
            self._process_csi(sequence)
            return

        # DEC special sequences
        if sequence.startswith('\x1b#'):
            self._process_dec_special(sequence)
            return

        # Simple escape sequences
        if len(sequence) == 2:
            code = sequence[1]
            buffer = self._current_buffer

            if code == '7':  # ESC 7 - Save Cursor
                buffer.save_cursor()

            elif code == '8':  # ESC 8 - Restore Cursor
                buffer.restore_cursor()

            elif code == 'D':  # Index
                cursor_row = buffer.cursor.row if not buffer.modes.origin else buffer.cursor.row + buffer.scroll_region.top
                if cursor_row != buffer.scroll_region.bottom - 1:
                    max_rows = buffer.rows if not buffer.modes.origin else buffer.scroll_region.rows
                    buffer.cursor.row = min(buffer.cursor.row + 1, max_rows - 1)
                    buffer.max_cursor_row = max(buffer.max_cursor_row, buffer.cursor.row)
                else:
                    buffer.scroll_up(1)
                buffer.cursor.delayed_wrap = False

            elif code == 'E':  # Next Line
                buffer.cursor.col = 0
                cursor_row = buffer.cursor.row if not buffer.modes.origin else buffer.cursor.row + buffer.scroll_region.top
                if cursor_row != buffer.scroll_region.bottom - 1:
                    max_rows = buffer.rows if not buffer.modes.origin else buffer.scroll_region.rows
                    buffer.cursor.row = min(buffer.cursor.row + 1, max_rows - 1)
                    buffer.max_cursor_row = max(buffer.max_cursor_row, buffer.cursor.row)
                else:
                    buffer.scroll_up(1)
                buffer.cursor.delayed_wrap = False

            elif code == 'H':  # HTS - Set tab stop at current position
                buffer.tab_stops.set_tab_stop(buffer.cursor.col)

            elif code == 'M':  # Reverse Index
                cursor_row = buffer.cursor.row if not buffer.modes.origin else buffer.cursor.row + buffer.scroll_region.top
                if cursor_row != buffer.scroll_region.top:
                    buffer.cursor.row = max(0, buffer.cursor.row - 1)
                else:
                    buffer.scroll_down(1)
                buffer.cursor.delayed_wrap = False

            else:
                self._logger.warning(f"Unknown simple ESC sequence: {repr(sequence)}")

            return

        self._logger.warning(f"Unknown ESC sequence: {repr(sequence)}")

    def _process_osc(self, sequence: str):
        """Handle Operating System Command sequences."""
        # Remove ESC] prefix and terminator (BEL or ST)
        if sequence.endswith('\x07'):  # BEL
            params = sequence[2:-1]
        elif sequence.endswith('\x1b\\'):  # ST
            params = sequence[2:-2]
        else:
            return

        try:
            # Split into command number and parameter
            parts = params.split(';', 1)
            command = int(parts[0])
            param = parts[1] if len(parts) > 1 else ''

            if command == 0:  # Set window title and icon name
                self._terminal_title = param

            elif command == 1:  # Set icon name only
                pass

            elif command == 2:  # Set window title only
                self._terminal_title = param

            elif command == 4:  # Change/query color number
                # Format: 4;index;spec
                color_parts = param.split(';')
                if len(color_parts) >= 2:
                    if len(color_parts) == 1:  # Query
                        pass
                    else:  # Set
                        pass

            elif command == 7:  # Set working directory
                self._current_directory = param

            elif command == 52:  # Manipulate selection data
                self._handle_selection_data(param)

        except (ValueError, IndexError) as e:
            self._logger.warning(f"Invalid OSC sequence: {sequence}, error: {e}")

    def _handle_selection_data(self, param: str):
        """Handle OSC 52 selection data operations."""
        try:
            clipboard, data = param.split(';', 1)
            if clipboard in ('c', 'p', 's'):  # Primary, clipboard, or secondary selection
                if data == '?':  # Query
                    # Should emit response with current selection
                    pass
                elif data:  # Set
                    # Base64 decode data and update clipboard
                    decoded = base64.b64decode(data).decode('utf-8')
                    # Note: Actual clipboard operation will be handled by widget
                    self.clipboard_data = decoded
                else:  # Clear
                    self.clipboard_data = None
        except (ValueError, TypeError) as e:
            self._logger.warning(f"Invalid selection data: {param}: {e}")

    def _process_private_mode(self, params: str, set_mode: bool):
        """Handle DEC private mode sequences (DECSET/DECRST)."""
        buffer = self._current_buffer
        try:
            modes = [int(x) for x in params.split(';')]
            for mode in modes:
                if mode == 1:  # DECCKM - Application Cursor Keys
                    buffer.modes.application_cursor = set_mode
                elif mode == 3:  # DECCOLM - 80/132 Column Mode
                    # In xterm this changes column count but for now we'll ignore
                    buffer.clear()
                    buffer.scroll_region.top = 0
                    buffer.scroll_region.bottom = buffer.rows
                    buffer.scroll_region.rows = buffer.rows
                elif mode == 5:  # DECSCNM - Screen Mode (Reverse)
                    self._screen_reverse_mode = set_mode
                elif mode == 6:  # DECOM - Origin Mode
                    buffer.modes.origin = set_mode
                    buffer.cursor.row = 0
                    buffer.max_cursor_row = 0
                    buffer.cursor.col = 0
                    buffer.cursor.delayed_wrap = False
                elif mode == 7:  # DECAWM - Auto-wrap Mode
                    buffer.modes.auto_wrap = set_mode
                elif mode == 12:  # att610 - Start/Stop Blinking Cursor
                    buffer.cursor.blink = set_mode
                elif mode == 25:  # DECTCEM - Text Cursor Enable Mode
                    buffer.cursor.visible = set_mode
                elif mode == 1000:  # X11 mouse reporting - normal tracking mode
                    self._mouse_tracking.enabled = set_mode
                    self._mouse_tracking.mode = 1000 if set_mode else 0
                elif mode == 1002:  # X11 mouse reporting - button event tracking
                    self._mouse_tracking.enabled = set_mode
                    self._mouse_tracking.mode = 1002 if set_mode else 0
                elif mode == 1003:  # X11 mouse reporting - any event tracking
                    self._mouse_tracking.enabled = set_mode
                    self._mouse_tracking.mode = 1003 if set_mode else 0
                elif mode == 1004:  # Send focus in/out events
                    buffer.focus_tracking = set_mode
                elif mode == 1005:  # UTF-8 mouse mode
                    self._mouse_tracking.utf8_mode = set_mode
                elif mode == 1006:  # SGR mouse mode
                    self._mouse_tracking.sgr_mode = set_mode
                elif mode == 1047:  # Use Alternate Screen Buffer
                    self._handle_alternate_screen(set_mode)
                elif mode == 1048:  # Save/Restore cursor
                    if set_mode:
                        buffer.save_cursor()
                    else:
                        buffer.restore_cursor()
                elif mode == 1049:  # Alternate Screen + save/restore cursor
                    if set_mode:
                        buffer.save_cursor()
                        self._handle_alternate_screen(True)
                    else:
                        self._handle_alternate_screen(False)
                        self.current_buffer.restore_cursor()
                elif mode == 2004:  # Bracketed paste mode
                    buffer.modes.bracketed_paste = set_mode
                else:
                    self._logger.warning(f"Unknown PM operation {mode}")
        except ValueError as e:
            self._logger.warning(f"Invalid private mode parameter: {params}")

    def _handle_alternate_screen(self, enable: bool):
        """Switch between main and alternate screen buffers."""
        if (enable and self._current_buffer == self._alternate_buffer) or \
           (not enable and self._current_buffer == self._main_buffer):
            return

        if enable:
            if not self._alternate_buffer:
                buffer = self._current_buffer
                self._alternate_buffer = TerminalBuffer(buffer.rows, buffer.cols, False)
            self._current_buffer = self._alternate_buffer
        else:
            self._current_buffer = self._main_buffer
            self._alternate_buffer = None

    def _process_csi(self, sequence: str) -> None:
        """Process CSI (Control Sequence Introducer) sequence."""
        buffer = self._current_buffer
        code = sequence[-1]

        # Parse parameters
        params_str = sequence[2:-1]  # Remove ESC[ and final character
        params = [int(p) if p.isdigit() else 0 for p in params_str.split(';')] if params_str else [0]

        if code == 'A':  # CUU - Cursor Up
            buffer.move_cursor_up(max(1, params[0]))

        elif code == 'B':  # CUD - Cursor Down
            buffer.move_cursor_down(max(1, params[0]))

        elif code == 'C':  # CUF - Cursor Forward
            buffer.move_cursor_forward(max(1, params[0]))

        elif code == 'D':  # CUB - Cursor Back
            buffer.move_cursor_back(max(1, params[0]))

        elif code == 'G':  # CHA - Cursor Horizontal Absolute
            col = max(0, params[0] - 1)  # Convert 1-based to 0-based
            buffer.cursor.col = min(col, buffer.cols - 1)
            buffer.cursor.delayed_wrap = False

        elif code == 'H':  # CUP - Cursor Position
            row = params[0] - 1 if params else 0
            col = params[1] - 1 if len(params) > 1 else 0
            buffer.set_cursor_position(row, col)

        elif code == 'J':  # ED - Erase in Display
            mode = params[0] if params else 0
            if mode == 0:  # Clear from cursor to end
                buffer.clear_region(
                    buffer.cursor.row,
                    buffer.cursor.col,
                    buffer.rows - 1,
                    buffer.cols - 1
                )
            elif mode == 1:  # Clear from start to cursor
                buffer.clear_region(0, 0, buffer.cursor.row, buffer.cursor.col)
            elif mode == 2:  # Clear entire screen
                buffer.clear_region(0, 0, buffer.rows - 1, buffer.cols - 1)

        elif code == 'K':  # EL - Erase in Line
            mode = params[0] if params else 0
            if mode == 0:  # Clear from cursor to end
                buffer.clear_region(
                    buffer.cursor.row,
                    buffer.cursor.col,
                    buffer.cursor.row,
                    buffer.cols - 1
                )
            elif mode == 1:  # Clear from start to cursor
                buffer.clear_region(
                    buffer.cursor.row,
                    0,
                    buffer.cursor.row,
                    buffer.cursor.col
                )
            elif mode == 2:  # Clear entire line
                buffer.clear_region(
                    buffer.cursor.row,
                    0,
                    buffer.cursor.row,
                    buffer.cols - 1
                )

        elif code == 'L':  # IL - Insert Line
            count = max(1, params[0] if params else 1)
            buffer.insert_lines(count)

        elif code == 'M':  # DL - Delete Line
            count = max(1, params[0] if params else 1)
            buffer.delete_lines(count)

        elif code == 'P':  # DCH - Delete Character
            count = max(1, params[0] if params else 1)
            buffer.delete_chars(count)

        elif code == 'S':  # SU - Scroll Up
            count = max(1, params[0] if params else 1)
            buffer.scroll_up(count)

        elif code == 'T':  # SD - Scroll Down
            count = max(1, params[0] if params else 1)
            buffer.scroll_down(count)

        elif code == 'X':  # ECH - Erase Character
            count = max(1, params[0] if params else 1)
            buffer.erase_chars(count)

        elif code == '@':  # ICH - Insert Character
            count = max(1, params[0] if params else 1)
            buffer.insert_chars(count)

        elif code == 'd':  # VPA - Line Position Absolute
            row = max(0, params[0] - 1) if params else 0  # Convert 1-based to 0-based
            max_rows = buffer.rows if not buffer.modes.origin else buffer.scroll_region.rows
            buffer.cursor.row = min(row, max_rows - 1)
            buffer.max_cursor_row = max(buffer.max_cursor_row, buffer.cursor.row)
            buffer.cursor.delayed_wrap = False

        elif code == 'f':  # HVP - Horizontal and Vertical Position
            row = params[0] if params else 1
            col = params[1] if len(params) > 1 else 1
            buffer.set_cursor_position(row, col)

        elif code == 'g':  # TBC - Tab clear
            mode = params[0] if params else 0

            if mode == 0:  # Clear tab stop at current position
                buffer.tab_stops.clear_tab_stop(buffer.cursor.col)
            elif mode == 3:  # Clear all tab stops
                buffer.tab_stops.clear_all_tab_stops()

        elif code == 'm':  # SGR - Select Graphic Rendition
            self._process_sgr(params)

        elif code == 'r':  # DECSTBM - Set Top and Bottom Margins
            top = max(0, params[0] - 1) if params else 0
            bottom = min(buffer.rows, params[1]) if len(params) > 1 and params[1] else buffer.rows
            if top < bottom:
                buffer.scroll_region.top = top
                buffer.scroll_region.bottom = bottom
                buffer.scroll_region.rows = bottom - top

        elif code == 's':  # Save cursor position
            buffer.save_cursor()

        elif code == 'u':  # Restore cursor position
            buffer.restore_cursor()

        else:
            self._logger.warning(f"Unknown CSI sequence {repr(sequence)}")

    def _process_sgr(self, params: list[int]) -> None:
        """Process SGR (Select Graphic Rendition) sequence."""
        buffer = self._current_buffer
        i = 0
        while i < len(params):
            param = params[i]

            if param == 0:  # Reset
                buffer.attributes.current = CharacterAttributes.NONE
                buffer.attributes.foreground = None
                buffer.attributes.background = None

            elif param == 1:  # Bold
                buffer.attributes.current |= CharacterAttributes.BOLD

            elif param == 2:  # Dim
                buffer.attributes.current |= CharacterAttributes.DIM

            elif param == 3:  # Italic
                buffer.attributes.current |= CharacterAttributes.ITALIC

            elif param == 4:  # Underline
                buffer.attributes.current |= CharacterAttributes.UNDERLINE

            elif param == 5:  # Blink
                buffer.attributes.current |= CharacterAttributes.BLINK

            elif param == 7:  # Inverse
                buffer.attributes.current |= CharacterAttributes.INVERSE

            elif param == 8:  # Hidden
                buffer.attributes.current |= CharacterAttributes.HIDDEN

            elif param == 9:  # Strike
                buffer.attributes.current |= CharacterAttributes.STRIKE

            elif param == 21:  # Normal intensity (not bold)
                buffer.attributes.current &= ~CharacterAttributes.BOLD

            elif param == 22:  # Normal intensity (not bold and not dim)
                buffer.attributes.current &= ~(CharacterAttributes.BOLD | CharacterAttributes.DIM)

            elif param == 23:  # Not italic
                buffer.attributes.current &= ~CharacterAttributes.ITALIC

            elif param == 24:  # Not underlined
                buffer.attributes.current &= ~CharacterAttributes.UNDERLINE

            elif param == 25:  # Not blinking
                buffer.attributes.current &= ~CharacterAttributes.BLINK

            elif param == 27:  # Not inverse
                buffer.attributes.current &= ~CharacterAttributes.INVERSE

            elif param == 28:  # Not hidden
                buffer.attributes.current &= ~CharacterAttributes.HIDDEN

            elif param == 29:  # Not strike
                buffer.attributes.current &= ~CharacterAttributes.STRIKE

            elif 30 <= param <= 37:  # Standard foreground color
                buffer.attributes.current |= CharacterAttributes.CUSTOM_FG
                buffer.attributes.foreground = self._ansi_colors[param - 30]

            elif 40 <= param <= 47:  # Standard background color
                buffer.attributes.current |= CharacterAttributes.CUSTOM_BG
                buffer.attributes.background = self._ansi_colors[param - 40]

            elif param == 38:  # Extended foreground color
                if i + 2 < len(params):
                    if params[i + 1] == 5:  # 256 colors
                        color_index = params[i + 2]
                        buffer.attributes.current |= CharacterAttributes.CUSTOM_FG
                        buffer.attributes.foreground = self._xterm_to_rgb(color_index)
                        i += 2
                    elif params[i + 1] == 2 and i + 4 < len(params):  # RGB
                        r, g, b = params[i + 2:i + 5]
                        buffer.attributes.current |= CharacterAttributes.CUSTOM_FG
                        buffer.attributes.foreground = (r << 16) | (g << 8) | b
                        i += 4

            elif param == 48:  # Extended background color
                if i + 2 < len(params):
                    if params[i + 1] == 5:  # 256 colors
                        color_index = params[i + 2]
                        buffer.attributes.current |= CharacterAttributes.CUSTOM_BG
                        buffer.attributes.background = self._xterm_to_rgb(color_index)
                        i += 2
                    elif params[i + 1] == 2 and i + 4 < len(params):  # RGB
                        r, g, b = params[i + 2:i + 5]
                        buffer.attributes.current |= CharacterAttributes.CUSTOM_BG
                        buffer.attributes.background = (r << 16) | (g << 8) | b
                        i += 4

            else:
                self._logger.warning(f"Unknown SGR sequence {params}")

            i += 1

    def create_state_metadata(self) -> Dict:
        """
        Create metadata dictionary capturing current state.

        Returns:
            Dictionary containing terminal state metadata
        """
        metadata = {}

        # Capture main buffer state
        metadata['main_buffer'] = self._serialize_buffer_snapshot(
            self._main_buffer.create_snapshot()
        )

        # Capture alternate buffer state if it exists
        if self._alternate_buffer:
            metadata['alternate_buffer'] = self._serialize_buffer_snapshot(
                self._alternate_buffer.create_snapshot()
            )

        # Capture terminal dimensions
        metadata['dimensions'] = {
            'rows': self._current_buffer.rows,
            'cols': self._current_buffer.cols
        }

        # Capture additional state
        metadata['terminal_title'] = self._terminal_title
        metadata['screen_reverse_mode'] = self._screen_reverse_mode
        metadata['mouse_tracking'] = {
            'enabled': self._mouse_tracking.enabled,
            'mode': self._mouse_tracking.mode,
            'utf8_mode': self._mouse_tracking.utf8_mode,
            'sgr_mode': self._mouse_tracking.sgr_mode
        }

        if self._current_directory:
            metadata['current_directory'] = self._current_directory

        return metadata

    def _serialize_buffer_snapshot(self, snapshot: TerminalBufferSnapshot) -> Dict:
        """
        Convert buffer snapshot to serializable dictionary.

        Args:
            snapshot: Buffer snapshot to serialize

        Returns:
            Dictionary containing serialized snapshot data
        """
        serialized = {
            'cursor': {
                'row': snapshot.cursor.row,
                'col': snapshot.cursor.col,
                'visible': snapshot.cursor.visible,
                'blink': snapshot.cursor.blink,
                'delayed_wrap': snapshot.cursor.delayed_wrap
            },
            'attributes': {
                'current': snapshot.attributes.current.value,
                'foreground': snapshot.attributes.foreground,
                'background': snapshot.attributes.background
            },
            'scroll_region': {
                'top': snapshot.scroll_region.top,
                'bottom': snapshot.scroll_region.bottom,
                'rows': snapshot.scroll_region.rows
            },
            'modes': {
                'origin': snapshot.modes.origin,
                'auto_wrap': snapshot.modes.auto_wrap,
                'application_keypad': snapshot.modes.application_keypad,
                'application_cursor': snapshot.modes.application_cursor,
                'bracketed_paste': snapshot.modes.bracketed_paste
            },
            'history_scrollback': snapshot.history_scrollback,
            'max_cursor_row': snapshot.max_cursor_row
        }

        # Serialize line data
        serialized['lines'] = []
        for line in snapshot.lines:
            line_data = []
            for col in range(line.width):
                char, attrs, fg, bg = line.get_character(col)
                line_data.append({
                    'char': char,
                    'attributes': attrs.value,
                    'fg_color': fg,
                    'bg_color': bg
                })
            serialized['lines'].append(line_data)

        return serialized

    def restore_from_metadata(self, metadata: Dict) -> None:
        """
        Restore terminal state from metadata.

        Args:
            metadata: Dictionary containing state metadata
        """
        # Restore dimensions first
        dims = metadata['dimensions']
        self.resize(dims['rows'], dims['cols'])

        # Restore main buffer
        main_buffer = self._deserialize_buffer_snapshot(
            metadata['main_buffer'],
            dims['rows'],
            dims['cols']
        )
        self._main_buffer = main_buffer
        self._current_buffer = main_buffer

        # Restore alternate buffer if it existed
        if 'alternate_buffer' in metadata:
            self._alternate_buffer = self._deserialize_buffer_snapshot(
                metadata['alternate_buffer'],
                dims['rows'],
                dims['cols']
            )

        # Restore terminal title
        self._terminal_title = metadata['terminal_title']

        # Restore screen mode
        self._screen_reverse_mode = metadata['screen_reverse_mode']

        # Restore mouse tracking
        tracking = metadata['mouse_tracking']
        self._mouse_tracking.enabled = tracking['enabled']
        self._mouse_tracking.mode = tracking['mode']
        self._mouse_tracking.utf8_mode = tracking['utf8_mode']
        self._mouse_tracking.sgr_mode = tracking['sgr_mode']

        # Restore current directory if it was set
        if 'current_directory' in metadata:
            self._current_directory = metadata['current_directory']

    def _deserialize_buffer_snapshot(self, data: Dict, rows: int, cols: int) -> TerminalBuffer:
        """
        Create new terminal buffer from serialized snapshot data.

        Args:
            data: Dictionary containing serialized snapshot data
            rows: Number of rows for new buffer
            cols: Number of columns for new buffer

        Returns:
            New TerminalBuffer with restored state
        """
        # Create new buffer with specified dimensions
        buffer = TerminalBuffer(rows, cols, data['history_scrollback'])

        # Restore line data
        buffer.lines.clear()
        for line_data in data['lines']:
            line = buffer.get_new_line()
            for col, char_data in enumerate(line_data):
                line.set_character(
                    col,
                    char_data['char'],
                    CharacterAttributes(char_data['attributes']),
                    char_data['fg_color'],
                    char_data['bg_color']
                )
            buffer.lines.append(line)

        # Restore cursor state
        cursor_data = data['cursor']
        buffer.cursor.row = cursor_data['row']
        buffer.cursor.col = cursor_data['col']
        buffer.cursor.visible = cursor_data['visible']
        buffer.cursor.blink = cursor_data['blink']
        buffer.cursor.delayed_wrap = cursor_data['delayed_wrap']

        # Restore attributes
        attr_data = data['attributes']
        buffer.attributes.current = CharacterAttributes(attr_data['current'])
        buffer.attributes.foreground = attr_data['foreground']
        buffer.attributes.background = attr_data['background']

        # Restore scroll region
        scroll_data = data['scroll_region']
        buffer.scroll_region.top = scroll_data['top']
        buffer.scroll_region.bottom = scroll_data['bottom']
        buffer.scroll_region.rows = scroll_data['rows']

        # Restore modes
        mode_data = data['modes']
        buffer.modes.origin = mode_data['origin']
        buffer.modes.auto_wrap = mode_data['auto_wrap']
        buffer.modes.application_keypad = mode_data['application_keypad']
        buffer.modes.application_cursor = mode_data['application_cursor']
        buffer.modes.bracketed_paste = mode_data['bracketed_paste']

        buffer.max_cursor_row = data['max_cursor_row']

        return buffer

    def get_terminal_size(self) -> Tuple[int, int]:
        """
        Get current terminal dimensions.

        Returns:
            Tuple of (rows, cols)
        """
        buffer = self._current_buffer
        return (buffer.rows, buffer.cols)
