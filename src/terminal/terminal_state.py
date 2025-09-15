"""Terminal state management."""

import logging
from dataclasses import dataclass
from typing import Dict, Tuple, Any

from terminal.terminal_buffer import TerminalBuffer, TerminalCharacterAttributes


@dataclass
class MouseTrackingState:
    """Mouse tracking configuration."""
    enabled: bool = False
    mode: int = 0  # 0=off, 1000=normal, 1002=button, 1003=any
    utf8_mode: bool = False
    sgr_mode: bool = False


class TerminalState:
    """Manages terminal emulator state and processing."""

    def __init__(self, rows: int, cols: int) -> None:
        """
        Initialize terminal state.

        Args:
            rows: Initial number of rows
            cols: Initial number of columns
        """
        self._logger = logging.getLogger("TerminalState")

        # Initialize buffers
        self._main_buffer = TerminalBuffer(rows, cols, True)
        self._alternate_buffer: TerminalBuffer | None = None
        self._current_buffer = self._main_buffer

        # Terminal state
        self._terminal_title = ""
        self._current_directory = ""
        self._escape_seq_buffer = ""
        self._in_escape_seq = False
        self._screen_reverse_mode = False
        self._mouse_tracking = MouseTrackingState()

        # Default colors (will be set by widget)
        self._default_fg = 0
        self._default_bg = 0

        # ANSI color mapping - will be populated by widget
        self._ansi_colors: dict = {}

    def current_buffer(self) -> TerminalBuffer:
        """Get current terminal buffer."""
        return self._current_buffer

    def mouse_tracking(self) -> MouseTrackingState:
        """Get mouse tracking state."""
        return self._mouse_tracking

    def screen_reverse_mode(self) -> bool:
        """Get screen reverse mode state."""
        return self._screen_reverse_mode

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
                        "Unknown escape sequence - discarding: %r", self._escape_seq_buffer
                    )
                    self._escape_seq_buffer = ""

                self._escape_seq_buffer += char

                if self._is_escape_sequence_complete(self._escape_seq_buffer):
                    self._process_escape_sequence(self._escape_seq_buffer)
                    self._escape_seq_buffer = ""
                    self._in_escape_seq = False

                elif len(self._escape_seq_buffer) > 128:  # Safety limit
                    self._logger.warning(
                        "Escape sequence too long, discarding: %r", self._escape_seq_buffer
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
        return len(sequence) == 2 and sequence[1] in '78=>DEFHMNOVWXZ\\^_clmno|}~'

    def _process_dec_special(self, sequence: str) -> None:
        """Process DEC special sequences."""
        buffer = self._current_buffer
        code = sequence[-1]

        if code == '8':  # DECALN - Screen Alignment Pattern
            buffer.decaln()

        else:
            self._logger.warning("Unknown DEC special sequence %r", sequence)

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
                buffer.index()

            elif code == 'E':  # Next Line
                buffer.next_line()

            elif code == 'H':  # HTS - Set tab stop at current position
                buffer.set_tab_stop()

            elif code == 'M':  # Reverse Index
                buffer.reverse_index()

            else:
                self._logger.warning("Unknown simple ESC sequence: %r", sequence)

            return

        self._logger.warning("Unknown ESC sequence: %r", sequence)

    def _process_osc(self, sequence: str) -> None:
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

        except (ValueError, IndexError) as e:
            self._logger.warning("Invalid OSC sequence: %r, error: %s", sequence, e)

    def _process_private_mode(self, params: str, set_mode: bool) -> None:
        """Handle DEC private mode sequences (DECSET/DECRST)."""
        buffer = self._current_buffer
        try:
            modes = [int(x) for x in params.split(';')]
            for mode in modes:
                buffer_mode = buffer.modes()
                if mode == 1:  # DECCKM - Application Cursor Keys
                    buffer_mode.application_cursor = set_mode

                elif mode == 3:  # DECCOLM - 80/132 Column Mode
                    # In xterm this changes column count but for now we'll ignore
                    buffer.clear()
                    scroll_region = buffer.scroll_region()
                    scroll_region.top = 0
                    scroll_region.bottom = buffer.rows()
                    scroll_region.rows = buffer.rows()

                elif mode == 5:  # DECSCNM - Screen Mode (Reverse)
                    self._screen_reverse_mode = set_mode

                elif mode == 6:  # DECOM - Origin Mode
                    buffer.set_origin(set_mode)

                elif mode == 7:  # DECAWM - Auto-wrap Mode
                    buffer_mode.auto_wrap = set_mode

                elif mode == 12:  # att610 - Start/Stop Blinking Cursor
                    buffer.set_cursor_blink(set_mode)

                elif mode == 25:  # DECTCEM - Text Cursor Enable Mode
                    buffer.set_cursor_visible(set_mode)

                elif mode == 1000:  # X11 mouse reporting - normal tracking mode
                    self._mouse_tracking.enabled = set_mode
                    self._mouse_tracking.mode = 1000 if set_mode else 0

                elif mode == 1002:  # X11 mouse reporting - button event tracking
                    self._mouse_tracking.enabled = set_mode
                    self._mouse_tracking.mode = 1002 if set_mode else 0

                elif mode == 1003:  # X11 mouse reporting - any event tracking
                    self._mouse_tracking.enabled = set_mode
                    self._mouse_tracking.mode = 1003 if set_mode else 0

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
                        self._current_buffer.restore_cursor()

                elif mode == 2004:  # Bracketed paste mode
                    buffer_mode.bracketed_paste = set_mode

                else:
                    self._logger.warning("Unknown PM operation %d", mode)

        except ValueError as e:
            self._logger.warning("Invalid private mode parameter: %r, error: %s", params, e)

    def _handle_alternate_screen(self, enable: bool) -> None:
        """Switch between main and alternate screen buffers."""
        if (enable and self._current_buffer == self._alternate_buffer) or \
           (not enable and self._current_buffer == self._main_buffer):
            return

        if enable:
            if not self._alternate_buffer:
                buffer = self._current_buffer
                self._alternate_buffer = TerminalBuffer(buffer.rows(), buffer.cols(), False)

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
            buffer.set_cursor_horizontal(max(1, params[0]))

        elif code == 'H':  # CUP - Cursor Position
            col = max(1, params[1]) if len(params) > 1 else 1
            buffer.set_cursor_position(max(1, params[0]), col)

        elif code == 'J':  # ED - Erase in Display
            buffer.erase_in_display(params[0])

        elif code == 'K':  # EL - Erase in Line
            buffer.erase_in_line(params[0])

        elif code == 'L':  # IL - Insert Line
            buffer.insert_lines(max(1, params[0]))

        elif code == 'M':  # DL - Delete Line
            buffer.delete_lines(max(1, params[0]))

        elif code == 'P':  # DCH - Delete Character
            buffer.delete_chars(max(1, params[0]))

        elif code == 'S':  # SU - Scroll Up
            buffer.scroll_up(max(1, params[0]))

        elif code == 'T':  # SD - Scroll Down
            buffer.scroll_down(max(1, params[0]))

        elif code == 'X':  # ECH - Erase Character
            buffer.erase_chars(max(1, params[0]))

        elif code == '@':  # ICH - Insert Character
            buffer.insert_chars(max(1, params[0]))

        elif code == 'd':  # VPA - Line Position Absolute
            buffer.set_cursor_vertical(max(1, params[0]))

        elif code == 'f':  # HVP - Horizontal and Vertical Position
            col = max(1, params[1]) if len(params) > 1 else 1
            buffer.set_cursor_position(max(1, params[0]), col)

        elif code == 'g':  # TBC - Tab clear
            mode = params[0] if params else 0

            if mode == 0:  # Clear tab stop at current position
                buffer.clear_tab_stop()
            elif mode == 3:  # Clear all tab stops
                buffer.clear_all_tab_stops()

        elif code == 'm':  # SGR - Select Graphic Rendition
            self._process_sgr(params)

        elif code == 'r':  # DECSTBM - Set Top and Bottom Margins
            top = max(1, params[0])
            bottom = max(1, params[1]) if len(params) > 1 else buffer.rows()
            buffer.set_top_and_bottom_margins(top, bottom)

        elif code == 's':  # Save cursor position
            buffer.save_cursor()

        elif code == 'u':  # Restore cursor position
            buffer.restore_cursor()

        else:
            self._logger.warning("Unknown CSI sequence %r", sequence)

    def _process_sgr(self, params: list[int]) -> None:
        """Process SGR (Select Graphic Rendition) sequence."""
        buffer = self._current_buffer
        i = 0
        while i < len(params):
            param = params[i]

            attributes = buffer.attributes()

            if param == 0:  # Reset
                attributes.current = TerminalCharacterAttributes.NONE
                attributes.foreground = None
                attributes.background = None

            elif param == 1:  # Bold
                attributes.current |= TerminalCharacterAttributes.BOLD

            elif param == 2:  # Dim
                attributes.current |= TerminalCharacterAttributes.DIM

            elif param == 3:  # Italic
                attributes.current |= TerminalCharacterAttributes.ITALIC

            elif param == 4:  # Underline
                attributes.current |= TerminalCharacterAttributes.UNDERLINE

            elif param == 5:  # Blink
                attributes.current |= TerminalCharacterAttributes.BLINK

            elif param == 7:  # Inverse
                attributes.current |= TerminalCharacterAttributes.INVERSE

            elif param == 8:  # Hidden
                attributes.current |= TerminalCharacterAttributes.HIDDEN

            elif param == 9:  # Strike
                attributes.current |= TerminalCharacterAttributes.STRIKE

            elif param == 21:  # Normal intensity (not bold)
                attributes.current &= ~TerminalCharacterAttributes.BOLD

            elif param == 22:  # Normal intensity (not bold and not dim)
                attributes.current &= ~(TerminalCharacterAttributes.BOLD | TerminalCharacterAttributes.DIM)

            elif param == 23:  # Not italic
                attributes.current &= ~TerminalCharacterAttributes.ITALIC

            elif param == 24:  # Not underlined
                attributes.current &= ~TerminalCharacterAttributes.UNDERLINE

            elif param == 25:  # Not blinking
                attributes.current &= ~TerminalCharacterAttributes.BLINK

            elif param == 27:  # Not inverse
                attributes.current &= ~TerminalCharacterAttributes.INVERSE

            elif param == 28:  # Not hidden
                attributes.current &= ~TerminalCharacterAttributes.HIDDEN

            elif param == 29:  # Not strike
                attributes.current &= ~TerminalCharacterAttributes.STRIKE

            elif 30 <= param <= 37:  # Standard foreground color
                attributes.current |= TerminalCharacterAttributes.CUSTOM_FG
                attributes.foreground = self._ansi_colors[param - 30]

            elif param == 38:  # Extended foreground color
                if i + 2 < len(params):
                    if params[i + 1] == 5:  # 256 colors
                        color_index = params[i + 2]
                        attributes.current |= TerminalCharacterAttributes.CUSTOM_FG
                        attributes.foreground = self._xterm_to_rgb(color_index)
                        i += 2

                    elif params[i + 1] == 2 and i + 4 < len(params):  # RGB
                        r, g, b = params[i + 2:i + 5]
                        attributes.current |= TerminalCharacterAttributes.CUSTOM_FG
                        attributes.foreground = (r << 16) | (g << 8) | b
                        i += 4

            elif param == 39:  # Default foreground color
                attributes.current &= ~TerminalCharacterAttributes.CUSTOM_FG
                attributes.foreground = None

            elif 40 <= param <= 47:  # Standard background color
                attributes.current |= TerminalCharacterAttributes.CUSTOM_BG
                attributes.background = self._ansi_colors[param - 40]

            elif param == 48:  # Extended background color
                if i + 2 < len(params):
                    if params[i + 1] == 5:  # 256 colors
                        color_index = params[i + 2]
                        attributes.current |= TerminalCharacterAttributes.CUSTOM_BG
                        attributes.background = self._xterm_to_rgb(color_index)
                        i += 2

                    elif params[i + 1] == 2 and i + 4 < len(params):  # RGB
                        r, g, b = params[i + 2:i + 5]
                        attributes.current |= TerminalCharacterAttributes.CUSTOM_BG
                        attributes.background = (r << 16) | (g << 8) | b
                        i += 4

            elif param == 49:  # Default background color
                attributes.current &= ~TerminalCharacterAttributes.CUSTOM_BG
                attributes.background = None

            elif 90 <= param <= 97:  # Bright foreground colors
                attributes.current |= TerminalCharacterAttributes.CUSTOM_FG
                attributes.foreground = self._ansi_colors[param - 90 + 8]  # Map to bright color indices

            elif 100 <= param <= 107:  # Bright background colors
                attributes.current |= TerminalCharacterAttributes.CUSTOM_BG
                attributes.background = self._ansi_colors[param - 100 + 8]  # Map to bright color indices

            else:
                self._logger.warning("Unknown SGR sequence: %r", params)

            i += 1

    def create_state_metadata(self) -> Dict[str, Any]:
        """
        Create metadata dictionary capturing current state.

        Returns:
            Dictionary containing terminal state metadata
        """
        metadata: Dict[str, Any] = {}

        # Get state from main buffer
        metadata['main_buffer'] = self._main_buffer.get_state()

        # Get alternate buffer state if it exists
        if self._alternate_buffer:
            metadata['alternate_buffer'] = self._alternate_buffer.get_state()

        # Additional terminal state
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

    def restore_from_metadata(self, metadata: Dict[str, Any]) -> None:
        """
        Restore terminal state from metadata.

        Args:
            metadata: Dictionary containing state metadata
        """
        # Restore buffer states
        self._main_buffer.restore_state(metadata['main_buffer'])
        self._current_buffer = self._main_buffer

        if 'alternate_buffer' in metadata:
            if not self._alternate_buffer:
                self._alternate_buffer = TerminalBuffer(
                    metadata['alternate_buffer'].dimensions['rows'],
                    metadata['alternate_buffer'].dimensions['cols'],
                    False
                )
            self._alternate_buffer.restore_state(metadata['alternate_buffer'])

        # Restore terminal settings
        self._terminal_title = metadata['terminal_title']
        self._screen_reverse_mode = metadata['screen_reverse_mode']

        # Restore mouse tracking
        tracking = metadata['mouse_tracking']
        self._mouse_tracking.enabled = tracking['enabled']
        self._mouse_tracking.mode = tracking['mode']
        self._mouse_tracking.utf8_mode = tracking['utf8_mode']
        self._mouse_tracking.sgr_mode = tracking['sgr_mode']

        if 'current_directory' in metadata:
            self._current_directory = metadata['current_directory']

    def current_directory(self) -> str:
        """Get current working directory."""
        return self._current_directory

    def get_terminal_size(self) -> Tuple[int, int]:
        """
        Get current terminal dimensions.

        Returns:
            Tuple of (rows, cols)
        """
        buffer = self._current_buffer
        return (buffer.rows(), buffer.cols())

    def terminal_rows(self) -> int:
        """Get the number of rows in the terminal display."""
        return self._current_buffer.rows()

    def terminal_columns(self) -> int:
        """Get the number of columns in the terminal display."""
        return self._current_buffer.cols()

    def terminal_history_lines(self) -> int:
        """Get the number of lines of history including the current display"""
        return self._current_buffer.history_lines()

    def application_cursor_mode(self) -> bool:
        """Get if terminal is in application cursor mode."""
        return self._current_buffer.modes().application_cursor

    def application_keypad_mode(self) -> bool:
        """Get if terminal is in application keypad mode."""
        return self._current_buffer.modes().application_keypad

    def bracketed_paste_mode(self) -> bool:
        """Get if terminal is in bracketed paste mode."""
        return self._current_buffer.modes().bracketed_paste

    def blinking_chars_on_screen(self) -> bool:
        """Determine if there are any blinking characters on-screen."""
        return self._current_buffer.blinking_chars_on_screen()
