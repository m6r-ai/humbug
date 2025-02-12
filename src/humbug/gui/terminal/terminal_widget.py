"""Terminal widget implementation."""

from dataclasses import dataclass
from typing import List, Optional, Dict, Tuple
from enum import Flag, auto
import array
import logging
import struct

from PySide6.QtWidgets import QWidget
from PySide6.QtCore import Qt, Signal, QRect, QPoint, QTimer
from PySide6.QtGui import (
    QPainter, QPaintEvent, QColor, QFontMetrics, QFont,
    QResizeEvent, QKeyEvent, QMouseEvent, QTextCursor,
    QGuiApplication
)

from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager


@dataclass
class TerminalSize:
    """Terminal size in rows and columns."""
    rows: int
    cols: int

    def __eq__(self, other) -> bool:
        if not isinstance(other, TerminalSize):
            return False
        return self.rows == other.rows and self.cols == other.cols

    def to_struct(self) -> bytes:
        """
        Convert terminal size to struct format for TIOCSWINSZ.

        Returns:
            bytes: Packed struct in format suitable for TIOCSWINSZ ioctl
        """
        return struct.pack('HHHH', self.rows, self.cols, 0, 0)


class CharacterAttributes(Flag):
    """Bit flags for character attributes."""
    NONE = 0
    BOLD = auto()
    ITALIC = auto()
    UNDERLINE = auto()
    STRIKE = auto()
    HIDDEN = auto()
    BLINK = auto()
    INVERSE = auto()
    DIM = auto()
    CUSTOM_FG = auto()
    CUSTOM_BG = auto()


class TerminalLine:
    """Fixed-width line of terminal characters."""
    def __init__(self, width: int):
        """Initialize empty line with given width."""
        self.width = width
        # For each character cell we store:
        # - Unicode codepoint (4 bytes)
        # - Attributes flags (4 bytes)
        # - FG color (4 bytes)
        # - BG color (4 bytes)
        self.data = array.array('L', [0] * (width * 4))

    def set_character(
        self,
        index: int,
        char: str,
        attributes: CharacterAttributes = CharacterAttributes.NONE,
        fg_color: Optional[int] = None,
        bg_color: Optional[int] = None
    ):
        """Set character and attributes at position."""
        if 0 <= index < self.width:
            base = index * 4
            self.data[base] = ord(char)
            self.data[base + 1] = attributes.value
            self.data[base + 2] = fg_color if fg_color is not None else 0
            self.data[base + 3] = bg_color if bg_color is not None else 0

    def get_character(self, index: int) -> Tuple[str, CharacterAttributes, Optional[int], Optional[int]]:
        """Get character and attributes at position."""
        if 0 <= index < self.width:
            base = index * 4
            char = chr(self.data[base])
            attributes = CharacterAttributes(self.data[base + 1])
            fg_color = self.data[base + 2] if self.data[base + 2] != 0 else None
            bg_color = self.data[base + 3] if self.data[base + 3] != 0 else None
            return (char, attributes, fg_color, bg_color)
        return (' ', CharacterAttributes.NONE, None, None)


class TerminalWidget(QWidget):
    """Terminal widget implementation."""

    data_ready = Signal(bytes)  # Emitted when user input is ready
    size_changed = Signal()  # Emitted when terminal size changes

    def __init__(self, parent: Optional[QWidget] = None):
        """Initialize terminal widget."""
        super().__init__(parent)
        self._logger = logging.getLogger("TerminalWidget")
        self._style_manager = StyleManager()

        # Enable focus and input
        self.setFocusPolicy(Qt.StrongFocus)

        # Storage for terminal content
        self._lines: List[TerminalLine] = []
        self._viewport_offset = 0  # Lines scrolled up from bottom

        # Terminal dimensions
        self._rows = 0
        self._cols = 0

        self._scroll_region_top = 0
        self._scroll_region_bottom = self._rows

        # Cursor state
        self._cursor_row = 0
        self._cursor_col = 0
        self._cursor_visible = True
        self._saved_cursor = None  # For save/restore cursor position

        # Selection state
        self._selection_start: Optional[Tuple[int, int]] = None  # (row, col)
        self._selection_end: Optional[Tuple[int, int]] = None    # (row, col)
        self._selecting = False

        # Color tables for ANSI colors
        self._ansi_colors = {
            # Standard colors (0-7)
            0: ColorRole.TERM_BLACK,
            1: ColorRole.TERM_RED,
            2: ColorRole.TERM_GREEN,
            3: ColorRole.TERM_YELLOW,
            4: ColorRole.TERM_BLUE,
            5: ColorRole.TERM_MAGENTA,
            6: ColorRole.TERM_CYAN,
            7: ColorRole.TERM_WHITE,

            # Bright colors (8-15) if needed
            8: ColorRole.TERM_BRIGHT_BLACK,
            9: ColorRole.TERM_BRIGHT_RED,
            10: ColorRole.TERM_BRIGHT_GREEN,
            11: ColorRole.TERM_BRIGHT_YELLOW,
            12: ColorRole.TERM_BRIGHT_BLUE,
            13: ColorRole.TERM_BRIGHT_MAGENTA,
            14: ColorRole.TERM_BRIGHT_CYAN,
            15: ColorRole.TERM_BRIGHT_WHITE,
        }

        # Current rendering attributes
        self._current_attributes = CharacterAttributes.NONE
        self._current_fg = None  # Used when CUSTOM_FG is set
        self._current_bg = None  # Used when CUSTOM_BG is set

        # ANSI escape sequence handling
        self._escape_seq_buffer = ""
        self._in_escape_seq = False

        # Text cursor for selection
        self._text_cursor = QTextCursor()

        # Default colors (can be customized later)
        self._default_fg = self._style_manager.get_color(ColorRole.TEXT_PRIMARY)
        self._default_bg = self._style_manager.get_color(ColorRole.TAB_BACKGROUND_ACTIVE)

        # Blink state
        self._blink_state = False
        self._blink_timer = QTimer(self)
        self._blink_timer.timeout.connect(self._toggle_blink)
        self._blink_timer.start(500)  # Toggle every 500ms

        self._current_size = None

        # Calculate initial size
        self._update_dimensions()
        self._initialize_buffer()

        # Connect style changed signal
        self._style_manager.style_changed.connect(self._handle_style_changed)
        self._handle_style_changed()

    def _handle_style_changed(self):
        """Handle style changes."""
        # Update terminal font
        font = QFont(self._style_manager.monospace_font_families)
        base_size = self._style_manager.base_font_size
        font.setPointSizeF(base_size * self._style_manager.zoom_factor)
        self.setFont(font)

        # Update default colors from style manager
        self._default_fg = self._style_manager.get_color(ColorRole.TEXT_PRIMARY).rgb()
        self._default_bg = self._style_manager.get_color(ColorRole.TAB_BACKGROUND_ACTIVE).rgb()

        # Force redraw with new colors
        self.update()

    def calculate_size(self) -> TerminalSize:
        """Calculate current terminal size in rows and columns."""
        fm = QFontMetrics(self.font())
        char_width = fm.horizontalAdvance(' ')
        char_height = fm.height()

        if char_width <= 0 or char_height <= 0:
            self._logger.warning(f"Invalid character dimensions: width={char_width}, height={char_height}")
            return TerminalSize(24, 80)  # Default fallback size

        viewport_width = self.width()
        viewport_height = self.height()

        cols = max(viewport_width // char_width, 1)
        rows = max(viewport_height // char_height, 1)

        return TerminalSize(rows, cols)

    def _update_dimensions(self) -> None:
        """Update terminal dimensions based on widget size and font metrics."""
        new_size = self.calculate_size()

        if new_size.cols != self._cols or new_size.rows != self._rows:
            old_rows, old_cols = self._rows, self._cols
            self._rows, self._cols = new_size.rows, new_size.cols

            # Calculate scroll region adjustments
            if old_rows > 0 and old_cols > 0:
                # Calculate the relative positions of scroll regions
                old_top_ratio = self._scroll_region_top / old_rows
                old_bottom_ratio = self._scroll_region_bottom / old_rows

                # Adjust scroll region to maintain relative positioning
                self._scroll_region_top = min(
                    round(old_top_ratio * self._rows),
                    self._rows - 1
                )
                self._scroll_region_bottom = min(
                    round(old_bottom_ratio * self._rows),
                    self._rows
                )

                # Ensure minimum scroll region size of 1 line
                if self._scroll_region_bottom <= self._scroll_region_top:
                    self._scroll_region_bottom = min(
                        self._scroll_region_top + 1,
                        self._rows
                    )

                self._reflow_content(old_rows, old_cols)

            else:
                # Initial setup - scroll region is full terminal
                self._scroll_region_top = 0
                self._scroll_region_bottom = self._rows

            self.size_changed.emit()

    def _initialize_buffer(self) -> None:
        """Initialize empty terminal buffer."""
        # Create initial lines
        self._lines = []
        print(f"init buffer {self._rows}")
        self._add_new_lines(self._rows)

    def _add_new_lines(self, count: int) -> None:
        """Add new empty lines to the buffer."""
        for _ in range(count):
            line = TerminalLine(self._cols)
            # Fill line with spaces using default attributes
            for i in range(self._cols):
                line.set_character(i, ' ')

            self._lines.append(line)

    def _xterm_to_rgb(self, color_index: int) -> int:
        """
        Convert xterm-256 color index to RGB value.

        Args:
            color_index: Color index from 0-255

        Returns:
            32-bit RGB color value (0x00RRGGBB format)

        The xterm-256 color scheme consists of:
        - Colors 0-15: Standard ANSI colors (handled by _ansi_colors)
        - Colors 16-231: 6x6x6 RGB color cube
        - Colors 232-255: Grayscale colors
        """
        # Use color table for standard colors (0-15)
        if color_index < 16:
            color_role = self._ansi_colors[color_index]
            return self._style_manager.get_color(color_role).rgb()

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
            # The formula converts index (0-5) to actual color value
            def component_to_value(component: int) -> int:
                if component == 0:
                    return 0
                return 40 * component + 55

            r = component_to_value(red)
            g = component_to_value(green)
            b = component_to_value(blue)

            return (r << 16) | (g << 8) | b

        # Handle grayscale colors (indices 232-255)
        # These are evenly spaced from dark to light
        # First gray (232) is 8,8,8
        # Last gray (255) is 238,238,238
        gray_value = 8 + (color_index - 232) * 10
        return (gray_value << 16) | (gray_value << 8) | gray_value

    def _process_sgr(self, params: list[int]) -> None:
        """Process SGR (Select Graphic Rendition) sequence."""
        i = 0
        while i < len(params):
            param = params[i]

            if param == 0:  # Reset
                self._current_attributes = CharacterAttributes.NONE
                self._current_fg = None
                self._current_bg = None

            elif param == 1:  # Bold
                self._current_attributes |= CharacterAttributes.BOLD

            elif param == 2:  # Dim
                self._current_attributes |= CharacterAttributes.DIM

            elif param == 3:  # Italic
                self._current_attributes |= CharacterAttributes.ITALIC

            elif param == 4:  # Underline
                self._current_attributes |= CharacterAttributes.UNDERLINE

            elif param == 5:  # Blink
                self._current_attributes |= CharacterAttributes.BLINK

            elif param == 7:  # Inverse
                self._current_attributes |= CharacterAttributes.INVERSE

            elif param == 8:  # Hidden
                self._current_attributes |= CharacterAttributes.HIDDEN

            elif param == 9:  # Strike
                self._current_attributes |= CharacterAttributes.STRIKE

            elif 30 <= param <= 37:  # Standard foreground color
                self._current_attributes |= CharacterAttributes.CUSTOM_FG
                color_role = self._ansi_colors[param - 30]
                self._current_fg = self._style_manager.get_color(color_role).rgb()

            elif 40 <= param <= 47:  # Standard background color
                self._current_attributes |= CharacterAttributes.CUSTOM_BG
                color_role = self._ansi_colors[param - 40]
                self._current_bg = self._style_manager.get_color(color_role).rgb()

            elif param == 38:  # Extended foreground color
                if i + 2 < len(params):
                    if params[i + 1] == 5:  # 256 colors
                        color_index = params[i + 2]
                        if color_index < 16:  # Use our color table for first 16 colors
                            self._current_attributes |= CharacterAttributes.CUSTOM_FG
                            color_role = self._ansi_colors[color_index]
                            self._current_fg = self._style_manager.get_color(color_role).rgb()
                        else:
                            # For other colors, construct RGB directly
                            self._current_attributes |= CharacterAttributes.CUSTOM_FG
                            self._current_fg = self._xterm_to_rgb(color_index)
                        i += 2
                    elif params[i + 1] == 2 and i + 4 < len(params):  # RGB
                        r, g, b = params[i + 2:i + 5]
                        self._current_attributes |= CharacterAttributes.CUSTOM_FG
                        self._current_fg = (r << 16) | (g << 8) | b
                        i += 4

            elif param == 48:  # Extended background color
                if i + 2 < len(params):
                    if params[i + 1] == 5:  # 256 colors
                        color_index = params[i + 2]
                        if color_index < 16:  # Use our color table for first 16 colors
                            self._current_attributes |= CharacterAttributes.CUSTOM_BG
                            color_role = self._ansi_colors[color_index]
                            self._current_bg = self._style_manager.get_color(color_role).rgb()
                        else:
                            # For other colors, construct RGB directly
                            self._current_attributes |= CharacterAttributes.CUSTOM_BG
                            self._current_bg = self._xterm_to_rgb(color_index)
                        i += 2
                    elif params[i + 1] == 2 and i + 4 < len(params):  # RGB
                        r, g, b = params[i + 2:i + 5]
                        self._current_attributes |= CharacterAttributes.CUSTOM_BG
                        self._current_bg = (r << 16) | (g << 8) | b
                        i += 4

            i += 1

    def _clear_region(self, start_row, start_col, end_row, end_col):
        """Clear a rectangular region of the terminal."""
        for row in range(start_row, end_row + 1):
            line_index = len(self._lines) - self._rows + row
            if 0 <= line_index < len(self._lines):
                line = self._lines[line_index]
                start = start_col if row == start_row else 0
                end = end_col if row == end_row else self._cols - 1
                for col in range(start, end + 1):
                    line.set_character(col, ' ')

    def _process_escape_sequence(self, sequence: str) -> None:
        """Process ANSI escape sequence."""
        # CSI sequences
        if sequence.startswith('\x1b['):
            code = sequence[-1]
            print(f"escape code {code}")
            # Parse just what we need based on the sequence
            if code == 'A':  # Up
                param = sequence[2:-1]
                amount = int(param) if param.isdigit() else 1
                self._cursor_row = max(0, self._cursor_row - amount)

            elif code == 'B':  # Down
                param = sequence[2:-1]
                amount = int(param) if param.isdigit() else 1
                self._cursor_row = min(self._rows - 1, self._cursor_row + amount)

            elif code == 'C':  # Forward
                param = sequence[2:-1]
                amount = int(param) if param.isdigit() else 1
                self._cursor_col = min(self._cols - 1, self._cursor_col + amount)

            elif code == 'D':  # Backward
                param = sequence[2:-1]
                amount = int(param) if param.isdigit() else 1
                self._cursor_col = max(0, self._cursor_col - amount)

            elif code == 'G':  # CHA - Cursor Horizontal Absolute
                param = sequence[2:-1]
                col = max(0, int(param) - 1) if param.isdigit() else 1  # Convert 1-based to 0-based
                self._cursor_col = min(col, self._cols - 1)

            elif code == 'H':  # Cursor position
                pos = sequence[2:-1].split(';')
                row = int(pos[0]) if pos and pos[0].isdigit() else 1
                col = int(pos[1]) if len(pos) > 1 and pos[1].isdigit() else 1
                self._cursor_row = min(self._rows - 1, max(0, row - 1))  # Convert to 0-based
                self._cursor_col = min(self._cols - 1, max(0, col - 1))

            elif code == 'J':  # Clear screen
                param = sequence[2:-1]
                mode = int(param) if param.isdigit() else 0
                if mode == 0:  # Clear from cursor to end
                    self._clear_region(
                        self._cursor_row,
                        self._cursor_col,
                        self._rows - 1,
                        self._cols - 1
                    )
                elif mode == 1:  # Clear from start to cursor
                    self._clear_region(0, 0, self._cursor_row, self._cursor_col)
                elif mode == 2:  # Clear entire screen
                    self._clear_region(0, 0, self._rows - 1, self._cols - 1)

            elif code == 'K':  # Clear line
                param = sequence[2:-1]
                mode = int(param) if param.isdigit() else 0
                if mode == 0:  # Clear from cursor to end
                    self._clear_region(
                        self._cursor_row,
                        self._cursor_col,
                        self._cursor_row,
                        self._cols - 1
                    )
                elif mode == 1:  # Clear from start to cursor
                    self._clear_region(
                        self._cursor_row,
                        0,
                        self._cursor_row,
                        self._cursor_col
                    )
                elif mode == 2:  # Clear entire line
                    self._clear_region(
                        self._cursor_row,
                        0,
                        self._cursor_row,
                        self._cols - 1
                    )

            elif code == 'L':  # IL - Insert Line
                param = sequence[2:-1]
                count = max(1, int(param) if param.isdigit() else 0)
                self._insert_lines(count)

            elif code == 'M':  # DL - Delete Line
                param = sequence[2:-1]
                count = max(1, int(param) if param.isdigit() else 0)
                self._delete_lines(count)

            elif code == 'P':  # DCH - Delete Character
                param = sequence[2:-1]
                count = max(1, int(param) if param.isdigit() else 0)
                self._delete_chars(count)

            elif code == 'S':  # SU - Scroll Up
                param = sequence[2:-1]
                count = max(1, int(param) if param.isdigit() else 0)
                self._scroll_up(count)

            elif code == 'T':  # SD - Scroll Down
                param = sequence[2:-1]
                count = max(1, int(param) if param.isdigit() else 0)
                self._scroll_down(count)

            elif code == 'X':  # ECH - Erase Character
                param = sequence[2:-1]
                count = max(1, int(param) if param.isdigit() else 0)
                self._erase_chars(count)

            elif code == '@':  # ICH - Insert Character
                param = sequence[2:-1]
                count = max(1, int(param) if param.isdigit() else 0)
                self._insert_chars(count)

            elif code == 'd':  # VPA - Line Position Absolute
                param = sequence[2:-1]
                row = max(0, int(param) - 1) if param.isdigit() else 1 # Convert 1-based to 0-based
                self._cursor_row = min(row, self._rows - 1)

            elif code == 'f':  # HVP - Horizontal & Vertical Position
                pos = sequence[2:-1].split(';')
                row = int(pos[0]) if pos and pos[0].isdigit() else 1
                col = int(pos[1]) if len(pos) > 1 and pos[1].isdigit() else 1
                self._cursor_row = min(self._rows - 1, max(0, row - 1))  # Convert to 0-based
                self._cursor_col = min(self._cols - 1, max(0, col - 1))

            elif code == 'm':  # SGR - Select Graphic Rendition
                params = sequence[2:-1].split(';')
                params = [int(p) if p.isdigit() else 0 for p in params]
                self._process_sgr(params)

            elif code == 'r':  # DECSTBM - Set Scrolling Region
                return
                params = sequence[2:-1].split(';')
                top = max(0, int(params[0]) - 1) if params and params[0].isdigit() else 0
                bottom = min(self._rows, int(params[1])) if len(params) > 1 and params[1].isdigit() else self._rows
                if top < bottom:
                    self._scroll_region_top = top
                    self._scroll_region_bottom = bottom
                    self._cursor_row = 0
                    self._cursor_col = 0

            elif code == 's':  # Save cursor position
                self._saved_cursor = (self._cursor_row, self._cursor_col)

            elif code == 'u':  # Restore cursor position
                if self._saved_cursor:
                    self._cursor_row, self._cursor_col = self._saved_cursor

        # Simple escape sequences
        elif len(sequence) == 2:
            if sequence[1] == 'M':  # Reverse Index
                if self._cursor_row == 0:
                    self._add_new_lines(1)
                else:
                    self._cursor_row -= 1
            elif sequence[1] == 'D':  # Index
                if self._cursor_row == self._rows - 1:
                    self._add_new_lines(1)
                else:
                    self._cursor_row += 1
            elif sequence[1] == 'E':  # Next Line
                if self._cursor_row == self._rows - 1:
                    self._add_new_lines(1)
                else:
                    self._cursor_row += 1
                self._cursor_col = 0

    def _insert_lines(self, count: int) -> None:
        """Insert blank lines at cursor position."""
        if not (self._scroll_region_top <= self._cursor_row < self._scroll_region_bottom):
            return

        # Calculate lines to move
        start = len(self._lines) - self._rows + self._cursor_row
        end = len(self._lines) - self._rows + self._scroll_region_bottom

        # Create new blank lines
        new_lines = []
        for _ in range(count):
            line = TerminalLine(self._cols)
            for col in range(self._cols):
                line.set_character(col, ' ')
            new_lines.append(line)

        # Insert new lines and remove excess
        self._lines[start:start] = new_lines
        self._lines[end:end + count] = []

    def _delete_lines(self, count: int) -> None:
        """Delete lines at cursor position."""
        if not (self._scroll_region_top <= self._cursor_row < self._scroll_region_bottom):
            return

        # Calculate lines to remove
        start = len(self._lines) - self._rows + self._cursor_row
        end = len(self._lines) - self._rows + self._scroll_region_bottom

        # Remove lines and add blank lines at bottom
        del self._lines[start:min(start + count, end)]
        for _ in range(count):
            line = TerminalLine(self._cols)
            for col in range(self._cols):
                line.set_character(col, ' ')
            self._lines.insert(end, line)

    def _insert_chars(self, count: int) -> None:
        """Insert blank characters at cursor position."""
        line_index = len(self._lines) - self._rows + self._cursor_row
        if 0 <= line_index < len(self._lines):
            line = self._lines[line_index]
            # Move existing characters right
            for col in range(self._cols - 1, self._cursor_col - 1, -1):
                if col >= self._cursor_col + count:
                    char, attrs, fg, bg = line.get_character(col - count)
                    line.set_character(col, char, attrs, fg, bg)

            # Insert spaces
            for col in range(self._cursor_col, min(self._cursor_col + count, self._cols)):
                line.set_character(col, ' ')

    def _delete_chars(self, count: int) -> None:
        """Delete characters at cursor position."""
        line_index = len(self._lines) - self._rows + self._cursor_row
        if 0 <= line_index < len(self._lines):
            line = self._lines[line_index]
            # Move characters left
            for col in range(self._cursor_col, self._cols):
                if col + count < self._cols:
                    char, attrs, fg, bg = line.get_character(col + count)
                    line.set_character(col, char, attrs, fg, bg)
                else:
                    line.set_character(col, ' ')

    def _erase_chars(self, count: int) -> None:
        """Erase characters at cursor position."""
        line_index = len(self._lines) - self._rows + self._cursor_row
        if 0 <= line_index < len(self._lines):
            line = self._lines[line_index]
            for col in range(self._cursor_col, min(self._cursor_col + count, self._cols)):
                line.set_character(col, ' ')

    def _scroll_up(self, count: int) -> None:
        """Scroll up within current scroll region."""
        if self._scroll_region_top >= self._scroll_region_bottom:
            return

        # Calculate actual lines in scroll region
        start = len(self._lines) - self._rows + self._scroll_region_top
        end = len(self._lines) - self._rows + self._scroll_region_bottom

        # Remove lines from top and add blank lines at bottom
        del self._lines[start:min(start + count, end)]
        for _ in range(count):
            line = TerminalLine(self._cols)
            for col in range(self._cols):
                line.set_character(col, ' ')
            self._lines.insert(end, line)

    def _scroll_down(self, count: int) -> None:
        """Scroll down within current scroll region."""
        if self._scroll_region_top >= self._scroll_region_bottom:
            return

        # Calculate actual lines in scroll region
        start = len(self._lines) - self._rows + self._scroll_region_top
        end = len(self._lines) - self._rows + self._scroll_region_bottom

        # Remove lines from bottom and add blank lines at top
        del self._lines[max(start, end - count):end]
        for _ in range(count):
            line = TerminalLine(self._cols)
            for col in range(self._cols):
                line.set_character(col, ' ')
            self._lines.insert(start, line)

    def _write_char(self, char: str) -> None:
        """Write a single character at the current cursor position."""
        if char == '\r':
            self._cursor_col = 0
            return

        if char == '\n':
            if self._cursor_row == self._rows - 1:
                # Add new line to history and scroll
                self._add_new_lines(1)
            else:
                self._cursor_row += 1
            return

        if char == '\b':
            self._cursor_col = max(0, self._cursor_col - 1)
            return

        if char == '\t':
            # Move to next tab stop (every 8 columns)
            spaces = 8 - (self._cursor_col % 8)
            self._cursor_col = min(self._cursor_col + spaces, self._cols - 1)
            return

        # Handle printable characters
        if ord(char) >= 32:
            # Get current line
            print(f"write char {repr(char)} {len(self._lines)} {self._rows}")
            line_index = len(self._lines) - self._rows + self._cursor_row
            if line_index >= 0 and line_index < len(self._lines):
                print(f"update line {line_index} {self._cursor_col}")
                line = self._lines[line_index]

                # Write character
                line.set_character(
                    self._cursor_col,
                    char,
                    self._current_attributes,
                    self._current_fg if self._current_attributes & CharacterAttributes.CUSTOM_FG else None,
                    self._current_bg if self._current_attributes & CharacterAttributes.CUSTOM_BG else None
                )

                # Move cursor
                self._cursor_col += 1
                if self._cursor_col >= self._cols:
                    self._cursor_col = 0
                    if self._cursor_row < self._rows - 1:
                        self._cursor_row += 1
                    else:
                        # Add new line and scroll
                        self._add_new_lines(1)

                # Update the affected area
                self.update()

    def put_data(self, data: bytes) -> None:
        """Display received data with ANSI sequence handling.

        Args:
            data: Raw bytes from terminal

        Raises:
            UnicodeDecodeError: If data cannot be decoded
        """
        if not self._current_size:
            self._current_size = self.calculate_size()

        text = data.decode(errors='replace')

        print(f"put data {repr(text)}")
        i = 0
        while i < len(text):
            char = text[i]

            if self._in_escape_seq:
                self._escape_seq_buffer += char

                # Process escape sequence when complete
                if self._is_escape_sequence_complete(self._escape_seq_buffer):
                    self._process_escape_sequence(self._escape_seq_buffer)
                    self._escape_seq_buffer = ""
                    self._in_escape_seq = False
                elif len(self._escape_seq_buffer) > 128:  # Safety limit
                    self._logger.warning(f"Escape sequence too long, discarding: {repr(self._escape_seq_buffer)}")
                    self._escape_seq_buffer = ""
                    self._in_escape_seq = False

            elif char == '\x1b':  # Start of new escape sequence
                self._in_escape_seq = True
                self._escape_seq_buffer = char

            else:
                self._write_char(char)

            i += 1

    def _is_escape_sequence_complete(self, sequence: str) -> bool:
        """Check if an escape sequence is complete.

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

        # Simple ESC sequences
        return len(sequence) == 2 and sequence[1] in '=>\7\\8cDEHM'

    def _reflow_content(self, old_rows: int, old_cols: int) -> None:
        """Reflow terminal content for new dimensions."""
        # Create new lines with new width
        new_lines = []
        print(f"reflow {old_rows},{old_cols} -> {self._rows},{self._cols}")

        # Copy content from old lines, truncating or padding as needed
        for old_line in self._lines:
            new_line = TerminalLine(self._cols)

            # Copy existing characters
            for col in range(min(old_cols, self._cols)):
                char, attributes, fg_color, bg_color = old_line.get_character(col)
                new_line.set_character(col, char, attributes, fg_color, bg_color)

            # Pad with empty characters if needed
            for col in range(old_cols, self._cols):
                new_line.set_character(col, ' ')

            new_lines.append(new_line)

        # Add additional empty lines if needed
        lines_needed = self._rows - len(new_lines)
        if lines_needed > 0:
            for _ in range(lines_needed):
                new_line = TerminalLine(self._cols)
                for col in range(self._cols):
                    new_line.set_character(col, ' ')

                new_lines.append(new_line)

        self._lines = new_lines

        # Update cursor position if needed
        self._cursor_col = min(self._cursor_col, self._cols - 1)
        self._cursor_row = min(self._cursor_row, self._rows - 1)

        # Ensure scroll region stays within bounds
        if self._scroll_region_bottom > self._rows:
            self._scroll_region_bottom = self._rows
            if self._scroll_region_top >= self._scroll_region_bottom:
                self._scroll_region_top = max(0, self._scroll_region_bottom - 1)

        # Force complete repaint
        self.update()

    def _pixel_pos_to_text_pos(self, pos: QPoint) -> Tuple[int, int]:
        """Convert pixel coordinates to text position."""
        fm = QFontMetrics(self.font())
        char_width = fm.horizontalAdvance(' ')
        char_height = fm.height()

        col = max(0, min(pos.x() // char_width, self._cols - 1))
        row = max(0, min(pos.y() // char_height, self._rows - 1))

        return (row, col)

    def mousePressEvent(self, event: QMouseEvent) -> None:
        """Handle mouse press for selection."""
        if event.button() == Qt.LeftButton:
            self._selecting = True
            pos = self._pixel_pos_to_text_pos(event.position().toPoint())
            self._selection_start = pos
            self._selection_end = pos
            self._update_text_cursor()
            self.update()
        super().mousePressEvent(event)

    def mouseReleaseEvent(self, event: QMouseEvent) -> None:
        """Handle mouse release for selection."""
        if event.button() == Qt.LeftButton:
            self._selecting = False
        super().mouseReleaseEvent(event)

    def mouseMoveEvent(self, event: QMouseEvent) -> None:
        """Handle mouse movement for selection."""
        if self._selecting:
            pos = self._pixel_pos_to_text_pos(event.position().toPoint())
            if pos != self._selection_end:
                self._selection_end = pos
                self._update_text_cursor()
                self.update()
        super().mouseMoveEvent(event)

    def keyPressEvent(self, event: QKeyEvent) -> None:
        """Handle key press events."""
        text = event.text()
        if text:
            self.data_ready.emit(text.encode())
        event.accept()

    def resizeEvent(self, event: QResizeEvent) -> None:
        """Handle resize events."""
        super().resizeEvent(event)
        print("resizeevent")
        self._update_dimensions()

    def _toggle_blink(self):
        """Toggle blink state and update display if needed."""
        old_state = self._blink_state
        self._blink_state = not self._blink_state
        # Only update if we have any blinking characters
        if any(any((line.get_character(col)[1] & CharacterAttributes.BLINK)
                for col in range(self._cols))
                for line in self._lines[-self._rows:]):
            self.update()

    def _draw_character(
        self,
        painter: QPainter,
        x: int,
        y: int,
        char: str,
        attributes: CharacterAttributes,
        fg_color: Optional[int],
        bg_color: Optional[int],
        char_width: int,
        char_height: int,
        fm: QFontMetrics
    ) -> None:
        """Draw a single character cell with attributes."""
        # Handle inverse video by swapping colors
        if attributes & CharacterAttributes.INVERSE:
            if fg_color is not None or bg_color is not None:
                fg_color, bg_color = bg_color, fg_color
            else:
                fg_color, bg_color = self._default_bg, self._default_fg

        # Draw background
        bg = QColor(bg_color) if bg_color is not None and (attributes & CharacterAttributes.CUSTOM_BG) else QColor(self._default_bg)
        painter.fillRect(QRect(x, y, char_width, char_height), bg)

        # Handle hidden text by using background color for foreground
        if attributes & CharacterAttributes.HIDDEN:
            fg_color = bg_color if bg_color is not None else self._default_bg

        # Set up font attributes
        font = painter.font()
        font.setBold(bool(attributes & CharacterAttributes.BOLD))
        font.setItalic(bool(attributes & CharacterAttributes.ITALIC))
        font.setUnderline(bool(attributes & CharacterAttributes.UNDERLINE))
        font.setStrikeOut(bool(attributes & CharacterAttributes.STRIKE))

        # Handle dim text by using alpha channel
        if attributes & CharacterAttributes.DIM:
            fg = QColor(fg_color) if fg_color is not None and (attributes & CharacterAttributes.CUSTOM_FG) else QColor(self._default_fg)
            fg.setAlpha(128)  # 50% opacity
            painter.setPen(fg)
        else:
            fg = QColor(fg_color) if fg_color is not None and (attributes & CharacterAttributes.CUSTOM_FG) else QColor(self._default_fg)
            painter.setPen(fg)

        painter.setFont(font)

        # Draw the character
        # Note: Blinking would be handled by a timer updating the visibility
        if not (attributes & CharacterAttributes.BLINK and self._blink_state):
            painter.drawText(x, y + fm.ascent(), char)

    def paintEvent(self, event: QPaintEvent) -> None:
        """Handle paint events efficiently."""
        print("paint event")
        painter = QPainter(self)

        # Get font metrics for character dimensions
        fm = QFontMetrics(self.font())
        char_width = fm.horizontalAdvance(' ')
        char_height = fm.height()

        # Get the region that needs repainting
        region = event.rect()

        # Calculate the character cell range to repaint
        start_row = max(0, region.top() // char_height)
        end_row = min(self._rows, (region.bottom() + char_height - 1) // char_height)
        start_col = max(0, region.left() // char_width)
        end_col = min(self._cols, (region.right() + char_width - 1) // char_width)

        # Paint visible character cells
        for row in range(start_row, end_row):
            y = row * char_height

            # Get actual line index accounting for scroll position
            line_index = len(self._lines) - self._rows + row
            if line_index < 0 or line_index >= len(self._lines):
                continue

            line = self._lines[line_index]

            for col in range(start_col, end_col):
                x = col * char_width

                # Get character
                char, attributes, fg_color, bg_color = line.get_character(col)
                self._draw_character(painter, x, y, char, attributes, fg_color, bg_color,
                                    char_width, char_height, fm)

        # Draw selection if active
        if self._selection_start and self._selection_end:
            start_row, start_col = self._selection_start
            end_row, end_col = self._selection_end

            # Ensure start is before end
            if (start_row > end_row) or (start_row == end_row and start_col > end_col):
                start_row, start_col, end_row, end_col = end_row, end_col, start_row, start_col

            selection_color = self.palette().highlight().color()
            selection_text_color = self.palette().highlightedText().color()

            for row in range(max(start_row, 0), min(end_row + 1, self._rows)):
                if row < start_row or row > end_row:
                    continue

                y = row * char_height

                # Calculate selection range for this row
                row_start = start_col if row == start_row else 0
                row_end = end_col if row == end_row else self._cols

                # Draw selection background
                selection_rect = QRect(
                    row_start * char_width,
                    y,
                    (row_end - row_start) * char_width,
                    char_height
                )

                if selection_rect.intersects(event.rect()):
                    painter.fillRect(selection_rect, selection_color)

                    # Draw selected text
                    line_index = len(self._lines) - self._rows + row
                    if 0 <= line_index < len(self._lines):
                        line = self._lines[line_index]
                        for col in range(row_start, row_end):
                            x = col * char_width
                            char, attributes, _fg_color, _bg_color = line.get_character(col)

                            # Set up font attributes
                            font = painter.font()
                            if attributes & CharacterAttributes.BOLD:
                                font.setBold(True)
                            if attributes & CharacterAttributes.ITALIC:
                                font.setItalic(True)
                            if attributes & CharacterAttributes.UNDERLINE:
                                font.setUnderline(True)

                            painter.setFont(font)

                            # Draw with selection colors
                            painter.setPen(selection_text_color)
                            painter.drawText(x, y + fm.ascent(), char)

        # Draw cursor if visible
        if self._cursor_visible and self._viewport_offset == 0:
            cursor_x = self._cursor_col * char_width
            cursor_y = self._cursor_row * char_height

            if QRect(cursor_x, cursor_y, char_width, char_height).intersects(region):
                # Get character under cursor for inversion
                line_index = len(self._lines) - self._rows + self._cursor_row
                if 0 <= line_index < len(self._lines):
                    line = self._lines[line_index]
                    char, _attributes, _fg_color, _bg_color = line.get_character(self._cursor_col)

                    # Draw inverted cursor block
                    painter.fillRect(
                        cursor_x, cursor_y, char_width, char_height,
                        self.palette().text().color()
                    )

                    # Draw character in inverted colors
                    painter.setPen(self.palette().base().color())
                    painter.drawText(
                        cursor_x,
                        cursor_y + fm.ascent(),
                        char
                    )

    def _update_text_cursor(self) -> None:
        """Update text cursor based on selection state."""
        if not self._selection_start or not self._selection_end:
            self._text_cursor = QTextCursor()
            return

        # Create a cursor that represents the selection
        cursor = QTextCursor()
        cursor.setPosition(0)  # Start position

        # Calculate selection range
        start_row, start_col = self._selection_start
        end_row, end_col = self._selection_end

        # Ensure start is before end
        if (start_row > end_row) or (start_row == end_row and start_col > end_col):
            start_row, start_col, end_row, end_col = end_row, end_col, start_row, start_col

        cursor.setPosition(start_row * self._cols + start_col)
        cursor.setPosition(end_row * self._cols + end_col, QTextCursor.KeepAnchor)

        self._text_cursor = cursor

    def _get_selected_text(self) -> str:
        """Get currently selected text."""
        if not self._selection_start or not self._selection_end:
            return ""

        # Calculate selection range
        start_row, start_col = self._selection_start
        end_row, end_col = self._selection_end

        # Ensure start is before end
        if (start_row > end_row) or (start_row == end_row and start_col > end_col):
            start_row, start_col, end_row, end_col = end_row, end_col, start_row, start_col

        # Build selected text
        text = []
        for row in range(start_row, end_row + 1):
            line_index = len(self._lines) - self._rows + row
            if 0 <= line_index < len(self._lines):
                line = self._lines[line_index]

                start = start_col if row == start_row else 0
                end = end_col if row == end_row else self._cols

                row_text = ""
                for col in range(start, end):
                    char, _attributes, _fg_color, _bg_color = line.get_character(col)
                    row_text += char

                text.append(row_text.rstrip())  # Remove trailing spaces

        return "\n".join(text)

    def _clear_selection(self) -> None:
        """Clear current selection."""
        self._selection_start = None
        self._selection_end = None
        self._update_text_cursor()
        self.update()

    # Interface methods required by TerminalTab
    def textCursor(self) -> QTextCursor:
        """Return current text cursor (for compatibility with QPlainTextEdit)."""
        return self._text_cursor

    def setTextCursor(self, cursor: QTextCursor) -> None:
        """Set text cursor (for compatibility with QPlainTextEdit)."""
        self._text_cursor = cursor
        self.update()

    def cut(self) -> None:
        """Cut selected text to clipboard."""
        self.copy()
        self._clear_selection()

    def copy(self) -> None:
        """Copy selected text to clipboard."""
        if not self._selection_start or not self._selection_end:
            return

        # Get selected text
        text = self._get_selected_text()
        if text:
            QGuiApplication.clipboard().setText(text)

    def paste(self) -> None:
        """Paste text from clipboard."""
        text = QGuiApplication.clipboard().text()
        if text:
            self.put_data(text.encode())

    def scroll_to(self, position: int) -> None:
        """
        Scroll terminal to specified position.

        Args:
            position: Number of lines to scroll up from bottom
        """
        old_offset = self._viewport_offset
        self._viewport_offset = min(
            max(0, position),
            max(0, len(self._lines) - self._rows)
        )

        if self._viewport_offset != old_offset:
            self.update()

    def clear(self) -> None:
        """Clear the terminal."""
        self._lines = []
        self._add_new_lines(self._rows)
        self._cursor_row = 0
        self._cursor_col = 0
        self._clear_selection()
        self.update()
