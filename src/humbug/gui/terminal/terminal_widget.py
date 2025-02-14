"""Terminal widget implementation."""

import array
import base64
from dataclasses import dataclass
from enum import Flag, auto
import logging
import struct
from typing import List, Optional, Tuple

from PySide6.QtWidgets import QWidget, QAbstractScrollArea, QMenu
from PySide6.QtCore import Qt, Signal, QRect, QPoint, QTimer
from PySide6.QtGui import (
    QPainter, QPaintEvent, QColor, QFontMetrics, QFont,
    QResizeEvent, QKeyEvent, QMouseEvent,
    QGuiApplication, QWheelEvent
)

from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager
from humbug.language.language_manager import LanguageManager


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


@dataclass
class TerminalSelection:
    """Represents a selection in the terminal."""
    start_row: int
    start_col: int
    end_row: int
    end_col: int

    def is_empty(self) -> bool:
        """Check if selection is empty."""
        return (
            self.start_row == self.end_row and
            self.start_col == self.end_col
        )

    def normalize(self) -> 'TerminalSelection':
        """Return normalized selection (start before end)."""
        if (
            (self.start_row > self.end_row) or
            (self.start_row == self.end_row and self.start_col > self.end_col)
        ):
            return TerminalSelection(
                self.end_row,
                self.end_col,
                self.start_row,
                self.start_col
            )

        return self


class TerminalWidget(QAbstractScrollArea):
    """Terminal widget implementation."""

    data_ready = Signal(bytes)  # Emitted when user input is ready
    size_changed = Signal()  # Emitted when terminal size changes

    def __init__(self, parent: Optional[QWidget] = None):
        """Initialize terminal widget."""
        super().__init__(parent)
        self._logger = logging.getLogger("TerminalWidget")
        self._style_manager = StyleManager()

        # Initialize language manager for localization
        self._language_manager = LanguageManager()

        # Set up scrollbar behavior
        self.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOn)
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)

        # Connect scrollbar signals
        self.verticalScrollBar().valueChanged.connect(self._handle_scroll)

        # Enable focus and input
        self.setFocusPolicy(Qt.StrongFocus)

        # Storage for terminal content
        self._lines: List[TerminalLine] = []

        # Terminal dimensions
        self._rows = 0
        self._cols = 0

        # Scroll region tracking.  Note: the bottom row is actually the first row that is outside
        # the scroll region.
        self._scroll_region_top = 0
        self._scroll_region_bottom = self._rows
        self._scroll_region_rows = self._rows

        # Origin mode row offset
        self._origin_mode = False

        self._using_alternate_screen = False
        self._main_screen_buffer = None
        self._main_screen_cursor = None
        self._main_screen_attrs = None

        # Window/terminal state
        self._terminal_title = ""
        self._current_directory = None

        # Operation modes
        self._application_keypad_mode = False
        self._application_cursor_keys = False
        self._auto_wrap = True
        self._bracketed_paste = False

        # Mouse handling
        self._mouse_tracking = False
        self._mouse_tracking_mode = 0  # 0=off, 1000=normal, 1002=button, 1003=any
        self._mouse_utf8_mode = False
        self._mouse_sgr_mode = False

        # Focus tracking
        self._focus_tracking = False

        # Cursor state
        self._cursor_row = 0
        self._cursor_col = 0
        self._cursor_delayed_wrap = False
        self._cursor_visible = True
        self._cursor_blink = True
        self._saved_cursor = None  # For save/restore cursor position

        # Selection state
        self._selection: Optional[TerminalSelection] = None
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

        # Set context menu policy
        self.setContextMenuPolicy(Qt.CustomContextMenu)
        self.customContextMenuRequested.connect(self._show_terminal_context_menu)

        # Connect style changed signal
        self._style_manager.style_changed.connect(self._handle_style_changed)
        self._handle_style_changed()

    def focusNextPrevChild(self, _next: bool) -> bool:
        """Override to prevent tab from changing focus."""
        return False

    def _show_terminal_context_menu(self, pos) -> None:
        """
        Show a context menu for the terminal widget.

        Args:
            pos: Position in widget coordinates
        """
        # Create context menu
        menu = QMenu(self)

        # Standard edit actions
        copy_action = menu.addAction(self._language_manager.strings.copy)
        copy_action.setEnabled(self.has_selection())
        copy_action.triggered.connect(self.copy)

        paste_action = menu.addAction(self._language_manager.strings.paste)
        paste_action.setEnabled(True)
        paste_action.triggered.connect(self.paste)

        # Show menu at the global position
        menu.exec_(self.mapToGlobal(pos))

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
        self.viewport().update()

    def calculate_size(self) -> TerminalSize:
        """Calculate current terminal size in rows and columns."""
        fm = QFontMetrics(self.font())
        char_width = fm.horizontalAdvance(' ')
        char_height = fm.height()

        if char_width <= 0 or char_height <= 0:
            self._logger.warning(f"Invalid character dimensions: width={char_width}, height={char_height}")
            return TerminalSize(24, 80)  # Default fallback size

        # Get the width of the vertical scrollbar
        scrollbar_width = self.verticalScrollBar().width()

        # Calculate available viewport width, subtracting scrollbar width
        viewport_width = max(0, self.width() - scrollbar_width)
        viewport_height = self.height()

        cols = max(viewport_width // char_width, 1)
        rows = max(viewport_height // char_height, 1)
        print(f"size: {rows}x{cols}")

        return TerminalSize(rows, cols)

    def _update_dimensions(self) -> None:
        """Update terminal dimensions based on widget size and font metrics."""
        new_size = self.calculate_size()

        if new_size.cols != self._cols or new_size.rows != self._rows:
            old_rows = self._rows
            old_cols = self._cols
            self._rows = new_size.rows
            self._cols = new_size.cols

            self._scroll_region_bottom = min(
                self._scroll_region_bottom + self._rows - old_rows, self._rows
            )
            print(f"update bottom {self._scroll_region_bottom} {old_rows} {new_size.rows}")

            # Ensure minimum scroll region size of 2 lines
            if self._scroll_region_bottom < self._scroll_region_top + 1:
                self._scroll_region_bottom = min(
                    self._scroll_region_top + 2,
                    self._rows
                )

            self._scroll_region_rows = self._scroll_region_bottom - self._scroll_region_top

            # Calculate scroll region adjustments
            if old_rows > 0 and old_cols > 0:
                self._reflow_content(old_rows, old_cols)

            self.size_changed.emit()

    def _initialize_buffer(self) -> None:
        """Initialize empty terminal buffer."""
        # Create initial lines
        self._lines = []
        self._add_new_lines(self._rows)

    def _handle_scroll(self, value: int):
        """Handle scrollbar value changes."""
        self.viewport().update()

    def _update_scrollbar(self):
        """Update scrollbar range based on content size."""
        history_lines = max(0, len(self._lines) - self._rows)

        # Set range and update scroll position if needed
        vbar = self.verticalScrollBar()
        old_at_bottom = vbar.value() == vbar.maximum()
        vbar.setRange(0, history_lines)

        # If we were at bottom before, stay at bottom
        if old_at_bottom:
            vbar.setValue(vbar.maximum())

    def _get_new_line(self) -> TerminalLine:
        line = TerminalLine(self._cols)
        # Fill line with spaces using default attributes

        for i in range(self._cols):
            line.set_character(i, ' ')

        return line

    def _add_new_lines(self, count: int) -> None:
        """Add new empty lines to the buffer."""
        for _ in range(count):
            self._lines.append(self._get_new_line())

        # Update scrollbar range
        self._update_scrollbar()

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
                    if clipboard == 'c':
                        QGuiApplication.clipboard().setText(decoded)
                else:  # Clear
                    if clipboard == 'c':
                        QGuiApplication.clipboard().clear()
        except (ValueError, TypeError) as e:
            self._logger.warning(f"Invalid selection data: {param}: {e}")

    def _process_osc(self, sequence: str):
        """Handle Operating System Command sequences."""
        print(f"OSC: {repr(sequence)}")

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
#                self.window_title_changed.emit(param)
                pass

            elif command == 1:  # Set icon name only
#                self.icon_name_changed.emit(param)
                pass

            elif command == 2:  # Set window title only
#                self.window_title_changed.emit(param)
                pass

            elif command == 4:  # Change/query color number
                # Format: 4;index;spec
                color_parts = param.split(';')
                if len(color_parts) >= 2:
                    index = int(color_parts[0])
                    if len(color_parts) == 1:  # Query
                        # Should emit response with current color
                        pass
                    else:  # Set
                        color_spec = color_parts[1]
                        # Parse color spec and update color table
#                        self._update_color(index, color_spec)

            elif command == 7:  # Set working directory
                self._current_directory = param

            elif command in (10, 11, 12, 13, 14, 15, 16, 17, 19):  # Set/query various resources
                # 10: text foreground
                # 11: text background
                # etc.
                if param == '?':  # Query
                    # Should emit response with current value
                    pass
                else:
                    self._update_terminal_resource(command, param)

            elif command == 52:  # Manipulate selection data
                # Handle clipboard operations
                self._handle_selection_data(param)

            elif command == 104:  # Reset color number
                if param:  # Reset specific colors
                    for spec in param.split(';'):
                        try:
                            index = int(spec)
#                            self._reset_color(index)
                        except ValueError:
                            pass
                else:  # Reset all colors
#                    self._reset_all_colors()
                    pass

            elif command == 110:  # Reset text foreground color
#                self._reset_color(10)
                pass

            elif command == 111:  # Reset text background color
#                self._reset_color(11)
                pass

            elif command == 112:  # Reset text cursor color
#                self._reset_color(12)
                pass

        except (ValueError, IndexError) as e:
            self._logger.warning(f"Invalid OSC sequence: {sequence}, error: {e}")

    def _handle_alternate_screen(self, enable: bool):
        """Switch between main and alternate screen buffers."""
        if enable == self._using_alternate_screen:
            return

        self._clear_selection()

        if enable:
            # Save main screen state
            self._main_screen_buffer = self._lines[:]
            self._main_screen_cursor = (self._cursor_row, self._cursor_col)
            self._main_screen_attrs = {
                'fg': self._current_fg,
                'bg': self._current_bg,
                'attrs': self._current_attributes,
                'scroll_value': self.verticalScrollBar().value()
            }
            # TODO: add support for saving the origin mode settings

            # Clear and initialize alternate screen
            self._lines = []
            self._add_new_lines(self._rows)
            self._cursor_row = 0
            self._cursor_col = 0
            self._cursor_delayed_wrap = False

        else:
            # Restore main screen
            self._lines = self._main_screen_buffer
            self._cursor_row, self._cursor_col = self._main_screen_cursor
            self._current_fg = self._main_screen_attrs['fg']
            self._current_bg = self._main_screen_attrs['bg']
            self._current_attributes = self._main_screen_attrs['attrs']

            # Update scrollbar for main screen
            self._update_scrollbar()

            self.verticalScrollBar().setValue(self._main_screen_attrs['scroll_value'])

        self._using_alternate_screen = enable
        self.viewport().update()

    def _process_private_mode(self, params: str, set_mode: bool):
        """Handle DEC private mode sequences (DECSET/DECRST)."""
        try:
            modes = [int(x) for x in params.split(';')]
            for mode in modes:
                if mode == 1:  # DECCKM - Application Cursor Keys
                    self._application_cursor_keys = set_mode
                elif mode == 3:  # DECCOLM - 80/132 Column Mode
                    # In xterm this changes column count but for now we'll ignore
                    self.clear()
                    self._scroll_region_top = 0
                    self._scroll_region_bottom = self._rows
                    self._scroll_region_rows = self._rows
                elif mode == 6:  # DECOM - Origin Mode
                    self._origin_mode = set_mode
                    self._cursor_row = 0
                    self._cursor_col = 0
                    self._cursor_delayed_wrap = False
                elif mode == 7:  # DECAWM - Auto-wrap Mode
                    self._auto_wrap = set_mode
                elif mode == 12:  # att610 - Start/Stop Blinking Cursor
                    self._cursor_blink = set_mode
                elif mode == 25:  # DECTCEM - Text Cursor Enable Mode
                    self._cursor_visible = set_mode
                elif mode == 1000:  # X11 mouse reporting - normal tracking mode
                    self._mouse_tracking = set_mode
                    self._mouse_tracking_mode = 1000 if set_mode else 0
                elif mode == 1002:  # X11 mouse reporting - button event tracking
                    self._mouse_tracking = set_mode
                    self._mouse_tracking_mode = 1002 if set_mode else 0
                elif mode == 1003:  # X11 mouse reporting - any event tracking
                    self._mouse_tracking = set_mode
                    self._mouse_tracking_mode = 1003 if set_mode else 0
                elif mode == 1004:  # Send focus in/out events
                    self._focus_tracking = set_mode
                elif mode == 1005:  # UTF-8 mouse mode
                    self._mouse_utf8_mode = set_mode
                elif mode == 1006:  # SGR mouse mode
                    self._mouse_sgr_mode = set_mode
                elif mode == 1047:  # Use Alternate Screen Buffer
                    self._handle_alternate_screen(set_mode)
                elif mode == 1048:  # Save/Restore cursor
                    if set_mode:
#                        self._save_cursor()
                        pass
                    else:
#                        self._restore_cursor()
                        pass
                elif mode == 1049:  # Alternate Screen + save/restore cursor
                    if set_mode:
#                        self._save_cursor()
                        self._handle_alternate_screen(True)
                    else:
                        self._handle_alternate_screen(False)
#                        self._restore_cursor()
                elif mode == 2004:  # Bracketed paste mode
                    self._bracketed_paste = set_mode
        except ValueError as e:
            self._logger.warning(f"Invalid private mode parameter: {params}")

    def _process_sgr(self, params: list[int]) -> None:
        """Process SGR (Select Graphic Rendition) sequence."""
        print(f"SGR {params}")
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

            else:
                print(f"Unknown SGR sequence {params}")
                self._logger.warning(f"Unknown SGR sequence {params}")

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

    def _process_csi(self, sequence: str) -> None:
        code = sequence[-1]
        print(f"CSI code {code}: {repr(sequence)}")

        # Parse just what we need based on the sequence
        if code == 'A':  # Up
            param = sequence[2:-1]
            amount = max(1, int(param)) if param.isdigit() else 1
            self._cursor_row = max(0, self._cursor_row - amount)
            self._cursor_delayed_wrap = False
            print(f"csr at {self._cursor_row},{self._cursor_col} {amount}")

        elif code == 'B':  # Down
            param = sequence[2:-1]
            amount = max(1, int(param)) if param.isdigit() else 1
            max_rows = self._rows if not self._origin_mode else self._scroll_region_rows
            self._cursor_row = min(max_rows - 1, self._cursor_row + amount)
            self._cursor_delayed_wrap = False
            print(f"csr at {self._cursor_row},{self._cursor_col} {amount}")

        elif code == 'C':  # Forward
            param = sequence[2:-1]
            amount = max(1, int(param)) if param.isdigit() else 1
            self._cursor_col = min(self._cols - 1, self._cursor_col + amount)
            self._cursor_delayed_wrap = False
            print(f"csr at {self._cursor_row},{self._cursor_col} {amount}")

        elif code == 'D':  # Backward
            param = sequence[2:-1]
            amount = max(1, int(param)) if param.isdigit() else 1
            self._cursor_col = max(0, self._cursor_col - amount)
            self._cursor_delayed_wrap = False
            print(f"csr at {self._cursor_row},{self._cursor_col} {amount}")

        elif code == 'G':  # CHA - Cursor Horizontal Absolute
            param = sequence[2:-1]
            col = max(0, int(param) - 1) if param.isdigit() else 1  # Convert 1-based to 0-based
            self._cursor_col = min(col, self._cols - 1)
            self._cursor_delayed_wrap = False

        elif code == 'H':  # Cursor position
            pos = sequence[2:-1].split(';')
            row = int(pos[0]) if pos and pos[0].isdigit() else 1
            col = int(pos[1]) if len(pos) > 1 and pos[1].isdigit() else 1
            max_rows = self._rows if not self._origin_mode else self._scroll_region_rows
            self._cursor_row = min(max_rows - 1, max(0, row - 1))  # Convert to 0-based
            self._cursor_col = min(self._cols - 1, max(0, col - 1))
            self._cursor_delayed_wrap = False

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
            max_rows = self._rows if not self._origin_mode else self._scroll_region_rows
            self._cursor_row = min(row, max_rows - 1)
            self._cursor_delayed_wrap = False

        elif code == 'f':  # HVP - Horizontal & Vertical Position
            pos = sequence[2:-1].split(';')
            row = int(pos[0]) if pos and pos[0].isdigit() else 1
            col = int(pos[1]) if len(pos) > 1 and pos[1].isdigit() else 1
            max_rows = self._rows if not self._origin_mode else self._scroll_region_rows
            self._cursor_row = min(max_rows - 1, max(0, row - 1))  # Convert to 0-based
            self._cursor_col = min(self._cols - 1, max(0, col - 1))
            self._cursor_delayed_wrap = False

        elif code == 'm':  # SGR - Select Graphic Rendition
            params = sequence[2:-1].split(';')
            params = [int(p) if p.isdigit() else 0 for p in params]
            self._process_sgr(params)

        elif code == 'r':  # DECSTBM - Set Scrolling Region
            params = sequence[2:-1].split(';')
            top = max(0, int(params[0]) - 1) if params and params[0].isdigit() else 0
            bottom = min(self._rows, int(params[1])) if len(params) > 1 and params[1].isdigit() else self._rows
            if top < bottom:
                self._scroll_region_top = top
                self._scroll_region_bottom = bottom
                self._scroll_region_rows = bottom - top
                print(f"STBM set bottom {self._scroll_region_bottom}")
                # TODO: we need to think about origin mode

        elif code == 's':  # Save cursor position
            self._saved_cursor = (self._cursor_row, self._cursor_col)

        elif code == 'u':  # Restore cursor position
            if self._saved_cursor:
                self._cursor_row, self._cursor_col = self._saved_cursor

        else:
            print(f"Unknown CSI sequence {repr(sequence)}")
            self._logger.warning(f"Unknown CSI sequence {repr(sequence)}")

    def _process_dec_special(self, sequence: str) -> None:
        code = sequence[-1]
        print(f"DEC special code {code}: {repr(sequence)}")

        if code == '8':
            for r in range(self._rows):
                ln = len(self._lines) - self._rows + r
                line = self._lines[ln]
                for c in range(self._cols):
                    line.set_character(c, 'E')

        else:
            print(f"Unknown CSI sequence {repr(sequence)}")
            self._logger.warning(f"Unknown CSI sequence {repr(sequence)}")

    def _process_escape_sequence(self, sequence: str) -> None:
        """Process ANSI escape sequence."""
        # OSC sequences
        if sequence.startswith('\x1b]'):
            self._process_osc(sequence)
            return

        # Private mode sequences
        if sequence.startswith('\x1b[?'):
            code = sequence[-1]
            print(f"PM code {code}: {repr(sequence)}")
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
            print(f"ESC code {code}: {repr(sequence)}")
            if code == 'D':  # Index
                cursor_row = self._cursor_row if not self._origin_mode else self._cursor_row + self._scroll_region_top
                if cursor_row != self._scroll_region_bottom - 1:
                    max_rows = self._rows if not self._origin_mode else self._scroll_region_rows
                    self._cursor_row = min(self._cursor_row + 1, max_rows - 1)
                else:
                    self._scroll_up(1)

                self._cursor_delayed_wrap = False
            elif code == 'E':  # Next Line
                self._cursor_col = 0
                self._cursor_delayed_wrap = False
                cursor_row = self._cursor_row if not self._origin_mode else self._cursor_row + self._scroll_region_top
                if cursor_row != self._scroll_region_bottom - 1:
                    max_rows = self._rows if not self._origin_mode else self._scroll_region_rows
                    self._cursor_row = min(self._cursor_row + 1, max_rows - 1)
                else:
                    self._scroll_up(1)

                self._cursor_delayed_wrap = False

            elif code == 'M':  # Reverse Index
                cursor_row = self._cursor_row if not self._origin_mode else self._cursor_row + self._scroll_region_top
                if cursor_row != self._scroll_region_top:
                    self._cursor_row = max(0, self._cursor_row - 1)
                else:
                    self._scroll_down(1)

                self._cursor_delayed_wrap = False

            print(f"Unknown simple ESC sequence: {repr(sequence)}")
            self._logger.warning(f"Unknown simple ESC sequence: {repr(sequence)}")
            return

        print(f"Unknown ESC sequence: {repr(sequence)}")
        self._logger.warning(f"Unknown ESC sequence: {repr(sequence)}")

    def _insert_lines(self, count: int) -> None:
        """Insert blank lines at cursor position."""
        cursor_row = self._cursor_row if not self._origin_mode else self._cursor_row + self._scroll_region_top
        if not (self._scroll_region_top <= cursor_row < self._scroll_region_bottom):
            return

        # Calculate lines to move
        start = len(self._lines) - self._rows + cursor_row
        end = len(self._lines) - self._rows + self._scroll_region_bottom

        # Clip the count
        count = min(count, end - start)

        # Insert blank lines at the cursor and delete them at the end of the scrolling region
        for _ in range(count):
            self._lines.insert(start, self._get_new_line())
            del self._lines[end]

        self._cursor_col = 0
        self._cursor_delayed_wrap = False

    def _delete_lines(self, count: int) -> None:
        """Delete lines at cursor position."""
        cursor_row = self._cursor_row if not self._origin_mode else self._cursor_row + self._scroll_region_top
        if not (self._scroll_region_top <= cursor_row < self._scroll_region_bottom):
            return

        # Calculate lines to remove
        start = len(self._lines) - self._rows + cursor_row
        end = len(self._lines) - self._rows + self._scroll_region_bottom

        # Clip the count
        count = min(count, end - start)

        # Insert blank lines at the end of the scrolling region and remove them at the cursor
        for _ in range(count):
            self._lines.insert(end, self._get_new_line())
            del self._lines[start]

        self._cursor_col = 0
        self._cursor_delayed_wrap = False

    def _insert_chars(self, count: int) -> None:
        """Insert blank characters at cursor position."""
        cursor_row = self._cursor_row if not self._origin_mode else self._cursor_row + self._scroll_region_top
        line_index = len(self._lines) - self._rows + cursor_row
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
        cursor_row = self._cursor_row if not self._origin_mode else self._cursor_row + self._scroll_region_top
        line_index = len(self._lines) - self._rows + cursor_row
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
        cursor_row = self._cursor_row if not self._origin_mode else self._cursor_row + self._scroll_region_top
        line_index = len(self._lines) - self._rows + cursor_row
        if 0 <= line_index < len(self._lines):
            line = self._lines[line_index]
            for col in range(self._cursor_col, min(self._cursor_col + count, self._cols)):
                line.set_character(col, ' ')

    def _scroll_up(self, count: int) -> None:
        """Scroll up within current scroll region."""
        # Calculate actual lines in scroll region
        start = len(self._lines) - self._rows + self._scroll_region_top
        end = len(self._lines) - self._rows + self._scroll_region_bottom

        # Insert blank lines at the bottom of the scrolling region and remove lines from the top
        for _ in range(count):
            self._lines.insert(end, self._get_new_line())

            # If we're using the main screen and the scrolling region top is the top of the screen
            # the we don't actually delete anything, we simply let the scrolled line roll into
            # the history buffer
            if self._using_alternate_screen or self._scroll_region_top != 0:
                scrolled_line = self._lines.pop(start)
                if not self._using_alternate_screen:
                    self._lines.insert(len(self._lines) - self._rows, scrolled_line)

        # If we made our history longer then we need to update the scrollbar position
        if not self._using_alternate_screen:
            self._update_scrollbar()

    def _scroll_down(self, count: int) -> None:
        """Scroll down within current scroll region."""
        # Calculate actual lines in scroll region
        start = len(self._lines) - self._rows + self._scroll_region_top
        end = len(self._lines) - self._rows + self._scroll_region_bottom

        # Insert blank lines at the top of the scrolling region and remove lines from the bottom
        for _ in range(count):
            self._lines.insert(start, self._get_new_line())
            del self._lines[end]

    def _write_char(self, char: str) -> None:
        """Write a single character at the current cursor position."""
        cursor_row = self._cursor_row if not self._origin_mode else self._cursor_row + self._scroll_region_top
        max_rows = self._rows if not self._origin_mode else self._scroll_region_rows

        print(f"write char: {repr(char)}")

        if char == '\r':
            self._cursor_col = 0
            self._cursor_delayed_wrap = False
            return

        if char in '\n\f\v':
            if cursor_row != self._scroll_region_bottom - 1:
                self._cursor_row = min(self._cursor_row + 1, max_rows - 1)
            else:
                self._scroll_up(1)

            self._cursor_delayed_wrap = False
            return

        if char == '\b':
            self._cursor_col = max(0, self._cursor_col - 1)
            self._cursor_delayed_wrap = False
            print(f"new col: {self._cursor_col}")
            return

        # From this point on we're dealing with "printable" characters and we should now handle
        # delayed wrapping
        if self._cursor_delayed_wrap:
            self._cursor_col = 0
            self._cursor_delayed_wrap = False
            if cursor_row != self._scroll_region_bottom - 1:
                self._cursor_row = min(self._cursor_row + 1, max_rows - 1)
                cursor_row += 1
            else:
                self._scroll_up(1)

        if char == '\t':
            # Move to next tab stop (every 8 columns)
            spaces = 8 - (self._cursor_col % 8)
            self._cursor_col = min(self._cursor_col + spaces, self._cols - 1)
            return

        # Handle printable characters
        if ord(char) >= 32:
            line_index = len(self._lines) - self._rows + cursor_row
            line = self._lines[line_index]

            print(f"wc: {self._cursor_row},{self._cursor_col}: {char}")
            # Write character
            line.set_character(
                self._cursor_col,
                char,
                self._current_attributes,
                self._current_fg if self._current_attributes & CharacterAttributes.CUSTOM_FG else None,
                self._current_bg if self._current_attributes & CharacterAttributes.CUSTOM_BG else None
            )

            # Move cursor, but handle the special case of writing in the last column.  If we write in the
            # last column we don't move the cursor but set a "delayed wrap" flag that will trigger a
            # wrap around on the next printable character being written to the terminal.
            if self._cursor_col == self._cols - 1:
                self._cursor_delayed_wrap = True
            else:
                self._cursor_col += 1

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
            i += 1

            if self._in_escape_seq:
                # Is this character a control character?  If yes, then we have to process it immediately
                # Weird eh?  But this is what a slow serial terminal needed and tools like vttest check
                # for this!
                if char in '\r\n\b\f\t\v':
                    self._write_char(char)
                    continue

                # If we find another escape character then this suggests we've just seen something we
                # didn't understand.  We'll log it and move on
                if char == '\x1b':
                    self._logger.warning(f"Unknown escape sequence - discarding: {repr(self._escape_seq_buffer)}")
                    print(f"Unknown escape sequence - discarding: {repr(self._escape_seq_buffer)}")
                    self._escape_seq_buffer = ""

                self._escape_seq_buffer += char

                # Process escape sequence when complete
                if self._is_escape_sequence_complete(self._escape_seq_buffer):
                    self._process_escape_sequence(self._escape_seq_buffer)
                    self._escape_seq_buffer = ""
                    self._in_escape_seq = False
                elif len(self._escape_seq_buffer) > 128:  # Safety limit
                    self._logger.warning(f"Escape sequence too long, discarding: {repr(self._escape_seq_buffer)}")
                    print(f"Escape sequence too long, discarding: {repr(self._escape_seq_buffer)}")
                    self._escape_seq_buffer = ""
                    self._in_escape_seq = False

            elif char == '\x1b':  # Start of new escape sequence
                self._in_escape_seq = True
                self._escape_seq_buffer = char

            else:
                self._write_char(char)

        # Update the affected area
        self.viewport().update()

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
        return len(sequence) == 2 and sequence[1] in '=>\7\\8cDEHM'

    def _reflow_content(self, old_rows: int, old_cols: int) -> None:
        """Reflow terminal content for new dimensions."""
        # Create new lines with new width
        new_lines = []

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
                new_lines.append(self._get_new_line())

        self._lines = new_lines

        # Update cursor position if needed
        self._cursor_col = min(self._cursor_col, self._cols - 1)
        self._cursor_row = min(self._cursor_row, self._rows - 1)

        # Update scrollbar after reflowing content
        self._update_scrollbar()

        # Force complete repaint
        self.viewport().update()

    def _pixel_pos_to_text_pos(self, pos: QPoint) -> Tuple[int, int]:
        """Convert pixel coordinates to text position.

        Args:
            pos: Mouse position in viewport coordinates

        Returns:
            Tuple of (row, col) in terminal buffer coordinates
        """
        fm = QFontMetrics(self.font())
        char_width = fm.horizontalAdvance(' ')
        char_height = fm.height()

        # Convert pixel position to viewport row/col
        viewport_col = max(0, min(pos.x() // char_width, self._cols - 1))
        viewport_row = max(0, min(pos.y() // char_height, self._rows - 1))

        # Adjust row for scroll position
        first_visible_line = self.verticalScrollBar().value()
        buffer_row = viewport_row + first_visible_line

        return (buffer_row, viewport_col)

    def _make_sgr_mouse_report(self, row: int, col: int, button: Qt.MouseButton, pressed: bool) -> str:
        """Create an SGR mouse report."""
        btn_num = {
            Qt.LeftButton: 0,
            Qt.MiddleButton: 1,
            Qt.RightButton: 2
        }.get(button, 3)

        if not pressed:
            btn_num += 3

        return f"\x1b[<{btn_num};{col + 1};{row + 1}{'M' if pressed else 'm'}"

    def _make_normal_mouse_report(self, row: int, col: int, button: Qt.MouseButton) -> str:
        """Create a normal X10/X11 mouse report."""
        btn_num = {
            Qt.LeftButton: 0,
            Qt.MiddleButton: 1,
            Qt.RightButton: 2
        }.get(button, 3)

        # Ensure values fit in a byte
        cb = 32 + btn_num
        cx = 32 + min(255, col + 1)
        cy = 32 + min(255, row + 1)

        return f"\x1b[M{chr(cb)}{chr(cx)}{chr(cy)}"

    def mousePressEvent(self, event: QMouseEvent) -> None:
        """Handle mouse press for both tracking and selection."""
        if event.button() == Qt.LeftButton:
            # Handle text selection
            self._selecting = True
            pos = self._pixel_pos_to_text_pos(event.position().toPoint())
            self._selection = TerminalSelection(pos[0], pos[1], pos[0], pos[1])
            self.viewport().update()

        # Handle mouse tracking if enabled
        if self._mouse_tracking:
            pos = event.position().toPoint()
            button = event.button()
            row, col = self._pixel_pos_to_text_pos(pos)

            # Construct mouse report based on mode
            if self._mouse_sgr_mode:
                report = self._make_sgr_mouse_report(row, col, button, True)
            else:
                report = self._make_normal_mouse_report(row, col, button)

            if report:
                self.data_ready.emit(report.encode())

        super().mousePressEvent(event)

    def mouseReleaseEvent(self, event: QMouseEvent) -> None:
        """Handle mouse release for both tracking and selection."""
        if event.button() == Qt.LeftButton:
            self._selecting = False

        # Handle mouse tracking if enabled
        if self._mouse_tracking:
            pos = event.position().toPoint()
            button = event.button()
            row, col = self._pixel_pos_to_text_pos(pos)

            if self._mouse_sgr_mode:
                report = self._make_sgr_mouse_report(row, col, button, False)
                if report:
                    self.data_ready.emit(report.encode())

        super().mouseReleaseEvent(event)

    def mouseMoveEvent(self, event: QMouseEvent) -> None:
        """Handle mouse movement for selection and tracking."""
        # Handle text selection
        if self._selecting and self._selection is not None:
            pos = self._pixel_pos_to_text_pos(event.position().toPoint())
            if (pos[0] != self._selection.end_row or
                pos[1] != self._selection.end_col):
                self._selection.end_row = pos[0]
                self._selection.end_col = pos[1]
                self.viewport().update()

        # Handle mouse tracking if enabled and in button event mode (1002) or any event mode (1003)
        if self._mouse_tracking and self._mouse_tracking_mode in (1002, 1003):
            row, col = self._pixel_pos_to_text_pos(event.position().toPoint())
            buttons = event.buttons()

            # For 1002 mode, only report if buttons are pressed
            if self._mouse_tracking_mode == 1002 and not buttons:
                return

            btn_num = 32  # Default to button release
            if buttons & Qt.LeftButton:
                btn_num = 32
            elif buttons & Qt.MiddleButton:
                btn_num = 33
            elif buttons & Qt.RightButton:
                btn_num = 34

            if self._mouse_sgr_mode:
                report = f"\x1b[<{btn_num};{col + 1};{row + 1}M"
            else:
                cb = 32 + btn_num
                cx = 32 + min(255, col + 1)
                cy = 32 + min(255, row + 1)
                report = f"\x1b[M{chr(cb)}{chr(cx)}{chr(cy)}"

            self.data_ready.emit(report.encode())

        super().mouseMoveEvent(event)

    def wheelEvent(self, event: QWheelEvent) -> None:
        """Handle mouse wheel scrolling."""
        if event.modifiers() & Qt.ControlModifier:
            # Let parent handle if Control is pressed (e.g., for zoom)
            event.ignore()
            return

        # Calculate number of lines to scroll
        delta = event.angleDelta().y()
        lines = delta // 40  # Adjust divisor to control scroll speed

        # Update scroll position
        vbar = self.verticalScrollBar()
        vbar.setValue(vbar.value() - lines)

        event.accept()

    def keyPressEvent(self, event: QKeyEvent):
        """Handle key press events including control sequences."""
        text = event.text()
        key = event.key()
        modifiers = event.modifiers()

        # Handle keypad in application mode
        if self._application_keypad_mode and not modifiers:
            # Map keypad keys to application mode sequences
            keypad_map = {
                Qt.Key_0: b'\x1bOp',
                Qt.Key_1: b'\x1bOq',
                Qt.Key_2: b'\x1bOr',
                Qt.Key_3: b'\x1bOs',
                Qt.Key_4: b'\x1bOt',
                Qt.Key_5: b'\x1bOu',
                Qt.Key_6: b'\x1bOv',
                Qt.Key_7: b'\x1bOw',
                Qt.Key_8: b'\x1bOx',
                Qt.Key_9: b'\x1bOy',
                Qt.Key_Minus: b'\x1bOm',
                Qt.Key_Plus: b'\x1bOl',
                Qt.Key_Period: b'\x1bOn',
                Qt.Key_Enter: b'\x1bOM',
            }

            if key in keypad_map:
                self.data_ready.emit(keypad_map[key])
                event.accept()
                return

        # Handle control key combinations
        if modifiers & Qt.ControlModifier:
            if key >= Qt.Key_A and key <= Qt.Key_Z:
                # Calculate control character (1-26)
                ctrl_char = bytes([key - Qt.Key_A + 1])
                self.data_ready.emit(ctrl_char)
                event.accept()
                return

            # Handle special control sequences
            ctrl_map = {
                Qt.Key_2: b'\x00',  # Ctrl+@, Ctrl+2
                Qt.Key_3: b'\x1b',  # Ctrl+[, Ctrl+3
                Qt.Key_4: b'\x1c',  # Ctrl+\, Ctrl+4
                Qt.Key_5: b'\x1d',  # Ctrl+], Ctrl+5
                Qt.Key_6: b'\x1e',  # Ctrl+^, Ctrl+6
                Qt.Key_7: b'\x1f',  # Ctrl+_, Ctrl+7
                Qt.Key_8: b'\x7f',  # Ctrl+8 (delete)
            }
            if key in ctrl_map:
                self.data_ready.emit(ctrl_map[key])
                event.accept()
                return

        # Handle application cursor key mode and normal mode
        if self._application_cursor_keys:
            # Handle cursor keys in application mode
            if key == Qt.Key_Up:
                self.data_ready.emit(b'\x1bOA')
            elif key == Qt.Key_Down:
                self.data_ready.emit(b'\x1bOB')
            elif key == Qt.Key_Right:
                self.data_ready.emit(b'\x1bOC')
            elif key == Qt.Key_Left:
                self.data_ready.emit(b'\x1bOD')
        else:
            # Normal mode key handling
            if key == Qt.Key_Up:
                self.data_ready.emit(b'\x1b[A')
            elif key == Qt.Key_Down:
                self.data_ready.emit(b'\x1b[B')
            elif key == Qt.Key_Right:
                self.data_ready.emit(b'\x1b[C')
            elif key == Qt.Key_Left:
                self.data_ready.emit(b'\x1b[D')

        if key == Qt.Key_Return or key == Qt.Key_Enter:
            self.data_ready.emit(b'\r')
        elif key == Qt.Key_Backspace:
            self.data_ready.emit(b'\x7f' if modifiers & Qt.ControlModifier else b'\b')
        elif key == Qt.Key_Delete:
            self.data_ready.emit(b'\x1b[3~')
        elif key == Qt.Key_Tab:
            self.data_ready.emit(b'\t')
        elif text:
            self.data_ready.emit(text.encode())

        event.accept()

    def resizeEvent(self, event: QResizeEvent) -> None:
        """Handle resize events."""
        super().resizeEvent(event)

        self._update_dimensions()

        # Update scrollbar after resize
        self._update_scrollbar()

    def _toggle_blink(self):
        """Toggle blink state and update display if needed."""
        old_state = self._blink_state
        self._blink_state = not self._blink_state
        # Only update if we have any blinking characters
        if any(any((line.get_character(col)[1] & CharacterAttributes.BLINK)
                for col in range(self._cols))
                for line in self._lines[-self._rows:]):
            self.viewport().update()

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
        painter = QPainter(self.viewport())

        # Get font metrics for character dimensions
        fm = QFontMetrics(self.font())
        char_width = fm.horizontalAdvance(' ')
        char_height = fm.height()

        # Get the region that needs repainting
        region = event.rect()

        # Calculate scroll position - show newest lines when scrollbar at bottom
        first_visible_line = self.verticalScrollBar().value()

        # Calculate the character cell range to repaint
        start_row = max(0, region.top() // char_height)
        end_row = min(self._rows, (region.bottom() + char_height - 1) // char_height)
        start_col = max(0, region.left() // char_width)
        end_col = min(self._cols, (region.right() + char_width - 1) // char_width)

        # Paint visible character cells
        for row in range(start_row, end_row):
            y = row * char_height

            # Get actual line index accounting for scroll position
            line_index = first_visible_line + row
            if line_index >= len(self._lines):
                continue

            line = self._lines[line_index]

            for col in range(start_col, end_col):
                x = col * char_width

                # Get character and attributes
                char, attributes, fg_color, bg_color = line.get_character(col)
                self._draw_character(
                    painter, x, y, char, attributes, fg_color, bg_color,
                    char_width, char_height, fm
                )

        # Draw selection if active
        if self.has_selection():
            selection = self._selection.normalize()

            # Adjust for scroll position
            first_visible_line = self.verticalScrollBar().value()
            visible_start_row = selection.start_row - first_visible_line
            visible_end_row = selection.end_row - first_visible_line

            selection_color = self.palette().highlight().color()
            selection_text_color = self.palette().highlightedText().color()

            for row in range(max(visible_start_row, 0), min(visible_end_row + 1, self._rows)):
                y = row * char_height

                # Calculate selection range for this row
                row_start = selection.start_col if row + first_visible_line == selection.start_row else 0
                row_end = selection.end_col if row + first_visible_line == selection.end_row else self._cols

                # Draw selection background
                selection_rect = QRect(
                    row_start * char_width,
                    y,
                    (row_end - row_start) * char_width,
                    char_height
                )

                if selection_rect.intersects(region):
                    painter.fillRect(selection_rect, selection_color)

                    # Draw selected text
                    line_index = first_visible_line + row
                    if line_index < len(self._lines):
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

        # Draw cursor if visible and in view
        if self._cursor_visible:
            cursor_line = len(self._lines) - self._rows + self._cursor_row
            visible_cursor_row = cursor_line - first_visible_line

            if 0 <= visible_cursor_row < self._rows:  # Only draw if cursor is in visible area
                cursor_x = self._cursor_col * char_width
                cursor_y = visible_cursor_row * char_height

                cursor_rect = QRect(cursor_x, cursor_y, char_width, char_height)
                if cursor_rect.intersects(region):
                    # Get character under cursor for inversion
                    if cursor_line < len(self._lines):
                        line = self._lines[cursor_line]
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

    def _scroll_to_bottom(self):
        """Scroll the view to show the bottom of the terminal."""
        vbar = self.verticalScrollBar()
        vbar.setValue(vbar.maximum())

    def _get_selected_text(self) -> str:
        """Get currently selected text."""
        if not self.has_selection():
            return ""

        # Get normalized selection
        selection = self._selection.normalize()

        # Build selected text
        text = []
        for row in range(selection.start_row, selection.end_row + 1):
            line = self._lines[row]

            start = selection.start_col if row == selection.start_row else 0
            end = selection.end_col if row == selection.end_row else self._cols

            row_text = ""
            for col in range(start, end):
                char, _attributes, _fg_color, _bg_color = line.get_character(col)
                row_text += char

            text.append(row_text.rstrip())  # Remove trailing spaces

        return "\n".join(text)

    def _clear_selection(self) -> None:
        """Clear current selection."""
        if self._selection is not None:
            self._selection = None
            self.viewport().update()

    def has_selection(self) -> bool:
        """Check if there is an active selection."""
        return self._selection is not None and not self._selection.is_empty()

    def copy(self) -> None:
        """Copy selected text to clipboard."""
        if not self.has_selection():
            return

        text = self._get_selected_text()
        if text:
            QGuiApplication.clipboard().setText(text)

    def paste(self) -> None:
        """Paste text from clipboard."""
        text = QGuiApplication.clipboard().text()
        if text:
            self.data_ready.emit(text.encode())

    def clear(self) -> None:
        """Clear the terminal."""
        self._lines = []
        self._add_new_lines(self._rows)
        self._cursor_row = 0
        self._cursor_col = 0
        self._cursor_delayed_wrap = False
        self._clear_selection()
        self.viewport().update()
