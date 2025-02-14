"""Terminal widget implementation."""

import base64
from dataclasses import dataclass
import logging
from typing import Optional, Tuple

from PySide6.QtWidgets import QWidget, QAbstractScrollArea, QMenu
from PySide6.QtCore import Qt, Signal, QRect, QPoint, QTimer
from PySide6.QtGui import (
    QPainter, QPaintEvent, QColor, QFontMetrics, QFont,
    QResizeEvent, QKeyEvent, QMouseEvent,
    QGuiApplication, QWheelEvent
)

from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager
from humbug.gui.terminal.terminal_buffer import TerminalBuffer, CharacterAttributes
from humbug.gui.terminal.terminal_selection import TerminalSelection
from humbug.gui.terminal.terminal_size import TerminalSize
from humbug.language.language_manager import LanguageManager


@dataclass
class MouseTrackingState:
    """Mouse tracking configuration."""
    enabled: bool = False
    mode: int = 0  # 0=off, 1000=normal, 1002=button, 1003=any
    utf8_mode: bool = False
    sgr_mode: bool = False


class TerminalWidget(QAbstractScrollArea):
    """Terminal widget implementation."""

    data_ready = Signal(bytes)  # Emitted when user input is ready
    size_changed = Signal()  # Emitted when terminal size changes

    def __init__(self, parent: Optional[QWidget] = None):
        """Initialize terminal widget."""
        super().__init__(parent)
        self._logger = logging.getLogger("TerminalWidget")
        self._style_manager = StyleManager()
        self._language_manager = LanguageManager()

        # Set up scrollbar behavior
        self.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOn)
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        self.verticalScrollBar().valueChanged.connect(self._handle_scroll)
        self.setFocusPolicy(Qt.StrongFocus)

        # Initialize buffers
        self._main_buffer = TerminalBuffer(24, 80)  # Default size
        self._alternate_buffer = None
        self._current_buffer = self._main_buffer

        # Selection state
        self._selection: Optional[TerminalSelection] = None
        self._selecting = False

        # Terminal state
        self._terminal_title = ""
        self._current_directory = None
        self._escape_seq_buffer = ""
        self._in_escape_seq = False
        self._screen_reverse_mode = False

        self._mouse_tracking = MouseTrackingState()

        # Default colors
        self._default_fg = self._style_manager.get_color(ColorRole.TEXT_PRIMARY)
        self._default_bg = self._style_manager.get_color(ColorRole.TAB_BACKGROUND_ACTIVE)

        # ANSI color mapping
        self._ansi_colors = {
            0: ColorRole.TERM_BLACK,
            1: ColorRole.TERM_RED,
            2: ColorRole.TERM_GREEN,
            3: ColorRole.TERM_YELLOW,
            4: ColorRole.TERM_BLUE,
            5: ColorRole.TERM_MAGENTA,
            6: ColorRole.TERM_CYAN,
            7: ColorRole.TERM_WHITE,
            8: ColorRole.TERM_BRIGHT_BLACK,
            9: ColorRole.TERM_BRIGHT_RED,
            10: ColorRole.TERM_BRIGHT_GREEN,
            11: ColorRole.TERM_BRIGHT_YELLOW,
            12: ColorRole.TERM_BRIGHT_BLUE,
            13: ColorRole.TERM_BRIGHT_MAGENTA,
            14: ColorRole.TERM_BRIGHT_CYAN,
            15: ColorRole.TERM_BRIGHT_WHITE,
        }

        # Blink handling
        self._blink_state = False
        self._blink_timer = QTimer(self)
        self._blink_timer.timeout.connect(self._toggle_blink)
        self._blink_timer.start(500)  # Toggle every 500ms

        # Initialize size and connect signals
        self._update_dimensions()
        self.setContextMenuPolicy(Qt.CustomContextMenu)
        self.customContextMenuRequested.connect(self._show_terminal_context_menu)
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
        buffer = self._current_buffer

        if new_size.cols != buffer.cols or new_size.rows != buffer.rows:
            buffer.resize(new_size.rows, new_size.cols)
            self.size_changed.emit()

    def _handle_scroll(self, value: int):
        """Handle scrollbar value changes."""
        self.viewport().update()

    def _update_scrollbar(self):
        """Update scrollbar range based on content size."""
        buffer = self._current_buffer
        history_lines = max(0, len(buffer.lines) - buffer.rows)

        # Set range and update scroll position if needed
        vbar = self.verticalScrollBar()
        old_at_bottom = vbar.value() == vbar.maximum()
        vbar.setRange(0, history_lines)

        # If we were at bottom before, stay at bottom
        if old_at_bottom:
            vbar.setValue(vbar.maximum())

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
        buffer = self._current_buffer

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

    def _handle_alternate_screen(self, enable: bool):
        """Switch between main and alternate screen buffers."""
        if (enable and self._current_buffer == self._alternate_buffer) or \
           (not enable and self._current_buffer == self._main_buffer):
            return

        self._clear_selection()

        if enable:
            if not self._alternate_buffer:
                size = self.calculate_size()
                self._alternate_buffer = TerminalBuffer(size.rows, size.cols)
            self._current_buffer = self._alternate_buffer
        else:
            self._current_buffer = self._main_buffer
            self._alternate_buffer = None

        # Update scrollbar for current buffer
        self._update_scrollbar()
        self.viewport().update()

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
                    buffer = self._current_buffer
                    buffer.clear()
                    self._clear_selection()
                    buffer.scroll_region.top = 0
                    buffer.scroll_region.bottom = buffer.rows
                    buffer.scroll_region.rows = buffer.rows
                    self._update_scrollbar()
                    self.viewport().update()
                elif mode == 5:  # DECSCNM - Screen Mode (Reverse)
                    self._screen_reverse_mode = set_mode
                    self.viewport().update()  # Force redraw with new colors
                elif mode == 6:  # DECOM - Origin Mode
                    buffer.modes.origin = set_mode
                    buffer.cursor.row = 0
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
                        buffer.cursor.saved_position = (
                            buffer.cursor.row,
                            buffer.cursor.col,
                            buffer.cursor.delayed_wrap,
                            buffer.modes.origin
                        )
                    elif buffer.cursor.saved_position:
                        buffer.cursor.row, buffer.cursor.col, buffer.cursor.delayed_wrap, origin = buffer.cursor.saved_position
                        buffer.modes.origin = origin
                elif mode == 1049:  # Alternate Screen + save/restore cursor
                    if set_mode:
                        buffer.cursor.saved_position = (
                            buffer.cursor.row,
                            buffer.cursor.col,
                            buffer.cursor.delayed_wrap,
                            buffer.modes.origin
                        )
                        self._handle_alternate_screen(True)
                    else:
                        self._handle_alternate_screen(False)
                        if buffer.cursor.saved_position:
                            buffer.cursor.row, buffer.cursor.col, buffer.cursor.delayed_wrap, origin = buffer.cursor.saved_position
                            buffer.modes.origin = origin
                elif mode == 2004:  # Bracketed paste mode
                    buffer.modes.bracketed_paste = set_mode
                else:
                    print(f"Unknown PM operation {mode}")
                    self._logger.warning(f"Unknown PM operation {mode}")
        except ValueError as e:
            self._logger.warning(f"Invalid private mode parameter: {params}")

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
                color_role = self._ansi_colors[param - 30]
                buffer.attributes.foreground = self._style_manager.get_color(color_role).rgb()

            elif 40 <= param <= 47:  # Standard background color
                buffer.attributes.current |= CharacterAttributes.CUSTOM_BG
                color_role = self._ansi_colors[param - 40]
                buffer.attributes.background = self._style_manager.get_color(color_role).rgb()

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
                print(f"Unknown SGR sequence {params}")
                self._logger.warning(f"Unknown SGR sequence {params}")

            i += 1

    def _clear_region(self, start_row: int, start_col: int, end_row: int, end_col: int) -> None:
        """Clear a rectangular region of the terminal."""
        buffer = self._current_buffer
        for row in range(start_row, end_row + 1):
            line_index = len(buffer.lines) - buffer.rows + row
            if 0 <= line_index < len(buffer.lines):
                line = buffer.lines[line_index]
                start = start_col if row == start_row else 0
                end = end_col if row == end_row else buffer.cols - 1
                for col in range(start, end + 1):
                    line.set_character(col, ' ')

    def _process_csi(self, sequence: str) -> None:
        """Process CSI (Control Sequence Introducer) sequence."""
        buffer = self._current_buffer
        code = sequence[-1]
        print(f"CSI code {code}: {repr(sequence)}")

        # Parse parameters
        params_str = sequence[2:-1]  # Remove ESC[ and final character
        params = [int(p) if p.isdigit() else 0 for p in params_str.split(';')] if params_str else [0]

        if code == 'A':  # CUU - Cursor Up
            amount = max(1, params[0])
            buffer.cursor.row = max(0, buffer.cursor.row - amount)
            buffer.cursor.delayed_wrap = False

        elif code == 'B':  # CUD - Cursor Down
            amount = max(1, params[0])
            max_rows = buffer.rows if not buffer.modes.origin else buffer.scroll_region.rows
            buffer.cursor.row = min(max_rows - 1, buffer.cursor.row + amount)
            buffer.cursor.delayed_wrap = False

        elif code == 'C':  # CUF - Cursor Forward
            amount = max(1, params[0])
            buffer.cursor.col = min(buffer.cols - 1, buffer.cursor.col + amount)
            buffer.cursor.delayed_wrap = False

        elif code == 'D':  # CUB - Cursor Back
            amount = max(1, params[0])
            buffer.cursor.col = max(0, buffer.cursor.col - amount)
            buffer.cursor.delayed_wrap = False

        elif code == 'G':  # CHA - Cursor Horizontal Absolute
            col = max(0, params[0] - 1)  # Convert 1-based to 0-based
            buffer.cursor.col = min(col, buffer.cols - 1)
            buffer.cursor.delayed_wrap = False

        elif code == 'H':  # CUP - Cursor Position
            row = params[0] if params else 1
            col = params[1] if len(params) > 1 else 1
            max_rows = buffer.rows if not buffer.modes.origin else buffer.scroll_region.rows
            buffer.cursor.row = min(max_rows - 1, max(0, row - 1))  # Convert to 0-based
            buffer.cursor.col = min(buffer.cols - 1, max(0, col - 1))
            buffer.cursor.delayed_wrap = False

        elif code == 'J':  # ED - Erase in Display
            mode = params[0] if params else 0
            if mode == 0:  # Clear from cursor to end
                self._clear_region(
                    buffer.cursor.row,
                    buffer.cursor.col,
                    buffer.rows - 1,
                    buffer.cols - 1
                )
            elif mode == 1:  # Clear from start to cursor
                self._clear_region(0, 0, buffer.cursor.row, buffer.cursor.col)
            elif mode == 2:  # Clear entire screen
                self._clear_region(0, 0, buffer.rows - 1, buffer.cols - 1)

        elif code == 'K':  # EL - Erase in Line
            mode = params[0] if params else 0
            if mode == 0:  # Clear from cursor to end
                self._clear_region(
                    buffer.cursor.row,
                    buffer.cursor.col,
                    buffer.cursor.row,
                    buffer.cols - 1
                )
            elif mode == 1:  # Clear from start to cursor
                self._clear_region(
                    buffer.cursor.row,
                    0,
                    buffer.cursor.row,
                    buffer.cursor.col
                )
            elif mode == 2:  # Clear entire line
                self._clear_region(
                    buffer.cursor.row,
                    0,
                    buffer.cursor.row,
                    buffer.cols - 1
                )

        elif code == 'L':  # IL - Insert Line
            count = max(1, params[0] if params else 1)
            self._insert_lines(count)

        elif code == 'M':  # DL - Delete Line
            count = max(1, params[0] if params else 1)
            self._delete_lines(count)

        elif code == 'P':  # DCH - Delete Character
            count = max(1, params[0] if params else 1)
            self._delete_chars(count)

        elif code == 'S':  # SU - Scroll Up
            count = max(1, params[0] if params else 1)
            self._scroll_up(count)

        elif code == 'T':  # SD - Scroll Down
            count = max(1, params[0] if params else 1)
            self._scroll_down(count)

        elif code == 'X':  # ECH - Erase Character
            count = max(1, params[0] if params else 1)
            self._erase_chars(count)

        elif code == '@':  # ICH - Insert Character
            count = max(1, params[0] if params else 1)
            self._insert_chars(count)

        elif code == 'd':  # VPA - Line Position Absolute
            row = max(0, params[0] - 1) if params else 0  # Convert 1-based to 0-based
            max_rows = buffer.rows if not buffer.modes.origin else buffer.scroll_region.rows
            buffer.cursor.row = min(row, max_rows - 1)
            buffer.cursor.delayed_wrap = False

        elif code == 'f':  # HVP - Horizontal and Vertical Position
            row = params[0] if params else 1
            col = params[1] if len(params) > 1 else 1
            max_rows = buffer.rows if not buffer.modes.origin else buffer.scroll_region.rows
            buffer.cursor.row = min(max_rows - 1, max(0, row - 1))  # Convert to 0-based
            buffer.cursor.col = min(buffer.cols - 1, max(0, col - 1))
            buffer.cursor.delayed_wrap = False

        elif code == 'g':  # TBC - Tab clear
            params = [int(p) if p.isdigit() else 0 for p in sequence[2:-1].split(';')] if sequence[2:-1] else [0]
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
            buffer.cursor.saved_position = (
                buffer.cursor.row,
                buffer.cursor.col,
                buffer.cursor.delayed_wrap,
                buffer.modes.origin
            )

        elif code == 'u':  # Restore cursor position
            if buffer.cursor.saved_position:
                buffer.cursor.row, buffer.cursor.col, buffer.cursor.delayed_wrap, origin = buffer.cursor.saved_position
                buffer.modes.origin = origin

        else:
            print(f"Unknown CSI sequence {repr(sequence)}")
            self._logger.warning(f"Unknown CSI sequence {repr(sequence)}")

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
            print(f"Unknown DEC special sequence {repr(sequence)}")
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
            buffer = self._current_buffer
            print(f"ESC code {code}: {repr(sequence)}")
            if code == 'D':  # Index
                cursor_row = buffer.cursor.row if not buffer.modes.origin else buffer.cursor.row + buffer.scroll_region.top
                if cursor_row != buffer.scroll_region.bottom - 1:
                    max_rows = buffer.rows if not buffer.modes.origin else buffer.scroll_region.rows
                    buffer.cursor.row = min(buffer.cursor.row + 1, max_rows - 1)
                else:
                    self._scroll_up(1)
                buffer.cursor.delayed_wrap = False

            elif code == 'E':  # Next Line
                buffer.cursor.col = 0
                cursor_row = buffer.cursor.row if not buffer.modes.origin else buffer.cursor.row + buffer.scroll_region.top
                if cursor_row != buffer.scroll_region.bottom - 1:
                    max_rows = buffer.rows if not buffer.modes.origin else buffer.scroll_region.rows
                    buffer.cursor.row = min(buffer.cursor.row + 1, max_rows - 1)
                else:
                    self._scroll_up(1)
                buffer.cursor.delayed_wrap = False

            # Add handling for tab stop sequences
            elif code == 'H':  # HTS - Set tab stop at current position
                buffer.tab_stops.set_tab_stop(buffer.cursor.col)

            elif code == 'M':  # Reverse Index
                cursor_row = buffer.cursor.row if not buffer.modes.origin else buffer.cursor.row + buffer.scroll_region.top
                if cursor_row != buffer.scroll_region.top:
                    buffer.cursor.row = max(0, buffer.cursor.row - 1)
                else:
                    self._scroll_down(1)
                buffer.cursor.delayed_wrap = False

            else:
                print(f"Unknown simple ESC sequence: {repr(sequence)}")
                self._logger.warning(f"Unknown simple ESC sequence: {repr(sequence)}")

            return

        print(f"Unknown ESC sequence: {repr(sequence)}")
        self._logger.warning(f"Unknown ESC sequence: {repr(sequence)}")

    def _insert_lines(self, count: int) -> None:
        """Insert blank lines at cursor position."""
        buffer = self._current_buffer
        cursor_row = buffer.cursor.row if not buffer.modes.origin else buffer.cursor.row + buffer.scroll_region.top
        if not (buffer.scroll_region.top <= cursor_row < buffer.scroll_region.bottom):
            return

        # Calculate lines to move
        start = len(buffer.lines) - buffer.rows + cursor_row
        end = len(buffer.lines) - buffer.rows + buffer.scroll_region.bottom

        # Clip the count
        count = min(count, end - start)

        # Insert blank lines at the cursor and delete them at the end of the scrolling region
        for _ in range(count):
            buffer.lines.insert(start, buffer.get_new_line())
            del buffer.lines[end]

        buffer.cursor.col = 0
        buffer.cursor.delayed_wrap = False

    def _delete_lines(self, count: int) -> None:
        """Delete lines at cursor position."""
        buffer = self._current_buffer
        cursor_row = buffer.cursor.row if not buffer.modes.origin else buffer.cursor.row + buffer.scroll_region.top
        if not (buffer.scroll_region.top <= cursor_row < buffer.scroll_region.bottom):
            return

        # Calculate lines to remove
        start = len(buffer.lines) - buffer.rows + cursor_row
        end = len(buffer.lines) - buffer.rows + buffer.scroll_region.bottom

        # Clip the count
        count = min(count, end - start)

        # Insert blank lines at the end of the scrolling region and remove them at the cursor
        for _ in range(count):
            buffer.lines.insert(end, buffer.get_new_line())
            del buffer.lines[start]

        buffer.cursor.col = 0
        buffer.cursor.delayed_wrap = False

    def _insert_chars(self, count: int) -> None:
        """Insert blank characters at cursor position."""
        buffer = self._current_buffer
        cursor_row = buffer.cursor.row if not buffer.modes.origin else buffer.cursor.row + buffer.scroll_region.top
        line_index = len(buffer.lines) - buffer.rows + cursor_row
        if 0 <= line_index < len(buffer.lines):
            line = buffer.lines[line_index]
            # Move existing characters right
            for col in range(buffer.cols - 1, buffer.cursor.col - 1, -1):
                if col >= buffer.cursor.col + count:
                    char, attrs, fg, bg = line.get_character(col - count)
                    line.set_character(col, char, attrs, fg, bg)

            # Insert spaces
            for col in range(buffer.cursor.col, min(buffer.cursor.col + count, buffer.cols)):
                line.set_character(col, ' ')

    def _delete_chars(self, count: int) -> None:
        """Delete characters at cursor position."""
        buffer = self._current_buffer
        cursor_row = buffer.cursor.row if not buffer.modes.origin else buffer.cursor.row + buffer.scroll_region.top
        line_index = len(buffer.lines) - buffer.rows + cursor_row
        if 0 <= line_index < len(buffer.lines):
            line = buffer.lines[line_index]
            # Move characters left
            for col in range(buffer.cursor.col, buffer.cols):
                if col + count < buffer.cols:
                    char, attrs, fg, bg = line.get_character(col + count)
                    line.set_character(col, char, attrs, fg, bg)
                else:
                    line.set_character(col, ' ')

    def _erase_chars(self, count: int) -> None:
        """Erase characters at cursor position."""
        buffer = self._current_buffer
        cursor_row = buffer.cursor.row if not buffer.modes.origin else buffer.cursor.row + buffer.scroll_region.top
        line_index = len(buffer.lines) - buffer.rows + cursor_row
        if 0 <= line_index < len(buffer.lines):
            line = buffer.lines[line_index]
            for col in range(buffer.cursor.col, min(buffer.cursor.col + count, buffer.cols)):
                line.set_character(col, ' ')

    def _scroll_up(self, count: int) -> None:
        """Scroll up within current scroll region."""
        buffer = self._current_buffer

        # Calculate actual lines in scroll region
        start = len(buffer.lines) - buffer.rows + buffer.scroll_region.top
        end = len(buffer.lines) - buffer.rows + buffer.scroll_region.bottom

        # Insert blank lines at the bottom of the scrolling region and remove lines from the top
        for _ in range(count):
            buffer.lines.insert(end, buffer.get_new_line())

            # If we're using the main screen and the scrolling region top is the top of the screen
            # then we don't actually delete anything, we simply let the scrolled line roll into
            # the history buffer
            if buffer == self._alternate_buffer or buffer.scroll_region.top != 0:
                scrolled_line = buffer.lines.pop(start)
                if buffer != self._alternate_buffer:
                    buffer.lines.insert(len(buffer.lines) - buffer.rows, scrolled_line)

        # Update scrollbar if history changed
        if buffer == self._main_buffer:
            self._update_scrollbar()

    def _scroll_down(self, count: int) -> None:
        """Scroll down within current scroll region."""
        buffer = self._current_buffer
        # Calculate actual lines in scroll region
        start = len(buffer.lines) - buffer.rows + buffer.scroll_region.top
        end = len(buffer.lines) - buffer.rows + buffer.scroll_region.bottom

        # Insert blank lines at the top of the scrolling region and remove lines from the bottom
        for _ in range(count):
            buffer.lines.insert(start, buffer.get_new_line())
            del buffer.lines[end]

    def _write_char(self, char: str) -> None:
        """Write a single character at the current cursor position."""
        buffer = self._current_buffer

        if char == '\r':
            buffer.cursor.col = 0
            buffer.cursor.delayed_wrap = False
            return

        if char in '\n\f\v':
            cursor_row = buffer.cursor.row if not buffer.modes.origin else buffer.cursor.row + buffer.scroll_region.top
            if cursor_row != buffer.scroll_region.bottom - 1:
                max_rows = buffer.rows if not buffer.modes.origin else buffer.scroll_region.rows
                buffer.cursor.row = min(buffer.cursor.row + 1, max_rows - 1)
            else:
                self._scroll_up(1)

            buffer.cursor.delayed_wrap = False
            return

        if char == '\b':
            buffer.cursor.col = max(0, buffer.cursor.col - 1)
            buffer.cursor.delayed_wrap = False
            return

        # Handle delayed wrapping for printable characters
        if buffer.cursor.delayed_wrap:
            buffer.cursor.col = 0
            buffer.cursor.delayed_wrap = False
            cursor_row = buffer.cursor.row if not buffer.modes.origin else buffer.cursor.row + buffer.scroll_region.top
            if cursor_row != buffer.scroll_region.bottom - 1:
                max_rows = buffer.rows if not buffer.modes.origin else buffer.scroll_region.rows
                buffer.cursor.row = min(buffer.cursor.row + 1, max_rows - 1)
                cursor_row += 1
            else:
                self._scroll_up(1)

        if char == '\t':
            # Get next tab stop
            next_stop = buffer.tab_stops.get_next_tab_stop(buffer.cursor.col)
            if next_stop is not None:
                # Move to tab stop
                buffer.cursor.col = next_stop
            else:
                # Move to end of line if no more stops
                buffer.cursor.col = buffer.cols - 1

            return

        # Handle printable characters
        if ord(char) >= 32:
            cursor_row = buffer.cursor.row if not buffer.modes.origin else buffer.cursor.row + buffer.scroll_region.top
            line_index = len(buffer.lines) - buffer.rows + cursor_row
            line = buffer.lines[line_index]

            # Write character
            line.set_character(
                buffer.cursor.col,
                char,
                buffer.attributes.current,
                buffer.attributes.foreground if buffer.attributes.current & CharacterAttributes.CUSTOM_FG else None,
                buffer.attributes.background if buffer.attributes.current & CharacterAttributes.CUSTOM_BG else None
            )

            # Handle cursor movement and wrapping
            if buffer.cursor.col == buffer.cols - 1:
                buffer.cursor.delayed_wrap = buffer.modes.auto_wrap
            else:
                buffer.cursor.col += 1

    def put_data(self, data: bytes) -> None:
        """
        Display received data with ANSI sequence handling.

        Args:
            data: Raw bytes from terminal

        Raises:
            UnicodeDecodeError: If data cannot be decoded
        """
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
    def _pixel_pos_to_text_pos(self, pos: QPoint) -> Tuple[int, int]:
        """Convert pixel coordinates to text position.

        Args:
            pos: Mouse position in viewport coordinates

        Returns:
            Tuple of (row, col) in terminal buffer coordinates
        """
        buffer = self._current_buffer
        fm = QFontMetrics(self.font())
        char_width = fm.horizontalAdvance(' ')
        char_height = fm.height()

        # Convert pixel position to viewport row/col
        viewport_col = max(0, min(pos.x() // char_width, buffer.cols - 1))
        viewport_row = max(0, min(pos.y() // char_height, buffer.rows - 1))

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
        buffer = self._current_buffer
        if event.button() == Qt.LeftButton:
            # Handle text selection
            self._selecting = True
            pos = self._pixel_pos_to_text_pos(event.position().toPoint())
            self._selection = TerminalSelection(pos[0], pos[1], pos[0], pos[1])
            self.viewport().update()

        # Handle mouse tracking if enabled
        if self._mouse_tracking.enabled:
            pos = event.position().toPoint()
            button = event.button()
            row, col = self._pixel_pos_to_text_pos(pos)

            # Construct mouse report based on mode
            if self._mouse_tracking.sgr_mode:
                report = self._make_sgr_mouse_report(row, col, button, True)
            else:
                report = self._make_normal_mouse_report(row, col, button)

            if report:
                self.data_ready.emit(report.encode())

        super().mousePressEvent(event)

    def mouseReleaseEvent(self, event: QMouseEvent) -> None:
        """Handle mouse release for both tracking and selection."""
        buffer = self._current_buffer
        if event.button() == Qt.LeftButton:
            self._selecting = False

        # Handle mouse tracking if enabled
        if self._mouse_tracking.enabled:
            pos = event.position().toPoint()
            button = event.button()
            row, col = self._pixel_pos_to_text_pos(pos)

            if self._mouse_tracking.sgr_mode:
                report = self._make_sgr_mouse_report(row, col, button, False)
                if report:
                    self.data_ready.emit(report.encode())

        super().mouseReleaseEvent(event)

    def mouseMoveEvent(self, event: QMouseEvent) -> None:
        """Handle mouse movement for selection and tracking."""
        buffer = self._current_buffer
        # Handle text selection
        if self._selecting and self._selection is not None:
            pos = self._pixel_pos_to_text_pos(event.position().toPoint())
            if (pos[0] != self._selection.end_row or
                pos[1] != self._selection.end_col):
                self._selection.end_row = pos[0]
                self._selection.end_col = pos[1]
                self.viewport().update()

        # Handle mouse tracking if enabled and in button event mode (1002) or any event mode (1003)
        if self._mouse_tracking.enabled and self._mouse_tracking.mode in (1002, 1003):
            row, col = self._pixel_pos_to_text_pos(event.position().toPoint())
            buttons = event.buttons()

            # For 1002 mode, only report if buttons are pressed
            if self._mouse_tracking.mode == 1002 and not buttons:
                return

            btn_num = 32  # Default to button release
            if buttons & Qt.LeftButton:
                btn_num = 32
            elif buttons & Qt.MiddleButton:
                btn_num = 33
            elif buttons & Qt.RightButton:
                btn_num = 34

            if self._mouse_tracking.sgr_mode:
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
        buffer = self._current_buffer
        text = event.text()
        key = event.key()
        modifiers = event.modifiers()

        # Handle keypad in application mode
        if buffer.modes.application_keypad and not modifiers:
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
        if buffer.modes.application_cursor:
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

    def _toggle_blink(self):
        """Toggle blink state and update display if needed."""
        buffer = self._current_buffer
        old_state = self._blink_state
        self._blink_state = not self._blink_state

        # Only update if we have any blinking characters
        if any(any((line.get_character(col)[1] & CharacterAttributes.BLINK)
                for col in range(buffer.cols))
                for line in buffer.lines[-buffer.rows:]):
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
        # Determine initial colors, considering custom and default colors
        fg = (QColor(fg_color) if fg_color is not None and (attributes & CharacterAttributes.CUSTOM_FG)
            else QColor(self._default_fg))
        bg = (QColor(bg_color) if bg_color is not None and (attributes & CharacterAttributes.CUSTOM_BG)
            else QColor(self._default_bg))

        # Handle inverse video by swapping foreground and background colors
        if attributes & CharacterAttributes.INVERSE:
            fg, bg = bg, fg

        # Handle global screen reverse mode - swap colors if enabled
        if self._screen_reverse_mode:
            fg, bg = bg, fg

        # Handle hidden text by using background color for foreground
        if attributes & CharacterAttributes.HIDDEN:
            fg = QColor(bg)

        # Draw background
        painter.fillRect(QRect(x, y, char_width, char_height), bg)

        # Handle dim text by using alpha channel
        if attributes & CharacterAttributes.DIM:
            fg.setAlpha(128)  # 50% opacity

        painter.setPen(fg)

        # Set up font attributes
        font = painter.font()
        font.setBold(bool(attributes & CharacterAttributes.BOLD))
        font.setItalic(bool(attributes & CharacterAttributes.ITALIC))
        font.setUnderline(bool(attributes & CharacterAttributes.UNDERLINE))
        font.setStrikeOut(bool(attributes & CharacterAttributes.STRIKE))
        painter.setFont(font)

        # Draw the character if it's not blinking or if it's in the visible blink phase
        if not (attributes & CharacterAttributes.BLINK and self._blink_state):
            painter.drawText(x, y + fm.ascent(), char)

    def paintEvent(self, event: QPaintEvent) -> None:
        """Handle paint events efficiently."""
        painter = QPainter(self.viewport())
        buffer = self._current_buffer

        # Get font metrics for character dimensions
        fm = QFontMetrics(self.font())
        char_width = fm.horizontalAdvance(' ')
        char_height = fm.height()

        # Get the region that needs repainting
        region = event.rect()

        # Calculate scroll position
        first_visible_line = self.verticalScrollBar().value()

        # Calculate the character cell range to repaint
        start_row = max(0, region.top() // char_height)
        end_row = min(buffer.rows, (region.bottom() + char_height - 1) // char_height)
        start_col = max(0, region.left() // char_width)
        end_col = min(buffer.cols, (region.right() + char_width - 1) // char_width)

        # Paint visible character cells
        for row in range(start_row, end_row):
            y = row * char_height

            # Get actual line index accounting for scroll position
            line_index = first_visible_line + row
            if line_index >= len(buffer.lines):
                continue

            line = buffer.lines[line_index]

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
            visible_start_row = selection.start_row - first_visible_line
            visible_end_row = selection.end_row - first_visible_line

            selection_color = self.palette().highlight().color()
            selection_text_color = self.palette().highlightedText().color()

            for row in range(max(visible_start_row, 0), min(visible_end_row + 1, buffer.rows)):
                y = row * char_height

                # Calculate selection range for this row
                row_start = selection.start_col if row + first_visible_line == selection.start_row else 0
                row_end = selection.end_col if row + first_visible_line == selection.end_row else buffer.cols

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
                    if line_index < len(buffer.lines):
                        line = buffer.lines[line_index]
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
        if buffer.cursor.visible:
            cursor_line = len(buffer.lines) - buffer.rows + buffer.cursor.row
            visible_cursor_row = cursor_line - first_visible_line

            if 0 <= visible_cursor_row < buffer.rows:  # Only draw if cursor is in visible area
                cursor_x = buffer.cursor.col * char_width
                cursor_y = visible_cursor_row * char_height

                cursor_rect = QRect(cursor_x, cursor_y, char_width, char_height)
                if cursor_rect.intersects(region):
                    # Get character under cursor for inversion
                    if cursor_line < len(buffer.lines):
                        line = buffer.lines[cursor_line]
                        char, _attributes, _fg_color, _bg_color = line.get_character(buffer.cursor.col)

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

        buffer = self._current_buffer
        # Get normalized selection
        selection = self._selection.normalize()

        # Build selected text
        text = []
        for row in range(selection.start_row, selection.end_row + 1):
            if row >= len(buffer.lines):
                break

            line = buffer.lines[row]
            start = selection.start_col if row == selection.start_row else 0
            end = selection.end_col if row == selection.end_row else buffer.cols

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
