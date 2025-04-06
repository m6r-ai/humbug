"""Terminal widget implementation."""

from dataclasses import dataclass
import logging
from typing import Tuple, Dict, List, cast

from PySide6.QtWidgets import QWidget, QAbstractScrollArea, QMenu
from PySide6.QtCore import Qt, Signal, QRect, QPoint, QTimer, QPointF, QRectF
from PySide6.QtGui import (
    QPainter, QPaintEvent, QColor, QFontMetricsF,
    QResizeEvent, QKeyEvent, QMouseEvent,
    QGuiApplication, QWheelEvent, QFont
)

from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager
from humbug.gui.tab.terminal.terminal_selection import TerminalSelection
from humbug.terminal.terminal_buffer import CharacterAttributes
from humbug.terminal.terminal_state import TerminalState


@dataclass
class TerminalMatch:
    """Represents a match in the terminal buffer."""
    row: int           # Row in buffer where match was found
    start_col: int     # Starting column of match
    end_col: int       # Ending column of match
    is_current: bool   # Whether this is the currently selected match


class TerminalWidget(QAbstractScrollArea):
    """Terminal widget implementation."""

    data_ready = Signal(bytes)  # Emitted when user input is ready
    size_changed = Signal()  # Emitted when terminal size changes

    def __init__(self, parent: QWidget | None = None):
        """Initialize terminal widget."""
        super().__init__(parent)
        self._logger = logging.getLogger("TerminalWidget")
        self._style_manager = StyleManager()

        # Set up scrollbar behavior
        self.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOn)
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.verticalScrollBar().valueChanged.connect(self._handle_scroll)
        self.setFocusPolicy(Qt.FocusPolicy.StrongFocus)

        self.setViewportMargins(4, 4, 4, 4)

        # Initialize terminal state
        self._state = TerminalState(24, 80)  # Default size

        # Initialize highlight tracking
        self._search_highlights: Dict[int, List[Tuple[int, int, bool]]] = {}

        # Selection state
        self._selection: TerminalSelection | None = None
        self._selecting = False

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

        # Initialize color mapping in state
        self._update_colors()

        # Blink handling
        self._blink_state = False
        self._blink_timer = QTimer(self)
        self._blink_timer.timeout.connect(self._toggle_blink)
        self._blink_timer.start(500)  # Toggle every 500ms

        self._has_focus = self.hasFocus()

        # Cache for character dimensions
        self._char_width: float = 0.0
        self._char_height: float = 0.0
        self._char_ascent: float = 0.0

        # Initialize size and connect signals
        self.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.customContextMenuRequested.connect(self._show_terminal_context_menu)
        self._style_manager.style_changed.connect(self._handle_style_changed)
        self._handle_style_changed()

        # Find functionality attributes
        self._matches: List[TerminalMatch] = []
        self._current_match = -1
        self._last_search = ""
        self._selection_active = False

    def _update_colors(self) -> None:
        """Update color mappings in terminal state."""
        # Update default colors
        self._state.set_default_colors(
            self._default_fg.rgb(),
            self._default_bg.rgb()
        )

        # Update ANSI color mapping
        color_map = {
            index: self._style_manager.get_color(role).rgb()
            for index, role in self._ansi_colors.items()
        }
        self._state.set_ansi_colors(color_map)

    def _handle_style_changed(self):
        """Handle style changes."""
        # Update terminal font
        font = QFont()
        font.setFamilies(self._style_manager.monospace_font_families)
        font.setFixedPitch(True)
        base_size = self._style_manager.base_font_size
        font.setPointSizeF(base_size * self._style_manager.zoom_factor)
        self.setFont(font)

        fm = QFontMetricsF(self.font())
        self._char_width = fm.horizontalAdvance(' ')
        self._char_height = fm.height()
        self._char_ascent = fm.ascent()

        # Update default colors
        self._default_fg = self._style_manager.get_color(ColorRole.TEXT_PRIMARY)
        self._default_bg = self._style_manager.get_color(ColorRole.TAB_BACKGROUND_ACTIVE)

        # Update color mappings in state
        self._update_colors()

        # Force redraw with new colors
        self.viewport().update()

    def update_dimensions(self) -> None:
        """Update terminal dimensions based on widget size and font metrics."""

        # Get the width of the vertical scrollbar
        scrollbar_width = self.verticalScrollBar().width()

        # Calculate available viewport width, subtracting scrollbar width and margins
        viewport_width = max(0, self.width() - scrollbar_width - (2 * 4))
        viewport_height = self.height() - (2 * 4)

        cols = int(max(viewport_width / self._char_width, 1))
        rows = int(max(viewport_height / self._char_height, 1))

        # Update state dimensions
        self._state.resize(rows, cols)
        self._update_scrollbar()
        self.size_changed.emit()

    def _handle_scroll(self, _value: int):
        """Handle scrollbar value changes."""
        self.viewport().update()

    def _update_scrollbar(self):
        """Update scrollbar range based on content size."""
        terminal_rows = self._state.terminal_rows
        history_lines = max(0, self._state.terminal_history_lines - terminal_rows)

        # Set range and update scroll position if needed
        vbar = self.verticalScrollBar()
        vbar.setPageStep(terminal_rows)
        old_at_bottom = vbar.value() == vbar.maximum()
        vbar.setRange(0, history_lines)

        # If we were at bottom before, stay at bottom
        if old_at_bottom:
            vbar.setValue(vbar.maximum())

    def scroll_to_match(self, row: int) -> None:
        """
        Scroll to ensure a specific row is visible.

        Args:
            row: Row to scroll to
        """
        visible_lines = int((self.viewport().height() - (2 * 4)) / self._char_height)

        # Get effective row position in viewport
        scroll_pos = self.verticalScrollBar().value()
        viewport_row = row - scroll_pos

        # If row is outside visible area, scroll to it
        if viewport_row < 0:
            # Row is above visible area - scroll up
            self.verticalScrollBar().setValue(row)
        elif viewport_row >= visible_lines:
            # Row is below visible area - scroll down
            scroll_to = row - visible_lines + 1
            self.verticalScrollBar().setValue(scroll_to)

    def _scroll_to_bottom(self):
        """Scroll the view to show the bottom of the terminal."""
        vbar = self.verticalScrollBar()
        vbar.setValue(vbar.maximum())

    def _toggle_blink(self):
        """Toggle blink state and update display if needed."""
        self._blink_state = not self._blink_state
        if self._state.blinking_chars_on_screen():
            self.viewport().update()
            return

        # Always update if cursor is visible and blinking
        buffer = self._state.current_buffer
        cursor = buffer.cursor
        if cursor.visible and cursor.blink:
            # Convert cursor position to viewport coordinates
            history_lines = self._state.terminal_history_lines
            terminal_rows = self._state.terminal_rows
            first_visible_line = self.verticalScrollBar().value()

            cursor_line = history_lines - terminal_rows + cursor.row
            visible_cursor_row = cursor_line - first_visible_line

            if 0 <= visible_cursor_row < terminal_rows:
                # Only update the cursor region
                cursor_x = cursor.col * self._char_width
                cursor_y = visible_cursor_row * self._char_height

                # Create QRectF for precise cursor region, but convert to QRect for update
                cursor_rect_f = QRectF(
                    cursor_x,
                    cursor_y,
                    self._char_width,
                    self._char_height
                )
                # Add a small padding to ensure we capture the full character
                cursor_rect = cursor_rect_f.adjusted(-1, -1, 1, 1).toRect()
                self.viewport().update(cursor_rect)

    def _pixel_pos_to_text_pos(self, pos: QPoint) -> Tuple[int, int]:
        """Convert pixel coordinates to text position.

        Args:
            pos: Mouse position in viewport coordinates

        Returns:
            Tuple of (row, col) in terminal buffer coordinates
        """
        terminal_rows, terminal_cols = self._state.get_terminal_size()

        # Convert pixel position to viewport row/col
        viewport_col = max(0, min(int(pos.x() / self._char_width), terminal_cols - 1))
        viewport_row = max(0, min(int(pos.y() / self._char_height), terminal_rows - 1))

        # Adjust row for scroll position
        first_visible_line = self.verticalScrollBar().value()
        buffer_row = viewport_row + first_visible_line

        return (buffer_row, viewport_col)

    def _make_sgr_mouse_report(self, row: int, col: int, button: Qt.MouseButton, pressed: bool) -> str:
        """Create an SGR mouse report."""
        btn_num = {
            Qt.MouseButton.LeftButton: 0,
            Qt.MouseButton.MiddleButton: 1,
            Qt.MouseButton.RightButton: 2
        }.get(button, 3)

        if not pressed:
            btn_num += 3

        return f"\x1b[<{btn_num};{col + 1};{row + 1}{'M' if pressed else 'm'}"

    def _make_normal_mouse_report(self, row: int, col: int, button: Qt.MouseButton) -> str:
        """Create a normal X10/X11 mouse report."""
        btn_num = {
            Qt.MouseButton.LeftButton: 0,
            Qt.MouseButton.MiddleButton: 1,
            Qt.MouseButton.RightButton: 2
        }.get(button, 3)

        # Ensure values fit in a byte
        cb = 32 + btn_num
        cx = 32 + min(255, col + 1)
        cy = 32 + min(255, row + 1)

        return f"\x1b[M{chr(cb)}{chr(cx)}{chr(cy)}"

    def mousePressEvent(self, event: QMouseEvent) -> None:
        """Handle mouse press for both tracking and selection."""
        if event.button() & Qt.MouseButton.LeftButton:
            # Handle text selection
            self._selecting = True
            row, col = self._pixel_pos_to_text_pos(event.position().toPoint())
            self._selection = TerminalSelection(row, col, row, col)
            self.viewport().update()

        # Handle mouse tracking if enabled
        if self._state.mouse_tracking.enabled:
            pos = event.position().toPoint()
            button = event.button()
            row, col = self._pixel_pos_to_text_pos(pos)

            # Construct mouse report based on mode
            if self._state.mouse_tracking.sgr_mode:
                report = self._make_sgr_mouse_report(row, col, button, True)
            else:
                report = self._make_normal_mouse_report(row, col, button)

            if report:
                self.data_ready.emit(report.encode())

        super().mousePressEvent(event)

    def mouseReleaseEvent(self, event: QMouseEvent) -> None:
        """Handle mouse release for both tracking and selection."""
        if event.button() & Qt.MouseButton.LeftButton:
            self._selecting = False

        # Handle mouse tracking if enabled
        if self._state.mouse_tracking.enabled:
            pos = event.position().toPoint()
            button = event.button()
            row, col = self._pixel_pos_to_text_pos(pos)

            if self._state.mouse_tracking.sgr_mode:
                report = self._make_sgr_mouse_report(row, col, button, False)
                if report:
                    self.data_ready.emit(report.encode())

        super().mouseReleaseEvent(event)

    def mouseMoveEvent(self, event: QMouseEvent) -> None:
        """Handle mouse movement for selection and tracking."""
        # Handle text selection
        if self._selecting and self._selection is not None:
            row, col = self._pixel_pos_to_text_pos(event.position().toPoint())
            if (row != self._selection.end_row or
                col != self._selection.end_col):
                self._selection.end_row = row
                self._selection.end_col = col
                self.viewport().update()

        # Handle mouse tracking if enabled and in button event mode (1002) or any event mode (1003)
        if (
            self._state.mouse_tracking.enabled and
            self._state.mouse_tracking.mode in (1002, 1003)
        ):
            row, col = self._pixel_pos_to_text_pos(event.position().toPoint())
            buttons = event.buttons()

            # For 1002 mode, only report if buttons are pressed
            if self._state.mouse_tracking.mode == 1002 and not buttons:
                return

            btn_num = 32  # Default to button release
            if buttons & Qt.MouseButton.LeftButton:
                btn_num = 32
            elif buttons & Qt.MouseButton.MiddleButton:
                btn_num = 33
            elif buttons & Qt.MouseButton.RightButton:
                btn_num = 34

            if self._state.mouse_tracking.sgr_mode:
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
        if event.modifiers() & Qt.KeyboardModifier.ControlModifier:
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

        # Handle Alt/Meta key combinations
        if modifiers & Qt.KeyboardModifier.AltModifier:
            # Alt + letter sends ESC + letter
            if key >= Qt.Key.Key_A and key <= Qt.Key.Key_Z:
                self.data_ready.emit(b'\x1b' + chr(key).lower().encode())
                event.accept()
                return

            # Alt + number sends ESC + number
            if key >= Qt.Key.Key_0 and key <= Qt.Key.Key_9:
                self.data_ready.emit(b'\x1b' + chr(key).encode())
                event.accept()
                return

        # Handle keypad in application mode
        if self._state.application_keypad_mode and not modifiers:
            keypad_map: Dict[int, bytes] = {
                Qt.Key.Key_0: b'\x1bOp',
                Qt.Key.Key_1: b'\x1bOq',
                Qt.Key.Key_2: b'\x1bOr',
                Qt.Key.Key_3: b'\x1bOs',
                Qt.Key.Key_4: b'\x1bOt',
                Qt.Key.Key_5: b'\x1bOu',
                Qt.Key.Key_6: b'\x1bOv',
                Qt.Key.Key_7: b'\x1bOw',
                Qt.Key.Key_8: b'\x1bOx',
                Qt.Key.Key_9: b'\x1bOy',
                Qt.Key.Key_Minus: b'\x1bOm',
                Qt.Key.Key_Plus: b'\x1bOl',
                Qt.Key.Key_Period: b'\x1bOn',
                Qt.Key.Key_Enter: b'\x1bOM',
                Qt.Key.Key_Equal: b'\x1bOX',  # equals key
                Qt.Key.Key_Slash: b'\x1bOo',  # divide key
                Qt.Key.Key_Asterisk: b'\x1bOj', # multiply key
            }

            if key in keypad_map:
                self.data_ready.emit(keypad_map[key])
                event.accept()
                return

        # Handle Shift + Function keys
        if modifiers & Qt.KeyboardModifier.ShiftModifier:
            shift_fn_map: Dict[int, bytes] = {
                Qt.Key.Key_F1: b'\x1b[1;2P',
                Qt.Key.Key_F2: b'\x1b[1;2Q',
                Qt.Key.Key_F3: b'\x1b[1;2R',
                Qt.Key.Key_F4: b'\x1b[1;2S',
                Qt.Key.Key_F5: b'\x1b[15;2~',
                Qt.Key.Key_F6: b'\x1b[17;2~',
                Qt.Key.Key_F7: b'\x1b[18;2~',
                Qt.Key.Key_F8: b'\x1b[19;2~',
                Qt.Key.Key_F9: b'\x1b[20;2~',
                Qt.Key.Key_F10: b'\x1b[21;2~',
                Qt.Key.Key_F11: b'\x1b[23;2~',
                Qt.Key.Key_F12: b'\x1b[24;2~',
            }
            if key in shift_fn_map:
                self.data_ready.emit(shift_fn_map[key])
                event.accept()
                return

        # Handle Control + Function keys
        if modifiers & Qt.KeyboardModifier.ControlModifier:
            ctrl_fn_map: Dict[int, bytes] = {
                Qt.Key.Key_F1: b'\x1b[1;5P',
                Qt.Key.Key_F2: b'\x1b[1;5Q',
                Qt.Key.Key_F3: b'\x1b[1;5R',
                Qt.Key.Key_F4: b'\x1b[1;5S',
                Qt.Key.Key_F5: b'\x1b[15;5~',
                Qt.Key.Key_F6: b'\x1b[17;5~',
                Qt.Key.Key_F7: b'\x1b[18;5~',
                Qt.Key.Key_F8: b'\x1b[19;5~',
                Qt.Key.Key_F9: b'\x1b[20;5~',
                Qt.Key.Key_F10: b'\x1b[21;5~',
                Qt.Key.Key_F11: b'\x1b[23;5~',
                Qt.Key.Key_F12: b'\x1b[24;5~',
            }
            if key in ctrl_fn_map:
                self.data_ready.emit(ctrl_fn_map[key])
                event.accept()
                return

        # Handle standard function keys
        fn_map: Dict[int, bytes] = {
            Qt.Key.Key_F1: b'\x1bOP',
            Qt.Key.Key_F2: b'\x1bOQ',
            Qt.Key.Key_F3: b'\x1bOR',
            Qt.Key.Key_F4: b'\x1bOS',
            Qt.Key.Key_F5: b'\x1b[15~',
            Qt.Key.Key_F6: b'\x1b[17~',
            Qt.Key.Key_F7: b'\x1b[18~',
            Qt.Key.Key_F8: b'\x1b[19~',
            Qt.Key.Key_F9: b'\x1b[20~',
            Qt.Key.Key_F10: b'\x1b[21~',
            Qt.Key.Key_F11: b'\x1b[23~',
            Qt.Key.Key_F12: b'\x1b[24~',
        }
        if key in fn_map and not modifiers:
            self.data_ready.emit(fn_map[key])
            event.accept()
            return

        # Handle control key combinations
        if modifiers & Qt.KeyboardModifier.ControlModifier:
            if key >= Qt.Key.Key_A and key <= Qt.Key.Key_Z:
                # Calculate control character (1-26)
                ctrl_char = bytes([key - Qt.Key.Key_A + 1])
                self.data_ready.emit(ctrl_char)
                event.accept()
                return

            # Handle special control sequences
            ctrl_map: Dict[int, bytes] = {
                Qt.Key.Key_2: b'\x00',  # Ctrl+@, Ctrl+2
                Qt.Key.Key_3: b'\x1b',  # Ctrl+[, Ctrl+3
                Qt.Key.Key_4: b'\x1c',  # Ctrl+\, Ctrl+4
                Qt.Key.Key_5: b'\x1d',  # Ctrl+], Ctrl+5
                Qt.Key.Key_6: b'\x1e',  # Ctrl+^, Ctrl+6
                Qt.Key.Key_7: b'\x1f',  # Ctrl+_, Ctrl+7
                Qt.Key.Key_8: b'\x7f',  # Ctrl+8 (delete)
                Qt.Key.Key_Space: b'\x00',  # Ctrl+Space
                Qt.Key.Key_Backslash: b'\x1c',  # Ctrl+\
                Qt.Key.Key_BracketRight: b'\x1d',  # Ctrl+]
                Qt.Key.Key_BracketLeft: b'\x1b',  # Ctrl+[
                Qt.Key.Key_Minus: b'\x1f',  # Ctrl+-
            }
            if key in ctrl_map:
                self.data_ready.emit(ctrl_map[key])
                event.accept()
                return

        # Handle cursor keys based on mode
        cursor_map: Dict[int, bytes] = {}
        if self._state.application_cursor_mode:
            cursor_map = {
                Qt.Key.Key_Up: b'\x1bOA',
                Qt.Key.Key_Down: b'\x1bOB',
                Qt.Key.Key_Right: b'\x1bOC',
                Qt.Key.Key_Left: b'\x1bOD',
                Qt.Key.Key_Home: b'\x1bOH',
                Qt.Key.Key_End: b'\x1bOF',
            }
        else:
            cursor_map = {
                Qt.Key.Key_Up: b'\x1b[A',
                Qt.Key.Key_Down: b'\x1b[B',
                Qt.Key.Key_Right: b'\x1b[C',
                Qt.Key.Key_Left: b'\x1b[D',
                Qt.Key.Key_Home: b'\x1b[H',
                Qt.Key.Key_End: b'\x1b[F',
            }

        # Add control and shift modifiers for cursor keys
        if key in cursor_map:
            base_seq = cursor_map[key]
            if modifiers & Qt.KeyboardModifier.ControlModifier:
                if b'O' in base_seq:
                    mod_seq = base_seq.replace(b'O', b'[1;5')
                else:
                    mod_seq = base_seq[:-1] + b';5' + base_seq[-1:]
                self.data_ready.emit(mod_seq)
            elif modifiers & Qt.KeyboardModifier.ShiftModifier:
                if b'O' in base_seq:
                    mod_seq = base_seq.replace(b'O', b'[1;2')
                else:
                    mod_seq = base_seq[:-1] + b';2' + base_seq[-1:]
                self.data_ready.emit(mod_seq)
            else:
                self.data_ready.emit(base_seq)
            event.accept()
            return

        # Handle other special keys
        special_map: Dict[int, bytes] = {
            Qt.Key.Key_Return: b'\r',
            Qt.Key.Key_Enter: b'\r',
            Qt.Key.Key_Backspace: b'\b' if modifiers & Qt.KeyboardModifier.ControlModifier else b'\x7f',
            Qt.Key.Key_Delete: b'\x1b[3~',
            Qt.Key.Key_Insert: b'\x1b[2~',
            Qt.Key.Key_PageUp: b'\x1b[5~',
            Qt.Key.Key_PageDown: b'\x1b[6~',
            Qt.Key.Key_Tab: b'\t',
            Qt.Key.Key_Backtab: b'\x1b[Z',  # Shift+Tab
        }

        if key in special_map:
            self.data_ready.emit(special_map[key])
            event.accept()
            return

        # Handle regular text input
        if text:
            self.data_ready.emit(text.encode())

        event.accept()

    def paintEvent(self, event: QPaintEvent) -> None:
        """Handle paint events efficiently with proper floating-point character metrics."""
        painter = QPainter(self.viewport())
        buffer = self._state.current_buffer

        # Pre-calculate dimensions
        terminal_rows, terminal_cols = self._state.get_terminal_size()
        terminal_history_lines = self._state.terminal_history_lines
        first_visible_line = self.verticalScrollBar().value()

        # Get clip region and calculate visible character range
        region = event.rect()
        start_row = int(region.top() / self._char_height)
        end_row = min(terminal_rows, int((region.bottom() + self._char_height - 1) / self._char_height))
        start_col = int(region.left() / self._char_width)
        end_col = min(terminal_cols, int((region.right() + self._char_width - 1) / self._char_width))

        # Pre-create QColor objects for default colors
        default_fg = QColor(self._default_fg.rgb())
        default_bg = QColor(self._default_bg.rgb())

        # Pre-create common font variants
        base_font = painter.font()
        font_variants = {
            CharacterAttributes.NONE: self._create_font_variant(base_font),
            CharacterAttributes.BOLD: self._create_font_variant(base_font, bold=True),
            CharacterAttributes.ITALIC: self._create_font_variant(base_font, italic=True),
            CharacterAttributes.UNDERLINE: self._create_font_variant(base_font, underline=True),
            CharacterAttributes.STRIKE: self._create_font_variant(base_font, strike=True),
            CharacterAttributes.BOLD | CharacterAttributes.ITALIC:
                self._create_font_variant(base_font, bold=True, italic=True),
            CharacterAttributes.BOLD | CharacterAttributes.UNDERLINE:
                self._create_font_variant(base_font, bold=True, underline=True),
            CharacterAttributes.BOLD | CharacterAttributes.STRIKE:
                self._create_font_variant(base_font, bold=True, strike=True),
            CharacterAttributes.ITALIC | CharacterAttributes.UNDERLINE:
                self._create_font_variant(base_font, italic=True, underline=True),
            CharacterAttributes.ITALIC | CharacterAttributes.STRIKE:
                self._create_font_variant(base_font, italic=True, strike=True),
            CharacterAttributes.UNDERLINE | CharacterAttributes.STRIKE:
                self._create_font_variant(base_font, underline=True, strike=True),
            CharacterAttributes.BOLD | CharacterAttributes.ITALIC | CharacterAttributes.UNDERLINE:
                self._create_font_variant(base_font, bold=True, italic=True, underline=True),
            CharacterAttributes.BOLD | CharacterAttributes.ITALIC | CharacterAttributes.STRIKE:
                self._create_font_variant(base_font, bold=True, italic=True, strike=True),
        }

        # Batch similar characters together for efficient drawing
        for row in range(start_row, end_row):
            y = row * self._char_height  # Floating point y-position
            line_index = first_visible_line + row

            if line_index >= terminal_history_lines:
                break

            line = buffer.lines[line_index]
            current_run_start_col = start_col
            current_text = ""
            current_attrs = CharacterAttributes.NONE
            current_colors: Tuple[int | None, int | None] = (None, None)

            for col in range(start_col, end_col):
                char, attrs, fg_color, bg_color = line.get_character(col)

                # Check if this character can be batched with the current run
                colors = (fg_color, bg_color)
                if attrs == current_attrs and colors == current_colors:
                    current_text += char
                    continue

                # Draw previous run if it exists
                if current_text:
                    self._draw_character_run(
                        painter, current_run_start_col, y, current_text, current_attrs,
                        current_colors, default_fg, default_bg,
                        font_variants, row, first_visible_line
                    )

                # Start new run
                current_text = char
                current_run_start_col = col
                current_attrs = attrs
                current_colors = colors

            # Draw final run for this row
            if current_text:
                self._draw_character_run(
                    painter, current_run_start_col, y, current_text, current_attrs,
                    current_colors, default_fg, default_bg,
                    font_variants, row, first_visible_line
                )

        # Draw selection overlay if present
        if self.has_selection():
            self._draw_selection(
                painter,
                event.rect(),
                first_visible_line,
                terminal_rows,
                terminal_cols,
                terminal_history_lines
            )

        # Draw cursor if visible
        if buffer.cursor.visible and self._has_focus:
            self._draw_cursor(
                painter,
                buffer,
                terminal_rows,
                terminal_history_lines,
                first_visible_line
            )

    def _create_font_variant(
        self,
        base_font: QFont,
        bold: bool = False,
        italic: bool = False,
        underline: bool = False,
        strike: bool = False
    ) -> QFont:
        """Create and return a font variant."""
        font = QFont(base_font)
        if bold:
            font.setBold(True)

        if italic:
            font.setItalic(True)

        if underline:
            font.setUnderline(True)

        if strike:
            font.setStrikeOut(True)

        return font

    def _draw_character_run(
        self,
        painter: QPainter,
        start_col: int,
        y: float,
        text: str,
        attrs: CharacterAttributes,
        colors: tuple,
        default_fg: QColor,
        default_bg: QColor,
        font_variants: dict,
        row_index: int,
        first_visible_line: int
    ) -> None:
        """Draw a run of characters with the same attributes efficiently."""
        if not text:
            return

        # Get row index for highlights
        row = row_index + first_visible_line
        highlights = self.get_row_highlights(row)

        # Set up colors
        fg_color, bg_color = colors
        fg = (QColor(fg_color) if fg_color is not None and
            (attrs & CharacterAttributes.CUSTOM_FG) else default_fg)
        bg = (QColor(bg_color) if bg_color is not None and
            (attrs & CharacterAttributes.CUSTOM_BG) else default_bg)

        # Handle inverse video and screen reverse mode
        if attrs & CharacterAttributes.INVERSE:
            fg, bg = bg, fg

        if self._state.screen_reverse_mode:
            fg, bg = bg, fg

        # Handle hidden text
        if attrs & CharacterAttributes.HIDDEN:
            fg = bg

        # Set font based on attributes
        font_key = CharacterAttributes.NONE
        if attrs & CharacterAttributes.BOLD:
            font_key |= CharacterAttributes.BOLD

        if attrs & CharacterAttributes.ITALIC:
            font_key |= CharacterAttributes.ITALIC

        if attrs & CharacterAttributes.UNDERLINE:
            font_key |= CharacterAttributes.UNDERLINE

        if attrs & CharacterAttributes.STRIKE:
            font_key |= CharacterAttributes.STRIKE

        painter.setFont(font_variants.get(font_key, painter.font()))

        # Handle dim text
        if attrs & CharacterAttributes.DIM:
            fg.setAlpha(128)

        # Calculate start x position
        x_start = start_col * self._char_width

        # If no highlights or blinking chars, draw entire run at once
        if not highlights and not (attrs & CharacterAttributes.BLINK):
            width = len(text) * self._char_width

            # Draw background - use precise width calculation
            painter.fillRect(
                QRectF(x_start, y, width, self._char_height),
                bg
            )

            # Draw text
            if not (attrs & CharacterAttributes.BLINK and self._blink_state):
                painter.setPen(fg)
                painter.drawText(
                    QPointF(x_start, y + self._char_ascent),
                    text
                )
            return

        # Handle runs with highlights or blinking characters
        current_batch_start = 0
        current_batch_len = 0
        current_highlight_bg = bg

        for i, char in enumerate(text):
            col = start_col + i

            # Find highlight at this position
            is_highlighted = False
            is_current = False
            for start_col_hl, end_col_hl, current in highlights:
                if start_col_hl <= col < end_col_hl:
                    is_highlighted = True
                    is_current = current
                    break

            # Determine background color for this character
            char_bg = bg
            if is_highlighted:
                char_bg = self._style_manager.get_color(
                    ColorRole.TEXT_FOUND if is_current else ColorRole.TEXT_FOUND_DIM
                )

            # If background color changes or this is the last character, draw the current batch
            if char_bg != current_highlight_bg or i == len(text) - 1:
                # If this is the last character and it has the same background, include it in the batch
                if i == len(text) - 1 and char_bg == current_highlight_bg:
                    current_batch_len += 1

                # Draw the current batch if it's not empty
                if current_batch_len > 0:
                    batch_x = x_start + (current_batch_start * self._char_width)
                    batch_width = current_batch_len * self._char_width

                    # Draw background
                    painter.fillRect(
                        QRectF(batch_x, y, batch_width, self._char_height),
                        current_highlight_bg
                    )

                    # Draw text if not blinking or in visible state
                    if not (attrs & CharacterAttributes.BLINK and self._blink_state):
                        painter.setPen(fg)
                        batch_text = text[current_batch_start:current_batch_start + current_batch_len]
                        painter.drawText(
                            QPointF(batch_x, y + self._char_ascent),
                            batch_text
                        )

                # Start a new batch for the current character
                if i == len(text) - 1 and char_bg != current_highlight_bg:
                    # Draw the final character separately if it has a different background
                    painter.fillRect(
                        QRectF(x_start + (i * self._char_width), y, self._char_width, self._char_height),
                        char_bg
                    )

                    if not (attrs & CharacterAttributes.BLINK and self._blink_state):
                        painter.setPen(fg)
                        painter.drawText(
                            QPointF(x_start + (i * self._char_width), y + self._char_ascent),
                            char
                        )
                else:
                    # Start a new batch
                    current_batch_start = i
                    current_batch_len = 1
                    current_highlight_bg = char_bg
            else:
                # Continue the current batch
                current_batch_len += 1

    def _draw_selection(
        self,
        painter: QPainter,
        region: QRect,
        first_visible_line: int,
        terminal_rows: int,
        terminal_cols: int,
        terminal_history_lines: int
    ) -> None:
        """Draw text selection overlay using floating-point positioning."""
        selection = cast(TerminalSelection, self._selection).normalize()
        visible_start_row = selection.start_row - first_visible_line
        visible_end_row = selection.end_row - first_visible_line

        selection_color = self.palette().highlight().color()
        selection_text_color = self.palette().highlightedText().color()

        for row in range(max(visible_start_row, 0), min(visible_end_row + 1, terminal_rows)):
            y = row * self._char_height
            row_start = selection.start_col if row + first_visible_line == selection.start_row else 0
            row_end = selection.end_col if row + first_visible_line == selection.end_row else terminal_cols

            # Create selection rectangle using floating-point coordinates
            selection_rect = QRectF(
                row_start * self._char_width,
                y,
                (row_end - row_start) * self._char_width,
                self._char_height
            )

            if selection_rect.intersects(QRectF(region)):
                painter.fillRect(selection_rect, selection_color)
                line_index = first_visible_line + row
                if line_index < terminal_history_lines:
                    line = self._state.current_buffer.lines[line_index]
                    painter.setPen(selection_text_color)
                    for col in range(row_start, row_end):
                        char, _attrs, _fg, _bg = line.get_character(col)
                        # Draw text using floating-point position
                        painter.drawText(
                            QPointF(col * self._char_width, y + self._char_ascent),
                            char
                        )

    def _draw_cursor(
        self,
        painter: QPainter,
        buffer,
        terminal_rows: int,
        terminal_history_lines: int,
        first_visible_line: int
    ) -> None:
        """Draw terminal cursor using floating-point positioning."""
        # Don't draw if cursor should be hidden
        if (not buffer.cursor.visible or
            not self._has_focus or
            (buffer.cursor.blink and not self._blink_state)):
            return

        cursor_line = terminal_history_lines - terminal_rows + buffer.cursor.row
        visible_cursor_row = cursor_line - first_visible_line

        if 0 <= visible_cursor_row < terminal_rows:
            # Calculate cursor position using floating-point coordinates
            cursor_x = buffer.cursor.col * self._char_width
            cursor_y = visible_cursor_row * self._char_height

            if cursor_line < terminal_history_lines:
                line = buffer.lines[cursor_line]
                char, _attrs, _fg, _bg = line.get_character(buffer.cursor.col)

                # Draw inverted cursor using floating-point rectangle
                painter.fillRect(
                    QRectF(cursor_x, cursor_y, self._char_width, self._char_height),
                    self._style_manager.get_color(ColorRole.TEXT_PRIMARY)
                )
                painter.setPen(self._style_manager.get_color(ColorRole.TAB_BACKGROUND_ACTIVE))
                painter.drawText(
                    QPointF(cursor_x, cursor_y + self._char_ascent),
                    char
                )

    def get_selected_text(self) -> str:
        """Get currently selected text."""
        if not self.has_selection():
            return ""

        buffer = self._state.current_buffer
        terminal_cols = self._state.terminal_columns
        terminal_history_lines = self._state.terminal_history_lines

        # Get normalized selection
        selection = cast(TerminalSelection, self._selection).normalize()

        # Build selected text
        text = []
        for row in range(selection.start_row, selection.end_row + 1):
            if row >= terminal_history_lines:
                break

            line = buffer.lines[row]
            start = selection.start_col if row == selection.start_row else 0
            end = selection.end_col if row == selection.end_row else terminal_cols

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

        text = self.get_selected_text()
        if text:
            QGuiApplication.clipboard().setText(text)

    def paste(self) -> None:
        """Paste text from clipboard."""
        text = QGuiApplication.clipboard().text()
        if text:
            # Handle bracketed paste mode
            if self._state.bracketed_paste_mode:
                self.data_ready.emit(b'\x1b[200~')  # Start bracketed paste
                self.data_ready.emit(text.encode())
                self.data_ready.emit(b'\x1b[201~')  # End bracketed paste
            else:
                self.data_ready.emit(text.encode())

    def _show_terminal_context_menu(self, pos) -> None:
        """Show context menu for terminal operations."""
        menu = QMenu(self)

        # Copy action
        copy_action = menu.addAction("Copy")
        copy_action.setEnabled(self.has_selection())
        copy_action.triggered.connect(self.copy)

        # Paste action
        paste_action = menu.addAction("Paste")
        paste_action.setEnabled(True)
        paste_action.triggered.connect(self.paste)

        # Show menu at click position
        menu.exec_(self.mapToGlobal(pos))

    def put_data(self, data: bytes) -> None:
        """Display received data with ANSI sequence handling."""
        self._state.put_data(data)
        self.viewport().update()
        self._update_scrollbar()

    def resizeEvent(self, event: QResizeEvent) -> None:
        """Handle resize events."""
        super().resizeEvent(event)
        self.update_dimensions()
        self.viewport().update()

    def focusNextPrevChild(self, _next: bool) -> bool:
        """Override to prevent tab from changing focus."""
        return False

    def focusInEvent(self, event):
        """Handle focus in event."""
        super().focusInEvent(event)
        self._has_focus = True
        self.viewport().update()

    def focusOutEvent(self, event):
        """Handle focus out event."""
        super().focusOutEvent(event)
        self._has_focus = False
        self.viewport().update()

    def get_terminal_size(self) -> Tuple[int, int]:
        """Get current terminal dimensions."""
        return self._state.get_terminal_size()

    def create_state_metadata(self) -> Dict:
        """Create metadata dictionary capturing widget state."""
        return self._state.create_state_metadata()

    def restore_from_metadata(self, metadata: Dict) -> None:
        """Restore terminal state from metadata."""
        self._state.restore_from_metadata(metadata)
        self._clear_selection()
        self.viewport().update()
        self._update_scrollbar()

    def get_title(self) -> str:
        """Get current terminal title."""
        return self._state.terminal_title

    def get_current_directory(self) -> str | None:
        """Get current working directory if known."""
        return self._state._current_directory

    def set_search_highlights(self, row: int, highlights: List[Tuple[int, int, bool]]) -> None:
        """Set search highlights for a given row.

        Args:
            row: Row to set highlights for
            highlights: List of (start_col, end_col, is_current) highlight ranges
        """
        if highlights:
            self._search_highlights[row] = highlights
        else:
            self._search_highlights.pop(row, None)

        self.viewport().update()

    def clear_search_highlights(self) -> None:
        """Clear all search highlights."""
        self._search_highlights.clear()
        self.viewport().update()

    def get_row_highlights(self, row: int) -> List[Tuple[int, int, bool]]:
        """Get highlights for a given row.

        Args:
            row: Row to get highlights for

        Returns:
            List of (start_col, end_col, format) highlight ranges
        """
        return self._search_highlights.get(row, [])

    # Integrated find functionality methods
    def find_text(self, text: str, forward: bool = True) -> None:
        """
        Find all instances of text and highlight them.

        Args:
            text: Text to search for
            forward: Whether to search forward from current position
        """
        # Clear existing highlights if search text changed
        if text != self._last_search:
            self.clear_search_highlights()
            self._matches = []
            self._current_match = -1
            self._last_search = text

            # Find all matches
            if text:
                buffer = self._state.current_buffer
                rows = buffer.history_lines

                # Search through all lines in buffer
                for row in range(rows):
                    line = buffer.lines[row]
                    line_text = ""

                    # Build text for this line
                    for col in range(buffer.cols):
                        char, _, _, _ = line.get_character(col)
                        line_text += char

                    # Find all matches in this line
                    pos = 0
                    while True:
                        pos = line_text.find(text, pos)
                        if pos == -1:
                            break

                        # Add match to list
                        self._matches.append(TerminalMatch(
                            row=row,
                            start_col=pos,
                            end_col=pos + len(text),
                            is_current=False  # All matches start as non-current
                        ))
                        pos += 1

                # Set selection active if we found any matches
                self._selection_active = bool(self._matches)

        if not self._matches:
            return

        # Move to next/previous match
        if self._current_match == -1:
            # First search - start at beginning or end depending on direction
            self._current_match = 0 if forward else len(self._matches) - 1
        else:
            # Move to next/previous match
            if forward:
                self._current_match = (self._current_match + 1) % len(self._matches)
            else:
                self._current_match = (self._current_match - 1) if self._current_match > 0 else len(self._matches) - 1

        # Update current match status
        self._update_current_match()

        # Update highlights
        self._update_highlights()

        # Ensure current match is visible
        self._scroll_to_match()

    def _update_current_match(self) -> None:
        """Update which match is marked as current."""
        for i, match in enumerate(self._matches):
            match.is_current = (i == self._current_match)

    def _update_highlights(self) -> None:
        """Update highlight display in terminal."""
        # Clear existing highlights
        self.clear_search_highlights()

        if not self._selection_active:
            return

        # Group matches by row
        row_matches: Dict[int, List[Tuple[int, int, bool]]] = {}
        for match in self._matches:
            if match.row not in row_matches:
                row_matches[match.row] = []

            row_matches[match.row].append(
                (match.start_col, match.end_col, match.is_current)
            )

        # Update terminal with new highlights
        for row, highlights in row_matches.items():
            self.set_search_highlights(row, highlights)

    def clear_find(self) -> None:
        """Clear all find state."""
        self._matches = []
        self._current_match = -1
        self._last_search = ""
        self._selection_active = False
        self.clear_search_highlights()

    def _scroll_to_match(self) -> None:
        """Scroll to ensure the current match is visible."""
        if not self._matches or self._current_match == -1:
            return

        match = self._matches[self._current_match]
        self.scroll_to_match(match.row)

    def get_match_status(self) -> Tuple[int, int]:
        """
        Get the current match status.

        Returns:
            Tuple of (current_match, total_matches)
        """
        if not self._matches:
            return 0, 0

        return self._current_match + 1, len(self._matches)
