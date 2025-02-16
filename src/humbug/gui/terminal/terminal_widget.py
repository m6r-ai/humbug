"""Terminal widget implementation."""

import logging
from typing import Optional, Tuple, Dict

from PySide6.QtWidgets import QWidget, QAbstractScrollArea, QMenu
from PySide6.QtCore import Qt, Signal, QRect, QPoint, QTimer
from PySide6.QtGui import (
    QPainter, QPaintEvent, QColor, QFontMetrics,
    QResizeEvent, QKeyEvent, QMouseEvent,
    QGuiApplication, QWheelEvent, QFont
)

from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager
from humbug.gui.terminal.terminal_selection import TerminalSelection
from humbug.gui.terminal.terminal_state import TerminalState
from humbug.gui.terminal.terminal_buffer import CharacterAttributes


class TerminalWidget(QAbstractScrollArea):
    """Terminal widget implementation."""

    data_ready = Signal(bytes)  # Emitted when user input is ready
    size_changed = Signal()  # Emitted when terminal size changes

    def __init__(self, parent: Optional[QWidget] = None):
        """Initialize terminal widget."""
        super().__init__(parent)
        self._logger = logging.getLogger("TerminalWidget")
        self._style_manager = StyleManager()

        # Set up scrollbar behavior
        self.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOn)
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        self.verticalScrollBar().valueChanged.connect(self._handle_scroll)
        self.setFocusPolicy(Qt.StrongFocus)

        # Initialize terminal state
        self._state = TerminalState(24, 80)  # Default size

        # Selection state
        self._selection: Optional[TerminalSelection] = None
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

        # Initialize size and connect signals
        self.setContextMenuPolicy(Qt.CustomContextMenu)
        self.customContextMenuRequested.connect(self._show_terminal_context_menu)
        self._style_manager.style_changed.connect(self._handle_style_changed)
        self._handle_style_changed()

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
        # Update default colors
        self._default_fg = self._style_manager.get_color(ColorRole.TEXT_PRIMARY)
        self._default_bg = self._style_manager.get_color(ColorRole.TAB_BACKGROUND_ACTIVE)

        # Update color mappings in state
        self._update_colors()

        # Force redraw with new colors
        self.viewport().update()

    def update_dimensions(self) -> None:
        """Update terminal dimensions based on widget size and font metrics."""
        fm = QFontMetrics(self.font())
        char_width = fm.horizontalAdvance(' ')
        char_height = fm.height()

        rows = 24  # Default dimensions
        cols = 80

        if char_width > 0 and char_height > 0:
            # Get the width of the vertical scrollbar
            scrollbar_width = self.verticalScrollBar().width()

            # Calculate available viewport width, subtracting scrollbar width
            viewport_width = max(0, self.width() - scrollbar_width)
            viewport_height = self.height()

            cols = max(viewport_width // char_width, 1)
            rows = max(viewport_height // char_height, 1)

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

    def _scroll_to_bottom(self):
        """Scroll the view to show the bottom of the terminal."""
        vbar = self.verticalScrollBar()
        vbar.setValue(vbar.maximum())

    def _toggle_blink(self):
        """Toggle blink state and update display if needed."""
        self._blink_state = not self._blink_state
        if self._state.blinking_chars_on_screen():
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

        terminal_rows, terminal_cols = self._state.get_terminal_size()

        # Convert pixel position to viewport row/col
        viewport_col = max(0, min(pos.x() // char_width, terminal_cols - 1))
        viewport_row = max(0, min(pos.y() // char_height, terminal_rows - 1))

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
        if event.button() == Qt.LeftButton:
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
            pos = self._pixel_pos_to_text_pos(event.position().toPoint())
            if (pos[0] != self._selection.end_row or
                pos[1] != self._selection.end_col):
                self._selection.end_row = pos[0]
                self._selection.end_col = pos[1]
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
            if buttons & Qt.LeftButton:
                btn_num = 32
            elif buttons & Qt.MiddleButton:
                btn_num = 33
            elif buttons & Qt.RightButton:
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
        if self._state.application_keypad_mode and not modifiers:
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

        # Handle cursor keys based on mode
        if self._state.application_cursor_mode:
            cursor_map = {
                Qt.Key_Up: b'\x1bOA',
                Qt.Key_Down: b'\x1bOB',
                Qt.Key_Right: b'\x1bOC',
                Qt.Key_Left: b'\x1bOD'
            }
        else:
            cursor_map = {
                Qt.Key_Up: b'\x1b[A',
                Qt.Key_Down: b'\x1b[B',
                Qt.Key_Right: b'\x1b[C',
                Qt.Key_Left: b'\x1b[D'
            }

        if key in cursor_map:
            self.data_ready.emit(cursor_map[key])
            event.accept()
            return

        # Handle other special keys
        special_map = {
            Qt.Key_Return: b'\r',
            Qt.Key_Enter: b'\r',
            Qt.Key_Backspace: b'\x7f' if modifiers & Qt.ControlModifier else b'\b',
            Qt.Key_Delete: b'\x1b[3~',
            Qt.Key_Tab: b'\t'
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
        """Handle paint events efficiently."""
        import time
        py_start = time.perf_counter()
        painter = QPainter(self.viewport())
        buffer = self._state.current_buffer

        # Get font metrics for character dimensions - cache these values
        fm = QFontMetrics(self.font())
        char_width = fm.horizontalAdvance(' ')
        char_height = fm.height()
        ascent = fm.ascent()

        # Pre-calculate dimensions
        terminal_rows, terminal_cols = self._state.get_terminal_size()
        terminal_history_lines = self._state.terminal_history_lines
        first_visible_line = self.verticalScrollBar().value()

        # Get clip region and calculate visible character range
        region = event.rect()
        start_row = region.top() // char_height
        end_row = min(terminal_rows, (region.bottom() + char_height - 1) // char_height)
        start_col = region.left() // char_width
        end_col = min(terminal_cols, (region.right() + char_width - 1) // char_width)

        # Pre-create QColor objects for default colors
        default_fg = QColor(self._default_fg.rgb())
        default_bg = QColor(self._default_bg.rgb())

        # Create a reusable QRect for character cells
        char_rect = QRect(0, 0, char_width, char_height)

        # Pre-create common font variants
        base_font = painter.font()
        font_variants = {
            CharacterAttributes.BOLD: self._create_font_variant(base_font, bold=True),
            CharacterAttributes.ITALIC: self._create_font_variant(base_font, italic=True),
            CharacterAttributes.UNDERLINE: self._create_font_variant(base_font, underline=True),
            CharacterAttributes.STRIKE: self._create_font_variant(base_font, strike=True),
            CharacterAttributes.BOLD | CharacterAttributes.ITALIC: self._create_font_variant(base_font, bold=True, italic=True),
            CharacterAttributes.BOLD | CharacterAttributes.UNDERLINE: self._create_font_variant(base_font, bold=True, underline=True),
            CharacterAttributes.BOLD | CharacterAttributes.STRIKE: self._create_font_variant(base_font, bold=True, strike=True),
            CharacterAttributes.ITALIC | CharacterAttributes.UNDERLINE: self._create_font_variant(base_font, italic=True, underline=True),
            CharacterAttributes.ITALIC | CharacterAttributes.STRIKE: self._create_font_variant(base_font, italic=True, strike=True),
            CharacterAttributes.UNDERLINE | CharacterAttributes.STRIKE: self._create_font_variant(base_font, underline=True, strike=True),
            CharacterAttributes.BOLD | CharacterAttributes.ITALIC | CharacterAttributes.UNDERLINE: self._create_font_variant(base_font, bold=True, italic=True, underline=True),
            CharacterAttributes.BOLD | CharacterAttributes.ITALIC | CharacterAttributes.STRIKE: self._create_font_variant(base_font, bold=True, italic=True, strike=True),
            # Add other common combinations as needed
        }

        # Batch similar characters together
        for row in range(start_row, end_row):
            y = row * char_height
            line_index = first_visible_line + row

            if line_index >= terminal_history_lines:
                break

            line = buffer.lines[line_index]
            current_run = []
            current_attrs = None
            current_colors = None

            for col in range(start_col, end_col):
                x = col * char_width
                char, attrs, fg_color, bg_color = line.get_character(col)

                # We can skip processing if:
                # 1. Character is a space
                # 2. Has default attributes
                # 3. Has default colors
                # 4. Screen reverse mode is OFF
                # Otherwise spaces need to be painted for correct background color
                if (char == ' ' and attrs == CharacterAttributes.NONE and 
                    fg_color is None and bg_color is None and
                    not self._state.screen_reverse_mode):
                    if current_run:
                        self._draw_character_run(painter, current_run, current_attrs, 
                                            current_colors, char_width, char_height, 
                                            ascent, default_fg, default_bg, 
                                            font_variants)
                        current_run = []
                    # If we're skipping this space, still need to paint default background
                    painter.fillRect(x, y, char_width, char_height, default_bg)
                    continue

                # Check if this character can be batched with the current run
                colors = (fg_color, bg_color)
                if attrs == current_attrs and colors == current_colors:
                    current_run.append((char, x, y))
                else:
                    # Draw previous run if it exists
                    if current_run:
                        self._draw_character_run(painter, current_run, current_attrs,
                                            current_colors, char_width, char_height,
                                            ascent, default_fg, default_bg,
                                            font_variants)
                    # Start new run
                    current_run = [(char, x, y)]
                    current_attrs = attrs
                    current_colors = colors

            # Draw final run for this row
            if current_run:
                self._draw_character_run(painter, current_run, current_attrs,
                                    current_colors, char_width, char_height,
                                    ascent, default_fg, default_bg, font_variants)

        # Draw selection overlay if present
        if self.has_selection():
            self._draw_selection(painter, event.rect(), char_width, char_height,
                            first_visible_line, terminal_rows, terminal_cols,
                            terminal_history_lines, ascent)

        # Draw cursor if visible
        if buffer.cursor.visible:
            self._draw_cursor(painter, buffer, char_width, char_height,
                            terminal_rows, terminal_history_lines,
                            first_visible_line, ascent)

        py_elapsed = (time.perf_counter() - py_start) * 1000
        print(f"painter elapsed {py_elapsed:.2f}")

    def _create_font_variant(self, base_font: QFont, bold: bool = False,
                            italic: bool = False, underline: bool = False,
                            strike: bool = False) -> QFont:
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

    def _draw_character_run(self, painter: QPainter, run: list, attrs: CharacterAttributes,
                        colors: tuple, char_width: int, char_height: int, ascent: int,
                        default_fg: QColor, default_bg: QColor, font_variants: dict) -> None:
        """Draw a run of characters with the same attributes efficiently."""
        if not run:
            return

        # Set up colors
        fg_color, bg_color = colors
        fg = (QColor(fg_color) if fg_color is not None and
            (attrs & CharacterAttributes.CUSTOM_FG) else default_fg)
        bg = (QColor(bg_color) if bg_color is not None and
            (attrs & CharacterAttributes.CUSTOM_BG) else default_bg)

        # Handle inverse video
        if attrs & CharacterAttributes.INVERSE:
            fg, bg = bg, fg

        # Handle screen reverse mode
        if self._state.screen_reverse_mode:
            fg, bg = bg, fg

        # Handle hidden text
        if attrs & CharacterAttributes.HIDDEN:
            fg = bg

        # Draw background for entire run
        x_start = run[0][1]
        y = run[0][2]
        width = (run[-1][1] - x_start) + char_width
        painter.fillRect(x_start, y, width, char_height, bg)

        # Handle dim text
        if attrs & CharacterAttributes.DIM:
            fg.setAlpha(128)

        # Set text color
        painter.setPen(fg)

        # Set font based on attributes
        # Build font key from relevant attributes
        font_key = CharacterAttributes.NONE
        if attrs & CharacterAttributes.BOLD:
            font_key |= CharacterAttributes.BOLD

        if attrs & CharacterAttributes.ITALIC:
            font_key |= CharacterAttributes.ITALIC

        if attrs & CharacterAttributes.UNDERLINE:
            font_key |= CharacterAttributes.UNDERLINE

        if attrs & CharacterAttributes.STRIKE:
            font_key |= CharacterAttributes.STRIKE

        # Use cached font variant if available, otherwise create base font with attributes
        if font_key in font_variants:
            painter.setFont(font_variants[font_key])
        else:
            font = QFont(painter.font())
            font.setBold(bool(attrs & CharacterAttributes.BOLD))
            font.setItalic(bool(attrs & CharacterAttributes.ITALIC))
            font.setUnderline(bool(attrs & CharacterAttributes.UNDERLINE))
            font.setStrikeOut(bool(attrs & CharacterAttributes.STRIKE))
            painter.setFont(font)

        # Draw all characters in run
        if not (attrs & CharacterAttributes.BLINK and self._blink_state):
            for char, x, y in run:
                painter.drawText(x, y + ascent, char)

    def _draw_selection(self, painter: QPainter, region: QRect, char_width: int,
                    char_height: int, first_visible_line: int, terminal_rows: int,
                    terminal_cols: int, terminal_history_lines: int, ascent: int) -> None:
        """Draw text selection overlay."""
        selection = self._selection.normalize()
        visible_start_row = selection.start_row - first_visible_line
        visible_end_row = selection.end_row - first_visible_line

        selection_color = self.palette().highlight().color()
        selection_text_color = self.palette().highlightedText().color()

        for row in range(max(visible_start_row, 0), min(visible_end_row + 1, terminal_rows)):
            y = row * char_height
            row_start = selection.start_col if row + first_visible_line == selection.start_row else 0
            row_end = selection.end_col if row + first_visible_line == selection.end_row else terminal_cols

            selection_rect = QRect(
                row_start * char_width,
                y,
                (row_end - row_start) * char_width,
                char_height
            )

            if selection_rect.intersects(region):
                painter.fillRect(selection_rect, selection_color)
                line_index = first_visible_line + row
                if line_index < terminal_history_lines:
                    line = self._state.current_buffer.lines[line_index]
                    painter.setPen(selection_text_color)
                    for col in range(row_start, row_end):
                        char, _attrs, _fg, _bg = line.get_character(col)
                        painter.drawText(col * char_width, y + ascent, char)

    def _draw_cursor(self, painter: QPainter, buffer, char_width: int, char_height: int,
                    terminal_rows: int, terminal_history_lines: int,
                    first_visible_line: int, ascent: int) -> None:
        """Draw terminal cursor."""
        cursor_line = terminal_history_lines - terminal_rows + buffer.cursor.row
        visible_cursor_row = cursor_line - first_visible_line

        if 0 <= visible_cursor_row < terminal_rows:
            cursor_x = buffer.cursor.col * char_width
            cursor_y = visible_cursor_row * char_height

            if cursor_line < terminal_history_lines:
                line = buffer.lines[cursor_line]
                char, _attrs, _fg, _bg = line.get_character(buffer.cursor.col)

                painter.fillRect(
                    cursor_x, cursor_y, char_width, char_height,
                    self.palette().text().color()
                )
                painter.setPen(self.palette().base().color())
                painter.drawText(cursor_x, cursor_y + ascent, char)

    def _get_selected_text(self) -> str:
        """Get currently selected text."""
        if not self.has_selection():
            return ""

        buffer = self._state.current_buffer
        terminal_cols = self._state.terminal_columns
        terminal_history_lines = self._state.terminal_history_lines

        # Get normalized selection
        selection = self._selection.normalize()

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

        text = self._get_selected_text()
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

    def get_current_directory(self) -> Optional[str]:
        """Get current working directory if known."""
        return self._state._current_directory
