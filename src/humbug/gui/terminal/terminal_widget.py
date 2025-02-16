"""Terminal widget implementation."""

import logging
from typing import Optional, Tuple, Dict

from PySide6.QtWidgets import QWidget, QAbstractScrollArea, QMenu
from PySide6.QtCore import Qt, Signal, QRect, QPoint, QTimer
from PySide6.QtGui import (
    QPainter, QPaintEvent, QColor, QFontMetrics,
    QResizeEvent, QKeyEvent, QMouseEvent,
    QGuiApplication, QWheelEvent
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
              else QColor(self._default_fg.rgb()))
        bg = (QColor(bg_color) if bg_color is not None and (attributes & CharacterAttributes.CUSTOM_BG)
              else QColor(self._default_bg.rgb()))

        # Handle inverse video by swapping foreground and background colors
        if attributes & CharacterAttributes.INVERSE:
            fg, bg = bg, fg

        # Handle global screen reverse mode - swap colors if enabled
        if self._state.screen_reverse_mode:
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
        buffer = self._state.current_buffer

        terminal_rows, terminal_cols = self._state.get_terminal_size()
        terminal_history_lines = self._state.terminal_history_lines

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
        end_row = min(terminal_rows, (region.bottom() + char_height - 1) // char_height)
        start_col = max(0, region.left() // char_width)
        end_col = min(terminal_cols, (region.right() + char_width - 1) // char_width)

        # Paint visible character cells
        for row in range(start_row, end_row):
            y = row * char_height

            # Get actual line index accounting for scroll position
            line_index = first_visible_line + row
            if line_index >= terminal_history_lines:
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

            for row in range(max(visible_start_row, 0), min(visible_end_row + 1, terminal_rows)):
                y = row * char_height

                # Calculate selection range for this row
                row_start = selection.start_col if row + first_visible_line == selection.start_row else 0
                row_end = selection.end_col if row + first_visible_line == selection.end_row else terminal_cols

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
                    if line_index < terminal_history_lines:
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
            cursor_line = terminal_history_lines - terminal_rows + buffer.cursor.row
            visible_cursor_row = cursor_line - first_visible_line

            if 0 <= visible_cursor_row < terminal_rows:  # Only draw if cursor is in visible area
                cursor_x = buffer.cursor.col * char_width
                cursor_y = visible_cursor_row * char_height

                cursor_rect = QRect(cursor_x, cursor_y, char_width, char_height)
                if cursor_rect.intersects(region):
                    # Get character under cursor for inversion
                    if cursor_line < terminal_history_lines:
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
