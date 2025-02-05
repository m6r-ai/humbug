"""Terminal widget implementation."""

from typing import Optional, Tuple
from dataclasses import dataclass
import re
import logging

from PySide6.QtWidgets import QPlainTextEdit, QWidget
from PySide6.QtCore import Signal, Qt
from PySide6.QtGui import (
    QTextCursor, QKeyEvent, QColor, QFont, QTextCharFormat,
    QMouseEvent
)

logger = logging.getLogger(__name__)


@dataclass
class TerminalColors:
    """ANSI terminal colors."""
    BLACK = QColor(0, 0, 0)
    RED = QColor(205, 0, 0)
    GREEN = QColor(0, 205, 0)
    YELLOW = QColor(205, 205, 0)
    BLUE = QColor(0, 0, 238)
    MAGENTA = QColor(205, 0, 205)
    CYAN = QColor(0, 205, 205)
    WHITE = QColor(229, 229, 229)

    BRIGHT_BLACK = QColor(127, 127, 127)
    BRIGHT_RED = QColor(255, 0, 0)
    BRIGHT_GREEN = QColor(0, 255, 0)
    BRIGHT_YELLOW = QColor(255, 255, 0)
    BRIGHT_BLUE = QColor(92, 92, 255)
    BRIGHT_MAGENTA = QColor(255, 0, 255)
    BRIGHT_CYAN = QColor(0, 255, 255)
    BRIGHT_WHITE = QColor(255, 255, 255)


class TerminalWidget(QPlainTextEdit):
    """Terminal display widget."""

    # Signal emitted when user input is ready
    data_ready = Signal(bytes)
    # Signal emitted for mouse events when tracking is enabled
    mouse_event = Signal(str)

    def __init__(self, parent: Optional[QWidget] = None):
        """Initialize terminal widget."""
        super().__init__(parent)
        self.setLineWrapMode(QPlainTextEdit.NoWrap)

        # Terminal colors
        self._colors = TerminalColors()

        # Set up default text format
        self._default_text_format = QTextCharFormat()
        self._default_text_format.setForeground(self._colors.WHITE)
        self._default_text_format.setBackground(self._colors.BLACK)
        self._default_text_format.setFontWeight(QFont.Normal)
        self._default_text_format.setFontUnderline(False)
        self._default_text_format.setFontItalic(False)

        # Current text format (initialized to default)
        self._current_text_format = self._default_text_format

        # Set up default appearance
        self.setStyleSheet("""
            QPlainTextEdit {
                background-color: black;
                color: #E5E5E5;
                border: none;
            }
        """)

        # Configure cursor
        self.setCursorWidth(8)

        # ANSI escape sequence handling
        self._escape_seq_buffer = ""
        self._in_escape_seq = False
        self._saved_cursor_position = None

        # Additional terminal state
        self._alternate_screen_buffer = ""
        self._main_screen_buffer = ""
        self._using_alternate_screen = False
        self._scroll_region: Optional[Tuple[int, int]] = None
        self._application_cursor_keys = False
        self._mouse_tracking = False
        self._mouse_tracking_sgr = False
        self._saved_mouse_tracking = False
        self._saved_mouse_tracking_sgr = False

    def keyPressEvent(self, event: QKeyEvent):
        """Send all keypresses to the process."""
        text = event.text()
        key = event.key()

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
            elif key == Qt.Key_Backspace:
                self.data_ready.emit(b'\b')
            elif key == Qt.Key_Delete:
                self.data_ready.emit(b'\x7f')
            elif text:
                self.data_ready.emit(text.encode())
        else:
            # Normal mode key handling
            if key == Qt.Key_Backspace:
                self.data_ready.emit(b'\b')
            elif key == Qt.Key_Delete:
                self.data_ready.emit(b'\x7f')
            elif text:
                self.data_ready.emit(text.encode())

        event.accept()

    def mousePressEvent(self, event: QMouseEvent):
        """Handle mouse events when tracking is enabled."""
        if self._mouse_tracking:
            pos = event.pos()
            char_width = self.fontMetrics().horizontalAdvance(' ')
            char_height = self.fontMetrics().height()
            x = pos.x() // char_width + 1
            y = pos.y() // char_height + 1
            button = event.button()

            if self._mouse_tracking_sgr:
                # SGR mouse mode
                if button == Qt.LeftButton:
                    self.mouse_event.emit(f'\x1b[<0;{x};{y}M')
                elif button == Qt.RightButton:
                    self.mouse_event.emit(f'\x1b[<2;{x};{y}M')
                elif button == Qt.MiddleButton:
                    self.mouse_event.emit(f'\x1b[<1;{x};{y}M')
            else:
                # Normal mouse mode
                cb = 0  # Left button
                if button == Qt.RightButton:
                    cb = 2
                elif button == Qt.MiddleButton:
                    cb = 1
                self.mouse_event.emit(f'\x1b[M{chr(32+cb)}{chr(32+x)}{chr(32+y)}')
        else:
            super().mousePressEvent(event)

    def mouseReleaseEvent(self, event: QMouseEvent):
        """Handle mouse release events when tracking is enabled."""
        if self._mouse_tracking:
            pos = event.pos()
            char_width = self.fontMetrics().horizontalAdvance(' ')
            char_height = self.fontMetrics().height()
            x = pos.x() // char_width + 1
            y = pos.y() // char_height + 1

            if self._mouse_tracking_sgr:
                # SGR mouse mode release
                self.mouse_event.emit(f'\x1b[<0;{x};{y}m')
            else:
                # Normal mouse mode release
                self.mouse_event.emit(f'\x1b[M{chr(32+3)}{chr(32+x)}{chr(32+y)}')
        else:
            super().mouseReleaseEvent(event)

    def put_data(self, data: bytes):
        """Display received data with ANSI sequence handling."""
        text = data.decode(errors='replace')

        for char in text:
            if self._in_escape_seq:
                self._escape_seq_buffer += char
                if char.isalpha() or char == 'm' or char == 'h' or char == 'l':
                    self._process_escape_sequence(self._escape_seq_buffer)
                    self._escape_seq_buffer = ""
                    self._in_escape_seq = False
            elif char == '\x1b':  # ESC
                self._in_escape_seq = True
                self._escape_seq_buffer = char
            else:
                self._insert_plain_text(char)

        self.ensureCursorVisible()

    def _insert_plain_text(self, text: str):
        """Insert text at current cursor position with scroll region support."""
        cursor = self.textCursor()

        # Apply the current text format
        cursor.mergeCharFormat(self._current_text_format)

        if text == '\r':
            # Move to start of line
            cursor.movePosition(QTextCursor.StartOfLine)
        elif text == '\n':
            # Handle newline with scroll region support
            if self._scroll_region is not None:
                top, bottom = self._scroll_region
                current_line = cursor.blockNumber()

                if current_line == bottom:
                    # At bottom of scroll region, need to scroll
                    cursor.movePosition(QTextCursor.Start)
                    for _ in range(top):
                        cursor.movePosition(QTextCursor.NextBlock)
                    scroll_start = cursor.position()

                    cursor.movePosition(QTextCursor.Start)
                    for _ in range(top + 1):
                        cursor.movePosition(QTextCursor.NextBlock)
                    scroll_text_start = cursor.position()

                    cursor.movePosition(QTextCursor.Start)
                    for _ in range(bottom + 1):
                        cursor.movePosition(QTextCursor.NextBlock)
                    scroll_end = cursor.position()

                    # Select and copy the text to be scrolled
                    cursor.setPosition(scroll_text_start)
                    cursor.setPosition(scroll_end, QTextCursor.KeepAnchor)
                    text_to_move = cursor.selectedText()

                    # Delete old content and insert at new position
                    cursor.setPosition(scroll_start)
                    cursor.setPosition(scroll_end, QTextCursor.KeepAnchor)
                    cursor.removeSelectedText()
                    cursor.setPosition(scroll_start)
                    cursor.insertText(text_to_move)

                    # Move to the end of the line
                    cursor.movePosition(QTextCursor.EndOfLine)
                else:
                    # Normal newline behavior
                    cursor.movePosition(QTextCursor.EndOfLine)
                    cursor.insertText('\n')
            else:
                # No scroll region, normal newline
                cursor.movePosition(QTextCursor.EndOfLine)
                cursor.insertText('\n')
            cursor.movePosition(QTextCursor.StartOfLine)
        elif text == '\b':  # Backspace
            cursor.movePosition(QTextCursor.Left)
            cursor.deleteChar()
        elif text == '\t':  # Tab
            # Insert 8 spaces for tab
            cursor.insertText(' ' * 8)
        elif text == '\x0b':  # Vertical tab
            cursor.movePosition(QTextCursor.Down)
        elif text == '\x0c':  # Form feed
            self.clear()
        else:
            cursor.insertText(text)

        self.setTextCursor(cursor)

    def _process_escape_sequence(self, sequence: str):
        """Handle ANSI escape sequences."""
        # Clear scrollback buffer
        if sequence == '\x1b[3J':
            cursor = self.textCursor()
            cursor.movePosition(QTextCursor.Start)
            cursor.movePosition(QTextCursor.End, QTextCursor.KeepAnchor)
            visible_content = cursor.selectedText()
            self.clear()
            self.insertPlainText(visible_content)
            return

        # Move cursor to home position
        if sequence == '\x1b[H':
            cursor = self.textCursor()
            cursor.movePosition(QTextCursor.Start)
            self.setTextCursor(cursor)
            return

        # Enable alternate screen buffer
        if sequence == '\x1b[?1049h':
            if not self._using_alternate_screen:
                self._main_screen_buffer = self.toPlainText()
                self.clear()
                self._using_alternate_screen = True
            return

        # Set scrolling region
        if sequence.startswith('\x1b[') and sequence.endswith('r'):
            try:
                params = sequence[2:-1].split(';')
                if len(params) == 2:
                    top = int(params[0]) - 1  # Convert to 0-based
                    bottom = int(params[1]) - 1
                    self._scroll_region = (top, bottom)
            except (ValueError, IndexError):
                self._scroll_region = None
            return

        # Reset insert mode
        if sequence == '\x1b[4l':
            self.setOverwriteMode(True)
            return

        # Move cursor to specific position
        if sequence.startswith('\x1b[') and sequence.endswith('H'):
            try:
                params = sequence[2:-1].split(';')
                if len(params) == 2:
                    row = int(params[0]) - 1  # Convert to 0-based
                    col = int(params[1]) - 1
                    cursor = self.textCursor()
                    cursor.movePosition(QTextCursor.Start)
                    for _ in range(row):
                        cursor.movePosition(QTextCursor.NextBlock)
                    cursor.movePosition(QTextCursor.Right, n=col)
                    self.setTextCursor(cursor)
            except (ValueError, IndexError):
                pass
            return

        # Set US ASCII character set (no action needed)
        if sequence == '\x1b(B':
            return

        # Disable alternate screen buffer
        if sequence == '\x1b[?1049l':
            if self._using_alternate_screen:
                self._alternate_screen_buffer = self.toPlainText()
                self.clear()
                self.setPlainText(self._main_screen_buffer)
                self._using_alternate_screen = False
            return

        # Save mouse tracking settings
        if sequence == '\x1b[?1001s':
            self._saved_mouse_tracking = self._mouse_tracking
            self._saved_mouse_tracking_sgr = self._mouse_tracking_sgr
            return

        # Enable mouse button tracking
        if sequence == '\x1b[?1002h':
            self._mouse_tracking = True
            return

        # Enable SGR mouse mode
        if sequence == '\x1b[?1006h':
            self._mouse_tracking_sgr = True
            return

        # Enable application keypad mode
        if sequence == '\x1b=':
            self._application_cursor_keys = True
            return

        # Handle basic color codes
        if sequence.startswith('\x1b[') and sequence.endswith('m'):
            self._handle_sgr_sequence(sequence[2:-1])
            return

        # Cursor movement
        if sequence.startswith('\x1b[') and sequence[-1] in 'ABCD':
            self._handle_cursor_sequence(sequence)
            return

        # Clear screen (entire screen)
        if sequence == '\x1b[2J':
            self.clear()
            return

        # Clear from cursor to end of screen
        if sequence == '\x1b[J':
            self._clear_to_end_of_screen()
            return

        # Clear to end of line
        if sequence == '\x1b[K':
            self._clear_to_end_of_line()
            return

        # Save cursor position
        if sequence == '\x1b7':
            cursor = self.textCursor()
            self._saved_cursor_position = (cursor.blockNumber(), cursor.columnNumber())
            return

        # Restore cursor position
        if sequence == '\x1b8':
            if self._saved_cursor_position:
                line, column = self._saved_cursor_position
                cursor = self.textCursor()
                cursor.movePosition(QTextCursor.Start)
                for _ in range(line):
                    cursor.movePosition(QTextCursor.NextBlock)
                cursor.movePosition(QTextCursor.Right, QTextCursor.MoveAnchor, column)
                self.setTextCursor(cursor)
            return

        logger.warning(f"Unhandled escape sequence: {sequence}")

    def _clear_to_end_of_screen(self):
        """Clear from cursor position to the end of screen."""
        cursor = self.textCursor()

        # First clear to end of current line
        cursor.clearSelection()
        cursor.movePosition(QTextCursor.EndOfLine, QTextCursor.KeepAnchor)
        cursor.removeSelectedText()

        # Then clear all lines below
        current_pos = cursor.position()
        cursor.movePosition(QTextCursor.End, QTextCursor.KeepAnchor)
        cursor.removeSelectedText()

        # Restore cursor position
        cursor.setPosition(current_pos)
        self.setTextCursor(cursor)

    def _handle_sgr_sequence(self, params: str):
        """Handle Select Graphic Rendition (SGR) sequences."""
        if not params:
            params = '0'  # Reset to default

        # Start with a copy of the current format
        current_format = QTextCharFormat(self._current_text_format)

        for param in params.split(';'):
            code = int(param)

            if code == 0:  # Reset all attributes
                current_format = QTextCharFormat(self._default_text_format)
            elif code == 1:  # Bold
                current_format.setFontWeight(QFont.Bold)
            elif code == 2:  # Faint
                current_format.setFontWeight(QFont.Light)
            elif code == 3:  # Italic
                current_format.setFontItalic(True)
            elif code == 4:  # Underline
                current_format.setFontUnderline(True)
            elif code == 22:  # Normal intensity (reset bold/faint)
                current_format.setFontWeight(QFont.Normal)
            elif code == 23:  # Not italic
                current_format.setFontItalic(False)
            elif code == 24:  # Not underlined
                current_format.setFontUnderline(False)
            elif code == 39:  # Default foreground color
                current_format.setForeground(self._default_text_format.foreground().color())
            elif code == 49:  # Default background color
                current_format.setBackground(self._default_text_format.background().color())
            # Foreground colors
            elif 30 <= code <= 37:
                color = [
                    self._colors.BLACK,
                    self._colors.RED,
                    self._colors.GREEN,
                    self._colors.YELLOW,
                    self._colors.BLUE,
                    self._colors.MAGENTA,
                    self._colors.CYAN,
                    self._colors.WHITE
                ][code - 30]
                current_format.setForeground(color)
            # Bright foreground colors
            elif 90 <= code <= 97:
                color = [
                    self._colors.BRIGHT_BLACK,
                    self._colors.BRIGHT_RED,
                    self._colors.BRIGHT_GREEN,
                    self._colors.BRIGHT_YELLOW,
                    self._colors.BRIGHT_BLUE,
                    self._colors.BRIGHT_MAGENTA,
                    self._colors.BRIGHT_CYAN,
                    self._colors.BRIGHT_WHITE
                ][code - 90]
                current_format.setForeground(color)
            # Background colors
            elif 40 <= code <= 47:
                color = [
                    self._colors.BLACK,
                    self._colors.RED,
                    self._colors.GREEN,
                    self._colors.YELLOW,
                    self._colors.BLUE,
                    self._colors.MAGENTA,
                    self._colors.CYAN,
                    self._colors.WHITE
                ][code - 40]
                current_format.setBackground(color)
            # Bright background colors
            elif 100 <= code <= 107:
                color = [
                    self._colors.BRIGHT_BLACK,
                    self._colors.BRIGHT_RED,
                    self._colors.BRIGHT_GREEN,
                    self._colors.BRIGHT_YELLOW,
                    self._colors.BRIGHT_BLUE,
                    self._colors.BRIGHT_MAGENTA,
                    self._colors.BRIGHT_CYAN,
                    self._colors.BRIGHT_WHITE
                ][code - 100]
                current_format.setBackground(color)

        # Update the current text format
        self._current_text_format = current_format

    def _handle_cursor_sequence(self, sequence: str):
        """Handle cursor movement sequences."""
        match = re.match(r'\x1b\[(\d*)([ABCD])', sequence)
        if not match:
            return

        count = int(match.group(1)) if match.group(1) else 1
        direction = match.group(2)

        cursor = self.textCursor()

        if direction == 'A':  # Up
            cursor.movePosition(QTextCursor.Up, n=count)
        elif direction == 'B':  # Down
            cursor.movePosition(QTextCursor.Down, n=count)
        elif direction == 'C':  # Forward
            cursor.movePosition(QTextCursor.Right, n=count)
        elif direction == 'D':  # Back
            cursor.movePosition(QTextCursor.Left, n=count)

        self.setTextCursor(cursor)

    def _clear_to_end_of_line(self):
        """Clear from cursor to end of current line."""
        cursor = self.textCursor()
        cursor.clearSelection()
        cursor.movePosition(QTextCursor.EndOfLine, QTextCursor.KeepAnchor)
        cursor.removeSelectedText()
        self.setTextCursor(cursor)

    def clear(self):
        """Clear the terminal."""
        self.setPlainText("")
