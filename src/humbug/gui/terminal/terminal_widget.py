"""Terminal widget implementation."""

from typing import Optional
from dataclasses import dataclass
import re
import logging

from PySide6.QtWidgets import QPlainTextEdit, QWidget
from PySide6.QtCore import Signal, Qt
from PySide6.QtGui import (
    QTextCursor, QKeyEvent, QColor, QFont, QTextCharFormat
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

    def __init__(self, parent: Optional[QWidget] = None):
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

    def keyPressEvent(self, event: QKeyEvent):
        """Send all keypresses to the process."""
        # Convert the key event into bytes to send
        text = event.text()
        key = event.key()

        if key == Qt.Key_Backspace:
            self.data_ready.emit(b'\b')
        elif key == Qt.Key_Delete:
            # On most Unix-like systems, the Delete key sends \x7f (DEL character)
            self.data_ready.emit(b'\x7f')
        elif text:  # Only send if there's actual text (not just modifiers)
            self.data_ready.emit(text.encode())

        event.accept()

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
        """Insert text at current cursor position."""
        cursor = self.textCursor()

        # Apply the current text format
        cursor.mergeCharFormat(self._current_text_format)

        if text == '\r':
            # Move to start of line
            cursor.movePosition(QTextCursor.StartOfLine)
        elif text == '\n':
            # Move to start of next line
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
        """Handle ANSI escape sequences.

        Args:
            sequence: The escape sequence to process

        Note:
            Handles the following sequences:
            - SGR (Select Graphic Rendition) sequences ending in 'm'
            - Cursor movement sequences ending in ABCD
            - Screen clearing sequences: [2J, [J, [K
            - Cursor save/restore: ]7, ]8
            - Bracketed paste mode: [?2004h, [?2004l
            - Working directory: ]7;f
        """
        # Basic color codes
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
        if sequence == '\x1b]7':
            cursor = self.textCursor()
            self._saved_cursor_position = (cursor.blockNumber(), cursor.columnNumber())
            return

        # Set current working directory
        if sequence.startswith('\x1b]7;f'):
            # We can ignore this as it's just informational
            return

        # Restore cursor position
        if sequence == '\x1b]8':
            if self._saved_cursor_position:
                line, column = self._saved_cursor_position
                cursor = self.textCursor()
                cursor.movePosition(QTextCursor.Start)
                for _ in range(line):
                    cursor.movePosition(QTextCursor.NextBlock)
                cursor.movePosition(QTextCursor.Right, QTextCursor.MoveAnchor, column)
                self.setTextCursor(cursor)
            return

        # Bracketed paste mode enable
        if sequence == '\x1b[?2004h':
            # Could implement bracketed paste mode if needed
            return

        # Bracketed paste mode disable
        if sequence == '\x1b[?2004l':
            # Could implement bracketed paste mode if needed
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
