"""Terminal widget implementation."""

from typing import Optional
from dataclasses import dataclass
import re

from PySide6.QtWidgets import QPlainTextEdit, QWidget
from PySide6.QtCore import Signal
from PySide6.QtGui import (
    QTextCharFormat, QTextCursor, QKeyEvent, QColor, QFont
)


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

    def keyPressEvent(self, event: QKeyEvent):
        """Send all keypresses to the process."""
        # Convert the key event into bytes to send
        text = event.text()
        if text:  # Only send if there's actual text (not just modifiers)
            self.data_ready.emit(text.encode())
        event.accept()

    def put_data(self, data: bytes):
        """Display received data with ANSI sequence handling."""
        print(f"put data {data}")
        text = data.decode(errors='replace')

        for char in text:
            print(f"process char: {ord(char)}")
            if self._in_escape_seq:
                print("esc seq")
                self._escape_seq_buffer += char
                if char.isalpha():
                    self._process_escape_sequence(self._escape_seq_buffer)
                    self._escape_seq_buffer = ""
                    self._in_escape_seq = False
            elif char == '\x1b':  # ESC
                self._in_escape_seq = True
                self._escape_seq_buffer = char
            else:
                print(f"plain: {char}")
                self._insert_plain_text(char)

        self.ensureCursorVisible()

    def _insert_plain_text(self, text: str):
        """Insert text at current cursor position."""
        cursor = self.textCursor()

        if text == '\r':
            # Move to start of line
            cursor.movePosition(QTextCursor.StartOfLine)
        elif text == '\n':
            cursor.insertText('\n')
        else:
            cursor.insertText(text)

        self.setTextCursor(cursor)

    def _process_escape_sequence(self, sequence: str):
        """Handle ANSI escape sequences."""
        # Basic color codes
        if sequence.startswith('\x1b[') and sequence.endswith('m'):
            self._handle_sgr_sequence(sequence[2:-1])
        # Cursor movement
        elif sequence.startswith('\x1b[') and sequence[-1] in 'ABCD':
            self._handle_cursor_sequence(sequence)
        # Clear screen
        elif sequence == '\x1b[2J':
            self.clear()
        # Clear to end of line
        elif sequence == '\x1b[K':
            self._clear_to_end_of_line()

    def _handle_sgr_sequence(self, params: str):
        """Handle Select Graphic Rendition (SGR) sequences."""
        if not params:
            params = '0'  # Reset to default

        char_format = QTextCharFormat()

        for param in params.split(';'):
            code = int(param)

            if code == 0:  # Reset
                char_format.setForeground(self._colors.WHITE)
                char_format.setBackground(self._colors.BLACK)
                char_format.setFontWeight(QFont.Normal)
            elif code == 1:  # Bold
                char_format.setFontWeight(QFont.Bold)
            elif code == 4:  # Underline
                char_format.setFontUnderline(True)
            elif code == 7:  # Inverse
                fg = char_format.foreground().color()
                bg = char_format.background().color()
                char_format.setForeground(bg)
                char_format.setBackground(fg)
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
                char_format.setForeground(color)
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
                char_format.setBackground(color)

        cursor = self.textCursor()
        cursor.mergeCharFormat(char_format)
        self.setTextCursor(cursor)

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
