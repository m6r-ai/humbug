# File: humbug/gui/terminal/terminal_widget.py

from typing import List, Optional
from dataclasses import dataclass
import re

from PySide6.QtWidgets import QPlainTextEdit, QWidget
from PySide6.QtCore import Qt, Signal
from PySide6.QtGui import (
    QTextCharFormat, QTextCursor, QKeyEvent, QColor
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
    """Custom terminal emulator widget."""

    # Signal emitted when user input is ready to be sent
    data_ready = Signal(bytes)

    def __init__(self, parent: Optional[QWidget] = None):
        """Initialize terminal widget."""
        super().__init__(parent)
        self.setLineWrapMode(QPlainTextEdit.NoWrap)

        # Terminal state
        self._input_start_pos = 0
        self._command_history: List[str] = []
        self._history_index = 0
        self._current_line = ""

        # Terminal settings
        self._cursor_blink = True
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

        # Handle input
        self.cursorPositionChanged.connect(self._handle_cursor_position_changed)

        # Setup ANSI escape sequence processing
        self._escape_seq_buffer = ""
        self._in_escape_seq = False

        # Initialize with prompt
        self._show_prompt()

    def _show_prompt(self):
        """Display command prompt."""
        self.appendPlainText("$ ")
        self._input_start_pos = self.textCursor().position()

    def _handle_cursor_position_changed(self):
        """Ensure cursor doesn't move before input area."""
        cursor = self.textCursor()
        if cursor.position() < self._input_start_pos:
            cursor.setPosition(self._input_start_pos)
            self.setTextCursor(cursor)

    def keyPressEvent(self, event: QKeyEvent):
        """Handle key press events."""
        cursor = self.textCursor()

        # Only allow input after prompt
        if cursor.position() < self._input_start_pos:
            cursor.setPosition(self._input_start_pos)
            self.setTextCursor(cursor)

        if event.key() == Qt.Key_Return or event.key() == Qt.Key_Enter:
            self._handle_return()
            return

        if event.key() == Qt.Key_Backspace:
            if cursor.position() <= self._input_start_pos:
                return
            super().keyPressEvent(event)
            return

        if event.key() == Qt.Key_Up:
            self._handle_history_up()
            return

        if event.key() == Qt.Key_Down:
            self._handle_history_down()
            return

        if event.key() == Qt.Key_Left:
            if cursor.position() <= self._input_start_pos:
                return

            super().keyPressEvent(event)
            return

        if event.key() == Qt.Key_Home:
            cursor.setPosition(self._input_start_pos)
            self.setTextCursor(cursor)
            return

        # Handle normal input
        if not event.text():
            return

        super().keyPressEvent(event)

    def _handle_return(self):
        """Process return key press."""
        cursor = self.textCursor()
        cursor.movePosition(QTextCursor.End)
        self.setTextCursor(cursor)

        # Get current command
        command = self.toPlainText()[self._input_start_pos:]

        # Add to history
        if command.strip():
            self._command_history.append(command)
            self._history_index = len(self._command_history)

        # Emit command
        self.data_ready.emit(command.encode() + b'\r\n')

        # Add newline
        self.appendPlainText("")

    def _handle_history_up(self):
        """Handle up arrow key for history."""
        if not self._command_history:
            return

        if self._history_index > 0:
            self._history_index -= 1
            self._set_command(self._command_history[self._history_index])

    def _handle_history_down(self):
        """Handle down arrow key for history."""
        if self._history_index < len(self._command_history) - 1:
            self._history_index += 1
            self._set_command(self._command_history[self._history_index])
        else:
            self._history_index = len(self._command_history)
            self._set_command("")

    def _set_command(self, command: str):
        """Set the current command text."""
        cursor = self.textCursor()
        cursor.setPosition(self._input_start_pos)
        cursor.movePosition(QTextCursor.End, QTextCursor.KeepAnchor)
        cursor.insertText(command)

    def put_data(self, data: bytes):
        """Process received data."""
        text = data.decode(errors='replace')

        for char in text:
            if self._in_escape_seq:
                self._escape_seq_buffer += char
                if char.isalpha():
                    self._process_escape_sequence(self._escape_seq_buffer)
                    self._escape_seq_buffer = ""
                    self._in_escape_seq = False
            elif char == '\x1b':  # ESC
                self._in_escape_seq = True
                self._escape_seq_buffer = char
            else:
                self._insert_plain_text(char)

    def _insert_plain_text(self, text: str):
        """Insert text at current cursor position."""
        cursor = self.textCursor()
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
                char_format.setFontWeight(QTextCharFormat.Normal)
            elif code == 1:  # Bold
                char_format.setFontWeight(QTextCharFormat.Bold)
            elif code == 4:  # Underline
                char_format.setFontUnderline(True)
            elif code == 7:  # Inverse
                fg = format.foreground().color()
                bg = format.background().color()
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
        cursor.mergeCharFormat(format)
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
        self._input_start_pos = 0
