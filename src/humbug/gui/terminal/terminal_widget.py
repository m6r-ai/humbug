"""Terminal widget implementation."""

from typing import Optional, Tuple
import re
import logging
from enum import IntEnum

from PySide6.QtWidgets import QPlainTextEdit, QWidget
from PySide6.QtCore import Signal, Qt
from PySide6.QtGui import (
    QTextCursor, QKeyEvent, QFont, QTextCharFormat, QMouseEvent,
    QTextFormat
)

from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager


class FormatProperty(IntEnum):
    """Properties used to track which format attributes are explicit vs default."""
    CUSTOM_FOREGROUND = QTextFormat.UserProperty
    CUSTOM_BACKGROUND = QTextFormat.UserProperty + 1
    CUSTOM_WEIGHT = QTextFormat.UserProperty + 2
    CUSTOM_ITALIC = QTextFormat.UserProperty + 3
    CUSTOM_UNDERLINE = QTextFormat.UserProperty + 4


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

        self._logger = logging.getLogger("TerminalTab")

        # Get style manager
        self._style_manager = StyleManager()

        # Set up default text format
        self._default_text_format = QTextCharFormat()
        self._update_default_format()

        # Current text format (initialized to default)
        self._current_text_format = self._default_text_format

        # Set up default appearance
        self.setStyleSheet(f"""
            QPlainTextEdit {{
                background-color: {self._style_manager.get_color_str(ColorRole.TERMINAL_BACKGROUND)};
                color: {self._style_manager.get_color_str(ColorRole.TERMINAL_TEXT)};
                border: none;
            }}
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
        self._application_keypad_mode = False
        self._mouse_tracking = False
        self._mouse_tracking_sgr = False
        self._saved_mouse_tracking = False
        self._saved_mouse_tracking_sgr = False
        self._bracketed_paste_mode = False
        self._current_directory = None

        # Connect style changed signal
        self._style_manager.style_changed.connect(self._handle_style_changed)

    def _update_default_format(self):
        """Update the default text format based on current style."""
        self._default_text_format = QTextCharFormat()
        self._default_text_format.setForeground(self._style_manager.get_color(ColorRole.TERMINAL_TEXT))
        self._default_text_format.setBackground(self._style_manager.get_color(ColorRole.TERMINAL_BACKGROUND))
        self._default_text_format.setFontWeight(QFont.Normal)
        self._default_text_format.setFontUnderline(False)
        self._default_text_format.setFontItalic(False)
        # Clear any custom property markers
        for prop in FormatProperty:
            self._default_text_format.setProperty(prop, False)

    def _handle_style_changed(self):
        """Handle style changes."""
        # Update default format
        self._update_default_format()

        # Update current format while preserving custom properties
        new_format = QTextCharFormat(self._default_text_format)

        # Check each custom property and preserve if set
        if self._current_text_format.property(FormatProperty.CUSTOM_FOREGROUND):
            new_format.setForeground(self._current_text_format.foreground())
            new_format.setProperty(FormatProperty.CUSTOM_FOREGROUND, True)

        if self._current_text_format.property(FormatProperty.CUSTOM_BACKGROUND):
            new_format.setBackground(self._current_text_format.background())
            new_format.setProperty(FormatProperty.CUSTOM_BACKGROUND, True)

        if self._current_text_format.property(FormatProperty.CUSTOM_WEIGHT):
            new_format.setFontWeight(self._current_text_format.fontWeight())
            new_format.setProperty(FormatProperty.CUSTOM_WEIGHT, True)

        if self._current_text_format.property(FormatProperty.CUSTOM_ITALIC):
            new_format.setFontItalic(self._current_text_format.fontItalic())
            new_format.setProperty(FormatProperty.CUSTOM_ITALIC, True)

        if self._current_text_format.property(FormatProperty.CUSTOM_UNDERLINE):
            new_format.setFontUnderline(self._current_text_format.fontUnderline())
            new_format.setProperty(FormatProperty.CUSTOM_UNDERLINE, True)

        self._current_text_format = new_format

        # Update appearance
        self.setStyleSheet(f"""
            QPlainTextEdit {{
                background-color: {self._style_manager.get_color_str(ColorRole.TERMINAL_BACKGROUND)};
                color: {self._style_manager.get_color_str(ColorRole.TERMINAL_TEXT)};
                border: none;
            }}
        """)

        # Update colors for all text blocks
        cursor = self.textCursor()
        saved_position = cursor.position()
        cursor.movePosition(QTextCursor.Start)

        while not cursor.atEnd():
            cursor.movePosition(QTextCursor.Right, QTextCursor.KeepAnchor)
            text_format = cursor.charFormat()
            new_format = QTextCharFormat(text_format)

            # Log format properties
            has_custom_fg = text_format.property(FormatProperty.CUSTOM_FOREGROUND)
            has_custom_bg = text_format.property(FormatProperty.CUSTOM_BACKGROUND)

            # Only update colors that aren't custom (i.e., are using defaults)
            if not has_custom_fg:
                new_format.setForeground(self._style_manager.get_color(ColorRole.TERMINAL_TEXT))

            if not has_custom_bg:
                new_format.setBackground(self._style_manager.get_color(ColorRole.TERMINAL_BACKGROUND))

            cursor.mergeCharFormat(new_format)
            cursor.clearSelection()

        # Restore cursor position
        cursor.setPosition(saved_position)
        self.setTextCursor(cursor)

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
            elif key == Qt.Key_Return or key == Qt.Key_Enter:
                self.data_ready.emit(b'\r')
            elif key == Qt.Key_Backspace:
                self.data_ready.emit(b'\x7f' if modifiers & Qt.ControlModifier else b'\b')
            elif key == Qt.Key_Delete:
                self.data_ready.emit(b'\x1b[3~')
            elif text:
                self.data_ready.emit(text.encode())
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
            elif key == Qt.Key_Return or key == Qt.Key_Enter:
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

        # Accept the event but don't call super() to prevent cursor movement
        event.accept()

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

    def insertFromMimeData(self, source):
        """Handle paste events with support for bracketed paste mode."""
        if source.hasText():
            text = source.text()
            if self._bracketed_paste_mode:
                # Wrap the pasted text in bracketed paste sequences
                self.data_ready.emit(b'\x1b[200~')
                self.data_ready.emit(text.encode())
                self.data_ready.emit(b'\x1b[201~')
            else:
                self.data_ready.emit(text.encode())

    def put_data(self, data: bytes):
        """Display received data with ANSI sequence handling.

        Args:
            data: Raw bytes from terminal

        Raises:
            UnicodeDecodeError: If data cannot be decoded
        """
        text = data.decode(errors='replace')

        for char in text:
            if self._in_escape_seq:
                self._escape_seq_buffer += char

                # Handle single-character sequences
                if len(self._escape_seq_buffer) == 2:  # ESC + one character
                    if char in {'=', '>', '\\', '7', '8', 'c', 'D', 'E', 'H', 'M'}:
                        self._process_escape_sequence(self._escape_seq_buffer)
                        self._escape_seq_buffer = ""
                        self._in_escape_seq = False
                        continue

                # Handle CSI sequences
                if len(self._escape_seq_buffer) >= 2 and self._escape_seq_buffer[1] == '[':
                    if char.isalpha() or char in {'@', '`', '~'}:
                        self._process_escape_sequence(self._escape_seq_buffer)
                        self._escape_seq_buffer = ""
                        self._in_escape_seq = False
                        continue

                # Handle OSC sequences
                if len(self._escape_seq_buffer) >= 2 and self._escape_seq_buffer[1] == ']':
                    if char in {'\\', '\x07'}:  # BEL character can also terminate OSC
                        self._process_escape_sequence(self._escape_seq_buffer)
                        self._escape_seq_buffer = ""
                        self._in_escape_seq = False
                        continue

                # Safety check for buffer length
                if len(self._escape_seq_buffer) > 32:  # Arbitrary reasonable limit
                    self._logger.warning(f"Escape sequence too long, discarding: {self._escape_seq_buffer}")
                    self._escape_seq_buffer = ""
                    self._in_escape_seq = False

            elif char == '\x1b':  # ESC
                self._in_escape_seq = True
                self._escape_seq_buffer = char
            else:
                self._insert_plain_text(char)

        self.ensureCursorVisible()

    def _handle_osc_sequence(self, sequence: str) -> bool:
        """Handle Operating System Command (OSC) sequences."""
        # Window title (ESC]0;)
        if sequence.startswith('\x1b]0;'):
            title = sequence[4:-1]  # Remove ESC]0; prefix and terminator
            # Emit signal to update window title
            self._logger.debug(f"Window title set to: {title}")
            return True

        # Handle current working directory notification (ESC]7;)
        if sequence.startswith('\x1b]7;'):
            try:
                if sequence.endswith('f'):  # Query current directory
                    if self._current_directory:
                        response = f"\x1b]7;{self._current_directory}\x1b\\"
                        self.data_ready.emit(response.encode())
                else:
                    self._current_directory = sequence[4:-1]  # Remove ESC]7; prefix and terminator
                    self._logger.debug(f"Current directory set to: {self._current_directory}")
                return True
            except Exception as e:
                self._logger.warning(f"Failed to process directory update: {e}")
                return True

        return False

    def _process_escape_sequence(self, sequence: str):
        """Handle ANSI escape sequences."""
        # Handle OSC sequences first
        if sequence.startswith('\x1b]'):
            if self._handle_osc_sequence(sequence):
                return

        # Handle keypad mode sequences
        if sequence == '\x1b=':  # ESC=
            self._application_keypad_mode = True
            return

        if sequence == '\x1b>':  # ESC>
            self._application_keypad_mode = False
            return

        # Handle bracketed paste mode
        if sequence == '\x1b[?2004h':
            self._bracketed_paste_mode = True
            return

        if sequence == '\x1b[?2004l':
            self._bracketed_paste_mode = False
            return

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
        if sequence == '\x1b[?1h':
            self._application_cursor_keys = True
            return

        # Enable application keypad mode
        if sequence == '\x1b[?1l':
            self._application_cursor_keys = False
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

        # Clear from start of line to cursor (ESC[1K)
        if sequence == '\x1b[1K':
            cursor = self.textCursor()
            start_pos = cursor.block().position()
            cursor.setPosition(start_pos)
            end_pos = self.textCursor().position()
            cursor.setPosition(end_pos, QTextCursor.KeepAnchor)
            cursor.removeSelectedText()
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

        self._logger.warning(f"Unhandled escape sequence: {sequence}")

    def _insert_plain_text(self, text: str):
        """Insert text at current cursor position with scroll region support."""
        cursor = self.textCursor()

        # Always apply the current format
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
        elif text == '\b':  # Backspace/Cursor Left
            cursor.movePosition(QTextCursor.Left)
        elif text == '\t':  # Tab
            # Handle each space in overwrite mode
            for _ in range(8):
                if not cursor.atEnd():
                    cursor.deleteChar()
                cursor.insertText(' ')
        elif text == '\x0b':  # Vertical tab
            cursor.movePosition(QTextCursor.Down)
        elif text == '\x0c':  # Form feed
            self.clear()
        else:
            # In a terminal, we always overwrite the character at cursor position
            if not cursor.atEnd():
                cursor.deleteChar()
            cursor.insertText(text)

        self.setTextCursor(cursor)

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
            try:
                code = int(param)
            except ValueError:
                continue

            if code == 0:  # Reset all attributes
                current_format = QTextCharFormat()
                current_format.setForeground(self._style_manager.get_color(ColorRole.TERMINAL_TEXT))
                current_format.setBackground(self._style_manager.get_color(ColorRole.TERMINAL_BACKGROUND))
                current_format.setFontWeight(QFont.Normal)
                current_format.setFontUnderline(False)
                current_format.setFontItalic(False)
                # Clear all custom property markers
                for prop in FormatProperty:
                    current_format.setProperty(prop, False)
            elif code == 1:  # Bold
                current_format.setFontWeight(QFont.Bold)
                current_format.setProperty(FormatProperty.CUSTOM_WEIGHT, True)
            elif code == 2:  # Faint
                current_format.setFontWeight(QFont.Light)
                current_format.setProperty(FormatProperty.CUSTOM_WEIGHT, True)
            elif code == 3:  # Italic
                current_format.setFontItalic(True)
                current_format.setProperty(FormatProperty.CUSTOM_ITALIC, True)
            elif code == 4:  # Underline
                current_format.setFontUnderline(True)
                current_format.setProperty(FormatProperty.CUSTOM_UNDERLINE, True)
            elif code == 22:  # Normal intensity (reset bold/faint)
                current_format.setFontWeight(QFont.Normal)
                current_format.setProperty(FormatProperty.CUSTOM_WEIGHT, False)
            elif code == 23:  # Not italic
                current_format.setFontItalic(False)
                current_format.setProperty(FormatProperty.CUSTOM_ITALIC, False)
            elif code == 24:  # Not underlined
                current_format.setFontUnderline(False)
                current_format.setProperty(FormatProperty.CUSTOM_UNDERLINE, False)
            elif code == 39:  # Default foreground color
                current_format.setForeground(self._style_manager.get_color(ColorRole.TERMINAL_TEXT))
                current_format.setProperty(FormatProperty.CUSTOM_FOREGROUND, False)
            elif code == 49:  # Default background color
                current_format.setBackground(self._style_manager.get_color(ColorRole.TERMINAL_BACKGROUND))
                current_format.setProperty(FormatProperty.CUSTOM_BACKGROUND, False)
            # Foreground colors
            elif 30 <= code <= 37:
                color_roles = [
                    ColorRole.TERM_BLACK,
                    ColorRole.TERM_RED,
                    ColorRole.TERM_GREEN,
                    ColorRole.TERM_YELLOW,
                    ColorRole.TERM_BLUE,
                    ColorRole.TERM_MAGENTA,
                    ColorRole.TERM_CYAN,
                    ColorRole.TERM_WHITE
                ]
                current_format.setForeground(self._style_manager.get_color(color_roles[code - 30]))
                current_format.setProperty(FormatProperty.CUSTOM_FOREGROUND, True)
            # Bright foreground colors
            elif 90 <= code <= 97:
                color_roles = [
                    ColorRole.TERM_BRIGHT_BLACK,
                    ColorRole.TERM_BRIGHT_RED,
                    ColorRole.TERM_BRIGHT_GREEN,
                    ColorRole.TERM_BRIGHT_YELLOW,
                    ColorRole.TERM_BRIGHT_BLUE,
                    ColorRole.TERM_BRIGHT_MAGENTA,
                    ColorRole.TERM_BRIGHT_CYAN,
                    ColorRole.TERM_BRIGHT_WHITE
                ]
                current_format.setForeground(self._style_manager.get_color(color_roles[code - 90]))
                current_format.setProperty(FormatProperty.CUSTOM_FOREGROUND, True)
            # Background colors
            elif 40 <= code <= 47:
                color_roles = [
                    ColorRole.TERM_BLACK,
                    ColorRole.TERM_RED,
                    ColorRole.TERM_GREEN,
                    ColorRole.TERM_YELLOW,
                    ColorRole.TERM_BLUE,
                    ColorRole.TERM_MAGENTA,
                    ColorRole.TERM_CYAN,
                    ColorRole.TERM_WHITE
                ]
                current_format.setBackground(self._style_manager.get_color(color_roles[code - 40]))
                current_format.setProperty(FormatProperty.CUSTOM_BACKGROUND, True)
            # Bright background colors
            elif 100 <= code <= 107:
                color_roles = [
                    ColorRole.TERM_BRIGHT_BLACK,
                    ColorRole.TERM_BRIGHT_RED,
                    ColorRole.TERM_BRIGHT_GREEN,
                    ColorRole.TERM_BRIGHT_YELLOW,
                    ColorRole.TERM_BRIGHT_BLUE,
                    ColorRole.TERM_BRIGHT_MAGENTA,
                    ColorRole.TERM_BRIGHT_CYAN,
                    ColorRole.TERM_BRIGHT_WHITE
                ]
                current_format.setBackground(self._style_manager.get_color(color_roles[code - 100]))
                current_format.setProperty(FormatProperty.CUSTOM_BACKGROUND, True)

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
