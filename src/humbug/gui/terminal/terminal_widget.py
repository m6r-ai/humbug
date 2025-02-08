"""Terminal widget implementation."""

from dataclasses import dataclass
import fcntl
from typing import Optional, Tuple
import re
import logging
from enum import IntEnum
import struct
import termios

from PySide6.QtWidgets import QPlainTextEdit, QWidget
from PySide6.QtCore import Signal, Qt
from PySide6.QtGui import (
    QTextCursor, QKeyEvent, QFont, QTextCharFormat, QMouseEvent,
    QTextFormat, QResizeEvent, QFontMetricsF
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
        """Convert terminal size to struct format for TIOCSWINSZ.

        Returns:
            bytes: Packed struct in format suitable for TIOCSWINSZ ioctl
        """
        return struct.pack('HHHH', self.rows, self.cols, 0, 0)


class TerminalWidget(QPlainTextEdit):
    """Terminal display widget."""

    # Signal emitted when user input is ready
    data_ready = Signal(bytes)
    # Signal emitted for mouse events when tracking is enabled
    mouse_event = Signal(str)
    size_changed = Signal()

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
                background-color: {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
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
        self._main_screen_formats = []
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

        self._current_size: Optional[TerminalSize] = None

        # Connect style changed signal
        self._style_manager.style_changed.connect(self._handle_style_changed)

    def _update_default_format(self):
        """Update the default text format based on current style."""
        self._default_text_format = QTextCharFormat()
        self._default_text_format.setForeground(self._style_manager.get_color(ColorRole.TEXT_PRIMARY))
        self._default_text_format.setBackground(self._style_manager.get_color(ColorRole.TAB_BACKGROUND_ACTIVE))
        self._default_text_format.setFontWeight(QFont.Normal)
        self._default_text_format.setFontUnderline(False)
        self._default_text_format.setFontItalic(False)
        # Clear any custom property markers
        for prop in FormatProperty:
            self._default_text_format.setProperty(prop, False)

    def _handle_style_changed(self):
        """Handle style changes."""
        # Update default format
        print("handle style changed")
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
                background-color: {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
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
                new_format.setForeground(self._style_manager.get_color(ColorRole.TEXT_PRIMARY))

            if not has_custom_bg:
                new_format.setBackground(self._style_manager.get_color(ColorRole.TAB_BACKGROUND_ACTIVE))

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
        if not self._mouse_tracking or (event.modifiers() & Qt.ShiftModifier):
            # Allow normal text selection when mouse tracking is disabled
            super().mousePressEvent(event)
            return

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

        print(f"Processing data: {repr(text)}")

        i = 0
        while i < len(text):
            char = text[i]

            if self._in_escape_seq:
                self._escape_seq_buffer += char

                # Check for sequence endings based on type
                if self._escape_seq_buffer.startswith('\x1b]'):  # OSC sequence
                    if char == '\x07':  # BEL terminator
                        self._process_escape_sequence(self._escape_seq_buffer)
                        self._escape_seq_buffer = ""
                        self._in_escape_seq = False
                    elif char == '\\' and self._escape_seq_buffer[-2:] == '\x1b\\':  # ST terminator
                        self._process_escape_sequence(self._escape_seq_buffer)
                        self._escape_seq_buffer = ""
                        self._in_escape_seq = False

                elif self._escape_seq_buffer.startswith('\x1b['):  # CSI sequence
                    if char.isalpha() or char in {'@', '`', '~'}:
                        self._process_escape_sequence(self._escape_seq_buffer)
                        self._escape_seq_buffer = ""
                        self._in_escape_seq = False

                elif self._escape_seq_buffer.startswith('\x1bP'):  # DCS sequence
                    if char == '\\' and self._escape_seq_buffer[-2:] == '\x1b\\':  # ST terminator
                        self._process_escape_sequence(self._escape_seq_buffer)
                        self._escape_seq_buffer = ""
                        self._in_escape_seq = False

                elif len(self._escape_seq_buffer) == 2:  # Simple ESC sequences
                    if char in {'=', '>', '\\', '7', '8', 'c', 'D', 'E', 'H', 'M'}:
                        self._process_escape_sequence(self._escape_seq_buffer)
                        self._escape_seq_buffer = ""
                        self._in_escape_seq = False

                # Reset if we find a new escape sequence while processing one
                elif char == '\x1b' and len(self._escape_seq_buffer) > 1:
                    self._logger.debug(f"Found new escape sequence while processing: {repr(self._escape_seq_buffer)}")
                    # Process what we have so far as raw text
                    for c in self._escape_seq_buffer:
                        self._insert_plain_text(c)
                    self._escape_seq_buffer = char

                # Safety check for buffer length - separate limits for different sequence types
                elif (
                    (self._escape_seq_buffer.startswith('\x1b]') and len(self._escape_seq_buffer) > 256) or  # OSC
                    (self._escape_seq_buffer.startswith('\x1b[') and len(self._escape_seq_buffer) > 32) or   # CSI
                    (self._escape_seq_buffer.startswith('\x1bP') and len(self._escape_seq_buffer) > 32) or   # DCS
                    (len(self._escape_seq_buffer) > 16)  # Other
                ):
                    self._logger.warning(
                        f"Escape sequence exceeded maximum length, discarding: {repr(self._escape_seq_buffer)}"
                    )
                    self._escape_seq_buffer = ""
                    self._in_escape_seq = False

            elif char == '\x1b':  # Start of new escape sequence
                self._in_escape_seq = True
                self._escape_seq_buffer = char

            else:
                self._insert_plain_text(char)

            i += 1

        self.ensureCursorVisible()

    def _handle_osc_sequence(self, sequence: str) -> bool:
        """Handle Operating System Command (OSC) sequences.

        Args:
            sequence: The complete OSC sequence (starting with ESC])

        Returns:
            bool: True if sequence was handled, False otherwise
        """
        # Extract the OSC command number and parameter
        parts = sequence[2:].split(';', 1)
        if not parts:
            return False

        try:
            command = int(parts[0])
            param = parts[1][:-1] if len(parts) > 1 else ''  # Remove terminator

            if command == 0:  # Window title
                self._logger.debug(f"Window title set to: {param}")
                return True

            if command == 7:  # Current directory notification
                if param == 'f':  # Query current directory
                    if self._current_directory:
                        response = f"\x1b]7;{self._current_directory}\x1b\\"
                        self.data_ready.emit(response.encode())
                else:
                    self._current_directory = param
                    self._logger.debug(f"Current directory set to: {param}")
                return True

            if command == 10:  # Set foreground color
                self._logger.debug(f"Set text foreground color: {param}")
                # TODO: Implement color parsing and setting
                return True

            if command == 11:  # Set background color
                self._logger.debug(f"Set text background color: {param}")
                # TODO: Implement color parsing and setting
                return True

        except (ValueError, IndexError) as e:
            self._logger.warning(f"Failed to parse OSC sequence: {sequence}, error: {e}")

        return False

    def _process_escape_sequence(self, sequence: str):
        """Handle ANSI escape sequences.

        Args:
            sequence: The complete escape sequence starting with ESC
        """
        print(f"Processing escape sequence: {repr(sequence)}")
        # Handle OSC sequences first
        if sequence.startswith('\x1b]'):
            if self._handle_osc_sequence(sequence):
                return

        # Handle Control Sequence Introducer (CSI) sequences
        if sequence.startswith('\x1b['):
            # Extract the command and parameters
            command = sequence[-1]
            params = sequence[2:-1]  # Remove ESC[ and command char

            # Handle based on command type
            if command in 'ABCD':  # Cursor movement
                self._handle_cursor_sequence(sequence)
                return

            if command in 'Hf':  # Cursor position
                self._handle_cursor_position(params)
                return

            if command == 'J':  # Clear screen
                self._handle_clear_screen(params)
                return

            if command == 'K':  # Erase in line
                self._handle_erase_in_line(params)
                return

            if command in '@PML':  # Insert/Delete operations
                self._handle_insert_delete(command, params)
                return

            if command == 'm':  # SGR - Select Graphic Rendition
                self._handle_sgr_sequence(params)
                return

            if command == 'n':  # Device Status Reports
                self._handle_device_status(params)
                return

            if command == 'c':  # Device Attributes
                self._handle_device_attributes(params)
                return

            if command == 'g':  # Tab Controls
                self._handle_tab_control(params)
                return

            if command == 'r':  # Scrolling Region
                self._handle_scroll_region(params)
                return

            if command in 'hl':  # Mode Settings
                self._handle_mode_setting(command, params)
                return

            # Handle window operations
            if command == 't':
                self._handle_window_operation(params)
                return

        # Handle keypad mode sequences
        if sequence == '\x1b=':  # Enable application keypad mode
            self._application_keypad_mode = True
            return

        if sequence == '\x1b>':  # Disable application keypad mode
            self._application_keypad_mode = False
            return

        # Handle single-character sequences
        if len(sequence) == 2:  # ESC + one character
            if self._handle_simple_sequence(sequence[1]):
                return

        self._logger.warning(f"Unhandled escape sequence: {sequence}")

    def _handle_window_operation(self, params: str):
        """Handle window operation sequences.

        Args:
            params: The parameters for the window operation
        """
        try:
            parts = params.split(';')
            op = int(parts[0])

            if op == 18:  # Report terminal size
                # Get size in characters
                size = self._calculate_size()
                self.data_ready.emit(f'\x1b[8;{size.rows};{size.cols}t'.encode())
                return

            if op == 22:  # Push/pop window title
                if len(parts) > 1:
                    sub_op = int(parts[1])
                    if sub_op == 1:  # Push window title
                        self._logger.debug("Push window title")
                    elif sub_op == 2:  # Pop window title
                        self._logger.debug("Pop window title")

        except (ValueError, IndexError) as e:
            self._logger.warning(f"Invalid window operation parameters: {params}, error: {e}")

    def _handle_cursor_position(self, params: str):
        """Handle cursor position (CUP) sequences."""
        try:
            parts = params.split(';')
            row = int(parts[0]) if parts[0] else 1
            col = int(parts[1]) if len(parts) > 1 and parts[1] else 1
            self._move_cursor_to(row - 1, col - 1)  # Convert to 0-based indices
        except (ValueError, IndexError):
            self._logger.warning(f"Invalid cursor position params: {params}")

    def _handle_clear_screen(self, params: str):
        """Handle clear screen (ED) sequences."""
        param = params if params else '0'
        if param == '0':  # Clear from cursor to end of screen
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
        elif param == '1':  # Clear from cursor to beginning of screen
            cursor = self.textCursor()
            end_pos = cursor.position()
            cursor.movePosition(QTextCursor.Start, QTextCursor.KeepAnchor)
            cursor.removeSelectedText()
            cursor.setPosition(end_pos)
        elif param == '2':  # Clear entire screen
            self.clear()
        elif param == '3':  # Clear scrollback buffer
            cursor = self.textCursor()
            pos = cursor.position()
            cursor.movePosition(QTextCursor.Start)
            cursor.movePosition(QTextCursor.End, QTextCursor.KeepAnchor)
            visible_content = cursor.selectedText()
            self.clear()
            self.insertPlainText(visible_content)
            cursor.setPosition(pos)
            self.setTextCursor(cursor)
        else:
            print(f"handle_clear_screen: unknown param {param}")
            self._logger.warning(f"handle_clear_screen: unknown param {param}")

    def _handle_erase_in_line(self, params: str):
        """Handle erase in line (EL) sequences."""
        param = params if params else '0'
        cursor = self.textCursor()
        if param == '0':  # Clear from cursor to end of line
            cursor.clearSelection()
            cursor.movePosition(QTextCursor.EndOfLine, QTextCursor.KeepAnchor)
            cursor.removeSelectedText()
        elif param == '1':  # Clear from cursor to start of line
            start_pos = cursor.block().position()
            end_pos = cursor.position()
            cursor.setPosition(start_pos)
            cursor.setPosition(end_pos, QTextCursor.KeepAnchor)
            cursor.removeSelectedText()
        elif param == '2':  # Clear entire line
            cursor.movePosition(QTextCursor.StartOfLine)
            cursor.movePosition(QTextCursor.EndOfLine, QTextCursor.KeepAnchor)
            cursor.removeSelectedText()
        else:
            print(f"handle_clear_line: unknown param {param}")
            self._logger.warning(f"handle_clear_line: unknown param {param}")

        self.setTextCursor(cursor)

    def _handle_insert_delete(self, command: str, params: str):
        """Handle insert and delete operations."""
        count = int(params) if params else 1
        cursor = self.textCursor()

        if command == '@':  # Insert blank characters
            cursor.insertText(' ' * count)
        elif command == 'P':  # Delete characters
            end_pos = cursor.position() + count
            cursor.setPosition(end_pos, QTextCursor.KeepAnchor)
            cursor.removeSelectedText()
        elif command == 'L':  # Insert lines
            pos = cursor.position()
            cursor.movePosition(QTextCursor.StartOfLine)
            for _ in range(count):
                cursor.insertText('\n')
            cursor.setPosition(pos)
        elif command == 'M':  # Delete lines
            cursor.movePosition(QTextCursor.StartOfLine)
            for _ in range(count):
                cursor.movePosition(QTextCursor.EndOfLine, QTextCursor.KeepAnchor)
                cursor.movePosition(QTextCursor.Right, QTextCursor.KeepAnchor)
                cursor.removeSelectedText()
        else:
            print(f"handle_insert_delete: unknown command {command}")
            self._logger.warning(f"handle_insert_delete: unknown command {command}")

        self.setTextCursor(cursor)

    def _handle_device_status(self, params: str):
        """Handle Device Status Report (DSR) sequences."""
        if params == '5':  # Device status report
            self.data_ready.emit(b'\x1b[0n')  # Device OK
        elif params == '6':  # Cursor position report
            cursor = self.textCursor()
            row = cursor.blockNumber() + 1
            col = cursor.columnNumber() + 1
            self.data_ready.emit(f'\x1b[{row};{col}R'.encode())
        else:
            print(f"handle_device_status: unknown params {params}")
            self._logger.warning(f"handle_insert_delete: unknown params {params}")

    def _handle_device_attributes(self, params: str):
        """Handle Device Attributes (DA) sequences."""
        if not params or params == '0':
            # Report as VT100 with Advanced Video Option
            self.data_ready.emit(b'\x1b[?1;2c')
        elif params == '>':  # Secondary Device Attributes
            # Report as VT220
            self.data_ready.emit(b'\x1b[>1;10;0c')
        else:
            print(f"handle_device_attributes: unknown params {params}")
            self._logger.warning(f"handle_insert_attributes: unknown params {params}")

    def _handle_tab_control(self, params: str):
        """Handle tab control sequences."""
        if not params or params == '0':  # Clear tab at cursor
            pass  # Implement tab clear
        elif params == '3':  # Clear all tabs
            pass  # Implement clear all tabs

        print(f"handle_device_attributes: unknown params {params}")
        self._logger.warning(f"handle_insert_attributes: unknown params {params}")

    def _handle_scroll_region(self, params: str):
        """Handle scrolling region (DECSTBM) sequences."""
        try:
            if params:
                top, bottom = map(lambda x: int(x) - 1, params.split(';'))
                self._scroll_region = (top, bottom)
            else:
                self._scroll_region = None
        except (ValueError, IndexError):
            self._scroll_region = None

    def _handle_mode_setting(self, command: str, params: str):
        """Handle mode setting sequences."""
        set_mode = (command == 'h')
        if params.startswith('?'):  # Private modes
            self._handle_private_mode(params[1:], set_mode)
        else:  # ANSI modes
            self._handle_ansi_mode(params, set_mode)

    def _handle_private_mode(self, mode: str, set_mode: bool):
        """
        Handle private mode settings.

        Args:
            mode: The mode number as a string
            set_mode: True to set mode, False to reset it
        """
        if mode == '1':  # Application Cursor Keys
            self._application_cursor_keys = set_mode
        elif mode == '7':  # Auto-wrap Mode
            self.setLineWrapMode(
                QPlainTextEdit.WidgetWidth if set_mode
                else QPlainTextEdit.NoWrap
            )
        elif mode == '12':  # Send/receive (SRM)
            pass  # Not implemented
        elif mode == '25':  # Show/Hide Cursor
            self.setCursorWidth(8 if set_mode else 0)
        elif mode == '1049':  # Alternate Screen Buffer
            if set_mode and not self._using_alternate_screen:
                # Create deep copy of main screen
                doc = self.document().clone()
                self._main_screen_buffer = doc.toRawText()
                self._main_screen_formats = []
                cursor = self.textCursor()
                for i in range(len(self._main_screen_buffer)):
                    cursor.setPosition(i)
                    cursor.movePosition(QTextCursor.Right, QTextCursor.KeepAnchor)
                    self._main_screen_formats.append(cursor.charFormat())

                self._saved_cursor_position = (
                    self.textCursor().blockNumber(),
                    self.textCursor().columnNumber()
                )
                self.clear()
                self._using_alternate_screen = True
            elif not set_mode and self._using_alternate_screen:
                # Save alternate screen
                self._alternate_screen_buffer = self.toPlainText()
                self.clear()
                # Restore main screen with formats
                cursor = self.textCursor()
                cursor.insertText(self._main_screen_buffer)
                cursor.setPosition(0)
                for i, fmt in enumerate(self._main_screen_formats):
                    cursor.setPosition(i)
                    cursor.movePosition(QTextCursor.Right, QTextCursor.KeepAnchor)
                    cursor.setCharFormat(fmt)
                if self._saved_cursor_position:
                    cursor.clearSelection()
                    cursor.movePosition(QTextCursor.Start)
                    line, column = self._saved_cursor_position
                    for _ in range(line):
                        cursor.movePosition(QTextCursor.NextBlock)

                    cursor.movePosition(QTextCursor.Right, n=column)
                    self.setTextCursor(cursor)
                self._using_alternate_screen = False
        elif mode == '2004':  # Bracketed Paste Mode
            self._bracketed_paste_mode = set_mode
        elif mode == '1001s':  # Save mouse tracking state
            self._saved_mouse_tracking = self._mouse_tracking
            self._saved_mouse_tracking_sgr = self._mouse_tracking_sgr
        elif mode == '1001r':  # Restore mouse tracking state
            self._mouse_tracking = self._saved_mouse_tracking
            self._mouse_tracking_sgr = self._saved_mouse_tracking_sgr
        elif mode == '1001':  # Toggle mouse tracking
            self._mouse_tracking = set_mode
        elif mode == '1002':  # Enable mouse button tracking
            self._mouse_tracking = set_mode
        elif mode == '1006':  # Enable SGR mouse mode
            self._mouse_tracking_sgr = set_mode
        else:
            print(f"handle_private_mode: unknown mode {mode}")
            self._logger.warning(f"handle_private_mode: unknown mode {mode}")

    def _handle_ansi_mode(self, mode: str, set_mode: bool):
        """Handle ANSI mode settings."""
        if mode == '4':  # Insert Mode
            self.setOverwriteMode(not set_mode)
        elif mode == '20':  # Automatic Newline
            print(f"handle_ansi_mode: unknown mode {mode}")
            self._logger.warning(f"handle_ansi_mode: unknown mode {mode}")
            # Not implemented
        else:
            print(f"handle_ansi_mode: unknown mode {mode}")
            self._logger.warning(f"handle_ansi_mode: unknown mode {mode}")

    def _handle_simple_sequence(self, char: str) -> bool:
        """Handle simple ESC + char sequences.

        Returns:
            bool: True if sequence was handled
        """
        if char == '7':  # Save Cursor
            cursor = self.textCursor()
            self._saved_cursor_position = (
                cursor.blockNumber() - self.firstVisibleBlock().blockNumber(),
                cursor.columnNumber()
            )
            return True

        if char == '8':  # Restore Cursor
            if self._saved_cursor_position:
                line, column = self._saved_cursor_position
                self._move_cursor_to(line, column)

            return True

        if char == 'D':  # Index - Move cursor down one line
            cursor = self.textCursor()
            if cursor.blockNumber() == self.document().blockCount() - 1:
                self.insertPlainText('\n')

            cursor.movePosition(QTextCursor.Down)
            self.setTextCursor(cursor)
            return True

        if char == 'M':  # Reverse Index
            cursor = self.textCursor()
            if cursor.blockNumber() == 0:
                cursor.movePosition(QTextCursor.Start)
                cursor.insertText('\n')
                cursor.movePosition(QTextCursor.Up)
            else:
                cursor.movePosition(QTextCursor.Up)

            self.setTextCursor(cursor)
            return True

        if char == 'E':  # Next Line
            cursor = self.textCursor()
            cursor.movePosition(QTextCursor.NextBlock)
            cursor.movePosition(QTextCursor.StartOfLine)
            self.setTextCursor(cursor)
            return True

        if char == 'H':  # Horizontal Tab Set
            print(f"handle_simple_sequences: unknown char {char}")
            self._logger.warning(f"handle_simple_sequences: unknown char {char}")
            return True

        if char == 'c':  # Reset to Initial State
            self.clear()
            self._current_text_format = QTextCharFormat(self._default_text_format)
            self._scroll_region = None
            self._saved_cursor_position = None
            self._application_cursor_keys = False
            self._application_keypad_mode = False
            self._bracketed_paste_mode = False
            self.setOverwriteMode(True)
            return True

        # Character Set Selection (Just log for now)
        if char in '()':
            print(f"handle_simple_sequences: unknown char {char}")
            self._logger.warning(f"handle_simple_sequences: unknown char {char}")
            return True

        return False

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
                current_format.setForeground(self._style_manager.get_color(ColorRole.TEXT_PRIMARY))
                current_format.setBackground(self._style_manager.get_color(ColorRole.TAB_BACKGROUND_ACTIVE))
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
                current_format.setForeground(self._style_manager.get_color(ColorRole.TEXT_PRIMARY))
                current_format.setProperty(FormatProperty.CUSTOM_FOREGROUND, False)
            elif code == 49:  # Default background color
                current_format.setBackground(self._style_manager.get_color(ColorRole.TAB_BACKGROUND_ACTIVE))
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
            else:
                print(f"handle_sgr_sequence: unknown code {code}")
                self._logger.warning(f"handle_sgr_sequence: unknown code {code}")

        # Update the current text format
        self._current_text_format = current_format

    def _move_cursor_to(self, row: int, col: int) -> None:
        """
        Move cursor to specified position within visible terminal area.

        Args:
            row: Target row number (0-based) relative to visible area
            col: Target column number (0-based)

        Raises:
            ValueError: If row or col is negative
        """
        if row < 0 or col < 0:
            raise ValueError("Row and column must be non-negative")

        print(f"move cursor: {row} {col}")
        # Get the document and cursor
        document = self.document()
        cursor = self.textCursor()

        # Calculate the first visible block number
        first_visible = self.firstVisibleBlock().blockNumber()

        # Calculate target block number relative to visible area
        target_block = first_visible + row

        # Move cursor to start to prepare for movement
        cursor.movePosition(QTextCursor.Start)

        # If we need to extend the document, add newlines
        current_blocks = document.blockCount()
        blocks_needed = target_block - current_blocks
        if blocks_needed > 0:
            cursor.movePosition(QTextCursor.End)
            cursor.insertText('\n' * blocks_needed)
            cursor.movePosition(QTextCursor.Start)

        # Move to target block
        if target_block > 0:
            cursor.movePosition(QTextCursor.NextBlock, n=target_block)

        # Move to start of line
        cursor.movePosition(QTextCursor.StartOfLine)

        # Get the current line text
        block = cursor.block()
        line_text = block.text()

        # If we need to extend the line, add spaces
        spaces_needed = col - len(line_text)
        if spaces_needed > 0:
            # Move to end of existing text
            cursor.movePosition(QTextCursor.EndOfLine)
            cursor.insertText(' ' * spaces_needed)
            # Move back to start of line
            cursor.movePosition(QTextCursor.StartOfLine)
            # Now move to desired column
            cursor.movePosition(QTextCursor.Right, n=col)
        else:
            # Move to target column
            cursor.movePosition(QTextCursor.Right, n=min(col, len(line_text)))

        self.setTextCursor(cursor)

    def _handle_cursor_sequence(self, sequence: str):
        """Handle cursor movement sequences (CUU, CUD, CUF, CUB).

        Args:
            sequence: The complete escape sequence starting with ESC[

        The sequences are:
            - CUU (A): Move cursor up n lines
            - CUD (B): Move cursor down n lines
            - CUF (C): Move cursor forward n characters
            - CUB (D): Move cursor back n characters
        """
        match = re.match(r'\x1b\[(\d*)([ABCD])', sequence)
        if not match:
            self._logger.warning(f"Invalid cursor movement sequence: {sequence}")
            return False

        cursor = self.textCursor()
        current_row = cursor.blockNumber()
        current_col = cursor.columnNumber()
        count = int(match.group(1)) if match.group(1) else 1
        direction = match.group(2)

        try:
            if direction == 'A':  # Up
                self._move_cursor_to(max(0, current_row - count), current_col)
            elif direction == 'B':  # Down
                self._move_cursor_to(current_row + count, current_col)
            elif direction == 'C':  # Forward
                target_col = min(current_col + count, self._current_size.cols - 1)
                self._move_cursor_to(current_row, target_col)
            elif direction == 'D':  # Back
                self._move_cursor_to(current_row, max(0, current_col - count))
        except ValueError as e:
            self._logger.warning(f"Invalid cursor movement: {e}")

    def _insert_plain_text(self, text: str):
        """Insert text at current cursor position with scroll region and line wrapping support.

        Args:
            text: Text to insert
        """
        print(f"plain text: {repr(text)}")
        cursor = self.textCursor()
        cursor.mergeCharFormat(self._current_text_format)

        if text == '\r':
            cursor.movePosition(QTextCursor.StartOfLine)
        elif text == '\n':
            # Get current column position as we need to maintain it
            current_col = cursor.columnNumber()

            if self._scroll_region is not None:
                top, bottom = self._scroll_region
                # Get position relative to visible area
                first_visible = self.firstVisibleBlock().blockNumber()
                current_line = cursor.blockNumber() - first_visible

                if current_line == bottom:
                    # At bottom of scroll region, need to scroll
                    scroll_start_block = first_visible + top
                    scroll_text_start_block = scroll_start_block + 1
                    scroll_end_block = first_visible + bottom + 1

                    # Move to scroll region start
                    cursor.movePosition(QTextCursor.Start)
                    cursor.movePosition(QTextCursor.NextBlock, n=scroll_start_block)
                    scroll_start = cursor.position()

                    # Move to text start position
                    cursor.movePosition(QTextCursor.Start)
                    cursor.movePosition(QTextCursor.NextBlock, n=scroll_text_start_block)
                    scroll_text_start = cursor.position()

                    # Move to scroll end
                    cursor.movePosition(QTextCursor.Start)
                    cursor.movePosition(QTextCursor.NextBlock, n=scroll_end_block)
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
                    # Normal newline behavior - move down but maintain column
                    current_row = cursor.blockNumber() - first_visible
                    cursor.movePosition(QTextCursor.EndOfLine)
                    cursor.insertText('\n')
                    self._move_cursor_to(current_row + 1, current_col)
            else:
                # No scroll region - move down but maintain column
                current_row = cursor.blockNumber() - self.firstVisibleBlock().blockNumber()
                cursor.movePosition(QTextCursor.EndOfLine)
                cursor.insertText('\n')
                self._move_cursor_to(current_row + 1, current_col)
        elif text == '\b':  # Backspace/Cursor Left
            cursor.movePosition(QTextCursor.Left)
        elif text == '\t':  # Tab
            # Handle each space in overwrite mode
            spaces_to_next_tab = 8 - (cursor.columnNumber() % 8)
            for _ in range(spaces_to_next_tab):
                if not cursor.atEnd():
                    cursor.deleteChar()
                cursor.insertText(' ')
                # Check for line wrap
                if cursor.columnNumber() >= self._current_size.cols:
                    cursor.insertText('\n')
                    cursor.movePosition(QTextCursor.StartOfLine)
        elif text == '\x0b':  # Vertical tab
            # Move down one line and to start of line
            current_row = cursor.blockNumber() - self.firstVisibleBlock().blockNumber()
            self._move_cursor_to(current_row + 1, 0)
        elif text == '\x0c':  # Form feed
            self.clear()
        else:
            # Check if we need to wrap at terminal width
            if cursor.columnNumber() >= self._current_size.cols:
                cursor.insertText('\n')
                cursor.movePosition(QTextCursor.StartOfLine)

            # In a terminal, we always overwrite the character at cursor position
            if not cursor.atEnd():
                cursor.deleteChar()

            cursor.insertText(text)

            # If we're at the bottom of the visible area and not in a scroll region,
            # ensure the cursor remains visible
            if (self._scroll_region is None and
                cursor.blockNumber() - self.firstVisibleBlock().blockNumber() >= self._current_size.rows):
                # Remove the top line
                cursor.movePosition(QTextCursor.Start)
                cursor.movePosition(QTextCursor.NextBlock, QTextCursor.KeepAnchor)
                cursor.removeSelectedText()
                # Restore cursor position
                cursor.movePosition(QTextCursor.End)
                cursor.movePosition(QTextCursor.StartOfLine)
                cursor.movePosition(QTextCursor.Right, n=cursor.columnNumber())

        self.setTextCursor(cursor)
        self.ensureCursorVisible()

    def _calculate_size(self) -> TerminalSize:
        """Calculate current terminal size in rows and columns."""
        # Get precise font metrics using QFontMetricsF.  Annoyingly Qt rounds up the font metric when
        # working out how many lines to display, but rounds down when we ask it how tall a character
        # is, so we have to ensure we've rounded up!
        fm = QFontMetricsF(self.font())
        char_width = int(fm.horizontalAdvance(' ') + 0.999)
        char_height = int(fm.height() + 0.999)

        if char_width == 0 or char_height == 0:
            self._logger.warning(f"Invalid character dimensions: width={char_width}, height={char_height}")
            return TerminalSize(24, 80)  # Default fallback size

        viewport = self.viewport()
        viewport_width = viewport.width()
        viewport_height = viewport.height()

        # Calculate rows and columns
        cols = int(max(viewport_width // char_width, 1))
        rows = int(max(viewport_height // char_height, 1))

        return TerminalSize(rows, cols)

    def update_pty_size(self, fd: int) -> None:
        """Update PTY size using current terminal dimensions.

        Args:
            fd: File descriptor for PTY

        Raises:
            OSError: If ioctl call fails
        """
        try:
            size = self._calculate_size()
            print(f"pty size {size.rows}, {size.cols}")
            fcntl.ioctl(fd, termios.TIOCSWINSZ, size.to_struct())
        except OSError as e:
            self._logger.error(f"Failed to update PTY size: {e}")
            raise

    def resizeEvent(self, event: QResizeEvent):
        """Handle resize events."""
        super().resizeEvent(event)
        new_size = self._calculate_size()

        if self._current_size != new_size:
            old_size = self._current_size
            self._current_size = new_size
            self._logger.debug(
                f"Terminal size changed: {old_size} -> {new_size}"
            )
            self._reflow_content(old_size, new_size)
            # Ensure cursor position is valid after reflow
            cursor = self.textCursor()
            self._move_cursor_to(cursor.blockNumber(), min(cursor.columnNumber(), new_size.cols - 1))
            self.size_changed.emit()

    def _reflow_content(self, old_size: Optional[TerminalSize], new_size: TerminalSize):
        """Reflow terminal content for new dimensions.

        Args:
            old_size: Previous terminal size
            new_size: New terminal size
        """
        if old_size is None:
            return

        # Save cursor position relative to content
        original_cursor = self.textCursor()
        first_visible = self.firstVisibleBlock().blockNumber()

        # Calculate cursor position in characters from start of document
        char_pos = 0
        for block_num in range(first_visible, original_cursor.blockNumber()):
            block = self.document().findBlockByNumber(block_num)
            char_pos += len(block.text()) + 1  # +1 for newline
        char_pos += original_cursor.columnNumber()

        # Start reflow from beginning
        cursor = self.textCursor()
        cursor.movePosition(QTextCursor.Start)

        # Store the total number of blocks we'll process to prevent infinite loops
        total_blocks = self.document().blockCount()
        blocks_processed = 0
        chars_processed = 0  # Track processed characters to recalculate cursor position

        while not cursor.atEnd() and blocks_processed < total_blocks:
            block_start = cursor.position()
            cursor.movePosition(QTextCursor.EndOfLine, QTextCursor.KeepAnchor)
            line = cursor.selectedText()

            if len(line) > new_size.cols:
                # Reset cursor to start of current line
                cursor.setPosition(block_start)
                remaining_text = line

                while remaining_text:
                    # Take the next chunk that fits in the new width
                    chunk = remaining_text[:new_size.cols]
                    remaining_text = remaining_text[new_size.cols:]

                    # Select and replace current line's content
                    cursor.movePosition(QTextCursor.EndOfLine, QTextCursor.KeepAnchor)
                    cursor.insertText(chunk)
                    chars_processed += len(chunk)

                    if remaining_text:  # More text to wrap
                        cursor.insertText('\n')
                        chars_processed += 1

                    # Update cursor position calculation if we've wrapped a line before the original cursor
                    if chars_processed <= char_pos:
                        char_pos += 1  # Account for the added newline in cursor position

                cursor.movePosition(QTextCursor.NextBlock)
            else:
                # Line fits, move to next block
                chars_processed += len(line) + 1  # +1 for newline
                cursor.clearSelection()
                cursor.movePosition(QTextCursor.NextBlock)

            blocks_processed += 1

        # Calculate new cursor position
        cursor.movePosition(QTextCursor.Start)
        remaining_chars = char_pos
        new_block_num = 0
        new_col = 0

        while remaining_chars > 0 and not cursor.atEnd():
            block = cursor.block()
            line_length = len(block.text())

            if remaining_chars > line_length:
                # Move to next block
                cursor.movePosition(QTextCursor.NextBlock)
                remaining_chars -= (line_length + 1)  # +1 for newline
                new_block_num += 1
            else:
                # Found the right block and column
                new_col = remaining_chars
                remaining_chars = 0

        # Move cursor to calculated position
        self._move_cursor_to(
            new_block_num - first_visible,  # Convert to visible-area relative position
            min(new_col, new_size.cols - 1)  # Ensure we don't exceed terminal width
        )

    def clear(self):
        """Clear the terminal."""
        self.setPlainText("")
