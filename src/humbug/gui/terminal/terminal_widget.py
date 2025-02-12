"""Terminal widget implementation."""

from dataclasses import dataclass
from typing import List, Optional, Dict, Tuple
from enum import Flag, auto
import array
import logging
import struct
from PySide6.QtWidgets import QWidget
from PySide6.QtCore import Qt, Signal, QRect, QPoint
from PySide6.QtGui import (
    QPainter, QPaintEvent, QColor, QFontMetrics, QFont,
    QResizeEvent, QKeyEvent, QMouseEvent, QTextCursor,
    QGuiApplication
)


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
        """
        Convert terminal size to struct format for TIOCSWINSZ.

        Returns:
            bytes: Packed struct in format suitable for TIOCSWINSZ ioctl
        """
        return struct.pack('HHHH', self.rows, self.cols, 0, 0)


class CharacterAttributes(Flag):
    """Bit flags for character attributes."""
    NONE = 0
    BOLD = auto()
    ITALIC = auto()
    UNDERLINE = auto()
    CUSTOM_FG = auto()
    CUSTOM_BG = auto()


class TerminalLine:
    """Fixed-width line of terminal characters."""
    def __init__(self, width: int):
        """Initialize empty line with given width."""
        self.width = width
        # For each character cell we store:
        # - Unicode codepoint (4 bytes)
        # - Attributes flags (4 bytes)
        # - FG color (4 bytes)
        # - BG color (4 bytes)
        self.data = array.array('L', [0] * (width * 4))

    def set_character(
        self,
        index: int,
        char: str,
        attributes: CharacterAttributes = CharacterAttributes.NONE,
        fg_color: Optional[int] = None,
        bg_color: Optional[int] = None
    ):
        """Set character and attributes at position."""
        if 0 <= index < self.width:
            base = index * 4
            self.data[base] = ord(char)
            self.data[base + 1] = attributes.value
            self.data[base + 2] = fg_color if fg_color is not None else 0
            self.data[base + 3] = bg_color if bg_color is not None else 0

    def get_character(self, index: int) -> Tuple[str, CharacterAttributes, Optional[int], Optional[int]]:
        """Get character and attributes at position."""
        if 0 <= index < self.width:
            base = index * 4
            char = chr(self.data[base])
            attributes = CharacterAttributes(self.data[base + 1])
            fg_color = self.data[base + 2] if self.data[base + 2] != 0 else None
            bg_color = self.data[base + 3] if self.data[base + 3] != 0 else None
            return (char, attributes, fg_color, bg_color)
        return (' ', CharacterAttributes.NONE, None, None)


class TerminalWidget(QWidget):
    """Terminal widget implementation."""

    data_ready = Signal(bytes)  # Emitted when user input is ready
    size_changed = Signal()  # Emitted when terminal size changes

    def __init__(self, parent: Optional[QWidget] = None):
        """Initialize terminal widget."""
        super().__init__(parent)
        self._logger = logging.getLogger("TerminalWidget")

        # Enable focus and input
        self.setFocusPolicy(Qt.StrongFocus)

        # Storage for terminal content
        self._lines: List[TerminalLine] = []
        self._viewport_offset = 0  # Lines scrolled up from bottom

        # Terminal dimensions
        self._rows = 0
        self._cols = 0

        # Cursor state
        self._cursor_row = 0
        self._cursor_col = 0
        self._cursor_visible = True

        # Selection state
        self._selection_start: Optional[Tuple[int, int]] = None  # (row, col)
        self._selection_end: Optional[Tuple[int, int]] = None    # (row, col)
        self._selecting = False

        # ANSI escape sequence handling
        self._escape_seq_buffer = ""
        self._in_escape_seq = False

        # Text cursor for selection
        self._text_cursor = QTextCursor()

        # Default colors (can be customized later)
        self._default_fg = QColor(Qt.white).rgb()
        self._default_bg = QColor(Qt.black).rgb()

        self._current_size = None

        # Calculate initial size
        self._update_dimensions()
        self._initialize_buffer()

    def calculate_size(self) -> TerminalSize:
        """Calculate current terminal size in rows and columns."""
        fm = QFontMetrics(self.font())
        char_width = fm.horizontalAdvance(' ')
        char_height = fm.height()

        if char_width <= 0 or char_height <= 0:
            self._logger.warning(f"Invalid character dimensions: width={char_width}, height={char_height}")
            return TerminalSize(24, 80)  # Default fallback size

        viewport_width = self.width()
        viewport_height = self.height()

        cols = max(viewport_width // char_width, 1)
        rows = max(viewport_height // char_height, 1)

        return TerminalSize(rows, cols)

    def _update_dimensions(self) -> None:
        """Update terminal dimensions based on widget size and font metrics."""
        new_size = self.calculate_size()

        if new_size.cols != self._cols or new_size.rows != self._rows:
            old_rows, old_cols = self._rows, self._cols
            self._rows, self._cols = new_size.rows, new_size.cols

            if old_rows > 0 and old_cols > 0:
                print("update dims")
                self._reflow_content(old_rows, old_cols)

            self.size_changed.emit()

    def _initialize_buffer(self) -> None:
        """Initialize empty terminal buffer."""
        # Create initial lines
        self._lines = []
        print(f"init buffer {self._rows}")
        self._add_new_lines(self._rows)

    def _add_new_lines(self, count: int) -> None:
        """Add new empty lines to the buffer."""
        for _ in range(count):
            line = TerminalLine(self._cols)
            # Fill line with spaces using default attributes
            for i in range(self._cols):
                line.set_character(i, ' ')

            self._lines.append(line)

    def _write_char(self, char: str) -> None:
        """Write a single character at the current cursor position."""
        if char == '\r':
            self._cursor_col = 0
            return

        if char == '\n':
            if self._cursor_row == self._rows - 1:
                # Add new line to history and scroll
                self._add_new_lines(1)
            else:
                self._cursor_row += 1
            return

        if char == '\b':
            self._cursor_col = max(0, self._cursor_col - 1)
            return

        if char == '\t':
            # Move to next tab stop (every 8 columns)
            spaces = 8 - (self._cursor_col % 8)
            self._cursor_col = min(self._cursor_col + spaces, self._cols - 1)
            return

        # Handle printable characters
        if ord(char) >= 32:
            # Get current line
            print(f"write char {repr(char)} {len(self._lines)} {self._rows}")
            line_index = len(self._lines) - self._rows + self._cursor_row
            if line_index >= 0 and line_index < len(self._lines):
                print(f"update line {line_index} {self._cursor_col}")
                line = self._lines[line_index]

                # Write character
                line.set_character(self._cursor_col, char)

                # Move cursor
                self._cursor_col += 1
                if self._cursor_col >= self._cols:
                    self._cursor_col = 0
                    if self._cursor_row < self._rows - 1:
                        self._cursor_row += 1
                    else:
                        # Add new line and scroll
                        self._add_new_lines(1)

                # Update the affected area
                self.update()

    def put_data(self, data: bytes) -> None:
        """Display received data with ANSI sequence handling.

        Args:
            data: Raw bytes from terminal

        Raises:
            UnicodeDecodeError: If data cannot be decoded
        """
        if not self._current_size:
            self._current_size = self.calculate_size()

        text = data.decode(errors='replace')

        print(f"put data {repr(text)}")
        i = 0
        while i < len(text):
            char = text[i]

            if self._in_escape_seq:
                self._escape_seq_buffer += char

                # Process escape sequence when complete
                if self._is_escape_sequence_complete(self._escape_seq_buffer):
#                    self._process_escape_sequence(self._escape_seq_buffer)
                    self._escape_seq_buffer = ""
                    self._in_escape_seq = False
                elif len(self._escape_seq_buffer) > 128:  # Safety limit
                    self._logger.warning(f"Escape sequence too long, discarding: {repr(self._escape_seq_buffer)}")
                    self._escape_seq_buffer = ""
                    self._in_escape_seq = False

            elif char == '\x1b':  # Start of new escape sequence
                self._in_escape_seq = True
                self._escape_seq_buffer = char

            else:
                self._write_char(char)

            i += 1

    def _is_escape_sequence_complete(self, sequence: str) -> bool:
        """Check if an escape sequence is complete.

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

        # Simple ESC sequences
        return len(sequence) == 2 and sequence[1] in '=>\7\\8cDEHM'

    def _reflow_content(self, old_rows: int, old_cols: int) -> None:
        """Reflow terminal content for new dimensions."""
        # Create new lines with new width
        new_lines = []
        print(f"reflow {old_rows},{old_cols} -> {self._rows},{self._cols}")

        # Copy content from old lines, truncating or padding as needed
        for old_line in self._lines:
            new_line = TerminalLine(self._cols)

            # Copy existing characters
            for col in range(min(old_cols, self._cols)):
                char, attributes, fg_color, bg_color = old_line.get_character(col)
                new_line.set_character(col, char, attributes, fg_color, bg_color)

            # Pad with empty characters if needed
            for col in range(old_cols, self._cols):
                new_line.set_character(col, ' ')

            new_lines.append(new_line)

        # Add additional empty lines if needed
        lines_needed = self._rows - len(new_lines)
        if lines_needed > 0:
            for _ in range(lines_needed):
                new_line = TerminalLine(self._cols)
                for col in range(self._cols):
                    new_line.set_character(col, ' ')

                new_lines.append(new_line)

        self._lines = new_lines

        # Update cursor position if needed
        self._cursor_col = min(self._cursor_col, self._cols - 1)
        self._cursor_row = min(self._cursor_row, self._rows - 1)

        # Force complete repaint
        self.update()

    def _pixel_pos_to_text_pos(self, pos: QPoint) -> Tuple[int, int]:
        """Convert pixel coordinates to text position."""
        fm = QFontMetrics(self.font())
        char_width = fm.horizontalAdvance(' ')
        char_height = fm.height()

        col = max(0, min(pos.x() // char_width, self._cols - 1))
        row = max(0, min(pos.y() // char_height, self._rows - 1))

        return (row, col)

    def mousePressEvent(self, event: QMouseEvent) -> None:
        """Handle mouse press for selection."""
        if event.button() == Qt.LeftButton:
            self._selecting = True
            pos = self._pixel_pos_to_text_pos(event.position().toPoint())
            self._selection_start = pos
            self._selection_end = pos
            self._update_text_cursor()
            self.update()
        super().mousePressEvent(event)

    def mouseReleaseEvent(self, event: QMouseEvent) -> None:
        """Handle mouse release for selection."""
        if event.button() == Qt.LeftButton:
            self._selecting = False
        super().mouseReleaseEvent(event)

    def mouseMoveEvent(self, event: QMouseEvent) -> None:
        """Handle mouse movement for selection."""
        if self._selecting:
            pos = self._pixel_pos_to_text_pos(event.position().toPoint())
            if pos != self._selection_end:
                self._selection_end = pos
                self._update_text_cursor()
                self.update()
        super().mouseMoveEvent(event)

    def keyPressEvent(self, event: QKeyEvent) -> None:
        """Handle key press events."""
        text = event.text()
        if text:
            self.data_ready.emit(text.encode())
        event.accept()

    def resizeEvent(self, event: QResizeEvent) -> None:
        """Handle resize events."""
        super().resizeEvent(event)
        print("resizeevent")
        self._update_dimensions()

    def paintEvent(self, event: QPaintEvent) -> None:
        """Handle paint events efficiently."""
        print("paint event")
        painter = QPainter(self)

        # Get font metrics for character dimensions
        fm = QFontMetrics(self.font())
        char_width = fm.horizontalAdvance(' ')
        char_height = fm.height()

        # Get the region that needs repainting
        region = event.rect()

        # Calculate the character cell range to repaint
        start_row = max(0, region.top() // char_height)
        end_row = min(self._rows, (region.bottom() + char_height - 1) // char_height)
        start_col = max(0, region.left() // char_width)
        end_col = min(self._cols, (region.right() + char_width - 1) // char_width)

        # Paint visible character cells
        for row in range(start_row, end_row):
            y = row * char_height

            # Get actual line index accounting for scroll position
            line_index = len(self._lines) - self._rows + row
            if line_index < 0 or line_index >= len(self._lines):
                continue

            line = self._lines[line_index]

            for col in range(start_col, end_col):
                x = col * char_width

                # Get character
                char, attributes, fg_color, bg_color = line.get_character(col)

                # Draw background if not default
                if bg_color is not None:
                    painter.fillRect(
                        QRect(x, y, char_width, char_height),
                        QColor(bg_color)
                    )

                # Set up font attributes
                font = painter.font()
                if attributes & CharacterAttributes.BOLD:
                    font.setBold(True)
                if attributes & CharacterAttributes.ITALIC:
                    font.setItalic(True)
                if attributes & CharacterAttributes.UNDERLINE:
                    font.setUnderline(True)

                painter.setFont(font)

                # Draw character
                if fg_color is not None:
                    painter.setPen(QColor(fg_color))
                else:
                    painter.setPen(QColor(self._default_fg))

                painter.drawText(x, y + fm.ascent(), char)

        # Draw selection if active
        if self._selection_start and self._selection_end:
            start_row, start_col = self._selection_start
            end_row, end_col = self._selection_end

            # Ensure start is before end
            if (start_row > end_row) or (start_row == end_row and start_col > end_col):
                start_row, start_col, end_row, end_col = end_row, end_col, start_row, start_col

            selection_color = self.palette().highlight().color()
            selection_text_color = self.palette().highlightedText().color()

            for row in range(max(start_row, 0), min(end_row + 1, self._rows)):
                if row < start_row or row > end_row:
                    continue

                y = row * char_height

                # Calculate selection range for this row
                row_start = start_col if row == start_row else 0
                row_end = end_col if row == end_row else self._cols

                # Draw selection background
                selection_rect = QRect(
                    row_start * char_width,
                    y,
                    (row_end - row_start) * char_width,
                    char_height
                )

                if selection_rect.intersects(event.rect()):
                    painter.fillRect(selection_rect, selection_color)

                    # Draw selected text
                    line_index = len(self._lines) - self._rows + row
                    if 0 <= line_index < len(self._lines):
                        line = self._lines[line_index]
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

        # Draw cursor if visible
        if self._cursor_visible and self._viewport_offset == 0:
            cursor_x = self._cursor_col * char_width
            cursor_y = self._cursor_row * char_height

            if QRect(cursor_x, cursor_y, char_width, char_height).intersects(region):
                # Get character under cursor for inversion
                line_index = len(self._lines) - self._rows + self._cursor_row
                if 0 <= line_index < len(self._lines):
                    line = self._lines[line_index]
                    char, _attributes, _fg_color, _bg_color = line.get_character(self._cursor_col)

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

    def _update_text_cursor(self) -> None:
        """Update text cursor based on selection state."""
        if not self._selection_start or not self._selection_end:
            self._text_cursor = QTextCursor()
            return

        # Create a cursor that represents the selection
        cursor = QTextCursor()
        cursor.setPosition(0)  # Start position

        # Calculate selection range
        start_row, start_col = self._selection_start
        end_row, end_col = self._selection_end

        # Ensure start is before end
        if (start_row > end_row) or (start_row == end_row and start_col > end_col):
            start_row, start_col, end_row, end_col = end_row, end_col, start_row, start_col

        cursor.setPosition(start_row * self._cols + start_col)
        cursor.setPosition(end_row * self._cols + end_col, QTextCursor.KeepAnchor)

        self._text_cursor = cursor

    def _get_selected_text(self) -> str:
        """Get currently selected text."""
        if not self._selection_start or not self._selection_end:
            return ""

        # Calculate selection range
        start_row, start_col = self._selection_start
        end_row, end_col = self._selection_end

        # Ensure start is before end
        if (start_row > end_row) or (start_row == end_row and start_col > end_col):
            start_row, start_col, end_row, end_col = end_row, end_col, start_row, start_col

        # Build selected text
        text = []
        for row in range(start_row, end_row + 1):
            line_index = len(self._lines) - self._rows + row
            if 0 <= line_index < len(self._lines):
                line = self._lines[line_index]

                start = start_col if row == start_row else 0
                end = end_col if row == end_row else self._cols

                row_text = ""
                for col in range(start, end):
                    char, _attributes, _fg_color, _bg_color = line.get_character(col)
                    row_text += char

                text.append(row_text.rstrip())  # Remove trailing spaces

        return "\n".join(text)

    def _clear_selection(self) -> None:
        """Clear current selection."""
        self._selection_start = None
        self._selection_end = None
        self._update_text_cursor()
        self.update()

    # Interface methods required by TerminalTab
    def textCursor(self) -> QTextCursor:
        """Return current text cursor (for compatibility with QPlainTextEdit)."""
        return self._text_cursor

    def setTextCursor(self, cursor: QTextCursor) -> None:
        """Set text cursor (for compatibility with QPlainTextEdit)."""
        self._text_cursor = cursor
        self.update()

    def cut(self) -> None:
        """Cut selected text to clipboard."""
        self.copy()
        self._clear_selection()

    def copy(self) -> None:
        """Copy selected text to clipboard."""
        if not self._selection_start or not self._selection_end:
            return

        # Get selected text
        text = self._get_selected_text()
        if text:
            QGuiApplication.clipboard().setText(text)

    def paste(self) -> None:
        """Paste text from clipboard."""
        text = QGuiApplication.clipboard().text()
        if text:
            self.put_data(text.encode())

    def scroll_to(self, position: int) -> None:
        """
        Scroll terminal to specified position.

        Args:
            position: Number of lines to scroll up from bottom
        """
        old_offset = self._viewport_offset
        self._viewport_offset = min(
            max(0, position),
            max(0, len(self._lines) - self._rows)
        )

        if self._viewport_offset != old_offset:
            self.update()

    def clear(self) -> None:
        """Clear the terminal."""
        self._lines = []
        self._add_new_lines(self._rows)
        self._cursor_row = 0
        self._cursor_col = 0
        self._clear_selection()
        self.update()
