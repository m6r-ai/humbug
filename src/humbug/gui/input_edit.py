"""Input edit widget with Markdown code highlighting."""

import logging
from PySide6.QtWidgets import QFrame, QTextEdit, QSizePolicy
from PySide6.QtCore import Qt, QSize, Signal, QMimeData
from PySide6.QtGui import (
    QKeyEvent, QTextCharFormat, QSyntaxHighlighter, QColor,
    QTextCursor, QTextBlockFormat
)

# Set up logging
logger = logging.getLogger("input_edit")
logger.setLevel(logging.DEBUG)


class MarkdownHighlighter(QSyntaxHighlighter):
    """Syntax highlighter for Markdown code blocks."""

    def __init__(self, parent=None):
        """Initialize the highlighter."""
        super().__init__(parent)
        self.code_format = QTextCharFormat()
        # Use a monospace font for code, with fallbacks
        self.code_format.setFontFamilies(["Menlo", "Monaco", "Courier New", "monospace"])
        self.code_format.setFontFixedPitch(True)

        # For code blocks and fences, use monospace and background
        self.block_format = QTextCharFormat()
        self.block_format.setFontFamilies(["Menlo", "Monaco", "Courier New", "monospace"])
        self.block_format.setFontFixedPitch(True)
        self.block_format.setBackground(QColor("#2d2d2d"))

        # Create block format for full-width background
        self.code_block_format = QTextBlockFormat()
        self.code_block_format.setBackground(QColor("#2d2d2d"))
        self.normal_block_format = QTextBlockFormat()

    def highlightBlock(self, text):
        """Apply highlighting to the given block of text."""
        # Get the current text block
        current_block = self.currentBlock()
        cursor = QTextCursor(current_block)

        # Get the previous state
        prev_state = self.previousBlockState()
        if prev_state == -1:  # Initial state
            prev_state = 0

        # Are we currently inside a code block?
        in_code_block = (prev_state == 1)

        # Check if this line is a code fence
        if self.is_code_fence(text):
            # Set format for the fence itself
            self.setFormat(0, len(text), self.block_format)
            # Set full-width background
            cursor.setBlockFormat(self.code_block_format)
            # Toggle the state for the next line
            new_state = 0 if in_code_block else 1
            self.setCurrentBlockState(new_state)
        else:
            # Not a fence - use code block formatting if we're inside a block
            if in_code_block:
                self.setFormat(0, len(text), self.block_format)
                cursor.setBlockFormat(self.code_block_format)
            else:
                cursor.setBlockFormat(self.normal_block_format)
            # Keep the same state
            self.setCurrentBlockState(prev_state)

            # Handle inline code only if we're not in a code block
            if not in_code_block:
                i = 0
                while i < len(text):
                    if text[i] == '`':
                        start = i
                        i += 1
                        while i < len(text):
                            if text[i] == '`':
                                # Found the closing backtick
                                self.setFormat(start, i - start + 1, self.code_format)
                                break
                            i += 1
                    i += 1

    @staticmethod
    def is_code_fence(text: str) -> bool:
        """Check if text contains a code fence marker (```)."""
        count = 0
        i = 0
        # Skip leading whitespace
        while i < len(text) and text[i].isspace():
            i += 1

        # Count consecutive backticks
        while i < len(text) and text[i] == '`':
            count += 1
            i += 1

        return count == 3


class InputEdit(QTextEdit):
    """Editable input area for user messages with Markdown code highlighting."""

    submitted = Signal(str)

    def __init__(self, parent=None):
        """Initialize the input edit area."""
        super().__init__(parent)
        self.setFrameStyle(QFrame.NoFrame)
        self.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        self.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Minimum)

        # Input history
        self.input_history = []
        self.history_index = -1
        self.current_input = ""

        # Add the syntax highlighter
        self.highlighter = MarkdownHighlighter(self.document())

        # Watch for document changes
        self.document().contentsChanged.connect(self._on_content_changed)

        self.setStyleSheet("""
            QTextEdit {
                background-color: black;
                color: white;
                selection-background-color: #606060;
                border: none;
            }
            QTextEdit:focus {
                background-color: #404040;
            }
        """)

    def _on_content_changed(self):
        """Handle content changes."""
        self.updateGeometry()

    def minimumSizeHint(self) -> QSize:
        """Calculate the minimum size needed."""
        height = max(int(self.document().size().height()), 40)
        width = self.width() if self.width() > 0 else self.minimumWidth()
        return QSize(width, height)

    def sizeHint(self) -> QSize:
        """Calculate the desired size."""
        return self.minimumSizeHint()

    def keyPressEvent(self, event: QKeyEvent):
        """Handle special key events."""
        if event.key() == Qt.Key_J and event.modifiers() == Qt.ControlModifier:
            text = self.toPlainText().strip()
            if text:
                self.submitted.emit(text)
                if text not in self.input_history:
                    self.input_history.append(text)
                self.history_index = -1
                self.clear()
            return

        if self.textCursor().atStart() and not self.textCursor().hasSelection():
            if event.key() == Qt.Key_Up and self.input_history:
                if self.history_index == -1:
                    self.current_input = self.toPlainText()
                self.history_index = min(len(self.input_history) - 1,
                                       self.history_index + 1)
                self.setPlainText(self.input_history[-self.history_index - 1])
                return

            if event.key() == Qt.Key_Down:
                if self.history_index > 0:
                    self.history_index -= 1
                    self.setPlainText(self.input_history[-self.history_index - 1])
                elif self.history_index == 0:
                    self.history_index = -1
                    self.setPlainText(self.current_input)
                return

        super().keyPressEvent(event)

    def insertFromMimeData(self, source: QMimeData) -> None:
        """Override default paste behavior to insert only plain text."""
        if source.hasText():
            cursor = self.textCursor()
            cursor.insertText(source.text())
