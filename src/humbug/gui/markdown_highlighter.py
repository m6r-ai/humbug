"""Markdown code highlighter."""

from typing import Optional

from PySide6.QtGui import (
    QTextCharFormat, QSyntaxHighlighter, QColor,
    QTextCursor, QTextBlockFormat, QTextDocument
)


class MarkdownHighlighter(QSyntaxHighlighter):
    """Syntax highlighter for Markdown code blocks."""

    def __init__(self, parent: Optional[QTextDocument] = None) -> None:
        """Initialize the highlighter."""
        super().__init__(parent)

        # Consistent font family fallback sequence for all code formats
        self.code_font_families = ["Menlo", "Monaco", "Courier New", "monospace"]

        # For inline code
        self.code_format = QTextCharFormat()
        self.code_format.setFontFamilies(self.code_font_families)
        self.code_format.setFontFixedPitch(True)

        # For code blocks and fences
        self.block_format = QTextCharFormat()
        self.block_format.setFontFamilies(self.code_font_families)
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
