"""Markdown code highlighter."""

import logging
from typing import Optional

from PySide6.QtGui import (
    QTextCharFormat, QSyntaxHighlighter, QColor,
    QTextCursor, QTextBlockFormat, QTextDocument
)

from humbug.syntax.markdown_parser import MarkdownParser

class MarkdownHighlighter(QSyntaxHighlighter):
    """Syntax highlighter for Markdown code blocks."""

    def __init__(self, parent: Optional[QTextDocument] = None) -> None:
        """Initialize the highlighter."""
        super().__init__(parent)

        # Consistent font family fallback sequence for all code formats
        self._code_font_families = ["Menlo", "Monaco", "Courier New", "monospace"]

        # For inline code
        self._code_format = QTextCharFormat()
        self._code_format.setFontFamilies(self._code_font_families)
        self._code_format.setFontFixedPitch(True)

        # For code blocks and fences
        self._block_format = QTextCharFormat()
        self._block_format.setFontFamilies(self._code_font_families)
        self._block_format.setFontFixedPitch(True)
        self._block_format.setBackground(QColor("#2d2d2d"))

        # Create block format for full-width background
        self._code_block_format = QTextBlockFormat()
        self._code_block_format.setBackground(QColor("#2d2d2d"))
        self._normal_block_format = QTextBlockFormat()

        self._logger = logging.getLogger("MainWindow")

    def highlightBlock(self, text: str) -> None:
        """Apply highlighting to the given block of text."""
        self._logger.debug(f"highlight block: {text}")

        parser = MarkdownParser(text)

        while True:
            token = parser.get_next_token()
            if token is None:
                break

            self._logger.debug(f"token {token}")

#        # Apply formatting
#        if in_code_block or is_fence:
#            # Apply code block formatting
#            self.setFormat(0, len(text), self.block_format)
#            cursor = QTextCursor(current_block)
#            cursor.setBlockFormat(self.code_block_format)
#        else:
#            # Apply normal formatting and look for inline code
#            cursor = QTextCursor(current_block)
#            cursor.setBlockFormat(self.normal_block_format)
#            i = 0
#            while i < len(text):
#                if text[i] == '`':
#                    start = i
#                    i += 1
#                    while i < len(text):
#                        if text[i] == '`':
#                            # Found the closing backtick
#                            self.setFormat(start, i - start + 1, self.code_format)
#                            self.logger.debug("Applied inline code format %d-%d", start, i + 1)
#                            break
#                        i += 1
#                i += 1
#
#        # Set state for next block
#        new_state = self.CODE_BLOCK_STATE if in_code_block else self.NORMAL_STATE
#        self.setCurrentBlockState(new_state)
#
#        if new_state != prev_state:
#            self.logger.debug("State changed from %d to %d", prev_state, new_state)
