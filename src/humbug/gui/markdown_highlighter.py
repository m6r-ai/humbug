"""Markdown code highlighter."""

import logging
from typing import Optional

from PySide6.QtGui import (
    QTextCharFormat, QSyntaxHighlighter, QColor,
    QTextCursor, QTextBlockFormat, QTextDocument, QTextBlockUserData
)

from humbug.syntax.markdown_parser import MarkdownParser


class ParserData(QTextBlockUserData):
    def __init__(self):
        super().__init__()
        self.fence = False


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

        self._logger = logging.getLogger("MarkdownHighlighter")

    def highlightBlock(self, text: str) -> None:
        """Apply highlighting to the given block of text."""
        parser = MarkdownParser(text)
        current_block = self.currentBlock()
        prev_block = current_block.previous()

        prev_parser_data = None
        if prev_block:
            prev_parser_data = prev_block.userData()

        if not prev_parser_data:
            prev_parser_data = ParserData()

        in_fenced_block = prev_parser_data.fence
        in_code_block = False

        # Set our background based on how we last saw things
        block_format = self._code_block_format if in_fenced_block else self._normal_block_format
        cursor = QTextCursor(current_block)
        cursor.setBlockFormat(block_format)

        while True:
            token = parser.get_next_token()
            print(f"token {token}")
            if token is None:
                break

            match token.type:
                case 'FENCE':
                    in_fenced_block = not in_fenced_block
                    if in_fenced_block:
                        # When we detect an opening fence highlight it as text too
                        block_format = self._code_block_format if in_fenced_block else self._normal_block_format
                        cursor = QTextCursor(current_block)
                        cursor.setBlockFormat(block_format)

                case 'BACKTICK':
                    in_code_block = not in_code_block

                case _:
                    if in_code_block or in_fenced_block:
                        self.setFormat(token.start, len(token.value), self._code_format)

        parser_data = ParserData()
        parser_data.fence = in_fenced_block
        current_block.setUserData(parser_data)
