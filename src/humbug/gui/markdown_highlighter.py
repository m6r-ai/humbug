"""Markdown code highlighter."""

import logging
from typing import Optional

from PySide6.QtGui import (
    QTextCharFormat, QSyntaxHighlighter, QColor,
    QTextDocument, QTextBlockUserData
)
from PySide6.QtCore import Signal

from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager
from humbug.syntax.markdown_parser import MarkdownParser


class ParserData(QTextBlockUserData):
    def __init__(self):
        super().__init__()
        self.fence = False


class MarkdownHighlighter(QSyntaxHighlighter):
    """Syntax highlighter for Markdown code blocks."""

    # Signal emitted when code block state changes
    codeBlockStateChanged = Signal(bool)

    def __init__(self, parent: Optional[QTextDocument] = None) -> None:
        """Initialize the highlighter."""
        super().__init__(parent)

        # Consistent font family fallback sequence for all code formats
        self._code_font_families = ["Menlo", "Monaco", "Courier New", "monospace"]
        self._has_code_block = False

        style_manager = StyleManager()

        # For inline code
        self._code_format = QTextCharFormat()
        self._code_format.setFontFamilies(self._code_font_families)
        self._code_format.setFontFixedPitch(True)
        self._code_format.setForeground(style_manager.get_color(ColorRole.SYNTAX_CODE))
        self._code_format.setBackground(style_manager.get_color(ColorRole.CODE_BLOCK_BACKGROUND))

        # For fenced format
        self._fence_format = QTextCharFormat()
        self._fence_format.setFontFamilies(self._code_font_families)
        self._fence_format.setFontFixedPitch(True)

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

        while True:
            token = parser.get_next_token()
            if token is None:
                break

            if token.type == 'FENCE':
                self.setFormat(0, len(text), self._fence_format)

                in_fenced_block = not in_fenced_block
                continue

            if token.type == 'BACKTICK':
                if not in_fenced_block:
                    in_code_block = not in_code_block
                    continue

            if in_fenced_block:
                self.setFormat(token.start, len(token.value), self._fence_format)
                continue

            if in_code_block:
                self.setFormat(token.start, len(token.value), self._code_format)

        parser_data = ParserData()
        parser_data.fence = in_fenced_block
        current_block.setUserData(parser_data)

        # Check if document contains any code blocks
        has_code = False
        block = self.document().firstBlock()
        while block.isValid():
            data = block.userData()
            if data and data.fence:
                has_code = True
                break
            block = block.next()

        # Emit signal if state changed
        if has_code != self._has_code_block:
            self._has_code_block = has_code
            self.codeBlockStateChanged.emit(has_code)
