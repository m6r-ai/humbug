"""Editor highlighter."""

import logging
from typing import Optional

from PySide6.QtGui import (
    QTextCharFormat, QSyntaxHighlighter,
    QTextDocument, QTextBlockUserData
)
from PySide6.QtCore import Signal

from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager
from humbug.syntax.programming_language import ProgrammingLanguage
from humbug.syntax.parser_factory import ParserFactory


class EditorHighlighterBlockData(QTextBlockUserData):
    """Data associated with each text block."""
    def __init__(self):
        super().__init__()
        self.fence_depth = 0
        self.parser_state = None


class EditorHighlighter(QSyntaxHighlighter):
    """Syntax highlighter for source code files."""

    # Signal emitted when code block state changes
    codeBlockStateChanged = Signal(bool)

    def __init__(self, parent: Optional[QTextDocument] = None) -> None:
        """Initialize the highlighter."""
        super().__init__(parent)

        # Consistent font family fallback sequence for all code formats
        self._code_font_families = ["Menlo", "Monaco", "Courier New", "monospace"]
        self._has_code_block = False
        self._language = ProgrammingLanguage.TEXT
        self._style_manager = StyleManager()

        # For inline code
        self._code_format = QTextCharFormat()
        self._code_format.setFontFamilies(self._code_font_families)
        self._code_format.setFontFixedPitch(True)
        self._code_format.setForeground(self._style_manager.get_color(ColorRole.SYNTAX_CODE))
        self._code_format.setBackground(self._style_manager.get_color(ColorRole.CODE_BLOCK_BACKGROUND))

        # For fenced format
        self._fence_format = QTextCharFormat()
        self._fence_format.setFontFamilies(self._code_font_families)
        self._fence_format.setFontFixedPitch(True)

        self._logger = logging.getLogger("EditorHighlighter")

    def set_language(self, language: ProgrammingLanguage) -> None:
        """
        Set the programming language for syntax highlighting.

        Args:
            language: The programming language to use
        """
        if self._language != language:
            self._language = language
            self.rehighlight()

    def highlightBlock(self, text: str) -> None:
        """Apply highlighting to the given block of text."""
        try:
            current_block = self.currentBlock()
            prev_block = current_block.previous()

            prev_block_data: EditorHighlighterBlockData = None
            prev_parser_state = None
            fence_depth = 0

            if prev_block:
                prev_block_data = prev_block.userData()
                if prev_block_data:
                    prev_parser_state = prev_block_data.parser_state
                    fence_depth = prev_block_data.fence_depth

            continuation_state = -1
            current_block_data: EditorHighlighterBlockData = current_block.userData()

            # Use the appropriate language parser
            parser = ParserFactory.create_parser(self._language)
            if not parser:
                return

            parser_state = parser.parse(prev_parser_state, text)

            # Apply syntax highlighting based on token types
            while True:
                token = parser.get_next_token()
                if token is None:
                    break

                self.setFormat(
                    token.start,
                    len(token.value),
                    self._style_manager.get_highlight(token.type)
                )

            # Check if we need to rehighlight everything from this block onwards
            if current_block_data:
                current_parser_state = current_block_data.parser_state
                if current_parser_state:
                    continuation_state = current_parser_state.continuation_state

            if continuation_state != parser_state.continuation_state:
                self.setCurrentBlockState(self.currentBlockState() + 1)

            block_data = EditorHighlighterBlockData()
            block_data.parser_state = parser_state
            block_data.fence_depth = fence_depth
            current_block.setUserData(block_data)

            # Check if document contains any code blocks (only relevant for text mode)
            if self._language == ProgrammingLanguage.TEXT:
                has_code = False
                block = self.document().firstBlock()
                while block.isValid():
                    data = block.userData()
                    if data and data.fence_depth > 0:
                        has_code = True
                        break

                    block = block.next()

                # Emit signal if state changed
                if has_code != self._has_code_block:
                    self._has_code_block = has_code
                    self.codeBlockStateChanged.emit(has_code)

        except Exception:
            self._logger.exception("highlighting exception")
