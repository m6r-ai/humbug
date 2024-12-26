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
from humbug.syntax.chat_parser import ChatParser, ChatParserState, ProgrammingLanguage


class EditorHighlighterBlockData(QTextBlockUserData):
    def __init__(self):
        super().__init__()
        self.fence_depth = 0
        self.parser_state = None


class EditorHighlighter(QSyntaxHighlighter):
    """Syntax highlighter for Chat code blocks."""

    # Signal emitted when code block state changes
    codeBlockStateChanged = Signal(bool)

    def __init__(self, parent: Optional[QTextDocument] = None) -> None:
        """Initialize the highlighter."""
        super().__init__(parent)

        # Consistent font family fallback sequence for all code formats
        self._code_font_families = ["Menlo", "Monaco", "Courier New", "monospace"]
        self._has_code_block = False

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

            language = ProgrammingLanguage.UNKNOWN
            contination_state = -1
            current_fence_depth = 0
            current_block_data: EditorHighlighterBlockData = current_block.userData()
            if current_block_data:
                current_fence_depth = current_block_data.fence_depth
                current_parser_data: ChatParserState = current_block_data.parser_state
                if current_parser_data:
                    language = current_parser_data.language
                    contination_state = current_parser_data.continuation_state

            parser = ChatParser()
            parser_state: ChatParserState = parser.parse(prev_parser_state, text)

            in_code_block = False

            while True:
                token = parser.get_next_token()
                if token is None:
                    break

                match token.type:
                    case 'FENCE_START':
                        self.setFormat(0, len(text), self._fence_format)
                        fence_depth += 1
                        continue

                    case 'FENCE_END':
                        self.setFormat(0, len(text), self._fence_format)
                        fence_depth -= 1
                        continue

                    case 'BACKTICK':
                        if fence_depth == 0:
                            in_code_block = not in_code_block
                            continue

                if fence_depth > 0:
                    self.setFormat(token.start, len(token.value), self._style_manager.get_highlight(token.type))
                    continue

                if in_code_block:
                    self.setFormat(token.start, len(token.value), self._code_format)

            # Check if we need to rehighlight everything from this block onwards.
            if (contination_state != parser_state.continuation_state) or (current_fence_depth != fence_depth) or (language != parser_state.language):
                # It doesn't matter what we set this to, it just needs to be different to what it was before
                self.setCurrentBlockState(self.currentBlockState() + 1)

            block_data = EditorHighlighterBlockData()
            block_data.parser_state = parser_state
            block_data.fence_depth = fence_depth
            current_block.setUserData(block_data)

            # Check if document contains any code blocks
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
