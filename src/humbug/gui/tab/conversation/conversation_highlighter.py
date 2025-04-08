"""Conversation code highlighter."""

import logging
from typing import cast

from PySide6.QtGui import QSyntaxHighlighter, QTextDocument, QTextBlockUserData
from PySide6.QtCore import Signal

from humbug.gui.style_manager import StyleManager
from humbug.syntax.lexer import TokenType
from humbug.syntax.markdown.markdown_parser import MarkdownParser, MarkdownParserState
from humbug.syntax.programming_language import ProgrammingLanguage


class ConversationHighlighterBlockData(QTextBlockUserData):
    def __init__(self) -> None:
        super().__init__()
        self.seen_fence = False
        self.fence_depth = 0
        self.parser_state: MarkdownParserState | None = None


class ConversationHighlighter(QSyntaxHighlighter):
    """Syntax highlighter for Conversation code blocks."""

    # Signal emitted when code block state changes
    codeBlockStateChanged = Signal(bool)

    def __init__(self, parent: QTextDocument) -> None:
        """Initialize the highlighter."""
        super().__init__(parent)

        self._style_manager = StyleManager()
        self._has_code_block = False

        self._logger = logging.getLogger("ConversationHighlighter")

    def highlightBlock(self, text: str) -> None:
        """Apply highlighting to the given block of text."""
        try:
            current_block = self.currentBlock()
            prev_block = current_block.previous()

            prev_block_data: ConversationHighlighterBlockData | None = None
            prev_parser_state = None
            fence_depth = 0
            seen_fence = False
            if prev_block is not None:
                prev_block_data = cast(ConversationHighlighterBlockData, prev_block.userData())
                if prev_block_data is not None:
                    prev_parser_state = prev_block_data.parser_state
                    fence_depth = prev_block_data.fence_depth
                    seen_fence = prev_block_data.seen_fence

            language = ProgrammingLanguage.UNKNOWN
            contination_state = -1
            current_fence_depth = 0
            current_block_data = cast(ConversationHighlighterBlockData, current_block.userData())
            if current_block_data is not None:
                current_fence_depth = current_block_data.fence_depth
                current_parser_data = current_block_data.parser_state
                if current_parser_data is not None:
                    language = current_parser_data.language
                    contination_state = current_parser_data.continuation_state

            parser = MarkdownParser()
            parser_state: MarkdownParserState = parser.parse(prev_parser_state, text)

            in_code_block = False

            style_manager = self._style_manager
            last_token_pos = 0

            while True:
                token = parser.get_next_token()
                if token is None:
                    # If we've reached the end of the line check if we had whitespace at the end.  If we
                    # did then we need to highlight that too.
                    if fence_depth > 0 and last_token_pos < len(text):
                        self.setFormat(
                            last_token_pos,
                            len(text) - last_token_pos,
                            style_manager.get_highlight(TokenType.TEXT)
                        )

                    break

                match token.type:
                    case TokenType.FENCE_START:
                        self.setFormat(0, len(text), style_manager.get_highlight(TokenType.LANGUAGE))
                        fence_depth += 1
                        seen_fence = True
                        continue

                    case TokenType.FENCE_END:
                        self.setFormat(0, len(text), style_manager.get_highlight(TokenType.LANGUAGE))
                        fence_depth -= 1
                        continue

                    case TokenType.BACKTICK:
                        if fence_depth == 0:
                            in_code_block = not in_code_block
                            continue


                if fence_depth > 0:
                    highlight_len = len(token.value) + token.start - last_token_pos
                    self.setFormat(last_token_pos, highlight_len, style_manager.get_highlight(token.type))
                    last_token_pos += highlight_len
                    continue

                if in_code_block:
                    self.setFormat(token.start, len(token.value), style_manager.get_highlight(TokenType.INLINE_CODE))
                    continue

                self.setFormat(token.start, len(token.value), style_manager.get_proportional_highlight(token.type))

            # Check if we need to rehighlight everything from this block onwards.
            if (contination_state != parser_state.continuation_state) or (current_fence_depth != fence_depth) or (language != parser_state.language):
                # It doesn't matter what we set this to, it just needs to be different to what it was before
                self.setCurrentBlockState(self.currentBlockState() + 1)

            block_data = ConversationHighlighterBlockData()
            block_data.parser_state = parser_state
            block_data.fence_depth = fence_depth
            block_data.seen_fence = seen_fence
            current_block.setUserData(block_data)

            # Have we got to the end of the doc?  If yes, then emit the code block state.
            if not current_block.next().isValid():
                self.codeBlockStateChanged.emit(seen_fence)

        except Exception:
            self._logger.exception("highlighting exception")
