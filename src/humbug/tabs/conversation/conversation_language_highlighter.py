"""Conversation language highlighter."""

import logging

from PySide6.QtGui import QSyntaxHighlighter, QTextDocument, QTextBlockUserData

from dmarkdown import MarkdownASTNode, MarkdownASTCodeBlockNode
from syntax import TokenType, ParserState

from humbug.style_manager import StyleManager


class ConversationLanguageHighlighterBlockData(QTextBlockUserData):
    """Data associated with each text block."""
    def __init__(self) -> None:
        super().__init__()
        self.parser_state: ParserState | None = None


class ConversationLanguageHighlighter(QSyntaxHighlighter):
    """Syntax highlighter for source code files."""

    def __init__(self, parent: QTextDocument, content_node: MarkdownASTNode | None) -> None:
        """Initialize the highlighter."""
        super().__init__(parent)

        self._content_node = content_node
        assert isinstance(self._content_node, MarkdownASTCodeBlockNode), "content node must be a MarkdownASTCodeBlockNode"

        # Consistent font family fallback sequence for all code formats
        self._style_manager = StyleManager()
        self._language = self._content_node.language
        self._logger = logging.getLogger("ConversationLanguageHighlighter")

    def highlightBlock(self, text: str) -> None:
        """Apply highlighting to the given block of text."""
        try:
            block_num = self.currentBlock().blockNumber()

            assert isinstance(self._content_node, MarkdownASTCodeBlockNode), "content node must be a MarkdownASTCodeBlockNode"
            tokens = self._content_node.tokens_by_line[block_num]

            # Apply syntax highlighting based on token types
            last_token_pos = 0
            for token in tokens:
                highlight_len = len(token.value) + token.start - last_token_pos
                self.setFormat(last_token_pos, highlight_len, self._style_manager.get_highlight(token.type))
                last_token_pos += highlight_len

            # If we've reached the end of the line check if we had whitespace at the end.  If we
            # did then we need to highlight that too.
            if last_token_pos < len(text):
                self.setFormat(
                    last_token_pos,
                    len(text) - last_token_pos,
                    self._style_manager.get_highlight(TokenType.TEXT)
                )

            continuation_state = -1
            if block_num > 0:
                # If the previous block has a continuation state, use it
                prev_parser_state = self._content_node.states_by_line[block_num - 1]
                if prev_parser_state is not None:
                    continuation_state = prev_parser_state.continuation_state

            parser_state = self._content_node.states_by_line[block_num]

            if parser_state is not None and continuation_state != parser_state.continuation_state:
                self.setCurrentBlockState(self.currentBlockState() + 1)

        except Exception:
            self._logger.exception("highlighting exception")

    def update_content(self, content_node: MarkdownASTNode | None) -> None:
        """Update the content node for the highlighter."""
        assert isinstance(content_node, MarkdownASTCodeBlockNode), "content node must be a MarkdownASTCodeBlockNode"
        self._content_node = content_node
