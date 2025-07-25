"""Editor highlighter."""

import logging
from typing import cast

from PySide6.QtGui import QSyntaxHighlighter, QTextDocument, QTextBlockUserData

from syntax import ProgrammingLanguage, ParserState, ParserRegistry

from humbug.style_manager import StyleManager


class EditorHighlighterBlockData(QTextBlockUserData):
    """Data associated with each text block."""
    def __init__(self) -> None:
        super().__init__()
        self.parser_state: ParserState | None = None


class EditorHighlighter(QSyntaxHighlighter):
    """Syntax highlighter for source code files."""

    def __init__(self, parent: QTextDocument) -> None:
        """Initialize the highlighter."""
        super().__init__(parent)

        # Consistent font family fallback sequence for all code formats
        self._style_manager = StyleManager()
        self._language = ProgrammingLanguage.TEXT
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

            prev_block_data: EditorHighlighterBlockData | None = None
            prev_parser_state = None

            if prev_block:
                prev_block_data = cast(EditorHighlighterBlockData, prev_block.userData())
                if prev_block_data:
                    prev_parser_state = prev_block_data.parser_state

            continuation_state = -1
            current_block_data = cast(EditorHighlighterBlockData, current_block.userData())

            # Use the appropriate language parser
            parser = ParserRegistry.create_parser(self._language)
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
                current_parser_state = cast(ParserState, current_block_data.parser_state)
                if current_parser_state:
                    continuation_state = current_parser_state.continuation_state

            if parser_state is not None:
                if continuation_state != parser_state.continuation_state:
                    self.setCurrentBlockState(self.currentBlockState() + 1)

            block_data = EditorHighlighterBlockData()
            block_data.parser_state = parser_state
            current_block.setUserData(block_data)

        except Exception:
            self._logger.exception("highlighting exception")
