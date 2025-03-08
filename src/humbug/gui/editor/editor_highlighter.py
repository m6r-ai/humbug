"""Editor highlighter."""

import logging
from typing import Optional

from PySide6.QtGui import (
    QSyntaxHighlighter, QTextDocument, QTextBlockUserData
)
from humbug.gui.style_manager import StyleManager
from humbug.syntax.programming_language import ProgrammingLanguage
from humbug.syntax.parser_registry import ParserRegistry


class EditorHighlighterBlockData(QTextBlockUserData):
    """Data associated with each text block."""
    def __init__(self):
        super().__init__()
        self.parser_state = None


class EditorHighlighter(QSyntaxHighlighter):
    """Syntax highlighter for source code files."""

    def __init__(self, parent: Optional[QTextDocument] = None) -> None:
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

            prev_block_data: EditorHighlighterBlockData = None
            prev_parser_state = None

            if prev_block:
                prev_block_data = prev_block.userData()
                if prev_block_data:
                    prev_parser_state = prev_block_data.parser_state

            continuation_state = -1
            current_block_data: EditorHighlighterBlockData = current_block.userData()

            # Use the appropriate language parser
            parser = ParserRegistry.create_parser(self._language)
            if not parser:
                return

            try:
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
                current_block.setUserData(block_data)
            finally:
                # Return the parser to the cache when done
                ParserRegistry.release_parser(self._language, parser)

        except Exception:
            self._logger.exception("highlighting exception")
