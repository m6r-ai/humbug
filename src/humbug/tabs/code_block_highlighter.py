"""Editor highlighter."""

import logging
from typing import cast, List

from PySide6.QtGui import QSyntaxHighlighter, QTextDocument, QTextBlockUserData

from syntax import TokenType, ProgrammingLanguage, ParserState, ParserRegistry, Token

from humbug.style_manager import StyleManager


class CodeBlockHighlighterBlockData(QTextBlockUserData):
    """Data associated with each text block."""
    def __init__(self) -> None:
        super().__init__()
        self.parser_state: ParserState | None = None


class CodeBlockHighlighter(QSyntaxHighlighter):
    """Syntax highlighter for source code files."""

    def __init__(self, parent: QTextDocument) -> None:
        """Initialize the highlighter."""
        super().__init__(parent)

        # Consistent font family fallback sequence for all code formats
        self._style_manager = StyleManager()
        self._syntax = ProgrammingLanguage.TEXT
        self._precomputed_tokens: List[List[Token]] | None = None
        self._precomputed_states: List[ParserState | None] | None = None
        self._use_precomputed = False
        self._logger = logging.getLogger("CodeBlockHighlighter")

    def set_syntax(self, syntax: ProgrammingLanguage) -> None:
        """
        Set the syntax highlighting.

        Args:
            syntax: The syntax to use
        """
        if self._syntax == syntax:
            return

        self._syntax = syntax

        # Clear pre-computed tokens when language changes
        self._precomputed_tokens = None
        self._precomputed_states = None
        self._use_precomputed = False

        # If our document is empty, no need to rehighlight
        if self.document().isEmpty():
            return

        self.rehighlight()

    def set_precomputed_tokens(
        self,
        tokens_by_line: List[List[Token]],
        states_by_line: List[ParserState | None]
    ) -> None:
        """
        Set pre-computed tokens to avoid re-parsing.

        Args:
            tokens_by_line: Pre-computed tokens for each line
            states_by_line: Pre-computed parser states for each line
        """
        self._precomputed_tokens = tokens_by_line
        self._precomputed_states = states_by_line
        self._use_precomputed = True

    def highlightBlock(self, text: str) -> None:
        """Apply highlighting to the given block of text."""
        token: Token | None

        try:
            current_block = self.currentBlock()
            prev_block = current_block.previous()

            prev_block_data: CodeBlockHighlighterBlockData | None = None
            prev_parser_state = None

            if prev_block:
                prev_block_data = cast(CodeBlockHighlighterBlockData, prev_block.userData())
                if prev_block_data:
                    prev_parser_state = prev_block_data.parser_state

            # Use pre-computed tokens if available
            if self._use_precomputed and self._precomputed_tokens and self._precomputed_states:
                block_number = current_block.blockNumber()

                if block_number < len(self._precomputed_tokens):
                    # Use pre-computed tokens for this line
                    tokens = self._precomputed_tokens[block_number]
                    parser_state = self._precomputed_states[block_number]

                    # Apply syntax highlighting from pre-computed tokens
                    last_token_pos = 0
                    for token in tokens:
                        highlight_len = len(token.value) + token.start - last_token_pos
                        self.setFormat(last_token_pos, highlight_len, self._style_manager.get_highlight(token.type))
                        last_token_pos += highlight_len

                    # Highlight any remaining whitespace
                    if last_token_pos < len(text):
                        self.setFormat(
                            last_token_pos,
                            len(text) - last_token_pos,
                            self._style_manager.get_highlight(TokenType.TEXT)
                        )

                    # Store parser state for next block
                    block_data = CodeBlockHighlighterBlockData()
                    block_data.parser_state = parser_state
                    current_block.setUserData(block_data)
                    return

            continuation_state = -1
            current_block_data = cast(CodeBlockHighlighterBlockData, current_block.userData())

            # Use the appropriate language parser
            parser = ParserRegistry.create_parser(self._syntax)
            if not parser:
                return

            parser_state = parser.parse(prev_parser_state, text)

            # Apply syntax highlighting based on token types
            last_token_pos = 0
            while True:
                token = parser.get_next_token()
                if token is None:
                    # If we've reached the end of the line check if we had whitespace at the end.  If we
                    # did then we need to highlight that too.
                    if last_token_pos < len(text):
                        self.setFormat(
                            last_token_pos,
                            len(text) - last_token_pos,
                            self._style_manager.get_highlight(TokenType.TEXT)
                        )
                    break

                highlight_len = len(token.value) + token.start - last_token_pos
                self.setFormat(last_token_pos, highlight_len, self._style_manager.get_highlight(token.type))
                last_token_pos += highlight_len

            # Check if we need to rehighlight everything from this block onwards
            if current_block_data is not None:
                current_parser_state = current_block_data.parser_state
                if current_parser_state is not None:
                    continuation_state = current_parser_state.continuation_state

            if parser_state is not None and continuation_state != parser_state.continuation_state:
                self.setCurrentBlockState(self.currentBlockState() + 1)

            block_data = CodeBlockHighlighterBlockData()
            block_data.parser_state = parser_state
            current_block.setUserData(block_data)

        except Exception:
            self._logger.exception("highlighting exception")
