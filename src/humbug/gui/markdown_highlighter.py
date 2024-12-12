"""Markdown code highlighter."""

from typing import Optional

from PySide6.QtGui import (
    QTextCharFormat, QSyntaxHighlighter, QColor,
    QTextCursor, QTextBlockFormat, QTextDocument
)

from humbug.gui.syntax_debug import HighlighterBase, SyntaxLogEvent


class MarkdownHighlighter(HighlighterBase, QSyntaxHighlighter):
    """Syntax highlighter for Markdown code blocks."""

    # Block states
    NORMAL_STATE = 0
    CODE_BLOCK_STATE = 1

    def __init__(self, parent: Optional[QTextDocument] = None) -> None:
        """Initialize the highlighter."""
        HighlighterBase.__init__(self, "markdown")
        QSyntaxHighlighter.__init__(self, parent)

        # Consistent font family fallback sequence for all code formats
        self.code_font_families = ["Menlo", "Monaco", "Courier New", "monospace"]

        # For inline code
        self.code_format = QTextCharFormat()
        self.code_format.setFontFamilies(self.code_font_families)
        self.code_format.setFontFixedPitch(True)

        # For code blocks and fences
        self.block_format = QTextCharFormat()
        self.block_format.setFontFamilies(self.code_font_families)
        self.block_format.setFontFixedPitch(True)
        self.block_format.setBackground(QColor("#2d2d2d"))

        # Create block format for full-width background
        self.code_block_format = QTextBlockFormat()
        self.code_block_format.setBackground(QColor("#2d2d2d"))
        self.normal_block_format = QTextBlockFormat()

    def _is_code_fence(self, text: str) -> bool:
        """Check if a line is a code fence.

        Args:
            text: Text to check

        Returns:
            True if the text is a code fence
        """
        stripped = text.lstrip()
        return stripped.startswith('```')

    def _get_inherited_state(self, block) -> int:
        """Get the state that should be inherited from previous blocks.

        Walks backwards through blocks until it finds a definitive state.

        Args:
            block: Current block

        Returns:
            State that should be inherited (0 or 1)
        """
        prev_block = block.previous()
        while prev_block.isValid():
            prev_state = prev_block.userState()
            prev_text = prev_block.text()

            # If we find a code fence, we're in the opposite state of what follows it
            if self._is_code_fence(prev_text):
                self._log_block_event(
                    SyntaxLogEvent.STATE_CHANGE,
                    block,
                    prev_state,
                    {"inherited": "fence found", "fence_text": prev_text}
                )
                return self.NORMAL_STATE if prev_state == self.CODE_BLOCK_STATE else self.CODE_BLOCK_STATE

            # If we have a valid state that's not from an empty block, use it
            if prev_state != -1 and prev_text.strip():
                self._log_block_event(
                    SyntaxLogEvent.STATE_CHANGE,
                    block,
                    prev_state,
                    {"inherited": "previous block", "prev_text": prev_text}
                )
                return prev_state

            prev_block = prev_block.previous()

        # If we couldn't find a definitive state, assume normal
        return self.NORMAL_STATE

    def highlightBlock(self, text: str) -> None:
        """Apply highlighting to the given block of text."""
        try:
            current_block = self.currentBlock()
            cursor = QTextCursor(current_block)

            # Get previous state, using inheritance if needed
            prev_state = self.previousBlockState()
            if prev_state == -1:
                prev_state = self._get_inherited_state(current_block)

            self._log_block_event(
                SyntaxLogEvent.BLOCK_START,
                current_block,
                prev_state,
                {"prev_state": prev_state, "text": text}
            )

            # Check if this line is a code fence
            is_fence = self._is_code_fence(text)
            if is_fence:
                self._log_block_event(
                    SyntaxLogEvent.STATE_CHANGE,
                    current_block,
                    prev_state,
                    {"is_fence": True, "text": text}
                )

            # Determine if we're in a code block
            in_code_block = prev_state == self.CODE_BLOCK_STATE
            if is_fence:
                # Toggle the code block state
                in_code_block = not in_code_block

            # Apply formatting
            if in_code_block or is_fence:
                # Apply code block formatting
                self.setFormat(0, len(text), self.block_format)
                cursor.setBlockFormat(self.code_block_format)
                self._log_block_event(
                    SyntaxLogEvent.FORMAT_APPLIED,
                    current_block,
                    self.CODE_BLOCK_STATE if in_code_block else self.NORMAL_STATE,
                    {"format": "code_block"}
                )
            else:
                # Apply normal formatting and look for inline code
                cursor.setBlockFormat(self.normal_block_format)
                i = 0
                while i < len(text):
                    if text[i] == '`':
                        start = i
                        i += 1
                        while i < len(text):
                            if text[i] == '`':
                                # Found the closing backtick
                                self.setFormat(start, i - start + 1, self.code_format)
                                self._log_block_event(
                                    SyntaxLogEvent.FORMAT_APPLIED,
                                    current_block,
                                    self.NORMAL_STATE,
                                    {"format": "inline_code", "start": start, "end": i + 1}
                                )
                                break
                            i += 1
                    i += 1

            # Set state for next block
            new_state = self.CODE_BLOCK_STATE if in_code_block else self.NORMAL_STATE
            self.setCurrentBlockState(new_state)

            if new_state != prev_state:
                self._log_block_event(
                    SyntaxLogEvent.STATE_CHANGE,
                    current_block,
                    new_state,
                    {"old_state": prev_state, "new_state": new_state}
                )

            self._log_block_event(
                SyntaxLogEvent.BLOCK_END,
                current_block,
                new_state
            )

        except Exception as e:
            self.debug.log_error("Error highlighting block",
                               self._get_block_position(current_block), e)
            raise
