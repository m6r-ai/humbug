"""Widget for displaying code blocks with syntax highlighting."""

import logging
from typing import List

from PySide6.QtWidgets import QWidget
from PySide6.QtCore import Qt, Signal
from PySide6.QtGui import QTextOption, QMouseEvent, QKeyEvent, QPalette, QBrush

from syntax import ProgrammingLanguage, Token, ParserState

from humbug.min_height_plain_text_edit import MinHeightPlainTextEdit
from humbug.style_manager import StyleManager
from humbug.tabs.code_block_highlighter import CodeBlockHighlighter


class CodeBlockTextEdit(MinHeightPlainTextEdit):
    """Plain text edit widget optimized for displaying code blocks."""

    mouse_pressed = Signal(QMouseEvent)
    mouse_released = Signal(QMouseEvent)
    page_key_scroll_requested = Signal()

    def __init__(self, parent: QWidget | None = None) -> None:
        """
        Initialize the CodeBlockTextEdit widget.

        Args:
            parent: Parent widget
        """
        super().__init__(
            parent=parent,
            horizontal_scrollbar_policy=Qt.ScrollBarPolicy.ScrollBarAsNeeded,
            word_wrap_mode=QTextOption.WrapMode.NoWrap
        )

        self.setReadOnly(True)  # Code blocks are always read-only

        self._style_manager = StyleManager()
        self._init_colour_mode = self._style_manager.color_mode()

        self._logger = logging.getLogger("CodeBlockTextEdit")

        # Highlighted text should retain any underlying colours (e.g. syntax highlighting)
        palette = self.palette()
        palette.setBrush(QPalette.ColorRole.HighlightedText, QBrush(Qt.BrushStyle.NoBrush))
        self.setPalette(palette)

        # Syntax highlighting - created lazily
        self._highlighter: CodeBlockHighlighter = CodeBlockHighlighter(self.document())
        self._syntax: ProgrammingLanguage = ProgrammingLanguage.TEXT

        # Apply initial style
        self.apply_style()

    def set_syntax(self, syntax: ProgrammingLanguage) -> None:
        """
        Set the syntax highlighting.

        Args:
            syntax: The syntax to use
        """
        if self._syntax == syntax:
            return

        self._syntax = syntax

        # Update highlighter if it exists
        if self._highlighter is not None:
            self._highlighter.set_syntax(syntax)

    def set_text_with_highlighting(
        self,
        text: str,
        tokens_by_line: List[List[Token]],
        states_by_line: List[ParserState | None]
    ) -> None:
        """
        Set text with pre-computed syntax highlighting tokens to avoid re-parsing.

        This method sets the pre-computed tokens first, then sets the text, ensuring
        that highlighting only runs once using the pre-computed tokens.

        Args:
            text: The text content to display
            tokens_by_line: Pre-computed tokens for each line
            states_by_line: Pre-computed parser states for each line
        """
        # Set pre-computed tokens BEFORE setting text to avoid double highlighting
        if self._highlighter is not None:
            self._highlighter.set_precomputed_tokens(tokens_by_line, states_by_line)

        self.set_text(text)

    def apply_style(self) -> None:
        """Apply style changes."""
        font = self.font()
        font.setFamilies(self._style_manager.monospace_font_families())
        font.setFixedPitch(True)
        font.setPointSizeF(self._style_manager.base_font_size() * self._style_manager.zoom_factor())
        self.setFont(font)

        self.setTabStopDistance(self._style_manager.get_space_width() * 8)

        # If we changed colour mode then re-highlight
        if self._style_manager.color_mode() != self._init_colour_mode:
            self._init_colour_mode = self._style_manager.color_mode()
            if self._highlighter:
                self._highlighter.rehighlight()

    def mousePressEvent(self, e: QMouseEvent) -> None:
        """Propagate mouse press events to parent."""
        super().mousePressEvent(e)
        self.mouse_pressed.emit(e)

    def mouseReleaseEvent(self, e: QMouseEvent) -> None:
        """Propagate mouse release events to parent."""
        super().mouseReleaseEvent(e)
        self.mouse_released.emit(e)

    def keyPressEvent(self, e: QKeyEvent) -> None:
        """Handle special key events."""
        # Handle horizontal scrolling
        if e.key() in (Qt.Key.Key_Left, Qt.Key.Key_Right):
            hbar = self.horizontalScrollBar()
            if hbar and hbar.isVisible():
                current = hbar.value()
                step = 50  # Adjust scroll step size as needed
                if e.key() == Qt.Key.Key_Left:
                    hbar.setValue(max(hbar.minimum(), current - step))

                else:
                    hbar.setValue(min(hbar.maximum(), current + step))

                e.accept()
                return

        # For all other cases, propagate the event up to the parent
        e.ignore()
