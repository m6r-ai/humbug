"""Widget for displaying code blocks with syntax highlighting."""

import logging

from PySide6.QtWidgets import QWidget
from PySide6.QtCore import Qt, Signal
from PySide6.QtGui import QTextOption, QMouseEvent, QKeyEvent, QPalette, QBrush, QWheelEvent

from syntax import ProgrammingLanguage

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
        self._highlighter: CodeBlockHighlighter | None = None
        self._language: ProgrammingLanguage = ProgrammingLanguage.TEXT

        # Apply initial style
        self.apply_style()

    def set_language(self, language: ProgrammingLanguage) -> None:
        """
        Set the programming language for syntax highlighting.

        Args:
            language: The programming language to use
        """
        if self._language == language:
            return

        self._language = language

        # Update highlighter if it exists
        if self._highlighter is not None:
            self._highlighter.set_language(language)

    def lazy_init_highlighter(self) -> None:
        """Initialize the syntax highlighter lazily when widget becomes visible."""
        if self._highlighter is None:
            self._highlighter = CodeBlockHighlighter(self.document())
            self._highlighter.set_language(self._language)

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

    def wheelEvent(self, e: QWheelEvent) -> None:
        """Handle wheel events for horizontal scrolling."""
        # Handle horizontal scrolling for compatible mice
        if e.angleDelta().x() != 0:
            # Get the horizontal scrollbar
            hbar = self.horizontalScrollBar()
            if hbar:
                # Use the horizontal component directly
                delta = e.angleDelta().x()
                hbar.setValue(hbar.value() - delta)

                # We've only handled the horizontal component - we need to let our parent
                # handle the vertical component.
                e.ignore()
                return

        # For all other cases, propagate the event up
        e.ignore()

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

        e.ignore()
