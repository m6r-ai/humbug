"""Widget for displaying plain text with minimal height possible."""

import logging
from PySide6.QtCore import Qt, QSize
from PySide6.QtGui import QTextOption, QTextCursor, QFontMetricsF
from PySide6.QtWidgets import (
    QFrame, QPlainTextEdit, QSizePolicy, QWidget
)


class MinHeightPlainTextEdit(QPlainTextEdit):
    """QPlainTextEdit that automatically adjusts its height."""

    def __init__(
        self,
        parent: QWidget | None = None,
        horizontal_scrollbar_policy: Qt.ScrollBarPolicy = Qt.ScrollBarPolicy.ScrollBarAlwaysOff,
        word_wrap_mode: QTextOption.WrapMode = QTextOption.WrapMode.WrapAtWordBoundaryOrAnywhere
    ) -> None:
        """
        Initialize the MinHeightPlainTextEdit widget.

        Args:
            parent: Parent widget
            horizontal_scrollbar_policy: Policy for horizontal scrollbar
            word_wrap_mode: Word wrap mode for text
        """
        super().__init__(parent)
        self._logger = logging.getLogger("MinHeightPlainTextEdit")

        self.document().documentLayout().documentSizeChanged.connect(self._on_content_resized)
        self.document().setDocumentMargin(0)
        self.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.setHorizontalScrollBarPolicy(horizontal_scrollbar_policy)
        self.horizontalScrollBar().rangeChanged.connect(self.scroll_changed)

        self.setFrameStyle(QFrame.Shape.NoFrame)

        # Force the widget to always use the width of its container
        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Minimum)

        # Set word wrap mode
        self.setWordWrapMode(word_wrap_mode)

        self._current_text = ""
        self.clear()

    def _on_content_resized(self) -> None:
        """Handle resizing this widget based on the document content."""
        self.updateGeometry()

    def scroll_changed(self, _min: int, _max: int) -> None:
        """Handle scrollbar range changes."""
        self.updateGeometry()

    def set_text(self, text: str) -> None:
        """Update text content incrementally based on differences."""
        old_text = self._current_text

        # If text is identical, do nothing
        if text == old_text:
            return

        # If old text is empty, just set it
        if not old_text:
            self.setPlainText(text)
            self._current_text = text
            self.updateGeometry()
            return

        # Find common prefix
        prefix_len = 0
        min_len = min(len(old_text), len(text))
        while prefix_len < min_len and old_text[prefix_len] == text[prefix_len]:
            prefix_len += 1

        # Find common suffix (but don't overlap with prefix)
        suffix_len = 0
        old_len = len(old_text)
        new_len = len(text)
        while (suffix_len < min_len - prefix_len and old_text[old_len - 1 - suffix_len] == text[new_len - 1 - suffix_len]):
            suffix_len += 1

        # Calculate the region that needs to be replaced
        # old_text[prefix_len : old_len - suffix_len] -> text[prefix_len : new_len - suffix_len]
        old_end = old_len - suffix_len
        new_end = new_len - suffix_len

        # Use cursor to perform incremental update
        cursor = self.textCursor()
        cursor.beginEditBlock()

        # Position cursor at the start of the changed region
        cursor.setPosition(prefix_len)

        # Select the old text that needs to be replaced
        cursor.setPosition(old_end, QTextCursor.MoveMode.KeepAnchor)

        # Replace with new text
        new_middle = text[prefix_len:new_end]
        cursor.insertText(new_middle)

        cursor.endEditBlock()

        # Update our cached text
        self._current_text = text
        self.updateGeometry()

    def clear(self) -> None:
        """Override clear to reset current text."""
        super().clear()
        self._current_text = ""

    def _size_hint_height(self) -> int:
        """Calculate the height of the widget including scrollbar if visible."""
        # This is a bit of a bizarre workaround.  When text wraps around there is a short window where our
        # height is actually too small to show all the text and despite us saying not to use a vertical scrollbar,
        # Qt will adjust it anyway!  We reset it back so things render correctly.
        self.verticalScrollBar().setValue(0)

        document_size = self.document().size()
        line_count = int(document_size.height())

        # Calculate line height from font metrics.  QPlainTextEdit does some slightly odd rounding so we need to match it.
        font_metrics = QFontMetricsF(self.font())
        line_height = int(font_metrics.height() + 0.99)

        height = line_count * line_height
        if self.horizontalScrollBar().isVisible():
            # Additional space for scrollbar with gap
            height += 14

        return height

    def minimumSizeHint(self) -> QSize:
        """Calculate minimum size based on content."""
        width = super().minimumSizeHint().width()
        height = self._size_hint_height()
        return QSize(width, height)

    def sizeHint(self) -> QSize:
        """Calculate ideal size based on content."""
        width = super().sizeHint().width()
        height = self._size_hint_height()
        return QSize(width, height)

    def find_text(self, text: str) -> bool:
        """
        Find text in the widget.

        Args:
            text: Text to search for

        Returns:
            True if text was found
        """
        # Clear any existing selection
        cursor = self.textCursor()
        cursor.clearSelection()
        self.setTextCursor(cursor)

        # Find the text
        found = self.find(text)
        if found:
            # Ensure found text is visible
            self.ensureCursorVisible()

        return found
