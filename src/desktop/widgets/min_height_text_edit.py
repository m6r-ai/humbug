"""Widget for displaying messages with minimal height possible."""

from PySide6.QtWidgets import (
    QFrame, QTextEdit, QSizePolicy, QWidget
)
from PySide6.QtCore import Qt, QSize, Signal
from PySide6.QtGui import QTextOption, QTextCursor, QWheelEvent, QResizeEvent


class MinHeightTextEdit(QTextEdit):
    """QTextEdit that automatically adjusts its height."""

    size_hint_changed = Signal()
    text_width_changed = Signal()

    def __init__(
        self,
        parent: QWidget | None = None
    ) -> None:
        """
        Initialize the MinHeightTextEdit widget.

        Args:
            parent: Parent widget
            horizontal_scrollbar_policy: Policy for horizontal scrollbar
            word_wrap_mode: Word wrap mode for text
        """
        super().__init__(parent)
        document = self.document()
        document.documentLayout().documentSizeChanged.connect(self._on_content_resized)
        document.setDocumentMargin(0)

        self.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.setFrameStyle(QFrame.Shape.NoFrame)

        # Force the widget to always use the width of its container
        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)

        # Set word wrap mode
        self.setWordWrapMode(QTextOption.WrapMode.WrapAtWordBoundaryOrAnywhere)

        self._current_text = ""
        self._allow_vertical_scroll = False
        self._height_cap: int | None = None

    def resizeEvent(self, event: QResizeEvent) -> None:
        old_width = self.document().textWidth()
        self.document().setTextWidth(self.viewport().width())
        if self.document().textWidth() != old_width:
            self.text_width_changed.emit()

        self._on_content_resized()
        super().resizeEvent(event)

    def _on_content_resized(self) -> None:
        """Handle resizing this widget based on the document content."""
        new_height = self._size_hint_height()
        if self._height_cap is None:
            self.setFixedHeight(new_height)

        else:
            self.setFixedHeight(self._height_cap)

        self.updateGeometry()
        self.size_hint_changed.emit()

    def prime_document_width(self, width: int) -> None:
        """
        Prime the document layout width before content is set.

        This prevents the bounce that occurs when a widget is added to a layout
        at its default size and then immediately resizes once content is set.

        Args:
            width: The expected display width in pixels
        """
        self.document().setTextWidth(width)

    def set_height_cap(self, cap: int | None) -> None:
        """Set a maximum height cap, or None to remove it."""
        self._height_cap = cap
        if cap is None:
            self.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
            self.setFixedHeight(self._size_hint_height())

        else:
            self.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAsNeeded)
            self.setFixedHeight(cap)

        self.updateGeometry()

    def set_allow_vertical_scroll(self, allow: bool) -> None:
        """Enable or disable vertical wheel scrolling when content exceeds widget height."""
        self._allow_vertical_scroll = allow

    def wheelEvent(self, e: QWheelEvent) -> None:
        """Handle wheel events for horizontal scrolling."""
        if self._allow_vertical_scroll and e.angleDelta().y() != 0:
            if self.verticalScrollBar().maximum() > 0:
                super().wheelEvent(e)
                return

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

    def clear(self) -> None:
        """Override clear to reset current text."""
        super().clear()
        self._current_text = ""

    def _size_hint_height(self) -> int:
        """Calculate the height of the widget including scrollbar if visible."""
        document_size = self.document().size()
        height = document_size.height()
        if self.horizontalScrollBar().isVisible():
            # Additional space for scrollbar with gap
            height += 14

        # Adjust for margins
        height += self.contentsMargins().top() + self.contentsMargins().bottom()

        return int(height + 0.99)

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
