"""Widget for displaying messages with minimal height possible."""

from typing import cast

from PySide6.QtWidgets import (
    QFrame, QTextEdit, QSizePolicy, QWidget
)
from PySide6.QtCore import Qt, QSize, QTimer
from PySide6.QtGui import QTextOption


class MinHeightTextEdit(QTextEdit):
    """QTextEdit that automatically adjusts its height."""

    def __init__(
        self,
        parent: QWidget | None = None,
        horizontal_scrollbar_policy: Qt.ScrollBarPolicy = Qt.ScrollBarPolicy.ScrollBarAlwaysOff,
        word_wrap_mode: QTextOption.WrapMode = QTextOption.WrapMode.WrapAtWordBoundaryOrAnywhere
    ) -> None:
        """
        Initialize the MinHeightTextEdit widget.

        Args:
            parent: Parent widget
            horizontal_scrollbar_policy: Policy for horizontal scrollbar
            word_wrap_mode: Word wrap mode for text
        """
        super().__init__(parent)
        self.document().documentLayout().documentSizeChanged.connect(self._on_content_changed)
        self.document().setDocumentMargin(0)
        self.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.setHorizontalScrollBarPolicy(horizontal_scrollbar_policy)
        self.setFrameStyle(QFrame.Shape.NoFrame)

        # Force the widget to always use the width of its container
        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Minimum)

        # Set word wrap mode
        self.setWordWrapMode(word_wrap_mode)

        # Batch update handling
        self._update_timer = QTimer(self)
        self._update_timer.setSingleShot(True)
        self._update_timer.setInterval(16)
        self._update_timer.timeout.connect(self._process_delayed_update)
        self._pending_update = False

        # Track current content length for incremental updates
        self._current_length = 0

    def _on_content_changed(self) -> None:
        """Queue a content update instead of processing immediately."""
        if not self._pending_update:
            self._pending_update = True
            self._update_timer.start()

    def _process_delayed_update(self) -> None:
        """Process the queued size update."""
        self._pending_update = False
        self.updateGeometry()

        # Ensure parent updates as well
        if self.parent():
            cast(QWidget, self.parent()).updateGeometry()

    def set_text(self, text: str) -> None:
        """Update text content if we have anything new."""
        if len(text) == self._current_length:
            # No new content
            return

        self.setPlainText(text)
        self._current_length = len(text)

    def clear(self) -> None:
        """Override clear to reset current length."""
        super().clear()
        self._current_length = 0
        self._on_content_changed()

    def _height(self) -> int:
        """Calculate the height of the widget including scrollbar if visible."""
        height = int(self.document().size().height())
        if self.horizontalScrollBar().isVisible():
            # Additional space for scrollbar with gap
            height += 14

        return height

    def minimumSizeHint(self) -> QSize:
        """Calculate minimum size based on content."""
        width = super().minimumSizeHint().width()
        return QSize(width, self._height())

    def sizeHint(self) -> QSize:
        """Calculate ideal size based on content."""
        width = super().sizeHint().width()
        return QSize(width, self._height())

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
