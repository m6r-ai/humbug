"""Widget for displaying messages with minimal height possible."""

from typing import cast

from PySide6.QtWidgets import (
    QFrame, QTextEdit, QSizePolicy, QWidget
)
from PySide6.QtCore import Qt, QSize, QTimer
from PySide6.QtGui import QTextOption


class MinHeightTextEdit(QTextEdit):
    """QTextEdit that automatically adjusts its height."""

    def __init__(self, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self.document().documentLayout().documentSizeChanged.connect(self._on_content_changed)
        self.document().setDocumentMargin(0)
        self.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.setFrameStyle(QFrame.Shape.NoFrame)

        # Force the widget to always use the width of its container
        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Minimum)

        # Set word wrap mode to adjust to widget width
        self.setWordWrapMode(QTextOption.WrapMode.WrapAtWordBoundaryOrAnywhere)

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

    def clear(self) -> None:
        """Override clear to reset current length."""
        super().clear()
        self._current_length = 0
        self._on_content_changed()

    def _height(self) -> int:
        return int(self.document().size().height())

    def minimumSizeHint(self) -> QSize:
        """Calculate minimum size based on content."""
        width = super().minimumSizeHint().width()
        return QSize(width, self._height())

    def sizeHint(self) -> QSize:
        """Calculate idea size based on content."""
        width = super().sizeHint().width()
        return QSize(width, self._height())
