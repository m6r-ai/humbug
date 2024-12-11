"""Widget for displaying parts of individual chat messages."""

from PySide6.QtWidgets import (
    QFrame, QTextEdit, QSizePolicy
)
from PySide6.QtCore import Qt, QSize
from PySide6.QtGui import QTextOption


class DynamicTextEdit(QTextEdit):
    """QTextEdit that automatically adjusts its height to content."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self.document().documentLayout().documentSizeChanged.connect(self._on_content_changed)
        self.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        self.setFrameStyle(QFrame.NoFrame)

        # Force the widget to always use the width of its container
        self.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Fixed)

        # Set word wrap mode to adjust to widget width
        self.setWordWrapMode(QTextOption.WrapAtWordBoundaryOrAnywhere)

    def wheelEvent(self, event):
        """Explicitly ignore wheel events to let them propagate up."""
        event.ignore()

    def _on_content_changed(self):
        """Update the widget size when content changes."""
        self.updateGeometry()
        # Ensure parent MessageWidget updates as well
        if self.parent():
            self.parent().updateGeometry()

    def resizeEvent(self, event):
        """Handle resize events."""
        super().resizeEvent(event)

        # Force document width to match widget width
        self.document().setTextWidth(self.viewport().width())
        self._on_content_changed()

    def minimumSizeHint(self) -> QSize:
        """Calculate minimum size based on content."""
        # Get the document height when wrapped to current width
        self.document().setTextWidth(self.viewport().width())
        height = int(self.document().size().height()) + 16

        # Use parent width for width calculation
        width = self.viewport().width()
        return QSize(width, height)

    def sizeHint(self) -> QSize:
        """Size hint is same as minimum size hint."""
        return self.minimumSizeHint()
