"""Widget for displaying parts of individual chat messages."""

import logging

from PySide6.QtWidgets import (
    QFrame, QTextEdit, QSizePolicy
)
from PySide6.QtCore import Qt, QSize, QTimer, Signal, Slot
from PySide6.QtGui import (
    QTextOption, QTextCursor, QMouseEvent, QKeyEvent
)

from humbug.gui.style_manager import StyleManager


class ChatTextEdit(QTextEdit):
    """QTextEdit that automatically adjusts its height to content."""

    mouseReleased = Signal(QMouseEvent)

    def __init__(self, parent=None):
        super().__init__(parent)
        self.document().documentLayout().documentSizeChanged.connect(self._on_content_changed)
        self.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        self.setFrameStyle(QFrame.NoFrame)

        # Force the widget to always use the width of its container
        self.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Minimum)

        # Set word wrap mode to adjust to widget width
        self.setWordWrapMode(QTextOption.WrapAtWordBoundaryOrAnywhere)

        # Calculate tab stops
        self._style_manager = StyleManager()
        self._style_manager.style_changed.connect(self._handle_style_changed)
        self._handle_style_changed(self._style_manager.zoom_factor)

        # Batch update handling
        self._update_timer = QTimer(self)
        self._update_timer.setSingleShot(True)
        self._update_timer.setInterval(16)
        self._update_timer.timeout.connect(self._process_delayed_update)
        self._pending_update = False

        # Track current content length for incremental updates
        self._current_length = 0

        # Track code block state
        self._has_code_block = False

        self._logger = logging.getLogger("ChatTextEdit")

    def _handle_style_changed(self, _factor: float) -> None:
        self.setTabStopDistance(self._style_manager.get_space_width() * 8)

    def mouseReleaseEvent(self, event):
        """Propagate mouse release events to parent."""
        super().mouseReleaseEvent(event)
        self.mouseReleased.emit(event)

    def set_has_code_block(self, has_code: bool):
        """Update word wrap mode based on whether content contains code blocks."""
        if has_code == self._has_code_block:
            return

        self._has_code_block = has_code
        if has_code:
            self.setWordWrapMode(QTextOption.NoWrap)
            self.setHorizontalScrollBarPolicy(Qt.ScrollBarAsNeeded)
        else:
            self.setWordWrapMode(QTextOption.WrapAtWordBoundaryOrAnywhere)
            self.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)

        # Force layout update
        self._on_content_changed()

    def has_code_block(self) -> bool:
        """Check if content contains code blocks."""
        return self._has_code_block

    def wheelEvent(self, event):
        """Handle wheel events for horizontal scrolling."""
        # If this is a code block, handle horizontal scrolling for compatible mice
        if self._has_code_block and event.angleDelta().x() != 0:
            # Get the horizontal scrollbar
            hbar = self.horizontalScrollBar()
            if hbar:
                # Use the horizontal component directly
                delta = event.angleDelta().x()
                hbar.setValue(hbar.value() - delta)
                event.accept()
                return

        # For all other cases, propagate the event up
        event.ignore()

    def keyPressEvent(self, event: QKeyEvent):
        """Handle special key events."""
        if event.key() == Qt.Key_Home:
            cursor = self.textCursor()
            cursor.movePosition(QTextCursor.StartOfLine)
            self.setTextCursor(cursor)
            event.accept()
            return

        if event.key() == Qt.Key_End:
            cursor = self.textCursor()
            cursor.movePosition(QTextCursor.EndOfLine)
            self.setTextCursor(cursor)
            event.accept()
            return

        super().keyPressEvent(event)

    @Slot()
    def _on_content_changed(self):
        """Queue a content update instead of processing immediately."""
        if not self._pending_update:
            self._pending_update = True
            self._update_timer.start()

    @Slot()
    def _process_delayed_update(self):
        """Process the queued size update."""
        self._pending_update = False
        self.updateGeometry()

        # Ensure parent MessageWidget updates as well
        if self.parent():
            self.parent().updateGeometry()

    def set_incremental_text(self, text: str):
        """Update text content incrementally by only adding new content."""
        if len(text) == self._current_length:
            # No new content
            return

        if len(text) < self._current_length:
            # Content is shorter than what we have - do a full reset
            self._logger.warning("text is shorter than before!: '%s'", text)
            self.clear()
            self._current_length = 0
            return

        # Only insert the new content
        cursor = self.textCursor()
        cursor.movePosition(QTextCursor.End)
        new_text = text[self._current_length:]
        cursor.insertText(new_text)
        self._current_length = len(text)

    def clear(self):
        """Override clear to reset current length."""
        super().clear()
        self._current_length = 0
        self._on_content_changed()

    def _height(self) -> int:
        height = int(self.document().size().height())
        if self._has_code_block and self.horizontalScrollBar().isVisible():
            # Additional space for scrollbar with gap
            height += 14

        return height

    def minimumSizeHint(self) -> QSize:
        """Calculate minimum size based on content."""
        width = super().minimumSizeHint().width()
        return QSize(width, self._height())

    def sizeHint(self) -> QSize:
        """Calculate idea size based on content."""
        width = super().sizeHint().width()
        return QSize(width, self._height())
