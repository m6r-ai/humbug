"""Widget for displaying log message text content."""

import logging
from typing import cast

from PySide6.QtWidgets import (
    QFrame, QTextEdit, QSizePolicy, QScrollArea, QWidget
)
from PySide6.QtCore import Qt, QSize, QTimer, Signal, QObject
from PySide6.QtGui import (
    QTextOption, QTextCursor, QMouseEvent, QKeyEvent, QPalette, QBrush, QWheelEvent
)

from humbug.gui.style_manager import StyleManager


class LogTextEdit(QTextEdit):
    """QTextEdit that automatically adjusts its height to content for log messages."""

    mousePressed = Signal(QMouseEvent)
    mouseReleased = Signal(QMouseEvent)

    def __init__(self, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self.document().documentLayout().documentSizeChanged.connect(self._on_content_changed)
        self.document().setDocumentMargin(0)
        self.setAcceptRichText(False)
        self.setReadOnly(True)  # Log messages are always read-only
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAsNeeded)
        self.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.setFrameStyle(QFrame.Shape.NoFrame)

        # Force the widget to always use the width of its container
        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Minimum)

        self.setWordWrapMode(QTextOption.WrapMode.NoWrap)

        self._style_manager = StyleManager()
        self._init_colour_mode = self._style_manager.color_mode()

        # Calculate tab stops
        self._style_manager.style_changed.connect(self._handle_style_changed)
        self._handle_style_changed()

        # Batch update handling
        self._update_timer = QTimer(self)
        self._update_timer.setSingleShot(True)
        self._update_timer.setInterval(16)
        self._update_timer.timeout.connect(self._process_delayed_update)
        self._pending_update = False

        # Track current content length for incremental updates
        self._current_length = 0

        self._logger = logging.getLogger("LogTextEdit")

        # Highlighted text should retain any underlying colours (e.g. syntax highlighting)
        palette = self.palette()
        palette.setBrush(QPalette.ColorRole.HighlightedText, QBrush(Qt.BrushStyle.NoBrush))
        self.setPalette(palette)

    def _handle_style_changed(self) -> None:
        font = self.font()
        font.setFamilies(self._style_manager.monospace_font_families())
        font.setFixedPitch(True)
        font.setPointSizeF(self._style_manager.base_font_size() * self._style_manager.zoom_factor())
        self.setFont(font)

        self.setTabStopDistance(self._style_manager.get_space_width() * 8)

    def mousePressEvent(self, event: QMouseEvent) -> None:
        """Propagate mouse press events to parent."""
        super().mousePressEvent(event)
        self.mousePressed.emit(event)

    def mouseReleaseEvent(self, event: QMouseEvent) -> None:
        """Propagate mouse release events to parent."""
        super().mouseReleaseEvent(event)
        self.mouseReleased.emit(event)

    def wheelEvent(self, event: QWheelEvent) -> None:
        """Handle wheel events for horizontal scrolling."""
        # Handle horizontal scrolling for compatible mice
        if event.angleDelta().x() != 0:
            # Get the horizontal scrollbar
            hbar = self.horizontalScrollBar()
            if hbar:
                # Use the horizontal component directly
                delta = event.angleDelta().x()
                hbar.setValue(hbar.value() - delta)

                # We've only handled the horizontal component - we need to let our parent
                # handle the vertical component.
                event.ignore()
                return

        # For all other cases, propagate the event up
        event.ignore()

    def keyPressEvent(self, event: QKeyEvent) -> None:
        """Handle special key events."""
        # Since this is read-only, we handle navigation keys differently
        # Let parent handle terminal navigation keys
        if event.key() in (
            Qt.Key.Key_Up, Qt.Key.Key_Down, Qt.Key.Key_PageUp, Qt.Key.Key_PageDown,
            Qt.Key.Key_Return
        ):
            event.ignore()
            return

        # Handle horizontal scrolling
        if event.key() in (Qt.Key.Key_Left, Qt.Key.Key_Right):
            hbar = self.horizontalScrollBar()
            if hbar and hbar.isVisible():
                current = hbar.value()
                step = 50  # Adjust scroll step size as needed
                if event.key() == Qt.Key.Key_Left:
                    hbar.setValue(max(hbar.minimum(), current - step))
                else:
                    hbar.setValue(min(hbar.maximum(), current + step))

                event.accept()
                return

        # For read-only widgets, ignore most other key events
        event.ignore()

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

        self.setText(text)
        self._current_length = len(text)

    def clear(self) -> None:
        """Override clear to reset current length."""
        super().clear()
        self._current_length = 0
        self._on_content_changed()

    def _height(self) -> int:
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
        """Calculate idea size based on content."""
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
