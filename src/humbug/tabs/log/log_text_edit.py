"""Widget for displaying log message text content."""

import logging

from PySide6.QtWidgets import QWidget
from PySide6.QtCore import Qt, Signal
from PySide6.QtGui import QTextOption, QMouseEvent, QKeyEvent, QPalette, QBrush, QWheelEvent

from humbug.min_height_text_edit import MinHeightTextEdit
from humbug.style_manager import StyleManager


class LogTextEdit(MinHeightTextEdit):
    """Text edit widget used for log messages."""

    mouse_pressed = Signal(QMouseEvent)
    mouse_released = Signal(QMouseEvent)

    def __init__(self, parent: QWidget | None = None) -> None:
        """
        Initialize the LogTextEdit widget.

        Args:
            parent: Parent widget
        """
        super().__init__(
            parent=parent,
            horizontal_scrollbar_policy=Qt.ScrollBarPolicy.ScrollBarAsNeeded,
            word_wrap_mode=QTextOption.WrapMode.NoWrap
        )

        self.setAcceptRichText(False)
        self.setReadOnly(True)  # Log messages are always read-only

        self._style_manager = StyleManager()
        self._init_colour_mode = self._style_manager.color_mode()

        # Calculate tab stops
        self.apply_style()

        self._logger = logging.getLogger("LogTextEdit")

        # Highlighted text should retain any underlying colours (e.g. syntax highlighting)
        palette = self.palette()
        palette.setBrush(QPalette.ColorRole.HighlightedText, QBrush(Qt.BrushStyle.NoBrush))
        self.setPalette(palette)

    def apply_style(self) -> None:
        """Apply style changes."""
        font = self.font()
        font.setFamilies(self._style_manager.monospace_font_families())
        font.setFixedPitch(True)
        font.setPointSizeF(self._style_manager.base_font_size() * self._style_manager.zoom_factor())
        self.setFont(font)

        self.setTabStopDistance(self._style_manager.get_space_width() * 8)

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
        # Since this is read-only, we handle navigation keys differently
        # Let parent handle terminal navigation keys
        if e.key() in (
            Qt.Key.Key_Up, Qt.Key.Key_Down, Qt.Key.Key_PageUp, Qt.Key.Key_PageDown,
            Qt.Key.Key_Return
        ):
            e.ignore()
            return

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

        # For read-only widgets, ignore most other key events
        e.ignore()
