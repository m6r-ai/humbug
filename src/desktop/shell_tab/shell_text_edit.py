"""Widget for displaying parts of individual system messages."""

import logging

from PySide6.QtWidgets import QWidget
from PySide6.QtCore import Qt, Signal
from PySide6.QtGui import (
    QKeyEvent, QTextOption, QMouseEvent, QPalette, QBrush
)

from desktop.widgets import MinHeightPlainTextEdit
from desktop.style_manager import StyleManager
from desktop.shell_tab.shell_command_highlighter import ShellCommandHighlighter


class ShellTextEdit(MinHeightPlainTextEdit):
    """Text edit widget that handles shell commands and responses."""

    mouse_pressed = Signal(QMouseEvent)
    mouse_released = Signal(QMouseEvent)
    page_key_scroll_requested = Signal()

    def __init__(self, is_input: bool, parent: QWidget | None = None) -> None:
        """
        Initialize the ShellTextEdit widget.

        Args:
            is_input: True if this is an input widget, False for output
            parent: Parent widget
        """
        super().__init__(
            parent=parent,
            horizontal_scrollbar_policy=(
                Qt.ScrollBarPolicy.ScrollBarAlwaysOff if is_input
                else Qt.ScrollBarPolicy.ScrollBarAsNeeded
            ),
            word_wrap_mode=(
                QTextOption.WrapMode.WrapAnywhere if is_input
                else QTextOption.WrapMode.NoWrap
            )
        )

        self._is_input = is_input
        self.setReadOnly(not is_input)

        self._style_manager = StyleManager()
        self._last_highlights_version = self._style_manager.highlights_version()

        # Calculate tab stops
        self.apply_style()

        self._highlighter: ShellCommandHighlighter | None = None

        self._logger = logging.getLogger("ShellTextEdit")

        # Highlighted text should retain any underlying colours (e.g. syntax highlighting)
        palette = self.palette()
        palette.setBrush(QPalette.ColorRole.HighlightedText, QBrush(Qt.BrushStyle.NoBrush))
        self.setPalette(palette)

    def apply_style(self) -> None:
        """Apply style changes."""
        font = self._style_manager.make_monospace_font()
        self.setFont(font)

        self.setTabStopDistance(self._style_manager.get_space_width() * 8)

        # Rehighlight when highlight formats change (covers palette and mode switches)
        current_hv = self._style_manager.highlights_version()
        if current_hv != self._last_highlights_version:
            self._last_highlights_version = current_hv
            if self._highlighter:
                self._highlighter.rehighlight()

    def keyPressEvent(self, e: QKeyEvent) -> None:
        """Handle special key events."""
        # If we're an input widget, just propagate the event
        if self._is_input:
            super().keyPressEvent(e)
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

        # For all other cases, propagate the event up to the parent
        e.ignore()

    def mousePressEvent(self, e: QMouseEvent) -> None:
        """Propagate mouse press events to parent."""
        super().mousePressEvent(e)
        self.mouse_pressed.emit(e)

    def mouseReleaseEvent(self, e: QMouseEvent) -> None:
        """Propagate mouse release events to parent."""
        super().mouseReleaseEvent(e)
        self.mouse_released.emit(e)

    def enable_highlighter(self) -> None:
        """Enable syntax highlighting for system commands."""
        self._highlighter = ShellCommandHighlighter(self.document())
        self.setWordWrapMode(QTextOption.WrapMode.WrapAnywhere)
