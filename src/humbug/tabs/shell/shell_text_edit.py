"""Widget for displaying parts of individual system messages."""

import logging

from PySide6.QtWidgets import QScrollArea, QWidget
from PySide6.QtCore import Qt, Signal, QObject
from PySide6.QtGui import (
    QTextOption, QTextCursor, QMouseEvent, QKeyEvent, QPalette, QBrush, QWheelEvent
)

from humbug.min_height_text_edit import MinHeightTextEdit
from humbug.style_manager import StyleManager
from humbug.tabs.shell.shell_command_highlighter import ShellCommandHighlighter


class ShellTextEdit(MinHeightTextEdit):
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

        self.setAcceptRichText(False)
        self.setReadOnly(not is_input)

        self._style_manager = StyleManager()
        self._init_colour_mode = self._style_manager.color_mode()

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
        # Is this a read-only widget?  If it is then we don't want to process certain key events,
        # leaving it to the parent to handle them.
        if self.isReadOnly():
            # Let parent handle terminal navigation keys even in read-only mode
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

            return

        # For editable widgets, we need special handling for certain keys
        if e.key() == Qt.Key.Key_Return and not e.modifiers() & Qt.KeyboardModifier.ShiftModifier:
            # Let parent handle Enter key for command submission
            e.ignore()
            return

        if e.key() in (Qt.Key.Key_PageUp, Qt.Key.Key_PageDown):
            # Find the scroll area viewport by walking up hierarchy
            widget: QObject = self
            viewport = None
            while widget:
                widget = widget.parent()
                if isinstance(widget, QScrollArea):
                    viewport = widget.viewport()
                    break

            if viewport is not None:
                # Calculate visible lines based on cursor height
                cursor_rect = self.cursorRect()
                line_height = cursor_rect.height()
                visible_lines = max(1, viewport.height() // line_height)

                # Move cursor by calculated lines
                cursor = self.textCursor()
                orig_pos = cursor.position()

                movement = QTextCursor.MoveOperation.Up if e.key() == Qt.Key.Key_PageUp else QTextCursor.MoveOperation.Down
                cursor.movePosition(movement, QTextCursor.MoveMode.MoveAnchor, visible_lines)

                # Only set cursor if it actually moved
                if cursor.position() != orig_pos:
                    self.setTextCursor(cursor)
                    self.page_key_scroll_requested.emit()

            e.accept()
            return

        if e.key() == Qt.Key.Key_Home:
            cursor = self.textCursor()
            cursor.movePosition(QTextCursor.MoveOperation.StartOfLine)
            self.setTextCursor(cursor)
            e.accept()
            return

        if e.key() == Qt.Key.Key_End:
            cursor = self.textCursor()
            cursor.movePosition(QTextCursor.MoveOperation.EndOfLine)
            self.setTextCursor(cursor)
            e.accept()
            return

        super().keyPressEvent(e)

    def enable_highlighter(self) -> None:
        """Enable syntax highlighting for system commands."""
        self._highlighter = ShellCommandHighlighter(self.document())
        self.setWordWrapMode(QTextOption.WrapMode.WrapAnywhere)
