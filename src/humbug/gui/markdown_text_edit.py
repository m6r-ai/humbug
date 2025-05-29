"""Widget for displaying parts of individual Markdown messages."""

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
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.mindspace.mindspace_settings import MindspaceSettings


class MarkdownTextEdit(QTextEdit):
    """QTextEdit that automatically adjusts its height to content to display Markdown."""

    mousePressed = Signal(QMouseEvent)
    mouseReleased = Signal(QMouseEvent)
    linkClicked = Signal(str)
    page_key_scroll_requested = Signal()

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

        # Calculate tab stops
        self._style_manager = StyleManager()
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

        # Track code block state
        self._has_code_block = False

        self._logger = logging.getLogger("MarkdownTextEdit")

        # Highlighted text should retain any underlying colours (e.g. syntax highlighting)
        palette = self.palette()
        palette.setBrush(QPalette.ColorRole.HighlightedText, QBrush(Qt.BrushStyle.NoBrush))
        self.setPalette(palette)

    def _handle_style_changed(self) -> None:
        self.setTabStopDistance(self._style_manager.get_space_width() * 8)
        self.document().setIndentWidth(self._style_manager.get_space_width() * 4)

    def mousePressEvent(self, e: QMouseEvent) -> None:
        """Propagate mouse press events to parent."""
        self.mousePressed.emit(e)

        # Check for link clicks
        anchor = self.anchorAt(e.pos())
        if anchor and e.button() == Qt.MouseButton.LeftButton:
            self.linkClicked.emit(anchor)
            e.accept()
            return

        # Default handling for other cases
        super().mousePressEvent(e)

    def mouseReleaseEvent(self, e: QMouseEvent) -> None:
        """Propagate mouse release events to parent."""
        super().mouseReleaseEvent(e)
        self.mouseReleased.emit(e)

    def set_has_code_block(self, has_code: bool) -> None:
        """Update word wrap mode based on whether content contains code blocks."""
        if has_code == self._has_code_block:
            return

        self._has_code_block = has_code
        if has_code:
            self.setWordWrapMode(QTextOption.WrapMode.NoWrap)
            self.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAsNeeded)

        else:
            self.setWordWrapMode(QTextOption.WrapMode.WrapAtWordBoundaryOrAnywhere)
            self.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)

        # Force layout update
        self._on_content_changed()

    def has_code_block(self) -> bool:
        """Check if content contains code blocks."""
        return self._has_code_block

    def wheelEvent(self, e: QWheelEvent) -> None:
        """Handle wheel events for horizontal scrolling."""
        # If this is a code block, handle horizontal scrolling for compatible mice
        if self._has_code_block and e.angleDelta().x() != 0:
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

    def _indent_single_line_soft_tabs(self, cursor: QTextCursor, tab_size: int) -> None:
        """Indent a single line using soft tabs (spaces).

        Args:
            cursor: The current text cursor
            tab_size: Number of spaces to use for indentation
        """
        current_column = cursor.position() - cursor.block().position()
        spaces_needed = tab_size - (current_column % tab_size)
        cursor.insertText(" " * spaces_needed)

    def _indent_single_line_hard_tabs(self, cursor: QTextCursor) -> None:
        """Indent a single line using hard tabs.

        Args:
            cursor: The current text cursor
        """
        cursor.insertText("\t")

    def _indent_block_soft_tabs(self, cursor: QTextCursor, tab_size: int) -> None:
        """Indent a block of text using soft tabs (spaces).

        Args:
            cursor: The current text cursor
            tab_size: Number of spaces to use for indentation
        """
        start = cursor.selectionStart()
        end = cursor.selectionEnd()
        reverse: bool = start == cursor.position()

        cursor.setPosition(start)
        cursor.movePosition(QTextCursor.MoveOperation.StartOfLine)

        # If selection ends at start of line, don't indent that line
        end_offs = 0
        check_cursor = QTextCursor(cursor)
        check_cursor.setPosition(end)
        if check_cursor.atBlockStart():
            end_offs = 1

        start += tab_size
        while cursor.position() <= end - end_offs:
            if not cursor.atBlockEnd():
                cursor.insertText(" " * tab_size)
                end += tab_size

            if not cursor.movePosition(QTextCursor.MoveOperation.NextBlock):
                break

        cursor.setPosition(start if not reverse else end)
        cursor.setPosition(end if not reverse else start, QTextCursor.MoveMode.KeepAnchor)

    def _indent_block_hard_tabs(self, cursor: QTextCursor) -> None:
        """Indent a block of text using hard tabs.

        Args:
            cursor: The current text cursor
        """
        start = cursor.selectionStart()
        end = cursor.selectionEnd()
        reverse: bool = start == cursor.position()

        cursor.setPosition(start)
        cursor.movePosition(QTextCursor.MoveOperation.StartOfLine)

        # If selection ends at start of line, don't indent that line
        end_offs = 0
        check_cursor = QTextCursor(cursor)
        check_cursor.setPosition(end)
        if check_cursor.atBlockStart():
            end_offs = 1

        start += 1
        while cursor.position() <= end - end_offs:
            if not cursor.atBlockEnd():
                cursor.insertText("\t")
                end += 1

            if not cursor.movePosition(QTextCursor.MoveOperation.NextBlock):
                break

        cursor.setPosition(start if not reverse else end)
        cursor.setPosition(end if not reverse else start, QTextCursor.MoveMode.KeepAnchor)

    def _outdent_single_line_soft_tabs(self, cursor: QTextCursor, tab_size: int) -> None:
        """Outdent a single line using soft tabs (spaces).

        Args:
            cursor: The current text cursor
            tab_size: Number of spaces to use for indentation
        """
        current_column = cursor.position() - cursor.block().position()
        deletes_needed = 1 + ((current_column - 1) % tab_size)
        deletes_needed = min(deletes_needed, current_column)

        while deletes_needed > 0:
            text = cursor.block().text()
            if not text or text[current_column - 1] != " ":
                break

            cursor.deletePreviousChar()
            current_column -= 1
            deletes_needed -= 1

    def _outdent_single_line_hard_tabs(self, cursor: QTextCursor) -> None:
        """Outdent a single line using hard tabs.

        Args:
            cursor: The current text cursor
        """
        current_column = cursor.position() - cursor.block().position()
        if current_column > 0:
            text = cursor.block().text()
            if text and text[current_column - 1] == "\t":
                cursor.deletePreviousChar()

    def _outdent_block_soft_tabs(self, cursor: QTextCursor, tab_size: int) -> None:
        """Outdent a block of text using soft tabs (spaces).

        Args:
            cursor: The current text cursor
            tab_size: Number of spaces to use for indentation
        """
        start = cursor.selectionStart()
        end = cursor.selectionEnd()
        reverse: bool = start == cursor.position()

        cursor.setPosition(start)
        cursor.movePosition(QTextCursor.MoveOperation.StartOfLine)

        # If selection ends at start of line, don't outdent that line
        end_offs = 0
        check_cursor = QTextCursor(cursor)
        check_cursor.setPosition(end)
        if check_cursor.atBlockStart():
            end_offs = 1

        # Work out how far to move the start position
        current_column = start - cursor.block().position()
        first_line = cursor.block().text()
        first_line_spaces = len(first_line) - len(first_line.lstrip(" "))
        first_line_spaces = min(first_line_spaces, tab_size)
        first_line_spaces = min(first_line_spaces, current_column)
        start -= first_line_spaces

        while cursor.position() <= end - end_offs:
            deletes_needed = tab_size

            while deletes_needed > 0:
                text = cursor.block().text()
                if not text or text[0] != " ":
                    break

                cursor.deleteChar()
                deletes_needed -= 1
                end -= 1

            if not cursor.movePosition(QTextCursor.MoveOperation.NextBlock):
                break

        cursor.setPosition(start if not reverse else end)
        cursor.setPosition(end if not reverse else start, QTextCursor.MoveMode.KeepAnchor)

    def _outdent_block_hard_tabs(self, cursor: QTextCursor) -> None:
        """Outdent a block of text using hard tabs.

        Args:
            cursor: The current text cursor
        """
        start = cursor.selectionStart()
        end = cursor.selectionEnd()
        reverse: bool = start == cursor.position()

        cursor.setPosition(start)
        cursor.movePosition(QTextCursor.MoveOperation.StartOfLine)

        # If selection ends at start of line, don't outdent that line
        end_offs = 0
        check_cursor = QTextCursor(cursor)
        check_cursor.setPosition(end)
        if check_cursor.atBlockStart():
            end_offs = 1

        # Work out how far to move the start position
        current_column = start - cursor.block().position()
        first_line = cursor.block().text()
        if first_line and first_line[0] == "\t" and current_column > 0:
            start -= 1

        while cursor.position() <= end - end_offs:
            text = cursor.block().text()
            if text and text[0] == "\t":
                cursor.deleteChar()
                end -= 1

            if not cursor.movePosition(QTextCursor.MoveOperation.NextBlock):
                break

        cursor.setPosition(start if not reverse else end)
        cursor.setPosition(end if not reverse else start, QTextCursor.MoveMode.KeepAnchor)

    def keyPressEvent(self, e: QKeyEvent) -> None:
        """Handle special key events."""
        # Is this a read-only widget?  If it is then we don't want to process certain key events,
        # leaving it to the parent to handle them.
        if self.isReadOnly():
            # Handle horizontal scrolling
            if self._has_code_block and e.key() in (Qt.Key.Key_Left, Qt.Key.Key_Right):
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

            if e.key() in (Qt.Key.Key_PageUp, Qt.Key.Key_PageDown, Qt.Key.Key_Up, Qt.Key.Key_Down):
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

        if e.key() == Qt.Key.Key_Tab:
            cursor = self.textCursor()
            mindspace_manager = MindspaceManager()
            if not mindspace_manager.has_mindspace():
                super().keyPressEvent(e)
                return

            settings = cast(MindspaceSettings, mindspace_manager.settings())

            scrollbar = self.verticalScrollBar()
            current_scroll = scrollbar.value()
            cursor.beginEditBlock()
            try:
                if not cursor.hasSelection():
                    if settings.use_soft_tabs:
                        self._indent_single_line_soft_tabs(cursor, settings.tab_size)

                    else:
                        self._indent_single_line_hard_tabs(cursor)

                else:
                    if settings.use_soft_tabs:
                        self._indent_block_soft_tabs(cursor, settings.tab_size)

                    else:
                        self._indent_block_hard_tabs(cursor)
            finally:
                cursor.endEditBlock()
                self.setTextCursor(cursor)
                scrollbar.setValue(current_scroll)

            e.accept()
            return

        if e.key() == Qt.Key.Key_Backtab:  # Shift+Tab
            cursor = self.textCursor()
            mindspace_manager = MindspaceManager()
            if not mindspace_manager.has_mindspace():
                super().keyPressEvent(e)
                return

            settings = cast(MindspaceSettings, mindspace_manager.settings())

            scrollbar = self.verticalScrollBar()
            current_scroll = scrollbar.value()
            cursor.beginEditBlock()
            try:
                if not cursor.hasSelection():
                    if settings.use_soft_tabs:
                        self._outdent_single_line_soft_tabs(cursor, settings.tab_size)

                    else:
                        self._outdent_single_line_hard_tabs(cursor)

                else:
                    if settings.use_soft_tabs:
                        self._outdent_block_soft_tabs(cursor, settings.tab_size)

                    else:
                        self._outdent_block_hard_tabs(cursor)

            finally:
                cursor.endEditBlock()
                self.setTextCursor(cursor)
                scrollbar.setValue(current_scroll)

            e.accept()
            return

        super().keyPressEvent(e)

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

    def find_text(self, text: str) -> bool:
        """Find text in the widget.

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
