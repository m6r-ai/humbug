"""Widget for displaying parts of individual conversation messages."""

import logging

from PySide6.QtWidgets import (
    QFrame, QTextEdit, QSizePolicy, QScrollArea
)
from PySide6.QtCore import Qt, QSize, QTimer, Signal, Slot
from PySide6.QtGui import (
    QTextOption, QTextCursor, QMouseEvent, QKeyEvent
)

from humbug.gui.style_manager import StyleManager
from humbug.mindspace.mindspace_manager import MindspaceManager


class ConversationTextEdit(QTextEdit):
    """QTextEdit that automatically adjusts its height to content."""

    mouseReleased = Signal(QMouseEvent)
    pageScrollRequested = Signal()

    def __init__(self, parent=None):
        super().__init__(parent)
        self.document().documentLayout().documentSizeChanged.connect(self._on_content_changed)
        self.document().setDocumentMargin(1)
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

        self._logger = logging.getLogger("ConversationTextEdit")

    def _handle_style_changed(self) -> None:
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

                # We've only handled the horizontal component - we need to let our parent
                # handle the vertical component.
                event.ignore()
                return

        # For all other cases, propagate the event up
        event.ignore()

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
        cursor.movePosition(QTextCursor.StartOfLine)

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

            if not cursor.movePosition(QTextCursor.NextBlock):
                break

        cursor.setPosition(start if not reverse else end)
        cursor.setPosition(end if not reverse else start, QTextCursor.KeepAnchor)

    def _indent_block_hard_tabs(self, cursor: QTextCursor) -> None:
        """Indent a block of text using hard tabs.

        Args:
            cursor: The current text cursor
        """
        start = cursor.selectionStart()
        end = cursor.selectionEnd()
        reverse: bool = start == cursor.position()

        cursor.setPosition(start)
        cursor.movePosition(QTextCursor.StartOfLine)

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

            if not cursor.movePosition(QTextCursor.NextBlock):
                break

        cursor.setPosition(start if not reverse else end)
        cursor.setPosition(end if not reverse else start, QTextCursor.KeepAnchor)

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
        cursor.movePosition(QTextCursor.StartOfLine)

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

            if not cursor.movePosition(QTextCursor.NextBlock):
                break

        cursor.setPosition(start if not reverse else end)
        cursor.setPosition(end if not reverse else start, QTextCursor.KeepAnchor)

    def _outdent_block_hard_tabs(self, cursor: QTextCursor) -> None:
        """Outdent a block of text using hard tabs.

        Args:
            cursor: The current text cursor
        """
        start = cursor.selectionStart()
        end = cursor.selectionEnd()
        reverse: bool = start == cursor.position()

        cursor.setPosition(start)
        cursor.movePosition(QTextCursor.StartOfLine)

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

            if not cursor.movePosition(QTextCursor.NextBlock):
                break

        cursor.setPosition(start if not reverse else end)
        cursor.setPosition(end if not reverse else start, QTextCursor.KeepAnchor)

    def keyPressEvent(self, event: QKeyEvent):
        """Handle special key events."""
        # Is this a read-only widget?  If it is then we don't want to process certain key events,
        # leaving it to the parent to handle them.
        if self.isReadOnly():
            if event.key() in (Qt.Key_PageUp, Qt.Key_PageDown, Qt.Key_Up, Qt.Key_Down):
                event.ignore()

            return

        if event.key() in (Qt.Key_PageUp, Qt.Key_PageDown):
            # Find the scroll area viewport by walking up hierarchy
            widget = self
            viewport = None
            while widget:
                if isinstance(widget.parent(), QScrollArea):
                    viewport = widget.parent().viewport()
                    break
                widget = widget.parent()

            if viewport:
                # Calculate visible lines based on cursor height
                cursor_rect = self.cursorRect()
                line_height = cursor_rect.height()
                visible_lines = max(1, viewport.height() // line_height)

                # Move cursor by calculated lines
                cursor = self.textCursor()
                orig_pos = cursor.position()

                movement = QTextCursor.Up if event.key() == Qt.Key_PageUp else QTextCursor.Down
                cursor.movePosition(movement, QTextCursor.MoveAnchor, visible_lines)

                # Only set cursor if it actually moved
                if cursor.position() != orig_pos:
                    self.setTextCursor(cursor)
                    # Signal for scroll - ConversationTab will handle ensuring cursor visibility
                    self.pageScrollRequested.emit()

            event.accept()
            return

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

        if event.key() == Qt.Key_Tab:
            cursor = self.textCursor()
            mindspace_manager = MindspaceManager()
            if not mindspace_manager.has_mindspace:
                super().keyPressEvent(event)
                return

            settings = mindspace_manager.settings

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

            event.accept()
            return

        if event.key() == Qt.Key_Backtab:  # Shift+Tab
            cursor = self.textCursor()
            mindspace_manager = MindspaceManager()
            if not mindspace_manager.has_mindspace:
                super().keyPressEvent(event)
                return

            settings = mindspace_manager.settings

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

        # Store the current cursor position and selection
        current_cursor = self.textCursor()
        position = current_cursor.position()
        has_selection = current_cursor.hasSelection()
        selection_start = current_cursor.selectionStart()
        selection_end = current_cursor.selectionEnd()

        # Create a new cursor for inserting text at the end
        insert_cursor = QTextCursor(self.document())
        insert_cursor.movePosition(QTextCursor.End)
        self.setTextCursor(insert_cursor)

        # Insert the new text
        new_text = text[self._current_length:]
        insert_cursor.insertText(new_text)
        self._current_length = len(text)

        # Restore the original cursor position and selection
        restored_cursor = self.textCursor()
        restored_cursor.setPosition(position)
        if has_selection:
            # If there was a selection, restore it
            if position == selection_end:
                # Cursor was at end of selection
                restored_cursor.setPosition(selection_start, QTextCursor.MoveAnchor)
                restored_cursor.setPosition(selection_end, QTextCursor.KeepAnchor)
            else:
                # Cursor was at start of selection
                restored_cursor.setPosition(selection_end, QTextCursor.KeepAnchor)

        self.setTextCursor(restored_cursor)

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
