from typing import List, Tuple, cast

from PySide6.QtWidgets import QPlainTextEdit, QWidget, QTextEdit
from PySide6.QtCore import Qt, QRect
from PySide6.QtGui import QPainter, QTextCursor, QKeyEvent, QPalette, QBrush, QTextCharFormat

from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager
from humbug.gui.tab.editor.editor_line_number_area import EditorLineNumberArea
from humbug.language.language_manager import LanguageManager
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.mindspace.mindspace_settings import MindspaceSettings


class EditorWidget(QPlainTextEdit):
    """Text editor widget with line numbers, syntax highlighting, and find functionality."""

    def __init__(self, parent: QWidget | None = None) -> None:
        """Initialize the editor."""
        super().__init__(parent)

        # Enable standard scrollbars
        self.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAsNeeded)
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAsNeeded)

        # Editor settings
        self.setLineWrapMode(QPlainTextEdit.LineWrapMode.NoWrap)  # No word wrap for code
        self.setTabStopDistance(32)  # 4 spaces worth of tab stops

        self._style_manager = StyleManager()

        # Setup line number area
        self._line_number_area = EditorLineNumberArea(self)
        font = self._line_number_area.font()
        font.setFamilies(self._style_manager.monospace_font_families)
        self.setFont(font)
        self._line_number_area.setFont(font)

        self.blockCountChanged.connect(self.update_line_number_area_width)
        self.updateRequest.connect(self._update_line_number_area)

        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._handle_language_changed)
        self.update_line_number_area_width()

        # Highlighted text should retain any underlying colours (e.g. syntax highlighting)
        palette = self.palette()
        palette.setBrush(QPalette.ColorRole.HighlightedText, QBrush(Qt.BrushStyle.NoBrush))
        self.setPalette(palette)

        # Initialize find functionality
        self._matches: List[Tuple[int, int]] = []  # List of (start, end) positions
        self._current_match = -1
        self._last_search = ""
        self._style_manager.style_changed.connect(self._handle_style_changed)

    def focusInEvent(self, event):
        """Handle focus in event."""
        super().focusInEvent(event)

        # Ensure this widget ends up with the focus
        self.setFocus()

    def _handle_language_changed(self) -> None:
        """Handle language changes by updating the UI."""
        self.update_line_number_area_width()
        self.viewport().update()

    def line_number_area_width(self) -> int:
        """Calculate the width needed for the line number area."""
        digits = 1
        max_num = max(1, self.blockCount())
        while max_num >= 10:
            max_num //= 10
            digits += 1

        digit_width = self.fontMetrics().horizontalAdvance('9')
        return digit_width * (digits + 4)

    def update_line_number_area_width(self):
        """Update the margins to accommodate the line numbers."""
        width = self.line_number_area_width()

        # Set margin on appropriate side based on layout direction
        if self.layoutDirection() == Qt.RightToLeft:
            self.setViewportMargins(0, 0, 0, 0)  # Right margin
        else:
            self.setViewportMargins(width, 0, 0, 0)  # Left margin

    def _update_line_number_area(self, rect, dy):
        """Handle updates to the line number area."""
        if dy:
            self._line_number_area.scroll(0, dy)
        else:
            self._line_number_area.update(0, rect.y(),
                self._line_number_area.width(), rect.height())

        if rect.contains(self.viewport().rect()):
            self.update_line_number_area_width()

    def resizeEvent(self, event):
        """Handle resize events."""
        super().resizeEvent(event)
        cr = self.contentsRect()
        width = self.line_number_area_width()

        if self.layoutDirection() == Qt.LayoutDirection.RightToLeft:
            self._line_number_area.setGeometry(
                cr.right() - width,
                cr.top(),
                width,
                cr.height()
            )
        else:
            self._line_number_area.setGeometry(
                cr.left(),
                cr.top(),
                width,
                cr.height()
            )

    def line_number_area_paint_event(self, event):
        """Paint the line numbers."""
        painter = QPainter(self._line_number_area)
        bg_color = self._style_manager.get_color(ColorRole.TAB_BACKGROUND_ACTIVE)
        painter.fillRect(event.rect(), bg_color)

        painter.setFont(self.font())

        block = self.firstVisibleBlock()
        block_number = block.blockNumber()
        offset = self.contentOffset()
        top = self.blockBoundingGeometry(block).translated(offset).top()
        bottom = top + self.blockBoundingRect(block).height()

        # Use two space widths for padding
        padding = self.fontMetrics().horizontalAdvance('9') * 2

        # Adjust alignment and padding based on layout direction
        is_rtl = self.layoutDirection() == Qt.RightToLeft
        alignment = Qt.AlignmentFlag.AlignLeft if is_rtl else Qt.AlignmentFlag.AlignRight

        while block.isValid() and top <= event.rect().bottom():
            if block.isVisible() and bottom >= event.rect().top():
                number = str(block_number + 1)
                text_color = self._style_manager.get_color(ColorRole.LINE_NUMBER)
                painter.setPen(text_color)
                text_rect = QRect(
                    0,
                    int(top),
                    self._line_number_area.width() - padding,
                    self.fontMetrics().height()
                )
                painter.drawText(text_rect, alignment, number)

            block = block.next()
            top = bottom
            bottom = top + self.blockBoundingRect(block).height()
            block_number += 1

    def _indent_single_line_soft_tabs(self, cursor: QTextCursor, tab_size: int) -> None:
        """
        Indent a single line using soft tabs.

        Args:
            cursor: The current text cursor
            tab_size: Number of spaces to use for indentation
        """
        # Calculate spaces needed to reach next tab stop
        current_column = cursor.position() - cursor.block().position()
        spaces_needed = tab_size - (current_column % tab_size)
        cursor.insertText(" " * spaces_needed)

    def _indent_single_line_hard_tabs(self, cursor: QTextCursor) -> None:
        """
        Indent a single line using hard tabs.

        Args:
            cursor: The current text cursor
        """
        cursor.insertText("\t")

    def _indent_block_soft_tabs(self, cursor: QTextCursor, tab_size: int) -> None:
        """
        Indent a block of text using soft tabs.

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
                # We hit the end of the file
                break

        cursor.setPosition(start if not reverse else end)
        cursor.setPosition(end if not reverse else start, QTextCursor.MoveMode.KeepAnchor)

    def _indent_block_hard_tabs(self, cursor: QTextCursor) -> None:
        """
        Indent a block of text using hard tabs.

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
                # We hit the end of the file
                break

        cursor.setPosition(start if not reverse else end)
        cursor.setPosition(end if not reverse else start, QTextCursor.MoveMode.KeepAnchor)

    def _outdent_single_line_soft_tabs(self, cursor: QTextCursor, tab_size: int) -> None:
        """
        Outdent a single line using soft tabs.

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
        """
        Outdent a single line using hard tabs.

        Args:
            cursor: The current text cursor
        """
        current_column = cursor.position() - cursor.block().position()
        if current_column > 0:
            text = cursor.block().text()
            if text and text[current_column - 1] == "\t":
                cursor.deletePreviousChar()

    def _outdent_block_soft_tabs(self, cursor: QTextCursor, tab_size: int) -> None:
        """
        Outdent a block of text using soft tabs.

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

        # Work out how far to move the start postion
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
                # We hit the end of the block
                break

        cursor.setPosition(start if not reverse else end)
        cursor.setPosition(end if not reverse else start, QTextCursor.MoveMode.KeepAnchor)

    def _outdent_block_hard_tabs(self, cursor: QTextCursor) -> None:
        """
        Outdent a block of text using hard tabs.

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

        # Work out how far to move the start postion
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
                # We hit the end of the block
                break

        cursor.setPosition(start if not reverse else end)
        cursor.setPosition(end if not reverse else start, QTextCursor.MoveMode.KeepAnchor)

    def keyPressEvent(self, event: QKeyEvent) -> None:
        """
        Handle special key events.

        Args:
            event: The key event to handle
        """
        if event.key() == Qt.Key.Key_Home:
            cursor = self.textCursor()
            cursor.movePosition(QTextCursor.MoveOperation.StartOfLine)
            self.setTextCursor(cursor)
            event.accept()
            return

        if event.key() == Qt.Key.Key_End:
            cursor = self.textCursor()
            cursor.movePosition(QTextCursor.MoveOperation.EndOfLine)
            self.setTextCursor(cursor)
            event.accept()
            return

        if event.key() == Qt.Key.Key_Tab:
            cursor = self.textCursor()
            mindspace_manager = MindspaceManager()
            if not mindspace_manager.has_mindspace:
                super().keyPressEvent(event)
                return

            settings = cast(MindspaceSettings, mindspace_manager.settings)

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

        if event.key() == Qt.Key.Key_Backtab:  # Shift+Tab
            cursor = self.textCursor()
            mindspace_manager = MindspaceManager()
            if not mindspace_manager.has_mindspace:
                super().keyPressEvent(event)
                return

            settings = cast(MindspaceSettings, mindspace_manager.settings)

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

    def _handle_style_changed(self) -> None:
        """Handle style changes affecting search highlighting."""
        self._highlight_matches()

    def find_text(self, text: str, forward: bool = True) -> None:
        """
        Find all instances of text and highlight them.

        Args:
            text: Text to search for
            forward: Whether to search forward from current position
        """
        # Clear existing highlights if search text changed
        if text != self._last_search:
            self._clear_highlights()
            self._matches = []
            self._current_match = -1
            self._last_search = text

        document = self.document()

        # Find all matches if this is a new search
        if not self._matches and text:
            cursor = QTextCursor(document)
            while True:
                cursor = document.find(text, cursor)
                if cursor.isNull():
                    break
                self._matches.append((cursor.selectionStart(), cursor.selectionEnd()))

        if not self._matches:
            return

        # Move to next/previous match
        if forward:
            self._current_match = (self._current_match + 1) % len(self._matches)
        else:
            self._current_match = (self._current_match - 1) if self._current_match > 0 else len(self._matches) - 1

        # Highlight all matches
        self._highlight_matches()

        # Scroll to current match
        self._scroll_to_match(self._current_match)

    def _highlight_matches(self) -> None:
        """Update the highlighting of all matches."""
        self._clear_highlights()

        if not self._matches:
            return

        found_format = QTextCharFormat()
        found_format.setBackground(self._style_manager.get_color(ColorRole.TEXT_FOUND))
        dim_found_format = QTextCharFormat()
        dim_found_format.setBackground(self._style_manager.get_color(ColorRole.TEXT_FOUND_DIM))

        # Create extra selections list
        selections = []

        # Highlight all matches
        for i, (start, end) in enumerate(self._matches):
            cursor = QTextCursor(self.document())
            cursor.setPosition(start)
            cursor.setPosition(end, QTextCursor.MoveMode.KeepAnchor)

            # Create extra selection
            extra_selection = QTextEdit.ExtraSelection()
            extra_selection.cursor = cursor

            # Use different format for current match
            if i == self._current_match:
                extra_selection.format = found_format
            else:
                extra_selection.format = dim_found_format

            selections.append(extra_selection)

        # Apply selections
        self.setExtraSelections(selections)

    def _scroll_to_match(self, match_index: int) -> None:
        """
        Scroll to ensure the given match is visible.

        Args:
            match_index: Index of match to scroll to
        """
        if 0 <= match_index < len(self._matches):
            cursor = QTextCursor(self.document())
            cursor.setPosition(self._matches[match_index][0])
            self.setTextCursor(cursor)
            self.ensureCursorVisible()

    def _clear_highlights(self) -> None:
        """Clear all search highlights."""
        self.setExtraSelections([])

    def get_match_status(self) -> Tuple[int, int]:
        """
        Get the current match status.

        Returns:
            Tuple of (current_match, total_matches)
        """
        if not self._matches:
            return 0, 0

        return self._current_match + 1, len(self._matches)

    def clear_find(self) -> None:
        """Clear all find state."""
        self._clear_highlights()
        self._matches = []
        self._current_match = -1
        self._last_search = ""
