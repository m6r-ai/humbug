from PySide6.QtWidgets import QPlainTextEdit, QWidget
from PySide6.QtCore import Qt
from PySide6.QtGui import QPainter, QTextCursor, QKeyEvent

from humbug.gui.color_role import ColorRole
from humbug.gui.line_number_area import LineNumberArea
from humbug.gui.style_manager import StyleManager
from humbug.workspace.workspace_manager import WorkspaceManager


class EditorTextEdit(QPlainTextEdit):
    """Text editor widget with line numbers and syntax highlighting."""

    def __init__(self, parent: QWidget = None):
        """Initialize the editor."""
        super().__init__(parent)

        # Enable standard scrollbars
        self.setVerticalScrollBarPolicy(Qt.ScrollBarAsNeeded)
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarAsNeeded)

        # Editor settings
        self.setLineWrapMode(QPlainTextEdit.NoWrap)  # No word wrap for code
        self.setTabStopDistance(32)  # 4 spaces worth of tab stops

        self._style_manager = StyleManager()

        # Setup line number area
        self._line_number_area = LineNumberArea(self)
        self._monospace_font_families = self._style_manager.monospace_font_families
        font = self._line_number_area.font()
        font.setFamilies(self._monospace_font_families)
        self._line_number_area.setFont(font)

        self.blockCountChanged.connect(self.update_line_number_area_width)
        self.updateRequest.connect(self._update_line_number_area)
        self.update_line_number_area_width()

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
        self.setViewportMargins(self.line_number_area_width(), 0, 0, 0)

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
        rect = cr
        rect.setWidth(width)
        self._line_number_area.setGeometry(rect)

    def line_number_area_paint_event(self, event):
        """Paint the line number area."""
        painter = QPainter(self._line_number_area)
        bg_color = self._style_manager.get_color(ColorRole.TAB_BACKGROUND_ACTIVE)
        painter.fillRect(event.rect(), bg_color)

        painter.setFont(self.font())

        block = self.firstVisibleBlock()
        block_number = block.blockNumber()
        offset = self.contentOffset()
        top = self.blockBoundingGeometry(block).translated(offset).top()
        bottom = top + self.blockBoundingRect(block).height()

        # Use one space width for left padding
        left_padding = self.fontMetrics().horizontalAdvance('9')

        while block.isValid() and top <= event.rect().bottom():
            if block.isVisible() and bottom >= event.rect().top():
                number = str(block_number + 1)
                text_color = self._style_manager.get_color(ColorRole.LINE_NUMBER)
                painter.setPen(text_color)
                painter.drawText(
                    left_padding,
                    int(top),
                    self._line_number_area.width() - (3 * left_padding),
                    self.fontMetrics().height(),
                    Qt.AlignRight,
                    number
                )

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

        cursor.setPosition(start)
        cursor.movePosition(QTextCursor.StartOfLine)
        first_line_pos = cursor.position()

        while cursor.position() <= end:
            if not cursor.atBlockStart():
                cursor.movePosition(QTextCursor.StartOfLine)

            cursor.insertText(" " * tab_size)
            end += tab_size

            if not cursor.movePosition(QTextCursor.NextBlock):
                break

        cursor.setPosition(first_line_pos)
        cursor.setPosition(end, QTextCursor.KeepAnchor)

    def _indent_block_hard_tabs(self, cursor: QTextCursor) -> None:
        """
        Indent a block of text using hard tabs.

        Args:
            cursor: The current text cursor
        """
        start = cursor.selectionStart()
        end = cursor.selectionEnd()

        cursor.setPosition(start)
        cursor.movePosition(QTextCursor.StartOfLine)
        first_line_pos = cursor.position()

        while cursor.position() <= end:
            if not cursor.atBlockStart():
                cursor.movePosition(QTextCursor.StartOfLine)

            cursor.insertText("\t")
            end += 1

            if not cursor.movePosition(QTextCursor.NextBlock):
                break

        cursor.setPosition(first_line_pos)
        cursor.setPosition(end, QTextCursor.KeepAnchor)

    def _outdent_single_line_soft_tabs(self, cursor: QTextCursor, tab_size: int) -> None:
        """
        Outdent a single line using soft tabs.

        Args:
            cursor: The current text cursor
            tab_size: Number of spaces to use for indentation
        """
        # Store initial column for cursor restoration
        initial_pos = cursor.position()
        line_start_pos = cursor.block().position()
        current_column = initial_pos - line_start_pos

        # Select the entire line
        cursor.movePosition(QTextCursor.StartOfLine)
        cursor.movePosition(QTextCursor.EndOfLine, QTextCursor.KeepAnchor)
        line_text = cursor.selectedText()

        # Count leading spaces
        leading_spaces = len(line_text) - len(line_text.lstrip())
        if leading_spaces == 0:
            return

        # Calculate how many spaces to remove
        spaces_to_remove = min(leading_spaces, tab_size)
        new_text = line_text[spaces_to_remove:]
        cursor.insertText(new_text)

        # Restore cursor position
        new_column = max(0, current_column - spaces_to_remove)
        cursor.setPosition(line_start_pos + new_column)

    def _outdent_single_line_hard_tabs(self, cursor: QTextCursor) -> None:
        """
        Outdent a single line using hard tabs.

        Args:
            cursor: The current text cursor

        Returns:
            bool: True if a tab was removed, False otherwise
        """
        # Store initial column for cursor restoration
        initial_pos = cursor.position()
        line_start_pos = cursor.block().position()
        current_column = initial_pos - line_start_pos

        cursor.movePosition(QTextCursor.StartOfLine)
        cursor.movePosition(QTextCursor.EndOfLine, QTextCursor.KeepAnchor)
        line_text = cursor.selectedText()

        if not line_text.startswith('\t'):
            return

        spaces_to_remove = 1
        cursor.insertText(line_text[1:])

        # Restore cursor position
        new_column = max(0, current_column - spaces_to_remove)
        cursor.setPosition(line_start_pos + new_column)

    def _outdent_block_soft_tabs(self, cursor: QTextCursor, tab_size: int) -> None:
        """
        Outdent a block of text using soft tabs.

        Args:
            cursor: The current text cursor
            tab_size: Number of spaces to use for indentation
        """
        start = cursor.selectionStart()
        end = cursor.selectionEnd()

        cursor.setPosition(start)
        cursor.movePosition(QTextCursor.StartOfLine)
        first_line_pos = cursor.position()

        total_chars_removed = 0

        while cursor.position() <= end:
            if not cursor.atBlockStart():
                cursor.movePosition(QTextCursor.StartOfLine)

            cursor.movePosition(QTextCursor.EndOfLine, QTextCursor.KeepAnchor)
            line_text = cursor.selectedText()
            leading_spaces = len(line_text) - len(line_text.lstrip(" "))

            if leading_spaces > 0:
                chars_to_remove = min(leading_spaces, tab_size)
                cursor.insertText(line_text[chars_to_remove:])
                end -= chars_to_remove
                total_chars_removed += chars_to_remove

            if not cursor.movePosition(QTextCursor.NextBlock):
                break

        end -= total_chars_removed
        cursor.setPosition(first_line_pos)
        cursor.setPosition(end, QTextCursor.KeepAnchor)

    def _outdent_block_hard_tabs(self, cursor: QTextCursor) -> None:
        """
        Outdent a block of text using hard tabs.

        Args:
            cursor: The current text cursor
        """
        start = cursor.selectionStart()
        end = cursor.selectionEnd()

        cursor.setPosition(start)
        cursor.movePosition(QTextCursor.StartOfLine)
        first_line_pos = cursor.position()

        tabs_removed = 0

        while cursor.position() <= end:
            if not cursor.atBlockStart():
                cursor.movePosition(QTextCursor.StartOfLine)

            cursor.movePosition(QTextCursor.EndOfLine, QTextCursor.KeepAnchor)
            line_text = cursor.selectedText()

            if line_text.startswith('\t'):
                cursor.insertText(line_text[1:])
                end -= 1
                tabs_removed += 1

            if not cursor.movePosition(QTextCursor.NextBlock):
                break

        end -= tabs_removed
        cursor.setPosition(first_line_pos)
        cursor.setPosition(end, QTextCursor.KeepAnchor)

    def keyPressEvent(self, event: QKeyEvent) -> None:
        """
        Handle special key events.

        Args:
            event: The key event to handle
        """
        workspace_manager = WorkspaceManager()
        if not workspace_manager.has_workspace:
            super().keyPressEvent(event)
            return

        settings = workspace_manager.settings
        cursor = self.textCursor()

        if event.key() == Qt.Key_Tab:
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
            event.accept()
            return

        if event.key() == Qt.Key_Backtab:  # Shift+Tab
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

        super().keyPressEvent(event)
