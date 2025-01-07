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
            cursor.insertText(" " * tab_size)
            end += tab_size
            cursor.movePosition(QTextCursor.NextBlock)

        cursor.setPosition(start if not reverse else end)
        cursor.setPosition(end if not reverse else start, QTextCursor.KeepAnchor)

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
        cursor.movePosition(QTextCursor.StartOfLine)

        # If selection ends at start of line, don't indent that line
        end_offs = 0
        check_cursor = QTextCursor(cursor)
        check_cursor.setPosition(end)
        if check_cursor.atBlockStart():
            end_offs = 1

        start += 1
        while cursor.position() <= end - end_offs:
            cursor.insertText("\t")
            end += 1
            cursor.movePosition(QTextCursor.NextBlock)

        cursor.setPosition(start if not reverse else end)
        cursor.setPosition(end if not reverse else start, QTextCursor.KeepAnchor)

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
        cursor.movePosition(QTextCursor.StartOfLine)

        # If selection ends at start of line, don't outdent that line
        end_offs = 0
        check_cursor = QTextCursor(cursor)
        check_cursor.setPosition(end)
        if check_cursor.atBlockStart():
            end_offs = 1

        # Work out how far to move the start postion
        first_line = cursor.block().text()
        first_line_spaces = len(first_line) - len(first_line.lstrip(" "))
        first_line_spaces = min(first_line_spaces, tab_size)
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

            cursor.movePosition(QTextCursor.NextBlock)

        cursor.setPosition(start if not reverse else end)
        cursor.setPosition(end if not reverse else start, QTextCursor.KeepAnchor)

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
        cursor.movePosition(QTextCursor.StartOfLine)

        # If selection ends at start of line, don't outdent that line
        end_offs = 0
        check_cursor = QTextCursor(cursor)
        check_cursor.setPosition(end)
        if check_cursor.atBlockStart():
            end_offs = 1

        # Work out how far to move the start postion
        first_line = cursor.block().text()
        if first_line and first_line[0] == "\t":
            start -= 1

        while cursor.position() <= end - end_offs:
            text = cursor.block().text()
            if not text or text[0] != "\t":
                break

            cursor.deleteChar()
            end -= 1
            cursor.movePosition(QTextCursor.NextBlock)

        cursor.setPosition(start if not reverse else end)
        cursor.setPosition(end if not reverse else start, QTextCursor.KeepAnchor)

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
