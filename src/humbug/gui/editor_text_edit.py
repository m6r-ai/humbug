from PySide6.QtWidgets import QPlainTextEdit, QWidget
from PySide6.QtCore import Qt
from PySide6.QtGui import QPainter, QTextCursor, QKeyEvent

from humbug.gui.color_role import ColorRole
from humbug.gui.line_number_area import LineNumberArea
from humbug.gui.style_manager import StyleManager


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
        return digit_width * (digits + 2

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
        bg_color = self._style_manager.get_color(ColorRole.BACKGROUND_SECONDARY)
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
                text_color = self._style_manager.get_color(ColorRole.TEXT_PRIMARY)
                painter.setPen(text_color)
                painter.drawText(
                    left_padding,
                    int(top),
                    self._line_number_area.width() - (2 * left_padding),
                    self.fontMetrics().height(),
                    Qt.AlignRight,
                    number
                )

            block = block.next()
            top = bottom
            bottom = top + self.blockBoundingRect(block).height()
            block_number += 1

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
