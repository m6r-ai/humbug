"""Single pane of the side-by-side diff view."""

import logging
from typing import List, Optional

from PySide6.QtWidgets import QPlainTextEdit, QWidget
from PySide6.QtCore import Qt, QRect
from PySide6.QtGui import (
    QColor, QPainter, QPaintEvent, QResizeEvent, QTextBlockUserData, QTextBlock
)

from humbug.color_role import ColorRole
from humbug.style_manager import StyleManager
from humbug.tabs.line_number_area import LineNumberArea
from humbug.tabs.diff.diff_row import DiffRow, DiffRowType


class _LineData(QTextBlockUserData):
    """Per-block metadata attached to each QTextBlock."""

    def __init__(self, row_type: DiffRowType, line_no: Optional[int]) -> None:
        super().__init__()
        self.row_type = row_type
        self.line_no = line_no


class DiffPane(QPlainTextEdit):
    """Read-only pane displaying one side of a side-by-side diff.

    The pane owns a LineNumberArea gutter that shows line numbers.  Background
    colours for each line are painted during the standard paint event using the
    row-type metadata stored in each QTextBlock.

    The pane hides its own vertical scrollbar; the parent DiffWidget provides a
    single shared scrollbar that drives both panes simultaneously.
    """

    def __init__(self, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._logger = logging.getLogger("DiffPane")
        self._style_manager = StyleManager()

        self.setReadOnly(True)
        self.setLineWrapMode(QPlainTextEdit.LineWrapMode.NoWrap)
        self.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAsNeeded)
        self.setFrameStyle(0)

        self._gutter = LineNumberArea(self, self._gutter_width, self._paint_gutter)
        self.blockCountChanged.connect(self._update_gutter_width)
        self.updateRequest.connect(self._update_gutter)
        self._update_gutter_width()

    def load_rows(self, rows: List[DiffRow], use_left: bool) -> None:
        """Populate the pane from a list of DiffRow objects.

        Args:
            rows: Row descriptors produced by DiffViewBuilder.
            use_left: If True, render left_text and left_line_no; otherwise
                      render right_text and right_line_no.
        """
        self.clear()
        cursor = self.textCursor()
        cursor.movePosition(cursor.MoveOperation.Start)
        cursor.beginEditBlock()

        for i, row in enumerate(rows):
            if i > 0:
                cursor.insertBlock()

            text = row.left_text if use_left else row.right_text
            line_no = row.left_line_no if use_left else row.right_line_no
            cursor.insertText(text)

            block = cursor.block()
            block.setUserData(_LineData(row.row_type, line_no))

        cursor.endEditBlock()

    def apply_style(self) -> None:
        """Reapply colours and font after a theme or zoom change."""
        zoom = self._style_manager.zoom_factor()
        base = self._style_manager.base_font_size()
        families = self._style_manager.monospace_font_families()

        font = self.font()
        font.setFamilies(families)
        font.setPointSizeF(base * zoom)
        self.setFont(font)

        bg = self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)
        fg = self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)
        self.setStyleSheet(f"""
            QPlainTextEdit {{
                background-color: {bg};
                color: {fg};
                border: none;
                padding: 0px;
            }}
        """)

        self._update_gutter_width()
        self.viewport().update()
        self._gutter.update()

    def resizeEvent(self, event: QResizeEvent) -> None:
        super().resizeEvent(event)
        cr = self.contentsRect()
        self._gutter.setGeometry(QRect(cr.left(), cr.top(), self._gutter_width(), cr.height()))

    def paintEvent(self, event: QPaintEvent) -> None:
        """Paint row backgrounds before the standard text painting."""
        self._paint_line_backgrounds(event)
        super().paintEvent(event)

    def _gutter_width(self) -> int:
        """Return the pixel width required for the line-number gutter.

        Matches the editor's formula: (digits + 4) character widths, giving
        2 character-widths of padding on each side of the number text.
        """
        digits = 1
        max_num = max(1, self.blockCount())
        while max_num >= 10:
            max_num //= 10
            digits += 1

        digit_width = self.fontMetrics().horizontalAdvance('9')
        return digit_width * (digits + 4)

    def _paint_gutter(self, event: QPaintEvent) -> None:
        """Paint line numbers into the gutter widget."""
        painter = QPainter(self._gutter)
        bg = self._style_manager.get_color(ColorRole.TAB_BACKGROUND_ACTIVE)
        painter.fillRect(event.rect(), bg)

        painter.setPen(self._style_manager.get_color(ColorRole.LINE_NUMBER))
        painter.setFont(self.font())

        # Right margin matches the editor: 2 character-widths from the right edge
        digit_width = self.fontMetrics().horizontalAdvance('9')
        right_margin = digit_width * 2

        block = self.firstVisibleBlock()
        top = int(self.blockBoundingGeometry(block).translated(self.contentOffset()).top())
        bottom = top + int(self.blockBoundingRect(block).height())

        while block.isValid() and top <= event.rect().bottom():
            if block.isVisible() and bottom >= event.rect().top():
                data = block.userData()
                if isinstance(data, _LineData) and data.line_no is not None:
                    painter.drawText(
                        QRect(
                            0,
                            top,
                            self._gutter.width() - right_margin,
                            self.fontMetrics().height(),
                        ),
                        Qt.AlignmentFlag.AlignRight | Qt.AlignmentFlag.AlignVCenter,
                        str(data.line_no),
                    )

            block = block.next()
            top = bottom
            bottom = top + int(self.blockBoundingRect(block).height())

    def _paint_line_backgrounds(self, event: QPaintEvent) -> None:
        """Fill each visible block's background rectangle with its row colour."""
        painter = QPainter(self.viewport())
        viewport_width = self.viewport().width()

        block = self.firstVisibleBlock()
        top = int(self.blockBoundingGeometry(block).translated(self.contentOffset()).top())
        bottom = top + int(self.blockBoundingRect(block).height())

        while block.isValid() and top <= event.rect().bottom():
            if block.isVisible() and bottom >= event.rect().top():
                color = self._background_for_block(block)
                if color is not None:
                    height = int(self.blockBoundingRect(block).height())
                    painter.fillRect(0, top, viewport_width, height, color)

            block = block.next()
            top = bottom
            bottom = top + int(self.blockBoundingRect(block).height())

    def _background_for_block(self, block: QTextBlock) -> QColor | None:
        """Return the background colour for a block, or None for the default."""
        data = block.userData()
        if not isinstance(data, _LineData):
            return None

        match data.row_type:
            case DiffRowType.REMOVED:
                return self._style_manager.get_color(ColorRole.DIFF_REMOVED_BACKGROUND)

            case DiffRowType.ADDED:
                return self._style_manager.get_color(ColorRole.DIFF_ADDED_BACKGROUND)

            case DiffRowType.CHANGED:
                return self._style_manager.get_color(ColorRole.DIFF_CHANGED_BACKGROUND)

            case DiffRowType.HEADER:
                return self._style_manager.get_color(ColorRole.DIFF_HEADER_BACKGROUND)

            case DiffRowType.FILLER:
                return self._style_manager.get_color(ColorRole.DIFF_FILLER_BACKGROUND)

            case _:
                return None

    def _update_gutter_width(self) -> None:
        self.setViewportMargins(self._gutter_width(), 0, 0, 0)

    def _update_gutter(self, rect: QRect, dy: int) -> None:
        if dy:
            self._gutter.scroll(0, dy)

        else:
            self._gutter.update(0, rect.y(), self._gutter.width(), rect.height())

        if rect.contains(self.viewport().rect()):
            self._update_gutter_width()
