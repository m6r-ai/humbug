"""Single pane of the side-by-side diff view."""

import logging
from typing import List, Optional

from PySide6.QtWidgets import QPlainTextEdit, QWidget
from PySide6.QtCore import Qt, QRect
from PySide6.QtGui import (
    QColor, QPainter, QPaintEvent, QResizeEvent, QTextBlockUserData, QTextBlock,
    QSyntaxHighlighter, QTextCharFormat, QTextCursor
)
from PySide6.QtWidgets import QTextEdit

from syntax import ProgrammingLanguage, ParserState, ParserRegistry, TokenType

from humbug.color_role import ColorRole
from humbug.style_manager import StyleManager
from humbug.tabs.line_number_area import LineNumberArea
from humbug.tabs.diff.diff_row import DiffRow, DiffRowType


class _BlockData(QTextBlockUserData):
    """Per-block metadata combining diff row info and syntax highlighter parser state.

    Keeping both pieces of data in a single QTextBlockUserData subclass avoids
    the problem of the syntax highlighter's setUserData call overwriting the diff
    row metadata (and vice versa), since Qt only allows one userData object per
    block.
    """

    def __init__(self, row_type: DiffRowType, line_no: Optional[int]) -> None:
        super().__init__()
        self.row_type = row_type
        self.line_no = line_no
        self.parser_state: ParserState | None = None


class _DiffPaneHighlighter(QSyntaxHighlighter):
    """Syntax highlighter for a DiffPane document.

    This is a slimmed-down highlighter that stores its parser state in the
    combined _BlockData object already attached to each block by load_rows(),
    rather than replacing it with a separate QTextBlockUserData.
    """

    def __init__(self, parent: QPlainTextEdit) -> None:
        super().__init__(parent.document())
        self._style_manager = StyleManager()
        self._syntax = ProgrammingLanguage.TEXT
        self._logger = logging.getLogger("_DiffPaneHighlighter")

    def set_syntax(self, syntax: ProgrammingLanguage) -> None:
        """
        Set the syntax highlighting language.

        Args:
            syntax: The programming language to use.
        """
        if self._syntax == syntax:
            return

        self._syntax = syntax

        if self.document().isEmpty():
            return

        self.rehighlight()

    def highlightBlock(self, text: str) -> None:
        """Apply syntax highlighting and update parser state on the block's _BlockData."""
        try:
            current_block = self.currentBlock()
            block_data = current_block.userData()

            # Only highlight blocks that have _BlockData attached (i.e. real diff rows).
            if not isinstance(block_data, _BlockData):
                return

            prev_parser_state: ParserState | None = None
            prev_block = current_block.previous()
            if prev_block.isValid():
                prev_data = prev_block.userData()
                if isinstance(prev_data, _BlockData):
                    prev_parser_state = prev_data.parser_state

            parser = ParserRegistry.create_parser(self._syntax)
            if not parser:
                return

            parser_state = parser.parse(prev_parser_state, text)

            last_token_pos = 0
            while True:
                token = parser.get_next_token()
                if token is None:
                    if last_token_pos < len(text):
                        self.setFormat(
                            last_token_pos,
                            len(text) - last_token_pos,
                            self._style_manager.get_highlight(TokenType.TEXT)
                        )
                    break

                highlight_len = len(token.value) + token.start - last_token_pos
                fmt: QTextCharFormat = self._style_manager.get_highlight(token.type)
                self.setFormat(last_token_pos, highlight_len, fmt)
                last_token_pos += highlight_len

            # Trigger rehighlight of subsequent blocks when continuation state changes.
            old_continuation = (
                block_data.parser_state.continuation_state
                if block_data.parser_state is not None else -1
            )
            new_continuation = (
                parser_state.continuation_state
                if parser_state is not None else 0
            )
            if old_continuation != new_continuation:
                self.setCurrentBlockState(self.currentBlockState() + 1)

            # Store updated parser state back into the existing _BlockData — do NOT
            # call setUserData here, which would replace the object and lose row_type
            # and line_no.
            block_data.parser_state = parser_state

        except Exception:
            self._logger.exception("highlighting exception")


class DiffPane(QPlainTextEdit):
    """Read-only pane displaying one side of a side-by-side diff.

    The pane owns a LineNumberArea gutter that shows line numbers and a
    _DiffPaneHighlighter for syntax highlighting.  Background colours for each
    line are painted during the standard paint event using the row-type metadata
    stored in each QTextBlock's _BlockData.

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

        self._highlighter = _DiffPaneHighlighter(self)

    def set_syntax(self, language: ProgrammingLanguage) -> None:
        """
        Set the syntax highlighting language for this pane.

        Args:
            language: The programming language to use for highlighting.
        """
        self._highlighter.set_syntax(language)

    def load_rows(self, rows: List[DiffRow], use_left: bool) -> None:
        """
        Populate the pane from a list of DiffRow objects.

        Each row becomes one QTextBlock.  A _BlockData object carrying the
        row type and line number is attached to the block before the highlighter
        runs, so the highlighter can read and update it in place without losing
        the diff metadata.

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
            block.setUserData(_BlockData(row.row_type, line_no))

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
        self._highlighter.rehighlight()

    def resizeEvent(self, event: QResizeEvent) -> None:
        super().resizeEvent(event)
        cr = self.contentsRect()
        self._gutter.setGeometry(QRect(cr.left(), cr.top(), self._gutter_width(), cr.height()))

    def paintEvent(self, event: QPaintEvent) -> None:
        """Paint row backgrounds before the standard text painting."""
        self._paint_line_backgrounds(event)
        super().paintEvent(event)

    def _gutter_width(self) -> int:
        """Return the pixel width required for the line-number gutter."""
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

        digit_width = self.fontMetrics().horizontalAdvance('9')
        right_margin = digit_width * 2

        block = self.firstVisibleBlock()
        top = int(self.blockBoundingGeometry(block).translated(self.contentOffset()).top())
        bottom = top + int(self.blockBoundingRect(block).height())

        while block.isValid() and top <= event.rect().bottom():
            if block.isVisible() and bottom >= event.rect().top():
                data = block.userData()
                if isinstance(data, _BlockData) and data.line_no is not None:
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
        if not isinstance(data, _BlockData):
            return None

        match data.row_type:
            case DiffRowType.REMOVED:
                return self._style_manager.get_color(ColorRole.DIFF_REMOVED_BACKGROUND)

            case DiffRowType.ADDED:
                return self._style_manager.get_color(ColorRole.DIFF_ADDED_BACKGROUND)

            case DiffRowType.CHANGED:
                return self._style_manager.get_color(ColorRole.DIFF_CHANGED_BACKGROUND)

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

    def find_matches(self, text: str) -> list[tuple[int, int]]:
        """Find all occurrences of *text* in this pane's document.

        Args:
            text: The search string.  An empty string returns an empty list.

        Returns:
            List of (start, end) character-position pairs, one per match.
        """
        matches: list[tuple[int, int]] = []
        if not text:
            return matches

        document = self.document()
        cursor = QTextCursor(document)
        while True:
            cursor = document.find(text, cursor)
            if cursor.isNull():
                break

            matches.append((cursor.selectionStart(), cursor.selectionEnd()))

        return matches

    def highlight_matches(
        self,
        matches: list[tuple[int, int]],
        current_index: int,
    ) -> None:
        """
        Apply extra-selection highlighting to the given match positions.

        The match at *current_index* receives the bright highlight colour;
        all others receive the dim colour.  Pass an empty *matches* list (or
        *current_index* == -1) to clear all highlights.

        Args:
            matches: List of (start, end) character-position pairs to highlight.
            current_index: Index into *matches* of the currently active match,
                or -1 if none.
        """
        if not matches:
            self.setExtraSelections([])
            return

        found_format = QTextCharFormat()
        found_format.setBackground(self._style_manager.get_color(ColorRole.TEXT_FOUND))
        dim_found_format = QTextCharFormat()
        dim_found_format.setBackground(self._style_manager.get_color(ColorRole.TEXT_FOUND_DIM))

        selections: list[QTextEdit.ExtraSelection] = []
        for i, (start, end) in enumerate(matches):
            cursor = QTextCursor(self.document())
            cursor.setPosition(start)
            cursor.setPosition(end, QTextCursor.MoveMode.KeepAnchor)

            sel = QTextEdit.ExtraSelection()
            sel.cursor = cursor  # type: ignore[attr-defined]
            sel.format = found_format if i == current_index else dim_found_format  # type: ignore[attr-defined]
            selections.append(sel)

        self.setExtraSelections(selections)

    def clear_find(self) -> None:
        """Remove all find highlights from this pane."""
        self.setExtraSelections([])

    def target_scroll_for_match(self, start: int) -> int:
        """
        Return the scrollbar value that would centre the match at *start* in the viewport.

        The pane's own vertical scrollbar is hidden (the shared scrollbar drives
        it), so we compute the target value without actually scrolling — the
        caller (DiffWidget) will animate the shared scrollbar to that value.

        Args:
            start: Character position of the match to scroll to.

        Returns:
            Target vertical scrollbar value to centre the match.
        """
        cursor = QTextCursor(self.document())
        cursor.setPosition(start)
        block_number = cursor.block().blockNumber()
        vbar = self.verticalScrollBar()
        visible_lines = self.viewport().height() // max(1, self.fontMetrics().lineSpacing())
        return max(vbar.minimum(), min(vbar.maximum(), block_number - visible_lines // 2))
