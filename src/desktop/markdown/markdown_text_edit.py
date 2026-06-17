"""Widget for displaying parts of individual Markdown messages."""

import logging
from typing import cast

from PySide6.QtWidgets import QWidget
from PySide6.QtCore import Qt, Signal, QMimeData, QUrl
from PySide6.QtGui import (
    QTextOption, QTextCursor, QMouseEvent, QKeyEvent, QPalette, QBrush
)
from PySide6.QtGui import QPainter, QPaintEvent, QColor, QTextDocument
from PySide6.QtGui import QMovie

from mindspace.mindspace_settings import MindspaceSettings

from desktop.widgets.min_height_text_edit import MinHeightTextEdit
from desktop.mindspace.mindspace_manager import MindspaceManager
from desktop.style_manager import StyleManager
from desktop.markdown.markdown_highlighter import MarkdownHighlighter
from desktop.color_role import ColorRole
from desktop.markdown.markdown_block_data import MarkdownBlockData


class MarkdownTextEdit(MinHeightTextEdit):
    """MinHeightTextEdit that displays Markdown."""

    mouse_pressed = Signal(QMouseEvent)
    mouse_released = Signal(QMouseEvent)
    link_clicked = Signal(str)
    page_key_scroll_requested = Signal()

    def __init__(self, is_input: bool, parent: QWidget | None = None, ) -> None:
        super().__init__(parent)

        self._is_input = is_input

        # Track code block state
        self._has_code_block = False

        self._logger = logging.getLogger("MarkdownTextEdit")

        # Highlighted text should retain any underlying colours (e.g. syntax highlighting)
        palette = self.palette()
        palette.setBrush(QPalette.ColorRole.HighlightedText, QBrush(Qt.BrushStyle.NoBrush))
        self.setPalette(palette)

        self.setReadOnly(not is_input)

        self._highlighter: MarkdownHighlighter | None = None

        # We only use the highlighter for input areas
        if is_input:
            self._highlighter = MarkdownHighlighter(self.document())
            self._highlighter.code_block_state_changed.connect(self._on_code_block_state_changed)

        # Calculate tab stops
        self._style_manager = StyleManager()
        self._last_highlights_version = self._style_manager.highlights_version()
        self._animated_gifs: dict[str, QMovie] = {}
        self.destroyed.connect(self.clear_animated_gifs)
        self.apply_style()

    def _on_code_block_state_changed(self, has_code_block: bool) -> None:
        """Handle changes in code block state."""
        if has_code_block == self._has_code_block:
            return

        self._has_code_block = has_code_block
        if has_code_block:
            self.setWordWrapMode(QTextOption.WrapMode.NoWrap)
            self.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAsNeeded)

        else:
            self.setWordWrapMode(QTextOption.WrapMode.WrapAtWordBoundaryOrAnywhere)
            self.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)

        # Force layout update
        self._on_content_resized()

    def apply_style(self) -> None:
        """Apply style changes."""
        style_manager = self._style_manager
        factor = style_manager.zoom_factor()
        font = self.font()
        base_font_size = style_manager.base_font_size()
        font.setPointSizeF(base_font_size * factor)
        self.setFont(font)
        space_width = style_manager.get_space_width()
        self.setTabStopDistance(space_width * 8)
        self.document().setIndentWidth(space_width * 4)

        if self._highlighter and self._style_manager.highlights_version() != self._last_highlights_version:
            self._last_highlights_version = self._style_manager.highlights_version()
            self._highlighter.rehighlight()

    def clear_animated_gifs(self) -> None:
        """Stop and discard all active animated GIF movies."""
        for movie in self._animated_gifs.values():
            movie.stop()

        self._animated_gifs.clear()

    def register_animated_gif(self, resource_name: str, path: str) -> None:
        """
        Register an animated GIF to be played inside the document.

        Creates a QMovie for the given file and connects its frameChanged signal
        so that each new frame is written back into the document resource and the
        viewport is repainted.  Any previously registered movie for the same
        resource name is stopped and replaced.

        Args:
            resource_name: The document resource name used for this image
            path: Absolute local file path to the GIF
        """
        existing = self._animated_gifs.get(resource_name)
        if existing is not None:
            existing.stop()

        movie = QMovie(path)
        self._animated_gifs[resource_name] = movie

        def _on_frame_changed(_frame: int) -> None:
            try:
                self.document().addResource(
                    QTextDocument.ResourceType.ImageResource,
                    QUrl(resource_name),
                    movie.currentImage()
                )
                self.viewport().update()
            except RuntimeError:
                movie.stop()

        movie.frameChanged.connect(_on_frame_changed)
        movie.start()

    def insertFromMimeData(self, source: QMimeData) -> None:
        """
        Insert plain text only from clipboard, ignoring formatting.

        Args:
            source: The mime data from the clipboard or drag-and-drop operation
        """
        if source.hasText():
            cursor = self.textCursor()
            cursor.insertText(source.text())

    def mousePressEvent(self, e: QMouseEvent) -> None:
        """Propagate mouse press events to parent."""
        self.mouse_pressed.emit(e)

        # Check for link clicks
        anchor = self.anchorAt(e.pos())
        if anchor and e.button() == Qt.MouseButton.LeftButton:
            self.link_clicked.emit(anchor)
            e.accept()
            return

        # Default handling for other cases
        super().mousePressEvent(e)

    def mouseReleaseEvent(self, e: QMouseEvent) -> None:
        """Propagate mouse release events to parent."""
        super().mouseReleaseEvent(e)
        self.mouse_released.emit(e)

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
        # Is this a display-only widget?  If it is then we don't want to process key events,
        # leaving it to the parent to handle them.
        if not self._is_input:
            e.ignore()
            return

        if e.key() in (Qt.Key.Key_PageUp, Qt.Key.Key_PageDown):
            if self._height_cap is not None:
                # The widget is in capped/scrollable mode - page within the input itself
                cursor_rect = self.cursorRect()
                line_height = cursor_rect.height()
                visible_lines = max(1, self._height_cap // line_height)

                cursor = self.textCursor()
                orig_pos = cursor.position()

                movement = QTextCursor.MoveOperation.Up if e.key() == Qt.Key.Key_PageUp else QTextCursor.MoveOperation.Down
                cursor.movePosition(movement, QTextCursor.MoveMode.MoveAnchor, visible_lines)

                if cursor.position() != orig_pos:
                    self.setTextCursor(cursor)
                    self.ensureCursorVisible()

            else:
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

    def paintEvent(self, event: QPaintEvent) -> None:
        """
        Paint the widget, adding blockquote border bars after Qt's normal rendering.

        For each visible text block that carries a non-empty blockquote_bar_offsets list,
        one coloured vertical bar is drawn per nesting level in the left margin.  The
        bars are painted after the normal text so they appear on top of the background
        fills.  The background fill is painted before the normal text so it sits behind
        the text content.

        Args:
            event: The paint event
        """
        if not self._is_input:
            self._paint_blockquote_backgrounds(event)

        super().paintEvent(event)

        if self._is_input:
            return

        bar_width = 3
        color: QColor = self._style_manager.get_color(ColorRole.BLOCKQUOTE_BORDER)

        painter = QPainter(self.viewport())
        painter.setPen(Qt.PenStyle.NoPen)
        painter.setBrush(color)

        doc = self.document()
        layout = doc.documentLayout()
        indent_width = doc.indentWidth()
        content_offset_x = -self.horizontalScrollBar().value()
        content_offset_y = -self.verticalScrollBar().value()

        block = doc.begin()
        while block.isValid():
            user_data = block.userData()
            if isinstance(user_data, MarkdownBlockData) and user_data.blockquote_bar_offsets:
                # Skip empty separator blocks that trail a blockquote — an empty
                # block whose next block has a lower blockquote depth exists only
                # as a cursor position and should not be painted.
                next_block = block.next()
                next_data = next_block.userData() if next_block.isValid() else None
                next_offsets = next_data.blockquote_bar_offsets if isinstance(next_data, MarkdownBlockData) else []
                if block.text() == "" and len(next_offsets) < len(user_data.blockquote_bar_offsets):
                    block = block.next()
                    continue

                block_rect = layout.blockBoundingRect(block).translated(content_offset_x, content_offset_y)

                # blockBoundingRect excludes top and bottom margins.  We extend the bar
                # upward through the top margin and downward through the bottom margin so
                # adjacent blocks produce a seamless bar.  round() rather than int() is
                # used throughout to avoid sub-pixel gaps from truncation.  Each bar level
                # is extended independently: a bar at depth N extends into the margin only
                # if the adjacent block also has the same bar offset at depth N.
                fmt = block.blockFormat()
                bottom_margin = fmt.bottomMargin()
                top_margin = fmt.topMargin()

                prev_block = block.previous()
                prev_offsets: list[int] = []
                if prev_block.isValid():
                    prev_data = prev_block.userData()
                    if isinstance(prev_data, MarkdownBlockData):
                        prev_offsets = prev_data.blockquote_bar_offsets

                # next_block, next_data, and next_depth were already computed above.

                # Only paint bars for blocks that intersect the dirty region
                if block_rect.bottom() >= event.rect().top() and block_rect.top() <= event.rect().bottom():
                    for level, list_offset in enumerate(user_data.blockquote_bar_offsets):
                        # Bar at depth (level+1): extend into margins only if the
                        # adjacent block has the same bar offset at this depth level.
                        prev_matches = len(prev_offsets) >= level + 1 and prev_offsets[level] == list_offset
                        next_matches = len(next_offsets) >= level + 1 and next_offsets[level] == list_offset
                        effective_top = top_margin if prev_matches else 0.0
                        effective_bottom = bottom_margin if next_matches else 0.0
                        bar_top = round(block_rect.top() - effective_top)
                        bar_height = round(block_rect.height() + effective_top + effective_bottom)
                        x = round((level + list_offset) * indent_width) + content_offset_x
                        painter.drawRect(x, bar_top, bar_width, bar_height)

            block = block.next()

        painter.end()

    def _paint_blockquote_backgrounds(self, event: QPaintEvent) -> None:
        """
        Paint background fills for all visible blockquote blocks before normal text rendering.

        For each visible text block that carries a non-empty blockquote_bar_offsets list a
        filled rectangle is drawn behind the text, extending across the full viewport width
        from the indented left edge.  Adjacent blockquote blocks at the same nesting depth
        have their top/bottom margins included so the fill is seamless.

        Args:
            event: The paint event
        """
        bg_color: QColor = self._style_manager.get_color(ColorRole.BLOCKQUOTE_BACKGROUND)

        painter = QPainter(self.viewport())
        painter.setPen(Qt.PenStyle.NoPen)
        painter.setBrush(bg_color)

        doc = self.document()
        layout = doc.documentLayout()
        indent_width = doc.indentWidth()
        content_offset_x = -self.horizontalScrollBar().value()
        content_offset_y = -self.verticalScrollBar().value()
        viewport_width = self.viewport().width()

        block = doc.begin()
        while block.isValid():
            user_data = block.userData()
            if isinstance(user_data, MarkdownBlockData) and user_data.blockquote_bar_offsets:
                next_block = block.next()
                next_data = next_block.userData() if next_block.isValid() else None
                next_offsets = next_data.blockquote_bar_offsets if isinstance(next_data, MarkdownBlockData) else []
                if block.text() == "" and len(next_offsets) < len(user_data.blockquote_bar_offsets):
                    block = block.next()
                    continue

                block_rect = layout.blockBoundingRect(block).translated(content_offset_x, content_offset_y)

                if block_rect.bottom() >= event.rect().top() and block_rect.top() <= event.rect().bottom():
                    fmt = block.blockFormat()
                    top_margin = fmt.topMargin()
                    bottom_margin = fmt.bottomMargin()

                    prev_block = block.previous()
                    prev_data = prev_block.userData() if prev_block.isValid() else None
                    prev_offsets = prev_data.blockquote_bar_offsets if isinstance(prev_data, MarkdownBlockData) else []

                    prev_matches = len(prev_offsets) >= 1
                    next_matches = len(next_offsets) >= 1
                    effective_top = top_margin if prev_matches else 0.0
                    effective_bottom = bottom_margin if next_matches else 0.0

                    bg_top = round(block_rect.top() - effective_top)
                    bg_height = round(block_rect.height() + effective_top + effective_bottom)
                    x = round(user_data.blockquote_bar_offsets[0] * indent_width) + content_offset_x
                    painter.drawRect(x, bg_top, viewport_width - x, bg_height)

            block = block.next()

        painter.end()

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
