"""Tests for moving the editor cursor to a line and column."""

import pytest

from desktop.editor_tab.editor_widget import EditorWidget


class TestEditorWidgetGotoLine:
    """Tests for EditorWidget.goto_line."""

    def test_moves_cursor_to_line_and_column(self, qapp) -> None:
        """A column within the line places the cursor at that column."""
        widget = EditorWidget()
        widget.setPlainText("short\nlonger line here")

        widget.goto_line(2, 5)

        cursor = widget.textCursor()
        assert cursor.blockNumber() == 1
        assert cursor.positionInBlock() == 4

    def test_column_past_line_end_clamps_to_line_end(self, qapp) -> None:
        """A column beyond the line length places the cursor at the end of the line."""
        widget = EditorWidget()
        widget.setPlainText("short\nlonger line here")

        widget.goto_line(1, 101)

        cursor = widget.textCursor()
        assert cursor.blockNumber() == 0
        assert cursor.positionInBlock() == 5

    def test_column_of_one_places_cursor_at_line_start(self, qapp) -> None:
        """A column of one places the cursor at the start of the line."""
        widget = EditorWidget()
        widget.setPlainText("short\nlonger line here")

        widget.goto_line(2, 1)

        cursor = widget.textCursor()
        assert cursor.blockNumber() == 1
        assert cursor.positionInBlock() == 0

    def test_line_below_one_raises(self, qapp) -> None:
        """A line number below one is rejected."""
        widget = EditorWidget()
        widget.setPlainText("short")

        with pytest.raises(ValueError):
            widget.goto_line(0)

    def test_line_past_document_raises(self, qapp) -> None:
        """A line number beyond the document is rejected."""
        widget = EditorWidget()
        widget.setPlainText("short")

        with pytest.raises(ValueError):
            widget.goto_line(2)

    def test_column_below_one_raises(self, qapp) -> None:
        """A column number below one is rejected."""
        widget = EditorWidget()
        widget.setPlainText("short")

        with pytest.raises(ValueError):
            widget.goto_line(1, 0)
