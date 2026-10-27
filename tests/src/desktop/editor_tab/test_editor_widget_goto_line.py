"""Tests for moving the editor cursor to a line and column."""

import pytest

from desktop.editor_tab.editor_tab import EditorTab
from desktop.editor_tab.editor_widget import EditorWidget
from desktop.tab.tab_base import TabBase


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


class TestGotoLineClamped:
    """Tests for the tab-level jump used by callers that cannot know the length."""

    def _tab(self, text: str) -> EditorTab:
        tab = EditorTab("goto-clamped", "")
        tab._editor_widget.setPlainText(text)  # pylint: disable=protected-access
        return tab

    def test_a_line_past_the_end_goes_to_the_last_line(self, qapp) -> None:
        """A line number beyond the document lands on its last line."""
        tab = self._tab("one\ntwo\nthree")

        tab.goto_line_clamped(9999)

        assert tab._editor_widget.get_cursor_info()['line'] == 3  # pylint: disable=protected-access

    def test_a_line_below_one_goes_to_the_first_line(self, qapp) -> None:
        """A line number below one lands on the first line."""
        tab = self._tab("one\ntwo\nthree")

        tab.goto_line_clamped(0)

        assert tab._editor_widget.get_cursor_info()['line'] == 1  # pylint: disable=protected-access

    def test_a_line_in_range_is_used_as_given(self, qapp) -> None:
        """A line within the document is not altered."""
        tab = self._tab("one\ntwo\nthree")

        tab.goto_line_clamped(2)

        assert tab._editor_widget.get_cursor_info()['line'] == 2  # pylint: disable=protected-access

    def test_a_tab_that_cannot_go_to_a_line_ignores_it(self, qapp) -> None:
        """The base class no-op means callers need no type check."""
        tab = TabBase("plain-tab")

        tab.goto_line_clamped(9999)

        assert tab.can_show_goto_line() is False
