"""Tests for block indentation and outdentation in the text editors."""

from PySide6.QtGui import QTextCursor

from desktop.editor_tab.editor_widget import EditorWidget
from desktop.markdown.markdown_text_edit import MarkdownTextEdit


def make_cursor(widget, start: int, end: int, reverse: bool) -> QTextCursor:
    """Build a cursor selecting between two positions, optionally reversed."""
    cursor = QTextCursor(widget.document())
    if reverse:
        cursor.setPosition(end)
        cursor.setPosition(start, QTextCursor.MoveMode.KeepAnchor)
    else:
        cursor.setPosition(start)
        cursor.setPosition(end, QTextCursor.MoveMode.KeepAnchor)
    return cursor


def selected_text(widget, cursor) -> str:
    """Return the text currently covered by the cursor's selection."""
    return widget.toPlainText()[cursor.selectionStart():cursor.selectionEnd()]


class TestEditorWidgetBlockIndent:
    """Block indent/outdent behaviour for EditorWidget."""

    def test_soft_tab_indent_keeps_line_start_anchor(self, qapp) -> None:
        """Indenting whole lines leaves the start at the start of the first line."""
        widget = EditorWidget()
        widget.setPlainText("aaa\nbbb\nccc\n")
        cursor = make_cursor(widget, 0, len("aaa\nbbb\n"), reverse=False)

        widget._indent_block_soft_tabs(cursor, 4)  # pylint: disable=protected-access

        assert widget.toPlainText() == "    aaa\n    bbb\nccc\n"
        assert cursor.selectionStart() == 0
        assert selected_text(widget, cursor) == "    aaa\n    bbb\n"

    def test_soft_tab_indent_tracks_midline_anchor(self, qapp) -> None:
        """Indenting a mid-line selection keeps the start on the same character."""
        widget = EditorWidget()
        widget.setPlainText("aaa\nbbb\nccc\n")
        cursor = make_cursor(widget, 1, len("aaa\nbbb\n") + 1, reverse=False)

        widget._indent_block_soft_tabs(cursor, 4)  # pylint: disable=protected-access

        assert widget.toPlainText() == "    aaa\n    bbb\n    ccc\n"
        assert cursor.selectionStart() == 5
        assert selected_text(widget, cursor) == "aa\n    bbb\n    c"

    def test_soft_tab_indent_reverse_selection_keeps_line_start_anchor(self, qapp) -> None:
        """A reversed whole-line selection keeps the start at the first line start."""
        widget = EditorWidget()
        widget.setPlainText("aaa\nbbb\nccc\n")
        cursor = make_cursor(widget, 0, len("aaa\nbbb\n"), reverse=True)

        widget._indent_block_soft_tabs(cursor, 4)  # pylint: disable=protected-access

        assert widget.toPlainText() == "    aaa\n    bbb\nccc\n"
        assert cursor.selectionStart() == 0
        assert selected_text(widget, cursor) == "    aaa\n    bbb\n"

    def test_soft_tab_indent_skips_empty_first_line(self, qapp) -> None:
        """An empty first line is not indented, so the start does not move."""
        widget = EditorWidget()
        widget.setPlainText("\nbbb\nccc\n")
        cursor = make_cursor(widget, 0, len("\nbbb\n") + 1, reverse=False)

        widget._indent_block_soft_tabs(cursor, 4)  # pylint: disable=protected-access

        assert widget.toPlainText() == "\n    bbb\n    ccc\n"
        assert cursor.selectionStart() == 0
        assert selected_text(widget, cursor) == "\n    bbb\n    c"

    def test_hard_tab_indent_keeps_line_start_anchor(self, qapp) -> None:
        """Hard-tab indenting whole lines leaves the start at the first line start."""
        widget = EditorWidget()
        widget.setPlainText("aaa\nbbb\nccc\n")
        cursor = make_cursor(widget, 0, len("aaa\nbbb\n"), reverse=False)

        widget._indent_block_hard_tabs(cursor)  # pylint: disable=protected-access

        assert widget.toPlainText() == "\taaa\n\tbbb\nccc\n"
        assert cursor.selectionStart() == 0
        assert selected_text(widget, cursor) == "\taaa\n\tbbb\n"

    def test_hard_tab_indent_tracks_midline_anchor(self, qapp) -> None:
        """Hard-tab indenting a mid-line selection keeps the start on the same character."""
        widget = EditorWidget()
        widget.setPlainText("aaa\nbbb\nccc\n")
        cursor = make_cursor(widget, 1, len("aaa\nbbb\n") + 1, reverse=False)

        widget._indent_block_hard_tabs(cursor)  # pylint: disable=protected-access

        assert widget.toPlainText() == "\taaa\n\tbbb\n\tccc\n"
        assert cursor.selectionStart() == 2
        assert selected_text(widget, cursor) == "aa\n\tbbb\n\tc"

    def test_hard_tab_indent_includes_last_line_when_selection_ends_midline(self, qapp) -> None:
        """A selection ending mid-line includes that line in the indent."""
        widget = EditorWidget()
        widget.setPlainText("aaa\nbbb\nccc\n")
        cursor = make_cursor(widget, 0, len("aaa\nbbb\n") + 1, reverse=False)

        widget._indent_block_hard_tabs(cursor)  # pylint: disable=protected-access

        assert widget.toPlainText() == "\taaa\n\tbbb\n\tccc\n"

    def test_hard_tab_indent_excludes_last_line_when_selection_ends_at_line_start(self, qapp) -> None:
        """A selection ending at a line start does not indent that line."""
        widget = EditorWidget()
        widget.setPlainText("aaa\nbbb\nccc\n")
        cursor = make_cursor(widget, 0, len("aaa\nbbb\n"), reverse=False)

        widget._indent_block_hard_tabs(cursor)  # pylint: disable=protected-access

        assert widget.toPlainText() == "\taaa\n\tbbb\nccc\n"

    def test_soft_tab_outdent_keeps_line_start_anchor(self, qapp) -> None:
        """Outdenting whole lines leaves the start at the start of the first line."""
        widget = EditorWidget()
        widget.setPlainText("    aaa\n    bbb\n    ccc\n")
        cursor = make_cursor(widget, 0, len("    aaa\n    bbb\n"), reverse=False)

        widget._outdent_block_soft_tabs(cursor, 4)  # pylint: disable=protected-access

        assert widget.toPlainText() == "aaa\nbbb\n    ccc\n"
        assert cursor.selectionStart() == 0
        assert selected_text(widget, cursor) == "aaa\nbbb\n"


class TestMarkdownTextEditBlockIndent:
    """Block indent/outdent behaviour for MarkdownTextEdit."""

    def test_soft_tab_indent_keeps_line_start_anchor(self, qapp) -> None:
        """Indenting whole lines leaves the start at the start of the first line."""
        widget = MarkdownTextEdit(is_input=True)
        widget.setPlainText("aaa\nbbb\nccc\n")
        cursor = make_cursor(widget, 0, len("aaa\nbbb\n"), reverse=False)

        widget._indent_block_soft_tabs(cursor, 4)  # pylint: disable=protected-access

        assert widget.toPlainText() == "    aaa\n    bbb\nccc\n"
        assert cursor.selectionStart() == 0

    def test_soft_tab_indent_tracks_midline_anchor(self, qapp) -> None:
        """Indenting a mid-line selection keeps the start on the same character."""
        widget = MarkdownTextEdit(is_input=True)
        widget.setPlainText("aaa\nbbb\nccc\n")
        cursor = make_cursor(widget, 1, len("aaa\nbbb\n") + 1, reverse=False)

        widget._indent_block_soft_tabs(cursor, 4)  # pylint: disable=protected-access

        assert widget.toPlainText() == "    aaa\n    bbb\n    ccc\n"
        assert cursor.selectionStart() == 5

    def test_hard_tab_indent_keeps_line_start_anchor(self, qapp) -> None:
        """Hard-tab indenting whole lines leaves the start at the first line start."""
        widget = MarkdownTextEdit(is_input=True)
        widget.setPlainText("aaa\nbbb\nccc\n")
        cursor = make_cursor(widget, 0, len("aaa\nbbb\n"), reverse=False)

        widget._indent_block_hard_tabs(cursor)  # pylint: disable=protected-access

        assert widget.toPlainText() == "\taaa\n\tbbb\nccc\n"
        assert cursor.selectionStart() == 0
