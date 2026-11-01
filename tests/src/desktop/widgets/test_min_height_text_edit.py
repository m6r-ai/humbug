"""Tests for the height-follows-content text edit."""

from PySide6.QtWidgets import QApplication

from desktop.markdown import MarkdownTextEdit


class TestTextWidthChanged:
    def test_a_resize_that_keeps_the_width_reports_no_change(self, qapp: QApplication) -> None:
        """Only a resize that changes the text width reports a text width change."""
        editor = MarkdownTextEdit(is_input=False)
        editor.resize(400, 300)
        editor.show()
        qapp.processEvents()

        changes: list[int] = []
        editor.text_width_changed.connect(lambda: changes.append(1))

        editor.resize(400, 200)
        qapp.processEvents()

        assert changes == []

        editor.resize(500, 200)
        qapp.processEvents()

        assert changes == [1]
