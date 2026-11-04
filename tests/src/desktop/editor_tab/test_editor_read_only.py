"""Tests for a read-only editor tab showing an earlier version of a file."""

import os

from desktop.editor_tab.editor_tab import EditorTab


class TestReadOnlyEditorTab:
    """A read-only tab shows content that must not be written back or replaced."""

    def test_the_tab_reports_itself_read_only(self, qapp):
        tab = EditorTab("t1", "", read_only=True)

        assert tab.is_read_only()

    def test_the_tab_icon_marks_the_earlier_version(self, qapp):
        tab = EditorTab("t1", "/tmp/notes.txt", read_only=True)

        assert tab.tab_icon() == "read-only"

    def test_a_writable_tab_reports_no_read_only_marker(self, qapp):
        tab = EditorTab("t1", "/tmp/notes.txt")

        assert not tab.is_read_only()
        assert tab.tab_icon() == "editor"

    def test_the_replace_row_is_not_offered(self, qapp):
        tab = EditorTab("t1", "/tmp/notes.txt", read_only=True)

        assert not tab._find_widget._replace_row_enabled  # pylint: disable=protected-access

    def test_saving_is_refused_and_the_file_is_left_alone(self, qapp, tmp_path):
        path = tmp_path / "notes.txt"
        path.write_text("current content", encoding="utf-8")
        tab = EditorTab("t1", str(path), read_only=True)
        tab.set_content("earlier content")

        saved = tab._editor_widget.save_file()  # pylint: disable=protected-access

        assert not saved
        assert path.read_text(encoding="utf-8") == "current content"

    def test_a_change_to_the_file_does_not_replace_the_version_being_shown(self, qapp, tmp_path):
        path = tmp_path / "notes.txt"
        path.write_text("current content", encoding="utf-8")
        tab = EditorTab("t1", str(path), read_only=True)
        tab.set_content("earlier content")

        path.write_text("changed on disk", encoding="utf-8")
        tab._handle_file_changed(str(path))  # pylint: disable=protected-access

        assert tab._editor_widget.toPlainText() == "earlier content"  # pylint: disable=protected-access

    def test_a_deleted_file_does_not_empty_the_version_being_shown(self, qapp, tmp_path):
        path = tmp_path / "notes.txt"
        path.write_text("current content", encoding="utf-8")
        tab = EditorTab("t1", str(path), read_only=True)
        tab.set_content("earlier content")

        os.remove(path)
        tab._handle_file_changed(str(path))  # pylint: disable=protected-access

        assert tab._editor_widget.toPlainText() == "earlier content"  # pylint: disable=protected-access
