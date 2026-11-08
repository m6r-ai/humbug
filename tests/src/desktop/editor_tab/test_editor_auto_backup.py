"""Tests for where an editor keeps its auto backup."""

from pathlib import Path

from desktop.editor_tab.editor_tab import EditorTab


class TestEditorAutoBackup:
    """An editor's backup is its own working state, not content in the mindspace."""

    def test_a_saved_files_backup_is_kept_under_humbug(
        self, qapp, mindspace_sandbox, tmp_path
    ):
        """The backup is written to the mindspace's own storage, not beside the file."""
        path = Path(mindspace_sandbox.path) / "notes.txt"
        path.write_text("content", encoding="utf-8")
        tab = EditorTab("t1", str(path))

        backup = Path(tab._editor_widget._backup_file_path())  # pylint: disable=protected-access

        assert backup.parent == Path(mindspace_sandbox.path) / ".humbug" / "backups"
        assert backup != Path(f"{path}.backup")

    def test_two_files_of_the_same_name_get_different_backups(
        self, qapp, mindspace_sandbox
    ):
        """A backup is named after the file's path, so same-named files do not collide."""
        first = Path(mindspace_sandbox.path) / "notes.txt"
        second = Path(mindspace_sandbox.path) / "draft" / "notes.txt"
        second.parent.mkdir()
        first.write_text("one", encoding="utf-8")
        second.write_text("two", encoding="utf-8")

        first_tab = EditorTab("t1", str(first))
        second_tab = EditorTab("t2", str(second))

        first_backup = first_tab._editor_widget._backup_file_path()  # pylint: disable=protected-access
        second_backup = second_tab._editor_widget._backup_file_path()  # pylint: disable=protected-access

        assert first_backup != second_backup

    def test_the_backup_holds_the_buffer_until_the_file_is_saved(
        self, qapp, mindspace_sandbox
    ):
        """The backup is a copy of the unsaved buffer, and is removed once the file is saved."""
        path = Path(mindspace_sandbox.path) / "notes.txt"
        path.write_text("old", encoding="utf-8")
        tab = EditorTab("t1", str(path))
        widget = tab._editor_widget  # pylint: disable=protected-access

        widget.setPlainText("new")
        widget._auto_backup()  # pylint: disable=protected-access

        backup = Path(widget._backup_file_path())  # pylint: disable=protected-access
        assert backup.read_text(encoding="utf-8") == "new"
        assert path.read_text(encoding="utf-8") == "old"
        assert not Path(f"{path}.backup").exists()

        tab.save()

        assert not backup.exists()

    def test_an_untitled_tab_backs_up_the_same_way(self, qapp, mindspace_sandbox):
        """An untitled tab's backup is a file in the same directory, holding the buffer."""
        tab = EditorTab("t1", "", untitled_number=7)
        widget = tab._editor_widget  # pylint: disable=protected-access

        widget.setPlainText("draft")
        widget._auto_backup()  # pylint: disable=protected-access

        backup = Path(widget._backup_file_path())  # pylint: disable=protected-access
        assert backup.parent == Path(mindspace_sandbox.path) / ".humbug" / "backups"
        assert backup.read_text(encoding="utf-8") == "draft"
