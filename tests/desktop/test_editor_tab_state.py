"""Tests for the editor tab's view-state and migration-state contracts."""

from desktop.editor_tab.editor_tab import EditorTab


class TestEditorTabViewState:
    """Tests for EditorTab.capture_view_state and restore_view_state."""

    def test_view_state_excludes_buffer_content(self, qapp) -> None:
        """View state describes the view only, never the unsaved buffer."""
        tab = EditorTab("t1", "")
        tab._editor_widget.setPlainText("unsaved text")  # pylint: disable=protected-access

        state = tab.capture_view_state()

        assert "content" not in state

    def test_view_state_includes_find_widget(self, qapp) -> None:
        """The find widget's state is part of the view state."""
        tab = EditorTab("t1", "")

        state = tab.capture_view_state()

        assert "find_widget" in state

    def test_restore_view_state_applies_scroll(self, qapp) -> None:
        """A saved scroll offset is reapplied to the editor."""
        tab = EditorTab("t1", "")
        tab._editor_widget.setPlainText("\n".join(f"line {i}" for i in range(200)))  # pylint: disable=protected-access

        tab.restore_view_state({"vertical_scroll": 4, "horizontal_scroll": 0})

        assert tab._editor_widget.verticalScrollBar().value() == 4  # pylint: disable=protected-access


class TestEditorTabMigrationState:
    """Tests for EditorTab.capture_migration_state and rebuild_from_migration_state."""

    def test_migration_state_includes_buffer_content(self, qapp) -> None:
        """Migration state carries the unsaved buffer across a column move."""
        tab = EditorTab("t1", "")
        tab._editor_widget.setPlainText("unsaved text")  # pylint: disable=protected-access

        state = tab.capture_migration_state()

        assert state["metadata"]["content"] == "unsaved text"

    def test_rebuild_preserves_unsaved_content(self, qapp) -> None:
        """Rebuilding from migration state reproduces the unsaved buffer."""
        tab = EditorTab("t1", "")
        tab._editor_widget.setPlainText("unsaved text")  # pylint: disable=protected-access

        state = tab.capture_migration_state()
        rebuilt = EditorTab.rebuild_from_migration_state(state, None)

        assert rebuilt._editor_widget.toPlainText() == "unsaved text"  # pylint: disable=protected-access

    def test_rebuild_preserves_tab_id(self, qapp) -> None:
        """The rebuilt tab keeps the original tab id."""
        tab = EditorTab("t1", "")

        state = tab.capture_migration_state()
        rebuilt = EditorTab.rebuild_from_migration_state(state, None)

        assert rebuilt.tab_id() == "t1"
