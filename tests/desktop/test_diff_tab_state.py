"""Tests for the diff tab's view-state and migration-state contracts."""

from desktop.diff_tab.diff_tab import DiffTab


class TestDiffTabViewState:
    """Tests for DiffTab.capture_view_state and restore_view_state."""

    def test_view_state_includes_find_widget(self, qapp) -> None:
        """The find widget's state is part of the view state."""
        tab = DiffTab("t1", "")

        state = tab.capture_view_state()

        assert "find_widget" in state

    def test_restore_view_state_restores_search_text(self, qapp) -> None:
        """A saved search term is reapplied to the find widget."""
        tab = DiffTab("t1", "")

        tab.restore_view_state({"find_widget": {"search_text": "needle"}})

        assert tab._find_widget.get_search_text() == "needle"  # pylint: disable=protected-access

    def test_restore_view_state_ignores_empty_state(self, qapp) -> None:
        """An empty state dictionary is a no-op."""
        tab = DiffTab("t1", "")

        tab.restore_view_state({})


class TestDiffTabMigrationState:
    """Tests for DiffTab.capture_migration_state and rebuild_from_migration_state."""

    def test_migration_state_includes_find_widget(self, qapp) -> None:
        """The find widget's state is carried across a column move."""
        tab = DiffTab("t1", "")
        tab._find_widget.set_search_text("needle")  # pylint: disable=protected-access

        state = tab.capture_migration_state()

        assert state["find_widget"]["search_text"] == "needle"

    def test_rebuild_preserves_find_widget_state(self, qapp) -> None:
        """Rebuilding from migration state reproduces the find widget state."""
        tab = DiffTab("t1", "")
        tab._find_widget.set_search_text("needle")  # pylint: disable=protected-access

        state = tab.capture_migration_state()
        rebuilt = DiffTab.rebuild_from_migration_state(state, None)

        assert rebuilt._find_widget.get_search_text() == "needle"  # pylint: disable=protected-access

    def test_rebuild_preserves_tab_id_and_path(self, qapp) -> None:
        """The rebuilt tab keeps the original tab id and path."""
        tab = DiffTab("t1", "")

        state = tab.capture_migration_state()
        rebuilt = DiffTab.rebuild_from_migration_state(state, None)

        assert rebuilt.tab_id() == "t1"
        assert rebuilt.path() == ""
