"""Tests for the preview tab's view-state and migration-state contracts."""

from desktop.preview_tab.preview_tab import PreviewTab


def _write_preview_file(tmp_path, name: str = "doc.md") -> str:
    """Write a small markdown file and return its path."""
    path = tmp_path / name
    path.write_text("# Heading\n\nSome body text.\n", encoding="utf-8")
    return str(path)


class TestPreviewTabViewState:
    """Tests for PreviewTab.capture_view_state and restore_view_state."""

    def test_view_state_includes_find_widget(self, qapp, tmp_path) -> None:
        """The find widget's state is part of the view state."""
        tab = PreviewTab("t1", _write_preview_file(tmp_path))

        state = tab.capture_view_state()

        assert "find_widget" in state

    def test_view_state_includes_scroll_position(self, qapp, tmp_path) -> None:
        """The scroll position is part of the view state."""
        tab = PreviewTab("t1", _write_preview_file(tmp_path))

        state = tab.capture_view_state()

        assert "scroll_position" in state

    def test_restore_view_state_ignores_empty_state(self, qapp, tmp_path) -> None:
        """An empty state dictionary is a no-op."""
        tab = PreviewTab("t1", _write_preview_file(tmp_path))

        tab.restore_view_state({})


class TestPreviewTabMigrationState:
    """Tests for PreviewTab.capture_migration_state and rebuild_from_migration_state."""

    def test_migration_state_carries_path(self, qapp, tmp_path) -> None:
        """Migration state carries the previewed path across a column move."""
        path = _write_preview_file(tmp_path)
        tab = PreviewTab("t1", path)

        state = tab.capture_migration_state()

        assert state["path"] == path

    def test_rebuild_preserves_tab_id_and_path(self, qapp, tmp_path) -> None:
        """The rebuilt tab keeps the original tab id and path."""
        path = _write_preview_file(tmp_path)
        tab = PreviewTab("t1", path)

        state = tab.capture_migration_state()
        rebuilt = PreviewTab.rebuild_from_migration_state(state, None)

        assert rebuilt.tab_id() == "t1"
        assert rebuilt.path() == path
