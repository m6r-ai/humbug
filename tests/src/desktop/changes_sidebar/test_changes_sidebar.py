"""Tests for the recent changes panel's presentation of recorded mutations."""

from PySide6.QtWidgets import QTreeWidget

from mindspace.journal_entry import JournalActor, JournalEntry, JournalOperation

from desktop.changes_sidebar.changes_sidebar import ChangesSidebar
from desktop.style_manager import StyleManager
from desktop.sidebar.sidebar_section_header import SidebarSectionHeader


def _entry(**overrides) -> JournalEntry:
    """Build a journal entry with plausible values."""
    values = {
        "sequence": 1,
        "timestamp": "2026-11-03T10:00:00+00:00",
        "path": "notes.txt",
        "operation": JournalOperation.WRITE,
        "actor": JournalActor.AI,
        "conversation": "conversation-1",
        "group": "",
        "existed_before": True,
        "pre_image_hash": "a" * 64,
        "post_image_hash": "b" * 64,
        "pre_image_trash": None,
    }
    values.update(overrides)
    return JournalEntry(**values)


class TestReplacedVersionAvailability:
    """A change has a replaced version to show exactly when one is still held."""

    def test_an_overwrite_with_a_captured_pre_image_has_one(self):
        assert ChangesSidebar._replaced_version_available(_entry())  # pylint: disable=protected-access

    def test_a_creation_has_none(self):
        entry = _entry(existed_before=False, pre_image_hash=None)

        assert not ChangesSidebar._replaced_version_available(entry)  # pylint: disable=protected-access

    def test_an_overwrite_too_large_to_capture_has_none(self):
        entry = _entry(pre_image_hash=None)

        assert not ChangesSidebar._replaced_version_available(entry)  # pylint: disable=protected-access

    def test_a_removal_the_trash_holds_has_one(self):
        entry = _entry(pre_image_hash=None, pre_image_trash="ab12cd34_notes.txt", post_image_hash=None)

        assert ChangesSidebar._replaced_version_available(entry)  # pylint: disable=protected-access

    def test_a_removed_directory_has_none(self):
        entry = _entry(
            pre_image_hash=None,
            pre_image_trash="ab12cd34_folder",
            pre_image_is_directory=True,
            post_image_hash=None,
        )

        assert not ChangesSidebar._replaced_version_available(entry)  # pylint: disable=protected-access

    def test_a_removed_directory_has_none(self):
        entry = _entry(
            pre_image_hash=None,
            pre_image_trash="ab12cd34_folder",
            pre_image_is_directory=True,
            post_image_hash=None,
        )

        assert not ChangesSidebar._replaced_version_available(entry)  # pylint: disable=protected-access


class TestRecordedTimeRendering:
    """A recorded timestamp is shown in the reader's own time."""

    def test_a_valid_timestamp_is_rendered_without_its_timezone(self):
        rendered = ChangesSidebar._format_time("2026-11-03T10:00:00+00:00")  # pylint: disable=protected-access

        assert "2026-11-03" in rendered
        assert "+00:00" not in rendered

    def test_an_unreadable_timestamp_is_shown_as_stored(self):
        assert ChangesSidebar._format_time("not a time") == "not a time"  # pylint: disable=protected-access


class TestInsetAlignment:
    """The title, the rows and the status text share one left inset."""

    def test_the_header_inset_follows_the_zoom_factor(self, qapp):
        header = SidebarSectionHeader("Changes")

        margins = header.layout().contentsMargins()
        expected = StyleManager().scale(SidebarSectionHeader.TITLE_INSET)

        assert margins.left() == expected
        assert margins.right() == expected

    def test_the_panel_insets_rows_and_status_text_like_the_title(self, qapp):
        panel = ChangesSidebar()
        style_manager = StyleManager()
        inset = style_manager.scale(SidebarSectionHeader.TITLE_INSET)

        sheet = panel.styleSheet()

        assert f"padding-left: {inset}px;" in sheet
        assert f"padding: {round(12 * style_manager.zoom_factor())}px {inset}px;" in sheet


class TestRowIconsFollowZoom:
    """A row's icon is rendered at the size the current zoom factor asks for."""

    def test_icons_are_rebuilt_when_the_zoom_factor_changes(self, mindspace_sandbox):  # pylint: disable=unused-argument
        panel = ChangesSidebar()
        panel._remember_entries([_entry()])  # pylint: disable=protected-access
        panel._render()  # pylint: disable=protected-access
        style_manager = StyleManager()
        original_zoom = style_manager.zoom_factor()
        row = panel._tree.topLevelItem(1)  # pylint: disable=protected-access

        try:
            assert row.icon(0).availableSizes()[0].width() == round(16 * original_zoom)

            style_manager.set_zoom(2.0)
            panel.apply_style()

            assert row.icon(0).availableSizes()[0].width() == round(16 * 2.0)
        finally:
            style_manager.set_zoom(original_zoom)
            panel.deleteLater()
