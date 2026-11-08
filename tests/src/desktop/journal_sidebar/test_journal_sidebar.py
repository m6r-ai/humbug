"""Tests for the journal panel's presentation of recorded mutations."""

import os

from PySide6.QtWidgets import QTreeWidget

from mindspace.journal_entry import JournalActor, JournalEntry, JournalOperation
from mindspace.mindspace import Mindspace

from desktop.journal_sidebar.journal_sidebar import JournalSidebar
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
        assert JournalSidebar._replaced_version_available(_entry())  # pylint: disable=protected-access

    def test_a_creation_has_none(self):
        entry = _entry(existed_before=False, pre_image_hash=None)

        assert not JournalSidebar._replaced_version_available(entry)  # pylint: disable=protected-access

    def test_an_overwrite_too_large_to_capture_has_none(self):
        entry = _entry(pre_image_hash=None)

        assert not JournalSidebar._replaced_version_available(entry)  # pylint: disable=protected-access

    def test_a_removal_the_trash_holds_has_one(self):
        entry = _entry(pre_image_hash=None, pre_image_trash="ab12cd34_notes.txt", post_image_hash=None)

        assert JournalSidebar._replaced_version_available(entry)  # pylint: disable=protected-access

    def test_a_removed_directory_has_none(self):
        entry = _entry(
            pre_image_hash=None,
            pre_image_trash="ab12cd34_folder",
            pre_image_is_directory=True,
            post_image_hash=None,
        )

        assert not JournalSidebar._replaced_version_available(entry)  # pylint: disable=protected-access

    def test_a_removed_directory_has_none(self):
        entry = _entry(
            pre_image_hash=None,
            pre_image_trash="ab12cd34_folder",
            pre_image_is_directory=True,
            post_image_hash=None,
        )

        assert not JournalSidebar._replaced_version_available(entry)  # pylint: disable=protected-access


class TestRecordedTimeRendering:
    """A recorded timestamp is shown in the reader's own time."""

    def test_a_valid_timestamp_is_rendered_without_its_timezone(self):
        rendered = JournalSidebar._format_time("2026-11-03T10:00:00+00:00")  # pylint: disable=protected-access

        assert "2026-11-03" in rendered
        assert "+00:00" not in rendered

    def test_an_unreadable_timestamp_is_shown_as_stored(self):
        assert JournalSidebar._format_time("not a time") == "not a time"  # pylint: disable=protected-access


class TestInsetAlignment:
    """The title, the rows and the status text share one left inset."""

    def test_the_header_inset_follows_the_zoom_factor(self, qapp):
        header = SidebarSectionHeader("Changes")

        margins = header.layout().contentsMargins()
        expected = StyleManager().scale(SidebarSectionHeader.TITLE_INSET)

        assert margins.left() == expected
        assert margins.right() == expected


class TestRowIconsFollowZoom:
    """A row's icon is rendered at the size the current zoom factor asks for."""

    def test_icons_are_rebuilt_when_the_zoom_factor_changes(self, mindspace_sandbox):  # pylint: disable=unused-argument
        panel = JournalSidebar()
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


class TestDetectedChanges:
    """A change nothing in Humbug made is listed like any other change."""

    def test_a_detected_change_is_listed_under_the_file_name(self):
        panel = JournalSidebar()

        rendered = panel._row_text(  # pylint: disable=protected-access
            _entry(path="notes.txt", actor=JournalActor.UNATTRIBUTED, conversation="")
        )

        assert rendered == "notes.txt"

    def test_a_detected_change_records_that_nothing_made_it(self):
        panel = JournalSidebar()

        tooltip = panel._tooltip_text(  # pylint: disable=protected-access
            _entry(path="sub/config.json", actor=JournalActor.UNATTRIBUTED, conversation="")
        )

        assert "Detected" in tooltip


class TestOlderChangeCount:
    """The count of changes not shown reflects the journal, not how often it was read."""

    def _entries(self, count):
        return [_entry(sequence=n) for n in range(1, count + 1)]

    def test_a_reload_rebuilds_the_count_rather_than_adding_to_it(self):
        panel = JournalSidebar()
        entries = self._entries(800)

        panel._remember_entries(entries, replace=True)  # pylint: disable=protected-access
        first = panel._older_count  # pylint: disable=protected-access

        panel._remember_entries(entries, replace=True)  # pylint: disable=protected-access

        assert panel._older_count == first  # pylint: disable=protected-access

    def test_a_reload_after_everything_was_forgotten_counts_nothing(self):
        panel = JournalSidebar()
        panel._remember_entries(self._entries(800), replace=True)  # pylint: disable=protected-access

        panel._remember_entries([], replace=True)  # pylint: disable=protected-access

        assert panel._older_count == 0  # pylint: disable=protected-access

    def test_an_incremental_refresh_adds_only_what_overflowed(self):
        panel = JournalSidebar()
        panel._remember_entries(self._entries(800), replace=True)  # pylint: disable=protected-access
        held = panel._entries  # pylint: disable=protected-access

        panel._remember_entries(held + self._entries(100), replace=False)  # pylint: disable=protected-access

        assert panel._older_count == 400  # pylint: disable=protected-access


class TestVersionActions:
    """Every action in the panel is about the change, not about the file on disk."""

    def _entry_for(self, path, **overrides):
        values = {"path": path}
        values.update(overrides)
        return _entry(**values)

    def test_a_creation_has_a_version_of_its_own_but_nothing_replaced(self):
        entry = self._entry_for("new.txt", existed_before=False, pre_image_hash=None)

        assert JournalSidebar._this_version_available(entry)  # pylint: disable=protected-access
        assert not JournalSidebar._replaced_version_available(entry)  # pylint: disable=protected-access

    def test_a_removal_has_nothing_of_its_own_but_something_replaced(self):
        entry = self._entry_for("gone.txt", operation=JournalOperation.DELETE, post_image_hash=None)

        assert not JournalSidebar._this_version_available(entry)  # pylint: disable=protected-access
        assert JournalSidebar._replaced_version_available(entry)  # pylint: disable=protected-access

    def test_an_overwrite_offers_both_versions(self):
        entry = self._entry_for("notes.txt")

        assert JournalSidebar._this_version_available(entry)  # pylint: disable=protected-access
        assert JournalSidebar._replaced_version_available(entry)  # pylint: disable=protected-access


class TestTooltipPaths:
    """A row's tooltip names paths as they sit in the mindspace, never in full."""

    def test_the_recorded_path_is_shown_as_recorded(self):
        """The path an entry records is already mindspace-relative and is reported unchanged."""
        panel = JournalSidebar()
        recorded = os.path.join("src", "notes.txt")

        tooltip = panel._tooltip_text(_entry(path=recorded))  # pylint: disable=protected-access

        assert tooltip.splitlines()[0] == recorded

    def test_an_absolute_transcript_path_is_shown_relative_to_the_mindspace(self, mindspace_sandbox):
        """An entry naming a conversation by its transcript path reports the path's place in the mindspace."""
        panel = JournalSidebar()
        transcript = os.path.join(mindspace_sandbox.conversations_dir, "chat.conv")
        entry = _entry(conversation=transcript)

        tooltip = panel._tooltip_text(entry)  # pylint: disable=protected-access

        relative = os.path.join(Mindspace.MINDSPACE_DIR, Mindspace.CONVERSATIONS_DIR, "chat.conv")
        assert f"conversation {relative}" in tooltip
        assert mindspace_sandbox.path not in tooltip

    def test_a_conversation_identifier_is_shown_unchanged(self):
        """A conversation named by its identifier is not a path and is reported as it is."""
        panel = JournalSidebar()

        tooltip = panel._tooltip_text(_entry(conversation="conversation-1"))  # pylint: disable=protected-access

        assert "conversation conversation-1" in tooltip
