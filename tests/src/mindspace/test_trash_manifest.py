"""Tests for Mindspace's manifest-backed trash operations."""
import os

import pytest

from mindspace.mindspace import Mindspace
from mindspace.mindspace_error import MindspaceError
from mindspace.mindspace_settings import MindspaceSettings


def _make_mindspace(tmp_path, monkeypatch) -> Mindspace:
    """Construct a Mindspace with an open mindspace at a temp path."""
    mindspace = Mindspace(
        on_settings_changed=lambda: None,
        on_interactions_updated=lambda: None,
    )
    path = os.path.join(str(tmp_path), "ms")
    os.makedirs(os.path.join(path, ".humbug"))
    monkeypatch.setattr(mindspace, "_path", path)
    monkeypatch.setattr(mindspace, "_settings", MindspaceSettings(enabled_tools={}))
    return mindspace


def _trash_something(mindspace: Mindspace, original_path: str, is_dir: bool = False) -> str:
    """Create original_path and move it into trash, recording the manifest entry."""
    os.makedirs(os.path.dirname(original_path), exist_ok=True)
    if is_dir:
        os.makedirs(original_path)

    else:
        with open(original_path, 'w', encoding='utf-8') as f:
            f.write("content")

    trash_path = mindspace.new_trash_path(original_path)
    os.rename(original_path, trash_path)
    mindspace.record_trashed(original_path, trash_path)
    return trash_path


class TestListTrashed:
    """Mindspace.list_trashed reflects manifest entries and orphaned items alike."""

    def test_records_nested_original_path(self, tmp_path, monkeypatch):
        mindspace = _make_mindspace(tmp_path, monkeypatch)
        original = os.path.join(mindspace.conversations_dir(), "Work", "Q3", "chat.conv")
        _trash_something(mindspace, original)

        entries = mindspace.list_trashed()
        assert len(entries) == 1
        assert entries[0].original_path == os.path.join(".humbug", "conversations", "Work", "Q3", "chat.conv")
        assert not entries[0].is_dir

    def test_orphaned_item_with_no_manifest_entry_is_still_listed(self, tmp_path, monkeypatch):
        mindspace = _make_mindspace(tmp_path, monkeypatch)
        orphan_path = os.path.join(mindspace.trash_dir(), "aaaaaaaa_orphan.conv")
        with open(orphan_path, 'w', encoding='utf-8') as f:
            f.write("content")

        entries = mindspace.list_trashed()
        assert len(entries) == 1
        assert entries[0].original_path == ""
        assert entries[0].trash_name == "aaaaaaaa_orphan.conv"

    def test_manifest_file_itself_is_not_listed(self, tmp_path, monkeypatch):
        mindspace = _make_mindspace(tmp_path, monkeypatch)
        _trash_something(mindspace, os.path.join(mindspace.conversations_dir(), "chat.conv"))

        assert all(entry.trash_name != "manifest.json" for entry in mindspace.list_trashed())


class TestRestoreTrashed:
    """Mindspace.restore_trashed moves items back to their recorded (or best-guess) location."""

    def test_restores_to_original_nested_location(self, tmp_path, monkeypatch):
        mindspace = _make_mindspace(tmp_path, monkeypatch)
        original = os.path.join(mindspace.conversations_dir(), "Work", "Q3", "chat.conv")
        trash_path = _trash_something(mindspace, original)

        restored = mindspace.restore_trashed(os.path.basename(trash_path))

        assert restored == original
        assert os.path.exists(original)
        assert not mindspace.list_trashed()

    def test_orphan_restores_into_conversations_root(self, tmp_path, monkeypatch):
        mindspace = _make_mindspace(tmp_path, monkeypatch)
        trash_name = "aaaaaaaa_orphan.conv"
        orphan_path = os.path.join(mindspace.trash_dir(), trash_name)
        with open(orphan_path, 'w', encoding='utf-8') as f:
            f.write("content")

        restored = mindspace.restore_trashed(trash_name)

        assert restored == os.path.join(mindspace.conversations_dir(), "orphan.conv")
        assert os.path.exists(restored)

    def test_restore_raises_on_destination_collision(self, tmp_path, monkeypatch):
        mindspace = _make_mindspace(tmp_path, monkeypatch)
        original = os.path.join(mindspace.conversations_dir(), "chat.conv")
        trash_path = _trash_something(mindspace, original)

        # Something new now occupies the original location.
        with open(original, 'w', encoding='utf-8') as f:
            f.write("new content")

        with pytest.raises(MindspaceError):
            mindspace.restore_trashed(os.path.basename(trash_path))

        # The trashed copy is left untouched, and its manifest entry preserved.
        assert os.path.exists(trash_path)
        assert len(mindspace.list_trashed()) == 1

    def test_restore_raises_when_no_longer_in_trash(self, tmp_path, monkeypatch):
        mindspace = _make_mindspace(tmp_path, monkeypatch)

        with pytest.raises(MindspaceError):
            mindspace.restore_trashed("missing_name.conv")


class TestPurgeAndEmptyTrash:
    """Mindspace.purge_trashed / empty_trash permanently remove trashed items."""

    def test_purge_removes_file_and_manifest_entry(self, tmp_path, monkeypatch):
        mindspace = _make_mindspace(tmp_path, monkeypatch)
        trash_path = _trash_something(mindspace, os.path.join(mindspace.conversations_dir(), "chat.conv"))

        mindspace.purge_trashed(trash_path)

        assert not os.path.exists(trash_path)
        assert not mindspace.list_trashed()

    def test_purge_removes_directory(self, tmp_path, monkeypatch):
        mindspace = _make_mindspace(tmp_path, monkeypatch)
        trash_path = _trash_something(
            mindspace, os.path.join(mindspace.conversations_dir(), "Folder"), is_dir=True
        )

        mindspace.purge_trashed(trash_path)

        assert not os.path.exists(trash_path)

    def test_empty_trash_removes_everything_including_orphans(self, tmp_path, monkeypatch):
        mindspace = _make_mindspace(tmp_path, monkeypatch)
        _trash_something(mindspace, os.path.join(mindspace.conversations_dir(), "chat.conv"))
        orphan_path = os.path.join(mindspace.trash_dir(), "aaaaaaaa_orphan.conv")
        with open(orphan_path, 'w', encoding='utf-8') as f:
            f.write("content")

        mindspace.empty_trash()

        assert not mindspace.list_trashed()
        assert os.listdir(mindspace.trash_dir()) == ["manifest.json"]
