"""Tests for MindspaceManager recent-mindspaces tracking."""

import json
import os

import pytest

# pylint: disable=wrong-import-position
from desktop.mindspace.mindspace_manager import MindspaceManager
from mindspace.mindspace_settings import MindspaceSettings


@pytest.fixture
def manager(qapp, tmp_path):
    """Create a MindspaceManager with an isolated home config file.

    The MindspaceManager is a singleton, so we reset its internal state
    before each test to get a clean instance.
    """
    # Reset the singleton so we get a fresh instance.
    MindspaceManager._instance = None

    # Point the home config to a temp file.
    config_path = tmp_path / "mindspace.json"

    mgr = MindspaceManager()
    mgr._home_config = str(config_path)

    yield mgr

    # Clean up the singleton for subsequent tests.
    MindspaceManager._instance = None


def _create_mindspace_dir(base: str, name: str) -> str:
    """Create a mindspace directory with a .humbug subfolder and return its path."""
    path = os.path.join(base, name)
    os.makedirs(os.path.join(path, ".humbug"))
    return path


def _set_current(mgr: MindspaceManager, path: str, monkeypatch) -> None:
    """Set the current mindspace path on the underlying Mindspace model."""
    monkeypatch.setattr(mgr._mindspace, "_path", path)


def _write_conv_file(path: str, parent: dict | None) -> None:
    """Write a minimal .conv file, optionally with a delegate parent reference."""
    with open(path, 'w', encoding='utf-8') as f:
        json.dump({"metadata": {"version": "0.1", "parent": parent}, "conversation": []}, f)


class TestRecentMindspaces:
    """Tests for the recent_mindspaces() method."""

    def test_returns_empty_when_no_config(self, manager, tmp_path):
        """recent_mindspaces() returns [] when the config file does not exist."""
        assert manager.recent_mindspaces() == []

    def test_returns_empty_when_no_recent_key(self, manager, tmp_path):
        """recent_mindspaces() returns [] when the config has no recentMindspaces key."""
        with open(manager._home_config, 'w', encoding='utf-8') as f:
            json.dump({"lastMindspace": ""}, f)

        assert manager.recent_mindspaces() == []

    def test_returns_recent_paths(self, manager, tmp_path):
        """recent_mindspaces() returns paths from the config file."""
        ms1 = _create_mindspace_dir(str(tmp_path), "alpha")
        ms2 = _create_mindspace_dir(str(tmp_path), "beta")

        with open(manager._home_config, 'w', encoding='utf-8') as f:
            json.dump({
                "lastMindspace": "",
                "recentMindspaces": [ms1, ms2],
            }, f)

        result = manager.recent_mindspaces()
        assert result == [ms1, ms2]

    def test_prunes_stale_paths(self, manager, tmp_path):
        """recent_mindspaces() silently removes paths that no longer exist on disk."""
        ms1 = _create_mindspace_dir(str(tmp_path), "alpha")
        stale = os.path.join(str(tmp_path), "deleted")

        with open(manager._home_config, 'w', encoding='utf-8') as f:
            json.dump({
                "lastMindspace": "",
                "recentMindspaces": [ms1, stale],
            }, f)

        result = manager.recent_mindspaces()
        assert result == [ms1]

    def test_excludes_current_mindspace(self, manager, tmp_path, monkeypatch):
        """recent_mindspaces() excludes the currently open mindspace."""
        ms1 = _create_mindspace_dir(str(tmp_path), "alpha")
        ms2 = _create_mindspace_dir(str(tmp_path), "beta")

        # Simulate that ms1 is the current mindspace.
        _set_current(manager, ms1, monkeypatch)

        with open(manager._home_config, 'w', encoding='utf-8') as f:
            json.dump({
                "lastMindspace": ms1,
                "recentMindspaces": [ms1, ms2],
            }, f)

        result = manager.recent_mindspaces()
        assert result == [ms2]

    def test_deduplicates_paths(self, manager, tmp_path):
        """recent_mindspaces() removes duplicate entries."""
        ms1 = _create_mindspace_dir(str(tmp_path), "alpha")

        with open(manager._home_config, 'w', encoding='utf-8') as f:
            json.dump({
                "lastMindspace": "",
                "recentMindspaces": [ms1, ms1, ms1],
            }, f)

        result = manager.recent_mindspaces()
        assert result == [ms1]

    def test_caps_at_max(self, manager, tmp_path):
        """recent_mindspaces() caps the result at MAX_RECENT_MINDSPACES."""
        paths = []
        for i in range(manager.MAX_RECENT_MINDSPACES + 5):
            p = _create_mindspace_dir(str(tmp_path), f"ms{i}")
            paths.append(p)

        with open(manager._home_config, 'w', encoding='utf-8') as f:
            json.dump({
                "lastMindspace": "",
                "recentMindspaces": paths,
            }, f)

        result = manager.recent_mindspaces()
        assert len(result) == manager.MAX_RECENT_MINDSPACES

    def test_ignores_non_string_entries(self, manager, tmp_path):
        """recent_mindspaces() silently skips non-string entries in the list."""
        ms1 = _create_mindspace_dir(str(tmp_path), "alpha")

        with open(manager._home_config, 'w', encoding='utf-8') as f:
            json.dump({
                "lastMindspace": "",
                "recentMindspaces": [ms1, 42, None, True],
            }, f)

        result = manager.recent_mindspaces()
        assert result == [ms1]

    def test_ignores_non_list_recent(self, manager, tmp_path):
        """recent_mindspaces() returns [] when recentMindspaces is not a list."""
        with open(manager._home_config, 'w', encoding='utf-8') as f:
            json.dump({
                "lastMindspace": "",
                "recentMindspaces": "not-a-list",
            }, f)

        assert manager.recent_mindspaces() == []


class TestUpdateHomeTracking:
    """Tests for the _update_home_tracking() method."""

    def test_writes_last_mindspace_and_recent(self, manager, tmp_path, monkeypatch):
        """_update_home_tracking() writes both lastMindspace and recentMindspaces."""
        ms1 = _create_mindspace_dir(str(tmp_path), "alpha")

        # Simulate opening ms1 (no previous config).
        _set_current(manager, ms1, monkeypatch)
        manager._update_home_tracking()

        with open(manager._home_config, encoding='utf-8') as f:
            data = json.load(f)

        assert data["lastMindspace"] == ms1
        assert data["recentMindspaces"] == []

    def test_promotes_previous_to_recent(self, manager, tmp_path, monkeypatch):
        """Switching from ms1 to ms2 promotes ms1 to the front of recent."""
        ms1 = _create_mindspace_dir(str(tmp_path), "alpha")
        ms2 = _create_mindspace_dir(str(tmp_path), "beta")

        # Start with ms1 open.
        _set_current(manager, ms1, monkeypatch)
        manager._update_home_tracking()

        # Now switch to ms2.
        _set_current(manager, ms2, monkeypatch)
        manager._update_home_tracking()

        with open(manager._home_config, encoding='utf-8') as f:
            data = json.load(f)

        assert data["lastMindspace"] == ms2
        assert data["recentMindspaces"] == [ms1]

    def test_preserves_existing_recent(self, manager, tmp_path, monkeypatch):
        """Switching mindspaces preserves existing recent entries."""
        ms1 = _create_mindspace_dir(str(tmp_path), "alpha")
        ms2 = _create_mindspace_dir(str(tmp_path), "beta")
        ms3 = _create_mindspace_dir(str(tmp_path), "gamma")

        # Start with ms1 and an existing recent list containing ms2 and ms3.
        with open(manager._home_config, 'w', encoding='utf-8') as f:
            json.dump({
                "lastMindspace": ms1,
                "recentMindspaces": [ms2, ms3],
            }, f)

        # Switch to ms2.
        _set_current(manager, ms2, monkeypatch)
        manager._update_home_tracking()

        with open(manager._home_config, encoding='utf-8') as f:
            data = json.load(f)

        assert data["lastMindspace"] == ms2
        # ms1 should be promoted to front, ms3 should still be there.
        assert data["recentMindspaces"] == [ms1, ms3]

    def test_excludes_current_from_recent(self, manager, tmp_path, monkeypatch):
        """The current mindspace is never included in the recent list."""
        ms1 = _create_mindspace_dir(str(tmp_path), "alpha")
        ms2 = _create_mindspace_dir(str(tmp_path), "beta")

        # Start with ms1, recent contains [ms2].
        with open(manager._home_config, 'w', encoding='utf-8') as f:
            json.dump({
                "lastMindspace": ms1,
                "recentMindspaces": [ms2],
            }, f)

        # Switch to ms2.
        _set_current(manager, ms2, monkeypatch)
        manager._update_home_tracking()

        with open(manager._home_config, encoding='utf-8') as f:
            data = json.load(f)

        assert data["lastMindspace"] == ms2
        assert data["recentMindspaces"] == [ms1]

    def test_prunes_stale_on_write(self, manager, tmp_path, monkeypatch):
        """_update_home_tracking() prunes stale paths from the recent list."""
        ms1 = _create_mindspace_dir(str(tmp_path), "alpha")
        stale = os.path.join(str(tmp_path), "deleted")

        with open(manager._home_config, 'w', encoding='utf-8') as f:
            json.dump({
                "lastMindspace": ms1,
                "recentMindspaces": [stale],
            }, f)

        _set_current(manager, ms1, monkeypatch)
        manager._update_home_tracking()

        with open(manager._home_config, encoding='utf-8') as f:
            data = json.load(f)

        assert stale not in data["recentMindspaces"]

    def test_caps_recent_at_max(self, manager, tmp_path, monkeypatch):
        """_update_home_tracking() caps the recent list at MAX_RECENT_MINDSPACES."""
        ms_current = _create_mindspace_dir(str(tmp_path), "current")
        paths = []
        for i in range(manager.MAX_RECENT_MINDSPACES + 5):
            p = _create_mindspace_dir(str(tmp_path), f"ms{i}")
            paths.append(p)

        with open(manager._home_config, 'w', encoding='utf-8') as f:
            json.dump({
                "lastMindspace": paths[0],
                "recentMindspaces": paths[1:],
            }, f)

        _set_current(manager, ms_current, monkeypatch)
        manager._update_home_tracking()

        with open(manager._home_config, encoding='utf-8') as f:
            data = json.load(f)

        assert len(data["recentMindspaces"]) == manager.MAX_RECENT_MINDSPACES

    def test_handles_no_previous_config(self, manager, tmp_path, monkeypatch):
        """_update_home_tracking() works when no config file exists yet."""
        ms1 = _create_mindspace_dir(str(tmp_path), "alpha")

        _set_current(manager, ms1, monkeypatch)
        manager._update_home_tracking()

        with open(manager._home_config, encoding='utf-8') as f:
            data = json.load(f)

        assert data["lastMindspace"] == ms1
        assert data["recentMindspaces"] == []

    def test_deduplicates_recent_on_write(self, manager, tmp_path, monkeypatch):
        """_update_home_tracking() deduplicates the recent list."""
        ms1 = _create_mindspace_dir(str(tmp_path), "alpha")
        ms2 = _create_mindspace_dir(str(tmp_path), "beta")

        with open(manager._home_config, 'w', encoding='utf-8') as f:
            json.dump({
                "lastMindspace": ms1,
                "recentMindspaces": [ms2, ms2, ms2],
            }, f)

        _set_current(manager, ms2, monkeypatch)
        manager._update_home_tracking()

        with open(manager._home_config, encoding='utf-8') as f:
            data = json.load(f)

        assert data["recentMindspaces"] == [ms1]


class TestGetLastMindspace:
    """Tests for the get_last_mindspace() method."""

    def test_returns_none_when_no_config(self, manager):
        """get_last_mindspace() returns None when no config file exists."""
        assert manager.get_last_mindspace() is None

    def test_returns_path(self, manager, tmp_path):
        """get_last_mindspace() returns the last opened mindspace path."""
        ms1 = _create_mindspace_dir(str(tmp_path), "alpha")

        with open(manager._home_config, 'w', encoding='utf-8') as f:
            json.dump({"lastMindspace": ms1}, f)

        assert manager.get_last_mindspace() == ms1

    def test_returns_none_for_stale_path(self, manager, tmp_path):
        """get_last_mindspace() returns None when the path no longer exists."""
        stale = os.path.join(str(tmp_path), "deleted")

        with open(manager._home_config, 'w', encoding='utf-8') as f:
            json.dump({"lastMindspace": stale}, f)

        assert manager.get_last_mindspace() is None

    def test_returns_none_for_empty_path(self, manager):
        """get_last_mindspace() returns None when lastMindspace is empty."""
        with open(manager._home_config, 'w', encoding='utf-8') as f:
            json.dump({"lastMindspace": ""}, f)

        assert manager.get_last_mindspace() is None

    def test_returns_none_for_invalid_json(self, manager):
        """get_last_mindspace() returns None when the config is invalid JSON."""
        with open(manager._home_config, 'w', encoding='utf-8') as f:
            f.write("not json")

        assert manager.get_last_mindspace() is None


class TestPinnedPaths:
    """Tests for is_path_pinned() / set_path_pinned() / migrate_pinned_path() / unpin_path_tree()."""

    @pytest.fixture
    def opened(self, manager, tmp_path):
        """Create and open a real mindspace, returning (manager, conversations_dir)."""
        ms_path = str(tmp_path / "mindspace")
        manager.create_mindspace(ms_path, [])
        manager.open_mindspace(ms_path)
        conv_dir = manager.mindspace().conversations_dir()
        return manager, conv_dir

    def test_not_pinned_by_default(self, opened):
        """A path with no pin entry is not pinned."""
        mgr, conv_dir = opened
        chat = os.path.join(conv_dir, "chat.conv")

        assert mgr.is_path_pinned(chat) is False

    def test_pin_then_unpin(self, opened):
        """set_path_pinned() toggles pin state and persists it."""
        mgr, conv_dir = opened
        chat = os.path.join(conv_dir, "chat.conv")

        mgr.set_path_pinned(chat, True)
        assert mgr.is_path_pinned(chat) is True

        mgr.set_path_pinned(chat, False)
        assert mgr.is_path_pinned(chat) is False

    def test_pin_persists_to_settings_file(self, opened):
        """Pinning a path writes it to settings.json under conversation.pinned."""
        mgr, conv_dir = opened
        chat = os.path.join(conv_dir, "chat.conv")

        mgr.set_path_pinned(chat, True)

        settings_path = os.path.join(mgr.mindspace_path(), ".humbug", "settings.json")
        with open(settings_path, encoding='utf-8') as f:
            data = json.load(f)

        assert data["conversation"]["pinned"] == [mgr.get_mindspace_relative_path(chat)]

    def test_pin_is_idempotent(self, opened):
        """Pinning an already-pinned path does not duplicate the entry."""
        mgr, conv_dir = opened
        chat = os.path.join(conv_dir, "chat.conv")

        mgr.set_path_pinned(chat, True)
        mgr.set_path_pinned(chat, True)

        assert mgr.settings().pinned_paths.count(mgr.get_mindspace_relative_path(chat)) == 1

    def test_pinned_paths_survive_reload(self, opened):
        """Pin state is still present after reloading settings from disk."""
        mgr, conv_dir = opened
        chat = os.path.join(conv_dir, "chat.conv")
        mgr.set_path_pinned(chat, True)

        settings_path = os.path.join(mgr.mindspace_path(), ".humbug", "settings.json")
        reloaded = MindspaceSettings.load(settings_path)

        assert mgr.get_mindspace_relative_path(chat) in reloaded.pinned_paths

    def test_migrate_pinned_path_on_rename(self, opened):
        """migrate_pinned_path() moves a pinned entry from old path to new path."""
        mgr, conv_dir = opened
        old_chat = os.path.join(conv_dir, "old.conv")
        new_chat = os.path.join(conv_dir, "new.conv")
        mgr.set_path_pinned(old_chat, True)

        mgr.migrate_pinned_path(old_chat, new_chat)

        assert mgr.is_path_pinned(old_chat) is False
        assert mgr.is_path_pinned(new_chat) is True

    def test_migrate_pinned_path_for_nested_child(self, opened):
        """migrate_pinned_path() rewrites pinned entries nested under a renamed folder."""
        mgr, conv_dir = opened
        old_folder = os.path.join(conv_dir, "old_folder")
        new_folder = os.path.join(conv_dir, "new_folder")
        # Folders (unlike conversation files) can be pinned at any depth.
        nested_subfolder_old = os.path.join(old_folder, "pinned_subfolder")
        nested_subfolder_new = os.path.join(new_folder, "pinned_subfolder")
        os.makedirs(nested_subfolder_old)
        mgr.set_path_pinned(nested_subfolder_old, True)

        mgr.migrate_pinned_path(old_folder, new_folder)

        assert mgr.is_path_pinned(nested_subfolder_old) is False
        assert mgr.is_path_pinned(nested_subfolder_new) is True

    def test_migrate_pinned_path_noop_when_unrelated(self, opened):
        """migrate_pinned_path() does not touch unrelated pinned entries."""
        mgr, conv_dir = opened
        pinned_chat = os.path.join(conv_dir, "pinned.conv")
        other_old = os.path.join(conv_dir, "other_old.conv")
        other_new = os.path.join(conv_dir, "other_new.conv")
        mgr.set_path_pinned(pinned_chat, True)

        mgr.migrate_pinned_path(other_old, other_new)

        assert mgr.is_path_pinned(pinned_chat) is True

    def test_unpin_path_tree_removes_pinned_file(self, opened):
        """unpin_path_tree() removes the pin entry for a deleted file."""
        mgr, conv_dir = opened
        chat = os.path.join(conv_dir, "chat.conv")
        mgr.set_path_pinned(chat, True)

        mgr.unpin_path_tree(chat)

        assert mgr.is_path_pinned(chat) is False

    def test_unpin_path_tree_removes_nested_children(self, opened):
        """unpin_path_tree() removes pinned entries nested under a deleted folder."""
        mgr, conv_dir = opened
        folder = os.path.join(conv_dir, "folder")
        # Folders (unlike conversation files) can be pinned at any depth.
        nested_subfolder = os.path.join(folder, "pinned_subfolder")
        os.makedirs(nested_subfolder)
        mgr.set_path_pinned(nested_subfolder, True)

        mgr.unpin_path_tree(folder)

        assert mgr.is_path_pinned(nested_subfolder) is False

    def test_unpin_path_tree_noop_when_nothing_pinned(self, opened):
        """unpin_path_tree() does not rewrite settings when nothing is pinned there."""
        mgr, conv_dir = opened
        chat = os.path.join(conv_dir, "chat.conv")
        settings_before = mgr.settings()

        mgr.unpin_path_tree(chat)

        # Same settings object — update_settings() was never called.
        assert mgr.settings() is settings_before

    def test_can_pin_path_true_for_top_level_conversation(self, opened):
        """A conversation directly in the conversations root can be pinned."""
        mgr, conv_dir = opened
        chat = os.path.join(conv_dir, "chat.conv")

        assert mgr.can_pin_path(chat) is True

    def test_can_pin_path_true_for_nested_conversation(self, opened):
        """A conversation nested inside a folder can be pinned — it moves with the section."""
        mgr, conv_dir = opened
        folder = os.path.join(conv_dir, "folder")
        os.makedirs(folder)
        nested_chat = os.path.join(folder, "chat.conv")

        assert mgr.can_pin_path(nested_chat) is True

    def test_can_pin_path_true_for_folder_at_any_depth(self, opened):
        """Folders can be pinned regardless of nesting depth."""
        mgr, conv_dir = opened
        top_folder = os.path.join(conv_dir, "top_folder")
        nested_folder = os.path.join(top_folder, "nested_folder")
        os.makedirs(nested_folder)

        assert mgr.can_pin_path(top_folder) is True
        assert mgr.can_pin_path(nested_folder) is True

    def test_can_pin_path_false_for_delegate_child(self, opened):
        """A fork/delegate child conversation cannot be pinned."""
        mgr, conv_dir = opened
        child = os.path.join(conv_dir, "child.conv")
        _write_conv_file(child, parent={"message_id": "m1", "tool_call_id": None})

        assert mgr.can_pin_path(child) is False

    def test_can_pin_path_true_for_root_conversation_file(self, opened):
        """A root conversation (no delegate parent) can be pinned."""
        mgr, conv_dir = opened
        root = os.path.join(conv_dir, "root.conv")
        _write_conv_file(root, parent=None)

        assert mgr.can_pin_path(root) is True

    def test_set_path_pinned_ignores_delegate_child(self, opened):
        """set_path_pinned() silently refuses to pin a delegate/fork child."""
        mgr, conv_dir = opened
        child = os.path.join(conv_dir, "child.conv")
        _write_conv_file(child, parent={"message_id": "m1", "tool_call_id": None})

        mgr.set_path_pinned(child, True)

        assert mgr.is_path_pinned(child) is False

    def test_set_path_pinned_allows_unpinning_delegate_child(self, opened):
        """Unpinning a delegate child is always allowed, even though pinning it is not."""
        mgr, conv_dir = opened
        child = os.path.join(conv_dir, "child.conv")
        _write_conv_file(child, parent={"message_id": "m1", "tool_call_id": None})
        # Simulate a legacy pinned entry that predates the delegate-child restriction.
        mgr.settings().pinned_paths.append(mgr.get_mindspace_relative_path(child))

        mgr.set_path_pinned(child, False)

        assert mgr.is_path_pinned(child) is False


class TestSoleConversationFolderPromotion:
    """Tests for set_path_pinned()'s sole-child-folder promotion."""

    @pytest.fixture
    def opened(self, manager, tmp_path):
        """Create and open a real mindspace, returning (manager, conversations_dir)."""
        ms_path = str(tmp_path / "mindspace")
        manager.create_mindspace(ms_path, [])
        manager.open_mindspace(ms_path)
        conv_dir = manager.mindspace().conversations_dir()
        return manager, conv_dir

    def test_pinning_sole_conversation_promotes_to_folder(self, opened):
        """Pinning the only conversation in a folder pins the folder instead."""
        mgr, conv_dir = opened
        folder = os.path.join(conv_dir, "folder")
        os.makedirs(folder)
        chat = os.path.join(folder, "chat.conv")
        _write_conv_file(chat, parent=None)

        mgr.set_path_pinned(chat, True)

        assert mgr.is_path_pinned(folder) is True
        assert mgr.is_path_pinned(chat) is False
        assert mgr.pinned_root_paths() == [folder]

    def test_pinning_one_of_several_conversations_does_not_promote(self, opened):
        """Pinning one of several conversations in a folder pins only that file."""
        mgr, conv_dir = opened
        folder = os.path.join(conv_dir, "folder")
        os.makedirs(folder)
        chat_a = os.path.join(folder, "a.conv")
        chat_b = os.path.join(folder, "b.conv")
        _write_conv_file(chat_a, parent=None)
        _write_conv_file(chat_b, parent=None)

        mgr.set_path_pinned(chat_a, True)

        assert mgr.is_path_pinned(chat_a) is True
        assert mgr.is_path_pinned(folder) is False

    def test_no_promotion_for_top_level_conversation(self, opened):
        """A conversation directly in the conversations root is never promoted."""
        mgr, conv_dir = opened
        chat = os.path.join(conv_dir, "chat.conv")
        _write_conv_file(chat, parent=None)

        mgr.set_path_pinned(chat, True)

        assert mgr.is_path_pinned(chat) is True

    def test_promotion_is_decided_once_at_pin_time(self, opened):
        """Adding a sibling later does not retroactively un-promote the folder."""
        mgr, conv_dir = opened
        folder = os.path.join(conv_dir, "folder")
        os.makedirs(folder)
        chat = os.path.join(folder, "chat.conv")
        _write_conv_file(chat, parent=None)
        mgr.set_path_pinned(chat, True)

        # A sibling conversation appears in the folder after the fact.
        _write_conv_file(os.path.join(folder, "sibling.conv"), parent=None)

        assert mgr.is_path_pinned(folder) is True


class TestFolderPinCompletionAndCascade:
    """Tests for promoting on completion, and cascading unpin to a folder's children."""

    @pytest.fixture
    def opened(self, manager, tmp_path):
        """Create and open a real mindspace, returning (manager, conversations_dir)."""
        ms_path = str(tmp_path / "mindspace")
        manager.create_mindspace(ms_path, [])
        manager.open_mindspace(ms_path)
        conv_dir = manager.mindspace().conversations_dir()
        return manager, conv_dir

    def test_pinning_the_last_unpinned_conversation_promotes_the_folder(self, opened):
        """Pinning every conversation in a folder, one by one, promotes it on the last pin."""
        mgr, conv_dir = opened
        folder = os.path.join(conv_dir, "folder")
        os.makedirs(folder)
        chat_a = os.path.join(folder, "a.conv")
        chat_b = os.path.join(folder, "b.conv")
        _write_conv_file(chat_a, parent=None)
        _write_conv_file(chat_b, parent=None)

        mgr.set_path_pinned(chat_a, True)
        assert mgr.is_path_pinned(folder) is False  # not yet — b is still unpinned

        mgr.set_path_pinned(chat_b, True)

        assert mgr.is_path_pinned(folder) is True
        assert mgr.pinned_root_paths() == [folder]

    def test_promotion_drops_the_now_redundant_individual_entries(self, opened):
        """Promoting to a folder pin removes the individual conversation entries."""
        mgr, conv_dir = opened
        folder = os.path.join(conv_dir, "folder")
        os.makedirs(folder)
        chat_a = os.path.join(folder, "a.conv")
        chat_b = os.path.join(folder, "b.conv")
        _write_conv_file(chat_a, parent=None)
        _write_conv_file(chat_b, parent=None)
        mgr.set_path_pinned(chat_a, True)
        mgr.set_path_pinned(chat_b, True)

        pinned = mgr.settings().pinned_paths
        assert mgr.get_mindspace_relative_path(folder) in pinned
        assert mgr.get_mindspace_relative_path(chat_a) not in pinned
        assert mgr.get_mindspace_relative_path(chat_b) not in pinned

    def test_unpinning_a_folder_also_unpins_its_conversations(self, opened):
        """Unpinning a folder cascades to unpin any of its still-individually-pinned children."""
        mgr, conv_dir = opened
        folder = os.path.join(conv_dir, "folder")
        os.makedirs(folder)
        chat_a = os.path.join(folder, "a.conv")
        chat_b = os.path.join(folder, "b.conv")
        _write_conv_file(chat_a, parent=None)
        _write_conv_file(chat_b, parent=None)
        # Simulate stale/redundant state: folder and a child both marked pinned.
        mgr.set_path_pinned(chat_a, True)
        mgr.settings().pinned_paths.append(mgr.get_mindspace_relative_path(folder))

        mgr.set_path_pinned(folder, False)

        assert mgr.is_path_pinned(folder) is False
        assert mgr.is_path_pinned(chat_a) is False
        assert mgr.settings().pinned_paths == []

    def test_pinning_a_folder_directly_prunes_stale_child_entries(self, opened):
        """Directly pinning a folder also cleans up any redundant individual child pins."""
        mgr, conv_dir = opened
        folder = os.path.join(conv_dir, "folder")
        os.makedirs(folder)
        chat_a = os.path.join(folder, "a.conv")
        _write_conv_file(chat_a, parent=None)
        mgr.set_path_pinned(chat_a, True)

        mgr.set_path_pinned(folder, True)

        assert mgr.settings().pinned_paths == [mgr.get_mindspace_relative_path(folder)]

    def test_unpinning_folder_does_not_touch_unrelated_pins(self, opened):
        """Unpinning a folder leaves pins outside that folder untouched."""
        mgr, conv_dir = opened
        folder = os.path.join(conv_dir, "folder")
        os.makedirs(folder)
        chat_a = os.path.join(folder, "a.conv")
        _write_conv_file(chat_a, parent=None)
        other = os.path.join(conv_dir, "other.conv")
        _write_conv_file(other, parent=None)
        mgr.set_path_pinned(folder, True)
        mgr.set_path_pinned(other, True)

        mgr.set_path_pinned(folder, False)

        assert mgr.is_path_pinned(other) is True


class TestFolderPinDepromotion:
    """Tests for de-promotion: unpinning one chat out of an already-pinned folder."""

    @pytest.fixture
    def opened(self, manager, tmp_path):
        """Create and open a real mindspace, returning (manager, conversations_dir)."""
        ms_path = str(tmp_path / "mindspace")
        manager.create_mindspace(ms_path, [])
        manager.open_mindspace(ms_path)
        conv_dir = manager.mindspace().conversations_dir()
        return manager, conv_dir

    def test_unpinning_one_chat_depromotes_the_folder(self, opened):
        """Unpinning one chat from a pinned folder unpins the folder and re-pins the rest."""
        mgr, conv_dir = opened
        folder = os.path.join(conv_dir, "folder")
        os.makedirs(folder)
        chat_a = os.path.join(folder, "a.conv")
        chat_b = os.path.join(folder, "b.conv")
        chat_c = os.path.join(folder, "c.conv")
        for chat in (chat_a, chat_b, chat_c):
            _write_conv_file(chat, parent=None)
        mgr.set_path_pinned(folder, True)

        mgr.set_path_pinned(chat_a, False)

        assert mgr.is_path_pinned(folder) is False
        assert mgr.is_path_pinned(chat_a) is False
        assert mgr.is_path_pinned(chat_b) is True
        assert mgr.is_path_pinned(chat_c) is True

    def test_effectively_pinned_chat_shows_as_unpin_before_depromotion(self, opened):
        """A chat covered only by its folder's pin is effectively pinned, even though its own path isn't stored."""
        mgr, conv_dir = opened
        folder = os.path.join(conv_dir, "folder")
        os.makedirs(folder)
        chat = os.path.join(folder, "chat.conv")
        _write_conv_file(chat, parent=None)
        mgr.set_path_pinned(folder, True)

        assert mgr.is_path_pinned(chat) is False
        assert mgr.is_effectively_pinned(chat) is True

    def test_unpinning_the_last_covered_chat_leaves_nothing_pinned(self, opened):
        """Unpinning the only chat under a pinned folder leaves the folder unpinned with nothing re-added."""
        mgr, conv_dir = opened
        folder = os.path.join(conv_dir, "folder")
        os.makedirs(folder)
        chat = os.path.join(folder, "chat.conv")
        _write_conv_file(chat, parent=None)
        mgr.set_path_pinned(folder, True)

        mgr.set_path_pinned(chat, False)

        assert mgr.settings().pinned_paths == []

    def test_unpinning_every_chat_one_at_a_time_ends_with_nothing_pinned(self, opened):
        """Unpinning every chat in a pinned folder one by one ends with the folder unpinned."""
        mgr, conv_dir = opened
        folder = os.path.join(conv_dir, "folder")
        os.makedirs(folder)
        chat_a = os.path.join(folder, "a.conv")
        chat_b = os.path.join(folder, "b.conv")
        _write_conv_file(chat_a, parent=None)
        _write_conv_file(chat_b, parent=None)
        mgr.set_path_pinned(folder, True)

        mgr.set_path_pinned(chat_a, False)
        assert mgr.is_effectively_pinned(chat_b) is True

        mgr.set_path_pinned(chat_b, False)

        assert mgr.settings().pinned_paths == []
        assert mgr.is_effectively_pinned(chat_a) is False
        assert mgr.is_effectively_pinned(chat_b) is False

    def test_depromotion_ignores_delegate_children(self, opened):
        """De-promotion never re-pins a delegate/fork child conversation."""
        mgr, conv_dir = opened
        folder = os.path.join(conv_dir, "folder")
        os.makedirs(folder)
        chat_a = os.path.join(folder, "a.conv")
        delegate_chat = os.path.join(folder, "delegate.conv")
        _write_conv_file(chat_a, parent=None)
        _write_conv_file(delegate_chat, parent={"message_id": "m1", "tool_call_id": None})
        mgr.set_path_pinned(folder, True)

        mgr.set_path_pinned(chat_a, False)

        assert mgr.is_path_pinned(delegate_chat) is False
        assert mgr.settings().pinned_paths == []

    def test_unpinning_an_unrelated_chat_does_not_depromote(self, opened):
        """Unpinning a conversation with no pinned ancestor is a plain no-op, not a de-promotion."""
        mgr, conv_dir = opened
        folder = os.path.join(conv_dir, "folder")
        os.makedirs(folder)
        chat = os.path.join(folder, "chat.conv")
        _write_conv_file(chat, parent=None)
        other = os.path.join(conv_dir, "other.conv")
        _write_conv_file(other, parent=None)
        mgr.set_path_pinned(other, True)
        settings_before = mgr.settings()

        mgr.set_path_pinned(chat, False)

        assert mgr.settings() is settings_before


class TestFolderHasPinnedContent:
    """Tests for folder_has_pinned_content()."""

    @pytest.fixture
    def opened(self, manager, tmp_path):
        """Create and open a real mindspace, returning (manager, conversations_dir)."""
        ms_path = str(tmp_path / "mindspace")
        manager.create_mindspace(ms_path, [])
        manager.open_mindspace(ms_path)
        conv_dir = manager.mindspace().conversations_dir()
        return manager, conv_dir

    def test_false_when_nothing_pinned(self, opened):
        """folder_has_pinned_content() is False for an untouched folder."""
        mgr, conv_dir = opened
        folder = os.path.join(conv_dir, "folder")
        os.makedirs(folder)

        assert mgr.folder_has_pinned_content(folder) is False

    def test_true_when_the_folder_itself_is_pinned(self, opened):
        """folder_has_pinned_content() is True when the folder's own path is pinned."""
        mgr, conv_dir = opened
        folder = os.path.join(conv_dir, "folder")
        os.makedirs(folder)
        mgr.set_path_pinned(folder, True)

        assert mgr.folder_has_pinned_content(folder) is True

    def test_true_when_only_a_child_conversation_is_pinned(self, opened):
        """folder_has_pinned_content() is True when just one conversation inside is pinned."""
        mgr, conv_dir = opened
        folder = os.path.join(conv_dir, "folder")
        os.makedirs(folder)
        chat_a = os.path.join(folder, "a.conv")
        chat_b = os.path.join(folder, "b.conv")
        _write_conv_file(chat_a, parent=None)
        _write_conv_file(chat_b, parent=None)
        mgr.set_path_pinned(chat_a, True)

        assert mgr.folder_has_pinned_content(folder) is True

    def test_false_after_full_unpin(self, opened):
        """folder_has_pinned_content() returns to False once nothing inside is pinned any more."""
        mgr, conv_dir = opened
        folder = os.path.join(conv_dir, "folder")
        os.makedirs(folder)
        chat = os.path.join(folder, "chat.conv")
        _write_conv_file(chat, parent=None)
        mgr.set_path_pinned(chat, True)
        mgr.set_path_pinned(chat, False)

        assert mgr.folder_has_pinned_content(folder) is False


class TestPinnedRootPaths:
    """Tests for pinned_root_paths()."""

    @pytest.fixture
    def opened(self, manager, tmp_path):
        """Create and open a real mindspace, returning (manager, conversations_dir)."""
        ms_path = str(tmp_path / "mindspace")
        manager.create_mindspace(ms_path, [])
        manager.open_mindspace(ms_path)
        conv_dir = manager.mindspace().conversations_dir()
        return manager, conv_dir

    def test_empty_when_nothing_pinned(self, opened):
        """pinned_root_paths() returns [] when nothing is pinned."""
        mgr, _conv_dir = opened

        assert mgr.pinned_root_paths() == []

    def test_returns_pinned_folder_and_file(self, opened):
        """pinned_root_paths() returns both pinned folders and pinned files."""
        mgr, conv_dir = opened
        folder = os.path.join(conv_dir, "folder")
        os.makedirs(folder)
        chat = os.path.join(conv_dir, "chat.conv")
        _write_conv_file(chat, parent=None)
        mgr.set_path_pinned(folder, True)
        mgr.set_path_pinned(chat, True)

        roots = set(mgr.pinned_root_paths())

        assert roots == {folder, chat}

    def test_nested_pinned_entry_is_not_a_second_root(self, opened):
        """A pinned entry nested under another pinned entry is not its own root."""
        mgr, conv_dir = opened
        folder = os.path.join(conv_dir, "folder")
        nested_chat = os.path.join(folder, "chat.conv")
        os.makedirs(folder)
        _write_conv_file(nested_chat, parent=None)
        mgr.set_path_pinned(folder, True)
        mgr.set_path_pinned(nested_chat, True)

        roots = mgr.pinned_root_paths()

        assert roots == [folder]

    def test_skips_stale_entries(self, opened):
        """pinned_root_paths() skips pinned entries that no longer exist on disk."""
        mgr, conv_dir = opened
        chat = os.path.join(conv_dir, "chat.conv")
        _write_conv_file(chat, parent=None)
        mgr.set_path_pinned(chat, True)
        os.remove(chat)

        assert mgr.pinned_root_paths() == []

    def test_returns_absolute_paths(self, opened):
        """pinned_root_paths() returns absolute filesystem paths."""
        mgr, conv_dir = opened
        chat = os.path.join(conv_dir, "chat.conv")
        _write_conv_file(chat, parent=None)
        mgr.set_path_pinned(chat, True)

        roots = mgr.pinned_root_paths()

        assert roots == [chat]
        assert os.path.isabs(roots[0])