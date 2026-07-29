"""Tests for MindspaceManager recent-mindspaces tracking."""

import json
import os

import pytest

# pylint: disable=wrong-import-position
from desktop.mindspace.mindspace_manager import MindspaceManager


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