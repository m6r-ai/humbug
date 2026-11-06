"""Tests for the saved settings of a mindspace."""

import json

from mindspace.mindspace_scan_pruned_paths import DEFAULT_SCAN_PRUNED_PATHS
from mindspace.mindspace_settings import MindspaceSettings


def _settings_file(tmp_path, data):
    path = tmp_path / "settings.json"
    path.write_text(json.dumps(data), encoding="utf-8")
    return str(path)


def _loaded_changes(tmp_path, changes):
    return MindspaceSettings.load(_settings_file(tmp_path, {"changes": changes}))


def test_a_mindspace_with_no_saved_paths_is_scanned_with_the_defaults(tmp_path):
    settings = _loaded_changes(tmp_path, {})

    assert settings.changes_pruned_paths == sorted(DEFAULT_SCAN_PRUNED_PATHS)


def test_a_saved_path_list_is_used_as_saved(tmp_path):
    settings = _loaded_changes(tmp_path, {"prunedPaths": ["build", "src/generated"]})

    assert settings.changes_pruned_paths == ["build", "src/generated"]


def test_a_saved_empty_list_means_the_mindspace_prunes_nothing_extra(tmp_path):
    settings = _loaded_changes(tmp_path, {"prunedPaths": []})

    assert settings.changes_pruned_paths == []


def test_a_saved_list_that_is_not_a_list_falls_back_to_the_defaults(tmp_path):
    settings = _loaded_changes(tmp_path, {"prunedPaths": "build"})

    assert settings.changes_pruned_paths == sorted(DEFAULT_SCAN_PRUNED_PATHS)


def test_non_string_entries_are_dropped_from_a_saved_list(tmp_path):
    settings = _loaded_changes(tmp_path, {"prunedPaths": ["build", 7, None, " htmlcov "]})

    assert settings.changes_pruned_paths == ["build", " htmlcov "]


def test_the_scanned_paths_round_trip_through_a_save(tmp_path):
    path = _settings_file(tmp_path, {})
    settings = MindspaceSettings(["filesystem"])
    settings.changes_pruned_paths = ["build", "src/generated"]
    settings.changes_checkpoint_interval = 30.0

    settings.save(path)

    reloaded = MindspaceSettings.load(path)
    assert reloaded.changes_pruned_paths == ["build", "src/generated"]
    assert reloaded.changes_checkpoint_interval == 30.0


def test_a_new_mindspace_starts_with_the_defaults(tmp_path):
    settings = MindspaceSettings(["filesystem"])

    assert settings.changes_pruned_paths == sorted(DEFAULT_SCAN_PRUNED_PATHS)
    assert settings.changes_checkpoint_interval == 5.0
