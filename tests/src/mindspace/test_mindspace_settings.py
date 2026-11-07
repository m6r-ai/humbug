"""Tests for the saved settings of a mindspace."""

import json

from mindspace.mindspace_scan_pruned_paths import DEFAULT_SCAN_PRUNED_PATHS
from mindspace.mindspace_settings import MindspaceSettings


def _settings_file(tmp_path, data):
    path = tmp_path / "settings.json"
    path.write_text(json.dumps(data), encoding="utf-8")
    return str(path)


def _loaded_journal(tmp_path, journal):
    return MindspaceSettings.load(_settings_file(tmp_path, {"journal": journal}))


def test_a_mindspace_with_no_saved_paths_is_scanned_with_the_defaults(tmp_path):
    settings = _loaded_journal(tmp_path, {})

    assert settings.journal_pruned_paths == sorted(DEFAULT_SCAN_PRUNED_PATHS)


def test_a_saved_path_list_is_used_as_saved(tmp_path):
    settings = _loaded_journal(tmp_path, {"prunedPaths": ["build", "src/generated"]})

    assert settings.journal_pruned_paths == ["build", "src/generated"]


def test_a_saved_empty_list_means_the_mindspace_prunes_nothing_extra(tmp_path):
    settings = _loaded_journal(tmp_path, {"prunedPaths": []})

    assert settings.journal_pruned_paths == []


def test_a_saved_list_that_is_not_a_list_falls_back_to_the_defaults(tmp_path):
    settings = _loaded_journal(tmp_path, {"prunedPaths": "build"})

    assert settings.journal_pruned_paths == sorted(DEFAULT_SCAN_PRUNED_PATHS)


def test_non_string_entries_are_dropped_from_a_saved_list(tmp_path):
    settings = _loaded_journal(tmp_path, {"prunedPaths": ["build", 7, None, " htmlcov "]})

    assert settings.journal_pruned_paths == ["build", " htmlcov "]


def test_the_scanned_paths_round_trip_through_a_save(tmp_path):
    path = _settings_file(tmp_path, {})
    settings = MindspaceSettings(["filesystem"])
    settings.journal_pruned_paths = ["build", "src/generated"]
    settings.journal_checkpoint_interval = 30.0

    settings.save(path)

    reloaded = MindspaceSettings.load(path)
    assert reloaded.journal_pruned_paths == ["build", "src/generated"]
    assert reloaded.journal_checkpoint_interval == 30.0


def test_a_new_mindspace_starts_with_the_defaults(tmp_path):
    settings = MindspaceSettings(["filesystem"])

    assert settings.journal_pruned_paths == sorted(DEFAULT_SCAN_PRUNED_PATHS)
    assert settings.journal_checkpoint_interval == 5.0


def test_settings_saved_before_the_journal_was_renamed_are_still_read(tmp_path):
    settings = MindspaceSettings.load(
        _settings_file(
            tmp_path,
            {"changes": {"checkpointIntervalSeconds": 30.0, "prunedPaths": ["build"]}},
        )
    )

    assert settings.journal_checkpoint_interval == 30.0
    assert settings.journal_pruned_paths == ["build"]


def test_a_save_writes_the_journal_section(tmp_path):
    path = _settings_file(tmp_path, {})
    MindspaceSettings(["filesystem"]).save(path)

    with open(path, encoding="utf-8") as f:
        saved = json.load(f)

    assert "journal" in saved
