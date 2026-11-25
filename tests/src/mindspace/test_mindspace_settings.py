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


def test_mission_control_settings_round_trip_through_a_save(tmp_path):
    path = _settings_file(tmp_path, {})
    settings = MindspaceSettings(["filesystem"])
    assert not settings.mission_resume_automatically and not settings.mission_reduce_motion

    settings.mission_resume_automatically = True
    settings.mission_reduce_motion = True
    settings.save(path)

    reloaded = MindspaceSettings.load(path)
    assert reloaded.mission_resume_automatically and reloaded.mission_reduce_motion


def test_mission_control_settings_of_the_wrong_type_fall_back_to_off(tmp_path):
    settings = MindspaceSettings.load(
        _settings_file(tmp_path, {"missions": {"resumeAutomatically": "yes", "reduceMotion": 1}})
    )

    assert not settings.mission_resume_automatically and not settings.mission_reduce_motion


def test_mission_templates_are_saved_and_loaded_in_order(tmp_path):
    """Saved templates come back as they were saved, in the same order."""
    path = str(tmp_path / "settings.json")
    settings = MindspaceSettings.load(_settings_file(tmp_path, {}))
    settings.mission_templates = [("Review a PR", "Review the open pull request"), ("Notes", "Write release notes")]
    settings.save(path)

    assert MindspaceSettings.load(path).mission_templates == [
        ("Review a PR", "Review the open pull request"), ("Notes", "Write release notes")
    ]


def test_a_mindspace_with_no_templates_has_none(tmp_path):
    """Settings saved before templates existed load with an empty list."""
    assert MindspaceSettings.load(_settings_file(tmp_path, {"missions": {}})).mission_templates == []


def test_malformed_templates_are_dropped(tmp_path):
    """A templates value that is not a list, and entries without a name or a request, are ignored."""
    assert MindspaceSettings.load(_settings_file(tmp_path, {"missions": {"templates": "x"}})).mission_templates == []
    entries = [
        {"name": "Good", "request": "Do it"}, {"name": "", "request": "x"}, {"name": "No request"}, "text", 3,
        {"name": 5, "request": "x"},
    ]
    loaded = MindspaceSettings.load(_settings_file(tmp_path, {"missions": {"templates": entries}}))
    assert loaded.mission_templates == [("Good", "Do it")]


def test_default_auto_approve_level_is_saved(tmp_path):
    """The level new missions start at is kept, and anything but off or safe falls back to off."""
    path = str(tmp_path / "settings.json")
    settings = MindspaceSettings.load(_settings_file(tmp_path, {}))
    assert settings.mission_default_auto_approve == "off"
    settings.mission_default_auto_approve = "safe"
    settings.save(path)
    assert MindspaceSettings.load(path).mission_default_auto_approve == "safe"

    for stored in ("all", "everything", 3):
        loaded = MindspaceSettings.load(_settings_file(tmp_path, {"missions": {"defaultAutoApprove": stored}}))
        assert loaded.mission_default_auto_approve == "off"
