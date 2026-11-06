"""Tests for the directories and paths the change scanner does not descend."""

from mindspace.mindspace_ignored_dirs import IGNORED_DIRS
from mindspace.mindspace_scan_pruned_paths import (
    DEFAULT_SCAN_PRUNED_PATHS, MANDATORY_SCAN_PRUNED_PATHS, scan_pruned_paths
)


def test_a_coverage_report_is_not_scanned():
    assert "htmlcov" in DEFAULT_SCAN_PRUNED_PATHS


def test_tool_caches_are_not_scanned():
    for name in (".mypy_cache", ".pytest_cache", ".ruff_cache", ".hypothesis", ".tox", ".nox"):
        assert name in DEFAULT_SCAN_PRUNED_PATHS


def test_everything_already_ignored_is_also_pruned():
    assert IGNORED_DIRS <= DEFAULT_SCAN_PRUNED_PATHS


def test_the_scan_and_the_project_do_not_hide_ordinary_source_directories():
    for name in ("src", "tests", "docs"):
        assert name not in DEFAULT_SCAN_PRUNED_PATHS


def test_a_mindspace_that_configures_nothing_still_cannot_scan_humbug():
    assert scan_pruned_paths([]) == MANDATORY_SCAN_PRUNED_PATHS


def test_the_mandatory_paths_cannot_be_removed_from_the_defaults():
    assert MANDATORY_SCAN_PRUNED_PATHS <= DEFAULT_SCAN_PRUNED_PATHS


def test_a_configured_entry_replaces_the_defaults_rather_than_adding_to_them():
    assert scan_pruned_paths(["generated"]) == frozenset({"generated"}) | MANDATORY_SCAN_PRUNED_PATHS


def test_a_path_from_the_mindspace_root_is_kept_whole():
    assert "src/generated" in scan_pruned_paths(["src/generated"])


def test_a_trailing_separator_does_not_change_an_entry():
    assert scan_pruned_paths(["build/"]) == scan_pruned_paths(["build"])


def test_a_leading_separator_does_not_change_an_entry():
    assert scan_pruned_paths(["/build"]) == scan_pruned_paths(["build"])


def test_a_windows_separator_is_read_as_a_path_boundary():
    assert "src/generated" in scan_pruned_paths(["src\\generated"])


def test_an_entry_that_climbs_out_of_the_mindspace_is_dropped():
    assert scan_pruned_paths(["../outside"]) == MANDATORY_SCAN_PRUNED_PATHS


def test_an_entry_that_climbs_out_of_the_mindspace_mid_path_is_dropped():
    assert scan_pruned_paths(["src/../../outside"]) == MANDATORY_SCAN_PRUNED_PATHS


def test_the_mindspace_root_itself_cannot_be_ignored():
    assert scan_pruned_paths(["."]) == MANDATORY_SCAN_PRUNED_PATHS


def test_blank_entries_are_dropped():
    assert scan_pruned_paths(["", "   ", "\t"]) == MANDATORY_SCAN_PRUNED_PATHS


def test_surrounding_whitespace_is_ignored():
    assert scan_pruned_paths(["  build  "]) == scan_pruned_paths(["build"])
