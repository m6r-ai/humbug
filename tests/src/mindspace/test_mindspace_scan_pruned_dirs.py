"""Tests for the directories the change scanner does not descend."""

from mindspace.mindspace_ignored_dirs import IGNORED_DIRS
from mindspace.mindspace_scan_pruned_dirs import SCAN_PRUNED_DIRS


def test_a_coverage_report_is_not_scanned():
    assert "htmlcov" in SCAN_PRUNED_DIRS


def test_tool_caches_are_not_scanned():
    for name in (".mypy_cache", ".pytest_cache", ".ruff_cache", ".hypothesis", ".tox", ".nox"):
        assert name in SCAN_PRUNED_DIRS


def test_everything_already_ignored_is_also_pruned():
    assert IGNORED_DIRS <= SCAN_PRUNED_DIRS


def test_the_scan_and_the_project_do_not_hide_ordinary_source_directories():
    for name in ("src", "tests", "docs"):
        assert name not in SCAN_PRUNED_DIRS
