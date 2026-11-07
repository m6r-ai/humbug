"""Tests for matching a path against a list of glob entries."""

import os

from path_matching import PathPatterns


def _matches(path, entries):
    return PathPatterns(entries).matches(path)


def test_a_single_name_matches_at_any_depth():
    assert _matches("venv", ["venv"])
    assert _matches("a/b/venv", ["venv"])


def test_a_single_name_does_not_match_a_longer_name():
    assert not _matches("venvity", ["venv"])
    assert not _matches("myvenv", ["venv"])
    assert not _matches("venv-2", ["venv"])


def test_a_single_name_matches_only_as_a_whole_component():
    assert not _matches("a/venv/b.txt", ["venv"])


def test_a_path_matches_its_trailing_components():
    assert _matches("src/generated", ["src/generated"])
    assert _matches("x/src/generated", ["src/generated"])


def test_a_path_does_not_match_a_longer_path():
    assert not _matches("src/generatedly", ["src/generated"])


def test_a_path_alone_does_not_match_what_is_below_it():
    assert not _matches("src/generated/a.txt", ["src/generated"])


def test_a_trailing_double_wildcard_matches_the_base_and_what_is_below_it():
    assert _matches("build", ["build/**"])
    assert _matches("build/object.o", ["build/**"])
    assert not _matches("build-tmp/object.o", ["build/**"])


def test_a_wildcard_name_matches_at_any_depth():
    assert _matches(".notes.txt.swp", ["*.swp"])
    assert _matches("src/.notes.txt.swp", ["*.swp"])


def test_a_wildcard_name_matches_the_whole_name():
    assert not _matches("a.swpx", ["*.swp"])


def test_a_star_in_a_whole_path_pattern_crosses_a_separator():
    assert _matches("a/b/x.txt", ["*.txt"])


def test_an_absolute_entry_matches_at_the_root_and_nowhere_else():
    assert _matches("/etc/shadow", ["/etc/shadow"])
    assert not _matches("x/etc/shadow", ["/etc/shadow"])


def test_an_empty_list_matches_nothing():
    assert not _matches("anything", [])


def test_a_blank_entry_matches_nothing():
    assert not _matches("anything", ["", "venv"])


def test_matching_folds_case_only_on_windows():
    assert _matches("Venv", ["venv"]) == (os.name == "nt")


def test_a_wildcard_entry_folds_case_like_fnmatch():
    assert _matches("A.SWP", ["*.swp"]) == (os.name == "nt")
