"""Tests for multi-repository discovery within a mindspace."""

import os

from git import find_repositories


def _mkrepo(path):
    """Create a fake repo (a directory containing a .git dir) at *path*."""
    os.makedirs(os.path.join(path, ".git"), exist_ok=True)


def test_finds_multiple_sibling_repos(tmp_path):
    root = str(tmp_path)
    _mkrepo(os.path.join(root, "projectA"))
    _mkrepo(os.path.join(root, "projectB"))
    os.makedirs(os.path.join(root, "notes"))  # not a repo

    repos = find_repositories(root)

    assert repos == sorted([
        os.path.join(root, "projectA"),
        os.path.join(root, "projectB"),
    ])


def test_includes_root_when_root_is_repo(tmp_path):
    root = str(tmp_path)
    _mkrepo(root)

    assert find_repositories(root) == [os.path.normpath(root)]


def test_surfaces_nested_repos(tmp_path):
    root = str(tmp_path)
    _mkrepo(os.path.join(root, "outer"))
    # A repo cloned inside another repo must be surfaced as its own repo.
    _mkrepo(os.path.join(root, "outer", "inner"))

    assert find_repositories(root) == sorted([
        os.path.join(root, "outer"),
        os.path.join(root, "outer", "inner"),
    ])


def test_surfaces_nested_repo_when_root_is_repo(tmp_path):
    # Mirrors the real case: mindspace is a repo containing a cloned project.
    root = str(tmp_path)
    _mkrepo(root)
    _mkrepo(os.path.join(root, "project-manager"))

    assert find_repositories(root) == sorted([
        os.path.normpath(root),
        os.path.join(root, "project-manager"),
    ])


def test_skips_heavy_directories(tmp_path):
    root = str(tmp_path)
    _mkrepo(os.path.join(root, "node_modules", "pkg"))
    _mkrepo(os.path.join(root, "real"))

    assert find_repositories(root) == [os.path.join(root, "real")]


def test_respects_max_depth(tmp_path):
    root = str(tmp_path)
    deep = os.path.join(root, "a", "b", "c", "d", "e")
    _mkrepo(deep)

    assert find_repositories(root, max_depth=2) == []
    assert find_repositories(root, max_depth=6) == [deep]


def test_empty_for_missing_directory(tmp_path):
    assert find_repositories(str(tmp_path / "does-not-exist")) == []
