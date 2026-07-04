"""Integration tests for repository initialisation and cloning."""

import os
import shutil
import subprocess

import pytest

from git import clone_repository, find_repositories, get_head_message, init_repository


pytestmark = pytest.mark.skipif(
    shutil.which("git") is None, reason="git executable not available"
)


def _git(cwd, *args):
    subprocess.run(["git"] + list(args), cwd=cwd, check=True, capture_output=True, text=True)


def _seed_repo(path):
    """Create a repository at *path* with a single committed file."""
    os.makedirs(path)
    _git(path, "init")
    _git(path, "config", "user.email", "test@example.com")
    _git(path, "config", "user.name", "Test User")
    with open(os.path.join(path, "a.txt"), "w", encoding="utf-8") as f:
        f.write("hello\n")
    _git(path, "add", ".")
    _git(path, "commit", "-m", "initial commit")


def test_init_creates_repository(tmp_path):
    root = str(tmp_path / "proj")
    os.makedirs(root)

    init_repository(root)

    assert os.path.isdir(os.path.join(root, ".git"))
    discovered = [os.path.normpath(p) for p in find_repositories(root)]
    assert os.path.normpath(root) in discovered


def test_init_repository_has_no_commits(tmp_path):
    root = str(tmp_path / "proj")
    os.makedirs(root)

    init_repository(root)

    assert get_head_message(root) == ""


def test_clone_copies_source_repo(tmp_path):
    src = str(tmp_path / "src")
    _seed_repo(src)

    dest_parent = str(tmp_path / "dest")
    os.makedirs(dest_parent)

    clone_repository(src, dest_parent)

    cloned = os.path.join(dest_parent, "src")
    assert os.path.isdir(os.path.join(cloned, ".git"))
    assert os.path.isfile(os.path.join(cloned, "a.txt"))
    assert get_head_message(cloned) == "initial commit"
