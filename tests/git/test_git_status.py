"""Tests for the staged/unstaged split in git status parsing."""

import os
import shutil
import subprocess

import pytest

from git import VCSStatusCode, get_status, stage_files


pytestmark = pytest.mark.skipif(
    shutil.which("git") is None, reason="git executable not available"
)


def _git(cwd, *args):
    subprocess.run(["git"] + list(args), cwd=cwd, check=True, capture_output=True, text=True)


def _write(path, content):
    with open(path, "w", encoding="utf-8") as f:
        f.write(content)


@pytest.fixture
def repo(tmp_path):
    root = str(tmp_path / "repo")
    os.makedirs(root)
    _git(root, "init")
    _git(root, "config", "user.email", "t@e.com")
    _git(root, "config", "user.name", "T")
    _git(root, "checkout", "-b", "main")
    _write(os.path.join(root, "README.md"), "hello\n")
    _git(root, "add", "README.md")
    _git(root, "commit", "-m", "init")
    return root


def _entry(repo_root, name):
    target = os.path.normpath(os.path.join(repo_root, name))
    for e in get_status(repo_root, repo_root):
        if os.path.normpath(e.path) == target:
            return e

    return None


def test_untracked_is_worktree_only(repo):
    _write(os.path.join(repo, "new.txt"), "x\n")
    e = _entry(repo, "new.txt")
    assert e is not None
    assert e.index_code is None
    assert e.worktree_code == VCSStatusCode.UNTRACKED
    assert e.is_unstaged() and not e.is_staged()


def test_staged_modification_is_index_only(repo):
    _write(os.path.join(repo, "README.md"), "hello\nmore\n")
    stage_files(repo, [os.path.join(repo, "README.md")])
    e = _entry(repo, "README.md")
    assert e is not None
    assert e.index_code == VCSStatusCode.MODIFIED
    assert e.worktree_code is None
    assert e.is_staged() and not e.is_unstaged()


def test_staged_then_modified_is_both(repo):
    readme = os.path.join(repo, "README.md")
    _write(readme, "hello\nstaged\n")
    stage_files(repo, [readme])
    _write(readme, "hello\nstaged\nunstaged\n")
    e = _entry(repo, "README.md")
    assert e is not None
    assert e.index_code == VCSStatusCode.MODIFIED
    assert e.worktree_code == VCSStatusCode.MODIFIED
    assert e.is_staged() and e.is_unstaged()


def test_deleted_file(repo):
    os.remove(os.path.join(repo, "README.md"))
    e = _entry(repo, "README.md")
    assert e is not None
    assert e.worktree_code == VCSStatusCode.DELETED
