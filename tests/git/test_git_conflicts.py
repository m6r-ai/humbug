"""Integration tests for conflict detection and resolution."""

import os
import shutil
import subprocess

import pytest

from git import (
    MergeState,
    VCSStatusCode,
    abort_merge,
    accept_ours,
    accept_theirs,
    get_merge_state,
    get_status,
    mark_resolved,
)


pytestmark = pytest.mark.skipif(
    shutil.which("git") is None, reason="git executable not available"
)


def _git(cwd, *args):
    subprocess.run(["git"] + list(args), cwd=cwd, check=True, capture_output=True, text=True)


def _write(path, content):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w", encoding="utf-8") as f:
        f.write(content)


def _read(path):
    with open(path, encoding="utf-8") as f:
        return f.read()


@pytest.fixture
def conflicted(tmp_path):
    """A repo mid-merge with a conflict in a.txt (ours='ours', theirs='theirs')."""
    root = str(tmp_path / "repo")
    os.makedirs(root)
    _git(root, "init")
    _git(root, "config", "user.email", "t@e.com")
    _git(root, "config", "user.name", "T")
    _git(root, "checkout", "-b", "main")
    _write(os.path.join(root, "a.txt"), "base\n")
    _git(root, "add", "."); _git(root, "commit", "-m", "base")

    _git(root, "checkout", "-b", "feature")
    _write(os.path.join(root, "a.txt"), "theirs\n")
    _git(root, "commit", "-am", "theirs change")

    _git(root, "checkout", "main")
    _write(os.path.join(root, "a.txt"), "ours\n")
    _git(root, "commit", "-am", "ours change")

    # Trigger the conflict (merge fails with non-zero, which is expected).
    subprocess.run(["git", "merge", "feature"], cwd=root, capture_output=True, text=True)
    return root


def _status_for(root, name):
    target = os.path.normpath(os.path.join(root, name))
    for e in get_status(root, root):
        if os.path.normpath(e.path) == target:
            return e

    return None


def test_conflict_detected(conflicted):
    entry = _status_for(conflicted, "a.txt")
    assert entry is not None
    assert entry.is_conflicted
    assert entry.code == VCSStatusCode.CONFLICTED
    # Conflicted files are not surfaced as staged or unstaged.
    assert not entry.is_staged()
    assert not entry.is_unstaged()


def test_merge_state_detected(conflicted):
    assert get_merge_state(conflicted) == MergeState.MERGE


def test_accept_ours(conflicted):
    path = os.path.join(conflicted, "a.txt")
    accept_ours(conflicted, [path])
    assert _read(path) == "ours\n"
    # "ours" equals HEAD here, so the file has no diff and drops out of status;
    # what matters is that it is no longer conflicted.
    assert not any(e.is_conflicted for e in get_status(conflicted, conflicted))


def test_accept_theirs(conflicted):
    path = os.path.join(conflicted, "a.txt")
    accept_theirs(conflicted, [path])
    assert _read(path) == "theirs\n"
    entry = _status_for(conflicted, "a.txt")
    assert entry is not None and not entry.is_conflicted


def test_mark_resolved(conflicted):
    path = os.path.join(conflicted, "a.txt")
    _write(path, "merged by hand\n")
    mark_resolved(conflicted, [path])
    entry = _status_for(conflicted, "a.txt")
    assert entry is not None and not entry.is_conflicted and entry.is_staged()


def test_abort_merge(conflicted):
    abort_merge(conflicted)
    assert get_merge_state(conflicted) == MergeState.NONE
    assert _read(os.path.join(conflicted, "a.txt")) == "ours\n"
    assert get_status(conflicted, conflicted) == []


def test_no_merge_state_when_clean(tmp_path):
    root = str(tmp_path / "clean")
    os.makedirs(root)
    _git(root, "init")
    assert get_merge_state(root) == MergeState.NONE
