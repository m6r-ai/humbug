"""Integration tests for revert, reset, undo, and stash operations."""

import os
import shutil
import subprocess

import pytest

from git import (
    GitCommandError,
    get_log,
    get_status,
    reset,
    revert_commit,
    stash_apply,
    stash_drop,
    stash_list,
    stash_pop,
    stash_push,
    undo_last_commit,
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
def repo(tmp_path):
    root = str(tmp_path / "repo")
    os.makedirs(root)
    _git(root, "init")
    _git(root, "config", "user.email", "t@e.com")
    _git(root, "config", "user.name", "T")
    _git(root, "checkout", "-b", "main")
    _write(os.path.join(root, "README.md"), "line1\n")
    _git(root, "add", "README.md")
    _git(root, "commit", "-m", "c1")
    _write(os.path.join(root, "README.md"), "line1\nline2\n")
    _git(root, "commit", "-am", "c2")
    return root


class TestRevert:
    def test_revert_creates_inverse_commit(self, repo):
        readme = os.path.join(repo, "README.md")
        assert _read(readme) == "line1\nline2\n"

        head = get_log(repo)[0].commit_hash
        revert_commit(repo, head)

        # A new commit exists and the c2 change is undone.
        assert _read(readme) == "line1\n"
        assert len(get_log(repo)) == 3
        assert get_log(repo)[0].subject.startswith("Revert")


class TestReset:
    def test_soft_reset_keeps_changes_staged(self, repo):
        reset(repo, "soft", "HEAD~1")
        # c2 is undone as a commit but its change is staged.
        assert len(get_log(repo)) == 1
        staged = [s for s in get_status(repo, repo) if s.is_staged()]
        assert any(os.path.basename(s.path) == "README.md" for s in staged)

    def test_hard_reset_discards_changes(self, repo):
        reset(repo, "hard", "HEAD~1")
        assert len(get_log(repo)) == 1
        assert _read(os.path.join(repo, "README.md")) == "line1\n"
        assert get_status(repo, repo) == []

    def test_invalid_mode_raises(self, repo):
        with pytest.raises(ValueError):
            reset(repo, "bogus")


class TestUndoLastCommit:
    def test_undo_keeps_changes_staged(self, repo):
        undo_last_commit(repo)
        assert len(get_log(repo)) == 1
        staged = [s for s in get_status(repo, repo) if s.is_staged()]
        assert staged  # the undone commit's changes remain staged


class TestStash:
    def test_stash_push_list_pop(self, repo):
        readme = os.path.join(repo, "README.md")
        _write(readme, "line1\nline2\nWIP\n")

        stash_push(repo, message="wip changes")
        # Working tree is clean after stashing.
        assert get_status(repo, repo) == []

        stashes = stash_list(repo)
        assert len(stashes) == 1
        assert "wip changes" in stashes[0].message
        assert stashes[0].ref == "stash@{0}"

        stash_pop(repo, stashes[0].ref)
        assert _read(readme) == "line1\nline2\nWIP\n"
        assert stash_list(repo) == []

    def test_stash_apply_keeps_entry_then_drop(self, repo):
        readme = os.path.join(repo, "README.md")
        _write(readme, "line1\nline2\nWIP\n")
        stash_push(repo)

        ref = stash_list(repo)[0].ref
        stash_apply(repo, ref)
        assert _read(readme) == "line1\nline2\nWIP\n"
        # apply keeps the entry
        assert len(stash_list(repo)) == 1

        stash_drop(repo, ref)
        assert stash_list(repo) == []

    def test_stash_includes_untracked(self, repo):
        _write(os.path.join(repo, "new.txt"), "x\n")
        stash_push(repo, include_untracked=True)
        assert get_status(repo, repo) == []
        stash_pop(repo)
        assert os.path.exists(os.path.join(repo, "new.txt"))

    def test_pop_with_no_stash_raises(self, repo):
        with pytest.raises(GitCommandError):
            stash_pop(repo)
