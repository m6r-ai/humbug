"""Integration tests for git mutation and remote operations.

These tests drive a real git CLI against temporary repositories, so they are
skipped automatically when git is not available on PATH.
"""

import os
import shutil
import subprocess

import pytest

from git import (
    GitCommandError,
    commit,
    create_branch,
    discard_changes,
    get_branch_info,
    get_current_branch,
    get_status,
    get_upstream_status,
    stage_files,
    switch_branch,
    unstage_files,
)


pytestmark = pytest.mark.skipif(
    shutil.which("git") is None, reason="git executable not available"
)


def _git(cwd, *args):
    """Run a raw git command in *cwd* for test setup."""
    subprocess.run(
        ["git"] + list(args),
        cwd=cwd,
        check=True,
        capture_output=True,
        text=True,
    )


def _write(path, content):
    """Write *content* to *path*, creating parent dirs as needed."""
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w", encoding="utf-8") as f:
        f.write(content)


@pytest.fixture
def repo(tmp_path):
    """Create an initialised git repo with one committed file."""
    root = str(tmp_path / "repo")
    os.makedirs(root)
    _git(root, "init")
    _git(root, "config", "user.email", "test@example.com")
    _git(root, "config", "user.name", "Test User")
    # Ensure a deterministic default branch name across git versions.
    _git(root, "checkout", "-b", "main")

    readme = os.path.join(root, "README.md")
    _write(readme, "hello\n")
    _git(root, "add", "README.md")
    _git(root, "commit", "-m", "initial commit")

    return root


def _status_for(repo_root, path):
    """Return the VCSFileStatus for *path*, or None if not reported."""
    for entry in get_status(repo_root, repo_root):
        if os.path.normpath(entry.path) == os.path.normpath(path):
            return entry

    return None


class TestStageUnstage:
    """Tests for staging and unstaging files."""

    def test_stage_new_file(self, repo):
        new_file = os.path.join(repo, "new.txt")
        _write(new_file, "content\n")

        before = _status_for(repo, new_file)
        assert before is not None
        assert before.is_unstaged() and not before.is_staged()

        stage_files(repo, [new_file])

        after = _status_for(repo, new_file)
        assert after is not None
        assert after.is_staged() and not after.is_unstaged()

    def test_unstage_file(self, repo):
        new_file = os.path.join(repo, "new.txt")
        _write(new_file, "content\n")
        stage_files(repo, [new_file])

        unstage_files(repo, [new_file])

        entry = _status_for(repo, new_file)
        assert entry is not None
        assert entry.is_unstaged() and not entry.is_staged()

    def test_stage_empty_list_is_noop(self, repo):
        stage_files(repo, [])  # must not raise

    def test_partially_staged_file_is_both(self, repo):
        readme = os.path.join(repo, "README.md")
        _write(readme, "hello\nworld\n")
        stage_files(repo, [readme])
        # Modify again after staging -> staged + unstaged simultaneously.
        _write(readme, "hello\nworld\nagain\n")

        entry = _status_for(repo, readme)
        assert entry is not None
        assert entry.is_staged() and entry.is_unstaged()


class TestDiscard:
    """Tests for discarding working-tree changes."""

    def test_discard_restores_tracked_file(self, repo):
        readme = os.path.join(repo, "README.md")
        _write(readme, "hello\nmodified\n")

        discard_changes(repo, [readme])

        with open(readme, encoding="utf-8") as f:
            assert f.read() == "hello\n"
        assert _status_for(repo, readme) is None


class TestCommit:
    """Tests for committing staged changes."""

    def test_commit_staged_changes(self, repo):
        new_file = os.path.join(repo, "new.txt")
        _write(new_file, "content\n")
        stage_files(repo, [new_file])

        commit(repo, "add new file")

        assert _status_for(repo, new_file) is None

    def test_commit_empty_message_raises(self, repo):
        with pytest.raises(ValueError):
            commit(repo, "   ")

    def test_commit_nothing_staged_raises(self, repo):
        with pytest.raises(GitCommandError):
            commit(repo, "nothing here")


class TestBranches:
    """Tests for branch inspection and manipulation."""

    def test_current_branch(self, repo):
        assert get_current_branch(repo) == "main"

    def test_create_and_switch_branch(self, repo):
        create_branch(repo, "feature-x")
        assert get_current_branch(repo) == "feature-x"

        switch_branch(repo, "main")
        assert get_current_branch(repo) == "main"

    def test_create_branch_empty_name_raises(self, repo):
        with pytest.raises(ValueError):
            create_branch(repo, "  ")

    def test_branch_info_lists_all(self, repo):
        create_branch(repo, "feature-x", checkout=False)
        info = get_branch_info(repo)
        assert info.current == "main"
        assert set(info.branches) == {"main", "feature-x"}


class TestUpstreamStatus:
    """Tests for upstream tracking status."""

    def test_no_upstream(self, repo):
        status = get_upstream_status(repo)
        assert status.upstream is None
        assert status.ahead == 0
        assert status.behind == 0

    def test_ahead_of_upstream(self, tmp_path, repo):
        # Create a bare "remote" and push main to it, then commit locally.
        remote = str(tmp_path / "remote.git")
        subprocess.run(["git", "init", "--bare", remote], check=True, capture_output=True, text=True)
        _git(repo, "remote", "add", "origin", remote)
        _git(repo, "push", "-u", "origin", "main")

        readme = os.path.join(repo, "README.md")
        _write(readme, "hello\nlocal change\n")
        _git(repo, "commit", "-am", "local commit")

        status = get_upstream_status(repo)
        assert status.upstream == "origin/main"
        assert status.ahead == 1
        assert status.behind == 0
