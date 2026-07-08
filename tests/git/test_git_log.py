"""Integration tests for git commit-history queries."""

import os
import shutil
import subprocess

import pytest

from git import (
    VCSStatusCode,
    get_commit_file_diff,
    get_commit_files,
    get_head_message,
    get_log,
)


pytestmark = pytest.mark.skipif(
    shutil.which("git") is None, reason="git executable not available"
)


def _git(cwd, *args, env=None):
    subprocess.run(["git"] + list(args), cwd=cwd, check=True, capture_output=True, text=True, env=env)


def _write(path, content):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w", encoding="utf-8") as f:
        f.write(content)


@pytest.fixture
def repo(tmp_path):
    """A repo with three commits: add README, add src/app.py, modify README."""
    root = str(tmp_path / "repo")
    os.makedirs(root)
    _git(root, "init")
    _git(root, "config", "user.email", "test@example.com")
    _git(root, "config", "user.name", "Test User")
    _git(root, "checkout", "-b", "main")

    _write(os.path.join(root, "README.md"), "hello\n")
    _git(root, "add", "README.md")
    _git(root, "commit", "-m", "initial commit")

    _write(os.path.join(root, "src", "app.py"), "print('hi')\n")
    _git(root, "add", "src/app.py")
    _git(root, "commit", "-m", "add app")

    _write(os.path.join(root, "README.md"), "hello\nworld\n")
    _git(root, "commit", "-am", "update readme")

    return root


def test_log_returns_commits_newest_first(repo):
    commits = get_log(repo)
    assert len(commits) == 3
    assert [c.subject for c in commits] == ["update readme", "add app", "initial commit"]
    assert all(c.author_name == "Test User" for c in commits)
    assert all(c.timestamp > 0 for c in commits)
    # Newest has one parent; root has none.
    assert len(commits[0].parents) == 1
    assert commits[-1].parents == []


def test_log_paging(repo):
    assert len(get_log(repo, max_count=2)) == 2
    skipped = get_log(repo, max_count=2, skip=2)
    assert len(skipped) == 1
    assert skipped[0].subject == "initial commit"


def test_log_empty_repo(tmp_path):
    root = str(tmp_path / "empty")
    os.makedirs(root)
    _git(root, "init")
    assert get_log(root) == []


def test_commit_files(repo):
    commits = get_log(repo)
    # "add app" commit added src/app.py.
    add_app = next(c for c in commits if c.subject == "add app")
    files = get_commit_files(repo, add_app.commit_hash)
    assert len(files) == 1
    assert files[0].code == VCSStatusCode.ADDED
    assert files[0].path == "src/app.py"


def test_commit_files_root_commit(repo):
    commits = get_log(repo)
    root_commit = commits[-1]
    files = get_commit_files(repo, root_commit.commit_hash)
    assert [f.path for f in files] == ["README.md"]
    assert files[0].code == VCSStatusCode.ADDED


def test_merge_commit_shows_files_and_diff(repo):
    # Build a merge: feature adds feat.txt; main advances; merge feature.
    _git(repo, "checkout", "-b", "feature")
    _write(os.path.join(repo, "feat.txt"), "hello\n")
    _git(repo, "add", "feat.txt")
    _git(repo, "commit", "-m", "feature work")
    _git(repo, "checkout", "main")
    _write(os.path.join(repo, "README.md"), "hello\nworld\nmain\n")
    _git(repo, "commit", "-am", "main advance")
    _git(repo, "merge", "feature", "--no-edit", "-m", "merge feature")

    merge = get_log(repo)[0]
    assert merge.subject == "merge feature"
    assert len(merge.parents) == 2

    # Merge must report the file it introduced (vs first parent), not empty.
    files = get_commit_files(repo, merge.commit_hash)
    assert any(f.path == "feat.txt" for f in files)

    diff = get_commit_file_diff(repo, merge.commit_hash, "feat.txt")
    assert "+hello" in diff


def test_commit_file_diff(repo):
    commits = get_log(repo)
    update = next(c for c in commits if c.subject == "update readme")
    diff = get_commit_file_diff(repo, update.commit_hash, "README.md")
    assert "+world" in diff
    assert "README.md" in diff


def test_head_message_returns_latest_subject(repo):
    assert get_head_message(repo) == "update readme"


def test_head_message_includes_body(repo):
    _git(repo, "commit", "--allow-empty", "-m", "Subject line", "-m", "Body paragraph.")
    assert get_head_message(repo) == "Subject line\n\nBody paragraph."


def test_head_message_empty_repo(tmp_path):
    root = str(tmp_path / "empty")
    os.makedirs(root)
    _git(root, "init")
    assert get_head_message(root) == ""
