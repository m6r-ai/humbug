"""Tests for cherry-pick, remote branches, rebase, ignore, clean, blame, compare."""

import os
import shutil
import subprocess

import pytest

from git import (
    VCSStatusCode,
    add_to_gitignore,
    checkout_remote_branch,
    cherry_pick,
    clean_untracked,
    discard_all_changes,
    get_blame,
    get_current_branch,
    get_log,
    get_ref_diff_files,
    get_ref_file_diff,
    get_status,
    list_remote_branches,
    rebase,
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
    _write(os.path.join(root, "f.txt"), "base\n")
    _git(root, "add", "."); _git(root, "commit", "-m", "c1")
    return root


class TestCherryPick:
    def test_cherry_pick_applies_commit(self, repo):
        # Make a commit on a side branch, then cherry-pick it onto main.
        _git(repo, "checkout", "-b", "side")
        _write(os.path.join(repo, "new.txt"), "hello\n")
        _git(repo, "add", "."); _git(repo, "commit", "-m", "add new")
        target = get_log(repo)[0].commit_hash

        _git(repo, "checkout", "main")
        cherry_pick(repo, target)

        assert os.path.exists(os.path.join(repo, "new.txt"))
        assert get_log(repo)[0].subject == "add new"


class TestRemoteBranches:
    def test_list_and_checkout_remote_branch(self, tmp_path, repo):
        # Create a bare "remote" with an extra branch, then clone-like fetch.
        remote = str(tmp_path / "remote.git")
        subprocess.run(["git", "init", "--bare", remote], check=True, capture_output=True, text=True)
        _git(repo, "remote", "add", "origin", remote)
        _git(repo, "push", "origin", "main")
        _git(repo, "checkout", "-b", "feature")
        _write(os.path.join(repo, "feat.txt"), "x\n")
        _git(repo, "add", "."); _git(repo, "commit", "-m", "feat")
        _git(repo, "push", "origin", "feature")

        # Fresh clone sees remote branches.
        clone = str(tmp_path / "clone")
        subprocess.run(["git", "clone", remote, clone], check=True, capture_output=True, text=True)
        remotes = list_remote_branches(clone)
        assert "origin/feature" in remotes
        assert not any(r.endswith("/HEAD") for r in remotes)

        checkout_remote_branch(clone, "origin/feature")
        assert get_current_branch(clone) == "feature"
        assert os.path.exists(os.path.join(clone, "feat.txt"))


class TestRebase:
    def test_rebase_onto_branch(self, repo):
        # main has c1; create feature from c1 with its own commit; add c2 to main;
        # rebase feature onto main -> feature includes c2.
        _git(repo, "checkout", "-b", "feature")
        _write(os.path.join(repo, "feat.txt"), "f\n")
        _git(repo, "add", "."); _git(repo, "commit", "-m", "feat commit")
        _git(repo, "checkout", "main")
        _write(os.path.join(repo, "f.txt"), "base\nmore\n")
        _git(repo, "commit", "-am", "c2 on main")
        _git(repo, "checkout", "feature")

        rebase(repo, "main")
        subjects = [c.subject for c in get_log(repo)]
        assert "c2 on main" in subjects  # feature now sits on top of main's c2
        assert "feat commit" in subjects


class TestGitignore:
    def test_add_to_gitignore(self, repo):
        add_to_gitignore(repo, "build/")
        assert "build/" in _read(os.path.join(repo, ".gitignore")).splitlines()
        # Idempotent.
        add_to_gitignore(repo, "build/")
        assert _read(os.path.join(repo, ".gitignore")).count("build/") == 1


class TestDiscardAndClean:
    def test_discard_all_changes(self, repo):
        _write(os.path.join(repo, "f.txt"), "base\nmodified\n")
        _git(repo, "add", "f.txt")  # even staged
        discard_all_changes(repo)
        assert _read(os.path.join(repo, "f.txt")) == "base\n"
        assert get_status(repo, repo) == []

    def test_clean_untracked(self, repo):
        _write(os.path.join(repo, "junk.txt"), "x\n")
        _write(os.path.join(repo, "sub", "more.txt"), "y\n")
        clean_untracked(repo)
        assert not os.path.exists(os.path.join(repo, "junk.txt"))
        assert not os.path.exists(os.path.join(repo, "sub"))


class TestBlame:
    def test_blame_reports_author_and_lines(self, repo):
        _write(os.path.join(repo, "f.txt"), "base\nsecond\n")
        _git(repo, "commit", "-am", "c2")
        blame = get_blame(repo, os.path.join(repo, "f.txt"))
        assert [b.content for b in blame] == ["base", "second"]
        assert all(b.author == "T" for b in blame)
        assert [b.line_number for b in blame] == [1, 2]
        assert all(len(b.short_hash) == 8 for b in blame)


class TestCompareRefs:
    def test_ref_diff_files_and_file_diff(self, repo):
        base = get_log(repo)[0].commit_hash
        _write(os.path.join(repo, "f.txt"), "base\nchanged\n")
        _write(os.path.join(repo, "added.txt"), "new\n")
        _git(repo, "add", "."); _git(repo, "commit", "-m", "c2")
        head = get_log(repo)[0].commit_hash

        files = get_ref_diff_files(repo, base, head)
        by_path = {f.path: f.code for f in files}
        assert by_path.get("f.txt") == VCSStatusCode.MODIFIED
        assert by_path.get("added.txt") == VCSStatusCode.ADDED

        diff = get_ref_file_diff(repo, base, head, "f.txt")
        assert "+changed" in diff
