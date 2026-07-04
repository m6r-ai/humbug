"""Integration tests for branch management, tags, remotes, and hunk staging."""

import os
import shutil
import subprocess

import pytest

from git import (
    GitCommandError,
    add_remote,
    apply_patch,
    create_tag,
    delete_branch,
    delete_tag,
    get_branch_info,
    get_staged_file_diff,
    get_status,
    get_unstaged_file_diff,
    list_remotes,
    list_tags,
    merge_branch,
    remove_remote,
    rename_branch,
    split_file_diff,
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
    _write(os.path.join(root, "f.txt"), "a\nb\nc\n")
    _git(root, "add", "."); _git(root, "commit", "-m", "init")
    return root


class TestBranchManagement:
    def test_delete_branch(self, repo):
        _git(repo, "branch", "temp")
        assert "temp" in get_branch_info(repo).branches
        delete_branch(repo, "temp")
        assert "temp" not in get_branch_info(repo).branches

    def test_rename_branch(self, repo):
        _git(repo, "branch", "old")
        rename_branch(repo, "old", "renamed")
        branches = get_branch_info(repo).branches
        assert "renamed" in branches and "old" not in branches

    def test_rename_empty_raises(self, repo):
        with pytest.raises(ValueError):
            rename_branch(repo, "main", "  ")

    def test_merge_branch(self, repo):
        _git(repo, "checkout", "-b", "feature")
        _write(os.path.join(repo, "g.txt"), "new\n")
        _git(repo, "add", "."); _git(repo, "commit", "-m", "add g")
        _git(repo, "checkout", "main")
        merge_branch(repo, "feature")
        assert os.path.exists(os.path.join(repo, "g.txt"))


class TestTags:
    def test_create_list_delete_tag(self, repo):
        create_tag(repo, "v1.0", message="release one")
        assert "v1.0" in list_tags(repo)
        delete_tag(repo, "v1.0")
        assert "v1.0" not in list_tags(repo)

    def test_lightweight_tag(self, repo):
        create_tag(repo, "light")
        assert "light" in list_tags(repo)

    def test_empty_tag_name_raises(self, repo):
        with pytest.raises(ValueError):
            create_tag(repo, "  ")


class TestRemotes:
    def test_add_list_remove_remote(self, repo):
        add_remote(repo, "origin", "https://example.com/repo.git")
        remotes = list_remotes(repo)
        assert len(remotes) == 1
        assert remotes[0].name == "origin"
        assert remotes[0].url == "https://example.com/repo.git"
        remove_remote(repo, "origin")
        assert list_remotes(repo) == []

    def test_add_empty_raises(self, repo):
        with pytest.raises(ValueError):
            add_remote(repo, "", "url")


_LONG = "".join(f"line{i}\n" for i in range(1, 21))  # 20 lines


def _two_hunk_repo(root):
    """Commit a 20-line file, then edit the first and last lines (two hunks)."""
    _write(os.path.join(root, "long.txt"), _LONG)
    _git(root, "add", "long.txt"); _git(root, "commit", "-m", "long")
    edited = _LONG.replace("line1\n", "LINE1\n").replace("line20\n", "LINE20\n")
    _write(os.path.join(root, "long.txt"), edited)


class TestSplitDiff:
    def test_split_header_and_hunks(self, repo):
        _two_hunk_repo(repo)
        diff = get_unstaged_file_diff(repo, os.path.join(repo, "long.txt"))
        header, hunks = split_file_diff(diff)
        assert header.startswith("diff --git")
        assert "+++ " in header
        assert len(hunks) == 2
        assert all(h.startswith("@@") for h in hunks)


class TestHunkStaging:
    def test_stage_single_hunk(self, repo):
        _two_hunk_repo(repo)
        path = os.path.join(repo, "long.txt")
        diff = get_unstaged_file_diff(repo, path)
        header, hunks = split_file_diff(diff)
        assert len(hunks) == 2

        # Stage only the first hunk (the LINE1 change).
        apply_patch(repo, header + hunks[0], cached=True)

        entry = next(s for s in get_status(repo, repo)
                     if os.path.basename(s.path) == "long.txt")
        assert entry.is_staged() and entry.is_unstaged()  # part staged, part not

        staged = get_staged_file_diff(repo, path)
        assert "+LINE1" in staged
        assert "+LINE20" not in staged  # second hunk stayed unstaged

    def test_unstage_hunk_reverse(self, repo):
        _write(os.path.join(repo, "f.txt"), "A\nb\nc\n")
        path = os.path.join(repo, "f.txt")
        _git(repo, "add", "f.txt")  # stage everything

        staged = get_staged_file_diff(repo, path)
        header, hunks = split_file_diff(staged)
        # Reverse-apply to the index to unstage the hunk.
        apply_patch(repo, header + hunks[0], cached=True, reverse=True)

        assert get_staged_file_diff(repo, path).strip() == ""

    def test_apply_bad_patch_raises(self, repo):
        with pytest.raises(GitCommandError):
            apply_patch(repo, "not a valid patch\n", cached=True)
