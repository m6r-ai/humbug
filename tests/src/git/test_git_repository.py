"""Tests for git repository operations."""

import os
import subprocess
import tempfile

import pytest

from git import (
    GitCommitInfo,
    GitRepository,
    find_repo_root,
)
from git.git_status import GitStatusCode


def _run(args: list[str], cwd: str) -> str:
    """Run a command and return stdout, raising on failure."""
    result = subprocess.run(args, cwd=cwd, capture_output=True, text=True, check=True)
    return result.stdout


@pytest.fixture
def temp_repo() -> str:
    """Create a temporary git repository with some content and history."""
    with tempfile.TemporaryDirectory() as repo_dir:
        _run(["git", "init"], cwd=repo_dir)
        _run(["git", "config", "user.email", "test@test.com"], cwd=repo_dir)
        _run(["git", "config", "user.name", "Test User"], cwd=repo_dir)

        # First commit
        with open(os.path.join(repo_dir, "file1.txt"), "w", encoding="utf-8") as f:
            f.write("line 1\nline 2\nline 3\n")

        _run(["git", "add", "file1.txt"], cwd=repo_dir)
        _run(["git", "commit", "-m", "Initial commit"], cwd=repo_dir)

        # Second commit on a new branch
        _run(["git", "checkout", "-b", "feature"], cwd=repo_dir)

        with open(os.path.join(repo_dir, "file2.txt"), "w", encoding="utf-8") as f:
            f.write("new file\n")

        _run(["git", "add", "file2.txt"], cwd=repo_dir)
        _run(["git", "commit", "-m", "Add file2"], cwd=repo_dir)

        # Modify file1 (uncommitted)
        with open(os.path.join(repo_dir, "file1.txt"), "a", encoding="utf-8") as f:
            f.write("line 4\n")

        # Create an untracked file
        with open(os.path.join(repo_dir, "untracked.txt"), "w", encoding="utf-8") as f:
            f.write("untracked content\n")

        yield repo_dir


@pytest.fixture
def repo(temp_repo: str) -> GitRepository:
    """Create a GitRepository from the temp_repo fixture."""
    root = find_repo_root(temp_repo, temp_repo)
    assert root is not None
    return GitRepository(root)


class TestFindRepoRoot:
    """Tests for find_repo_root."""

    def test_finds_root_from_file_in_repo(self, temp_repo: str) -> None:
        """Should return the repo root for a file inside it, when within boundary."""
        file_path = os.path.join(temp_repo, "file1.txt")
        result = find_repo_root(file_path, temp_repo)
        assert result is not None
        assert os.path.realpath(result) == os.path.realpath(temp_repo)

    def test_finds_root_from_subdirectory(self, temp_repo: str) -> None:
        """Should find the repo root from a subdirectory within the boundary."""
        subdir = os.path.join(temp_repo, "subdir")
        os.makedirs(subdir)
        result = find_repo_root(subdir, temp_repo)
        assert result is not None
        assert os.path.realpath(result) == os.path.realpath(temp_repo)

    def test_returns_root_when_boundary_is_repo_root(self, temp_repo: str) -> None:
        """Should return the repo root when boundary equals the repo root."""
        result = find_repo_root(temp_repo, temp_repo)
        assert result is not None
        assert os.path.realpath(result) == os.path.realpath(temp_repo)

    def test_returns_none_when_repo_outside_boundary(self, temp_repo: str) -> None:
        """Should return None when the repo root is outside the boundary."""
        subdir = os.path.join(temp_repo, "subdir")
        os.makedirs(subdir)

        # Now boundary is subdir, repo root is temp_repo (above boundary)
        result = find_repo_root(os.path.join(subdir, "file.txt"), subdir)
        # The repo root (temp_repo) is outside the boundary (subdir)
        assert result is None

    def test_returns_none_for_non_repo(self) -> None:
        """Should return None for a path not in any repo."""
        with tempfile.TemporaryDirectory() as non_repo:
            result = find_repo_root(non_repo, non_repo)
            assert result is None


class TestGitRepository:
    """Tests for the GitRepository class."""

    def test_root_returns_root_path(self, temp_repo: str, repo: GitRepository) -> None:
        """root() should return the absolute path to the repository root."""
        assert os.path.realpath(repo.root()) == os.path.realpath(temp_repo)


class TestIsFileTracked:
    """Tests for GitRepository.is_file_tracked."""

    def test_tracked_file(self, repo: GitRepository, temp_repo: str) -> None:
        """Should return True for a tracked file."""
        file_path = os.path.join(temp_repo, "file1.txt")
        assert repo.is_file_tracked(file_path) is True

    def test_untracked_file(self, repo: GitRepository, temp_repo: str) -> None:
        """Should return False for an untracked file."""
        file_path = os.path.join(temp_repo, "untracked.txt")
        assert repo.is_file_tracked(file_path) is False


class TestGetFileDiff:
    """Tests for GitRepository.get_file_diff."""

    def test_diff_for_modified_tracked_file(self, repo: GitRepository, temp_repo: str) -> None:
        """Should return a diff for a modified tracked file."""
        file_path = os.path.join(temp_repo, "file1.txt")
        diff = repo.get_file_diff(file_path)
        assert "+line 4" in diff

    def test_diff_for_untracked_file(self, repo: GitRepository, temp_repo: str) -> None:
        """Should return a synthetic diff for an untracked file."""
        file_path = os.path.join(temp_repo, "untracked.txt")
        diff = repo.get_file_diff(file_path)
        assert "+untracked content" in diff

    def test_empty_diff_for_unchanged_file(self, repo: GitRepository, temp_repo: str) -> None:
        """Should return empty string for an unchanged file."""
        file_path = os.path.join(temp_repo, "file2.txt")
        diff = repo.get_file_diff(file_path)
        assert diff == ""


    def test_diff_with_ref(self, repo: GitRepository, temp_repo: str) -> None:
        """Should diff against a specified ref, not just HEAD."""
        file_path = os.path.join(temp_repo, "file1.txt")
        # file1.txt was different at HEAD~1 (no line 4 yet)
        diff = repo.get_file_diff(file_path, ref="HEAD~1")
        assert "+line 4" in diff


class TestGetStatus:
    """Tests for GitRepository.get_status."""

    def test_status_shows_modified_and_untracked(self, repo: GitRepository) -> None:
        """Should show modified and untracked files."""
        entries = repo.get_status()
        paths = {os.path.basename(e.path) for e in entries}

        assert "file1.txt" in paths  # modified
        assert "untracked.txt" in paths  # untracked

    def test_status_codes(self, repo: GitRepository) -> None:
        """Should classify files correctly."""
        entries = repo.get_status()
        codes_by_name = {}
        for entry in entries:
            codes_by_name[os.path.basename(entry.path)] = entry.code

        assert codes_by_name["file1.txt"] == GitStatusCode.MODIFIED
        assert codes_by_name["untracked.txt"] == GitStatusCode.UNTRACKED


class TestGetCurrentBranch:
    """Tests for GitRepository.get_current_branch."""

    def test_returns_current_branch_name(self, repo: GitRepository) -> None:
        """Should return the current branch name."""
        branch = repo.get_current_branch()
        assert branch == "feature"


class TestGetBranches:
    """Tests for GitRepository.get_branches."""

    def test_returns_all_local_branches(self, repo: GitRepository) -> None:
        """Should return all local branches sorted alphabetically."""
        branches = repo.get_branches()
        # We created 'master'/'main' (initial) and 'feature'
        assert "feature" in branches
        assert len(branches) >= 2


class TestGetLog:
    """Tests for GitRepository.get_log."""

    def test_returns_commits_most_recent_first(self, repo: GitRepository) -> None:
        """Should return commits with most recent first."""
        commits = repo.get_log()
        assert len(commits) >= 2
        assert commits[0].subject == "Add file2"
        assert commits[1].subject == "Initial commit"

    def test_commit_info_fields(self, repo: GitRepository) -> None:
        """Should populate all GitCommitInfo fields."""
        commits = repo.get_log()
        assert len(commits) > 0

        commit = commits[0]
        assert isinstance(commit, GitCommitInfo)
        assert len(commit.hash) == 40  # full hash
        assert commit.author_name == "Test User"
        assert commit.author_email == "test@test.com"
        assert commit.subject == "Add file2"

    def test_max_count_limit(self, repo: GitRepository) -> None:
        """Should respect the max_count limit."""
        commits = repo.get_log(max_count=1)
        assert len(commits) == 1


    def test_skip(self, repo: GitRepository) -> None:
        """Should skip the specified number of commits from the tip."""
        all_commits = repo.get_log()
        assert len(all_commits) >= 2

        skipped = repo.get_log(skip=1)
        assert len(skipped) == len(all_commits) - 1
        assert skipped[0].subject == "Initial commit"

    def test_ref(self, repo: GitRepository) -> None:
        """Should start log from the specified ref."""
        commits = repo.get_log(ref="HEAD~1")
        assert len(commits) == 1
        assert commits[0].subject == "Initial commit"


    def test_path_filter(self, repo: GitRepository, temp_repo: str) -> None:
        """Should only return commits that touched the given file."""
        file2_path = os.path.join(temp_repo, "file2.txt")
        commits = repo.get_log(path=file2_path)
        assert len(commits) == 1
        assert commits[0].subject == "Add file2"


class TestShowFileAtRef:
    """Tests for GitRepository.show_file_at_ref."""

    def test_show_file_at_head(self, repo: GitRepository, temp_repo: str) -> None:
        """Should return file content at HEAD."""
        file_path = os.path.join(temp_repo, "file1.txt")
        content = repo.show_file_at_ref(file_path, "HEAD")
        assert content is not None
        assert "line 1" in content
        assert "line 4" not in content  # line 4 is uncommitted

    def test_show_file_at_commit(self, repo: GitRepository, temp_repo: str) -> None:
        """Should return file content at a specific commit."""
        commits = repo.get_log()
        first_commit_hash = commits[-1].hash  # Initial commit

        file_path = os.path.join(temp_repo, "file1.txt")
        content = repo.show_file_at_ref(file_path, first_commit_hash)
        assert content is not None
        assert "line 1" in content

    def test_show_nonexistent_file_at_ref(self, repo: GitRepository, temp_repo: str) -> None:
        """Should return None for a file that doesn't exist at the ref."""
        file_path = os.path.join(temp_repo, "file2.txt")
        # file2 doesn't exist on the initial commit's tree, get that hash
        commits = repo.get_log()
        first_commit_hash = commits[-1].hash

        content = repo.show_file_at_ref(file_path, first_commit_hash)
        assert content is None


class TestGetFileAtHead:
    """Tests for GitRepository.get_file_at_head."""

    def test_returns_head_content(self, repo: GitRepository, temp_repo: str) -> None:
        """Should return file content at HEAD."""
        file_path = os.path.join(temp_repo, "file1.txt")
        content = repo.get_file_at_head(file_path)
        assert content is not None
        assert "line 1" in content
        assert "line 4" not in content

    def test_returns_none_for_untracked(self, repo: GitRepository, temp_repo: str) -> None:
        """Should return None for an untracked file."""
        file_path = os.path.join(temp_repo, "untracked.txt")
        content = repo.get_file_at_head(file_path)
        assert content is None


class TestGetRemoteBranches:
    """Tests for GitRepository.get_remote_branches."""

    def test_returns_remote_branches(self, repo: GitRepository) -> None:
        """Should return remote branches after adding one via update-ref."""
        _run(["git", "update-ref", "refs/remotes/origin/main", "HEAD"], cwd=repo.root())

        branches = repo.get_remote_branches()
        assert "origin/main" in branches

    def test_returns_empty_when_no_remotes(self, repo: GitRepository) -> None:
        """Should return empty list when no remote branches exist."""
        branches = repo.get_remote_branches()
        assert branches == []


class TestGetChangedFilesAtRef:
    """Tests for GitRepository.get_changed_files_at_ref."""

    def test_returns_files_changed_in_commit(self, repo: GitRepository, temp_repo: str) -> None:
        """Should return files changed in the specified commit."""
        commits = repo.get_log()
        second_commit = commits[0]  # most recent first
        assert second_commit.subject == "Add file2"

        entries = repo.get_changed_files_at_ref(second_commit.hash)
        paths = {os.path.basename(e.path) for e in entries}
        assert "file2.txt" in paths

    def test_returns_files_for_initial_commit(self, repo: GitRepository) -> None:
        """Should return files added in the initial commit."""
        commits = repo.get_log()
        initial_commit = commits[-1]  # oldest
        assert initial_commit.subject == "Initial commit"

        entries = repo.get_changed_files_at_ref(initial_commit.hash)
        paths = {os.path.basename(e.path) for e in entries}
        assert "file1.txt" in paths
