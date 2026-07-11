"""Tests for the git AI tool."""

import asyncio
import os
import subprocess
import tempfile
from typing import Any
from unittest.mock import MagicMock

import pytest

from ai_tool import (
    AIToolAuthorizationDenied,
    AIToolCall,
    AIToolExecutionError,
)
from git_ai_tool.git_ai_tool import GitAITool
from mindspace.mindspace import Mindspace


def _run(args: list[str], cwd: str) -> str:
    """Run a command and return stdout, raising on failure."""
    result = subprocess.run(args, cwd=cwd, capture_output=True, text=True, check=True)
    return result.stdout


def _make_mindspace_mock(mindspace_path: str) -> MagicMock:
    """Create a mock Mindspace with the given path."""
    mindspace = MagicMock(spec=Mindspace)
    mindspace.mindspace_path.return_value = mindspace_path
    mindspace.MINDSPACE_DIR = ".humbug"
    return mindspace


def _make_tool_call(operation: str, **kwargs: Any) -> AIToolCall:
    """Create an AIToolCall for the given operation and arguments."""
    arguments: dict[str, Any] = {"operation": operation}
    arguments.update(kwargs)
    return AIToolCall(id="test-id", name="git", arguments=arguments)


def _make_auth_callback(authorized: bool = True) -> MagicMock:
    """Create a mocked authorization callback."""
    mock = MagicMock()

    async def mock_auth_callback(
        _tool_name: str,
        _arguments: dict[str, Any],
        _context: str,
        _auth_context: str | None,
        _destructive: bool
    ) -> bool:
        return authorized

    mock.side_effect = mock_auth_callback
    return mock


def _execute_tool(
    tool: GitAITool,
    tool_call: AIToolCall,
    auth_callback: MagicMock | None = None,
) -> Any:
    """Execute a tool call synchronously and return the result."""
    if auth_callback is None:
        auth_callback = _make_auth_callback(authorized=True)

    return asyncio.run(tool.execute(tool_call, None, auth_callback))


@pytest.fixture
def temp_repo_in_mindspace() -> Any:
    """Create a temp directory as mindspace with a git repo inside it.

    Yields (mindspace_path, mindspace_mock).
    """
    with tempfile.TemporaryDirectory() as mindspace_path:
        _run(["git", "init"], cwd=mindspace_path)
        _run(["git", "config", "user.email", "test@test.com"], cwd=mindspace_path)
        _run(["git", "config", "user.name", "Test User"], cwd=mindspace_path)

        with open(os.path.join(mindspace_path, "file1.txt"), "w", encoding="utf-8") as f:
            f.write("line 1\nline 2\n")

        _run(["git", "add", "file1.txt"], cwd=mindspace_path)
        _run(["git", "commit", "-m", "Initial commit"], cwd=mindspace_path)

        # Modify file1 (uncommitted change)
        with open(os.path.join(mindspace_path, "file1.txt"), "a", encoding="utf-8") as f:
            f.write("line 3\n")

        # Create untracked file
        with open(os.path.join(mindspace_path, "untracked.txt"), "w", encoding="utf-8") as f:
            f.write("untracked\n")

        mindspace_mock = _make_mindspace_mock(mindspace_path)

        yield mindspace_path, mindspace_mock


class TestGitAIToolDefinition:
    """Tests for tool definition and metadata."""

    def test_definition_has_correct_name(self) -> None:
        """Tool name should be 'git'."""
        tool = GitAITool(_make_mindspace_mock("/tmp"))
        definition = tool.get_definition()
        assert definition.name == "git"

    def test_definition_has_operations(self) -> None:
        """Tool should define the expected operations."""
        tool = GitAITool(_make_mindspace_mock("/tmp"))
        ops = tool.get_operation_definitions()
        assert set(ops.keys()) == {"status", "diff", "log", "branch", "show", "stat"}

    def test_brief_description(self) -> None:
        """Brief description should be concise."""
        tool = GitAITool(_make_mindspace_mock("/tmp"))
        desc = tool.get_brief_description()
        assert len(desc) < 200
        assert "git" in desc.lower() or "version" in desc.lower()


class TestStatusOperation:
    """Tests for the status operation."""

    def test_status_shows_changes(self, temp_repo_in_mindspace: Any) -> None:
        """Status should show modified and untracked files."""
        _mindspace_path, mindspace = temp_repo_in_mindspace
        tool = GitAITool(mindspace)
        result = _execute_tool(tool, _make_tool_call("status"))

        assert result.error is None
        assert "file1.txt" in result.content
        assert "untracked.txt" in result.content

    def test_status_with_path(self, temp_repo_in_mindspace: Any) -> None:
        """Status should work with a specific file path."""
        _mindspace_path, mindspace = temp_repo_in_mindspace
        tool = GitAITool(mindspace)
        result = _execute_tool(tool, _make_tool_call("status", path="file1.txt"))

        assert result.error is None

    def test_status_no_changes(self) -> None:
        """Status should report no changes for a clean repo."""
        with tempfile.TemporaryDirectory() as mindspace_path:
            _run(["git", "init"], cwd=mindspace_path)
            _run(["git", "config", "user.email", "t@t.com"], cwd=mindspace_path)
            _run(["git", "config", "user.name", "T"], cwd=mindspace_path)
            with open(os.path.join(mindspace_path, "f.txt"), "w", encoding="utf-8") as f:
                f.write("content\n")
            _run(["git", "add", "."], cwd=mindspace_path)
            _run(["git", "commit", "-m", "init"], cwd=mindspace_path)

            tool = GitAITool(_make_mindspace_mock(mindspace_path))
            result = _execute_tool(tool, _make_tool_call("status"))

            assert "No changes" in result.content


class TestDiffOperation:
    """Tests for the diff operation."""

    def test_diff_shows_changes(self, temp_repo_in_mindspace: Any) -> None:
        """Diff should show the uncommitted changes."""
        _mindspace_path, mindspace = temp_repo_in_mindspace
        tool = GitAITool(mindspace)
        result = _execute_tool(tool, _make_tool_call("diff"))

        assert result.error is None
        assert "+line 3" in result.content or "+untracked" in result.content

    def test_diff_no_changes(self) -> None:
        """Diff should report no changes for a clean repo."""
        with tempfile.TemporaryDirectory() as mindspace_path:
            _run(["git", "init"], cwd=mindspace_path)
            _run(["git", "config", "user.email", "t@t.com"], cwd=mindspace_path)
            _run(["git", "config", "user.name", "T"], cwd=mindspace_path)
            with open(os.path.join(mindspace_path, "f.txt"), "w", encoding="utf-8") as f:
                f.write("content\n")
            _run(["git", "add", "."], cwd=mindspace_path)
            _run(["git", "commit", "-m", "init"], cwd=mindspace_path)

            tool = GitAITool(_make_mindspace_mock(mindspace_path))
            result = _execute_tool(tool, _make_tool_call("diff"))

            assert "No changes" in result.content


    def test_diff_single_file_shows_only_that_file(self, temp_repo_in_mindspace: Any) -> None:
        """Diff with a file path should show only that file's diff."""
        _mindspace_path, mindspace = temp_repo_in_mindspace
        tool = GitAITool(mindspace)
        result = _execute_tool(tool, _make_tool_call("diff", path="file1.txt"))

        assert result.error is None
        assert "+line 3" in result.content
        assert "untracked" not in result.content

    def test_diff_single_untracked_file(self, temp_repo_in_mindspace: Any) -> None:
        """Diff with an untracked file path should show it as fully added."""
        _mindspace_path, mindspace = temp_repo_in_mindspace
        tool = GitAITool(mindspace)
        result = _execute_tool(tool, _make_tool_call("diff", path="untracked.txt"))

        assert result.error is None
        assert "+untracked" in result.content
        assert "line 3" not in result.content

    def test_diff_single_file_no_changes(self, temp_repo_in_mindspace: Any) -> None:
        """Diff with an unchanged tracked file should report no changes."""
        with tempfile.TemporaryDirectory() as mindspace_path:
            _run(["git", "init"], cwd=mindspace_path)
            _run(["git", "config", "user.email", "t@t.com"], cwd=mindspace_path)
            _run(["git", "config", "user.name", "T"], cwd=mindspace_path)
            with open(os.path.join(mindspace_path, "clean.txt"), "w", encoding="utf-8") as f:
                f.write("content\n")
            _run(["git", "add", "."], cwd=mindspace_path)
            _run(["git", "commit", "-m", "init"], cwd=mindspace_path)

            tool = GitAITool(_make_mindspace_mock(mindspace_path))
            result = _execute_tool(tool, _make_tool_call("diff", path="clean.txt"))

            assert result.error is None
            assert "No changes" in result.content


class TestLogOperation:
    """Tests for the log operation."""

    def test_log_shows_commits(self, temp_repo_in_mindspace: Any) -> None:
        """Log should show commit history."""
        _mindspace_path, mindspace = temp_repo_in_mindspace
        tool = GitAITool(mindspace)
        result = _execute_tool(tool, _make_tool_call("log"))

        assert result.error is None
        assert "Initial commit" in result.content

    def test_log_with_max_count(self, temp_repo_in_mindspace: Any) -> None:
        """Log should respect max_count."""
        _mindspace_path, mindspace = temp_repo_in_mindspace
        tool = GitAITool(mindspace)
        result = _execute_tool(tool, _make_tool_call("log", max_count=1))

        assert result.error is None


    def test_log_with_skip(self) -> None:
        """Log should skip the specified number of commits from the tip."""
        with tempfile.TemporaryDirectory() as mindspace_path:
            _run(["git", "init"], cwd=mindspace_path)
            _run(["git", "config", "user.email", "t@t.com"], cwd=mindspace_path)
            _run(["git", "config", "user.name", "T"], cwd=mindspace_path)

            for i in range(5):
                with open(os.path.join(mindspace_path, f"f{i}.txt"), "w", encoding="utf-8") as f:
                    f.write("content\n")
                _run(["git", "add", "."], cwd=mindspace_path)
                _run(["git", "commit", "-m", f"commit {i}"], cwd=mindspace_path)

            tool = GitAITool(_make_mindspace_mock(mindspace_path))

            # Get all 5 commits
            result_all = _execute_tool(tool, _make_tool_call("log", max_count=50))
            assert result_all.error is None
            assert "commit 4" in result_all.content
            assert "commit 0" in result_all.content

            # Skip the first 2, should start at commit 2
            result_skip = _execute_tool(tool, _make_tool_call("log", max_count=50, skip=2))
            assert result_skip.error is None
            assert "commit 4" not in result_skip.content
            assert "commit 3" not in result_skip.content
            assert "commit 2" in result_skip.content
            assert "commit 0" in result_skip.content

    def test_log_with_ref(self) -> None:
        """Log should start from the specified ref."""
        with tempfile.TemporaryDirectory() as mindspace_path:
            _run(["git", "init"], cwd=mindspace_path)
            _run(["git", "config", "user.email", "t@t.com"], cwd=mindspace_path)
            _run(["git", "config", "user.name", "T"], cwd=mindspace_path)

            for i in range(3):
                with open(os.path.join(mindspace_path, f"f{i}.txt"), "w", encoding="utf-8") as f:
                    f.write("content\n")
                _run(["git", "add", "."], cwd=mindspace_path)
                _run(["git", "commit", "-m", f"commit {i}"], cwd=mindspace_path)

            # Get the hash of the second commit (commit 1)
            log_output = _run(["git", "log", "--format=%H", "--max-count=2"], cwd=mindspace_path)
            commit_hashes = log_output.strip().split("\n")
            second_commit_hash = commit_hashes[1]

            tool = GitAITool(_make_mindspace_mock(mindspace_path))
            result = _execute_tool(tool, _make_tool_call("log", ref=second_commit_hash))

            assert result.error is None
            assert "commit 1" in result.content
            assert "commit 0" in result.content
            assert "commit 2" not in result.content


class TestBranchOperation:
    """Tests for the branch operation."""

    def test_branch_shows_current(self, temp_repo_in_mindspace: Any) -> None:
        """Branch should show the current branch."""
        _mindspace_path, mindspace = temp_repo_in_mindspace
        tool = GitAITool(mindspace)
        result = _execute_tool(tool, _make_tool_call("branch"))

        assert result.error is None
        assert "Current branch:" in result.content


    def test_branch_truncated_with_many_branches(self) -> None:
        """Branch should truncate when there are many local branches."""
        with tempfile.TemporaryDirectory() as mindspace_path:
            _run(["git", "init"], cwd=mindspace_path)
            _run(["git", "config", "user.email", "t@t.com"], cwd=mindspace_path)
            _run(["git", "config", "user.name", "T"], cwd=mindspace_path)
            with open(os.path.join(mindspace_path, "f.txt"), "w", encoding="utf-8") as f:
                f.write("content\n")
            _run(["git", "add", "."], cwd=mindspace_path)
            _run(["git", "commit", "-m", "init"], cwd=mindspace_path)

            # Create many branches with long names to exceed 64KB
            for i in range(1000):
                _run(["git", "branch", f"branch_{i:04d}_" + "x" * 200], cwd=mindspace_path)

            tool = GitAITool(_make_mindspace_mock(mindspace_path))
            result = _execute_tool(tool, _make_tool_call("branch"))

            assert "output truncated" in result.content
            assert "more branches omitted" in result.content
            assert "Current branch:" in result.content


class TestShowOperation:
    """Tests for the show operation."""

    def test_show_file_at_head(self, temp_repo_in_mindspace: Any) -> None:
        """Show should return file content at HEAD."""
        _mindspace_path, mindspace = temp_repo_in_mindspace
        tool = GitAITool(mindspace)
        result = _execute_tool(tool, _make_tool_call("show", path="file1.txt", ref="HEAD"))

        assert result.error is None
        assert "line 1" in result.content
        assert "line 3" not in result.content  # uncommitted

    def test_show_missing_path_raises(self, temp_repo_in_mindspace: Any) -> None:
        """Show should raise if path is missing."""
        _mindspace_path, mindspace = temp_repo_in_mindspace
        tool = GitAITool(mindspace)
        with pytest.raises(AIToolExecutionError):
            _execute_tool(tool, _make_tool_call("show", ref="HEAD"))

    def test_show_missing_ref_raises(self, temp_repo_in_mindspace: Any) -> None:
        """Show should raise if ref is missing."""
        _mindspace_path, mindspace = temp_repo_in_mindspace
        tool = GitAITool(mindspace)
        with pytest.raises(AIToolExecutionError):
            _execute_tool(tool, _make_tool_call("show", path="file1.txt"))


class TestHumbugExclusion:
    """Tests for .humbug/ directory exclusion."""

    def test_humbug_path_rejected(self) -> None:
        """Operations on .humbug/ paths should raise an error."""
        with tempfile.TemporaryDirectory() as mindspace_path:
            os.makedirs(os.path.join(mindspace_path, ".humbug"))
            tool = GitAITool(_make_mindspace_mock(mindspace_path))

            with pytest.raises(AIToolExecutionError, match="\\.humbug"):
                _execute_tool(tool, _make_tool_call("status", path=".humbug/settings.json"))


class TestNoRepo:
    """Tests for behavior when no repo exists."""

    def test_status_no_repo(self) -> None:
        """Status should raise an error when no repo exists in mindspace."""
        with tempfile.TemporaryDirectory() as mindspace_path:
            tool = GitAITool(_make_mindspace_mock(mindspace_path))
            with pytest.raises(AIToolExecutionError, match="No git repository"):
                _execute_tool(tool, _make_tool_call("status"))


class TestRepoOutsideMindspace:
    """Tests for behavior when repo root is outside the mindspace."""

    def test_repo_above_mindspace_not_found(self) -> None:
        """Should report no repo when repo root is outside mindspace."""
        with tempfile.TemporaryDirectory() as parent_dir:
            # Create a repo at parent_dir level
            _run(["git", "init"], cwd=parent_dir)
            _run(["git", "config", "user.email", "t@t.com"], cwd=parent_dir)
            _run(["git", "config", "user.name", "T"], cwd=parent_dir)
            with open(os.path.join(parent_dir, "f.txt"), "w", encoding="utf-8") as f:
                f.write("content\n")
            _run(["git", "add", "."], cwd=parent_dir)
            _run(["git", "commit", "-m", "init"], cwd=parent_dir)

            # Create a mindspace as a subdirectory
            mindspace_path = os.path.join(parent_dir, "mindspace_subdir")
            os.makedirs(mindspace_path)

            tool = GitAITool(_make_mindspace_mock(mindspace_path))
            with pytest.raises(AIToolExecutionError, match="No git repository"):
                _execute_tool(tool, _make_tool_call("status"))


class TestOutputLimits:
    """Tests for output size truncation."""

    def test_status_truncated_with_many_files(self) -> None:
        """Status should truncate when there are many changed files."""
        with tempfile.TemporaryDirectory() as mindspace_path:
            _run(["git", "init"], cwd=mindspace_path)
            _run(["git", "config", "user.email", "t@t.com"], cwd=mindspace_path)
            _run(["git", "config", "user.name", "T"], cwd=mindspace_path)

            # Create many untracked files with long names to exceed 64KB
            for i in range(1000):
                filename = f"file_{i:04d}_" + "x" * 200 + ".txt"
                with open(os.path.join(mindspace_path, filename), "w", encoding="utf-8") as f:
                    f.write("content\n")

            tool = GitAITool(_make_mindspace_mock(mindspace_path))
            result = _execute_tool(tool, _make_tool_call("status"))

            assert "output truncated" in result.content
            assert "more files omitted" in result.content

    def test_diff_large_changes_raise_execution_error(self) -> None:
        """Diff should fail when working-tree changes exceed the inline limit."""
        with tempfile.TemporaryDirectory() as mindspace_path:
            _run(["git", "init"], cwd=mindspace_path)
            _run(["git", "config", "user.email", "t@t.com"], cwd=mindspace_path)
            _run(["git", "config", "user.name", "T"], cwd=mindspace_path)

            # Create and commit a file, then make large uncommitted changes
            with open(os.path.join(mindspace_path, "big.txt"), "w", encoding="utf-8") as f:
                f.write("original\n")
            _run(["git", "add", "."], cwd=mindspace_path)
            _run(["git", "commit", "-m", "init"], cwd=mindspace_path)

            with open(os.path.join(mindspace_path, "big.txt"), "w", encoding="utf-8") as f:
                for _ in range(5000):
                    f.write("modified line with lots of text " * 10 + "\n")

            tool = GitAITool(_make_mindspace_mock(mindspace_path))
            with pytest.raises(AIToolExecutionError, match="too large to return inline") as exc_info:
                _execute_tool(tool, _make_tool_call("diff"))

            assert "output_path" in str(exc_info.value)

    def test_log_truncated_with_many_commits(self) -> None:
        """Log should truncate when there are many commits with long messages."""
        with tempfile.TemporaryDirectory() as mindspace_path:
            _run(["git", "init"], cwd=mindspace_path)
            _run(["git", "config", "user.email", "t@t.com"], cwd=mindspace_path)
            _run(["git", "config", "user.name", "T"], cwd=mindspace_path)

            # Create many commits with long messages
            for i in range(500):
                filename = f"f{i:04d}.txt"
                with open(os.path.join(mindspace_path, filename), "w", encoding="utf-8") as f:
                    f.write("content\n")
                _run(["git", "add", "."], cwd=mindspace_path)
                _run(["git", "commit", "-m", f"Commit {i} " + "x" * 100], cwd=mindspace_path)

            tool = GitAITool(_make_mindspace_mock(mindspace_path))
            result = _execute_tool(tool, _make_tool_call("log", max_count=500))

            assert "output truncated" in result.content
            assert "more commits omitted" in result.content

    def test_show_large_file_raises_execution_error(self) -> None:
        """Show should fail when the file content at a ref exceeds the inline limit."""
        with tempfile.TemporaryDirectory() as mindspace_path:
            _run(["git", "init"], cwd=mindspace_path)
            _run(["git", "config", "user.email", "t@t.com"], cwd=mindspace_path)
            _run(["git", "config", "user.name", "T"], cwd=mindspace_path)

            # Create and commit a large file
            with open(os.path.join(mindspace_path, "big.txt"), "w", encoding="utf-8") as f:
                for _ in range(5000):
                    f.write("line with lots of content " * 10 + "\n")
            _run(["git", "add", "."], cwd=mindspace_path)
            _run(["git", "commit", "-m", "init"], cwd=mindspace_path)

            tool = GitAITool(_make_mindspace_mock(mindspace_path))
            with pytest.raises(AIToolExecutionError, match="too large to return inline") as exc_info:
                _execute_tool(tool, _make_tool_call("show", path="big.txt", ref="HEAD"))

            assert "output_path" in str(exc_info.value)

    def test_show_large_file_writes_with_output_path(self) -> None:
        """Show should write full content when output_path is provided."""
        with tempfile.TemporaryDirectory() as mindspace_path:
            _run(["git", "init"], cwd=mindspace_path)
            _run(["git", "config", "user.email", "t@t.com"], cwd=mindspace_path)
            _run(["git", "config", "user.name", "T"], cwd=mindspace_path)

            payload = ("line with lots of content " * 10 + "\n") * 5000
            with open(os.path.join(mindspace_path, "big.txt"), "w", encoding="utf-8") as f:
                f.write(payload)
            _run(["git", "add", "."], cwd=mindspace_path)
            _run(["git", "commit", "-m", "init"], cwd=mindspace_path)

            tool = GitAITool(_make_mindspace_mock(mindspace_path))
            result = _execute_tool(
                tool,
                _make_tool_call(
                    "show",
                    path="big.txt",
                    ref="HEAD",
                    output_path="spilled/big-at-head.txt",
                ),
            )

            assert result.error is None
            assert "Wrote full git show output" in result.content
            assert "spilled/big-at-head.txt" in result.content

            written = os.path.join(mindspace_path, "spilled", "big-at-head.txt")
            with open(written, encoding="utf-8") as f:
                assert f.read() == payload

    def test_diff_large_changes_write_denied(self) -> None:
        """Diff with output_path should raise when the user denies write approval."""
        with tempfile.TemporaryDirectory() as mindspace_path:
            _run(["git", "init"], cwd=mindspace_path)
            _run(["git", "config", "user.email", "t@t.com"], cwd=mindspace_path)
            _run(["git", "config", "user.name", "T"], cwd=mindspace_path)

            with open(os.path.join(mindspace_path, "big.txt"), "w", encoding="utf-8") as f:
                f.write("original\n")
            _run(["git", "add", "."], cwd=mindspace_path)
            _run(["git", "commit", "-m", "init"], cwd=mindspace_path)

            with open(os.path.join(mindspace_path, "big.txt"), "w", encoding="utf-8") as f:
                for _ in range(5000):
                    f.write("modified line with lots of text " * 10 + "\n")

            tool = GitAITool(_make_mindspace_mock(mindspace_path))
            with pytest.raises(AIToolAuthorizationDenied, match="denied permission"):
                _execute_tool(
                    tool,
                    _make_tool_call("diff", output_path="out.diff"),
                    auth_callback=_make_auth_callback(authorized=False),
                )

    def test_show_small_file_returns_inline(self, temp_repo_in_mindspace: Any) -> None:
        """Show should return small files inline."""
        _mindspace_path, mindspace = temp_repo_in_mindspace
        tool = GitAITool(mindspace)
        result = _execute_tool(tool, _make_tool_call("show", path="file1.txt", ref="HEAD"))

        assert result.error is None
        assert "line 1" in result.content


class TestDiffWithRef:
    """Tests for diff with ref parameter."""

    def test_diff_with_ref(self) -> None:
        """Diff with ref should diff against that ref, not HEAD."""
        with tempfile.TemporaryDirectory() as mindspace_path:
            _run(["git", "init"], cwd=mindspace_path)
            _run(["git", "config", "user.email", "t@t.com"], cwd=mindspace_path)
            _run(["git", "config", "user.name", "T"], cwd=mindspace_path)

            with open(os.path.join(mindspace_path, "file1.txt"), "w", encoding="utf-8") as f:
                f.write("line 1\n")
            _run(["git", "add", "."], cwd=mindspace_path)
            _run(["git", "commit", "-m", "first"], cwd=mindspace_path)

            with open(os.path.join(mindspace_path, "file1.txt"), "a", encoding="utf-8") as f:
                f.write("line 2\n")
            _run(["git", "add", "."], cwd=mindspace_path)
            _run(["git", "commit", "-m", "second"], cwd=mindspace_path)

            tool = GitAITool(_make_mindspace_mock(mindspace_path))
            result = _execute_tool(tool, _make_tool_call("diff", path="file1.txt", ref="HEAD~1"))

            assert result.error is None
            assert "+line 2" in result.content


class TestRemoteBranches:
    """Tests for remote branch listing."""

    def test_branch_shows_remote_branches(self) -> None:
        """Branch should list remote branches."""
        with tempfile.TemporaryDirectory() as mindspace_path:
            _run(["git", "init"], cwd=mindspace_path)
            _run(["git", "config", "user.email", "t@t.com"], cwd=mindspace_path)
            _run(["git", "config", "user.name", "T"], cwd=mindspace_path)
            with open(os.path.join(mindspace_path, "f.txt"), "w", encoding="utf-8") as f:
                f.write("content\n")
            _run(["git", "add", "."], cwd=mindspace_path)
            _run(["git", "commit", "-m", "init"], cwd=mindspace_path)
            _run(["git", "update-ref", "refs/remotes/origin/main", "HEAD"], cwd=mindspace_path)

            tool = GitAITool(_make_mindspace_mock(mindspace_path))
            result = _execute_tool(tool, _make_tool_call("branch"))

            assert result.error is None
            assert "Remote branches:" in result.content
            assert "origin/main" in result.content


class TestStatOperation:
    """Tests for the stat operation."""

    def test_stat_shows_changed_files(self, temp_repo_in_mindspace: Any) -> None:
        """Stat should list files changed in a commit."""
        _mindspace_path, mindspace = temp_repo_in_mindspace
        tool = GitAITool(mindspace)
        result = _execute_tool(tool, _make_tool_call("stat", ref="HEAD"))

        assert result.error is None
        assert "file1.txt" in result.content

    def test_stat_missing_ref_raises(self, temp_repo_in_mindspace: Any) -> None:
        """Stat should raise if ref is missing."""
        _mindspace_path, mindspace = temp_repo_in_mindspace
        tool = GitAITool(mindspace)
        with pytest.raises(AIToolExecutionError):
            _execute_tool(tool, _make_tool_call("stat"))


class TestLogWithPath:
    """Tests for log with file path filtering."""

    def test_log_filtered_by_file(self) -> None:
        """Log with a file path should only show commits touching that file."""
        with tempfile.TemporaryDirectory() as mindspace_path:
            _run(["git", "init"], cwd=mindspace_path)
            _run(["git", "config", "user.email", "t@t.com"], cwd=mindspace_path)
            _run(["git", "config", "user.name", "T"], cwd=mindspace_path)

            # Commit 1: create file_a.txt
            with open(os.path.join(mindspace_path, "file_a.txt"), "w", encoding="utf-8") as f:
                f.write("a\n")
            _run(["git", "add", "."], cwd=mindspace_path)
            _run(["git", "commit", "-m", "commit A"], cwd=mindspace_path)

            # Commit 2: create file_b.txt
            with open(os.path.join(mindspace_path, "file_b.txt"), "w", encoding="utf-8") as f:
                f.write("b\n")
            _run(["git", "add", "."], cwd=mindspace_path)
            _run(["git", "commit", "-m", "commit B"], cwd=mindspace_path)

            tool = GitAITool(_make_mindspace_mock(mindspace_path))

            # Log for file_a should show both commits (A created it, B didn't touch it)
            result_a = _execute_tool(tool, _make_tool_call("log", path="file_a.txt"))
            assert result_a.error is None
            assert "commit A" in result_a.content
            assert "commit B" not in result_a.content

            # Log for file_b should show only commit B
            result_b = _execute_tool(tool, _make_tool_call("log", path="file_b.txt"))
            assert result_b.error is None
            assert "commit B" in result_b.content
            assert "commit A" not in result_b.content
