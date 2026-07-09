"""Git AI tool for version control operations on user repositories."""

import logging
import os
from typing import Any

from ai_tool import (
    AITool,
    AIToolAuthorizationCallback,
    AIToolCall,
    AIToolDefinition,
    AIToolExecutionError,
    AIToolOperationDefinition,
    AIToolParameter,
    AIToolResult,
)
from git import (
    GitCommandError,
    GitNotFoundError,
    GitRepository,
    find_repo_root,
)
from mindspace.mindspace import Mindspace


_MAX_RESPONSE_BYTES = 64 * 1024


class GitAITool(AITool):
    """
    Version control tool for interacting with git repositories.

    Provides read-only access to git repository state within the mindspace.
    All operations are scoped to repositories whose root is inside the mindspace
    boundary.  The ``.humbug/`` directory is never accessible through this tool.
    """

    def __init__(self, mindspace: Mindspace) -> None:
        """
        Initialize the git tool.

        Args:
            mindspace: The active mindspace, used for path resolution and
                repository boundary enforcement.
        """
        self._mindspace = mindspace
        self._logger = logging.getLogger("GitAITool")

    def get_definition(self) -> AIToolDefinition:
        """Get the tool definition."""
        return self._build_definition_from_operations(
            name="git",
            description_prefix=(
                "The git tool lets you (the AI) perform read-only version control operations on git "
                "repositories within the current mindspace. It supports querying repository status, "
                "viewing diffs, commit history, branches, and file contents at specific revisions.\n\n"
                "Only repositories whose root is within the mindspace boundary are accessible. "
                "The .humbug/ directory is excluded from all operations."
            ),
            additional_parameters=[
                AIToolParameter(
                    name="path",
                    type="string",
                    description=(
                        "File or directory path within the mindspace. Used to identify which "
                        "repository to operate on. May be absolute or relative to the mindspace root."
                    ),
                    required=False
                ),
                AIToolParameter(
                    name="ref",
                    type="string",
                    description=(
                        "Git ref (e.g. 'HEAD', a commit hash, branch name, or tag). "
                        "Used by the 'show' operation."
                    ),
                    required=False
                ),
                AIToolParameter(
                    name="max_count",
                    type="number",
                    description="Maximum number of commits to return (for 'log' operation). Default 50.",
                    required=False
                ),
                AIToolParameter(
                    name="skip",
                    type="number",
                    description=(
                        "Number of commits to skip from the tip before returning results "
                        "(for 'log' operation). Default 0. Use for paging through history."
                    ),
                    required=False
                )
            ]
        )

    def get_brief_description(self) -> str:
        """Get brief one-line description for system prompt."""
        return (
            "Version control operations (status, diff, log, branches, show file at ref, stat). "
            "Read-only access to git repositories within the mindspace."
        )

    def get_operation_definitions(self) -> dict[str, AIToolOperationDefinition]:
        """Get operation definitions for this tool."""
        return {
            "status": AIToolOperationDefinition(
                name="status",
                handler=self._status,
                extract_context=self._extract_status_context,
                allowed_parameters={"path"},
                required_parameters=set(),
                description=(
                    "Return the list of changed files in the repository. If 'path' is given, "
                    "shows the repository containing that path; otherwise uses the mindspace root. "
                    "Results include modified, added, deleted, renamed, copied, and untracked files."
                )
            ),
            "diff": AIToolOperationDefinition(
                name="diff",
                handler=self._diff,
                extract_context=self._extract_diff_context,
                allowed_parameters={"path", "ref"},
                required_parameters=set(),
                description=(
                    "Return a unified diff of working-tree changes in the repository. "
                    "If 'path' is a file, shows only the diff for that file. If 'path' is a directory, "
                    "shows the diff for the repository containing that path. If omitted, uses the "
                    "mindspace root. Use 'ref' to diff against a specific commit, branch, or tag "
                    "(default HEAD). Untracked files are shown as fully added."
                )
            ),
            "log": AIToolOperationDefinition(
                name="log",
                handler=self._log,
                extract_context=None,
                allowed_parameters={"path", "max_count", "skip", "ref"},
                required_parameters=set(),
                description=(
                    "Return commit history for the repository. Each entry includes hash, author, "
                    "date, and subject line. If 'path' is a file, shows only commits that touched "
                    "that file. If 'path' is a directory, shows history for the repository "
                    "containing that path. If omitted, uses the mindspace root. Use 'skip' to page "
                    "through older history and 'ref' to start from a specific branch, tag, or "
                    "commit hash (default HEAD)."
                )
            ),
            "branch": AIToolOperationDefinition(
                name="branch",
                handler=self._branch,
                extract_context=None,
                allowed_parameters={"path"},
                required_parameters=set(),
                description=(
                    "Return the current branch name, all local branches, and all remote branches. "
                    "If 'path' is given, shows branches for the repository containing that path; "
                    "otherwise uses the mindspace root."
                )
            ),
            "show": AIToolOperationDefinition(
                name="show",
                handler=self._show,
                extract_context=self._extract_show_context,
                allowed_parameters={"path", "ref"},
                required_parameters={"path", "ref"},
                description=(
                    "Return the content of a file at a specific git ref (e.g. 'HEAD', a commit "
                    "hash, branch name, or tag). The 'path' identifies both the file and the "
                    "repository. Returns an error if the file does not exist at the given ref."
                )
            ),
            "stat": AIToolOperationDefinition(
                name="stat",
                handler=self._stat,
                extract_context=self._extract_stat_context,
                allowed_parameters={"path", "ref"},
                required_parameters={"ref"},
                description=(
                    "Return the list of files changed in a specific commit. Requires 'ref' "
                    "(a commit hash, branch name, or tag). If 'path' is given, identifies "
                    "the repository; otherwise uses the mindspace root."
                )
            ),
        }

    def _resolve_path(self, path: str | None) -> str:
        """
        Resolve a path argument to an absolute filesystem path.

        If *path* is None, the mindspace root is used. The resolved path is
        checked to ensure it does not fall inside the ``.humbug/`` directory.

        Args:
            path: Path argument (absolute, relative, or None)

        Returns:
            Absolute filesystem path

        Raises:
            AIToolExecutionError: If the path resolves inside .humbug/ or
                outside the mindspace boundary.
        """
        mindspace_path = self._mindspace.mindspace_path()

        if not mindspace_path:
            raise AIToolExecutionError("No mindspace is open")

        if path is None:
            resolved = mindspace_path

        elif os.path.isabs(path):
            resolved = os.path.realpath(path)

        else:
            resolved = os.path.realpath(os.path.join(mindspace_path, path))

        # Enforce .humbug/ exclusion
        humbug_dir = os.path.join(mindspace_path, Mindspace.MINDSPACE_DIR)
        # Resolve symlinks (e.g. macOS /var -> /private/var) so comparisons are consistent
        _resolved_norm = os.path.realpath(os.path.normpath(resolved))
        _humbug_norm = os.path.realpath(os.path.normpath(humbug_dir))
        if _resolved_norm == _humbug_norm or _resolved_norm.startswith(
            _humbug_norm + os.sep
        ):
            raise AIToolExecutionError(
                "Git operations on the .humbug/ directory are not supported. "
                "This directory is managed by Humbug internally."
            )

        return resolved

    def _resolve_repo(self, path: str | None) -> GitRepository:
        """
        Resolve a path argument to a :class:`GitRepository` within the mindspace.

        Args:
            path: Path argument (absolute, relative, or None)

        Returns:
            GitRepository instance for the resolved repository

        Raises:
            AIToolExecutionError: If no repository is found within the mindspace
                boundary, or if the path resolves inside .humbug/.
        """
        resolved_path = self._resolve_path(path)
        mindspace_path = self._mindspace.mindspace_path()

        try:
            repo_root = find_repo_root(resolved_path, mindspace_path)

        except GitNotFoundError as e:
            raise AIToolExecutionError(
                "git executable not found. Please ensure git is installed and on your PATH."
            ) from e

        if repo_root is None:
            raise AIToolExecutionError(
                f"No git repository found within the mindspace for path '{resolved_path}'."
            )

        return GitRepository(repo_root)

    async def _status(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Status operation — list changed files."""
        path = tool_call.arguments.get("path")
        repo = self._resolve_repo(path)

        try:
            entries = repo.get_status()

        except GitCommandError as e:
            raise AIToolExecutionError(f"Failed to get git status: {e.stderr or str(e)}") from e

        if not entries:
            return AIToolResult(
                id=tool_call.id,
                name="git",
                content="No changes in the working tree."
            )

        lines: list[str] = []
        for entry in entries:
            rel_path = os.path.relpath(entry.path, repo.root())
            if entry.original_path:
                orig_rel = os.path.relpath(entry.original_path, repo.root())
                line = f"  {entry.code.name:<10} {orig_rel} -> {rel_path}"

            else:
                line = f"  {entry.code.name:<10} {rel_path}"

            if self._would_exceed_limit(lines, line):
                remaining = len(entries) - len(lines)
                lines.append(f"... output truncated, {remaining} more files omitted")
                break

            lines.append(line)

        return AIToolResult(
            id=tool_call.id,
            name="git",
            content=self._format_result(repo.root(), lines),
        )

    async def _diff(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Diff operation — show working-tree changes."""
        path = tool_call.arguments.get("path")
        repo = self._resolve_repo(path)
        ref = tool_call.arguments.get("ref", "HEAD")

        if not isinstance(ref, str) or not ref:
            ref = "HEAD"

        # If path is a specific file, diff just that file
        if path is not None:
            resolved_path = self._resolve_path(path)
            if os.path.isfile(resolved_path):
                return self._diff_single_file(tool_call, repo, resolved_path, ref)

        return self._diff_all_files(tool_call, repo, ref)

    def _diff_single_file(
        self, tool_call: AIToolCall, repo: GitRepository, file_path: str, ref: str = "HEAD"
    ) -> AIToolResult:
        """
        Return the diff for a single file.

        Args:
            tool_call: The original tool call.
            repo: The repository containing the file.
            file_path: Absolute path to the file to diff.
            ref: Git ref to diff against (default "HEAD").

        Returns:
            AIToolResult with the file diff, truncated if necessary.

        Raises:
            AIToolExecutionError: If the diff command fails.
        """
        try:
            diff_text = repo.get_file_diff(file_path, ref)

        except GitCommandError as e:
            raise AIToolExecutionError(
                f"Failed to get diff for '{file_path}': {e.stderr or str(e)}"
            ) from e

        except OSError as e:
            raise AIToolExecutionError(
                f"Failed to read file '{file_path}': {e}"
            ) from e

        if not diff_text:
            return AIToolResult(
                id=tool_call.id,
                name="git",
                content="No changes for this file.",
            )

        return AIToolResult(
            id=tool_call.id,
            name="git",
            content=self._truncate_content(diff_text),
        )

    def _diff_all_files(
        self, tool_call: AIToolCall, repo: GitRepository, ref: str = "HEAD"
    ) -> AIToolResult:
        """
        Return the combined diff for all changed files in the working tree.

        Args:
            tool_call: The original tool call.
            repo: The repository to diff.
            ref: Git ref to diff against (default "HEAD").

        Returns:
            AIToolResult with the combined diff, truncated if necessary.

        Raises:
            AIToolExecutionError: If the git status or diff command fails.
        """
        try:
            entries = repo.get_status()

        except GitCommandError as e:
            raise AIToolExecutionError(f"Failed to get git status: {e.stderr or str(e)}") from e

        if not entries:
            return AIToolResult(
                id=tool_call.id,
                name="git",
                content="No changes in the working tree.",
            )

        diff_parts: list[str] = []
        for entry in entries:
            diff_text = repo.get_file_diff(entry.path, ref)
            if diff_text:
                if self._would_exceed_limit(diff_parts, diff_text):
                    diff_parts.append("... output truncated, remaining diffs omitted")
                    break

                diff_parts.append(diff_text)

        if not diff_parts:
            return AIToolResult(
                id=tool_call.id,
                name="git",
                content="Changes detected but no diff text could be generated.",
            )

        return AIToolResult(
            id=tool_call.id,
            name="git",
            content="\n".join(diff_parts),
        )

    async def _log(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Log operation — show commit history."""
        path = tool_call.arguments.get("path")
        max_count = tool_call.arguments.get("max_count", 50)

        if not isinstance(max_count, int) or max_count < 1:
            max_count = 50

        max_count = min(max_count, 500)

        skip = tool_call.arguments.get("skip", 0)

        if not isinstance(skip, int) or skip < 0:
            skip = 0

        ref = tool_call.arguments.get("ref", "HEAD")

        if not isinstance(ref, str) or not ref:
            ref = "HEAD"

        repo = self._resolve_repo(path)

        # If path is a specific file, filter log to that file
        file_path: str | None = None
        if path is not None:
            resolved_path = self._resolve_path(path)
            if os.path.isfile(resolved_path):
                file_path = resolved_path

        try:
            commits = repo.get_log(max_count=max_count, skip=skip, ref=ref, path=file_path)

        except GitCommandError as e:
            raise AIToolExecutionError(f"Failed to get git log: {e.stderr or str(e)}") from e

        if not commits:
            return AIToolResult(
                id=tool_call.id,
                name="git",
                content="No commits in this repository."
            )

        lines: list[str] = []
        for i, commit in enumerate(commits):
            short_hash = commit.hash[:8]
            line1 = f"  {short_hash}  {commit.author_date}  {commit.author_name}"
            line2 = f"         {commit.subject}"
            line3 = ""

            if self._would_exceed_limit(lines, line1 + "\n" + line2 + "\n" + line3):
                remaining = len(commits) - i
                lines.append(f"... output truncated, {remaining} more commits omitted")
                break

            lines.append(line1)
            lines.append(line2)
            lines.append(line3)

        return AIToolResult(
            id=tool_call.id,
            name="git",
            content=self._format_result(repo.root(), lines),
        )

    async def _branch(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Branch operation — show current branch and list all branches."""
        path = tool_call.arguments.get("path")
        repo = self._resolve_repo(path)

        try:
            current = repo.get_current_branch()
            all_branches = repo.get_branches()
            remote_branches = repo.get_remote_branches()

        except GitCommandError as e:
            raise AIToolExecutionError(f"Failed to get branch info: {e.stderr or str(e)}") from e

        lines = [f"Repository: {repo.root()}", ""]
        lines.append(f"Current branch: {current}")
        lines.append("")

        if all_branches:
            lines.append("Local branches:")
            for branch in all_branches:
                marker = " *" if branch == current else ""
                line = f"  {branch}{marker}"

                if self._would_exceed_limit(lines, line):
                    branches_listed = len(lines) - 5  # 5 header lines before the branch list
                    remaining = len(all_branches) - branches_listed
                    lines.append(f"... output truncated, {remaining} more branches omitted")
                    break

                lines.append(line)

        else:
            lines.append("No local branches found.")

        if remote_branches:
            lines.append("")
            lines.append("Remote branches:")
            remote_header = len(lines)
            for branch in remote_branches:
                line = f"  {branch}"

                if self._would_exceed_limit(lines, line):
                    branches_listed = len(lines) - remote_header
                    remaining = len(remote_branches) - branches_listed
                    lines.append(f"... output truncated, {remaining} more branches omitted")
                    break

                lines.append(line)

        return AIToolResult(
            id=tool_call.id,
            name="git",
            content="\n".join(lines)
        )

    async def _show(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Show operation — display file content at a specific ref."""
        arguments = tool_call.arguments
        path = self._get_required_str_value("path", arguments)
        ref = self._get_required_str_value("ref", arguments)

        resolved_path = self._resolve_path(path)
        repo = self._resolve_repo(path)

        if not repo.is_file_tracked(resolved_path):
            raise AIToolExecutionError(
                f"File '{path}' is not tracked by git in repository '{repo.root()}'."
            )

        try:
            content = repo.show_file_at_ref(resolved_path, ref)

        except GitCommandError as e:
            raise AIToolExecutionError(
                f"Failed to show file at ref '{ref}': {e.stderr or str(e)}"
            ) from e

        if content is None:
            raise AIToolExecutionError(
                f"File '{path}' does not exist at ref '{ref}'."
            )

        return AIToolResult(
            id=tool_call.id,
            name="git",
            content=self._truncate_content(content)
        )

    async def _stat(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Stat operation — list files changed in a specific commit."""
        arguments = tool_call.arguments
        path = arguments.get("path")
        ref = self._get_required_str_value("ref", arguments)

        repo = self._resolve_repo(path)

        try:
            entries = repo.get_changed_files_at_ref(ref)

        except GitCommandError as e:
            raise AIToolExecutionError(
                f"Failed to get changed files for ref '{ref}': {e.stderr or str(e)}"
            ) from e

        if not entries:
            return AIToolResult(
                id=tool_call.id,
                name="git",
                content="No files changed in this commit.",
            )

        lines: list[str] = []
        for entry in entries:
            rel_path = os.path.relpath(entry.path, repo.root())
            if entry.original_path:
                orig_rel = os.path.relpath(entry.original_path, repo.root())
                line = f"  {entry.code.name:<10} {orig_rel} -> {rel_path}"

            else:
                line = f"  {entry.code.name:<10} {rel_path}"

            if self._would_exceed_limit(lines, line):
                remaining = len(entries) - len(lines)
                lines.append(f"... output truncated, {remaining} more files omitted")
                break

            lines.append(line)

        return AIToolResult(
            id=tool_call.id,
            name="git",
            content=self._format_result(repo.root(), lines),
        )

    def _would_exceed_limit(self, parts: list[str], new_part: str) -> bool:
        """
        Return True if adding *new_part* to *parts* would exceed the response size limit.

        Args:
            parts: Already accumulated response parts.
            new_part: The next part to potentially add.

        Returns:
            True if the combined size would exceed _MAX_RESPONSE_BYTES.
        """
        current_size = sum(len(p.encode("utf-8")) for p in parts)
        new_size = len(new_part.encode("utf-8"))
        return current_size + new_size > _MAX_RESPONSE_BYTES

    def _truncate_content(self, content: str) -> str:
        """
        Truncate *content* if it exceeds the response size limit.

        Args:
            content: Full content string.

        Returns:
            Content truncated to _MAX_RESPONSE_BYTES with a notice, or the
            original content if within the limit.
        """
        content_bytes = content.encode("utf-8")
        if len(content_bytes) <= _MAX_RESPONSE_BYTES:
            return content

        truncated = content_bytes[:_MAX_RESPONSE_BYTES].decode("utf-8", errors="ignore")
        omitted = len(content_bytes) - _MAX_RESPONSE_BYTES
        return f"{truncated}\n... output truncated, {omitted} bytes omitted"

    def _format_result(self, repo_root: str, lines: list[str]) -> str:
        """
        Format the standard result with a repo root header and joined lines.

        Args:
            repo_root: Absolute path to the repository root.
            lines: List of formatted output lines.

        Returns:
            Formatted result string.
        """
        return f"Repository: {repo_root}\n\n" + "\n".join(lines)

    def _extract_status_context(self, arguments: dict[str, Any]) -> str | None:
        """Extract context for status operation."""
        path = arguments.get("path", "mindspace root")
        return f"git status: {path}"

    def _extract_diff_context(self, arguments: dict[str, Any]) -> str | None:
        """Extract context for diff operation."""
        path = arguments.get("path", "mindspace root")
        return f"git diff: {path}"

    def _extract_show_context(self, arguments: dict[str, Any]) -> str | None:
        """Extract context for show operation."""
        path = arguments.get("path", "?")
        ref = arguments.get("ref", "?")
        return f"git show {ref}: {path}"

    def _extract_stat_context(self, arguments: dict[str, Any]) -> str | None:
        """Extract context for stat operation."""
        ref = arguments.get("ref", "?")
        return f"git stat: {ref}"
