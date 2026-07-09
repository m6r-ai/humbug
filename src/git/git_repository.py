"""Git repository operations."""

import os
import sys
import subprocess

from git.git_commit_info import GitCommitInfo
from git.git_error import GitCommandError, GitNotFoundError, GitNotRepositoryError
from git.git_status import GitFileStatus, GitStatusCode


_GIT_TIMEOUT = 10  # seconds


def _run_git(args: list[str], cwd: str) -> str:
    """
    Run a git command and return its stdout.

    Args:
        args: Git arguments (excluding the 'git' executable itself)
        cwd: Working directory for the command

    Returns:
        Stdout output as a string

    Raises:
        GitNotFoundError: If git is not installed or not on PATH
        GitCommandError: If the command exits with a non-zero return code
    """
    if sys.platform == "win32":
        creationflags = subprocess.CREATE_NO_WINDOW

    else:
        creationflags = 0

    try:
        result = subprocess.run(
            ["git"] + args,
            cwd=cwd,
            check=False,
            capture_output=True,
            text=True,
            encoding="utf-8",
            timeout=_GIT_TIMEOUT,
            creationflags=creationflags,
        )

    except FileNotFoundError as e:
        raise GitNotFoundError("git executable not found on PATH") from e

    except subprocess.TimeoutExpired as e:
        raise GitCommandError(
            f"git command timed out after {_GIT_TIMEOUT}s",
            returncode=-1,
            stderr=""
        ) from e

    if result.returncode != 0:
        raise GitCommandError(
            f"git {args[0]} failed with return code {result.returncode}",
            returncode=result.returncode,
            stderr=result.stderr.strip()
        )

    return result.stdout


def _xy_to_code(xy: str) -> GitStatusCode:
    """
    Map a porcelain XY status string to a GitStatusCode.

    The X column is the index (staged) status; the Y column is the
    working-tree status.  We collapse both into a single category,
    preferring the working-tree state when both are set.

    Args:
        xy: Two-character porcelain status string.

    Returns:
        Corresponding GitStatusCode.
    """
    if xy == "??":
        return GitStatusCode.UNTRACKED

    x, y = xy[0], xy[1]

    for char in (y, x):
        if char == "M":
            return GitStatusCode.MODIFIED

        if char == "A":
            return GitStatusCode.ADDED

        if char == "D":
            return GitStatusCode.DELETED

        if char == "R":
            return GitStatusCode.RENAMED

        if char == "C":
            return GitStatusCode.COPIED

    return GitStatusCode.UNKNOWN


def _diff_tree_status_to_code(status: str) -> GitStatusCode:
    """
    Map a ``git diff-tree`` name-status character to a GitStatusCode.

    Args:
        status: Single-character status code from diff-tree output.

    Returns:
        Corresponding GitStatusCode.
    """
    if status == "M":
        return GitStatusCode.MODIFIED

    if status == "A":
        return GitStatusCode.ADDED

    if status == "D":
        return GitStatusCode.DELETED

    if status == "R":
        return GitStatusCode.RENAMED

    if status == "C":
        return GitStatusCode.COPIED

    return GitStatusCode.UNKNOWN


def _within_subtree(abs_path: str, subtree_path: str) -> bool:
    """
    Return True if *abs_path* is inside *subtree_path*.

    Args:
        abs_path: Absolute path to test.
        subtree_path: Absolute path to the subtree root.

    Returns:
        True if abs_path is within subtree_path.
    """
    subtree_path = os.path.normpath(subtree_path)

    # Resolve symlinks (e.g. macOS /var -> /private/var) so comparisons are consistent
    abs_path = os.path.realpath(abs_path)
    subtree_path = os.path.realpath(subtree_path)

    if sys.platform == "win32":
        abs_path = os.path.normpath(abs_path).lower()
        subtree_path = subtree_path.lower()

    try:
        common = os.path.commonpath([abs_path, subtree_path])
        return common == subtree_path

    except ValueError:
        return False


def _parse_status(raw: str, repo_root: str, subtree_path: str | None) -> list[GitFileStatus]:
    """
    Parse raw ``git status --porcelain=v1 -z`` output into status entries.

    Args:
        raw: Raw stdout from ``git status --porcelain=v1 -z``.
        repo_root: Absolute path to the repository root.
        subtree_path: Optional absolute path to filter results.  Only files
            within this subtree are included.  If None, all entries are returned.

    Returns:
        List of :class:`GitFileStatus` objects, one per changed file.
    """
    entries: list[GitFileStatus] = []

    if not raw:
        return entries

    # -z uses NUL as the record terminator.  For renames the format is:
    #   "XY old\0new\0"  — two NUL-separated tokens for a single entry.
    # For all other statuses it is simply:
    #   "XY path\0"
    tokens = raw.split("\0")
    i = 0
    while i < len(tokens):
        token = tokens[i]
        i += 1

        if len(token) < 4:
            # Empty trailing token after the final NUL, or malformed entry.
            continue

        xy = token[:2]
        rel_path = token[3:]
        code = _xy_to_code(xy)

        original_path: str | None = None

        if code in (GitStatusCode.RENAMED, GitStatusCode.COPIED):
            # The next token is the *source* (original) path.
            if i < len(tokens):
                original_rel = tokens[i]
                i += 1
                original_abs = os.path.normpath(os.path.join(repo_root, original_rel))
                if subtree_path is None or _within_subtree(original_abs, subtree_path):
                    original_path = original_abs

        abs_path = os.path.normpath(os.path.join(repo_root, rel_path))

        if subtree_path is not None and not _within_subtree(abs_path, subtree_path):
            continue

        entries.append(GitFileStatus(code=code, path=abs_path, original_path=original_path))

    return entries


def find_repo_root(path: str, boundary: str) -> str | None:
    """
    Find the root of the git repository containing *path*, but only if it is
    within *boundary*.

    Uses ``git rev-parse --show-toplevel`` to discover the repository root,
    then checks whether that root is equal to or inside *boundary*.  Returns
    ``None`` if no repository is found, or if the discovered root is outside
    the boundary.

    Args:
        path: Absolute path to a file or directory
        boundary: Absolute path to a directory that the repository root must
            be within (typically the mindspace root)

    Returns:
        Absolute path to the repository root, or ``None`` if no repository
        root is found or the root is outside the boundary

    Raises:
        GitNotFoundError: If git is not installed or not on PATH
        GitCommandError: If an unexpected git error occurs
    """
    search_dir = path if os.path.isdir(path) else os.path.dirname(path)

    try:
        output = _run_git(
            ["rev-parse", "--show-toplevel"],
            cwd=search_dir
        )

    except GitCommandError as e:
        if e.returncode == 128:
            return None

        return None

    repo_root = os.path.realpath(output.strip())

    # Check that the repo root is equal to or inside the boundary.
    # Resolve symlinks (e.g. macOS /var -> /private/var) so comparisons are consistent.
    resolved_root = os.path.realpath(repo_root)
    resolved_boundary = os.path.realpath(boundary)

    if sys.platform == "win32":
        resolved_root = resolved_root.lower()
        resolved_boundary = resolved_boundary.lower()

    if resolved_root != resolved_boundary and not resolved_root.startswith(resolved_boundary + os.sep):
        return None

    return repo_root


class GitRepository:
    """
    A git repository identified by its root path.

    Encapsulates the repository root and provides methods for querying its
    state.  All methods operate relative to the root captured at construction,
    so callers never need to pass it explicitly.
    """

    def __init__(self, root: str) -> None:
        """
        Initialise the repository wrapper.

        In most cases callers should use find_repo_root to discover the root rather than
        constructing directly.

        Args:
            root: Absolute path to the repository root
        """
        self._root = root

    def root(self) -> str:
        """Return the absolute path to the repository root."""
        return self._root

    def is_file_tracked(self, file_path: str) -> bool:
        """
        Check whether a file is tracked by git.

        Args:
            file_path: Absolute path to the file

        Returns:
            True if the file is tracked, False if untracked or ignored

        Raises:
            GitNotFoundError: If git is not installed or not on PATH
            GitCommandError: If an unexpected git error occurs
        """
        try:
            self._run_git(
                ["ls-files", "--error-unmatch", "--", file_path]
            )

            return True

        except GitCommandError as e:
            if e.returncode == 1:
                return False

            raise

    def get_file_diff(self, file_path: str, ref: str = "HEAD") -> str:
        """
        Return a unified diff between the HEAD version and the working tree for a file.

        For tracked files, this runs ``git diff HEAD -- <file>``.  For untracked files
        the file content is returned as a synthetic unified diff with every line marked
        as added (equivalent to diffing against /dev/null).

        An empty string is returned when the file is tracked and identical to HEAD (i.e.
        there are no changes to show).

        Args:
            file_path: Absolute path to the file
            ref: Git ref to diff against (default "HEAD")

        Returns:
            Unified diff text, or an empty string if there are no differences

        Raises:
            GitNotFoundError: If git is not installed or not on PATH
            GitNotRepositoryError: If the repository root is not a git repository
            GitCommandError: If an unexpected git error occurs
            OSError: If the file cannot be read (untracked case)
        """
        if self.is_file_tracked(file_path):
            return self._tracked_file_diff(file_path, ref)

        with open(file_path, encoding="utf-8", errors="replace") as f:
            lines = f.readlines()

        rel_path = os.path.basename(file_path)
        num_lines = len(lines)

        header = (
            f"--- /dev/null\n"
            f"+++ b/{rel_path}\n"
            f"@@ -0,0 +1,{num_lines} @@\n"
        )

        body = "".join(f"+{line}" if line.endswith("\n") else f"+{line}\n" for line in lines)

        return header + body

    def get_file_at_head(self, file_path: str) -> str | None:
        """
        Return the content of a file at HEAD, or None if the file is not tracked.

        Uses ``git show HEAD:<relative-path>`` to retrieve the committed version of
        the file.  Returns None for untracked files so callers can treat them as
        having no prior content.

        Args:
            file_path: Absolute path to the file.

        Returns:
            File content as a string, or None if the file is not tracked by git.

        Raises:
            GitNotFoundError: If git is not installed or not on PATH.
            GitNotRepositoryError: If the repository root is not a git repository.
            GitCommandError: If an unexpected git error occurs.
        """
        if not self.is_file_tracked(file_path):
            return None

        rel_path_posix = self._rel_path_posix(file_path)

        try:
            return self._run_git(["show", f"HEAD:{rel_path_posix}"])

        except GitCommandError as e:
            # Return code 128 usually means the ref doesn't exist (e.g. initial commit
            # with no HEAD yet).  Treat that as "no prior content".
            if e.returncode == 128:
                return None

            raise

    def show_file_at_ref(self, file_path: str, ref: str) -> str | None:
        """
        Return the content of a file at an arbitrary git ref, or None if the file
        does not exist at that ref.

        Args:
            file_path: Absolute path to the file.
            ref: Git ref (e.g. ``HEAD``, a commit hash, a branch name, a tag).

        Returns:
            File content as a string, or None if the file does not exist at *ref*.

        Raises:
            GitNotFoundError: If git is not installed or not on PATH.
            GitNotRepositoryError: If the repository root is not a git repository.
            GitCommandError: If an unexpected git error occurs.
        """
        rel_path_posix = self._rel_path_posix(file_path)

        try:
            return self._run_git(["show", f"{ref}:{rel_path_posix}"])

        except GitCommandError as e:
            if e.returncode == 128:
                return None

            raise

    def get_current_branch(self) -> str:
        """
        Return the name of the current branch, or ``HEAD`` if in detached HEAD state.

        Returns:
            Branch name string, or ``HEAD`` for detached HEAD.

        Raises:
            GitNotFoundError: If git is not installed or not on PATH.
            GitNotRepositoryError: If the repository root is not a git repository.
            GitCommandError: If an unexpected git error occurs.
        """
        try:
            return self._run_git(["rev-parse", "--abbrev-ref", "HEAD"]).strip()

        except GitCommandError as e:
            if e.returncode == 128:
                raise GitNotRepositoryError(
                    f"'{self._root}' is not a git repository"
                ) from e

            raise

    def get_branches(self) -> list[str]:
        """
        Return a list of all local branch names in the repository.

        Returns:
            List of branch names, sorted alphabetically.

        Raises:
            GitNotFoundError: If git is not installed or not on PATH.
            GitNotRepositoryError: If the repository root is not a git repository.
            GitCommandError: If an unexpected git error occurs.
        """
        output = self._run_git(
            ["for-each-ref", "--format=%(refname:short)", "refs/heads/"]
        )

        branches = [line.strip() for line in output.splitlines() if line.strip()]

        return sorted(branches)

    def get_remote_branches(self) -> list[str]:
        """
        Return a list of all remote branch names in the repository.

        Returns:
            List of remote branch names (e.g. 'origin/main'), sorted alphabetically.

        Raises:
            GitNotFoundError: If git is not installed or not on PATH.
            GitNotRepositoryError: If the repository root is not a git repository.
            GitCommandError: If an unexpected git error occurs.
        """
        output = self._run_git(
            ["for-each-ref", "--format=%(refname:short)", "refs/remotes/"]
        )

        branches = [line.strip() for line in output.splitlines() if line.strip()]

        return sorted(branches)

    def get_log(self, max_count: int = 50, skip: int = 0, ref: str = "HEAD", path: str | None = None) -> list[GitCommitInfo]:
        """
        Return recent commit history for the repository.

        Uses a custom format delimited by NUL characters to safely handle
        multi-line commit messages.

        Args:
            max_count: Maximum number of commits to return (default 50).
            skip: Number of commits to skip from the tip before starting
                output (default 0).  Allows paging through history.
            ref: Git ref to start the log from (default "HEAD").  May be a
                branch name, tag, or commit hash.
            path: Optional absolute path to a file.  If given, only commits
                that touched this file are returned.

        Returns:
            List of GitCommitInfo objects, most recent first.

        Raises:
            GitNotFoundError: If git is not installed or not on PATH.
            GitNotRepositoryError: If the repository root is not a git repository.
            GitCommandError: If an unexpected git error occurs.
        """
        sep = "\x1e"  # ASCII record separator
        field_sep = "\x1f"  # ASCII unit separator

        fmt = field_sep.join(["%H", "%an", "%ae", "%aI", "%s"]) + sep

        args = ["log", f"--max-count={max_count}", f"--format={fmt}"]

        if skip > 0:
            args.append(f"--skip={skip}")

        args.append(ref)

        if path is not None:
            rel_path = self._rel_path_posix(path)
            args.append("--")
            args.append(rel_path)

        output = self._run_git(args)

        commits: list[GitCommitInfo] = []

        for record in output.split(sep):
            record = record.strip()
            if not record:
                continue

            parts = record.split(field_sep)
            if len(parts) < 5:
                continue

            commits.append(GitCommitInfo(
                hash=parts[0],
                author_name=parts[1],
                author_email=parts[2],
                author_date=parts[3],
                subject=parts[4],
            ))

        return commits

    def get_status(self, subtree_path: str | None = None) -> list[GitFileStatus]:
        """
        Return the list of changed files within the repository.

        Runs ``git status --porcelain=v1 -z`` and parses the result.

        Args:
            subtree_path: Optional absolute path to a directory.  Only files within
                this subtree are included in the result.  If None, all changed files
                in the repository are returned.

        Returns:
            List of GitFileStatus objects, one per changed file.

        Raises:
            GitNotFoundError: If git is not installed or not on PATH.
            GitCommandError: If the git command fails unexpectedly.
        """
        raw = self._run_git(
            ["status", "--porcelain=v1", "-z", "--untracked-files=all"]
        )

        return _parse_status(raw, self._root, subtree_path)

    def get_changed_files_at_ref(self, ref: str) -> list[GitFileStatus]:
        """
        Return the list of files changed in the commit at *ref*.

        Runs ``git diff-tree --no-commit-id --name-status --root -r -z <ref>`` and
        parses the result into GitFileStatus objects.

        Args:
            ref: Git ref identifying the commit to inspect.

        Returns:
            List of GitFileStatus objects, one per changed file.

        Raises:
            GitNotFoundError: If git is not installed or not on PATH.
            GitCommandError: If an unexpected git error occurs.
        """
        raw = self._run_git(
            ["diff-tree", "--no-commit-id", "--name-status", "--root", "-r", "-z", ref]
        )

        entries: list[GitFileStatus] = []

        if not raw:
            return entries

        tokens = raw.split("\0")
        i = 0
        while i < len(tokens):
            status = tokens[i]
            i += 1

            if not status:
                continue

            code = _diff_tree_status_to_code(status[0])

            if code in (GitStatusCode.RENAMED, GitStatusCode.COPIED):
                if i + 1 < len(tokens):
                    original_rel = tokens[i]
                    new_rel = tokens[i + 1]
                    i += 2
                    original_abs = os.path.normpath(os.path.join(self._root, original_rel))
                    new_abs = os.path.normpath(os.path.join(self._root, new_rel))
                    entries.append(GitFileStatus(code=code, path=new_abs, original_path=original_abs))

            else:
                if i < len(tokens):
                    rel_path = tokens[i]
                    i += 1
                    abs_path = os.path.normpath(os.path.join(self._root, rel_path))
                    entries.append(GitFileStatus(code=code, path=abs_path, original_path=None))

        return entries

    def _run_git(self, args: list[str]) -> str:
        """
        Run a git command in this repository.

        Args:
            args: Git arguments (excluding the 'git' executable itself)

        Returns:
            Stdout output as a string

        Raises:
            GitNotFoundError: If git is not installed or not on PATH
            GitCommandError: If the command exits with a non-zero return code
        """
        return _run_git(args, self._root)

    def _tracked_file_diff(self, file_path: str, ref: str = "HEAD") -> str:
        """
        Return the unified diff for a tracked file against HEAD.

        Args:
            file_path: Absolute path to the file
            ref: Git ref to diff against (default "HEAD")

        Returns:
            Unified diff text, or empty string if the file is unchanged
        """
        try:
            return self._run_git(["diff", ref, "--", file_path])

        except GitCommandError as e:
            # Return code 1 from plain diff means differences exist and is normal; git diff
            # HEAD uses return code 0 for both changed and unchanged, so any non-zero here
            # is genuinely unexpected.
            raise GitCommandError(
                f"Unexpected error getting diff for '{file_path}'",
                returncode=e.returncode,
                stderr=e.stderr
            ) from e

    def _rel_path_posix(self, file_path: str) -> str:
        """
        Convert an absolute file path to a repo-relative POSIX path.

        Args:
            file_path: Absolute path to a file within the repository

        Returns:
            Forward-slash relative path suitable for git object refs
        """
        # Resolve symlinks so the path is consistent with the (already-resolved) root
        resolved_file_path = os.path.realpath(file_path)
        rel_path = os.path.relpath(resolved_file_path, self._root)

        return rel_path.replace(os.sep, "/")
