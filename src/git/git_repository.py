"""Git repository operations."""

import os
import sys
import subprocess
from typing import List, Optional

from git.git_error import GitCommandError, GitNotFoundError, GitNotRepositoryError


_GIT_TIMEOUT = 10  # seconds


def _run_git(args: List[str], cwd: str) -> str:
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


def find_repo_root(path: str) -> str:
    """
    Find the root of the git repository containing the given path.

    Args:
        path: Absolute path to a file or directory

    Returns:
        Absolute path to the repository root

    Raises:
        GitNotFoundError: If git is not installed or not on PATH
        GitNotRepositoryError: If the path is not within a git repository
    """
    search_dir = path if os.path.isdir(path) else os.path.dirname(path)

    try:
        output = _run_git(
            ["rev-parse", "--show-toplevel"],
            cwd=search_dir
        )
        return os.path.normpath(output.strip())

    except GitCommandError as e:
        if e.returncode == 128:
            raise GitNotRepositoryError(
                f"'{path}' is not within a git repository"
            ) from e
        raise


def is_file_tracked(repo_root: str, file_path: str) -> bool:
    """
    Check whether a file is tracked by git.

    Args:
        repo_root: Absolute path to the repository root
        file_path: Absolute path to the file

    Returns:
        True if the file is tracked, False if untracked or ignored

    Raises:
        GitNotFoundError: If git is not installed or not on PATH
        GitCommandError: If an unexpected git error occurs
    """
    try:
        _run_git(
            ["ls-files", "--error-unmatch", "--", file_path],
            cwd=repo_root
        )
        return True

    except GitCommandError as e:
        if e.returncode == 1:
            return False

        raise


def get_file_diff(repo_root: str, file_path: str) -> str:
    """
    Return a unified diff between the HEAD version and the working tree for a file.

    For tracked files, this runs ``git diff HEAD -- <file>``.  For untracked files
    the file content is returned as a synthetic unified diff with every line marked
    as added (equivalent to diffing against /dev/null).

    An empty string is returned when the file is tracked and identical to HEAD (i.e.
    there are no changes to show).

    Args:
        repo_root: Absolute path to the repository root
        file_path: Absolute path to the file

    Returns:
        Unified diff text, or an empty string if there are no differences

    Raises:
        GitNotFoundError: If git is not installed or not on PATH
        GitNotRepositoryError: If repo_root is not a git repository
        GitCommandError: If an unexpected git error occurs
        OSError: If the file cannot be read (untracked case)
    """
    if is_file_tracked(repo_root, file_path):
        return _tracked_file_diff(repo_root, file_path)

    return _untracked_file_diff(file_path)


def _tracked_file_diff(repo_root: str, file_path: str) -> str:
    """
    Return the unified diff for a tracked file against HEAD.

    Args:
        repo_root: Absolute path to the repository root
        file_path: Absolute path to the file

    Returns:
        Unified diff text, or empty string if the file is unchanged
    """
    try:
        return _run_git(
            ["diff", "HEAD", "--", file_path],
            cwd=repo_root
        )

    except GitCommandError as e:
        # Return code 1 from plain diff means differences exist and is normal; git diff
        # HEAD uses return code 0 for both changed and unchanged, so any non-zero here
        # is genuinely unexpected.
        raise GitCommandError(
            f"Unexpected error getting diff for '{file_path}'",
            returncode=e.returncode,
            stderr=e.stderr
        ) from e


def _untracked_file_diff(file_path: str) -> str:
    """
    Build a synthetic unified diff that presents an untracked file as fully added.

    Args:
        file_path: Absolute path to the untracked file

    Returns:
        Unified diff text showing all lines as additions
    """
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


def get_file_at_head(repo_root: str, file_path: str) -> Optional[str]:
    """
    Return the content of a file at HEAD, or None if the file is not tracked.

    Uses ``git show HEAD:<relative-path>`` to retrieve the committed version of
    the file.  Returns None for untracked files so callers can treat them as
    having no prior content.

    Args:
        repo_root: Absolute path to the repository root.
        file_path: Absolute path to the file.

    Returns:
        File content as a string, or None if the file is not tracked by git.

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitNotRepositoryError: If repo_root is not a git repository.
        GitCommandError: If an unexpected git error occurs.
    """
    if not is_file_tracked(repo_root, file_path):
        return None

    rel_path = os.path.relpath(file_path, repo_root)
    # Use forward slashes — git always expects POSIX-style paths in object refs.
    rel_path_posix = rel_path.replace(os.sep, "/")

    try:
        return _run_git(
            ["show", f"HEAD:{rel_path_posix}"],
            cwd=repo_root
        )

    except GitCommandError as e:
        # Return code 128 usually means the ref doesn't exist (e.g. initial commit
        # with no HEAD yet).  Treat that as "no prior content".
        if e.returncode == 128:
            return None

        raise
