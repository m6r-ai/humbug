"""Git repository operations."""

import os
import sys
import subprocess

from git.git_error import GitCommandError, GitNotFoundError, GitNotRepositoryError


_GIT_TIMEOUT = 10  # seconds

# Network-bound operations (fetch/pull/push) can legitimately take much longer
# than local queries, so they use an extended timeout.
_GIT_NETWORK_TIMEOUT = 120  # seconds


def _run_git(
    args: list[str],
    cwd: str,
    timeout: int = _GIT_TIMEOUT,
    input_text: str | None = None,
    env: dict[str, str] | None = None,
) -> str:
    """
    Run a git command and return its stdout.

    Args:
        args: Git arguments (excluding the 'git' executable itself)
        cwd: Working directory for the command
        timeout: Maximum time in seconds to wait for the command to complete
        input_text: Optional text piped to the command's stdin (e.g. a patch)
        env: Optional extra environment variables merged over the current
            environment (e.g. GIT_EDITOR=true to keep git non-interactive)

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

    run_env = None
    if env:
        run_env = {**os.environ, **env}

    try:
        result = subprocess.run(
            ["git"] + args,
            cwd=cwd,
            check=False,
            capture_output=True,
            text=True,
            encoding="utf-8",
            timeout=timeout,
            creationflags=creationflags,
            input=input_text,
            env=run_env,
        )

    except FileNotFoundError as e:
        raise GitNotFoundError("git executable not found on PATH") from e

    except subprocess.TimeoutExpired as e:
        raise GitCommandError(
            f"git command timed out after {timeout}s",
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


def init_repository(path: str) -> None:
    """
    Initialise a new git repository at *path*.

    Args:
        path: Absolute path to an existing directory to turn into a repository.

    Raises:
        GitNotFoundError: If git is not installed or not on PATH
        GitCommandError: If the command fails
    """
    _run_git(["init"], cwd=path)


def clone_repository(url: str, parent_dir: str, timeout: int = 300) -> None:
    """
    Clone *url* into a new subdirectory of *parent_dir*.

    Args:
        url: The repository URL to clone.
        parent_dir: Existing directory to clone into (git creates a subfolder).
        timeout: Maximum time in seconds to allow for the clone.

    Raises:
        GitNotFoundError: If git is not installed or not on PATH
        GitCommandError: If the clone fails
    """
    _run_git(["clone", url], cwd=parent_dir, timeout=timeout)


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


# Directories that never contain a project repo worth surfacing and are
# expensive to descend into.  The scan skips these outright.
_SCAN_SKIP_DIRS = frozenset({
    ".git", "node_modules", "__pycache__", ".venv", "venv", ".tox",
    ".mypy_cache", ".pytest_cache", "build", "dist", ".idea",
})


def find_repositories(root: str, max_depth: int = 4) -> list[str]:
    """
    Find all git repositories at or below *root*.

    Walks the directory tree looking for directories that contain a ``.git``
    entry.  Descent continues *past* a discovered repository so that nested
    repositories (for example a project cloned inside a mindspace that is itself
    a repo) are surfaced too.  Descent stops at *max_depth* levels below *root*
    and at well-known heavy directories.

    Args:
        root: Absolute path to the directory to scan (e.g. a mindspace root).
        max_depth: Maximum directory depth below *root* to search.

    Returns:
        Sorted list of absolute repository root paths.  A repository at *root*
        itself is included.
    """
    if not os.path.isdir(root):
        return []

    root = os.path.normpath(root)
    found: list[str] = []
    _scan_for_repos(root, root, max_depth, found)
    return sorted(found)


def _scan_for_repos(current: str, root: str, max_depth: int, found: list[str]) -> None:
    """
    Recursively collect repository roots into *found*.

    A directory that is itself a repository is recorded and still descended
    into, so nested repositories are discovered.  The ``.git`` directory itself
    is never entered (it is in the skip set).

    Args:
        current: Directory currently being inspected.
        root: The original scan root, used to compute depth.
        max_depth: Maximum depth below root to descend.
        found: Accumulator list of discovered repository roots.
    """
    # A repository's marker is ``.git`` — a directory normally, but a file for
    # linked worktrees and submodules (a "gitdir:" pointer).
    if os.path.exists(os.path.join(current, ".git")):
        found.append(current)

    depth = 0 if current == root else current[len(root):].count(os.sep)
    if depth >= max_depth:
        return

    try:
        entries = os.scandir(current)

    except OSError:
        return

    with entries:
        for entry in entries:
            if not entry.is_dir(follow_symlinks=False):
                continue

            if entry.name in _SCAN_SKIP_DIRS or entry.name.startswith("."):
                continue

            _scan_for_repos(entry.path, root, max_depth, found)


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


def get_unstaged_file_diff(repo_root: str, file_path: str) -> str:
    """
    Return the unstaged diff for a file (index vs working tree).

    Args:
        repo_root: Absolute path to the repository root.
        file_path: Absolute path to the file.

    Returns:
        Unified diff text (empty if there are no unstaged changes).

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the command fails unexpectedly.
    """
    return _run_git(["diff", "--", file_path], cwd=repo_root)


def get_staged_file_diff(repo_root: str, file_path: str) -> str:
    """
    Return the staged diff for a file (HEAD vs index).

    Args:
        repo_root: Absolute path to the repository root.
        file_path: Absolute path to the file.

    Returns:
        Unified diff text (empty if nothing is staged for the file).

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the command fails unexpectedly.
    """
    return _run_git(["diff", "--cached", "--", file_path], cwd=repo_root)


def split_file_diff(diff_text: str) -> tuple[str, list[str]]:
    """
    Split a single-file unified diff into its header and hunks.

    Args:
        diff_text: The unified diff for one file.

    Returns:
        A tuple of (header, hunks) where *header* is the text preceding the
        first ``@@`` hunk (the ``diff --git`` / ``---`` / ``+++`` lines) and
        *hunks* is a list of hunk texts each starting with ``@@``.  Rejoining
        the header with any subset of hunks yields an applyable patch.
    """
    lines = diff_text.splitlines(keepends=True)
    header_lines: list[str] = []
    hunks: list[str] = []
    current: list[str] = []
    in_hunk = False

    for line in lines:
        if line.startswith("@@"):
            if current:
                hunks.append("".join(current))

            current = [line]
            in_hunk = True

        elif in_hunk:
            current.append(line)

        else:
            header_lines.append(line)

    if current:
        hunks.append("".join(current))

    return "".join(header_lines), hunks


def get_file_at_head(repo_root: str, file_path: str) -> str | None:
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
