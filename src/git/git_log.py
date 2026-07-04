"""Git commit history queries."""

from dataclasses import dataclass

from git.git_error import GitCommandError
from git.git_repository import _run_git
from git.git_status import VCSStatusCode


# Field and record separators for the log format (ASCII US / RS).
_FIELD_SEP = "\x1f"
_RECORD_SEP = "\x1e"

_LOG_FORMAT = _FIELD_SEP.join([
    "%H",   # full hash
    "%h",   # abbreviated hash
    "%P",   # parent hashes (space-separated)
    "%an",  # author name
    "%ae",  # author email
    "%at",  # author timestamp (epoch seconds)
    "%s",   # subject
]) + _RECORD_SEP


@dataclass(frozen=True)
class CommitInfo:
    """Metadata for a single commit in the log."""
    commit_hash: str
    short_hash: str
    parents: list[str]
    author_name: str
    author_email: str
    timestamp: int          # Author time, epoch seconds
    subject: str


@dataclass(frozen=True)
class CommitFileChange:
    """A single file changed by a commit."""
    code: VCSStatusCode
    path: str                       # Path relative to repo root (new path for renames)
    original_path: str | None = None  # Original path for renames/copies


@dataclass(frozen=True)
class BlameLine:
    """Authorship for a single line of a file."""
    line_number: int
    commit_hash: str
    short_hash: str
    author: str
    content: str


def get_log(repo_root: str, max_count: int = 200, skip: int = 0, path: str | None = None) -> list[CommitInfo]:
    """
    Return commits reachable from HEAD, newest first.

    Args:
        repo_root: Absolute path to the repository root.
        max_count: Maximum number of commits to return.
        skip: Number of commits to skip (for paging).
        path: If given, only commits that touched this path are returned
            (file history).

    Returns:
        List of CommitInfo, newest first.  Empty for a repo with no commits.

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the command fails unexpectedly.
    """
    args = [
        "log",
        f"--pretty=format:{_LOG_FORMAT}",
        f"--max-count={max_count}",
        f"--skip={skip}",
    ]
    if path:
        args += ["--", path]

    try:
        raw = _run_git(args, cwd=repo_root)

    except GitCommandError as e:
        # 128 on a repository with no commits yet (no HEAD).
        if e.returncode == 128:
            return []

        raise

    return _parse_log_records(raw)


def get_head_message(repo_root: str) -> str:
    """
    Return the full commit message (subject and body) of HEAD.

    Args:
        repo_root: Absolute path to the repository root.

    Returns:
        The full HEAD commit message, or "" if the repository has no commits.

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the command fails unexpectedly.
    """
    try:
        raw = _run_git(["log", "-1", "--pretty=format:%B"], cwd=repo_root)

    except GitCommandError as e:
        # 128 on a repository with no commits yet (no HEAD).
        if e.returncode == 128:
            return ""

        raise

    return raw.strip("\n")


def _parse_log_records(raw: str) -> list[CommitInfo]:
    """Parse the record-separated ``git log`` output into CommitInfo objects."""
    commits: list[CommitInfo] = []
    for record in raw.split(_RECORD_SEP):
        record = record.strip("\n")
        if not record:
            continue

        fields = record.split(_FIELD_SEP)
        if len(fields) < 7:
            continue

        full, short, parents, name, email, ts, subject = fields[:7]
        commits.append(CommitInfo(
            commit_hash=full,
            short_hash=short,
            parents=parents.split() if parents else [],
            author_name=name,
            author_email=email,
            timestamp=int(ts) if ts.isdigit() else 0,
            subject=subject,
        ))

    return commits


def get_commit_files(repo_root: str, commit_hash: str) -> list[CommitFileChange]:
    """
    Return the files changed by a commit.

    Args:
        repo_root: Absolute path to the repository root.
        commit_hash: The commit to inspect.

    Returns:
        List of CommitFileChange for the commit (against its first parent;
        for the root commit, every file appears as added).

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the command fails unexpectedly.
    """
    # `git show --first-parent` reports files for ordinary commits, the root
    # commit (all additions), and merge commits (vs their first parent) — unlike
    # diff-tree, which emits nothing for merges.  --format= drops the header.
    raw = _run_git(
        ["show", "--first-parent", "--name-status", "--format=", "-z", commit_hash],
        cwd=repo_root,
    )
    return _parse_name_status_z(raw)


def get_commit_file_diff(repo_root: str, commit_hash: str, file_path: str) -> str:
    """
    Return the unified diff a commit applied to a single file.

    Uses ``git show`` with an empty format so only the diff (no commit header)
    is returned.  Works for the root commit (shows the file as fully added).

    Args:
        repo_root: Absolute path to the repository root.
        commit_hash: The commit to inspect.
        file_path: Path (relative to repo root) of the file.

    Returns:
        Unified diff text for the file in that commit.

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the command fails unexpectedly.
    """
    # --first-parent makes merge commits show their diff against the first
    # parent instead of an empty combined diff.
    return _run_git(
        ["show", "--first-parent", "--format=", commit_hash, "--", file_path],
        cwd=repo_root,
    )


def get_ref_diff_files(repo_root: str, ref1: str, ref2: str) -> list[CommitFileChange]:
    """
    Return the files that differ between two refs (``git diff --name-status``).

    Args:
        repo_root: Absolute path to the repository root.
        ref1: The base ref (branch/commit/tag).
        ref2: The target ref.

    Returns:
        List of CommitFileChange describing ref1 -> ref2.

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the command fails unexpectedly.
    """
    raw = _run_git(
        ["diff", "--name-status", "-r", "-z", ref1, ref2],
        cwd=repo_root,
    )
    return _parse_name_status_z(raw)


def get_ref_file_diff(repo_root: str, ref1: str, ref2: str, file_path: str) -> str:
    """
    Return the diff of a single file between two refs.

    Args:
        repo_root: Absolute path to the repository root.
        ref1: The base ref.
        ref2: The target ref.
        file_path: Path (relative to repo root) of the file.

    Returns:
        Unified diff text.

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the command fails unexpectedly.
    """
    return _run_git(["diff", ref1, ref2, "--", file_path], cwd=repo_root)


def get_blame(repo_root: str, file_path: str) -> list[BlameLine]:
    """
    Return per-line authorship for a file (``git blame --line-porcelain``).

    Args:
        repo_root: Absolute path to the repository root.
        file_path: Absolute or repo-relative path to the file.

    Returns:
        List of BlameLine in file order.

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the command fails unexpectedly.
    """
    raw = _run_git(["blame", "--line-porcelain", "--", file_path], cwd=repo_root)

    lines: list[BlameLine] = []
    commit_hash = ""
    final_line = 0
    author = ""

    for line in raw.splitlines():
        parts = line.split(" ")
        if len(parts) >= 3 and len(parts[0]) == 40 and _is_hex(parts[0]):
            commit_hash = parts[0]
            final_line = int(parts[2]) if parts[2].isdigit() else 0

        elif line.startswith("author "):
            author = line[len("author "):]

        elif line.startswith("\t"):
            lines.append(BlameLine(
                line_number=final_line,
                commit_hash=commit_hash,
                short_hash=commit_hash[:8],
                author=author,
                content=line[1:],
            ))

    return lines


def _is_hex(text: str) -> bool:
    """Return True if *text* is all hex digits."""
    try:
        int(text, 16)
        return True

    except ValueError:
        return False


def _parse_name_status_z(raw: str) -> list[CommitFileChange]:
    """
    Parse NUL-separated ``--name-status -z`` output into CommitFileChange list.

    Handles rename/copy entries (status letter R/C followed by two path tokens).

    Args:
        raw: The raw NUL-separated name-status output.

    Returns:
        List of CommitFileChange.
    """
    changes: list[CommitFileChange] = []
    tokens = raw.split("\0")
    i = 0
    while i < len(tokens):
        status = tokens[i]
        i += 1
        if not status:
            continue

        code = _name_status_to_code(status[0])
        if status[0] in ("R", "C"):
            # Rename/copy: two path tokens (old, new).
            if i + 1 < len(tokens):
                original, new_path = tokens[i], tokens[i + 1]
                i += 2
                changes.append(CommitFileChange(code=code, path=new_path, original_path=original))

        elif i < len(tokens):
            changes.append(CommitFileChange(code=code, path=tokens[i]))
            i += 1

    return changes


def _name_status_to_code(char: str) -> VCSStatusCode:
    """Map a diff name-status letter to a VCSStatusCode."""
    mapping = {
        "A": VCSStatusCode.ADDED,
        "M": VCSStatusCode.MODIFIED,
        "D": VCSStatusCode.DELETED,
        "R": VCSStatusCode.RENAMED,
        "C": VCSStatusCode.COPIED,
    }
    return mapping.get(char, VCSStatusCode.UNKNOWN)
