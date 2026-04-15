"""Git working-tree status query."""

import os
from dataclasses import dataclass
from enum import Enum, auto

from git.git_error import GitCommandError
from git.git_repository import _run_git


class VcsStatusCode(Enum):
    """Normalised status category for a changed file."""
    MODIFIED = auto()
    ADDED = auto()
    DELETED = auto()
    RENAMED = auto()
    COPIED = auto()
    UNTRACKED = auto()
    UNKNOWN = auto()


@dataclass(frozen=True)
class VcsFileStatus:
    """Status information for a single file reported by git."""
    code: VcsStatusCode
    path: str                       # Absolute path to the file (new path for renames)
    original_path: str | None       # Absolute path to the original file (renames/copies only)


def _xy_to_code(xy: str) -> VcsStatusCode:
    """
    Map a porcelain XY status string to a VcsStatusCode.

    The X column is the index (staged) status; the Y column is the
    working-tree status.  We collapse both into a single category,
    preferring the working-tree state when both are set.

    Args:
        xy: Two-character porcelain status string.

    Returns:
        Corresponding VcsStatusCode.
    """
    if xy == "??":
        return VcsStatusCode.UNTRACKED

    x, y = xy[0], xy[1]

    for char in (y, x):
        if char == "M":
            return VcsStatusCode.MODIFIED

        if char == "A":
            return VcsStatusCode.ADDED

        if char == "D":
            return VcsStatusCode.DELETED

        if char == "R":
            return VcsStatusCode.RENAMED

        if char == "C":
            return VcsStatusCode.COPIED

    return VcsStatusCode.UNKNOWN


def get_status(repo_root: str, mindspace_path: str) -> list[VcsFileStatus]:
    """
    Return the list of changed files within *mindspace_path*.

    Runs ``git status --porcelain=v1 -z`` and filters results to only
    those files that live inside *mindspace_path*.  Paths are returned
    as absolute paths.

    Args:
        repo_root: Absolute path to the git repository root.
        mindspace_path: Absolute path to the mindspace directory.  Only
            files within this subtree are included in the result.

    Returns:
        List of VcsFileStatus objects, one per changed file.

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the git command fails unexpectedly.
    """
    try:
        raw = _run_git(
            ["status", "--porcelain=v1", "-z", "--untracked-files=all"],
            cwd=repo_root
        )
    except GitCommandError as e:
        raise e

    entries: list[VcsFileStatus] = []

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

        if code in (VcsStatusCode.RENAMED, VcsStatusCode.COPIED):
            # The next token is the *source* (original) path.
            if i < len(tokens):
                original_rel = tokens[i]
                i += 1
                original_abs = os.path.normpath(os.path.join(repo_root, original_rel))
                if _within_mindspace(original_abs, mindspace_path):
                    original_path = original_abs

        abs_path = os.path.normpath(os.path.join(repo_root, rel_path))

        if not _within_mindspace(abs_path, mindspace_path):
            continue

        entries.append(VcsFileStatus(code=code, path=abs_path, original_path=original_path))

    return entries


def _within_mindspace(abs_path: str, mindspace_path: str) -> bool:
    """
    Return True if *abs_path* is inside *mindspace_path*.

    Args:
        abs_path: Absolute path to test.
        mindspace_path: Absolute mindspace root path.

    Returns:
        True if abs_path is within mindspace_path.
    """
    try:
        common = os.path.commonpath([abs_path, mindspace_path])
        return common == mindspace_path

    except ValueError:
        return False
