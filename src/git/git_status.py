"""Git working-tree status query."""

import os
import sys
from dataclasses import dataclass
from enum import Enum, auto

from git.git_error import GitCommandError
from git.git_repository import _run_git


class VCSStatusCode(Enum):
    """Normalised status category for a changed file."""
    MODIFIED = auto()
    ADDED = auto()
    DELETED = auto()
    RENAMED = auto()
    COPIED = auto()
    UNTRACKED = auto()
    CONFLICTED = auto()
    UNKNOWN = auto()


# Porcelain XY combinations that indicate an unmerged (conflicted) file.
_UNMERGED_STATES = frozenset({"DD", "AU", "UD", "UA", "DU", "AA", "UU"})


@dataclass(frozen=True)
class VCSFileStatus:
    """Status information for a single file reported by git."""
    code: VCSStatusCode
    path: str                       # Absolute path to the file (new path for renames)
    original_path: str | None       # Absolute path to the original file (renames/copies only)
    # Per-column codes from the porcelain XY status.  ``index_code`` is None when
    # the file has no staged change (X column blank); ``worktree_code`` is None
    # when the working tree matches the index (Y column blank).  These let a
    # staging UI split files into "Staged" and "Changes" groups.
    index_code: VCSStatusCode | None = None
    worktree_code: VCSStatusCode | None = None
    is_conflicted: bool = False

    def is_staged(self) -> bool:
        """Return True if this file has changes staged in the index."""
        return self.index_code is not None

    def is_unstaged(self) -> bool:
        """Return True if this file has unstaged working-tree changes."""
        return self.worktree_code is not None


def _char_to_code(char: str) -> VCSStatusCode | None:
    """
    Map a single porcelain status character to a VCSStatusCode.

    Args:
        char: One character from the X or Y status column.

    Returns:
        The corresponding VCSStatusCode, or None if the column is blank.
    """
    mapping = {
        "M": VCSStatusCode.MODIFIED,
        "A": VCSStatusCode.ADDED,
        "D": VCSStatusCode.DELETED,
        "R": VCSStatusCode.RENAMED,
        "C": VCSStatusCode.COPIED,
        "?": VCSStatusCode.UNTRACKED,
    }

    if char in (" ", ""):
        return None

    return mapping.get(char, VCSStatusCode.UNKNOWN)


def _xy_to_code(xy: str) -> VCSStatusCode:
    """
    Map a porcelain XY status string to a VCSStatusCode.

    The X column is the index (staged) status; the Y column is the
    working-tree status.  We collapse both into a single category,
    preferring the working-tree state when both are set.

    Args:
        xy: Two-character porcelain status string.

    Returns:
        Corresponding VCSStatusCode.
    """
    if xy == "??":
        return VCSStatusCode.UNTRACKED

    x, y = xy[0], xy[1]

    for char in (y, x):
        if char == "M":
            return VCSStatusCode.MODIFIED

        if char == "A":
            return VCSStatusCode.ADDED

        if char == "D":
            return VCSStatusCode.DELETED

        if char == "R":
            return VCSStatusCode.RENAMED

        if char == "C":
            return VCSStatusCode.COPIED

    return VCSStatusCode.UNKNOWN


def get_status(repo_root: str, subtree_path: str) -> list[VCSFileStatus]:
    """
    Return the list of changed files within *subtree_path*.

    Runs ``git status --porcelain=v1 -z`` and filters results to only
    those files that live inside *subtree_path*.  Paths are returned
    as absolute paths.

    Args:
        repo_root: Absolute path to the git repository root.
        subtree_path: Absolute path to a directory.  Only files within
            this subtree are included in the result.

    Returns:
        List of VCSFileStatus objects, one per changed file.

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

    entries: list[VCSFileStatus] = []

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
        is_conflicted = xy in _UNMERGED_STATES

        # Untracked files ("??") have no staged component; git reports them in
        # the worktree column only.  For all other statuses the X column is the
        # index (staged) state and the Y column is the working-tree state.
        # Conflicted files are surfaced separately, so they carry neither a
        # staged nor an unstaged code (keeping them out of those groups).
        if is_conflicted:
            code = VCSStatusCode.CONFLICTED
            index_code = None
            worktree_code = None

        elif xy == "??":
            code = VCSStatusCode.UNTRACKED
            index_code = None
            worktree_code = VCSStatusCode.UNTRACKED

        else:
            code = _xy_to_code(xy)
            index_code = _char_to_code(xy[0])
            worktree_code = _char_to_code(xy[1])

        original_path: str | None = None

        if code in (VCSStatusCode.RENAMED, VCSStatusCode.COPIED):
            # The next token is the *source* (original) path.
            if i < len(tokens):
                original_rel = tokens[i]
                i += 1
                original_abs = os.path.normpath(os.path.join(repo_root, original_rel))
                if _within_subtree(original_abs, subtree_path):
                    original_path = original_abs

        abs_path = os.path.normpath(os.path.join(repo_root, rel_path))

        if not _within_subtree(abs_path, subtree_path):
            continue

        entries.append(VCSFileStatus(
            code=code,
            path=abs_path,
            original_path=original_path,
            index_code=index_code,
            worktree_code=worktree_code,
            is_conflicted=is_conflicted,
        ))

    return entries


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

    if sys.platform == "win32":
        abs_path = os.path.normpath(abs_path).lower()
        subtree_path = subtree_path.lower()

    try:
        common = os.path.commonpath([abs_path, subtree_path])
        return common == subtree_path

    except ValueError:
        return False
