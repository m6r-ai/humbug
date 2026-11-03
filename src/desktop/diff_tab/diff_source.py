"""
Sources of diff content for the diff view.

A diff view shows two versions of a text file side by side.  Which two versions
depends on what the reader wants to compare, so that choice lives in a source
object rather than inside the view: the working tree against HEAD, or a recorded
earlier version against the file as it stands now.
"""

from difflib import unified_diff

from git import GitCommandError, GitNotFoundError, GitRepository, find_repo_root
from mindspace.mindspace import Mindspace

DiffContent = tuple[list[str], list[str], str]


class DiffSourceError(Exception):
    """Raised when a source cannot produce diff content, with a display message."""


class GitWorkingTreeDiffSource:
    """
    Compares a file's working-tree content against its content at HEAD.

    A file with no HEAD version, such as an untracked one, has no old side.
    """

    def __init__(self, path: str, mindspace_path: str) -> None:
        """
        Initialise the source.

        Args:
            path: Absolute path to the file to compare.
            mindspace_path: Root of the open mindspace, used to bound repository
                discovery.
        """
        self._path = path
        self._mindspace_path = mindspace_path

    def fetch(self) -> DiffContent:
        """
        Return the HEAD content, the working-tree content, and their diff.

        Raises:
            DiffSourceError: The file is outside a repository, git is unavailable,
                or a version could not be read.
        """
        try:
            repo_root = find_repo_root(self._path, self._mindspace_path)
            if repo_root is None:
                raise DiffSourceError("This file is not inside a git repository.")

            repo = GitRepository(repo_root)
            diff_text = repo.get_file_diff(self._path)

            head_content = repo.get_file_at_head(self._path)
            old_lines = head_content.splitlines() if head_content is not None else []

            with open(self._path, encoding="utf-8", errors="replace") as f:
                new_lines = f.read().splitlines()

            return old_lines, new_lines, diff_text

        except GitNotFoundError as e:
            raise DiffSourceError("git is not available on this system.") from e

        except GitCommandError as e:
            raise DiffSourceError(f"git error: {e}") from e

        except OSError as e:
            raise DiffSourceError(f"Could not read file: {e}") from e


class JournalDiffSource:
    """
    Compares a version the change journal recorded against the file as it is now.

    The journal stores what a change replaced, so this answers "what did that
    change do?" for a file that has since moved on.
    """

    def __init__(self, path: str, sequence: int, mindspace: Mindspace) -> None:
        """
        Initialise the source.

        Args:
            path: Absolute path to the file to compare.
            sequence: Journal sequence number of the entry whose earlier version
                should be compared against the current file.
            mindspace: The open mindspace holding the journal.
        """
        self._path = path
        self._sequence = sequence
        self._mindspace = mindspace

    def fetch(self) -> DiffContent:
        """
        Return the recorded version, the current content, and their diff.

        Raises:
            DiffSourceError: The entry cannot be found, or its earlier content is
                no longer available.
        """
        relative_path = self._mindspace.get_mindspace_relative_path(self._path)
        if relative_path is None:
            raise DiffSourceError("This file is not inside the mindspace.")

        entry = next(
            (
                candidate
                for candidate in self._mindspace.file_journal().entries_for_path(relative_path)
                if candidate.sequence == self._sequence
            ),
            None,
        )
        if entry is None:
            raise DiffSourceError("The recorded change can no longer be found.")

        content = self._mindspace.journal_pre_image(entry)
        if content is None:
            raise DiffSourceError("The version this change replaced is no longer available.")

        old_lines = content.decode("utf-8", errors="replace").splitlines()

        try:
            with open(self._path, encoding="utf-8", errors="replace") as f:
                new_lines = f.read().splitlines()

        except OSError:
            new_lines = []

        diff_lines = list(
            unified_diff(
                old_lines,
                new_lines,
                fromfile=f"a/{relative_path} (replaced)",
                tofile=f"b/{relative_path} (now)",
                lineterm="",
            )
        )

        return old_lines, new_lines, "\n".join(diff_lines)
