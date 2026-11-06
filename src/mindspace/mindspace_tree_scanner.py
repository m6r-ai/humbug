"""Metadata snapshots of a mindspace tree, used to detect changes no journal entry explains."""

from dataclasses import dataclass, field
import os
import stat
import time

from path_matching import PathPatterns


@dataclass(frozen=True)
class TreeEntry:
    """
    What a metadata walk records about one file.

    The tuple is a prefilter, not a record of when something happened: it decides
    whether a file has to be read, and a file timestamp is never evidence of when
    it changed.  ``ctime`` carries most of the weight on Unix, where userland
    cannot set it, and carries none on Windows, where it is the creation time.

    Attributes:
        inode: The file's inode, which changes when it is replaced.
        size: Size in bytes.
        mtime_ns: Modification time in nanoseconds.
        ctime_ns: Inode change time on Unix, creation time on Windows.
        mode: The full mode, so a permission change is a change.
    """

    inode: int
    size: int
    mtime_ns: int
    ctime_ns: int
    mode: int

    def to_list(self) -> list[int]:
        """Return the tuple as a JSON-compatible list."""
        return [self.inode, self.size, self.mtime_ns, self.ctime_ns, self.mode]

    @staticmethod
    def from_list(values: list[int]) -> "TreeEntry":
        """
        Deserialise an entry from a list produced by ``to_list``.

        Args:
            values: The five integers in order.

        Returns:
            The entry.

        Raises:
            ValueError: The list does not hold five integers.
        """
        if len(values) != 5 or not all(isinstance(v, int) for v in values):
            raise ValueError("a tree entry must be five integers")

        return TreeEntry(values[0], values[1], values[2], values[3], values[4])


@dataclass
class TreeSnapshot:
    """
    The result of one metadata walk.

    Attributes:
        entries: Each file's metadata, keyed by its mindspace-relative path.
        entry_count: Directories and files visited, including pruned ones, which
            is what the cost of a walk is proportional to.
        duration_ms: How long the walk took, so a caller can adapt its interval.
    """

    entries: dict[str, TreeEntry] = field(default_factory=dict)
    entry_count: int = 0
    duration_ms: float = 0.0


@dataclass
class TreeDifference:
    """
    What changed between two snapshots.

    Attributes:
        added: Paths present now and absent before.
        removed: Paths present before and absent now.
        changed: Paths present in both whose metadata differs.
    """

    added: set[str] = field(default_factory=set)
    removed: set[str] = field(default_factory=set)
    changed: set[str] = field(default_factory=set)

    def paths(self) -> set[str]:
        """Return every path that changed in any way."""
        return self.added | self.removed | self.changed

    def is_empty(self) -> bool:
        """Return True if nothing changed."""
        return not (self.added or self.removed or self.changed)

    def to_dict(self) -> dict[str, list[str]]:
        """Return the difference as sorted lists, for storage or display."""
        return {
            "added": sorted(self.added),
            "removed": sorted(self.removed),
            "changed": sorted(self.changed),
        }

    @staticmethod
    def from_dict(data: dict[str, list[str]]) -> "TreeDifference":
        """
        Deserialise a difference from a dict produced by ``to_dict``.

        Args:
            data: The dict to read.

        Returns:
            The difference, with any missing section treated as empty.

        Raises:
            ValueError: A section is not a list of strings.
        """
        sections: dict[str, set[str]] = {}
        for name in ("added", "removed", "changed"):
            values = data.get(name, [])
            if not isinstance(values, list) or not all(isinstance(v, str) for v in values):
                raise ValueError(f"'{name}' must be a list of strings")

            sections[name] = set(values)

        return TreeDifference(sections["added"], sections["removed"], sections["changed"])

    @staticmethod
    def between(before: dict[str, TreeEntry], after: dict[str, TreeEntry]) -> "TreeDifference":
        """
        Return what differs between two snapshots.

        Args:
            before: The earlier snapshot's entries.
            after: The later snapshot's entries.
        """
        before_paths = before.keys()
        after_paths = after.keys()

        return TreeDifference(
            added=set(after_paths - before_paths),
            removed=set(before_paths - after_paths),
            changed={path for path in before_paths & after_paths if before[path] != after[path]},
        )


class MindspaceTreeScanner:
    """
    Walks a mindspace tree collecting file metadata, following no symlinks.

    A pruned directory is not descended and a pruned file is not recorded, which is
    what makes a walk cheap, and the cost is reported so a caller can lengthen its
    interval rather than block.  A path that cannot be inspected is skipped rather
    than failing the walk: a tree being written to while it is walked is normal, not
    exceptional.
    """

    def __init__(self, root: str, pruned_paths: frozenset[str]) -> None:
        """
        Initialise a scanner rooted at a mindspace.

        Args:
            root: Absolute path to the mindspace root.
            pruned_paths: The entries the scan does not cover, as normalised by
                ``mindspace_scan_pruned_paths.scan_pruned_paths``.
        """
        self._root = root
        self._pruned_paths = pruned_paths
        self._pruner = PathPatterns(pruned_paths)

    def root(self) -> str:
        """Return the absolute path the scanner walks."""
        return self._root

    def pruned_paths(self) -> frozenset[str]:
        """Return the entries the scan does not cover."""
        return self._pruned_paths

    def _is_pruned(self, relative_path: str) -> bool:
        """
        Return True if the file or directory at this position is outside the scan.

        The entry matching is ``path_matching``'s, shared with the filesystem tool's
        access lists.  It is applied to the path alone, because a name is what a
        single-component entry already matches by that rule's own terms.

        Args:
            relative_path: Its path from the mindspace root.
        """
        return self._pruner.matches(relative_path.replace(os.sep, "/"))

    def scan(self) -> TreeSnapshot:
        """
        Walk the tree and return every file's metadata.

        Returns:
            The snapshot, including how many entries were visited and how long the
            walk took.
        """
        started = time.perf_counter()
        entries: dict[str, TreeEntry] = {}
        visited = 0
        pending = [self._root]

        while pending:
            directory = pending.pop()

            try:
                iterator = os.scandir(directory)

            except OSError:
                continue

            with iterator:
                for entry in iterator:
                    visited += 1

                    try:
                        stat_result = entry.stat(follow_symlinks=False)

                    except OSError:
                        continue

                    mode = stat_result.st_mode
                    if stat.S_ISLNK(mode):
                        continue

                    if stat.S_ISDIR(mode):
                        relative = entry.path[len(self._root):].lstrip(os.sep)
                        if self._is_pruned(relative):
                            continue

                        pending.append(entry.path)
                        continue

                    if not stat.S_ISREG(mode):
                        continue

                    relative = entry.path[len(self._root):].lstrip(os.sep)
                    if self._is_pruned(relative):
                        continue

                    entries[relative] = TreeEntry(
                        stat_result.st_ino,
                        stat_result.st_size,
                        stat_result.st_mtime_ns,
                        stat_result.st_ctime_ns,
                        mode,
                    )

        return TreeSnapshot(
            entries=entries,
            entry_count=visited,
            duration_ms=(time.perf_counter() - started) * 1000.0,
        )
