"""Reversal of recorded file mutations."""

from dataclasses import dataclass, field
from enum import Enum
from collections.abc import Callable
import os
import tempfile

from mindspace.file_journal import FileJournal
from mindspace.journal_blob_store import JournalBlobStore
from mindspace.journal_entry import JournalActor, JournalEntry


class JournalRevertStatus(str, Enum):
    """The outcome of attempting to reverse the mutations of one path."""

    REVERTED = "reverted"
    CONFLICT = "conflict"
    INCOMPLETE = "incomplete"
    NOT_REVERTABLE = "not_revertable"
    FAILED = "failed"


@dataclass
class JournalRevertOutcome:
    """
    What happened to one path during a reversal.

    Attributes:
        path: The path, relative to the mindspace root.
        status: Whether the path was reverted, and if not, why.
        detail: A human-readable explanation, empty when the outcome is obvious
            from the status.
    """

    path: str
    status: JournalRevertStatus
    detail: str = ""


@dataclass
class JournalRevertResult:
    """
    The outcome of reversing a set of entries.

    Attributes:
        outcomes: One outcome per affected path, in the order the paths were
            first seen.
    """

    outcomes: list[JournalRevertOutcome] = field(default_factory=list)

    def reverted_paths(self) -> list[str]:
        """Return the paths that were successfully reverted."""
        return [o.path for o in self.outcomes if o.status == JournalRevertStatus.REVERTED]

    def failed_outcomes(self) -> list[JournalRevertOutcome]:
        """Return the outcomes that were not reverted."""
        return [o for o in self.outcomes if o.status != JournalRevertStatus.REVERTED]


class JournalReverser:
    """
    Restores paths to the state they held before a set of recorded mutations.

    Reversal is performed per path rather than as a global unwinding, so each
    path lands on a state that genuinely existed and a conflict on one path does
    not prevent the others being reverted.

    A path is only reverted when its current content is still what the newest
    recorded mutation left there, and when each recorded mutation began from
    what the previous one produced.  Either check failing means something
    changed that the journal cannot account for, and the path is reported as a
    conflict instead of being overwritten.
    """

    def __init__(
        self,
        journal: FileJournal,
        mindspace_path: str,
        trash_dir: str,
        restore_trashed: Callable[[str], str],
    ) -> None:
        """
        Initialise a reverser over a journal.

        Args:
            journal: The journal holding the entries and their pre-images.
            mindspace_path: Root of the mindspace the paths are relative to.
            trash_dir: Directory holding trash entries, for removals whose content
                the trash kept instead of the journal.
            restore_trashed: Restores a trash entry to its original location and
                returns the path it was restored to.
        """
        self._journal = journal
        self._mindspace_path = mindspace_path
        self._trash_dir = trash_dir
        self._restore_trashed = restore_trashed

    def revert(self, entries: list[JournalEntry]) -> JournalRevertResult:
        """
        Reverse the mutations described by entries.

        Args:
            entries: The entries to reverse.  Order does not matter; they are
                grouped by path and ordered by sequence before use.

        Returns:
            The outcome for each affected path.
        """
        result = JournalRevertResult()
        incomplete = self._incomplete_groups(entries)

        for path, path_entries in self._group_by_path(entries).items():
            if any(entry.group in incomplete for entry in path_entries):
                result.outcomes.append(
                    JournalRevertOutcome(
                        path,
                        JournalRevertStatus.INCOMPLETE,
                        "part of an operation that must be reverted as a whole",
                    )
                )
                continue

            result.outcomes.append(self._revert_path(path, path_entries))

        return result

    def _incomplete_groups(self, entries: list[JournalEntry]) -> set[str]:
        """
        Return the groups the selection only partly covers.

        An operation that records more than one entry — a move records both the
        destination and the source — can only be reversed as a whole, because
        reversing one half would leave the content in neither place or in both.

        Args:
            entries: The entries the caller asked to reverse.

        Returns:
            The groups for which the selection omits a recorded entry.
        """
        selected: dict[str, int] = {}
        for entry in entries:
            if entry.group:
                selected[entry.group] = selected.get(entry.group, 0) + 1

        if not selected:
            return set()

        recorded: dict[str, int] = {}
        for entry in self._journal.all_entries():
            if entry.group in selected:
                recorded[entry.group] = recorded.get(entry.group, 0) + 1

        return {group for group, count in selected.items() if recorded.get(group, 0) > count}

    @staticmethod
    def _group_by_path(entries: list[JournalEntry]) -> dict[str, list[JournalEntry]]:
        """Group entries by path, ordered by sequence within each path."""
        grouped: dict[str, list[JournalEntry]] = {}
        for entry in sorted(entries, key=lambda e: e.sequence):
            grouped.setdefault(entry.path, []).append(entry)

        return grouped

    def _revert_path(self, path: str, entries: list[JournalEntry]) -> JournalRevertOutcome:
        """
        Reverse the mutations of a single path.

        Args:
            path: The path, relative to the mindspace root.
            entries: That path's entries, ordered by sequence.

        Returns:
            The outcome of the attempt.
        """
        absolute_path = os.path.join(self._mindspace_path, path)

        earliest = entries[0]
        current = self._current_digest(absolute_path)

        if not earliest.existed_before and current is None:
            return JournalRevertOutcome(path, JournalRevertStatus.REVERTED, "already absent")

        conflict = self._divergence(current, entries)
        if conflict is not None:
            return JournalRevertOutcome(path, JournalRevertStatus.CONFLICT, conflict)

        if not earliest.existed_before:
            self._journal.record_removal(absolute_path, path, JournalActor.HUMAN, "")
            return self._remove(absolute_path, path)

        if earliest.pre_image_trash is not None:
            return self._restore_from_trash(earliest.pre_image_trash, absolute_path, path)

        if earliest.pre_image_hash is None:
            return JournalRevertOutcome(
                path, JournalRevertStatus.NOT_REVERTABLE, "the earlier content was not captured"
            )

        try:
            content = self._journal.pre_image(earliest)

        except OSError as e:
            return JournalRevertOutcome(path, JournalRevertStatus.FAILED, str(e))

        if content is None:
            return JournalRevertOutcome(
                path, JournalRevertStatus.NOT_REVERTABLE, "the earlier content is no longer stored"
            )

        self._journal.record_write(absolute_path, path, content, JournalActor.HUMAN, "")
        return self._write(absolute_path, path, content)

    def _restore_from_trash(
        self,
        trash_name: str,
        absolute_path: str,
        path: str,
    ) -> JournalRevertOutcome:
        """
        Restore a removal whose content the trash kept.

        The journal holds no copy of this content, so the trash entry is the only
        source.  Emptying the trash therefore makes the removal unrevertable,
        which is reported rather than discovered half way through.

        Args:
            trash_name: The trash entry holding the content.
            absolute_path: Path to restore to.
            path: The path as recorded, for the outcome.
        """
        trashed_path = os.path.join(self._trash_dir, trash_name)
        if not os.path.isfile(trashed_path):
            return JournalRevertOutcome(
                path, JournalRevertStatus.NOT_REVERTABLE, "the trash no longer holds this content"
            )

        self._journal.record_write_from_path(
            absolute_path, path, trashed_path, JournalActor.HUMAN, ""
        )

        try:
            restored_path = self._restore_trashed(trash_name)

        except OSError as e:
            return JournalRevertOutcome(path, JournalRevertStatus.FAILED, str(e))

        if os.path.abspath(restored_path) != os.path.abspath(absolute_path):
            return JournalRevertOutcome(
                path,
                JournalRevertStatus.FAILED,
                f"the trash restored the content to '{restored_path}' instead",
            )

        return JournalRevertOutcome(path, JournalRevertStatus.REVERTED, "restored from the trash")

    @staticmethod
    def _divergence(current: str | None, entries: list[JournalEntry]) -> str | None:
        """
        Return why the path cannot be safely reverted, or None if it can.

        Args:
            current: Digest of the path's current content, or None if it is absent.
            entries: That path's entries, ordered by sequence.
        """
        for previous, following in zip(entries, entries[1:]):
            if following.pre_image_hash != previous.post_image_hash:
                return "a change between the recorded mutations was not recorded"

        expected = entries[-1].post_image_hash
        if expected is None:
            if current is not None:
                return "the file is present but was recorded as removed"

            return None

        if current is None:
            return "the file is no longer present"

        if current != expected:
            return "the file has changed since it was recorded"

        return None

    @staticmethod
    def _current_digest(absolute_path: str) -> str | None:
        """Return the digest of a file's current content, or None if it is absent."""
        try:
            with open(absolute_path, 'rb') as f:
                return JournalBlobStore.digest_of(f.read())

        except FileNotFoundError:
            return None

        except IsADirectoryError:
            return None

    @staticmethod
    def _remove(absolute_path: str, path: str) -> JournalRevertOutcome:
        """Remove a file whose creation is being reversed."""
        try:
            os.remove(absolute_path)

        except FileNotFoundError:
            return JournalRevertOutcome(path, JournalRevertStatus.REVERTED, "already absent")

        except OSError as e:
            return JournalRevertOutcome(path, JournalRevertStatus.FAILED, str(e))

        return JournalRevertOutcome(path, JournalRevertStatus.REVERTED, "removed")

    @staticmethod
    def _write(absolute_path: str, path: str, content: bytes) -> JournalRevertOutcome:
        """
        Write content to a path, replacing it atomically.

        Args:
            absolute_path: Path to write.
            path: The path as recorded, for the outcome.
            content: The content to restore.
        """
        try:
            directory = os.path.dirname(absolute_path)
            os.makedirs(directory, exist_ok=True)

            with tempfile.NamedTemporaryFile(mode='wb', dir=directory, delete=False, suffix='.tmp') as tmp:
                tmp.write(content)
                temp_path = tmp.name

            os.replace(temp_path, absolute_path)

        except OSError as e:
            return JournalRevertOutcome(path, JournalRevertStatus.FAILED, str(e))

        return JournalRevertOutcome(path, JournalRevertStatus.REVERTED, "restored")
