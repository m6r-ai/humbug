"""Reversal of recorded file mutations."""

from dataclasses import dataclass, field
from enum import Enum
from collections.abc import Callable
import os
import tempfile

from mindspace.file_journal import FileJournal
from mindspace.journal_blob_store import JournalBlobStore
from mindspace.journal_entry import JournalActor, JournalEntry
from mindspace.mindspace_error import MindspaceError


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

    The reversal is itself a mutation, so it is recorded like any other and
    carries the actor and conversation that asked for it.  A restore that then
    fails voids its record, exactly as a writer does.
    """

    def __init__(
        self,
        journal: FileJournal,
        mindspace_path: str,
        trash_dir: str,
        restore_trashed: Callable[[str], str],
        actor: JournalActor = JournalActor.HUMAN,
        conversation: str = "",
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
            actor: Who is asking for the reversal, recorded against the mutations
                the reversal itself makes.
            conversation: Identifier of the conversation that asked for the
                reversal, empty when no conversation is involved.
        """
        self._journal = journal
        self._mindspace_path = mindspace_path
        self._trash_dir = trash_dir
        self._restore_trashed = restore_trashed
        self._actor = actor
        self._conversation = conversation

    def revert(self, entries: list[JournalEntry]) -> JournalRevertResult:
        """
        Reverse the mutations described by entries.

        Args:
            entries: The entries to reverse.  Order does not matter; they are
                grouped by path and ordered by sequence before use.  Entries that
                have since been cancelled are ignored.

        Returns:
            The outcome for each affected path.
        """
        result = JournalRevertResult()
        live = self._live_entries(entries)
        incomplete = self._incomplete_groups(live)

        for path, path_entries in self._group_by_path(live).items():
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

    def _live_entries(self, entries: list[JournalEntry]) -> list[JournalEntry]:
        """
        Return the entries that still describe a mutation.

        An entry the caller selected may have been cancelled since it was read,
        because the operation it described then failed.  Reversing it would undo
        a change that never happened, so it is dropped.

        Args:
            entries: The entries the caller asked to reverse.
        """
        cancelled = self._journal.voided_sequences()
        return [entry for entry in entries if entry.sequence not in cancelled]

    def _incomplete_groups(self, entries: list[JournalEntry]) -> set[str]:
        """
        Return the groups the selection only partly covers.

        An operation that records more than one entry — a move records both the
        destination and the source — can only be reversed as a whole, because
        reversing one half would leave the content in neither place or in both.

        A group with a cancelled member is incomplete too: part of the operation
        was withdrawn, so applying the rest would act on a change that is only
        half described.

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

        incomplete = {
            cancellation.group
            for cancellation in self._journal.cancellations()
            if cancellation.group in selected
        }
        recorded: dict[str, int] = {}
        for entry in self._journal.all_entries():
            if entry.group in selected:
                recorded[entry.group] = recorded.get(entry.group, 0) + 1

        return incomplete | {
            group for group, count in selected.items() if recorded.get(group, 0) > count
        }

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

        if os.path.isdir(absolute_path):
            return JournalRevertOutcome(path, JournalRevertStatus.CONFLICT, "a directory now occupies this path")

        earliest = entries[0]
        current = self._current_digest(absolute_path)

        if not earliest.existed_before and current is None:
            return JournalRevertOutcome(path, JournalRevertStatus.REVERTED, "already absent")

        conflict = self._divergence(current, entries)
        if conflict is not None:
            return JournalRevertOutcome(path, JournalRevertStatus.CONFLICT, conflict)

        if not earliest.existed_before:
            return self._remove_created(absolute_path, path)

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

        return self._restore_content(absolute_path, path, content)

    def _restore_content(self, absolute_path: str, path: str, content: bytes) -> JournalRevertOutcome:
        """
        Write a recorded pre-image back over a path.

        Args:
            absolute_path: Path to write.
            path: The path as recorded, for the outcome.
            content: The content to restore.
        """
        recorded = self._journal.record_write(absolute_path, path, content, self._actor, self._conversation)
        outcome = self._write(absolute_path, path, content)
        if outcome.status != JournalRevertStatus.REVERTED:
            self._journal.void_entries([recorded])

        return outcome

    def _remove_created(self, absolute_path: str, path: str) -> JournalRevertOutcome:
        """
        Remove a file whose creation is being reversed.

        Args:
            absolute_path: Path to remove.
            path: The path as recorded, for the outcome.
        """
        recorded = self._journal.record_removal(absolute_path, path, self._actor, self._conversation)
        outcome = self._remove(absolute_path, path)
        if outcome.status != JournalRevertStatus.REVERTED:
            self._journal.void_entries([recorded])

        return outcome

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
        if not os.path.exists(trashed_path):
            return JournalRevertOutcome(
                path, JournalRevertStatus.NOT_REVERTABLE, "the trash no longer holds this content"
            )

        # Recreating an empty directory writes no content, so there is nothing for a
        # later reversal to undo and nothing to record.  The removal this restores is
        # already in the index, and reversing it again reports a conflict once the
        # directory is back in place.
        recorded = None if os.path.isdir(trashed_path) else self._journal.record_write_from_path(
            absolute_path, path, trashed_path, self._actor, self._conversation
        )

        try:
            restored_path = self._restore_trashed(trash_name)

        except (MindspaceError, OSError) as e:
            self._journal.void_entries([recorded])
            return JournalRevertOutcome(path, JournalRevertStatus.FAILED, str(e))

        if os.path.abspath(restored_path) != os.path.abspath(absolute_path):
            self._journal.void_entries([recorded])
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

        A mutation whose pre-image was too large to capture says nothing about
        what the path held, so it is not evidence of a change that went
        unrecorded and does not break the chain.

        Args:
            current: Digest of the path's current content, or None if it is absent.
            entries: That path's entries, ordered by sequence.
        """
        for previous, following in zip(entries, entries[1:]):
            if not following.existed_before:
                if previous.post_image_hash is not None:
                    return "a change between the recorded mutations was not recorded"

                continue

            if following.pre_image_hash is None:
                continue

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
        """
        Return the digest of a file's current content, or None if it is absent.

        The content is hashed in chunks so that checking a large file does not
        cost memory proportional to its size.

        Args:
            absolute_path: The path to inspect.
        """
        try:
            return JournalBlobStore.digest_of_file(absolute_path)

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
