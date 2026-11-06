"""Records file mutations made through Humbug so that they can be reversed."""

from dataclasses import dataclass
from datetime import datetime, timezone
import hashlib
import logging
import os

from mindspace.journal_blob_store import JournalBlobStore
from mindspace.journal_entry import JournalActor, JournalEntry, JournalOperation
from mindspace.journal_index import JournalIndex


@dataclass
class JournalPurgeResult:
    """
    What a purge removed from the journal.

    Attributes:
        entries_dropped: Number of recorded mutations discarded.
        bytes_reclaimed: Number of bytes freed from the blob store.
    """

    entries_dropped: int
    bytes_reclaimed: int


class FileJournal:
    """
    Records the pre-image of each file mutation made inside a mindspace.

    Pre-image content is held in a content-addressed blob store and the
    mutations themselves in an append-only index.  Recording never aborts the
    mutation it describes: a failure is logged at ERROR and the mutation is
    left unrecorded, so the file is reported as not revertable rather than the
    write being refused.

    A writer records before it mutates, because the pre-image has to be read
    while the old content is still there.  If the mutation then fails, the
    writer voids the record it made, so the journal never claims a change that
    did not happen.  ``void_entries`` is the only way to withdraw a record; the
    index itself stays append-only.

    The journal stores an opaque conversation identifier and does not interpret
    it.  It has no notion of which conversation may act on which entry.
    """

    BLOB_DIR = "blobs"
    WARN_BYTES = 512 * 1024 * 1024

    def __init__(self) -> None:
        """Initialise a journal that is not yet attached to a directory."""
        self._logger = logging.getLogger("FileJournal")
        self._blobs = JournalBlobStore("")
        self._index = JournalIndex()

    def open(self, directory: str) -> None:
        """
        Attach the journal to a directory, indexing the entries it contains.

        Args:
            directory: Journal root, holding the blob tree and index segments.
        """
        self._blobs = JournalBlobStore(os.path.join(directory, self.BLOB_DIR))
        self._index.open(directory)

    def close(self) -> None:
        """Detach the journal from its directory, discarding all in-memory state."""
        self._blobs = JournalBlobStore("")
        self._index.close()

    def count(self) -> int:
        """Return the number of live mutations the journal holds."""
        return len(self._index.read_all())

    def size_bytes(self) -> int:
        """
        Return the number of bytes the journal occupies.

        This is the figure the size guard compares against WARN_BYTES, so it
        counts the blobs, the temporary files a sweep has not yet reclaimed, and
        the index segments.
        """
        return self._blobs.total_bytes() + self._index.total_bytes()

    def record_write(
        self,
        absolute_path: str,
        relative_path: str,
        content: bytes,
        actor: JournalActor,
        conversation: str,
        group: str = "",
    ) -> JournalEntry | None:
        """
        Record a write that is about to happen to a path.

        The pre-image is read from disk, not taken from the caller, so the entry
        describes what was actually there rather than what the caller believed
        was there.  It is copied in chunks, so a large file costs memory
        proportional to nothing and can still be reversed.

        Args:
            absolute_path: Path about to be written.
            relative_path: The same path relative to the mindspace root, for the
                record.
            content: The bytes about to be written, used for the post-image
                digest.
            actor: Who is performing the write.
            conversation: Identifier of the conversation responsible, or an
                empty string when no conversation is involved.
            group: Identifier shared with the other end of the same operation, or
                an empty string when the mutation stands alone.

        Returns:
            The entry as recorded, or None if recording failed.
        """
        return self._record(
            absolute_path,
            relative_path,
            JournalOperation.WRITE,
            JournalBlobStore.digest_of(content),
            actor,
            conversation,
            group,
        )

    def record_write_from_path(
        self,
        absolute_path: str,
        relative_path: str,
        content_path: str,
        actor: JournalActor,
        conversation: str,
        group: str = "",
    ) -> JournalEntry | None:
        """
        Record a write whose content is taken from another file.

        Used by operations that copy or move content rather than supplying it, so
        the post-image digest is computed by reading the source instead of the
        content being held in memory.

        Args:
            absolute_path: Path about to be written.
            relative_path: The same path relative to the mindspace root, for the
                record.
            content_path: File the written content comes from.
            actor: Who is performing the write.
            conversation: Identifier of the conversation responsible, or an empty
                string when no conversation is involved.
            group: Identifier shared with the other entries of one operation, or
                an empty string when the mutation stands alone.

        Returns:
            The entry as recorded, or None if recording failed.
        """
        try:
            post_image_hash = JournalBlobStore.digest_of_file(content_path)

        except OSError as e:
            self._logger.error("Failed to digest '%s' for the journal: %s", content_path, e)
            return None

        return self._record(
            absolute_path, relative_path, JournalOperation.WRITE, post_image_hash, actor, conversation, group
        )

    def record_append(
        self,
        absolute_path: str,
        relative_path: str,
        appended: bytes,
        actor: JournalActor,
        conversation: str,
        group: str = "",
    ) -> JournalEntry | None:
        """
        Record an append that is about to happen to a path.

        An append leaves the file holding its previous content followed by the
        appended bytes, so the post-image digest is computed by reading the file
        and hashing the result, without holding it in memory.

        Args:
            absolute_path: Path about to be appended to.
            relative_path: The same path relative to the mindspace root, for the
                record.
            appended: The bytes about to be added.
            actor: Who is performing the append.
            conversation: Identifier of the conversation responsible, or an empty
                string when no conversation is involved.
            group: Identifier shared with the other entries of one operation, or
                an empty string when the mutation stands alone.

        Returns:
            The entry as recorded, or None if recording failed.
        """
        try:
            digest = hashlib.sha256()
            with open(absolute_path, 'rb') as f:
                while chunk := f.read(1024 * 1024):
                    digest.update(chunk)

            digest.update(appended)

        except OSError as e:
            self._logger.error("Failed to digest '%s' for the journal: %s", relative_path, e)
            return None

        return self._record(
            absolute_path, relative_path, JournalOperation.WRITE, digest.hexdigest(), actor, conversation, group
        )

    def record_removal(
        self,
        absolute_path: str,
        relative_path: str,
        actor: JournalActor,
        conversation: str,
        group: str = "",
        pre_image_is_directory: bool = False,
    ) -> JournalEntry | None:
        """
        Record a removal that is about to happen to a path.

        The pre-image is read from disk before the removal, so the content of a
        removed file survives in the blob store and the removal can be reversed.

        Args:
            absolute_path: Path about to be removed.
            relative_path: The same path relative to the mindspace root, for the
                record.
            actor: Who is performing the removal.
            conversation: Identifier of the conversation responsible, or an empty
                string when no conversation is involved.
            group: Identifier shared with the other entries of one operation, or
                an empty string when the mutation stands alone.
            pre_image_is_directory: True when the path holds a directory.  A
                directory has no content to capture, so the entry records that the
                path existed without attempting to read it, and reversal recreates
                it.

        Returns:
            The entry as recorded, or None if recording failed.
        """
        return self._record(
            absolute_path,
            relative_path,
            JournalOperation.DELETE,
            None,
            actor,
            conversation,
            group,
            pre_image_is_directory,
        )

    def retain_file(self, path: str) -> str | None:
        """
        Store a file's content and return its digest, or None if it could not be stored.

        The file is copied in chunks, so it may be of any size: the cost of retaining
        it is the cost of reading and writing it once, not of holding it.

        Args:
            path: The file to retain.

        Returns:
            The digest identifying the stored content, or None.
        """
        try:
            return self._blobs.store_file(path)

        except OSError as e:
            self._logger.error("Failed to retain '%s' in the journal: %s", path, e)
            return None

    def stored_digests(self) -> set[str]:
        """
        Return the digest of every blob the store actually holds.

        A caller that has reclaimed content needs to know what survived, so that it
        stops naming a pre-image the store can no longer produce.  Naming a missing
        blob is worse than naming none: an entry with no pre-image is reported as not
        revertable, while one whose content is gone is reported as a conflict at the
        moment the user asks for it back.

        Returns:
            The digests present in the blob tree.
        """
        return self._blobs.digests()

    def record_detected(
        self,
        relative_path: str,
        pre_image_hash: str | None,
        post_image_hash: str | None,
        existed_before: bool,
    ) -> JournalEntry | None:
        """
        Record a change the journal observed but did not cause.

        The pre-image is named by digest rather than read from disk, because by the
        time a change is detected the path already holds its new content.  The digest
        names content the store already holds: the capture taken when the mindspace
        was first watched, or a previous detection.

        The entry is recorded against the unattributed actor, which states what is
        known — that nothing in Humbug made this change — rather than guessing at
        which process did.  Everything the rest of the journal does with an entry
        then applies unchanged, including reversal and the divergence check.

        Args:
            relative_path: The path, relative to the mindspace root.
            pre_image_hash: Digest of the content before the change, or None when
                the path did not exist or its content was not captured.
            post_image_hash: Digest of the content after the change, or None when
                the path was removed.
            existed_before: True if the path existed before the change.

        Returns:
            The entry as recorded, or None if recording failed.
        """
        entry = JournalEntry(
            sequence=0,
            timestamp=datetime.now(timezone.utc).isoformat(),
            path=relative_path,
            operation=JournalOperation.DELETE if post_image_hash is None else JournalOperation.WRITE,
            actor=JournalActor.UNATTRIBUTED,
            conversation="",
            group="",
            existed_before=existed_before,
            pre_image_hash=pre_image_hash,
            post_image_hash=post_image_hash,
        )

        try:
            return self._index.append(entry)

        except OSError as e:
            self._logger.error("Failed to record a detected change of '%s': %s", relative_path, e)
            return None

    def void_entries(self, entries: list[JournalEntry | None]) -> None:
        """
        Cancel records for mutations that were recorded but did not happen.

        A writer records a mutation before performing it, so a mutation that
        fails leaves a record describing something that never occurred.  Such a
        record is cancelled by appending a void record that names it, which is
        the only way to withdraw anything from an append-only index.  Cancelled
        records are not returned by any query, so the interface never offers to
        reverse a change that did not happen.

        Voiding is best effort.  If it fails the record stays live, which means
        the mutation is reported as a change whose reversal later conflicts:
        wrong, but safe, and the failure is logged.

        Args:
            entries: The records to cancel.  None is ignored, so a caller can pass
                whatever a writer returned without checking it first.
        """
        for entry in entries:
            if entry is None:
                continue

            void = JournalEntry(
                sequence=0,
                timestamp=datetime.now(timezone.utc).isoformat(),
                path=entry.path,
                operation=entry.operation,
                actor=entry.actor,
                conversation=entry.conversation,
                group=entry.group,
                existed_before=entry.existed_before,
                pre_image_hash=None,
                post_image_hash=None,
                pre_image_trash=None,
                void_of=entry.sequence,
            )

            try:
                self._index.append(void)

            except OSError as e:
                self._logger.error(
                    "Failed to cancel journal entry %d for '%s': %s", entry.sequence, entry.path, e
                )

    def voided_sequences(self) -> set[int]:
        """Return the sequence numbers of every record the journal has cancelled."""
        return self._index.voided_sequences()

    def cancellations(self) -> list[JournalEntry]:
        """Return the records that cancel other records."""
        return self._index.cancellations()

    def entries_for_path(self, relative_path: str) -> list[JournalEntry]:
        """
        Return every recorded mutation of a path, in sequence order.

        Args:
            relative_path: The path as recorded, relative to the mindspace root.

        Returns:
            The matching entries, oldest first.
        """
        return [entry for entry in self._index.read_all() if entry.path == relative_path]

    def all_entries(self) -> list[JournalEntry]:
        """Return every recorded mutation, in sequence order."""
        return self._index.read_all()

    def entries_from(self, sequence: int, limit: int) -> list[JournalEntry]:
        """
        Return up to limit live mutations from a sequence number onwards.

        This is how a view that already holds older entries picks up what has been
        recorded since it last looked, without re-reading the history it holds.
        Segments entirely older than the starting sequence are skipped, so the cost
        is the newest segment rather than the whole index.

        Args:
            sequence: The first sequence number to return.
            limit: Maximum number of entries to return.

        Returns:
            The matching entries, oldest first.
        """
        return self._index.read(sequence, limit)

    def newest_sequence(self) -> int:
        """
        Return the sequence number of the most recent record.

        Void records count: this is how a caller tells that records were added, not
        how many mutations they describe.  It is zero when nothing has been recorded.
        """
        return self._index.count()

    def entries_after(self, sequence: int) -> list[JournalEntry]:
        """
        Return every live mutation recorded after a sequence number.

        This is what "put the mindspace back as it was at this change" reverses: the
        chosen record stays, and everything later is undone.  Per-path replay then
        leaves each affected file holding what the newest record at or before the
        sequence left there, which is the state that existed at that moment.

        Args:
            sequence: The sequence number whose state should be kept.

        Returns:
            The later entries, oldest first, as a concrete list.
        """
        return self._index.read(sequence + 1, self._index.count())

    def entries_since(self, since: datetime) -> list[JournalEntry]:
        """
        Return the mutations recorded at or after a point in time.

        The result is a concrete list, so a caller that shows it to the user and
        then acts on their choice is acting on exactly what was shown, even if
        more mutations are recorded in the meantime.

        Args:
            since: The earliest time to include, in any timezone-aware form.

        Returns:
            The matching entries, oldest first.
        """
        return [entry for entry in self._index.read_all() if self._recorded_at(entry) >= since]

    def purge_through(self, sequence: int) -> JournalPurgeResult:
        """
        Discard every recorded mutation up to and including a sequence number.

        This is the destructive half of retention, anchored to a record rather
        than to a clock: the caller has chosen the last change worth keeping, and
        everything at or before it goes.  Segments holding only discarded records
        are deleted; the segment that straddles the cutoff is rewritten without
        them, atomically and under a new name if its first record went with them.
        Content no surviving record refers to is then reclaimed.

        Args:
            sequence: The last sequence number to discard.  Zero discards
                nothing; a number past the end discards everything.

        Returns:
            What was removed, for the caller to report.
        """
        through = max(0, min(sequence, self._index.count()))
        dropped = self._index.drop_segments_before(through + 1)
        reclaimed = self._sweep_unreferenced_blobs()

        self._logger.info("Purged %d journal entries and reclaimed %d bytes", dropped, reclaimed)
        return JournalPurgeResult(entries_dropped=dropped, bytes_reclaimed=reclaimed)

    def purge_all(self) -> JournalPurgeResult:
        """Discard every recorded mutation, leaving an empty journal."""
        return self.purge_through(self._index.count())

    def purge_before(self, before: datetime) -> JournalPurgeResult:
        """
        Discard every recorded mutation older than a point in time.

        A time window is a convenience over the sequence-addressed purge: it is
        resolved to a cutoff once, and exactly that set is discarded.

        Args:
            before: The earliest time to keep.

        Returns:
            What was removed, for the caller to report.
        """
        cutoff = 0
        for entry in self._index.read_all():
            if self._recorded_at(entry) < before:
                cutoff = max(cutoff, entry.sequence)

        return self.purge_through(cutoff)

    def _sweep_unreferenced_blobs(self) -> int:
        """
        Reclaim stored content that no surviving record refers to.

        Returns:
            The number of bytes reclaimed.
        """
        keep = {
            entry.pre_image_hash
            for entry in self._index.read_all()
            if entry.pre_image_hash is not None
        }
        keep |= {
            entry.post_image_hash
            for entry in self._index.read_all()
            if entry.post_image_hash is not None
        }

        return self._blobs.sweep(keep)

    def pre_image(self, entry: JournalEntry) -> bytes | None:
        """
        Return the stored pre-image content for an entry, if it was captured.

        Args:
            entry: The entry whose pre-image is wanted.

        Returns:
            The content the path held before the mutation, or None when there is
            none to return.

        Raises:
            OSError: The blob exists but could not be read.
        """
        if entry.pre_image_hash is None:
            return None

        return self._blobs.retrieve(entry.pre_image_hash)

    def post_image(self, entry: JournalEntry) -> bytes | None:
        """
        Return the content an entry recorded as the result of its mutation.

        This is the version the change produced, which is what a history view shows
        when it is asked about a row.  It is not the file: the file may have moved on,
        and reading it would answer a different question.

        Args:
            entry: The entry whose post-image is wanted.

        Returns:
            The content the mutation produced, or None when it is not held.

        Raises:
            OSError: The blob exists but could not be read.
        """
        if entry.post_image_hash is None:
            return None

        return self._blobs.retrieve(entry.post_image_hash)

    def _record(
        self,
        absolute_path: str,
        relative_path: str,
        operation: JournalOperation,
        post_image_hash: str | None,
        actor: JournalActor,
        conversation: str,
        group: str,
        pre_image_is_directory: bool = False,
    ) -> JournalEntry | None:
        """
        Record a mutation, capturing whatever the path holds beforehand.

        Args:
            absolute_path: Path about to be mutated.
            relative_path: The same path relative to the mindspace root.
            operation: The kind of mutation.
            post_image_hash: Digest of the content the mutation produces, or None
                when it removes the path.
            actor: Who is performing the mutation.
            conversation: Identifier of the conversation responsible.
            group: Identifier shared with the other entries of one operation, or
                an empty string when the mutation stands alone.
            pre_image_is_directory: True when the path holds a directory, which
                existed but has no content to capture.

        Returns:
            The entry as recorded, or None if recording failed.
        """
        try:
            if pre_image_is_directory:
                existed_before = True
                pre_image_hash = None

            else:
                existed_before = os.path.isfile(absolute_path)
                pre_image_hash = None
                if existed_before:
                    pre_image_hash = self._blobs.store_file(absolute_path)

            entry = JournalEntry(
                sequence=0,
                timestamp=datetime.now(timezone.utc).isoformat(),
                path=relative_path,
                operation=operation,
                actor=actor,
                conversation=conversation,
                group=group,
                existed_before=existed_before,
                pre_image_hash=pre_image_hash,
                post_image_hash=post_image_hash,
                pre_image_is_directory=pre_image_is_directory,
            )
            return self._index.append(entry)

        except OSError as e:
            self._logger.error("Failed to record a %s of '%s': %s", operation.value, relative_path, e)
            return None

    def _recorded_at(self, entry: JournalEntry) -> datetime:
        """
        Return when an entry was recorded, or the epoch when that is unreadable.

        Treating an unreadable timestamp as the epoch keeps it out of any window
        a caller asks for, rather than letting a damaged record appear as recent
        activity.

        Args:
            entry: The entry to inspect.
        """
        try:
            return datetime.fromisoformat(entry.timestamp)

        except ValueError:
            self._logger.warning("Journalled entry %d has an unreadable timestamp", entry.sequence)
            return datetime.min.replace(tzinfo=timezone.utc)
