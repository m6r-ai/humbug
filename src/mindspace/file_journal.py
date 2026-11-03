"""Records file mutations made through Humbug so that they can be reversed."""

from datetime import datetime, timezone
import hashlib
import logging
import os

from mindspace.journal_blob_store import JournalBlobStore
from mindspace.journal_entry import JournalActor, JournalEntry, JournalOperation
from mindspace.journal_index import JournalIndex


class FileJournal:
    """
    Records the pre-image of each file mutation made inside a mindspace.

    Pre-image content is held in a content-addressed blob store and the
    mutations themselves in an append-only index.  Recording never aborts the
    mutation it describes: a failure is logged at ERROR and the mutation is
    left unrecorded, so the file is reported as not revertable rather than the
    write being refused.

    The journal stores an opaque conversation identifier and interprets it.  It
    has no notion of which conversation may act on which entry.
    """

    BLOB_DIR = "blobs"
    MAX_CAPTURE_BYTES = 1024 * 1024

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
        """Return the number of recorded mutations."""
        return self._index.count()

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
        was there.  Content larger than MAX_CAPTURE_BYTES is recorded as
        metadata only: the entry names the mutation but cannot reverse it.

        Args:
            absolute_path: Path about to be written.
            relative_path: The same path relative to the mindspace root, for the
                record.
            content: The bytes about to be written, used for the post-image
                digest.
            actor: Who is performing the write.
            conversation: Identifier of the conversation responsible, or an
                empty string when no conversation is involved.

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
            group: Identifier shared with the other entries of one operation.
            trash_name: Name of the trash entry the content is about to be moved
                to.  The content is still copied into the journal's own store, so
                a reversal does not depend on the trash entry surviving.

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
        trash_name: str | None = None,
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
            trash_name,
            trash_name is None,
        )

    def _record(
        self,
        absolute_path: str,
        relative_path: str,
        operation: JournalOperation,
        post_image_hash: str | None,
        actor: JournalActor,
        conversation: str,
        group: str,
        pre_image_trash: str | None = None,
        store_pre_image: bool = True,
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
            pre_image_trash: Name of the trash entry holding the removed content,
                when the removal parked it there.
            store_pre_image: When False the pre-image is hashed but not copied into
                the blob store, because another store already holds the content.

        Returns:
            The entry as recorded, or None if recording failed.
        """
        try:
            existed_before, pre_image = self._read_pre_image(absolute_path)
            pre_image_hash = None
            if pre_image is not None:
                if store_pre_image:
                    pre_image_hash = self._blobs.store(pre_image)

                else:
                    pre_image_hash = JournalBlobStore.digest_of(pre_image)

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
                pre_image_trash=pre_image_trash,
            )
            return self._index.append(entry)

        except OSError as e:
            self._logger.error("Failed to record a %s of '%s': %s", operation.value, relative_path, e)
            return None

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

    def oldest_entry(self) -> JournalEntry | None:
        """
        Return the oldest recorded mutation, or None when the journal is empty.

        This is the horizon a caller can honestly offer: everything older has
        never been recorded, so no interface should promise to reach past it.
        """
        entries = self._index.read_all()
        return entries[0] if entries else None

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

    def _read_pre_image(self, absolute_path: str) -> tuple[bool, bytes | None]:
        """
        Read the content a path holds now, before it is mutated.

        Args:
            absolute_path: Path about to be mutated.

        Returns:
            A tuple of whether the path existed, and its content when it existed
            and was small enough to capture.

        Raises:
            OSError: The path exists but could not be inspected or read.
        """
        if not os.path.isfile(absolute_path):
            return False, None

        if os.path.getsize(absolute_path) > self.MAX_CAPTURE_BYTES:
            return True, None

        with open(absolute_path, 'rb') as f:
            return True, f.read()
