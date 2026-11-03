"""Append-only storage for the change journal's index of file mutations."""

import json
import logging
import os
import re

from mindspace.journal_entry import JournalEntry

_SEGMENT_NAME = re.compile(r"^index-(\d{8})\.jsonl$")


class JournalIndex:
    """
    Append-only index of journal entries, held in segmented JSON Lines files.

    Entries are appended, never rewritten, so an interrupted write can only
    damage the partial final line, which is skipped on read.  Sequence numbers
    start at 1 and are contiguous, so a sequence number is also the entry's
    position in the index.

    A segment is rotated once it grows past SEGMENT_MAX_BYTES.  Rotation starts
    a new segment and never removes anything.
    """

    SEGMENT_MAX_BYTES = 4 * 1024 * 1024
    SEGMENT_PREFIX = "index-"
    SEGMENT_SUFFIX = ".jsonl"

    # Wide enough to hold several entries, which matters because a path and a
    # digest are both bounded but a conversation identifier is not.
    TAIL_READ_BYTES = 64 * 1024

    def __init__(self) -> None:
        """Initialise an index that is not yet attached to a directory."""
        self._directory = ""
        self._segments: list[tuple[int, str]] = []
        self._next_sequence = 1
        self._logger = logging.getLogger("JournalIndex")

    def open(self, directory: str) -> None:
        """
        Attach the index to a directory, indexing the segments it contains.

        Args:
            directory: Directory holding the segment files.  It is created when
                the first entry is appended.
        """
        self._directory = directory
        self._segments = self._scan_segments()
        self._next_sequence = self._resolve_next_sequence()

    def close(self) -> None:
        """Detach the index from its directory, discarding all in-memory state."""
        self._directory = ""
        self._segments = []
        self._next_sequence = 1

    def count(self) -> int:
        """Return the number of entries in the index."""
        return self._next_sequence - 1

    def append(self, entry: JournalEntry) -> JournalEntry:
        """
        Append an entry, assigning it the next sequence number.

        The record is written and flushed to disk before this returns.

        Args:
            entry: Entry to append.  Its sequence field is overwritten.

        Returns:
            The entry as written, carrying its assigned sequence number.

        Raises:
            OSError: The entry could not be written.
        """
        written = JournalEntry(
            sequence=self._next_sequence,
            timestamp=entry.timestamp,
            path=entry.path,
            operation=entry.operation,
            actor=entry.actor,
            conversation=entry.conversation,
            group=entry.group,
            existed_before=entry.existed_before,
            pre_image_hash=entry.pre_image_hash,
            post_image_hash=entry.post_image_hash,
            pre_image_trash=entry.pre_image_trash,
        )

        segment_bytes = self._append_record(written)
        self._next_sequence += 1
        self._rotate_if_full(segment_bytes)
        return written

    def read(self, start_sequence: int, limit: int) -> list[JournalEntry]:
        """
        Read up to limit entries beginning at start_sequence.

        Unreadable lines, including a partial record left by an interrupted
        write, are skipped so that one damaged entry cannot hide the rest.

        Args:
            start_sequence: Sequence number of the first entry to return.
            limit: Maximum number of entries to return.

        Returns:
            The entries found, in sequence order.
        """
        if limit <= 0:
            return []

        entries: list[JournalEntry] = []

        for index, (_first_sequence, path) in enumerate(self._segments):
            if len(entries) >= limit:
                break

            if self._segment_precedes(index, start_sequence):
                continue

            for entry in self._read_segment(path):
                if entry.sequence < start_sequence:
                    continue

                entries.append(entry)
                if len(entries) >= limit:
                    return entries

        return entries

    def read_all(self) -> list[JournalEntry]:
        """Read every entry in the index, in sequence order."""
        return self.read(1, self.count())

    def _scan_segments(self) -> list[tuple[int, str]]:
        """Return the segment files in the index directory, ordered by first sequence."""
        try:
            names = os.listdir(self._directory)

        except FileNotFoundError:
            return []

        except OSError as e:
            self._logger.error("Failed to list the journal index directory: %s", e)
            return []

        segments: list[tuple[int, str]] = []
        for name in names:
            match = _SEGMENT_NAME.match(name)
            if match is not None:
                segments.append((int(match.group(1)), os.path.join(self._directory, name)))

        segments.sort()
        return segments

    def _resolve_next_sequence(self) -> int:
        """Return the sequence number the next appended entry should carry."""
        if not self._segments:
            return 1

        first_sequence, path = self._segments[-1]
        return self._last_sequence(path, first_sequence) + 1

    def _last_sequence(self, path: str, first_sequence: int) -> int:
        """
        Return the sequence number of the last complete entry in a segment.

        The tail of the segment is read first so that opening a mindspace does
        not depend on the size of its newest segment.  A segment whose tail
        holds no complete entry is measured by counting its entries.
        """
        try:
            size = os.path.getsize(path)

        except OSError as e:
            self._logger.error("Failed to inspect journal index segment '%s': %s", path, e)
            return first_sequence - 1

        if size == 0:
            return first_sequence - 1

        for line in reversed(self._read_tail(path, size)):
            sequence = self._sequence_from_line(path, line)
            if sequence is not None:
                return sequence

        return self._count_records(path, first_sequence)

    def _read_tail(self, path: str, size: int) -> list[bytes]:
        """
        Return the complete lines at the end of a segment.

        Args:
            path: Segment file to read.
            size: Size of the segment in bytes.
        """
        start = max(0, size - self.TAIL_READ_BYTES)

        try:
            with open(path, "rb") as f:
                f.seek(start)
                data = f.read()

        except OSError as e:
            self._logger.error("Failed to read journal index segment '%s': %s", path, e)
            return []

        lines = data.split(b"\n")

        if start > 0:
            lines = lines[1:]

        if lines and lines[-1] == b"":
            lines = lines[:-1]

        return [line for line in lines if line.strip()]

    def _count_records(self, path: str, first_sequence: int) -> int:
        """Count the complete records in a segment."""
        count = 0
        try:
            with open(path, "rb") as f:
                for line in f:
                    if line.strip() and line.endswith(b"\n"):
                        count += 1

        except OSError as e:
            self._logger.error("Failed to count journal index records in '%s': %s", path, e)
            return first_sequence - 1

        return first_sequence + count - 1

    def _sequence_from_line(self, path: str, line: bytes) -> int | None:
        """Return the sequence number held in an entry, or None if it is unreadable."""
        try:
            record = json.loads(line)
            return int(record["sequence"])

        except (ValueError, KeyError, TypeError) as e:
            self._logger.warning("Ignoring unreadable journal index entry in '%s': %s", path, e)
            return None

    def _read_segment(self, path: str) -> list[JournalEntry]:
        """Return the entries held in one segment, skipping unreadable lines."""
        entries: list[JournalEntry] = []

        try:
            with open(path, encoding="utf-8") as f:
                for line in f:
                    if not line.strip():
                        continue

                    try:
                        entries.append(JournalEntry.from_dict(json.loads(line)))

                    except (ValueError, KeyError, TypeError) as e:
                        self._logger.warning("Ignoring unreadable journal index entry in '%s': %s", path, e)

        except FileNotFoundError:
            return entries

        except OSError as e:
            self._logger.error("Failed to read journal index segment '%s': %s", path, e)

        return entries

    def _segment_precedes(self, index: int, start_sequence: int) -> bool:
        """Return True if a segment holds only entries before start_sequence."""
        if index + 1 >= len(self._segments):
            return False

        return self._segments[index + 1][0] <= start_sequence

    def _append_record(self, entry: JournalEntry) -> int:
        """
        Write one entry to the current segment and return the segment's new size.

        Args:
            entry: Entry to write.
        """
        os.makedirs(self._directory, exist_ok=True)
        path = self._current_segment_path()
        line = json.dumps(entry.to_dict(), ensure_ascii=False) + "\n"

        with open(path, "a", encoding="utf-8") as f:
            f.write(line)
            f.flush()
            os.fsync(f.fileno())

        return os.path.getsize(path)

    def _current_segment_path(self) -> str:
        """Return the path of the segment the next entry is appended to."""
        if not self._segments:
            self._segments.append((self._next_sequence, self._segment_path(self._next_sequence)))

        return self._segments[-1][1]

    def _segment_path(self, first_sequence: int) -> str:
        """Return the path a segment starting at first_sequence is stored at."""
        name = f"{self.SEGMENT_PREFIX}{first_sequence:08d}{self.SEGMENT_SUFFIX}"
        return os.path.join(self._directory, name)

    def _rotate_if_full(self, segment_bytes: int) -> None:
        """
        Start a new segment if the current one has grown past SEGMENT_MAX_BYTES.

        Args:
            segment_bytes: Size of the current segment after the last append.
        """
        if segment_bytes <= self.SEGMENT_MAX_BYTES:
            return

        self._segments.append((self._next_sequence, self._segment_path(self._next_sequence)))
        self._logger.debug(
            "Rotating the journal index at sequence %d after %d bytes",
            self._next_sequence, segment_bytes,
        )
