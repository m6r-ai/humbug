"""Append-only storage for the change journal's index of file mutations."""

from dataclasses import replace
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
    start at 1 and are contiguous, so a sequence number is also the record's
    position in the index.

    A record may cancel an earlier one: a mutation that was recorded before it
    was attempted and then did not happen is followed by a record naming it in
    its void_of field.  Reads return live records only, so a cancelled mutation
    is invisible to everything above this class while the index itself stays
    append-only.

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
        self._cancellations: list[JournalEntry] = []
        self._cancelled: set[int] = set()
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
        self._rebuild_cancellations()

    def close(self) -> None:
        """Detach the index from its directory, discarding all in-memory state."""
        self._directory = ""
        self._segments = []
        self._next_sequence = 1
        self._cancellations = []
        self._cancelled = set()

    def count(self) -> int:
        """Return the number of records in the index, including cancelled ones."""
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
        written = replace(entry, sequence=self._next_sequence)

        segment_bytes = self._append_record(written)
        self._next_sequence += 1

        if written.is_void() and written.void_of is not None:
            self._cancellations.append(written)
            self._cancelled.add(written.void_of)

        self._rotate_if_full(segment_bytes)
        return written

    def read(self, start_sequence: int, limit: int) -> list[JournalEntry]:
        """
        Read up to limit live entries beginning at start_sequence.

        Unreadable lines, including a partial record left by an interrupted
        write, are skipped so that one damaged entry cannot hide the rest, and
        records that cancel an earlier one are not returned.

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

            is_last = index + 1 == len(self._segments)
            segment = (
                self._read_segment_from(path, start_sequence) if is_last else self._read_segment(path)
            )

            for entry in segment:
                if entry.sequence < start_sequence or entry.is_void():
                    continue

                if entry.sequence in self._cancelled:
                    continue

                entries.append(entry)
                if len(entries) >= limit:
                    return entries

        return entries

    def read_all(self) -> list[JournalEntry]:
        """Read every live entry in the index, in sequence order."""
        return self.read(1, self.count())

    def voided_sequences(self) -> set[int]:
        """
        Return the sequence numbers of every record the index has cancelled.

        A caller that holds entries read before a cancellation needs this to
        tell whether what it holds is still live.  The set is kept as records are
        indexed and appended, so asking costs nothing.
        """
        return set(self._cancelled)

    def cancellations(self) -> list[JournalEntry]:
        """
        Return the records that cancel other records.

        Each one names the entry it cancels and repeats that entry's path and
        group, so a caller can tell that part of an operation was withdrawn
        without having to read the cancelled record itself.
        """
        return list(self._cancellations)

    def _rebuild_cancellations(self) -> None:
        """
        Index the records that cancel other records.

        Called when the index is opened and after a purge, which is the only
        operation that removes records it did not add.
        """
        cancellations: list[JournalEntry] = []

        for _first_sequence, path in self._segments:
            for entry in self._read_segment(path):
                if entry.is_void():
                    cancellations.append(entry)

        self._cancellations = cancellations
        self._cancelled = {entry.void_of for entry in cancellations if entry.void_of is not None}

    def total_bytes(self) -> int:
        """
        Return the number of bytes the segment files occupy.

        Returns:
            The total size of the index in bytes.
        """
        total = 0

        for _first_sequence, path in self._segments:
            try:
                total += os.path.getsize(path)

            except OSError:
                pass

        return total

    def drop_segments_before(self, sequence: int) -> int:
        """
        Discard every record whose sequence is before sequence.

        A segment holding only older records is deleted outright.  A segment
        that straddles the cutoff is rewritten with the older records removed,
        atomically and under a new name if its first record went with them, so
        the surviving records keep the sequence numbers they were written with
        and a sequence number is never reused.

        Rewriting a segment is the one place the index is not purely
        append-only.  It happens only when a caller has explicitly asked to
        forget history and has told the user it is irreversible; every other
        operation adds records and nothing else.

        Args:
            sequence: The first sequence number to keep.

        Returns:
            The number of records dropped.
        """
        dropped = 0
        surviving: list[tuple[int, str]] = []

        for index, (first_sequence, path) in enumerate(self._segments):
            next_first = self._segments[index + 1][0] if index + 1 < len(self._segments) else None

            if next_first is not None and next_first <= sequence:
                removed = next_first - first_sequence
                if self._remove_segment(path):
                    dropped += removed
                    continue

                surviving.append((first_sequence, path))
                continue

            purged = self._purge_segment(path, first_sequence, sequence)
            dropped += purged[1]
            if purged[1] == 0:
                surviving.append((first_sequence, path))
                continue

            if purged[0] is None:
                continue

            surviving.append(purged[0])

        self._segments = surviving
        if not surviving:
            self._start_empty_segment()

        # Purging removes records this index did not write, so what it has
        # cancelled has to be re-indexed from what survived.
        self._rebuild_cancellations()

        return dropped

    def _purge_segment(
        self, path: str, first_sequence: int, sequence: int
    ) -> tuple[tuple[int, str] | None, int]:
        """
        Remove the records before sequence from one segment.

        Args:
            path: The segment to purge.
            first_sequence: The sequence number the segment was named after.
            sequence: The first sequence number to keep.

        Returns:
            The segment's new identity and location, or None when nothing
            survived; and the number of records removed.
        """
        kept: list[str] = []
        first_kept: int | None = None
        removed = 0

        try:
            with open(path, encoding="utf-8") as f:
                for line in f:
                    if not line.strip():
                        continue

                    record_sequence = self._sequence_from_line(path, line.encode("utf-8"))
                    if record_sequence is not None and record_sequence < sequence:
                        removed += 1
                        continue

                    if record_sequence is not None and first_kept is None:
                        first_kept = record_sequence

                    kept.append(line if line.endswith("\n") else line + "\n")

        except OSError as e:
            self._logger.error("Failed to read journal index segment '%s': %s", path, e)
            return None, 0

        if removed == 0:
            return None, 0

        if not kept:
            return (None, removed) if self._remove_segment(path) else ((first_sequence, path), removed)

        destination = path if first_kept is None else self._segment_path(first_kept)
        temp_path = f"{destination}.tmp"

        try:
            with open(temp_path, "w", encoding="utf-8") as f:
                f.writelines(kept)
                f.flush()
                os.fsync(f.fileno())

            os.replace(temp_path, destination)

        except OSError as e:
            self._logger.error("Failed to rewrite journal index segment '%s': %s", path, e)
            try:
                os.remove(temp_path)

            except OSError:
                pass

            return None, 0

        if destination != path:
            self._remove_segment(path)

        return ((first_kept if first_kept is not None else first_sequence), destination), removed

    def _remove_segment(self, path: str) -> bool:
        """
        Delete a segment file.

        Args:
            path: The segment to delete.

        Returns:
            True if the file is gone.
        """
        try:
            os.remove(path)
            return True

        except FileNotFoundError:
            return True

        except OSError as e:
            self._logger.error("Failed to remove journal index segment '%s': %s", path, e)
            return False

    def _start_empty_segment(self) -> None:
        """
        Create the segment the next record will be appended to.

        The file is created immediately and named after the next sequence number,
        because its name is how the next sequence is recovered when the index is
        reopened.  Without it a purge of every segment would let sequence numbers
        start again from one.
        """
        os.makedirs(self._directory, exist_ok=True)
        path = self._segment_path(self._next_sequence)

        try:
            with open(path, "a", encoding="utf-8"):
                pass

        except OSError as e:
            self._logger.error("Failed to create journal index segment '%s': %s", path, e)
            return

        self._segments.append((self._next_sequence, path))

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
        """Return the records held in one segment, skipping unreadable lines."""
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

    def _read_segment_from(self, path: str, start_sequence: int) -> list[JournalEntry]:
        """
        Return the records in one segment whose sequence is at least start_sequence.

        The records are numbered contiguously, so the position of one can be
        estimated by interpolating its sequence across the file and then corrected by
        rewinding to a record boundary.  A caller that wants the newest few records
        therefore reads the tail of the segment rather than all of it, which is what
        keeps a single appended mutation cheap to display.

        Args:
            path: Segment file to read.
            start_sequence: The first sequence number wanted.

        Returns:
            The matching records, in sequence order.  Anything unreadable is skipped,
            exactly as the whole-segment read does.
        """
        entries: list[JournalEntry] = []
        first_sequence = self._first_sequence_of(path)
        if first_sequence is None:
            return entries

        offset = self._offset_for_sequence(path, first_sequence, start_sequence)
        data = self._read_from(path, offset)
        if data is None:
            return entries

        records = data.split(b"\n")
        if offset > 0:
            records = records[1:]

        for raw in records:
            if not raw.strip():
                continue

            try:
                entry = JournalEntry.from_dict(json.loads(raw))

            except (ValueError, KeyError, TypeError) as e:
                self._logger.warning("Ignoring unreadable journal index entry in '%s': %s", path, e)
                continue

            if entry.sequence >= start_sequence:
                entries.append(entry)

        return entries

    def _first_sequence_of(self, path: str) -> int | None:
        """Return the sequence number the segment's first record carries."""
        data = self._read_from(path, 0, 4096)
        if data is None:
            return None

        for raw in data.split(b"\n"):
            if not raw.strip():
                continue

            return self._sequence_from_line(path, raw)

        return None

    def _offset_for_sequence(self, path: str, first_sequence: int, sequence: int) -> int:
        """
        Return a byte offset at or before the record numbered sequence.

        The estimate interpolates the sequence across the file, assuming records of
        roughly equal size.  Being early is harmless because the records are filtered
        by sequence; being late would lose records, so the estimate is pulled back by
        the widest line found around it.

        Args:
            path: Segment file to read.
            first_sequence: The sequence number the segment's first record carries.
            sequence: The sequence number wanted.
        """
        try:
            size = os.path.getsize(path)

        except OSError:
            return 0

        if size == 0:
            return 0

        span = max(1, self._next_sequence - first_sequence)
        estimate = int(size * ((sequence - first_sequence) / span))

        data = self._read_from(path, max(0, estimate), self.TAIL_READ_BYTES)
        widest = max((len(line) for line in data.split(b"\n")), default=0) if data else 0
        return max(0, estimate - widest)

    def _read_from(self, path: str, offset: int, limit: int = -1) -> bytes | None:
        """
        Return up to limit bytes of a segment, read from a byte offset.

        Args:
            path: Segment file to read.
            offset: Byte offset to start at.
            limit: Maximum bytes to read, or -1 for the rest of the file.
        """
        try:
            with open(path, "rb") as f:
                f.seek(offset)
                return f.read(limit)

        except OSError as e:
            self._logger.error("Failed to read journal index segment '%s': %s", path, e)
            return None

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
