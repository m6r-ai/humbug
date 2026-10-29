import json
import logging
import os
import re
from collections.abc import Iterator
from datetime import datetime

from mindspace.mindspace_log_level import MindspaceLogLevel
from mindspace.mindspace_message import MindspaceMessage

_SEGMENT_NAME = re.compile(r"^log-(\d{8})\.jsonl$")


class MindspaceInteractions:
    """
    Append-only store for the mindspace interaction log.

    The log is the mindspace's audit trail, so it is never truncated and never
    rewritten.  Messages are appended to segmented JSON Lines files, each line
    holding one complete record, and each file named after the sequence number of
    its first record.  Sequence numbers start at 1 and are contiguous, so the last
    sequence number is also the number of records in the log.

    A segment is rotated once it grows past SEGMENT_MAX_BYTES.  Rotation starts a
    new segment; it never removes anything, so the log grows without bound and the
    only size limits are on individual records and on how much a caller reads.
    """

    SEGMENT_MAX_BYTES = 4 * 1024 * 1024
    MAX_CONTENT_CHARS = 4096
    SEGMENT_PREFIX = "log-"
    SEGMENT_SUFFIX = ".jsonl"

    # Wide enough to hold several maximum-size records, which matters because a record
    # holds up to MAX_CONTENT_CHARS characters and UTF-8 can take four bytes per character.
    TAIL_READ_BYTES = 64 * 1024

    def __init__(self) -> None:
        """Initialise a store that is not yet attached to a directory."""
        self._directory = ""
        self._segments: list[tuple[int, str]] = []
        self._next_sequence = 1
        self._write_error: str | None = None
        self._logger = logging.getLogger("MindspaceInteractions")

    def open(self, directory: str) -> None:
        """
        Attach the store to a log directory, indexing the segments it contains.

        Args:
            directory: Directory holding the log's segment files.  It is created
                when the first record is appended.
        """
        self._directory = directory
        self._segments = self._scan_segments()
        self._next_sequence = self._resolve_next_sequence()
        self._write_error = None

    def close(self) -> None:
        """Detach the store from its directory, discarding all in-memory state."""
        self._directory = ""
        self._segments = []
        self._next_sequence = 1
        self._write_error = None

    def count(self) -> int:
        """Return the number of records in the log."""
        return self._next_sequence - 1

    def write_error(self) -> str | None:
        """
        Return why the last append failed, or None if appends are succeeding.

        An audit trail that cannot be written is a failure the user has to be able
        to see, so the reason is kept until an append succeeds again.
        """
        return self._write_error

    def append(
        self,
        level: MindspaceLogLevel,
        content: str,
        timestamp: datetime | None = None,
    ) -> MindspaceMessage:
        """
        Append a record to the log.

        The record is written and flushed to disk before this returns.  A write
        failure is recorded in write_error() rather than raised, so that logging an
        interaction can never abort the action it is recording.

        Args:
            level: Severity level of the message.
            content: Message text.  Overlong content is truncated with a notice.
            timestamp: Time of the interaction, defaulting to now.

        Returns:
            The record that was appended.
        """
        message = MindspaceMessage.create(
            self._next_sequence, level, self._truncate_content(content), timestamp
        )

        try:
            segment_bytes = self._append_record(message)
            self._next_sequence += 1
            self._rotate_if_full(segment_bytes)
            self._write_error = None

        except OSError as e:
            self._write_error = str(e)
            self._logger.error("Failed to append to the interaction log: %s", e)

        return message

    def read(self, start_sequence: int, limit: int) -> list[MindspaceMessage]:
        """
        Read up to limit records beginning at start_sequence.

        Unreadable lines, including a partial record left by an interrupted write,
        are skipped so that one damaged record cannot hide the rest of the log.

        Args:
            start_sequence: Sequence number of the first record to return.
            limit: Maximum number of records to return.

        Returns:
            The records found, in sequence order.
        """
        if limit <= 0:
            return []

        messages: list[MindspaceMessage] = []

        for index, (_first_sequence, path) in enumerate(self._segments):
            if len(messages) >= limit:
                break

            if self._segment_precedes(index, start_sequence):
                continue

            for message in self._read_segment(path):
                if message.sequence < start_sequence:
                    continue

                messages.append(message)
                if len(messages) >= limit:
                    return messages

        return messages

    def newest(self, limit: int) -> list[MindspaceMessage]:
        """
        Read the most recent limit records.

        Args:
            limit: Maximum number of records to return.

        Returns:
            The newest records, in sequence order.
        """
        total = self.count()
        if total == 0 or limit <= 0:
            return []

        start_sequence = max(1, total - limit + 1)
        return self.read(start_sequence, limit)

    def _truncate_content(self, content: str) -> str:
        """Shorten content that exceeds MAX_CONTENT_CHARS, recording the omission."""
        if len(content) <= self.MAX_CONTENT_CHARS:
            return content

        omitted = len(content) - self.MAX_CONTENT_CHARS
        return f"{content[:self.MAX_CONTENT_CHARS]} ... [{omitted} characters omitted]"

    def _scan_segments(self) -> list[tuple[int, str]]:
        """Return the segment files in the log directory, ordered by first sequence."""
        try:
            names = os.listdir(self._directory)

        except FileNotFoundError:
            return []

        except OSError as e:
            self._logger.error("Failed to list the interaction log directory: %s", e)
            return []

        segments: list[tuple[int, str]] = []
        for name in names:
            match = _SEGMENT_NAME.match(name)
            if match is not None:
                segments.append((int(match.group(1)), os.path.join(self._directory, name)))

        segments.sort()
        return segments

    def _resolve_next_sequence(self) -> int:
        """Return the sequence number the next appended record should carry."""
        if not self._segments:
            return 1

        first_sequence, path = self._segments[-1]
        return self._last_sequence(path, first_sequence) + 1

    def _last_sequence(self, path: str, first_sequence: int) -> int:
        """
        Return the sequence number of the last complete record in a segment.

        The tail of the segment is read first so that opening a mindspace does not
        depend on the size of its newest segment.  A segment whose tail holds no
        complete record is measured by counting the records it does hold.
        """
        try:
            size = os.path.getsize(path)

        except OSError as e:
            self._logger.error("Failed to inspect interaction log segment '%s': %s", path, e)
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
            self._logger.error("Failed to read interaction log segment '%s': %s", path, e)
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
            self._logger.error("Failed to count interaction log records in '%s': %s", path, e)
            return first_sequence - 1

        return first_sequence + count - 1

    def _sequence_from_line(self, path: str, line: bytes) -> int | None:
        """Return the sequence number held in a record, or None if it is unreadable."""
        try:
            record = json.loads(line)
            return int(record["sequence"])

        except (ValueError, KeyError, TypeError) as e:
            self._logger.warning(
                "Ignoring unreadable interaction log record in '%s': %s", path, e
            )
            return None

    def _read_segment(self, path: str) -> Iterator[MindspaceMessage]:
        """Yield the records held in one segment, skipping unreadable lines."""
        try:
            with open(path, encoding="utf-8") as f:
                for line in f:
                    if not line.strip():
                        continue

                    try:
                        yield MindspaceMessage.from_dict(json.loads(line))

                    except (ValueError, KeyError, TypeError) as e:
                        self._logger.warning(
                            "Ignoring unreadable interaction log record in '%s': %s", path, e
                        )

        except FileNotFoundError:
            return

        except OSError as e:
            self._logger.error("Failed to read interaction log segment '%s': %s", path, e)

    def _segment_precedes(self, index: int, start_sequence: int) -> bool:
        """Return True if a segment holds only records before start_sequence."""
        if index + 1 >= len(self._segments):
            return False

        return self._segments[index + 1][0] <= start_sequence

    def _append_record(self, message: MindspaceMessage) -> int:
        """
        Write one record to the current segment and return the segment's new size.

        Args:
            message: Record to append.
        """
        os.makedirs(self._directory, exist_ok=True)
        path = self._current_segment_path()
        line = json.dumps(message.to_dict(), ensure_ascii=False) + "\n"

        with open(path, "a", encoding="utf-8") as f:
            f.write(line)
            f.flush()
            os.fsync(f.fileno())

        return os.path.getsize(path)

    def _current_segment_path(self) -> str:
        """Return the path of the segment the next record is appended to."""
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
            "Rotating the interaction log at sequence %d after %d bytes",
            self._next_sequence, segment_bytes,
        )
