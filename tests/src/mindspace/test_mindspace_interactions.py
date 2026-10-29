"""Tests for the append-only mindspace interaction log store."""
import json
import os

import pytest

from mindspace.mindspace import Mindspace
from mindspace.mindspace_interactions import MindspaceInteractions
from mindspace.mindspace_log_level import MindspaceLogLevel


def _open_store(path: str) -> MindspaceInteractions:
    """Return a store attached to the given log directory."""
    store = MindspaceInteractions()
    store.open(os.path.join(path, "log"))
    return store


def _segment_paths(path: str) -> list[str]:
    """Return the log's segment files in sequence order."""
    log_dir = os.path.join(path, "log")
    return [
        os.path.join(log_dir, name)
        for name in sorted(os.listdir(log_dir))
        if name.endswith(".jsonl")
    ]


def _fill(store: MindspaceInteractions, count: int) -> None:
    """Append count distinct records to a store."""
    for index in range(count):
        store.append(MindspaceLogLevel.INFO, f"entry {index}")


class TestRetention:
    """Tests that the log retains every record it is given."""

    def test_retains_more_than_the_previous_cap(self, tmp_path) -> None:
        """A log holding far more than 100 records still returns all of them."""
        store = _open_store(str(tmp_path))
        _fill(store, 250)

        messages = store.read(1, 1000)

        assert len(messages) == 250
        assert messages[0].content == "entry 0"
        assert messages[-1].content == "entry 249"

    def test_survives_reopen(self, tmp_path) -> None:
        """Records written by one session are read back by the next."""
        _fill(_open_store(str(tmp_path)), 120)

        store = _open_store(str(tmp_path))

        assert store.count() == 120
        assert len(store.read(1, 1000)) == 120

    def test_appends_do_not_rewrite_existing_records(self, tmp_path) -> None:
        """The bytes already written to a segment never change."""
        store = _open_store(str(tmp_path))
        _fill(store, 5)
        with open(_segment_paths(str(tmp_path))[0], "rb") as f:
            before = f.read()

        _fill(store, 5)

        with open(_segment_paths(str(tmp_path))[0], "rb") as f:
            after = f.read()

        assert after.startswith(before)


class TestSequenceNumbers:
    """Tests for the sequence numbers that identify records."""

    def test_sequence_starts_at_one_and_is_contiguous(self, tmp_path) -> None:
        """Records are numbered 1, 2, 3, ... in the order they were appended."""
        store = _open_store(str(tmp_path))
        _fill(store, 25)

        sequences = [message.sequence for message in store.read(1, 100)]

        assert sequences == list(range(1, 26))

    def test_sequence_continues_after_reopen(self, tmp_path) -> None:
        """A reopened log continues numbering from its last record."""
        _fill(_open_store(str(tmp_path)), 10)

        store = _open_store(str(tmp_path))
        message = store.append(MindspaceLogLevel.INFO, "after reopen")

        assert message.sequence == 11
        assert store.count() == 11

    def test_sequence_is_contiguous_across_rotation(self, tmp_path, monkeypatch) -> None:
        """Rotating the log leaves no gap in the sequence."""
        monkeypatch.setattr(MindspaceInteractions, "SEGMENT_MAX_BYTES", 1024)
        store = _open_store(str(tmp_path))
        _fill(store, 60)

        sequences = [message.sequence for message in store.read(1, 1000)]

        assert len(_segment_paths(str(tmp_path))) > 1
        assert sequences == list(range(1, 61))


class TestRotation:
    """Tests for segment rotation."""

    def test_rotation_starts_a_new_segment(self, tmp_path, monkeypatch) -> None:
        """Exceeding the segment size opens another segment file."""
        monkeypatch.setattr(MindspaceInteractions, "SEGMENT_MAX_BYTES", 1024)
        store = _open_store(str(tmp_path))

        _fill(store, 2)
        assert len(_segment_paths(str(tmp_path))) == 1

        _fill(store, 40)
        assert len(_segment_paths(str(tmp_path))) > 1

    def test_reads_span_segments(self, tmp_path, monkeypatch) -> None:
        """A read covering a rotation boundary returns records from both segments."""
        monkeypatch.setattr(MindspaceInteractions, "SEGMENT_MAX_BYTES", 1024)
        store = _open_store(str(tmp_path))
        _fill(store, 40)

        messages = store.read(1, 1000)

        assert [message.content for message in messages] == [
            f"entry {index}" for index in range(40)
        ]

    def test_read_starts_inside_a_later_segment(self, tmp_path, monkeypatch) -> None:
        """A read that starts mid-log skips the segments entirely before it."""
        monkeypatch.setattr(MindspaceInteractions, "SEGMENT_MAX_BYTES", 1024)
        store = _open_store(str(tmp_path))
        _fill(store, 40)

        messages = store.read(35, 100)

        assert [message.sequence for message in messages] == list(range(35, 41))


class TestDamagedRecords:
    """Tests for records that cannot be read back."""

    def test_partial_trailing_record_is_discarded(self, tmp_path) -> None:
        """An interrupted write loses only the record it was writing."""
        store = _open_store(str(tmp_path))
        _fill(store, 5)
        segment = _segment_paths(str(tmp_path))[-1]

        with open(segment, "a", encoding="utf-8") as f:
            f.write('{"sequence": 6, "level": "inf')

        reopened = _open_store(str(tmp_path))

        assert reopened.count() == 5
        assert [message.sequence for message in reopened.read(1, 100)] == [1, 2, 3, 4, 5]

    def test_unreadable_record_does_not_hide_later_records(self, tmp_path) -> None:
        """A corrupt record in the middle of a segment is skipped, not fatal."""
        store = _open_store(str(tmp_path))
        _fill(store, 3)
        segment = _segment_paths(str(tmp_path))[-1]

        with open(segment, "a", encoding="utf-8") as f:
            f.write("not json\n")
            f.write(json.dumps({
                "sequence": 4,
                "level": "info",
                "content": "after corruption",
                "timestamp": "2026-01-01T00:00:00+00:00"
            }) + "\n")

        messages = _open_store(str(tmp_path)).read(1, 100)

        assert [message.content for message in messages] == [
            "entry 0", "entry 1", "entry 2", "after corruption"
        ]

    def test_unwritable_log_reports_the_failure(self, tmp_path) -> None:
        """A store that cannot write reports why instead of failing silently."""
        blocked = os.path.join(str(tmp_path), "log")
        with open(blocked, "w", encoding="utf-8") as f:
            f.write("")

        store = _open_store(str(tmp_path))
        _fill(store, 1)

        assert store.count() == 0
        assert store.write_error() is not None


class TestContentBounds:
    """Tests for the per-record content limit."""

    def test_short_content_is_stored_unchanged(self, tmp_path) -> None:
        """Content within the limit is stored verbatim."""
        store = _open_store(str(tmp_path))
        content = "AI read file: 'src/mindspace/mindspace.py'"

        store.append(MindspaceLogLevel.INFO, content)

        assert store.read(1, 1)[0].content == content

    def test_overlong_content_is_truncated_with_a_notice(self, tmp_path) -> None:
        """Content beyond the limit is shortened and the omission is recorded."""
        store = _open_store(str(tmp_path))
        content = "x" * (MindspaceInteractions.MAX_CONTENT_CHARS + 500)

        store.append(MindspaceLogLevel.INFO, content)
        stored = store.read(1, 1)[0].content

        assert stored.startswith("x" * MindspaceInteractions.MAX_CONTENT_CHARS)
        assert "[500 characters omitted]" in stored

    def test_truncation_keeps_each_record_within_the_segment_budget(self, tmp_path) -> None:
        """A record of maximum size still leaves a segment room for more records."""
        store = _open_store(str(tmp_path))

        store.append(MindspaceLogLevel.INFO, "x" * 100_000)

        stored = store.read(1, 1)[0]
        assert len(stored.content) < 5000

    def test_reopen_after_a_maximum_size_record(self, tmp_path) -> None:
        """Numbering resumes correctly even when the newest record is large."""
        store = _open_store(str(tmp_path))
        store.append(MindspaceLogLevel.INFO, "y" * 100_000)
        store.append(MindspaceLogLevel.INFO, "after")

        reopened = _open_store(str(tmp_path))

        assert reopened.count() == 2
        assert reopened.read(2, 1)[0].content == "after"


class TestNewest:
    """Tests for reading the tail of the log."""

    def test_returns_the_newest_records_in_order(self, tmp_path) -> None:
        """The newest page holds the last records, still oldest first."""
        store = _open_store(str(tmp_path))
        _fill(store, 30)

        messages = store.newest(10)

        assert [message.sequence for message in messages] == list(range(21, 31))

    def test_returns_everything_when_the_log_is_short(self, tmp_path) -> None:
        """A log shorter than the page size is returned in full."""
        store = _open_store(str(tmp_path))
        _fill(store, 3)

        assert len(store.newest(10)) == 3

    def test_empty_log_returns_nothing(self, tmp_path) -> None:
        """An empty log has no newest records."""
        assert _open_store(str(tmp_path)).newest(10) == []


class TestMindspaceIntegration:
    """Tests for the log as wired into the Mindspace model."""

    def _create_mindspace(self, tmp_path) -> Mindspace:
        """Create a mindspace on disk and open it."""
        path = os.path.join(str(tmp_path), "ms")
        mindspace = self._model()
        mindspace.create_mindspace(path, [])
        mindspace.open_mindspace(path)
        return mindspace

    def _open_mindspace(self, tmp_path) -> Mindspace:
        """Open a mindspace that already exists on disk."""
        mindspace = self._model()
        mindspace.open_mindspace(os.path.join(str(tmp_path), "ms"))
        return mindspace

    @staticmethod
    def _model() -> Mindspace:
        """Return a Mindspace model with no mindspace open."""
        return Mindspace(
            on_settings_changed=lambda: None,
            on_interactions_updated=lambda: None,
        )

    def test_interactions_are_persisted_across_sessions(self, tmp_path) -> None:
        """An interaction logged in one session is readable in the next."""
        mindspace = self._create_mindspace(tmp_path)
        mindspace.add_interaction(MindspaceLogLevel.INFO, "User saved file 'a.py'")
        mindspace.close_mindspace()

        reopened = self._open_mindspace(tmp_path)

        messages = reopened.interactions().read(1, 10)
        assert [message.content for message in messages] == ["User saved file 'a.py'"]

    def test_log_lives_in_the_log_subdirectory(self, tmp_path) -> None:
        """Segments are written under .humbug/log so they stay out of tool reach."""
        mindspace = self._create_mindspace(tmp_path)
        mindspace.add_interaction(MindspaceLogLevel.WARN, "AI requested authorization")

        assert os.path.isdir(os.path.join(mindspace.mindspace_path(), ".humbug", "log"))
        assert os.listdir(os.path.join(mindspace.mindspace_path(), ".humbug", "log"))

    def test_closing_discards_in_memory_state_only(self, tmp_path) -> None:
        """Closing a mindspace leaves its log on disk."""
        mindspace = self._create_mindspace(tmp_path)
        mindspace.add_interaction(MindspaceLogLevel.ERROR, "Failed to open conversation")
        mindspace.close_mindspace()

        reopened = self._open_mindspace(tmp_path)
        assert reopened.interactions().count() == 1

    def test_interactions_requires_an_open_mindspace(self) -> None:
        """Reading the log without an open mindspace is a programming error."""
        with pytest.raises(AssertionError):
            self._model().interactions()
