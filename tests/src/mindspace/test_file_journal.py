"""Tests for the mindspace change journal."""

from datetime import datetime, timedelta, timezone
import json
import os

import pytest

from mindspace.file_journal import FileJournal
from mindspace.journal_blob_store import JournalBlobStore
from mindspace.journal_entry import JournalActor, JournalEntry, JournalOperation
from mindspace.journal_index import JournalIndex
from mindspace.mindspace import Mindspace
from mindspace.mindspace_settings import MindspaceSettings


def _make_mindspace(tmp_path, monkeypatch) -> Mindspace:
    """Construct a Mindspace with an open mindspace at a temp path."""
    mindspace = Mindspace(
        on_settings_changed=lambda: None,
        on_interactions_updated=lambda: None,
    )
    path = os.path.join(str(tmp_path), "ms")
    os.makedirs(os.path.join(path, ".humbug"))
    monkeypatch.setattr(mindspace, "_path", path)
    monkeypatch.setattr(mindspace, "_settings", MindspaceSettings(enabled_tools={}))
    return mindspace


def _entry(path: str = "notes.txt", sequence: int = 0) -> JournalEntry:
    """Build a journal entry with plausible values."""
    return JournalEntry(
        sequence=sequence,
        timestamp="2026-11-03T10:00:00+00:00",
        path=path,
        operation=JournalOperation.WRITE,
        actor=JournalActor.AI,
        conversation="conversation-1",
        group="",
        existed_before=True,
        pre_image_hash="a" * 64,
        post_image_hash="b" * 64,
    )


class TestJournalEntry:
    """A journal entry round-trips through its dict form and knows its revertability."""

    def test_round_trips_through_dict(self):
        entry = _entry()

        assert JournalEntry.from_dict(entry.to_dict()) == entry

    def test_round_trips_a_creation_without_a_pre_image(self):
        entry = _entry()
        entry.existed_before = False
        entry.pre_image_hash = None

        assert JournalEntry.from_dict(entry.to_dict()) == entry

    def test_round_trips_a_grouped_entry(self):
        entry = _entry()
        entry.group = "operation-1"

        assert JournalEntry.from_dict(entry.to_dict()) == entry

    def test_an_entry_without_a_group_reads_as_ungrouped(self):
        data = _entry().to_dict()
        del data["group"]

        assert JournalEntry.from_dict(data).group == ""

    def test_rejects_an_entry_missing_a_field(self):
        data = _entry().to_dict()
        del data["post_image_hash"]

        with pytest.raises(ValueError):
            JournalEntry.from_dict(data)

    def test_rejects_an_unknown_operation(self):
        data = _entry().to_dict()
        data["operation"] = "frobnicate"

        with pytest.raises(ValueError):
            JournalEntry.from_dict(data)

    def test_creation_is_revertable(self):
        entry = _entry()
        entry.existed_before = False
        entry.pre_image_hash = None

        assert entry.is_revertable()

    def test_overwrite_with_a_captured_pre_image_is_revertable(self):
        assert _entry().is_revertable()

    def test_overwrite_without_a_captured_pre_image_is_not_revertable(self):
        entry = _entry()
        entry.pre_image_hash = None

        assert not entry.is_revertable()


class TestJournalBlobStore:
    """Pre-image content is stored once and can be read back by digest."""

    def test_stores_and_retrieves_content(self, tmp_path):
        store = JournalBlobStore(str(tmp_path / "blobs"))

        digest = store.store(b"hello")

        assert store.retrieve(digest) == b"hello"

    def test_digest_is_the_sha256_of_the_content(self, tmp_path):
        store = JournalBlobStore(str(tmp_path / "blobs"))

        assert store.store(b"hello") == JournalBlobStore.digest_of(b"hello")

    def test_identical_content_is_stored_once(self, tmp_path):
        store = JournalBlobStore(str(tmp_path / "blobs"))

        first = store.store(b"hello")
        second = store.store(b"hello")

        assert first == second
        assert len(os.listdir(os.path.join(store.directory(), first[:2]))) == 1

    def test_fans_out_on_the_first_two_characters(self, tmp_path):
        store = JournalBlobStore(str(tmp_path / "blobs"))

        digest = store.store(b"hello")

        assert os.path.dirname(store.path_for(digest)).endswith(digest[:2])

    def test_retrieves_nothing_for_an_unknown_digest(self, tmp_path):
        store = JournalBlobStore(str(tmp_path / "blobs"))

        assert store.retrieve("0" * 64) is None


class TestJournalIndex:
    """The index appends entries, numbers them contiguously, and reads them back."""

    def test_assigns_contiguous_sequences_from_one(self, tmp_path):
        index = JournalIndex()
        index.open(str(tmp_path / "journal"))

        first = index.append(_entry("a.txt"))
        second = index.append(_entry("b.txt"))

        assert (first.sequence, second.sequence) == (1, 2)
        assert index.count() == 2

    def test_reads_entries_in_sequence_order(self, tmp_path):
        index = JournalIndex()
        index.open(str(tmp_path / "journal"))
        index.append(_entry("a.txt"))
        index.append(_entry("b.txt"))

        assert [e.path for e in index.read_all()] == ["a.txt", "b.txt"]

    def test_reading_a_range_returns_the_requested_entries(self, tmp_path):
        index = JournalIndex()
        index.open(str(tmp_path / "journal"))
        for name in ("a.txt", "b.txt", "c.txt"):
            index.append(_entry(name))

        assert [e.path for e in index.read(2, 1)] == ["b.txt"]

    def test_reopening_continues_the_sequence(self, tmp_path):
        directory = str(tmp_path / "journal")
        first_index = JournalIndex()
        first_index.open(directory)
        first_index.append(_entry("a.txt"))
        first_index.append(_entry("b.txt"))
        first_index.close()

        reopened = JournalIndex()
        reopened.open(directory)
        third = reopened.append(_entry("c.txt"))

        assert reopened.count() == 3
        assert third.sequence == 3

    def test_rotation_starts_a_new_segment(self, tmp_path, monkeypatch):
        monkeypatch.setattr(JournalIndex, "SEGMENT_MAX_BYTES", 200)
        directory = str(tmp_path / "journal")
        index = JournalIndex()
        index.open(directory)

        for name in ("a.txt", "b.txt", "c.txt"):
            index.append(_entry(name))

        segments = [name for name in os.listdir(directory) if name.endswith(".jsonl")]
        assert len(segments) > 1
        assert [e.path for e in index.read_all()] == ["a.txt", "b.txt", "c.txt"]

    def test_skips_an_unreadable_line(self, tmp_path):
        directory = str(tmp_path / "journal")
        index = JournalIndex()
        index.open(directory)
        index.append(_entry("a.txt"))

        segment = os.path.join(directory, sorted(os.listdir(directory))[0])
        with open(segment, 'a', encoding='utf-8') as f:
            f.write("{not json\n")

        assert [e.path for e in index.read_all()] == ["a.txt"]


class TestFileJournal:
    """The journal records a write's pre-image and reports what it could not capture."""

    def _journal(self, tmp_path) -> FileJournal:
        """Build a journal rooted in a temp directory."""
        journal = FileJournal()
        journal.open(str(tmp_path / "journal"))
        return journal

    def test_records_a_creation_without_a_pre_image(self, tmp_path):
        journal = self._journal(tmp_path)
        path = str(tmp_path / "new.txt")

        entry = journal.record_write(path, "new.txt", b"hello", JournalActor.AI, "conv")

        assert entry is not None
        assert not entry.existed_before
        assert entry.pre_image_hash is None
        assert journal.pre_image(entry) is None

    def test_records_an_overwrite_with_its_pre_image(self, tmp_path):
        journal = self._journal(tmp_path)
        path = tmp_path / "existing.txt"
        path.write_bytes(b"before")

        entry = journal.record_write(str(path), "existing.txt", b"after", JournalActor.AI, "conv")

        assert entry is not None
        assert entry.existed_before
        assert journal.pre_image(entry) == b"before"
        assert entry.post_image_hash == JournalBlobStore.digest_of(b"after")

    def test_records_an_oversized_file_without_capturing_it(self, tmp_path, monkeypatch):
        monkeypatch.setattr(FileJournal, "MAX_CAPTURE_BYTES", 4)
        journal = self._journal(tmp_path)
        path = tmp_path / "large.bin"
        path.write_bytes(b"far too large to capture")

        entry = journal.record_write(str(path), "large.bin", b"after", JournalActor.AI, "conv")

        assert entry is not None
        assert entry.existed_before
        assert entry.pre_image_hash is None
        assert not entry.is_revertable()

    def test_returns_none_when_the_entry_cannot_be_written(self, tmp_path, monkeypatch):
        journal = self._journal(tmp_path)
        path = tmp_path / "existing.txt"
        path.write_bytes(b"before")

        def _fail(entry):
            raise OSError("disk full")

        monkeypatch.setattr(journal._index, "append", _fail)

        assert journal.record_write(str(path), "existing.txt", b"after", JournalActor.AI, "conv") is None

    def test_entries_for_path_returns_only_that_path(self, tmp_path):
        journal = self._journal(tmp_path)
        journal.record_write(str(tmp_path / "a.txt"), "a.txt", b"one", JournalActor.AI, "conv")
        journal.record_write(str(tmp_path / "a.txt"), "a.txt", b"two", JournalActor.AI, "conv")
        journal.record_write(str(tmp_path / "b.txt"), "b.txt", b"three", JournalActor.AI, "conv")

        assert [e.sequence for e in journal.entries_for_path("a.txt")] == [1, 2]

    def test_records_the_conversation_and_actor(self, tmp_path):
        journal = self._journal(tmp_path)

        entry = journal.record_write(str(tmp_path / "a.txt"), "a.txt", b"x", JournalActor.HUMAN, "conversation-9")

        assert entry is not None
        assert entry.actor == JournalActor.HUMAN
        assert entry.conversation == "conversation-9"

    def test_records_the_group(self, tmp_path):
        journal = self._journal(tmp_path)

        entry = journal.record_write(str(tmp_path / "a.txt"), "a.txt", b"x", JournalActor.AI, "conv", "group-1")

        assert entry is not None
        assert entry.group == "group-1"

    def test_a_single_mutation_records_no_group(self, tmp_path):
        journal = self._journal(tmp_path)

        entry = journal.record_write(str(tmp_path / "a.txt"), "a.txt", b"x", JournalActor.AI, "conv")

        assert entry is not None
        assert entry.group == ""

    def test_entries_since_returns_only_later_mutations(self, tmp_path):
        journal = self._journal(tmp_path)
        journal.record_write(str(tmp_path / "a.txt"), "a.txt", b"one", JournalActor.AI, "conv")
        cutoff = datetime.now(timezone.utc)
        journal.record_write(str(tmp_path / "b.txt"), "b.txt", b"two", JournalActor.AI, "conv")

        assert [e.path for e in journal.entries_since(cutoff)] == ["b.txt"]

    def test_entries_since_returns_nothing_for_a_future_cutoff(self, tmp_path):
        journal = self._journal(tmp_path)
        journal.record_write(str(tmp_path / "a.txt"), "a.txt", b"one", JournalActor.AI, "conv")

        assert journal.entries_since(datetime.now(timezone.utc) + timedelta(hours=1)) == []

    def test_oldest_entry_of_an_empty_journal_is_none(self, tmp_path):
        assert self._journal(tmp_path).oldest_entry() is None

    def test_oldest_entry_is_the_first_mutation(self, tmp_path):
        journal = self._journal(tmp_path)
        journal.record_write(str(tmp_path / "a.txt"), "a.txt", b"one", JournalActor.AI, "conv")
        journal.record_write(str(tmp_path / "b.txt"), "b.txt", b"two", JournalActor.AI, "conv")

        oldest = journal.oldest_entry()

        assert oldest is not None
        assert oldest.sequence == 1

    def test_reopening_keeps_the_recorded_entries(self, tmp_path):
        directory = str(tmp_path / "journal")
        first = FileJournal()
        first.open(directory)
        first.record_write(str(tmp_path / "a.txt"), "a.txt", b"one", JournalActor.AI, "conv")
        first.close()

        reopened = FileJournal()
        reopened.open(directory)

        assert reopened.count() == 1
        assert [e.path for e in reopened.entries_for_path("a.txt")] == ["a.txt"]


class TestMindspaceJournal:
    """A mindspace owns its journal and opens it alongside its other state."""

    def test_journal_directory_lives_under_humbug(self, tmp_path, monkeypatch):
        mindspace = _make_mindspace(tmp_path, monkeypatch)

        assert mindspace.journal_dir().endswith(os.path.join(".humbug", "journal"))
        assert os.path.isdir(mindspace.journal_dir())

    def test_file_journal_is_unavailable_without_a_mindspace(self):
        mindspace = Mindspace(
            on_settings_changed=lambda: None,
            on_interactions_updated=lambda: None,
        )

        with pytest.raises(AssertionError):
            mindspace.file_journal()

    def test_opening_a_mindspace_opens_its_journal(self, tmp_path):
        path = os.path.join(str(tmp_path), "ms")
        os.makedirs(os.path.join(path, ".humbug"))
        MindspaceSettings(enabled_tools={}).save(os.path.join(path, ".humbug", "settings.json"))
        mindspace = Mindspace(
            on_settings_changed=lambda: None,
            on_interactions_updated=lambda: None,
        )

        mindspace.open_mindspace(path)
        try:
            entry = mindspace.file_journal().record_write(
                os.path.join(path, "a.txt"), "a.txt", b"one", JournalActor.AI, ""
            )

            assert entry is not None
            assert mindspace.file_journal().count() == 1

        finally:
            mindspace.close_mindspace()


class TestJournalRecordFormat:
    """A recorded entry is written as one JSON object per line."""

    def test_each_line_is_one_entry(self, tmp_path):
        journal = FileJournal()
        journal.open(str(tmp_path / "journal"))
        journal.record_write(str(tmp_path / "a.txt"), "a.txt", b"one", JournalActor.AI, "conv")
        journal.record_write(str(tmp_path / "b.txt"), "b.txt", b"two", JournalActor.AI, "conv")

        segment = os.path.join(str(tmp_path / "journal"), "index-00000001.jsonl")
        with open(segment, encoding="utf-8") as f:
            lines = [json.loads(line) for line in f if line.strip()]

        assert [line["path"] for line in lines] == ["a.txt", "b.txt"]
        assert [line["sequence"] for line in lines] == [1, 2]
