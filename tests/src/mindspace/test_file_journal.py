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
from mindspace.mindspace_error import MindspaceError
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


def _entry(path: str = "notes.txt", sequence: int = 0, **overrides) -> JournalEntry:
    """Build a journal entry with plausible values."""
    values = {
        "sequence": sequence,
        "timestamp": "2026-11-03T10:00:00+00:00",
        "path": path,
        "operation": JournalOperation.WRITE,
        "actor": JournalActor.AI,
        "conversation": "conversation-1",
        "group": "",
        "existed_before": True,
        "pre_image_hash": "a" * 64,
        "post_image_hash": "b" * 64,
        "void_of": None,
    }
    values.update(overrides)
    return JournalEntry(**values)


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

    def test_a_removal_the_trash_holds_is_revertable(self):
        entry = _entry(pre_image_hash=None, pre_image_trash="ab12_notes.txt", post_image_hash=None)

        assert entry.is_revertable()

    def test_round_trips_a_directory_record(self):
        entry = _entry(pre_image_hash=None, pre_image_is_directory=True)

        assert JournalEntry.from_dict(entry.to_dict()).pre_image_is_directory

    def test_an_entry_without_the_directory_field_reads_as_a_file(self):
        data = _entry().to_dict()
        del data["pre_image_is_directory"]

        assert not JournalEntry.from_dict(data).pre_image_is_directory

    def test_a_removal_the_trash_holds_is_revertable(self):
        entry = _entry(pre_image_hash=None, pre_image_trash="ab12_notes.txt", post_image_hash=None)

        assert entry.is_revertable()

    def test_round_trips_a_directory_record(self):
        entry = _entry(pre_image_hash=None, pre_image_is_directory=True)

        assert JournalEntry.from_dict(entry.to_dict()).pre_image_is_directory

    def test_an_entry_without_the_directory_field_reads_as_a_file(self):
        data = _entry().to_dict()
        del data["pre_image_is_directory"]

        assert not JournalEntry.from_dict(data).pre_image_is_directory


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


class TestJournalEntryVoiding:
    """A record that cancels another one is distinguishable from a mutation."""

    def test_round_trips_a_void_record(self):
        entry = _entry()
        entry.void_of = 7

        assert JournalEntry.from_dict(entry.to_dict()).void_of == 7

    def test_a_mutation_is_not_a_void_record(self):
        assert not _entry().is_void()

    def test_a_void_record_names_the_entry_it_cancels(self):
        entry = _entry()
        entry.void_of = 7

        assert entry.is_void()

    def test_an_entry_without_a_void_field_reads_as_a_mutation(self):
        data = _entry().to_dict()
        del data["void_of"]

        assert JournalEntry.from_dict(data).void_of is None


class TestJournalBlobStoreSweep:
    """Reclaiming blobs deletes exactly the content nothing refers to."""

    def test_sweep_keeps_the_digests_it_is_given(self, tmp_path):
        store = JournalBlobStore(str(tmp_path / "blobs"))
        kept = store.store(b"keep me")
        store.store(b"reclaim me")

        store.sweep({kept})

        assert store.retrieve(kept) == b"keep me"
        assert store.digests() == {kept}

    def test_sweep_reports_the_bytes_it_freed(self, tmp_path):
        store = JournalBlobStore(str(tmp_path / "blobs"))
        store.store(b"reclaim me")

        assert store.sweep(set()) == len(b"reclaim me")

    def test_sweep_removes_a_temporary_file_left_by_an_interrupted_store(self, tmp_path):
        store = JournalBlobStore(str(tmp_path / "blobs"))
        digest = store.store(b"hello")
        temp_path = os.path.join(os.path.dirname(store.path_for(digest)), "abandoned.tmp")
        with open(temp_path, "wb") as f:
            f.write(b"partial")

        store.sweep({digest})

        assert not os.path.exists(temp_path)

    def test_total_bytes_counts_stored_content(self, tmp_path):
        store = JournalBlobStore(str(tmp_path / "blobs"))
        store.store(b"hello")

        assert store.total_bytes() == len(b"hello")


class TestJournalIndexVoiding:
    """A cancelled record is invisible to reads but still in the file."""

    def test_a_voided_entry_is_not_returned(self, tmp_path):
        index = JournalIndex()
        index.open(str(tmp_path / "journal"))
        entry = index.append(_entry("a.txt"))
        index.append(_entry("b.txt"))

        index.append(_entry("a.txt", void_of=entry.sequence))

        assert [e.path for e in index.read_all()] == ["b.txt"]

    def test_voided_sequences_reports_what_was_cancelled(self, tmp_path):
        index = JournalIndex()
        index.open(str(tmp_path / "journal"))
        entry = index.append(_entry("a.txt"))

        index.append(_entry("a.txt", void_of=entry.sequence))

        assert index.voided_sequences() == {entry.sequence}

    def test_a_voided_entry_still_occupies_a_sequence_number(self, tmp_path):
        index = JournalIndex()
        index.open(str(tmp_path / "journal"))
        first = index.append(_entry("a.txt"))
        index.append(_entry("a.txt", void_of=first.sequence))

        third = index.append(_entry("b.txt"))

        assert (first.sequence, third.sequence) == (1, 3)


class TestJournalIndexRetention:
    """Purging drops older records and keeps the sequence numbering intact."""

    def test_purging_drops_the_records_before_the_cutoff(self, tmp_path):
        index = JournalIndex()
        index.open(str(tmp_path / "journal"))
        index.append(_entry("a.txt"))
        index.append(_entry("b.txt"))
        index.append(_entry("c.txt"))

        dropped = index.drop_segments_before(3)

        assert dropped == 2
        assert [e.path for e in index.read_all()] == ["c.txt"]

    def test_purging_leaves_the_remaining_records_readable(self, tmp_path):
        directory = str(tmp_path / "journal")
        index = JournalIndex()
        index.open(directory)
        index.append(_entry("a.txt"))
        index.append(_entry("b.txt"))
        index.drop_segments_before(2)

        reopened = JournalIndex()
        reopened.open(directory)

        assert [e.path for e in reopened.read_all()] == ["b.txt"]

    def test_purging_everything_does_not_reuse_a_sequence_number(self, tmp_path):
        directory = str(tmp_path / "journal")
        index = JournalIndex()
        index.open(directory)
        index.append(_entry("a.txt"))
        index.append(_entry("b.txt"))
        index.drop_segments_before(3)

        reopened = JournalIndex()
        reopened.open(directory)
        entry = reopened.append(_entry("c.txt"))

        assert entry.sequence == 3

    def test_purging_nothing_returns_zero(self, tmp_path):
        index = JournalIndex()
        index.open(str(tmp_path / "journal"))
        index.append(_entry("a.txt"))

        assert index.drop_segments_before(1) == 0
        assert index.total_bytes() > 0


class TestFileJournalVoiding:
    """Recording a mutation that then fails leaves no trace in the journal."""

    def _journal(self, tmp_path) -> FileJournal:
        journal = FileJournal()
        journal.open(str(tmp_path / "journal"))
        return journal

    def test_voiding_cancels_the_record(self, tmp_path):
        journal = self._journal(tmp_path)
        path = tmp_path / "existing.txt"
        path.write_bytes(b"before")
        entry = journal.record_write(str(path), "existing.txt", b"after", JournalActor.AI, "conv")

        journal.void_entries([entry])

        assert journal.all_entries() == []
        assert journal.count() == 0

    def test_voiding_ignores_a_recording_that_failed(self, tmp_path):
        journal = self._journal(tmp_path)

        journal.void_entries([None])

        assert journal.all_entries() == []

    def test_voiding_one_record_leaves_the_others_alone(self, tmp_path):
        journal = self._journal(tmp_path)
        first = journal.record_write(str(tmp_path / "a.txt"), "a.txt", b"one", JournalActor.AI, "conv")
        second = journal.record_write(str(tmp_path / "b.txt"), "b.txt", b"two", JournalActor.AI, "conv")

        journal.void_entries([first])

        assert [e.sequence for e in journal.all_entries()] == [second.sequence]


class TestFileJournalAppend:
    """An append records the content the file will hold once the bytes arrive."""

    def _journal(self, tmp_path) -> FileJournal:
        journal = FileJournal()
        journal.open(str(tmp_path / "journal"))
        return journal

    def test_records_the_pre_image_of_the_existing_content(self, tmp_path):
        journal = self._journal(tmp_path)
        path = tmp_path / "log.txt"
        path.write_bytes(b"first\n")

        entry = journal.record_append(str(path), "log.txt", b"second\n", JournalActor.AI, "conv")

        assert entry is not None
        assert journal.pre_image(entry) == b"first\n"

    def test_the_post_image_is_the_old_content_followed_by_the_appended_bytes(self, tmp_path):
        journal = self._journal(tmp_path)
        path = tmp_path / "log.txt"
        path.write_bytes(b"first\n")

        entry = journal.record_append(str(path), "log.txt", b"second\n", JournalActor.AI, "conv")

        assert entry is not None
        assert entry.post_image_hash == JournalBlobStore.digest_of(b"first\nsecond\n")

    def test_an_append_to_a_missing_file_records_nothing(self, tmp_path):
        journal = self._journal(tmp_path)

        entry = journal.record_append(str(tmp_path / "new.txt"), "new.txt", b"hello", JournalActor.AI, "conv")

        assert entry is None
        assert journal.all_entries() == []


class TestFileJournalWriteFromPath:
    """A copy records the content of the file it is copied from."""

    def _journal(self, tmp_path) -> FileJournal:
        journal = FileJournal()
        journal.open(str(tmp_path / "journal"))
        return journal

    def test_the_post_image_is_the_source_content(self, tmp_path):
        journal = self._journal(tmp_path)
        source = tmp_path / "source.txt"
        source.write_bytes(b"copied content")

        entry = journal.record_write_from_path(
            str(tmp_path / "dest.txt"), "dest.txt", str(source), JournalActor.HUMAN, ""
        )

        assert entry is not None
        assert entry.post_image_hash == JournalBlobStore.digest_of(b"copied content")

    def test_the_pre_image_of_the_destination_is_captured(self, tmp_path):
        journal = self._journal(tmp_path)
        source = tmp_path / "source.txt"
        source.write_bytes(b"new")
        destination = tmp_path / "dest.txt"
        destination.write_bytes(b"old")

        entry = journal.record_write_from_path(
            str(destination), "dest.txt", str(source), JournalActor.HUMAN, ""
        )

        assert entry is not None
        assert journal.pre_image(entry) == b"old"

    def test_an_unreadable_source_records_nothing(self, tmp_path):
        journal = self._journal(tmp_path)

        entry = journal.record_write_from_path(
            str(tmp_path / "dest.txt"), "dest.txt", str(tmp_path / "missing.txt"), JournalActor.HUMAN, ""
        )

        assert entry is None


class TestFileJournalRetention:
    """History can be forgotten, and the content only it held is reclaimed."""

    def _journal(self, tmp_path) -> FileJournal:
        journal = FileJournal()
        journal.open(str(tmp_path / "journal"))
        return journal

    def test_purging_drops_the_entries_before_the_cutoff(self, tmp_path):
        journal = self._journal(tmp_path)
        journal.record_write(str(tmp_path / "a.txt"), "a.txt", b"one", JournalActor.AI, "conv")
        cutoff = datetime.now(timezone.utc)
        journal.record_write(str(tmp_path / "b.txt"), "b.txt", b"two", JournalActor.AI, "conv")

        result = journal.purge_before(cutoff)

        assert result.entries_dropped == 1
        assert [e.path for e in journal.all_entries()] == ["b.txt"]

    def test_purging_reclaims_the_content_no_entry_refers_to(self, tmp_path):
        journal = self._journal(tmp_path)
        old_path = tmp_path / "a.txt"
        old_path.write_bytes(b"earlier content")
        journal.record_write(str(old_path), "a.txt", b"one", JournalActor.AI, "conv")
        cutoff = datetime.now(timezone.utc)
        journal.record_write(str(tmp_path / "b.txt"), "b.txt", b"two", JournalActor.AI, "conv")
        size_before = journal.size_bytes()

        result = journal.purge_before(cutoff)

        assert result.bytes_reclaimed == len(b"earlier content")
        assert journal.size_bytes() < size_before

    def test_purging_keeps_the_content_a_surviving_entry_still_needs(self, tmp_path):
        journal = self._journal(tmp_path)
        path = tmp_path / "a.txt"
        path.write_bytes(b"before")
        journal.record_write(str(path), "a.txt", b"after", JournalActor.AI, "conv")

        result = journal.purge_before(datetime.now(timezone.utc) + timedelta(hours=1))

        assert result.entries_dropped == 1
        assert journal.all_entries() == []

    def test_size_bytes_counts_the_index(self, tmp_path):
        journal = self._journal(tmp_path)

        assert journal.size_bytes() == 0
        journal.record_write(str(tmp_path / "a.txt"), "a.txt", b"one", JournalActor.AI, "conv")

        assert journal.size_bytes() > 0


class TestMindspaceTrashFile:
    """A file the user removes is moved to the trash and can be restored."""

    def _mindspace(self, tmp_path):
        path = os.path.join(str(tmp_path), "ms")
        os.makedirs(os.path.join(path, ".humbug"))
        MindspaceSettings(enabled_tools={}).save(os.path.join(path, ".humbug", "settings.json"))
        mindspace = Mindspace(
            on_settings_changed=lambda: None,
            on_interactions_updated=lambda: None,
        )
        mindspace.open_mindspace(path)
        return mindspace

    def test_the_file_moves_into_the_trash(self, tmp_path):
        mindspace = self._mindspace(tmp_path)
        target = tmp_path / "ms" / "notes.txt"
        target.write_bytes(b"content")
        try:
            trash_path = mindspace.trash_file(str(target), JournalActor.HUMAN)

            assert not target.exists()
            assert os.path.isfile(trash_path)

        finally:
            mindspace.close_mindspace()

    def test_the_removal_is_recorded_with_the_trash_entry(self, tmp_path):
        mindspace = self._mindspace(tmp_path)
        target = tmp_path / "ms" / "notes.txt"
        target.write_bytes(b"content")
        try:
            trash_path = mindspace.trash_file(str(target), JournalActor.HUMAN)

            entries = mindspace.file_journal().entries_for_path("notes.txt")
            assert len(entries) == 1
            assert entries[0].pre_image_trash == os.path.basename(trash_path)

        finally:
            mindspace.close_mindspace()

    def test_the_recorded_version_can_be_read_back_from_the_trash(self, tmp_path):
        mindspace = self._mindspace(tmp_path)
        target = tmp_path / "ms" / "notes.txt"
        target.write_bytes(b"content")
        try:
            mindspace.trash_file(str(target), JournalActor.HUMAN)

            entry = mindspace.file_journal().entries_for_path("notes.txt")[0]
            assert mindspace.journal_pre_image(entry) == b"content"

        finally:
            mindspace.close_mindspace()

    def test_a_trashed_file_can_be_reverted(self, tmp_path):
        mindspace = self._mindspace(tmp_path)
        target = tmp_path / "ms" / "notes.txt"
        target.write_bytes(b"content")
        try:
            mindspace.trash_file(str(target), JournalActor.HUMAN)
            entries = mindspace.file_journal().entries_for_path("notes.txt")

            result = mindspace.revert_journal_entries(entries)

            assert result.reverted_paths() == ["notes.txt"]
            assert target.read_bytes() == b"content"

        finally:
            mindspace.close_mindspace()

    def test_a_path_inside_humbug_is_refused(self, tmp_path):
        mindspace = self._mindspace(tmp_path)
        target = tmp_path / "ms" / ".humbug" / "notes.txt"
        target.write_bytes(b"content")
        try:
            with pytest.raises(MindspaceError):
                mindspace.trash_file(str(target), JournalActor.HUMAN)

        finally:
            mindspace.close_mindspace()


class TestDirectoryRemoval:
    """A removal of a directory records that the path existed without capturing content."""

    def _journal(self, tmp_path) -> FileJournal:
        journal = FileJournal()
        journal.open(str(tmp_path / "journal"))
        return journal

    def test_a_directory_removal_records_that_the_path_existed(self, tmp_path):
        journal = self._journal(tmp_path)
        path = tmp_path / "folder"
        path.mkdir()

        entry = journal.record_removal(
            str(path), "folder", JournalActor.HUMAN, "", trash_name="ab_folder", pre_image_is_directory=True
        )

        assert entry is not None
        assert entry.existed_before
        assert entry.pre_image_hash is None
        assert entry.pre_image_is_directory

    def test_a_directory_removal_is_revertable_through_the_trash(self, tmp_path):
        journal = self._journal(tmp_path)
        path = tmp_path / "folder"
        path.mkdir()

        entry = journal.record_removal(
            str(path), "folder", JournalActor.HUMAN, "", trash_name="ab_folder", pre_image_is_directory=True
        )

        assert entry is not None
        assert entry.is_revertable()

    def test_a_removal_whose_content_was_not_captured_is_not_revertable(self, tmp_path, monkeypatch):
        monkeypatch.setattr(FileJournal, "MAX_CAPTURE_BYTES", 4)
        journal = self._journal(tmp_path)
        path = tmp_path / "large.bin"
        path.write_bytes(b"far too large to capture")

        entry = journal.record_removal(str(path), "large.bin", JournalActor.AI, "")

        assert entry is not None
        assert not entry.is_revertable()


class TestMindspaceTrashDirectory:
    """An empty directory the user removes goes to the trash and can be restored."""

    def _mindspace(self, tmp_path):
        path = os.path.join(str(tmp_path), "ms")
        os.makedirs(os.path.join(path, ".humbug"))
        MindspaceSettings(enabled_tools={}).save(os.path.join(path, ".humbug", "settings.json"))
        mindspace = Mindspace(
            on_settings_changed=lambda: None,
            on_interactions_updated=lambda: None,
        )
        mindspace.open_mindspace(path)
        return mindspace

    def test_the_directory_moves_into_the_trash(self, tmp_path):
        mindspace = self._mindspace(tmp_path)
        target = tmp_path / "ms" / "folder"
        target.mkdir()
        try:
            trash_path = mindspace.trash_file(str(target), JournalActor.HUMAN)

            assert not target.exists()
            assert os.path.isdir(trash_path)

        finally:
            mindspace.close_mindspace()

    def test_the_removal_is_recorded_as_a_directory(self, tmp_path):
        mindspace = self._mindspace(tmp_path)
        target = tmp_path / "ms" / "folder"
        target.mkdir()
        try:
            mindspace.trash_file(str(target), JournalActor.HUMAN)

            entry = mindspace.file_journal().entries_for_path("folder")[0]
            assert entry.pre_image_is_directory
            assert entry.existed_before
            assert entry.is_revertable()

        finally:
            mindspace.close_mindspace()

    def test_a_trashed_directory_can_be_reverted(self, tmp_path):
        mindspace = self._mindspace(tmp_path)
        target = tmp_path / "ms" / "folder"
        target.mkdir()
        try:
            mindspace.trash_file(str(target), JournalActor.HUMAN)
            entries = mindspace.file_journal().entries_for_path("folder")

            result = mindspace.revert_journal_entries(entries)

            assert result.reverted_paths() == ["folder"]
            assert target.is_dir()

        finally:
            mindspace.close_mindspace()

    def test_an_emptied_trash_reports_the_removal_as_not_revertable(self, tmp_path):
        mindspace = self._mindspace(tmp_path)
        target = tmp_path / "ms" / "folder"
        target.mkdir()
        try:
            mindspace.trash_file(str(target), JournalActor.HUMAN)
            entries = mindspace.file_journal().entries_for_path("folder")
            mindspace.empty_trash()

            result = mindspace.revert_journal_entries(entries)

            assert result.failed_outcomes()[0].status.value == "not_revertable"
            assert not target.exists()

        finally:
            mindspace.close_mindspace()


class TestJournalIndexSeek:
    """Reading from a sequence number does not re-read the records before it."""

    def test_reads_the_requested_record_and_the_ones_after_it(self, tmp_path):
        index = JournalIndex()
        index.open(str(tmp_path / "journal"))
        for number in range(200):
            index.append(_entry(f"file{number}.txt"))

        assert [e.sequence for e in index.read(150, 10)] == list(range(150, 160))

    def test_reading_the_newest_record_only(self, tmp_path):
        index = JournalIndex()
        index.open(str(tmp_path / "journal"))
        for number in range(200):
            index.append(_entry(f"file{number}.txt"))

        entries = index.read(200, 1)

        assert [e.sequence for e in entries] == [200]
        assert entries[0].path == "file199.txt"

    def test_a_start_before_the_segment_still_reads_from_its_beginning(self, tmp_path):
        index = JournalIndex()
        index.open(str(tmp_path / "journal"))
        index.append(_entry("a.txt"))
        index.append(_entry("b.txt"))

        assert [e.path for e in index.read(1, 10)] == ["a.txt", "b.txt"]

    def test_reading_across_a_rotation_keeps_every_record(self, tmp_path, monkeypatch):
        monkeypatch.setattr(JournalIndex, "SEGMENT_MAX_BYTES", 400)
        index = JournalIndex()
        index.open(str(tmp_path / "journal"))
        for number in range(50):
            index.append(_entry(f"file{number}.txt"))

        assert [e.sequence for e in index.read(45, 10)] == list(range(45, 51))

    def test_records_of_very_different_sizes_are_still_found(self, tmp_path):
        index = JournalIndex()
        index.open(str(tmp_path / "journal"))
        index.append(_entry("small.txt"))
        index.append(_entry("x" * 4000 + ".txt"))
        index.append(_entry("last.txt"))

        assert [e.path for e in index.read(3, 1)] == ["last.txt"]

    def test_a_cancelled_record_is_still_skipped_when_seeking(self, tmp_path):
        index = JournalIndex()
        index.open(str(tmp_path / "journal"))
        for number in range(50):
            index.append(_entry(f"file{number}.txt"))
        index.append(_entry("voided.txt", void_of=45))

        assert [e.sequence for e in index.read(45, 10)] == [46, 47, 48, 49, 50]


class TestFileJournalIncrementalRead:
    """The journal exposes the newest record and what has been added since."""

    def _journal(self, tmp_path) -> FileJournal:
        journal = FileJournal()
        journal.open(str(tmp_path / "journal"))
        return journal

    def test_newest_sequence_is_zero_when_nothing_is_recorded(self, tmp_path):
        assert self._journal(tmp_path).newest_sequence() == 0

    def test_newest_sequence_advances_with_each_record(self, tmp_path):
        journal = self._journal(tmp_path)
        journal.record_write(str(tmp_path / "a.txt"), "a.txt", b"one", JournalActor.AI, "conv")
        journal.record_write(str(tmp_path / "b.txt"), "b.txt", b"two", JournalActor.AI, "conv")

        assert journal.newest_sequence() == 2

    def test_entries_from_returns_only_what_was_added_since(self, tmp_path):
        journal = self._journal(tmp_path)
        journal.record_write(str(tmp_path / "a.txt"), "a.txt", b"one", JournalActor.AI, "conv")
        seen = journal.newest_sequence()
        journal.record_write(str(tmp_path / "b.txt"), "b.txt", b"two", JournalActor.AI, "conv")

        assert [e.path for e in journal.entries_from(seen + 1, 10)] == ["b.txt"]
