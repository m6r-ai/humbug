"""Tests for reversing the mutations recorded in the mindspace change journal."""

import json
import os

from mindspace.file_journal import FileJournal
from mindspace.journal_blob_store import JournalBlobStore
from mindspace.journal_entry import JournalActor
from mindspace.journal_reversal import JournalReverser, JournalRevertOutcome, JournalRevertStatus
from mindspace.mindspace import Mindspace
from mindspace.mindspace_settings import MindspaceSettings


def _journal(tmp_path) -> FileJournal:
    """Build a journal rooted in a temp directory."""
    journal = FileJournal()
    journal.open(str(tmp_path / "journal"))
    return journal


def _reverser(journal: FileJournal, tmp_path) -> JournalReverser:
    """Build a reverser whose mindspace root is the temp directory."""
    return JournalReverser(journal, str(tmp_path), str(tmp_path), _no_trash)


def _no_trash(trash_name: str) -> str:
    """Stand in for a trash restore in tests that never reach the trash."""
    raise AssertionError(f"unexpected trash restore of '{trash_name}'")


def _write_and_record(journal: FileJournal, tmp_path, name: str, content: bytes) -> None:
    """
    Write content to a file and record the write the way a tool would.

    The record is made before the write, so the journal captures what the file
    held beforehand.
    """
    path = tmp_path / name
    journal.record_write(str(path), name, content, JournalActor.AI, "conv")
    path.write_bytes(content)


class TestRevertSinglePath:
    """A path is restored to the state it held before the reverted mutations."""

    def test_reverts_an_overwrite(self, tmp_path):
        journal = _journal(tmp_path)
        (tmp_path / "a.txt").write_bytes(b"original")
        _write_and_record(journal, tmp_path, "a.txt", b"changed")

        result = _reverser(journal, tmp_path).revert(journal.entries_for_path("a.txt"))

        assert result.reverted_paths() == ["a.txt"]
        assert (tmp_path / "a.txt").read_bytes() == b"original"

    def test_reverts_several_mutations_to_the_earliest_pre_image(self, tmp_path):
        journal = _journal(tmp_path)
        (tmp_path / "a.txt").write_bytes(b"one")
        _write_and_record(journal, tmp_path, "a.txt", b"two")
        _write_and_record(journal, tmp_path, "a.txt", b"three")

        result = _reverser(journal, tmp_path).revert(journal.entries_for_path("a.txt"))

        assert result.reverted_paths() == ["a.txt"]
        assert (tmp_path / "a.txt").read_bytes() == b"one"

    def test_reverting_a_creation_removes_the_file(self, tmp_path):
        journal = _journal(tmp_path)
        _write_and_record(journal, tmp_path, "new.txt", b"created")

        result = _reverser(journal, tmp_path).revert(journal.entries_for_path("new.txt"))

        assert result.reverted_paths() == ["new.txt"]
        assert not (tmp_path / "new.txt").exists()

    def test_reverting_a_creation_of_an_already_absent_file_succeeds(self, tmp_path):
        journal = _journal(tmp_path)
        _write_and_record(journal, tmp_path, "new.txt", b"created")
        os.remove(tmp_path / "new.txt")

        result = _reverser(journal, tmp_path).revert(journal.entries_for_path("new.txt"))

        assert result.outcomes[0].status == JournalRevertStatus.REVERTED
        assert result.outcomes[0].detail == "already absent"

    def test_reverting_nothing_changes_nothing(self, tmp_path):
        journal = _journal(tmp_path)

        result = _reverser(journal, tmp_path).revert([])

        assert result.outcomes == []


class TestRevertConflicts:
    """A path changed by something the journal did not record is reported, not overwritten."""

    def test_reports_a_file_changed_after_the_recorded_mutation(self, tmp_path):
        journal = _journal(tmp_path)
        (tmp_path / "a.txt").write_bytes(b"original")
        _write_and_record(journal, tmp_path, "a.txt", b"changed")
        (tmp_path / "a.txt").write_bytes(b"edited by hand")

        result = _reverser(journal, tmp_path).revert(journal.entries_for_path("a.txt"))

        assert result.outcomes[0].status == JournalRevertStatus.CONFLICT
        assert (tmp_path / "a.txt").read_bytes() == b"edited by hand"

    def test_reports_a_missing_file(self, tmp_path):
        journal = _journal(tmp_path)
        (tmp_path / "a.txt").write_bytes(b"original")
        _write_and_record(journal, tmp_path, "a.txt", b"changed")
        os.remove(tmp_path / "a.txt")

        result = _reverser(journal, tmp_path).revert(journal.entries_for_path("a.txt"))

        assert result.outcomes[0].status == JournalRevertStatus.CONFLICT
        assert "no longer present" in result.outcomes[0].detail

    def test_reports_a_change_between_two_recorded_mutations(self, tmp_path):
        journal = _journal(tmp_path)
        (tmp_path / "a.txt").write_bytes(b"one")
        _write_and_record(journal, tmp_path, "a.txt", b"two")
        (tmp_path / "a.txt").write_bytes(b"edited by hand")
        journal.record_write(str(tmp_path / "a.txt"), "a.txt", b"three", JournalActor.AI, "conv")
        (tmp_path / "a.txt").write_bytes(b"three")

        result = _reverser(journal, tmp_path).revert(journal.entries_for_path("a.txt"))

        assert result.outcomes[0].status == JournalRevertStatus.CONFLICT
        assert (tmp_path / "a.txt").read_bytes() == b"three"


class TestRevertNotRevertable:
    """A mutation whose pre-image was never captured is reported as such."""

    def test_reports_an_uncaptured_pre_image(self, tmp_path):
        journal = _journal(tmp_path)
        (tmp_path / "a.txt").write_bytes(b"changed")
        entry = journal.record_detected(
            "a.txt", "0" * 64, JournalBlobStore.digest_of(b"changed"), True
        )
        assert entry is not None

        result = _reverser(journal, tmp_path).revert([entry])

        assert result.outcomes[0].status == JournalRevertStatus.NOT_REVERTABLE
        assert (tmp_path / "a.txt").read_bytes() == b"changed"


class TestRevertSeveralPaths:
    """Paths are reverted independently, so one conflict does not block the rest."""

    def test_reverts_the_clean_paths_and_reports_the_conflicted_one(self, tmp_path):
        journal = _journal(tmp_path)
        (tmp_path / "a.txt").write_bytes(b"a-original")
        (tmp_path / "b.txt").write_bytes(b"b-original")
        _write_and_record(journal, tmp_path, "a.txt", b"a-changed")
        _write_and_record(journal, tmp_path, "b.txt", b"b-changed")
        (tmp_path / "b.txt").write_bytes(b"edited by hand")

        entries = journal.entries_for_path("a.txt") + journal.entries_for_path("b.txt")
        result = _reverser(journal, tmp_path).revert(entries)

        assert result.reverted_paths() == ["a.txt"]
        assert [o.path for o in result.failed_outcomes()] == ["b.txt"]
        assert (tmp_path / "a.txt").read_bytes() == b"a-original"
        assert (tmp_path / "b.txt").read_bytes() == b"edited by hand"

    def test_entry_order_does_not_matter(self, tmp_path):
        journal = _journal(tmp_path)
        (tmp_path / "a.txt").write_bytes(b"one")
        _write_and_record(journal, tmp_path, "a.txt", b"two")
        _write_and_record(journal, tmp_path, "a.txt", b"three")

        entries = list(reversed(journal.entries_for_path("a.txt")))
        result = _reverser(journal, tmp_path).revert(entries)

        assert result.reverted_paths() == ["a.txt"]
        assert (tmp_path / "a.txt").read_bytes() == b"one"


class TestMindspaceRevert:
    """A mindspace performs a reversal against its own root and journal."""

    def test_reverts_a_write_recorded_against_the_open_mindspace(self, tmp_path):
        path = os.path.join(str(tmp_path), "ms")
        os.makedirs(os.path.join(path, ".humbug"))
        MindspaceSettings(enabled_tools={}).save(os.path.join(path, ".humbug", "settings.json"))
        mindspace = Mindspace(
            on_settings_changed=lambda: None,
            on_interactions_updated=lambda: None,
        )

        mindspace.open_mindspace(path)
        try:
            target = os.path.join(path, "a.txt")
            with open(target, 'wb') as f:
                f.write(b"original")

            mindspace.file_journal().record_write(target, "a.txt", b"changed", JournalActor.AI, "conv")
            with open(target, 'wb') as f:
                f.write(b"changed")

            result = mindspace.revert_journal_entries(mindspace.file_journal().entries_for_path("a.txt"))

            assert result.reverted_paths() == ["a.txt"]
            with open(target, 'rb') as f:
                assert f.read() == b"original"

        finally:
            mindspace.close_mindspace()


class TestRevertIsRecorded:
    """A reversal is itself a mutation, so it leaves a continuous journal behind."""

    def test_restoring_a_file_appends_a_revert_entry(self, tmp_path):
        journal = _journal(tmp_path)
        (tmp_path / "a.txt").write_bytes(b"original")
        _write_and_record(journal, tmp_path, "a.txt", b"changed")

        _reverser(journal, tmp_path).revert(journal.entries_for_path("a.txt"))

        entries = journal.entries_for_path("a.txt")
        assert [e.sequence for e in entries] == [1, 2]
        assert entries[1].actor == JournalActor.HUMAN
        assert entries[1].pre_image_hash == entries[0].post_image_hash
        assert journal.pre_image(entries[1]) == b"changed"

    def test_removing_a_created_file_appends_a_delete_entry(self, tmp_path):
        journal = _journal(tmp_path)
        _write_and_record(journal, tmp_path, "new.txt", b"created")

        _reverser(journal, tmp_path).revert(journal.entries_for_path("new.txt"))

        entries = journal.entries_for_path("new.txt")
        assert [e.sequence for e in entries] == [1, 2]
        assert entries[1].operation.value == "delete"
        assert entries[1].post_image_hash is None
        assert entries[1].pre_image_hash == entries[0].post_image_hash

    def test_reverting_the_revert_restores_the_later_content(self, tmp_path):
        journal = _journal(tmp_path)
        (tmp_path / "a.txt").write_bytes(b"original")
        _write_and_record(journal, tmp_path, "a.txt", b"changed")
        _reverser(journal, tmp_path).revert(journal.entries_for_path("a.txt"))

        revert_entry = journal.entries_for_path("a.txt")[-1]
        result = _reverser(journal, tmp_path).revert([revert_entry])

        assert result.reverted_paths() == ["a.txt"]
        assert (tmp_path / "a.txt").read_bytes() == b"changed"

    def test_an_already_absent_creation_records_nothing(self, tmp_path):
        journal = _journal(tmp_path)
        _write_and_record(journal, tmp_path, "new.txt", b"created")
        os.remove(tmp_path / "new.txt")

        _reverser(journal, tmp_path).revert(journal.entries_for_path("new.txt"))

        assert len(journal.entries_for_path("new.txt")) == 1

    def test_a_conflicted_revert_records_nothing(self, tmp_path):
        journal = _journal(tmp_path)
        (tmp_path / "a.txt").write_bytes(b"original")
        _write_and_record(journal, tmp_path, "a.txt", b"changed")
        (tmp_path / "a.txt").write_bytes(b"edited by hand")

        _reverser(journal, tmp_path).revert(journal.entries_for_path("a.txt"))

        assert len(journal.entries_for_path("a.txt")) == 1

    def test_reports_a_restore_that_could_not_be_written(self, tmp_path, monkeypatch):
        journal = _journal(tmp_path)
        (tmp_path / "a.txt").write_bytes(b"original")
        _write_and_record(journal, tmp_path, "a.txt", b"changed")

        def _fail_replace(_source, _destination):
            raise OSError("disk full")

        monkeypatch.setattr(os, "replace", _fail_replace)
        result = _reverser(journal, tmp_path).revert(journal.entries_for_path("a.txt"))

        assert result.outcomes[0].status == JournalRevertStatus.FAILED
        assert "disk full" in result.outcomes[0].detail
        assert (tmp_path / "a.txt").read_bytes() == b"changed"


class TestGroupedOperations:
    """The entries of one operation are reverted together or not at all."""

    def _record_move(self, journal, tmp_path, group: str = "group-1") -> None:
        """Record and perform a move of a.txt to b.txt as two grouped entries."""
        (tmp_path / "a.txt").write_bytes(b"payload")
        journal.record_write_from_path(
            str(tmp_path / "b.txt"), "b.txt", str(tmp_path / "a.txt"), JournalActor.AI, "conv", group
        )
        journal.record_removal(str(tmp_path / "a.txt"), "a.txt", JournalActor.AI, "conv", group)
        os.replace(tmp_path / "a.txt", tmp_path / "b.txt")

    def test_reverting_one_half_of_a_move_is_refused(self, tmp_path):
        journal = _journal(tmp_path)
        self._record_move(journal, tmp_path)

        destination_only = [e for e in journal.all_entries() if e.path == "b.txt"]
        result = _reverser(journal, tmp_path).revert(destination_only)

        assert result.outcomes[0].status == JournalRevertStatus.INCOMPLETE
        assert (tmp_path / "b.txt").read_bytes() == b"payload"
        assert not (tmp_path / "a.txt").exists()

    def test_reverting_both_halves_restores_the_original_layout(self, tmp_path):
        journal = _journal(tmp_path)
        self._record_move(journal, tmp_path)

        result = _reverser(journal, tmp_path).revert(journal.all_entries())

        assert sorted(result.reverted_paths()) == ["a.txt", "b.txt"]
        assert (tmp_path / "a.txt").read_bytes() == b"payload"
        assert not (tmp_path / "b.txt").exists()

    def test_an_ungrouped_entry_is_not_affected_by_the_rule(self, tmp_path):
        journal = _journal(tmp_path)
        (tmp_path / "a.txt").write_bytes(b"original")
        _write_and_record(journal, tmp_path, "a.txt", b"changed")

        result = _reverser(journal, tmp_path).revert(journal.all_entries())

        assert result.reverted_paths() == ["a.txt"]


class TestRemovalRecordedWithATrashLocator:
    """
    A removal recorded before the journal held removed content is restored from the trash.

    Such a record names the trash entry that kept the content instead of storing
    it, because the trash was then where a user's removal went.  Nothing writes a
    record like that any more, so a mindspace that holds one is reproduced in the
    index format rather than through the writer.
    """

    def _removal(self, tmp_path, trash_name: str = "abc_a.txt") -> FileJournal:
        """Build a journal holding one removal recorded against a trash entry."""
        journal_dir = tmp_path / "journal"
        journal_dir.mkdir(exist_ok=True)
        trash_dir = tmp_path / "trash"
        trash_dir.mkdir(exist_ok=True)
        (tmp_path / "a.txt").write_bytes(b"payload")

        record = {
            "sequence": 1,
            "timestamp": "2026-01-01T00:00:00+00:00",
            "path": "a.txt",
            "operation": "delete",
            "actor": "human",
            "conversation": "",
            "group": "",
            "existed_before": True,
            "pre_image_hash": JournalBlobStore.digest_of(b"payload"),
            "post_image_hash": None,
            "pre_image_trash": trash_name,
            "pre_image_is_directory": False,
            "void_of": None,
        }
        segment = journal_dir / "index-00000001.jsonl"
        segment.write_text(json.dumps(record) + "\n", encoding="utf-8")
        os.replace(tmp_path / "a.txt", trash_dir / trash_name)

        journal = FileJournal()
        journal.open(str(journal_dir))
        return journal

    def test_the_content_is_not_copied_into_the_journal(self, tmp_path):
        journal = self._removal(tmp_path)

        entry = journal.entries_for_path("a.txt")[0]

        assert entry.pre_image_trash == "abc_a.txt"
        assert entry.pre_image_hash == JournalBlobStore.digest_of(b"payload")
        assert journal.pre_image(entry) is None

    def test_restores_the_content_from_the_trash(self, tmp_path):
        journal = self._removal(tmp_path)
        trash_dir = tmp_path / "trash"

        def restore(trash_name: str) -> str:
            os.replace(trash_dir / trash_name, tmp_path / "a.txt")
            return str(tmp_path / "a.txt")

        reverser = JournalReverser(journal, str(tmp_path), str(trash_dir), restore)
        result = reverser.revert(journal.entries_for_path("a.txt"))

        assert result.reverted_paths() == ["a.txt"]
        assert (tmp_path / "a.txt").read_bytes() == b"payload"

    def test_reports_an_emptied_trash_rather_than_losing_the_removal(self, tmp_path):
        journal = self._removal(tmp_path)
        trash_dir = tmp_path / "trash"
        os.remove(trash_dir / "abc_a.txt")

        def restore(trash_name: str) -> str:
            raise AssertionError("the trash entry is gone, so nothing should be restored")

        reverser = JournalReverser(journal, str(tmp_path), str(trash_dir), restore)
        result = reverser.revert(journal.entries_for_path("a.txt"))

        assert result.outcomes[0].status == JournalRevertStatus.NOT_REVERTABLE
        assert "no longer holds" in result.outcomes[0].detail


class TestDirectoryRemoval:
    """A recorded directory holds no content, so reversal recreates it."""

    def _removed_directory(self, tmp_path) -> FileJournal:
        """Record the removal of an empty directory and perform it."""
        journal = _journal(tmp_path)
        path = tmp_path / "folder"
        path.mkdir()
        journal.record_removal(
            str(path), "folder", JournalActor.HUMAN, "", pre_image_is_directory=True
        )
        os.rmdir(path)
        return journal

    def test_reverting_the_removal_recreates_the_directory(self, tmp_path):
        journal = self._removed_directory(tmp_path)

        result = _reverser(journal, tmp_path).revert(journal.entries_for_path("folder"))

        assert result.reverted_paths() == ["folder"]
        assert (tmp_path / "folder").is_dir()

    def test_recreating_a_directory_records_nothing(self, tmp_path):
        journal = self._removed_directory(tmp_path)

        _reverser(journal, tmp_path).revert(journal.entries_for_path("folder"))

        assert len(journal.entries_for_path("folder")) == 1

    def test_a_directory_that_is_present_is_reported_as_already_reverted(self, tmp_path):
        journal = self._removed_directory(tmp_path)
        (tmp_path / "folder").mkdir()

        result = _reverser(journal, tmp_path).revert(journal.entries_for_path("folder"))

        assert result.reverted_paths() == ["folder"]
        assert result.outcomes[0].detail == "already present"

    def test_a_removal_recorded_for_a_file_still_conflicts_with_a_directory(self, tmp_path):
        """
        A directory only counts as already reverted when the record says it was one.

        The early return for a present directory is what makes an empty folder
        reversible and must not swallow a file removal whose path a directory has
        since taken.
        """
        journal = _journal(tmp_path)
        (tmp_path / "thing").write_bytes(b"content")
        journal.record_removal(str(tmp_path / "thing"), "thing", JournalActor.HUMAN, "")
        os.remove(tmp_path / "thing")
        (tmp_path / "thing").mkdir()

        result = _reverser(journal, tmp_path).revert(journal.entries_for_path("thing"))

        assert result.outcomes[0].status == JournalRevertStatus.CONFLICT


class TestUncapturedPreImage:
    """A mutation whose pre-image was too large to capture is not evidence of divergence."""

    def test_a_chain_through_an_uncaptured_pre_image_can_still_be_reverted(self, tmp_path):
        journal = _journal(tmp_path)
        path = tmp_path / "big.bin"
        path.write_bytes(b"small")
        _write_and_record(journal, tmp_path, "big.bin", b"still small")
        journal.record_detected("big.bin", None, JournalBlobStore.digest_of(b"large"), True)
        path.write_bytes(b"large")

        result = _reverser(journal, tmp_path).revert(journal.entries_for_path("big.bin"))

        assert result.reverted_paths() == ["big.bin"]
        assert path.read_bytes() == b"small"


class TestVoidedEntries:
    """A mutation that was cancelled is not reversed."""

    def test_a_voided_entry_is_ignored(self, tmp_path):
        journal = _journal(tmp_path)
        path = tmp_path / "notes.txt"
        path.write_bytes(b"before")
        entry = journal.record_write(str(path), "notes.txt", b"after", JournalActor.AI, "conv")
        journal.void_entries([entry])

        outcome = _reverser(journal, tmp_path).revert([entry])

        assert outcome.outcomes == []
        assert path.read_bytes() == b"before"

    def test_an_operation_whose_other_half_was_voided_is_reversed_as_a_whole(self, tmp_path):
        journal = _journal(tmp_path)
        source = tmp_path / "source.txt"
        source.write_bytes(b"content")
        group = "group-1"
        destination_entry = journal.record_write_from_path(
            str(tmp_path / "dest.txt"), "dest.txt", str(source), JournalActor.AI, "", group
        )
        source_entry = journal.record_removal(str(source), "source.txt", JournalActor.AI, "", group)
        os.rename(source, tmp_path / "dest.txt")
        journal.void_entries([destination_entry])

        outcome = _reverser(journal, tmp_path).revert([destination_entry, source_entry])

        assert outcome.outcomes[0].status == JournalRevertStatus.INCOMPLETE


class TestReversalRecordsWhatItDid:
    """A reversal is attributed and is itself revertable."""

    def test_the_reversal_carries_the_actor_and_conversation_it_was_given(self, tmp_path):
        journal = _journal(tmp_path)
        path = tmp_path / "notes.txt"
        path.write_bytes(b"before")
        entry = journal.record_write(str(path), "notes.txt", b"after", JournalActor.AI, "conv")
        path.write_bytes(b"after")
        reverser = JournalReverser(
            journal, str(tmp_path), str(tmp_path), _no_trash, JournalActor.AI, "conversation-9"
        )

        reverser.revert([entry])

        recorded = journal.entries_for_path("notes.txt")[-1]
        assert recorded.actor == JournalActor.AI
        assert recorded.conversation == "conversation-9"

    def test_a_failed_restore_leaves_no_record(self, tmp_path, monkeypatch):
        journal = _journal(tmp_path)
        path = tmp_path / "notes.txt"
        path.write_bytes(b"before")
        entry = journal.record_write(str(path), "notes.txt", b"after", JournalActor.AI, "conv")
        path.write_bytes(b"after")

        def _fail(_absolute_path: str, path_str: str, _content: bytes) -> JournalRevertOutcome:
            return JournalRevertOutcome(path_str, JournalRevertStatus.FAILED, "disk full")

        monkeypatch.setattr(JournalReverser, "_write", staticmethod(_fail))
        count_before = len(journal.entries_for_path("notes.txt"))

        result = _reverser(journal, tmp_path).revert([entry])

        assert result.failed_outcomes()[0].status == JournalRevertStatus.FAILED
        assert len(journal.entries_for_path("notes.txt")) == count_before


class TestDirectoryInPlaceOfFile:
    """A directory where a file is expected is reported, not overwritten."""

    def test_a_directory_at_the_path_is_a_conflict(self, tmp_path):
        journal = _journal(tmp_path)
        path = tmp_path / "notes.txt"
        path.write_bytes(b"before")
        entry = journal.record_write(str(path), "notes.txt", b"after", JournalActor.AI, "conv")
        path.unlink()
        path.mkdir()

        outcome = _reverser(journal, tmp_path).revert([entry])

        assert outcome.outcomes[0].status == JournalRevertStatus.CONFLICT
        assert path.is_dir()
