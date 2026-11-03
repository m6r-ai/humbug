"""Tests for reversing the mutations recorded in the mindspace change journal."""

import os

from mindspace.file_journal import FileJournal
from mindspace.journal_blob_store import JournalBlobStore
from mindspace.journal_entry import JournalActor
from mindspace.journal_reversal import JournalReverser, JournalRevertStatus
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
        journal.MAX_CAPTURE_BYTES = 4
        (tmp_path / "a.txt").write_bytes(b"far too large to capture")
        _write_and_record(journal, tmp_path, "a.txt", b"changed")

        result = _reverser(journal, tmp_path).revert(journal.entries_for_path("a.txt"))

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


class TestTrashBackedRemoval:
    """A removal keeps its content in the trash, so the trash is what restores it."""

    def _removal(self, tmp_path, trash_name: str = "abc_a.txt") -> FileJournal:
        """Record and perform a removal that parks the content in a trash directory."""
        journal = _journal(tmp_path)
        trash_dir = tmp_path / "trash"
        trash_dir.mkdir(exist_ok=True)
        (tmp_path / "a.txt").write_bytes(b"payload")
        journal.record_removal(
            str(tmp_path / "a.txt"), "a.txt", JournalActor.HUMAN, "", trash_name=trash_name
        )
        os.replace(tmp_path / "a.txt", trash_dir / trash_name)
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
