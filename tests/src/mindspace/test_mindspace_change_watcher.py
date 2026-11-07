"""Tests for the change watcher that reports changes no journal entry explains."""

import json
import os

from mindspace.file_journal import FileJournal
from mindspace.journal_entry import JournalActor
from mindspace.journal_reversal import JournalReverser
from mindspace.mindspace_change_watcher import ChangeCheckpoint, MindspaceChangeWatcher
from mindspace.mindspace_tree_scanner import TreeSnapshot


def _mindspace(tmp_path):
    root = tmp_path / "space"
    (root / ".humbug").mkdir(parents=True)
    return root


def _watcher(root, interval=5.0, pruned=frozenset({".humbug"})):
    journal = FileJournal()
    journal.open(str(root / ".humbug" / "journal"))
    watcher = MindspaceChangeWatcher(str(root), journal, pruned, interval)
    watcher.open()
    return watcher, journal


def journal_reversal_for(journal, root):
    return JournalReverser(
        journal,
        str(root),
        str(root / ".humbug" / "trash"),
        lambda name: "",
        JournalActor.HUMAN,
        "",
    )


def test_a_first_checkpoint_reports_nothing(tmp_path):
    root = _mindspace(tmp_path)
    (root / "a.txt").write_bytes(b"one")
    watcher, _journal = _watcher(root)

    checkpoint = watcher.checkpoint()

    assert checkpoint.difference.is_empty()
    assert checkpoint.unrecorded() == set()


def test_reports_a_file_created_between_checkpoints(tmp_path):
    root = _mindspace(tmp_path)
    watcher, _journal = _watcher(root)
    watcher.checkpoint()
    (root / "new.txt").write_bytes(b"content")

    checkpoint = watcher.checkpoint()

    assert checkpoint.difference.added == {"new.txt"}
    assert checkpoint.unrecorded() == {"new.txt"}


def test_reports_a_file_removed_between_checkpoints(tmp_path):
    root = _mindspace(tmp_path)
    (root / "gone.txt").write_bytes(b"content")
    watcher, _journal = _watcher(root)
    watcher.checkpoint()
    os.remove(root / "gone.txt")

    checkpoint = watcher.checkpoint()

    assert checkpoint.difference.removed == {"gone.txt"}


def test_reports_a_file_whose_content_changed(tmp_path):
    root = _mindspace(tmp_path)
    (root / "a.txt").write_bytes(b"one")
    watcher, _journal = _watcher(root)
    watcher.checkpoint()
    (root / "a.txt").write_bytes(b"a different length entirely")

    checkpoint = watcher.checkpoint()

    assert checkpoint.difference.changed == {"a.txt"}


def test_reports_a_change_of_mode_alone(tmp_path):
    root = _mindspace(tmp_path)
    (root / "a.txt").write_bytes(b"one")
    watcher, _journal = _watcher(root)
    watcher.checkpoint()
    os.chmod(root / "a.txt", 0o600)

    checkpoint = watcher.checkpoint()

    assert checkpoint.difference.changed == {"a.txt"}


def test_attributes_a_change_the_journal_recorded(tmp_path):
    root = _mindspace(tmp_path)
    (root / "a.txt").write_bytes(b"one")
    watcher, journal = _watcher(root)
    watcher.checkpoint()

    journal.record_write(str(root / "a.txt"), "a.txt", b"two", JournalActor.AI, "conv")
    (root / "a.txt").write_bytes(b"two")

    checkpoint = watcher.checkpoint()

    assert checkpoint.difference.changed == {"a.txt"}
    assert checkpoint.attributed == {"a.txt"}
    assert checkpoint.unrecorded() == set()


def test_does_not_attribute_a_change_the_journal_did_not_record(tmp_path):
    root = _mindspace(tmp_path)
    (root / "a.txt").write_bytes(b"one")
    (root / "b.txt").write_bytes(b"one")
    watcher, journal = _watcher(root)
    watcher.checkpoint()

    journal.record_write(str(root / "a.txt"), "a.txt", b"two", JournalActor.AI, "conv")
    (root / "a.txt").write_bytes(b"two")
    (root / "b.txt").write_bytes(b"changed outside Humbug")

    checkpoint = watcher.checkpoint()

    assert checkpoint.attributed == {"a.txt"}
    assert checkpoint.unrecorded() == {"b.txt"}


def test_records_nothing_when_nothing_changed(tmp_path):
    root = _mindspace(tmp_path)
    (root / "a.txt").write_bytes(b"one")
    watcher, _journal = _watcher(root)
    watcher.checkpoint()

    watcher.checkpoint()

    assert watcher.recorded_changes(10) == []


def test_records_only_the_checkpoints_that_reported_a_change(tmp_path):
    root = _mindspace(tmp_path)
    watcher, _journal = _watcher(root)
    watcher.checkpoint()
    (root / "a.txt").write_bytes(b"one")
    watcher.checkpoint()
    watcher.checkpoint()
    (root / "b.txt").write_bytes(b"two")
    watcher.checkpoint()

    recorded = watcher.recorded_changes(10)

    assert len(recorded) == 2
    assert recorded[0].difference.added == {"a.txt"}
    assert recorded[1].difference.added == {"b.txt"}


def test_reports_the_horizon_as_the_oldest_recorded_checkpoint(tmp_path):
    root = _mindspace(tmp_path)
    watcher, _journal = _watcher(root)
    watcher.checkpoint()
    (root / "a.txt").write_bytes(b"one")
    first = watcher.checkpoint()
    (root / "b.txt").write_bytes(b"two")
    watcher.checkpoint()

    assert watcher.horizon() == first.timestamp


def test_horizon_is_empty_when_nothing_was_recorded(tmp_path):
    root = _mindspace(tmp_path)
    watcher, _journal = _watcher(root)
    watcher.checkpoint()

    assert watcher.horizon() == ""


def test_a_reopened_watcher_reports_changes_made_while_it_was_closed(tmp_path):
    root = _mindspace(tmp_path)
    (root / "a.txt").write_bytes(b"one")
    watcher, journal = _watcher(root)
    watcher.checkpoint()
    watcher.close()
    journal.close()

    (root / "a.txt").write_bytes(b"changed while Humbug was not running")
    (root / "b.txt").write_bytes(b"also new")

    reopened, _reopened_journal = _watcher(root)
    checkpoint = reopened.checkpoint()

    assert checkpoint.difference.changed == {"a.txt"}
    assert checkpoint.difference.added == {"b.txt"}


def test_a_reopened_watcher_reports_nothing_when_the_tree_is_unchanged(tmp_path):
    root = _mindspace(tmp_path)
    (root / "a.txt").write_bytes(b"one")
    watcher, journal = _watcher(root)
    watcher.checkpoint()
    watcher.close()
    journal.close()

    reopened, _reopened_journal = _watcher(root)
    checkpoint = reopened.checkpoint()

    assert checkpoint.difference.is_empty()


def test_does_not_descend_a_pruned_directory(tmp_path):
    root = _mindspace(tmp_path)
    watcher, _journal = _watcher(root)
    watcher.checkpoint()
    (root / ".humbug" / "a.txt").write_bytes(b"internal state")

    checkpoint = watcher.checkpoint()

    assert checkpoint.difference.is_empty()


def test_a_malformed_baseline_is_treated_as_absent(tmp_path):
    root = _mindspace(tmp_path)
    watch_dir = root / ".humbug" / "watch"
    watch_dir.mkdir(parents=True)
    (watch_dir / "baseline.json").write_text("not json", encoding="utf-8")
    (root / "a.txt").write_bytes(b"one")

    watcher, _journal = _watcher(root)
    checkpoint = watcher.checkpoint()

    assert checkpoint.difference.is_empty()


def test_a_baseline_missing_entries_is_treated_as_absent(tmp_path):
    root = _mindspace(tmp_path)
    watch_dir = root / ".humbug" / "watch"
    watch_dir.mkdir(parents=True)
    (watch_dir / "baseline.json").write_text(json.dumps({"sequence": 3}), encoding="utf-8")
    (root / "a.txt").write_bytes(b"one")

    watcher, _journal = _watcher(root)
    checkpoint = watcher.checkpoint()

    assert checkpoint.difference.is_empty()


def test_a_torn_record_does_not_lose_the_records_before_it(tmp_path):
    root = _mindspace(tmp_path)
    watcher, _journal = _watcher(root)
    watcher.checkpoint()
    (root / "a.txt").write_bytes(b"one")
    watcher.checkpoint()

    changes_path = root / ".humbug" / "watch" / "changes.jsonl"
    with open(changes_path, "a", encoding="utf-8") as f:
        f.write("{ this line was torn by a crash\n")

    recorded = watcher.recorded_changes(10)

    assert len(recorded) == 1
    assert recorded[0].difference.added == {"a.txt"}


def test_checkpoint_round_trips_through_a_dict():
    checkpoint = ChangeCheckpoint.from_dict({
        "sequence": 2,
        "timestamp": "2026-11-05T10:00:00+00:00",
        "difference": {"added": ["a.txt"], "removed": [], "changed": []},
        "attributed": ["a.txt"],
        "unwatched": False,
    })

    assert checkpoint.sequence == 2
    assert checkpoint.difference.added == {"a.txt"}
    assert checkpoint.attributed == {"a.txt"}
    assert ChangeCheckpoint.from_dict(checkpoint.to_dict()) == checkpoint


def test_checkpoint_from_dict_rejects_a_missing_field():
    try:
        ChangeCheckpoint.from_dict({"sequence": 1})

    except ValueError:
        return

    raise AssertionError("a checkpoint without a timestamp must be rejected")


def test_interval_grows_when_a_walk_takes_longer_than_the_budget(tmp_path):
    root = _mindspace(tmp_path)
    watcher, _journal = _watcher(root, interval=1.0)

    slow = TreeSnapshot(entry_count=1, duration_ms=900.0)
    assert watcher.interval_for(slow) > 1.0


def test_interval_keeps_the_configured_value_for_a_fast_walk(tmp_path):
    root = _mindspace(tmp_path)
    watcher, _journal = _watcher(root, interval=5.0)

    fast = TreeSnapshot(entry_count=1, duration_ms=7.0)
    assert watcher.interval_for(fast) == 5.0


def test_records_a_detected_change_in_the_journal(tmp_path):
    root = _mindspace(tmp_path)
    (root / "a.txt").write_bytes(b"one")
    watcher, journal = _watcher(root)
    watcher.checkpoint()
    (root / "a.txt").write_bytes(b"two")

    watcher.checkpoint()

    entries = journal.entries_for_path("a.txt")
    assert len(entries) == 1
    assert entries[0].actor == JournalActor.UNATTRIBUTED


def test_records_a_creation_as_unrevertable_by_removal(tmp_path):
    root = _mindspace(tmp_path)
    watcher, journal = _watcher(root)
    watcher.checkpoint()
    (root / "new.txt").write_bytes(b"content")

    watcher.checkpoint()

    entries = journal.entries_for_path("new.txt")
    assert len(entries) == 1
    assert entries[0].existed_before is False
    assert entries[0].post_image_hash is not None


def test_a_detected_change_carries_a_usable_pre_image(tmp_path):
    root = _mindspace(tmp_path)
    (root / "a.txt").write_bytes(b"before")
    watcher, journal = _watcher(root)
    watcher.checkpoint()
    (root / "a.txt").write_bytes(b"after")

    watcher.checkpoint()

    entry = journal.entries_for_path("a.txt")[0]
    assert journal.pre_image(entry) == b"before"


def test_a_detected_change_can_be_reverted(tmp_path):
    root = _mindspace(tmp_path)
    (root / "a.txt").write_bytes(b"before")
    watcher, journal = _watcher(root)
    watcher.checkpoint()
    (root / "a.txt").write_bytes(b"after")
    watcher.checkpoint()

    result = journal_reversal_for(journal, root).revert(journal.entries_for_path("a.txt"))

    assert result.reverted_paths() == ["a.txt"]
    assert (root / "a.txt").read_bytes() == b"before"


def test_a_detected_change_that_is_not_reverted_survives_a_sweep(tmp_path):
    root = _mindspace(tmp_path)
    (root / "a.txt").write_bytes(b"before")
    watcher, journal = _watcher(root)
    watcher.checkpoint()
    (root / "a.txt").write_bytes(b"after")
    watcher.checkpoint()

    journal.purge_through(0)

    entry = journal.entries_for_path("a.txt")[0]
    assert journal.pre_image(entry) == b"before"


def test_a_change_the_journal_recorded_is_not_recorded_again(tmp_path):
    root = _mindspace(tmp_path)
    (root / "a.txt").write_bytes(b"one")
    watcher, journal = _watcher(root)
    watcher.checkpoint()

    journal.record_write(str(root / "a.txt"), "a.txt", b"two", JournalActor.AI, "conv")
    (root / "a.txt").write_bytes(b"two")

    watcher.checkpoint()

    assert len(journal.entries_for_path("a.txt")) == 1
    assert journal.entries_for_path("a.txt")[0].actor == JournalActor.AI


def test_records_a_detected_removal_with_the_content_it_replaced(tmp_path):
    root = _mindspace(tmp_path)
    (root / "gone.txt").write_bytes(b"content")
    watcher, journal = _watcher(root)
    watcher.checkpoint()
    os.remove(root / "gone.txt")

    watcher.checkpoint()

    entries = journal.entries_for_path("gone.txt")
    assert len(entries) == 1
    assert entries[0].post_image_hash is None
    assert journal.pre_image(entries[0]) == b"content"


def test_a_slow_walk_lengthens_the_interval_for_later_checkpoints(tmp_path):
    root = _mindspace(tmp_path)
    watcher, _journal = _watcher(root, interval=1.0)

    watcher.note_walk_cost(TreeSnapshot(entry_count=1, duration_ms=900.0))

    assert watcher.interval_seconds() > 1.0


def test_a_fast_walk_leaves_the_configured_interval_alone(tmp_path):
    root = _mindspace(tmp_path)
    watcher, _journal = _watcher(root, interval=5.0)

    watcher.note_walk_cost(TreeSnapshot(entry_count=1, duration_ms=7.0))

    assert watcher.interval_seconds() == 5.0


def test_a_purge_stops_the_baseline_claiming_content_it_reclaimed(tmp_path):
    root = _mindspace(tmp_path)
    (root / "a.txt").write_bytes(b"before")
    watcher, journal = _watcher(root)
    watcher.checkpoint()
    (root / "a.txt").write_bytes(b"after")
    watcher.checkpoint()
    retained_before = journal.stored_digests()

    journal.purge_all()
    watcher.redeclare_retained()

    assert journal.stored_digests() != retained_before


def test_a_purge_leaves_the_baseline_claiming_only_content_that_survived(tmp_path):
    root = _mindspace(tmp_path)
    (root / "kept.txt").write_bytes(b"kept")
    watcher, journal = _watcher(root)
    watcher.checkpoint()

    journal.purge_through(0)
    watcher.redeclare_retained()

    assert set(watcher._baseline_digests.values()) == journal.stored_digests()  # pylint: disable=protected-access


def test_a_write_recorded_before_it_happens_is_not_recorded_again(tmp_path):
    root = _mindspace(tmp_path)
    (root / "fred.txt").write_bytes(b"one")
    watcher, journal = _watcher(root)
    watcher.checkpoint()

    journal.record_write(str(root / "fred.txt"), "fred.txt", b"two", JournalActor.AI, "conv")
    watcher.checkpoint()
    (root / "fred.txt").write_bytes(b"two")
    watcher.checkpoint()
    watcher.checkpoint()

    entries = journal.entries_for_path("fred.txt")
    assert len(entries) == 1
    assert entries[0].actor == JournalActor.AI


def test_a_write_recorded_after_it_happens_is_not_recorded_again(tmp_path):
    root = _mindspace(tmp_path)
    (root / "fred.txt").write_bytes(b"one")
    watcher, journal = _watcher(root)
    watcher.checkpoint()

    journal.record_write(str(root / "fred.txt"), "fred.txt", b"two", JournalActor.AI, "conv")
    (root / "fred.txt").write_bytes(b"two")
    watcher.checkpoint()
    watcher.checkpoint()

    entries = journal.entries_for_path("fred.txt")
    assert len(entries) == 1
    assert entries[0].actor == JournalActor.AI


def test_a_change_the_journal_never_saw_is_still_recorded(tmp_path):
    root = _mindspace(tmp_path)
    (root / "fred.txt").write_bytes(b"one")
    watcher, journal = _watcher(root)
    watcher.checkpoint()

    journal.record_write(str(root / "a.txt"), "a.txt", b"x", JournalActor.AI, "conv")
    (root / "fred.txt").write_bytes(b"changed by something else")
    watcher.checkpoint()

    assert journal.entries_for_path("fred.txt")[0].actor == JournalActor.UNATTRIBUTED


def test_a_baseline_spans_a_restart_taken_with_the_same_scanned_paths(tmp_path):
    root = _mindspace(tmp_path)
    (root / "fred.txt").write_bytes(b"one")
    watcher, _journal = _watcher(root)
    watcher.checkpoint()
    (root / "fred.txt").write_bytes(b"changed while Humbug was closed")

    reopened, _reopened_journal = _watcher(root)

    assert reopened.checkpoint().difference.changed == {"fred.txt"}


def test_a_restart_after_a_scanned_path_was_pruned_reports_nothing(tmp_path):
    root = _mindspace(tmp_path)
    (root / "generated").mkdir()
    (root / "generated" / "report.html").write_bytes(b"one")
    (root / "keep.txt").write_bytes(b"two")
    watcher, _journal = _watcher(root)
    watcher.checkpoint()

    pruned, pruned_journal = _watcher(root, pruned=frozenset({".humbug", "generated"}))
    checkpoint = pruned.checkpoint()

    assert checkpoint.difference.is_empty()
    assert checkpoint.unrecorded() == set()
    assert pruned_journal.all_entries() == []


def test_reporting_resumes_after_a_scanned_path_change_re_establishes_the_baseline(tmp_path):
    root = _mindspace(tmp_path)
    (root / "generated").mkdir()
    (root / "generated" / "report.html").write_bytes(b"one")
    (root / "keep.txt").write_bytes(b"two")
    watcher, _journal = _watcher(root)
    watcher.checkpoint()

    pruned, _pruned_journal = _watcher(root, pruned=frozenset({".humbug", "generated"}))
    pruned.checkpoint()
    (root / "keep.txt").write_bytes(b"changed afterwards")

    assert pruned.checkpoint().unrecorded() == {"keep.txt"}


def test_a_baseline_written_before_the_scanned_paths_were_recorded_is_kept(tmp_path):
    root = _mindspace(tmp_path)
    (root / "fred.txt").write_bytes(b"one")
    watcher, _journal = _watcher(root)
    watcher.checkpoint()

    baseline_path = root / ".humbug" / "watch" / "baseline.json"
    baseline = json.loads(baseline_path.read_text(encoding="utf-8"))
    del baseline["pruned"]
    baseline_path.write_text(json.dumps(baseline), encoding="utf-8")
    (root / "fred.txt").write_bytes(b"changed while Humbug was closed")

    reopened, _reopened_journal = _watcher(root)

    assert reopened.checkpoint().difference.changed == {"fred.txt"}


def test_a_swap_file_appearing_and_going_is_not_reported(tmp_path):
    root = _mindspace(tmp_path)
    (root / "fred.txt").write_bytes(b"one")
    watcher, journal = _watcher(root, pruned=frozenset({".humbug", "*.swp"}))
    watcher.checkpoint()

    (root / ".fred.txt.swp").write_bytes(b"vim buffer")
    appeared = watcher.checkpoint()
    (root / ".fred.txt.swp").unlink()
    went = watcher.checkpoint()

    assert appeared.difference.is_empty()
    assert went.difference.is_empty()
    assert journal.all_entries() == []
