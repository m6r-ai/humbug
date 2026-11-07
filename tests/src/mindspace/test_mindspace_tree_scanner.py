"""Tests for the mindspace tree scanner."""

import os

from mindspace.mindspace_tree_scanner import (
    MindspaceTreeScanner, TreeDifference, TreeEntry, TreeSnapshot
)


def _scanner(root, pruned=frozenset()):
    return MindspaceTreeScanner(str(root), pruned)


def test_a_path_at_the_root_keeps_its_first_character(tmp_path):
    (tmp_path / "fred.txt").write_bytes(b"content")

    snapshot = _scanner(tmp_path).scan()

    assert set(snapshot.entries) == {"fred.txt"}


def test_the_recorded_path_is_relative_to_the_mindspace_root(tmp_path):
    (tmp_path / "sub").mkdir()
    (tmp_path / "sub" / "fred.txt").write_bytes(b"content")

    snapshot = _scanner(tmp_path).scan()

    assert set(snapshot.entries) == {os.path.join("sub", "fred.txt")}


def test_records_metadata_for_each_file(tmp_path):
    (tmp_path / "a.txt").write_bytes(b"one")
    (tmp_path / "sub").mkdir()
    (tmp_path / "sub" / "b.txt").write_bytes(b"two")

    snapshot = _scanner(tmp_path).scan()

    assert set(snapshot.entries) == {"a.txt", os.path.join("sub", "b.txt")}
    assert isinstance(snapshot.entries["a.txt"], TreeEntry)
    assert snapshot.entries["a.txt"].size == 3


def test_records_directories_in_the_entry_count(tmp_path):
    (tmp_path / "a.txt").write_bytes(b"one")
    (tmp_path / "sub").mkdir()
    (tmp_path / "sub" / "b.txt").write_bytes(b"two")

    snapshot = _scanner(tmp_path).scan()

    assert snapshot.entry_count >= 3
    assert snapshot.duration_ms >= 0.0


def test_does_not_descend_a_pruned_directory(tmp_path):
    (tmp_path / "keep.txt").write_bytes(b"one")
    (tmp_path / "skip").mkdir()
    (tmp_path / "skip" / "hidden.txt").write_bytes(b"two")

    snapshot = _scanner(tmp_path, frozenset({"skip"})).scan()

    assert set(snapshot.entries) == {"keep.txt"}


def test_a_pruned_name_matches_a_directory_at_any_depth(tmp_path):
    (tmp_path / "keep.txt").write_bytes(b"one")
    (tmp_path / "sub").mkdir()
    (tmp_path / "sub" / "keep.txt").write_bytes(b"two")
    (tmp_path / "sub" / "build").mkdir()
    (tmp_path / "sub" / "build" / "object.o").write_bytes(b"three")

    snapshot = _scanner(tmp_path, frozenset({"build"})).scan()

    assert set(snapshot.entries) == {"keep.txt", os.path.join("sub", "keep.txt")}


def test_a_pruned_path_matches_only_that_directory(tmp_path):
    (tmp_path / "src").mkdir()
    (tmp_path / "src" / "keep.txt").write_bytes(b"one")
    (tmp_path / "src" / "generated").mkdir()
    (tmp_path / "src" / "generated" / "skip.txt").write_bytes(b"two")

    snapshot = _scanner(tmp_path, frozenset({"src/generated"})).scan()

    assert set(snapshot.entries) == {os.path.join("src", "keep.txt")}


def test_a_pruned_path_does_not_prune_a_same_named_directory_elsewhere(tmp_path):
    (tmp_path / "src").mkdir()
    (tmp_path / "src" / "generated").mkdir()
    (tmp_path / "src" / "generated" / "skip.txt").write_bytes(b"one")
    (tmp_path / "vendored").mkdir()
    (tmp_path / "vendored" / "generated").mkdir()
    (tmp_path / "vendored" / "generated" / "keep.txt").write_bytes(b"two")

    scanner = _scanner(tmp_path, frozenset({"src/generated"}))
    snapshot = scanner.scan()

    assert scanner.pruned_paths() == frozenset({"src/generated"})
    assert set(snapshot.entries) == {os.path.join("vendored", "generated", "keep.txt")}


def test_a_pruned_file_is_not_recorded(tmp_path):
    (tmp_path / "keep.txt").write_bytes(b"one")
    (tmp_path / "notes.txt").write_bytes(b"two")

    snapshot = _scanner(tmp_path, frozenset({"notes.txt"})).scan()

    assert set(snapshot.entries) == {"keep.txt"}


def test_a_wildcard_skips_matching_files_at_any_depth(tmp_path):
    (tmp_path / "keep.txt").write_bytes(b"one")
    (tmp_path / ".keep.txt.swp").write_bytes(b"two")
    (tmp_path / "sub").mkdir()
    (tmp_path / "sub" / ".other.txt.swp").write_bytes(b"three")
    (tmp_path / "sub" / "keep.txt").write_bytes(b"four")

    snapshot = _scanner(tmp_path, frozenset({"*.swp"})).scan()

    assert set(snapshot.entries) == {"keep.txt", os.path.join("sub", "keep.txt")}


def test_a_wildcard_skips_a_matching_directory(tmp_path):
    (tmp_path / "keep.txt").write_bytes(b"one")
    (tmp_path / "build").mkdir()
    (tmp_path / "build" / "object.o").write_bytes(b"two")
    (tmp_path / "build-tmp").mkdir()
    (tmp_path / "build-tmp" / "object.o").write_bytes(b"three")
    (tmp_path / "built").mkdir()
    (tmp_path / "built" / "keep.txt").write_bytes(b"four")

    snapshot = _scanner(tmp_path, frozenset({"build*"})).scan()

    assert set(snapshot.entries) == {"keep.txt", os.path.join("built", "keep.txt")}


def test_a_wildcard_matches_a_name_that_extends_its_stem(tmp_path):
    (tmp_path / "builds").mkdir()
    (tmp_path / "builds" / "object.o").write_bytes(b"one")
    (tmp_path / "keep.txt").write_bytes(b"two")

    snapshot = _scanner(tmp_path, frozenset({"build*"})).scan()

    assert set(snapshot.entries) == {"keep.txt"}


def test_does_not_follow_a_symlink_to_a_file(tmp_path):
    target = tmp_path / "target.txt"
    target.write_bytes(b"content")
    os.symlink(target, tmp_path / "link.txt")

    snapshot = _scanner(tmp_path).scan()

    assert set(snapshot.entries) == {"target.txt"}


def test_does_not_descend_a_directory_reached_through_a_symlink(tmp_path):
    outside = tmp_path / "outside"
    outside.mkdir()
    (outside / "inner.txt").write_bytes(b"content")
    root = tmp_path / "root"
    root.mkdir()
    (root / "keep.txt").write_bytes(b"one")
    os.symlink(outside, root / "linked")

    snapshot = _scanner(root).scan()

    assert set(snapshot.entries) == {"keep.txt"}


def test_entry_to_list_round_trips():
    entry = TreeEntry(1, 2, 3, 4, 5)

    assert TreeEntry.from_list(entry.to_list()) == entry


def test_entry_from_list_rejects_a_wrong_length():
    try:
        TreeEntry.from_list([1, 2, 3])

    except ValueError:
        return

    raise AssertionError("a short list must be rejected")


def test_difference_reports_added_removed_and_changed():
    before = {
        "kept.txt": TreeEntry(1, 1, 1, 1, 1),
        "gone.txt": TreeEntry(2, 2, 2, 2, 2),
        "edited.txt": TreeEntry(3, 3, 3, 3, 3),
    }
    after = {
        "kept.txt": TreeEntry(1, 1, 1, 1, 1),
        "edited.txt": TreeEntry(3, 4, 3, 3, 3),
        "new.txt": TreeEntry(4, 4, 4, 4, 4),
    }

    difference = TreeDifference.between(before, after)

    assert difference.added == {"new.txt"}
    assert difference.removed == {"gone.txt"}
    assert difference.changed == {"edited.txt"}
    assert difference.paths() == {"new.txt", "gone.txt", "edited.txt"}
    assert not difference.is_empty()


def test_difference_is_empty_when_nothing_moved():
    entries = {"a.txt": TreeEntry(1, 1, 1, 1, 1)}

    assert TreeDifference.between(entries, entries).is_empty()


def test_difference_round_trips_through_a_dict():
    difference = TreeDifference({"new.txt"}, {"gone.txt"}, {"edited.txt"})

    assert TreeDifference.from_dict(difference.to_dict()) == difference


def test_difference_from_dict_rejects_a_non_string_path():
    try:
        TreeDifference.from_dict({"added": [1], "removed": [], "changed": []})

    except ValueError:
        return

    raise AssertionError("a non-string path must be rejected")


def test_snapshot_carries_an_empty_entry_map_by_default():
    assert TreeSnapshot().entries == {}
