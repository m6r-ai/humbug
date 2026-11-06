"""Tests for opening the version of a file a change journal entry produced."""

import os

from mindspace.journal_entry import JournalActor

from desktop.journal_sidebar.recorded_version import open_recorded_version


def _write(path: str, text: str) -> None:
    """Put text in a file, so that a following record can capture or replace it."""
    with open(path, "w", encoding="utf-8") as handle:
        handle.write(text)


def _record_overwritten_write(sandbox) -> tuple[int, str]:
    """
    Record a change whose produced version the journal still holds.

    The journal stores the content a change replaced, so a change's own version is
    held only once something later replaced it.  Recording a second write is what
    puts the first write's version into the store.

    Returns:
        The sequence of the first change, and the text it produced.
    """
    journal = sandbox.manager.mindspace().file_journal()
    path = os.path.join(sandbox.path, "notes.txt")
    _write(path, "first")
    first = journal.record_write(path, "notes.txt", b"second", JournalActor.AI, "")
    _write(path, "second")
    journal.record_write(path, "notes.txt", b"third", JournalActor.AI, "")
    assert first is not None
    return first.sequence, "second"


def _contexts(sandbox):
    """Return the mindspace's context registry."""
    return sandbox.manager.mindspace().contexts()


class TestTheVersionShown:
    """A change is shown as the version it produced, not as the file it names."""

    def test_the_tab_shows_the_recorded_text(self, mindspace_sandbox):
        sequence, text = _record_overwritten_write(mindspace_sandbox)

        context_id = open_recorded_version(sequence, ephemeral=True)

        contexts = _contexts(mindspace_sandbox)
        assert contexts.get_model(context_id, str) == text

    def test_the_tab_is_a_read_only_view(self, mindspace_sandbox):
        sequence, _text = _record_overwritten_write(mindspace_sandbox)

        context_id = open_recorded_version(sequence, ephemeral=True)

        info = _contexts(mindspace_sandbox).get(context_id)
        assert info is not None
        assert info.read_only

    def test_the_tab_carries_the_path_of_the_file_the_change_affected(self, mindspace_sandbox):
        sequence, _text = _record_overwritten_write(mindspace_sandbox)

        context_id = open_recorded_version(sequence, ephemeral=True)

        info = _contexts(mindspace_sandbox).get(context_id)
        assert info is not None
        assert info.path == os.path.join(mindspace_sandbox.path, "notes.txt")


class TestEphemerality:
    """A passing look opens a tab that closes; a deliberate one opens a tab that stays."""

    def test_a_passing_look_opens_an_ephemeral_tab(self, mindspace_sandbox):
        sequence, _text = _record_overwritten_write(mindspace_sandbox)

        info = _contexts(mindspace_sandbox).get(open_recorded_version(sequence, ephemeral=True))

        assert info is not None
        assert info.is_ephemeral

    def test_a_deliberate_look_opens_a_tab_that_stays(self, mindspace_sandbox):
        sequence, _text = _record_overwritten_write(mindspace_sandbox)

        info = _contexts(mindspace_sandbox).get(open_recorded_version(sequence, ephemeral=False))

        assert info is not None
        assert not info.is_ephemeral

    def test_promoting_the_open_tab_leaves_one_tab_showing_the_version(self, mindspace_sandbox):
        sequence, _text = _record_overwritten_write(mindspace_sandbox)
        ephemeral_id = open_recorded_version(sequence, ephemeral=True)

        permanent_id = open_recorded_version(sequence, ephemeral=False)

        contexts = _contexts(mindspace_sandbox)
        assert permanent_id == ephemeral_id
        assert len(contexts.list_all()) == 1

    def test_promoting_the_open_tab_keeps_it_read_only(self, mindspace_sandbox):
        sequence, _text = _record_overwritten_write(mindspace_sandbox)
        open_recorded_version(sequence, ephemeral=True)

        permanent_id = open_recorded_version(sequence, ephemeral=False)

        info = _contexts(mindspace_sandbox).get(permanent_id)
        assert info is not None
        assert info.read_only
        assert not info.is_ephemeral


class TestNothingToShow:
    """A change that produced no version opens nothing at all."""

    def test_a_removal_opens_no_tab(self, mindspace_sandbox):
        journal = mindspace_sandbox.manager.mindspace().file_journal()
        path = os.path.join(mindspace_sandbox.path, "gone.txt")
        _write(path, "content")
        entry = journal.record_removal(path, "gone.txt", JournalActor.AI, "")
        os.remove(path)
        assert entry is not None

        assert open_recorded_version(entry.sequence, ephemeral=True) is None
        assert _contexts(mindspace_sandbox).list_all() == []

    def test_a_version_the_journal_no_longer_holds_opens_no_tab(self, mindspace_sandbox):
        journal = mindspace_sandbox.manager.mindspace().file_journal()
        path = os.path.join(mindspace_sandbox.path, "notes.txt")
        _write(path, "first")
        entry = journal.record_write(path, "notes.txt", b"second", JournalActor.AI, "")
        assert entry is not None

        assert open_recorded_version(entry.sequence, ephemeral=True) is None
        assert _contexts(mindspace_sandbox).list_all() == []

    def test_a_sequence_no_change_has_opens_no_tab(self, mindspace_sandbox):
        assert open_recorded_version(999, ephemeral=True) is None
        assert _contexts(mindspace_sandbox).list_all() == []
