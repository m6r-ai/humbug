"""Tests for what clicking and dragging a change journal row does."""

import os

from PySide6.QtWidgets import QTreeWidgetItem

from mindspace.journal_entry import JournalActor

from desktop.journal_sidebar.journal_sidebar import JournalSidebar


def _write(path: str, text: str) -> None:
    """Put text in a file, so that a following record can capture or replace it."""
    with open(path, "w", encoding="utf-8") as handle:
        handle.write(text)


def _panel_for(sandbox) -> JournalSidebar:
    """Build a panel listing the changes recorded in a mindspace."""
    panel = JournalSidebar()
    panel.set_mindspace(sandbox.path)
    return panel


def _row_for(panel: JournalSidebar, sequence: int) -> QTreeWidgetItem:
    """Return the row that stands for a given change."""
    for index in range(panel._tree.topLevelItemCount()):  # pylint: disable=protected-access
        item = panel._tree.topLevelItem(index)  # pylint: disable=protected-access
        if item is None:
            continue

        entry = panel._entry_for_item(item)  # pylint: disable=protected-access
        if entry is not None and entry.sequence == sequence:
            return item

    raise AssertionError(f"no row for change {sequence}")


def _record_two_writes(sandbox) -> tuple[int, str]:
    """
    Record a change whose produced version the journal holds, and the one after it.

    The journal stores the content a change replaced, so a change's own version is
    held only once something later replaced it.

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


class TestClickingARow:
    """A click is a passing look at the change; a double click is a lasting one."""

    def test_a_click_opens_the_version_the_change_produced(self, mindspace_sandbox):
        sequence, text = _record_two_writes(mindspace_sandbox)
        panel = _panel_for(mindspace_sandbox)
        try:
            panel._on_item_clicked(_row_for(panel, sequence))  # pylint: disable=protected-access

            contexts = _contexts(mindspace_sandbox)
            opened = [info.context_id for info in contexts.list_all()]
            assert len(opened) == 1
            assert contexts.get_model(opened[0], str) == text
        finally:
            panel.deleteLater()

    def test_a_click_opens_a_tab_that_closes_beside_another(self, mindspace_sandbox):
        sequence, _text = _record_two_writes(mindspace_sandbox)
        panel = _panel_for(mindspace_sandbox)
        try:
            panel._on_item_clicked(_row_for(panel, sequence))  # pylint: disable=protected-access

            (info,) = _contexts(mindspace_sandbox).list_all()
            assert info.is_ephemeral
        finally:
            panel.deleteLater()

    def test_a_double_click_keeps_the_open_tab_and_closes_nothing(self, mindspace_sandbox):
        sequence, _text = _record_two_writes(mindspace_sandbox)
        panel = _panel_for(mindspace_sandbox)
        try:
            row = _row_for(panel, sequence)
            panel._on_item_clicked(row)  # pylint: disable=protected-access
            panel._on_item_double_clicked(row)  # pylint: disable=protected-access

            (info,) = _contexts(mindspace_sandbox).list_all()
            assert not info.is_ephemeral
        finally:
            panel.deleteLater()

    def test_clicking_a_group_heading_opens_nothing(self, mindspace_sandbox):
        _record_two_writes(mindspace_sandbox)
        panel = _panel_for(mindspace_sandbox)
        try:
            heading = panel._tree.topLevelItem(0)  # pylint: disable=protected-access
            assert heading is not None

            panel._on_item_clicked(heading)  # pylint: disable=protected-access

            assert _contexts(mindspace_sandbox).list_all() == []
        finally:
            panel.deleteLater()


class TestDraggingARow:
    """A dragged row carries the change it stands for, and nothing else."""

    def test_a_row_naming_a_change_drags_that_change(self, mindspace_sandbox):
        sequence, _text = _record_two_writes(mindspace_sandbox)
        panel = _panel_for(mindspace_sandbox)
        try:
            assert panel._drag_sequence_for(_row_for(panel, sequence)) == sequence  # pylint: disable=protected-access
        finally:
            panel.deleteLater()

    def test_a_row_for_a_removal_cannot_be_dragged(self, mindspace_sandbox):
        journal = mindspace_sandbox.manager.mindspace().file_journal()
        path = os.path.join(mindspace_sandbox.path, "gone.txt")
        _write(path, "content")
        entry = journal.record_removal(path, "gone.txt", JournalActor.AI, "")
        os.remove(path)
        assert entry is not None
        panel = _panel_for(mindspace_sandbox)
        try:
            assert panel._drag_sequence_for(_row_for(panel, entry.sequence)) is None  # pylint: disable=protected-access
        finally:
            panel.deleteLater()

    def test_a_group_heading_cannot_be_dragged(self, mindspace_sandbox):
        _record_two_writes(mindspace_sandbox)
        panel = _panel_for(mindspace_sandbox)
        try:
            heading = panel._tree.topLevelItem(0)  # pylint: disable=protected-access
            assert heading is not None

            assert panel._drag_sequence_for(heading) is None  # pylint: disable=protected-access
        finally:
            panel.deleteLater()
