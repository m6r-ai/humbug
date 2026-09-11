"""Tests for the TrashSidebar panel."""

import json
import os

from PySide6.QtCore import QModelIndex
from PySide6.QtGui import QFont, QFontMetrics
from PySide6.QtWidgets import QStyleOptionViewItem
import pytest

# pylint: disable=wrong-import-position
from desktop.message_box import MessageBox, MessageBoxButton
from desktop.mindspace.mindspace_manager import MindspaceManager
from desktop.style_manager import StyleManager
from desktop.trash_sidebar.trash_sidebar import TrashSidebar, TrashTreeDelegate, _TRASH_NAME_ROLE
from desktop.user.user_manager import UserManager


def _write_conv_file(path: str) -> None:
    """Write a minimal root .conv file (no delegate parent)."""
    with open(path, 'w', encoding='utf-8') as f:
        json.dump({"metadata": {"version": "0.1", "parent": None}, "conversation": []}, f)


@pytest.fixture
def sidebar_env(qapp, tmp_path, monkeypatch):
    """Open a real mindspace in a sandboxed HOME and return (mindspace_manager, mindspace_path, conv_dir)."""
    home_dir = tmp_path / "home"
    home_dir.mkdir()
    monkeypatch.setenv("HOME", str(home_dir))

    MindspaceManager._instance = None  # pylint: disable=protected-access
    UserManager._instance = None  # pylint: disable=protected-access

    mgr = MindspaceManager()
    mgr._home_config = str(tmp_path / "mindspace.json")  # pylint: disable=protected-access
    ms_path = str(tmp_path / "mindspace")
    mgr.create_mindspace(ms_path, [])
    mgr.open_mindspace(ms_path)
    conv_dir = mgr.mindspace().conversations_dir()

    monkeypatch.setattr(MessageBox, "show_message", lambda *_a, **_k: MessageBoxButton.YES)

    yield mgr, ms_path, conv_dir

    MindspaceManager._instance = None  # pylint: disable=protected-access
    UserManager._instance = None  # pylint: disable=protected-access


@pytest.fixture
def panel(sidebar_env):
    """A TrashSidebar configured for the sandboxed mindspace."""
    _mgr, ms_path, _conv_dir = sidebar_env
    widget = TrashSidebar()
    widget.set_mindspace(ms_path)
    yield widget
    widget.deleteLater()


def _trash_a_file(mgr: MindspaceManager, conv_dir: str, name: str = "chat.conv") -> str:
    """Trash a fresh conversation file and return its original path."""
    chat = os.path.join(conv_dir, name)
    _write_conv_file(chat)
    trash_path = mgr.mindspace().new_trash_path(chat)
    os.rename(chat, trash_path)
    mgr.mindspace().record_trashed(chat, trash_path)
    return chat


class TestTrashSidebarListing:
    """The panel lists whatever Mindspace.list_trashed() reports."""

    def test_no_mindspace_shows_empty_state(self, qapp):  # pylint: disable=unused-argument
        widget = TrashSidebar()
        widget.set_mindspace("")

        assert widget._tree.topLevelItemCount() == 0  # pylint: disable=protected-access
        assert not widget._status_label.isHidden()  # pylint: disable=protected-access
        widget.deleteLater()

    def test_empty_trash_shows_empty_state(self, panel):
        assert panel._tree.topLevelItemCount() == 0  # pylint: disable=protected-access
        assert not panel._status_label.isHidden()  # pylint: disable=protected-access

    def test_trashed_item_is_listed(self, sidebar_env, panel):
        mgr, _ms_path, conv_dir = sidebar_env
        _trash_a_file(mgr, conv_dir)

        panel.refresh()

        assert panel._tree.topLevelItemCount() == 1  # pylint: disable=protected-access
        assert panel._status_label.isHidden()  # pylint: disable=protected-access


class TestTrashSidebarActions:
    """Restore / Delete Forever / Empty Trash act on the underlying mindspace."""

    def test_restore_moves_file_back_and_removes_row(self, sidebar_env, panel):
        mgr, _ms_path, conv_dir = sidebar_env
        chat = _trash_a_file(mgr, conv_dir)
        panel.refresh()
        trash_name = panel._tree.topLevelItem(0).data(0, _TRASH_NAME_ROLE)  # pylint: disable=protected-access

        panel._restore(trash_name)  # pylint: disable=protected-access

        assert os.path.exists(chat)
        assert not mgr.mindspace().list_trashed()
        assert panel._tree.topLevelItemCount() == 0  # pylint: disable=protected-access

    def test_delete_forever_removes_item_permanently(self, sidebar_env, panel):
        mgr, _ms_path, conv_dir = sidebar_env
        _trash_a_file(mgr, conv_dir)
        panel.refresh()
        trash_name = panel._tree.topLevelItem(0).data(0, _TRASH_NAME_ROLE)  # pylint: disable=protected-access

        panel._delete_forever(trash_name)  # pylint: disable=protected-access

        assert not mgr.mindspace().list_trashed()
        assert panel._tree.topLevelItemCount() == 0  # pylint: disable=protected-access

    def test_empty_trash_clears_everything(self, sidebar_env, panel):
        mgr, _ms_path, conv_dir = sidebar_env
        _trash_a_file(mgr, conv_dir, "chat1.conv")
        _trash_a_file(mgr, conv_dir, "chat2.conv")
        panel.refresh()

        panel._empty_trash()  # pylint: disable=protected-access

        assert not mgr.mindspace().list_trashed()
        assert panel._tree.topLevelItemCount() == 0  # pylint: disable=protected-access


class TestTrashSidebarClickToOpen:
    """Clicking a trashed conversation file emits file_clicked with the trash panel_id."""

    def test_click_on_conversation_file_emits_signal(self, sidebar_env, panel):
        mgr, _ms_path, conv_dir = sidebar_env
        _trash_a_file(mgr, conv_dir)
        panel.refresh()

        emitted: list[tuple[str, str, bool]] = []
        panel.file_clicked.connect(lambda pid, p, eph: emitted.append((pid, p, eph)))

        item = panel._tree.topLevelItem(0)  # pylint: disable=protected-access
        panel._on_item_clicked(item)  # pylint: disable=protected-access

        assert len(emitted) == 1
        assert emitted[0][0] == "trash"
        assert emitted[0][1].endswith(".conv")
        assert emitted[0][2] is True  # ephemeral on single click

    def test_click_on_non_conversation_file_does_not_emit(self, sidebar_env, panel):
        mgr, _ms_path, conv_dir = sidebar_env
        readme = os.path.join(conv_dir, "README.md")
        with open(readme, "w", encoding="utf-8") as f:
            f.write("# Project\n")
        trash_path = mgr.mindspace().new_trash_path(readme)
        os.rename(readme, trash_path)
        mgr.mindspace().record_trashed(readme, trash_path)
        panel.refresh()

        emitted: list[tuple[str, str, bool]] = []
        panel.file_clicked.connect(lambda pid, p, eph: emitted.append((pid, p, eph)))

        item = panel._tree.topLevelItem(0)  # pylint: disable=protected-access
        panel._on_item_clicked(item)  # pylint: disable=protected-access

        assert len(emitted) == 0

    def test_double_click_on_conversation_file_emits_signal(self, sidebar_env, panel):
        mgr, _ms_path, conv_dir = sidebar_env
        _trash_a_file(mgr, conv_dir)
        panel.refresh()

        emitted: list[tuple[str, str, bool]] = []
        panel.file_clicked.connect(lambda pid, p, eph: emitted.append((pid, p, eph)))

        item = panel._tree.topLevelItem(0)  # pylint: disable=protected-access
        panel._on_item_double_clicked(item)  # pylint: disable=protected-access

        assert len(emitted) == 1
        assert emitted[0][0] == "trash"
        assert emitted[0][2] is False  # non-ephemeral on double click


class TestTrashTreeDelegateSpacing:
    """The delegate provides consistent, zoom-scaled row height matching the conversation sidebar."""

    def test_size_hint_matches_conversation_sidebar_formula(self, qapp):  # pylint: disable=unused-argument
        style_manager = StyleManager()
        zoom = style_manager.zoom_factor()
        delegate = TrashTreeDelegate(None, style_manager)  # type: ignore[arg-type]

        option = QStyleOptionViewItem()
        font = QFont()
        option.fontMetrics = QFontMetrics(font)  # type: ignore[attr-defined]

        size = delegate.sizeHint(option, QModelIndex())

        fm_height = option.fontMetrics.height()
        expected_height = max(fm_height + round(10 * zoom), round(24 * zoom))
        assert size.height() == expected_height
