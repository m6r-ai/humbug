"""Tests for undo/redo of delete, move, and rename in ConversationSidebar."""

import json
import os

import pytest

# pylint: disable=wrong-import-position
from desktop.conversation_sidebar.conversation_sidebar import ConversationSidebar
from desktop.message_box import MessageBox, MessageBoxButton
from desktop.mindspace.mindspace_manager import MindspaceManager
from desktop.user.user_manager import UserManager
from mindspace.mindspace import Mindspace


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

    MindspaceManager._instance = None
    UserManager._instance = None

    mgr = MindspaceManager()
    mgr._home_config = str(tmp_path / "mindspace.json")  # pylint: disable=protected-access
    ms_path = str(tmp_path / "mindspace")
    mgr.create_mindspace(ms_path, [])
    mgr.open_mindspace(ms_path)
    conv_dir = mgr.mindspace().conversations_dir()

    # Auto-confirm any delete/move confirmation dialogs.
    monkeypatch.setattr(MessageBox, "show_message", lambda *_a, **_k: MessageBoxButton.YES)

    yield mgr, ms_path, conv_dir

    MindspaceManager._instance = None
    UserManager._instance = None


@pytest.fixture
def sidebar(sidebar_env):
    """A ConversationSidebar configured for the sandboxed mindspace."""
    _mgr, ms_path, _conv_dir = sidebar_env
    widget = ConversationSidebar()
    widget.set_mindspace(ms_path)
    yield widget
    widget.deleteLater()


def _trash_dir(ms_path: str) -> str:
    return os.path.join(ms_path, Mindspace.MINDSPACE_DIR, Mindspace.TRASH_DIR)


def _trashed_names(ms_path: str) -> list[str]:
    """List trash directory entries, excluding the manifest file itself."""
    return [name for name in os.listdir(_trash_dir(ms_path)) if name != "manifest.json"]


class TestDeleteUndoRedo:
    """Deleting moves items to trash rather than removing them, and is reversible."""

    def test_delete_file_moves_to_trash(self, sidebar_env, sidebar):
        _mgr, ms_path, conv_dir = sidebar_env
        chat = os.path.join(conv_dir, "chat.conv")
        _write_conv_file(chat)

        sidebar._handle_delete_file(chat)  # pylint: disable=protected-access

        assert not os.path.exists(chat)
        assert os.path.isdir(_trash_dir(ms_path))
        assert len(_trashed_names(ms_path)) == 1

    def test_undo_restores_deleted_file(self, sidebar_env, sidebar):
        _mgr, _ms_path, conv_dir = sidebar_env
        chat = os.path.join(conv_dir, "chat.conv")
        _write_conv_file(chat)

        sidebar._handle_delete_file(chat)  # pylint: disable=protected-access
        assert sidebar.can_undo()

        sidebar.undo()

        assert os.path.exists(chat)
        assert not sidebar.can_undo()
        assert sidebar.can_redo()

    def test_redo_deletes_again(self, sidebar_env, sidebar):
        _mgr, _ms_path, conv_dir = sidebar_env
        chat = os.path.join(conv_dir, "chat.conv")
        _write_conv_file(chat)

        sidebar._handle_delete_file(chat)  # pylint: disable=protected-access
        sidebar.undo()
        sidebar.redo()

        assert not os.path.exists(chat)
        assert sidebar.can_undo()
        assert not sidebar.can_redo()

    def test_delete_empty_folder_can_be_undone(self, sidebar_env, sidebar):
        _mgr, _ms_path, conv_dir = sidebar_env
        folder = os.path.join(conv_dir, "Empty")
        os.makedirs(folder)

        sidebar._handle_delete_folder(folder)  # pylint: disable=protected-access
        assert not os.path.exists(folder)

        sidebar.undo()

        assert os.path.isdir(folder)


class TestMoveUndoRedo:
    """Moving a conversation is reversible, and re-fires the pin-migration hook either way."""

    def test_move_then_undo_restores_original_location(self, sidebar_env, sidebar):
        _mgr, _ms_path, conv_dir = sidebar_env
        chat = os.path.join(conv_dir, "chat.conv")
        _write_conv_file(chat)
        dest_dir = os.path.join(conv_dir, "Folder")
        os.makedirs(dest_dir)
        dest = os.path.join(dest_dir, "chat.conv")

        sidebar._perform_move_operation(chat, dest, {chat})  # pylint: disable=protected-access

        assert os.path.exists(dest)
        assert not os.path.exists(chat)

        sidebar.undo()

        assert os.path.exists(chat)
        assert not os.path.exists(dest)

    def test_move_undo_redo_migrates_pin_state_both_ways(self, sidebar_env, sidebar):
        mgr, _ms_path, conv_dir = sidebar_env
        chat = os.path.join(conv_dir, "chat.conv")
        _write_conv_file(chat)
        mgr.set_path_pinned(chat, True)
        dest_dir = os.path.join(conv_dir, "Folder")
        os.makedirs(dest_dir)
        dest = os.path.join(dest_dir, "chat.conv")

        sidebar._perform_move_operation(chat, dest, {chat})  # pylint: disable=protected-access
        assert mgr.is_path_pinned(dest)
        assert not mgr.is_path_pinned(chat)

        sidebar.undo()
        assert mgr.is_path_pinned(chat)
        assert not mgr.is_path_pinned(dest)

        sidebar.redo()
        assert mgr.is_path_pinned(dest)
        assert not mgr.is_path_pinned(chat)


class TestRenameUndoRedo:
    """Renaming a conversation is reversible."""

    def test_rename_then_undo_restores_original_name(self, sidebar_env, sidebar):
        _mgr, _ms_path, conv_dir = sidebar_env
        chat = os.path.join(conv_dir, "chat.conv")
        _write_conv_file(chat)

        # Force a synchronous rescan so the newly written file is indexed.
        sidebar._conversations_index.set_conversations_dir(conv_dir)  # pylint: disable=protected-access

        index = sidebar._dag_model.index_for_path(chat)  # pylint: disable=protected-access
        assert index.isValid()

        sidebar._complete_rename_operation(index, "renamed")  # pylint: disable=protected-access

        new_path = os.path.join(conv_dir, "renamed.conv")
        assert os.path.exists(new_path)
        assert not os.path.exists(chat)

        sidebar.undo()

        assert os.path.exists(chat)
        assert not os.path.exists(new_path)

        sidebar.redo()

        assert os.path.exists(new_path)
        assert not os.path.exists(chat)


class TestTrashManifestSync:
    """The trash manifest tracks original locations through delete/undo/redo."""

    def test_manifest_records_original_location_on_delete(self, sidebar_env, sidebar):
        mgr, _ms_path, conv_dir = sidebar_env
        folder = os.path.join(conv_dir, "Work")
        os.makedirs(folder)
        chat = os.path.join(folder, "chat.conv")
        _write_conv_file(chat)

        sidebar._handle_delete_file(chat)  # pylint: disable=protected-access

        entries = mgr.mindspace().list_trashed()
        assert len(entries) == 1
        assert entries[0].original_path == os.path.join(mgr.mindspace().conversations_rel_path(), "Work", "chat.conv")
        assert not entries[0].is_dir

    def test_manifest_entry_removed_on_undo(self, sidebar_env, sidebar):
        mgr, _ms_path, conv_dir = sidebar_env
        chat = os.path.join(conv_dir, "chat.conv")
        _write_conv_file(chat)

        sidebar._handle_delete_file(chat)  # pylint: disable=protected-access
        sidebar.undo()

        assert not mgr.mindspace().list_trashed()

    def test_manifest_entry_restored_on_redo(self, sidebar_env, sidebar):
        mgr, _ms_path, conv_dir = sidebar_env
        chat = os.path.join(conv_dir, "chat.conv")
        _write_conv_file(chat)

        sidebar._handle_delete_file(chat)  # pylint: disable=protected-access
        sidebar.undo()
        sidebar.redo()

        entries = mgr.mindspace().list_trashed()
        assert len(entries) == 1
        assert entries[0].original_path == os.path.join(mgr.mindspace().conversations_rel_path(), "chat.conv")


class TestUndoHistoryScopedToMindspace:
    """Switching mindspaces abandons undo history rather than carrying it over."""

    def test_switching_mindspace_clears_undo_history(self, sidebar_env, sidebar):
        _mgr, _ms_path, conv_dir = sidebar_env
        chat = os.path.join(conv_dir, "chat.conv")
        _write_conv_file(chat)

        sidebar._handle_delete_file(chat)  # pylint: disable=protected-access
        assert sidebar.can_undo()

        sidebar.set_mindspace("")

        assert not sidebar.can_undo()
        assert not sidebar.can_redo()
