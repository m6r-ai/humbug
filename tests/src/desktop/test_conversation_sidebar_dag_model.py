"""Tests for the synthetic Pinned section in ConversationSidebarDAGModel."""

import json
import os

import pytest
from PySide6.QtCore import Qt

# pylint: disable=wrong-import-position
from desktop.conversation_sidebar.conversation_sidebar_dag_model import ConversationSidebarDAGModel
from desktop.conversation_sidebar.conversation_sidebar_index import ConversationSidebarIndex
from desktop.mindspace.mindspace_manager import MindspaceManager
from desktop.sidebar.sidebar_tree_icon_provider import SidebarTreeIconProvider
from desktop.user.user_manager import UserManager


def _write_conv_file(path: str) -> None:
    """Write a minimal root .conv file (no delegate parent)."""
    with open(path, 'w', encoding='utf-8') as f:
        json.dump({"metadata": {"version": "0.1", "parent": None}, "conversation": []}, f)


@pytest.fixture
def sidebar_env(qapp, tmp_path, monkeypatch):
    """
    Open a real mindspace in a sandboxed HOME and return (mindspace_manager, conv_dir).

    UserManager (used by the DAG model for sort settings) reads/writes under
    the real home directory unless HOME is redirected first, so this must
    happen before anything constructs it.
    """
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

    yield mgr, conv_dir

    MindspaceManager._instance = None
    UserManager._instance = None


def _build_model(
    conv_dir: str,
    icon_provider: SidebarTreeIconProvider | None = None
) -> ConversationSidebarDAGModel:
    """Build a DAG model over the given conversations directory."""
    index = ConversationSidebarIndex()
    index.set_conversations_dir(conv_dir)
    provider = icon_provider or SidebarTreeIconProvider()
    return ConversationSidebarDAGModel(index, provider)


class TestPinnedSection:
    """Tests for the synthetic Pinned section built from pinned_root_paths()."""

    def test_no_pinned_section_when_nothing_pinned(self, sidebar_env):
        """No Pinned header exists when nothing is pinned."""
        _mgr, conv_dir = sidebar_env
        _write_conv_file(os.path.join(conv_dir, "chat.conv"))

        model = _build_model(conv_dir)

        assert not model.pinned_section_index().isValid()

    def test_pinned_conversation_appears_under_pinned_section(self, sidebar_env):
        """A pinned root conversation appears only under the Pinned header."""
        mgr, conv_dir = sidebar_env
        chat = os.path.join(conv_dir, "chat.conv")
        _write_conv_file(chat)
        mgr.set_path_pinned(chat, True)

        model = _build_model(conv_dir)

        pinned_index = model.pinned_section_index()
        assert pinned_index.isValid()
        assert model.data(pinned_index) == "Pinned"
        assert model.rowCount(pinned_index) == 1

        chat_index = model.index_for_path(chat)
        assert chat_index.isValid()
        assert model.parent(chat_index) == pinned_index

    def test_pinned_folder_is_removed_from_its_natural_location(self, sidebar_env):
        """Pinning a folder moves it out of its natural location."""
        mgr, conv_dir = sidebar_env
        folder = os.path.join(conv_dir, "folder")
        os.makedirs(folder)
        nested_chat = os.path.join(folder, "nested.conv")
        _write_conv_file(nested_chat)
        mgr.set_path_pinned(folder, True)

        model = _build_model(conv_dir)

        pinned_index = model.pinned_section_index()
        assert pinned_index.isValid()
        assert model.rowCount(pinned_index) == 1

        folder_index = model.index(0, 0, pinned_index)
        assert model.data(folder_index) == "folder"

        # The nested conversation comes along inside the pinned folder.
        assert model.rowCount(folder_index) == 1
        nested_index = model.index(0, 0, folder_index)
        assert model.data(nested_index) == "nested"

        root_names = [
            model.data(model.index(row, 0))
            for row in range(model.rowCount())
        ]
        assert root_names.count("folder") == 0

    def test_pinned_folder_is_not_duplicated_at_top_level(self, sidebar_env):
        """A pinned folder is shown only under Pinned."""
        mgr, conv_dir = sidebar_env
        pinned_folder = os.path.join(conv_dir, "pinned_folder")
        other_folder = os.path.join(conv_dir, "other_folder")
        os.makedirs(pinned_folder)
        os.makedirs(other_folder)
        mgr.set_path_pinned(pinned_folder, True)

        model = _build_model(conv_dir)

        root_names = [
            model.data(model.index(row, 0))
            for row in range(model.rowCount())
        ]
        assert root_names == ["Pinned", "other_folder"]

        pinned_index = model.pinned_index_for_path(pinned_folder)
        assert model.is_pinned_section_copy(pinned_index)
        assert not model.natural_index_for_path(pinned_folder).isValid()

    def test_unpinning_a_chat_in_a_pinned_folder_keeps_it_in_that_folder(self, sidebar_env):
        """An individually unpinned chat returns to the natural copy of its folder."""
        mgr, conv_dir = sidebar_env
        folder = os.path.join(conv_dir, "folder")
        os.makedirs(folder)
        chat = os.path.join(folder, "chat.conv")
        other_chat = os.path.join(folder, "other.conv")
        _write_conv_file(chat)
        _write_conv_file(other_chat)
        mgr.set_path_pinned(folder, True)
        mgr.set_path_pinned(chat, False)

        model = _build_model(conv_dir)

        folder_index = model.index_for_path(folder)
        names = {
            model.data(model.index(row, 0, folder_index))
            for row in range(model.rowCount(folder_index))
        }
        assert names == {"chat"}

    def test_unpinning_removes_the_section(self, sidebar_env):
        """Unpinning the only pinned item removes the Pinned header on rebuild."""
        mgr, conv_dir = sidebar_env
        chat = os.path.join(conv_dir, "chat.conv")
        _write_conv_file(chat)
        mgr.set_path_pinned(chat, True)
        mgr.set_path_pinned(chat, False)

        model = _build_model(conv_dir)

        assert not model.pinned_section_index().isValid()
        chat_index = model.index_for_path(chat)
        assert chat_index.isValid()
        assert not model.parent(chat_index).isValid()

    def test_pinned_section_is_not_selectable_or_editable(self, sidebar_env):
        """The Pinned header is a group label, not an interactive item."""
        mgr, conv_dir = sidebar_env
        chat = os.path.join(conv_dir, "chat.conv")
        _write_conv_file(chat)
        mgr.set_path_pinned(chat, True)

        model = _build_model(conv_dir)
        pinned_index = model.pinned_section_index()

        assert model.path_for_index(pinned_index) is None
        flags = model.flags(pinned_index)
        assert not flags & Qt.ItemFlag.ItemIsSelectable
        assert not flags & Qt.ItemFlag.ItemIsEditable
        assert flags & Qt.ItemFlag.ItemIsEnabled


class TestPinnedFolderGrouping:
    """Tests for the folder-context grouping of individually pinned conversations."""

    def test_sole_conversation_promotes_whole_folder(self, sidebar_env):
        """Pinning the only conversation in a folder shows the real folder under Pinned."""
        mgr, conv_dir = sidebar_env
        folder = os.path.join(conv_dir, "folder")
        os.makedirs(folder)
        chat = os.path.join(folder, "chat.conv")
        _write_conv_file(chat)
        mgr.set_path_pinned(chat, True)

        model = _build_model(conv_dir)

        pinned_index = model.pinned_section_index()
        assert model.rowCount(pinned_index) == 1
        folder_index = model.index(0, 0, pinned_index)
        assert model.data(folder_index) == "folder"
        assert model.path_for_index(folder_index) == folder

        # It's a real, interactive folder node — not a display-only group label.
        flags = model.flags(folder_index)
        assert flags & Qt.ItemFlag.ItemIsSelectable

    def test_one_of_several_conversations_creates_a_group_label(self, sidebar_env):
        """Pinning one of several conversations groups it under a folder label."""
        mgr, conv_dir = sidebar_env
        folder = os.path.join(conv_dir, "folder")
        os.makedirs(folder)
        pinned_chat = os.path.join(folder, "pinned.conv")
        other_chat = os.path.join(folder, "other.conv")
        _write_conv_file(pinned_chat)
        _write_conv_file(other_chat)
        mgr.set_path_pinned(pinned_chat, True)

        model = _build_model(conv_dir)

        pinned_index = model.pinned_section_index()
        assert model.rowCount(pinned_index) == 1
        group_index = model.index(0, 0, pinned_index)
        assert model.data(group_index) == "folder"

        # The group label is display-only, unlike a real folder node.
        assert model.path_for_index(group_index) is None
        flags = model.flags(group_index)
        assert not flags & Qt.ItemFlag.ItemIsSelectable

        # Only the pinned conversation is inside it.
        assert model.rowCount(group_index) == 1
        conv_index = model.index(0, 0, group_index)
        assert model.data(conv_index) == "pinned"

        # The real folder stays in its original (top-level) location with both conversations.
        real_folder_index = model.index_for_path(folder)
        assert real_folder_index.isValid()
        assert not model.parent(real_folder_index).isValid()
        assert model.rowCount(real_folder_index) == 1
        assert model.data(model.index(0, 0, real_folder_index)) == "other"

    def test_pinning_all_conversations_promotes_to_a_real_folder(self, sidebar_env):
        """Pinning every conversation in a folder promotes it to a real folder node."""
        mgr, conv_dir = sidebar_env
        folder = os.path.join(conv_dir, "folder")
        os.makedirs(folder)
        chat_a = os.path.join(folder, "a.conv")
        chat_b = os.path.join(folder, "b.conv")
        _write_conv_file(chat_a)
        _write_conv_file(chat_b)
        mgr.set_path_pinned(chat_a, True)
        mgr.set_path_pinned(chat_b, True)

        model = _build_model(conv_dir)

        pinned_index = model.pinned_section_index()
        assert model.rowCount(pinned_index) == 1
        folder_index = model.index(0, 0, pinned_index)
        assert model.rowCount(folder_index) == 2
        names = {model.data(model.index(row, 0, folder_index)) for row in range(2)}
        assert names == {"a", "b"}

        # It's the real, interactive folder — not a display-only group label.
        assert model.path_for_index(folder_index) == folder
        assert model.group_folder_path(folder_index) is None
        assert model.flags(folder_index) & Qt.ItemFlag.ItemIsSelectable

    def test_pinning_two_of_three_conversations_stays_a_group(self, sidebar_env):
        """Pinning some, but not all, conversations in a folder keeps it a group label."""
        mgr, conv_dir = sidebar_env
        folder = os.path.join(conv_dir, "folder")
        os.makedirs(folder)
        chat_a = os.path.join(folder, "a.conv")
        chat_b = os.path.join(folder, "b.conv")
        chat_c = os.path.join(folder, "c.conv")
        _write_conv_file(chat_a)
        _write_conv_file(chat_b)
        _write_conv_file(chat_c)
        mgr.set_path_pinned(chat_a, True)
        mgr.set_path_pinned(chat_b, True)

        model = _build_model(conv_dir)

        pinned_index = model.pinned_section_index()
        group_index = model.index(0, 0, pinned_index)
        assert model.group_folder_path(group_index) == folder
        assert model.rowCount(group_index) == 2
        names = {model.data(model.index(row, 0, group_index)) for row in range(2)}
        assert names == {"a", "b"}

        # The real folder stays in its normal location with just the unpinned
        # conversation — a and b are pinned roots in their own right, so they
        # (correctly) don't also appear here.
        real_folder_index = model.index_for_path(folder)
        assert model.rowCount(real_folder_index) == 1
        assert model.data(model.index(0, 0, real_folder_index)) == "c"

    def test_unpinning_the_only_grouped_conversation_removes_the_group(self, sidebar_env):
        """Unpinning the last pinned conversation from a folder drops its group."""
        mgr, conv_dir = sidebar_env
        folder = os.path.join(conv_dir, "folder")
        os.makedirs(folder)
        pinned_chat = os.path.join(folder, "pinned.conv")
        other_chat = os.path.join(folder, "other.conv")
        _write_conv_file(pinned_chat)
        _write_conv_file(other_chat)
        mgr.set_path_pinned(pinned_chat, True)
        mgr.set_path_pinned(pinned_chat, False)

        model = _build_model(conv_dir)

        assert not model.pinned_section_index().isValid()

    def test_pinned_group_paths_lists_the_real_folder_path(self, sidebar_env):
        """pinned_group_paths() reports the grouped folder's real path."""
        mgr, conv_dir = sidebar_env
        folder = os.path.join(conv_dir, "folder")
        os.makedirs(folder)
        pinned_chat = os.path.join(folder, "pinned.conv")
        other_chat = os.path.join(folder, "other.conv")
        _write_conv_file(pinned_chat)
        _write_conv_file(other_chat)
        mgr.set_path_pinned(pinned_chat, True)

        model = _build_model(conv_dir)

        assert model.pinned_group_paths() == [folder]

    def test_pinned_group_paths_empty_without_any_grouping(self, sidebar_env):
        """pinned_group_paths() is empty when nothing needs folder-context grouping."""
        mgr, conv_dir = sidebar_env
        chat = os.path.join(conv_dir, "chat.conv")
        _write_conv_file(chat)
        mgr.set_path_pinned(chat, True)

        model = _build_model(conv_dir)

        assert model.pinned_group_paths() == []

    def test_pinned_group_index_for_path_resolves_the_group_label(self, sidebar_env):
        """pinned_group_index_for_path() finds the group label, not the real folder."""
        mgr, conv_dir = sidebar_env
        folder = os.path.join(conv_dir, "folder")
        os.makedirs(folder)
        pinned_chat = os.path.join(folder, "pinned.conv")
        other_chat = os.path.join(folder, "other.conv")
        _write_conv_file(pinned_chat)
        _write_conv_file(other_chat)
        mgr.set_path_pinned(pinned_chat, True)

        model = _build_model(conv_dir)

        group_index = model.pinned_group_index_for_path(folder)
        real_folder_index = model.index_for_path(folder)

        assert group_index.isValid()
        assert real_folder_index.isValid()
        assert group_index != real_folder_index
        assert model.parent(group_index) == model.pinned_section_index()
        assert not model.parent(real_folder_index).isValid()

    def test_pinned_group_index_for_path_invalid_when_absent(self, sidebar_env):
        """pinned_group_index_for_path() returns an invalid index for an ungrouped folder."""
        _mgr, conv_dir = sidebar_env
        folder = os.path.join(conv_dir, "folder")
        os.makedirs(folder)

        model = _build_model(conv_dir)

        assert not model.pinned_group_index_for_path(folder).isValid()

    def test_group_folder_path_returns_real_path_for_a_group_label(self, sidebar_env):
        """group_folder_path() returns the real folder path for a group label's index."""
        mgr, conv_dir = sidebar_env
        folder = os.path.join(conv_dir, "folder")
        os.makedirs(folder)
        pinned_chat = os.path.join(folder, "pinned.conv")
        other_chat = os.path.join(folder, "other.conv")
        _write_conv_file(pinned_chat)
        _write_conv_file(other_chat)
        mgr.set_path_pinned(pinned_chat, True)

        model = _build_model(conv_dir)
        group_index = model.pinned_group_index_for_path(folder)

        assert model.group_folder_path(group_index) == folder

    def test_group_folder_path_none_for_non_group_nodes(self, sidebar_env):
        """group_folder_path() returns None for any node that isn't a group label."""
        mgr, conv_dir = sidebar_env
        chat = os.path.join(conv_dir, "chat.conv")
        _write_conv_file(chat)
        mgr.set_path_pinned(chat, True)

        model = _build_model(conv_dir)

        pinned_index = model.pinned_section_index()
        chat_index = model.index_for_path(chat)
        assert model.group_folder_path(pinned_index) is None
        assert model.group_folder_path(chat_index) is None


class TestNaturalCopyPinIndicator:
    """Tests for the pin marker shown on a folder's natural-location copy."""

    def test_no_marker_when_nothing_pinned(self, sidebar_env):
        """An untouched folder shows its normal icon, not the pin marker."""
        _mgr, conv_dir = sidebar_env
        folder = os.path.join(conv_dir, "folder")
        os.makedirs(folder)

        icon_provider = SidebarTreeIconProvider()
        model = _build_model(conv_dir, icon_provider)

        folder_index = model.index_for_path(folder)
        icon = model.data(folder_index, Qt.ItemDataRole.DecorationRole)
        assert icon.cacheKey() != icon_provider.pinned_section_icon().cacheKey()

    def test_marker_shown_for_a_partially_pinned_folder(self, sidebar_env):
        """The natural copy shows the pin marker even if only one chat inside is pinned."""
        mgr, conv_dir = sidebar_env
        folder = os.path.join(conv_dir, "folder")
        os.makedirs(folder)
        chat_a = os.path.join(folder, "a.conv")
        chat_b = os.path.join(folder, "b.conv")
        _write_conv_file(chat_a)
        _write_conv_file(chat_b)
        mgr.set_path_pinned(chat_a, True)

        icon_provider = SidebarTreeIconProvider()
        model = _build_model(conv_dir, icon_provider)

        folder_index = model.natural_index_for_path(folder)
        icon = model.data(folder_index, Qt.ItemDataRole.DecorationRole)
        assert icon.cacheKey() == icon_provider.pinned_section_icon().cacheKey()

    def test_marker_cleared_after_full_unpin(self, sidebar_env):
        """The marker disappears once nothing in the folder is pinned any more."""
        mgr, conv_dir = sidebar_env
        folder = os.path.join(conv_dir, "folder")
        os.makedirs(folder)
        chat = os.path.join(folder, "chat.conv")
        _write_conv_file(chat)
        mgr.set_path_pinned(chat, True)
        mgr.set_path_pinned(chat, False)

        icon_provider = SidebarTreeIconProvider()
        model = _build_model(conv_dir, icon_provider)

        folder_index = model.index_for_path(folder)
        icon = model.data(folder_index, Qt.ItemDataRole.DecorationRole)
        assert icon.cacheKey() != icon_provider.pinned_section_icon().cacheKey()

    def test_is_pinned_section_copy_identifies_a_fully_pinned_folder(self, sidebar_env):
        """is_pinned_section_copy() identifies a folder lifted into Pinned."""
        mgr, conv_dir = sidebar_env
        folder = os.path.join(conv_dir, "folder")
        os.makedirs(folder)
        mgr.set_path_pinned(folder, True)

        model = _build_model(conv_dir)

        natural_index = model.natural_index_for_path(folder)
        pinned_index = model.pinned_index_for_path(folder)
        assert not natural_index.isValid()
        assert pinned_index.isValid()
        assert model.is_pinned_section_copy(pinned_index) is True

    def test_natural_and_pinned_index_invalid_without_a_matching_copy(self, sidebar_env):
        """pinned_index_for_path() is invalid for a folder that was never pinned."""
        _mgr, conv_dir = sidebar_env
        folder = os.path.join(conv_dir, "folder")
        os.makedirs(folder)

        model = _build_model(conv_dir)

        assert model.natural_index_for_path(folder).isValid()
        assert not model.pinned_index_for_path(folder).isValid()
