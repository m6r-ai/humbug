"""Tests for ephemerality syncing between tabs and the context registry."""

import os

import pytest

# pylint: disable=wrong-import-position
from desktop.conversation_tab.conversation_tab import ConversationTab
from desktop.mindspace.mindspace_manager import MindspaceManager
from desktop.tab_manager.tab_manager import TabManager
from mindspace.mindspace_settings import MindspaceSettings


@pytest.fixture
def manager(qapp, tmp_path):
    """Create a MindspaceManager with an isolated home config file."""
    MindspaceManager._instance = None

    mgr = MindspaceManager()
    mgr._home_config = str(tmp_path / "mindspace.json")

    yield mgr

    MindspaceManager._instance = None


@pytest.fixture
def tab_manager(qapp, manager):
    """Create a TabManager with a conversation context factory registered.

    The TabManager is constructed before a mindspace is open, matching how the
    application builds it.
    """
    tm = TabManager(lambda source_type, path: None, None)

    def _create_conversation_tab(info, _registry, parent):
        return ConversationTab(info.context_id, info.path, parent)

    tm.register_context_factory("conversation", _create_conversation_tab)
    return tm


def _open_mindspace(manager, tmp_path) -> None:
    """Open a mindspace at a temp path so the registry becomes available."""
    path = os.path.join(str(tmp_path), "ms")
    os.makedirs(os.path.join(path, ".humbug"))
    manager._mindspace._path = path  # pylint: disable=protected-access
    manager._mindspace._settings = MindspaceSettings(enabled_tools={})  # pylint: disable=protected-access


class TestEphemeralitySync:
    """Tests that the registry's ephemerality stays in step with the tabs."""

    def test_make_permanent_updates_registry(self, tab_manager, manager, tmp_path) -> None:
        """Making a tab permanent pushes is_ephemeral=False to the registry."""
        _open_mindspace(manager, tmp_path)
        tab_manager._subscribe_to_registry()  # pylint: disable=protected-access
        registry = manager.mindspace().contexts()
        transcript = str(tmp_path / "conv.json")
        cid = registry.open(context_type="conversation", path=transcript, is_ephemeral=True)

        tab = tab_manager._tabs[cid]  # pylint: disable=protected-access
        assert tab.is_ephemeral() is True
        assert registry.get(cid).is_ephemeral is True

        tab_manager._make_tab_permanent(tab, sync_registry=True)  # pylint: disable=protected-access

        assert tab.is_ephemeral() is False
        assert registry.get(cid).is_ephemeral is False

    def test_make_permanent_without_sync_leaves_registry(self, tab_manager, manager, tmp_path) -> None:
        """The registry is left untouched when the echo is suppressed."""
        _open_mindspace(manager, tmp_path)
        tab_manager._subscribe_to_registry()  # pylint: disable=protected-access
        registry = manager.mindspace().contexts()
        transcript = str(tmp_path / "conv.json")
        cid = registry.open(context_type="conversation", path=transcript, is_ephemeral=True)

        tab = tab_manager._tabs[cid]  # pylint: disable=protected-access
        tab_manager._make_tab_permanent(tab)  # pylint: disable=protected-access

        assert tab.is_ephemeral() is False
        assert registry.get(cid).is_ephemeral is True
