"""Tests for tabs a feature asks to open in the background."""

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
    """Create a TabManager with a conversation context factory registered."""
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


class TestBackgroundOpen:
    """A tab a provider claims opens behind the current tab; other tabs open as before."""

    def test_claimed_tab_opens_behind_the_current_tab(self, tab_manager, manager, tmp_path) -> None:
        """The tab is added to the active column but the tab the user is looking at stays current."""
        _open_mindspace(manager, tmp_path)
        tab_manager._subscribe_to_registry()  # pylint: disable=protected-access
        tab_manager.add_background_open_provider(lambda info, _requester: info.title == "helper")
        registry = manager.mindspace().contexts()
        first = registry.open(context_type="conversation", path=str(tmp_path / "a.conv"), title="mine")

        helper = registry.open(context_type="conversation", path=str(tmp_path / "b.conv"), title="helper")

        column = tab_manager._active_column  # pylint: disable=protected-access
        assert helper in tab_manager._tabs  # pylint: disable=protected-access
        assert column.currentWidget() is tab_manager._tabs[first]  # pylint: disable=protected-access
        assert column.indexOf(tab_manager._tabs[helper]) >= 0  # pylint: disable=protected-access

    def test_unclaimed_tab_opens_as_before(self, tab_manager, manager, tmp_path) -> None:
        """Without a provider claiming it, a new tab becomes current as it always has."""
        _open_mindspace(manager, tmp_path)
        tab_manager._subscribe_to_registry()  # pylint: disable=protected-access
        tab_manager.add_background_open_provider(lambda _info, _requester: False)
        registry = manager.mindspace().contexts()
        registry.open(context_type="conversation", path=str(tmp_path / "a.conv"), title="mine")

        second = registry.open(context_type="conversation", path=str(tmp_path / "b.conv"), title="other")

        column = tab_manager._active_column  # pylint: disable=protected-access
        assert column.currentWidget() is tab_manager._tabs[second]  # pylint: disable=protected-access

    def test_failing_provider_does_not_stop_the_tab_opening(self, tab_manager, manager, tmp_path) -> None:
        """A provider that raises is ignored and the tab opens normally."""
        _open_mindspace(manager, tmp_path)
        tab_manager._subscribe_to_registry()  # pylint: disable=protected-access

        def broken(_info, _requester):
            raise RuntimeError("broken")

        tab_manager.add_background_open_provider(broken)
        registry = manager.mindspace().contexts()
        registry.open(context_type="conversation", path=str(tmp_path / "a.conv"), title="mine")
        second = registry.open(context_type="conversation", path=str(tmp_path / "b.conv"), title="other")

        assert tab_manager._active_column.currentWidget() is tab_manager._tabs[second]  # pylint: disable=protected-access
