"""Tests for the Reopen Closed Tab history tracked by TabManager."""
# pylint: disable=protected-access

import os

import pytest

# pylint: disable=wrong-import-position
from desktop.conversation_tab.conversation_tab import ConversationTab
from desktop.mindspace.mindspace_manager import MindspaceManager
from desktop.tab_manager.tab_manager import TabManager
from mindspace.mindspace_settings import MindspaceSettings


class ModelWithCommand:
    """Stand-in for a context model that serialises a terminal-style command."""

    def __init__(self, command: str) -> None:
        self._command = command

    def save_content_state(self) -> dict[str, str]:
        """Return the content state a frontend needs to recreate this context."""
        return {"command": self._command}


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


@pytest.fixture
def commands_seen(tab_manager):
    """
    Register a terminal-style factory and collect the command each tab is given.

    This mirrors the real terminal factory, which reads its command from the
    registry's content state when the tab is created.
    """
    seen: list[str | None] = []

    def _create_terminal_tab(info, registry, parent):
        seen.append(registry.get_content_state(info.context_id).get("command"))
        return ConversationTab(info.context_id, info.path, parent)

    tab_manager.register_context_factory("terminal", _create_terminal_tab)
    return seen


def _open_mindspace(manager, tmp_path) -> None:
    """Open a mindspace at a temp path so the registry becomes available."""
    path = os.path.join(str(tmp_path), "ms")
    os.makedirs(os.path.join(path, ".humbug"))
    manager._mindspace._path = path  # pylint: disable=protected-access
    manager._mindspace._settings = MindspaceSettings(enabled_tools={})  # pylint: disable=protected-access


def _registry(tab_manager, manager, tmp_path):
    """Open a mindspace, subscribe the tab manager, and return the live registry."""
    _open_mindspace(manager, tmp_path)
    tab_manager._subscribe_to_registry()
    return manager.mindspace().contexts()


class TestReopenClosedTab:
    def test_reopen_recreates_the_closed_tab(self, tab_manager, manager, tmp_path):
        """Reopening a closed tab creates a new tab for the same path."""
        registry = _registry(tab_manager, manager, tmp_path)
        transcript = str(tmp_path / "conv.json")
        cid = registry.open(context_type="conversation", path=transcript, title="conv")

        tab_manager.close_tab_by_id(cid)
        assert cid not in tab_manager._tabs

        tab_manager.reopen_last_closed_tab()

        reopened = [tab for tab in tab_manager._tabs.values() if tab.path() == transcript]
        assert len(reopened) == 1
        assert reopened[0].tab_id() != cid

    def test_reopen_with_empty_history_does_nothing(self, tab_manager):
        """Reopening with nothing in the history leaves the workspace untouched."""
        tab_manager.reopen_last_closed_tab()
        assert not tab_manager._tabs

    def test_reopen_pops_most_recently_closed_first(self, tab_manager, manager, tmp_path):
        """The most recently closed tab is the first one reopened."""
        registry = _registry(tab_manager, manager, tmp_path)
        first_path = str(tmp_path / "first.json")
        second_path = str(tmp_path / "second.json")
        first_id = registry.open(context_type="conversation", path=first_path, title="first")
        second_id = registry.open(context_type="conversation", path=second_path, title="second")

        tab_manager.close_tab_by_id(first_id)
        tab_manager.close_tab_by_id(second_id)

        tab_manager.reopen_last_closed_tab()

        paths = {tab.path() for tab in tab_manager._tabs.values()}
        assert second_path in paths
        assert first_path not in paths
        assert len(tab_manager._closed_tab_history) == 1

    def test_closed_tab_history_is_capped(self, tab_manager, manager, tmp_path):
        """The history retains only the most recent closures, up to its limit."""
        registry = _registry(tab_manager, manager, tmp_path)

        for i in range(TabManager._MAX_CLOSED_TAB_HISTORY + 5):
            path = str(tmp_path / f"conv{i}.json")
            cid = registry.open(context_type="conversation", path=path, title=str(i))
            tab_manager.close_tab_by_id(cid)

        assert len(tab_manager._closed_tab_history) == TabManager._MAX_CLOSED_TAB_HISTORY


class TestContentState:
    def test_reopened_tab_is_given_its_captured_content_state(
        self, tab_manager, manager, tmp_path, commands_seen
    ):
        """A reopened tab receives the content state its closed tab held."""
        registry = _registry(tab_manager, manager, tmp_path)
        cid = registry.open(
            context_type="terminal", path=str(tmp_path / "term.json"), title="Terminal"
        )
        registry.register_model(cid, ModelWithCommand("npm run dev"))
        assert commands_seen == [None]

        tab_manager.close_tab_by_id(cid)
        tab_manager.reopen_last_closed_tab()

        assert commands_seen[-1] == "npm run dev"

    def test_content_state_is_captured_before_the_model_is_discarded(
        self, tab_manager, manager, tmp_path
    ):
        """The history snapshot holds content state taken while the tab was open."""
        registry = _registry(tab_manager, manager, tmp_path)
        cid = registry.open(
            context_type="conversation", path=str(tmp_path / "conv.json"), title="conv"
        )
        registry.register_model(cid, ModelWithCommand("npm run dev"))

        tab_manager.close_tab_by_id(cid)

        assert tab_manager._closed_tab_history[-1].content_state == {"command": "npm run dev"}
        assert registry.get_content_state(cid) == {}


class TestEphemeralTabs:
    def test_ephemeral_tabs_are_not_recorded(self, tab_manager, manager, tmp_path):
        """Preview tabs are not added to the history when they close."""
        registry = _registry(tab_manager, manager, tmp_path)
        cid = registry.open(
            context_type="conversation",
            path=str(tmp_path / "preview.json"),
            title="preview",
            is_ephemeral=True,
        )

        tab_manager.close_tab_by_id(cid)

        assert not tab_manager._closed_tab_history

    def test_a_reopened_tab_is_permanent(self, tab_manager, manager, tmp_path):
        """A reopened tab is permanent, so the next tab opened does not replace it."""
        registry = _registry(tab_manager, manager, tmp_path)
        path = str(tmp_path / "conv.json")
        cid = registry.open(context_type="conversation", path=path, title="conv")

        tab_manager.close_tab_by_id(cid)
        tab_manager.reopen_last_closed_tab()

        reopened = next(iter(tab_manager._tabs.values()))
        assert not reopened.is_ephemeral()


class TestAlreadyOpenTabs:
    def test_reopening_a_path_already_open_focuses_it(self, tab_manager, manager, tmp_path):
        """Reopening a path that is open again focuses that tab instead of duplicating it."""
        registry = _registry(tab_manager, manager, tmp_path)
        path = str(tmp_path / "conv.json")
        cid = registry.open(context_type="conversation", path=path, title="conv")

        tab_manager.close_tab_by_id(cid)
        reopened_by_other_means = registry.open(
            context_type="conversation", path=path, title="conv"
        )

        tab_manager.reopen_last_closed_tab()

        assert list(tab_manager._tabs) == [reopened_by_other_means]


class TestMindspaceChange:
    def test_history_is_cleared_when_the_mindspace_closes(self, tab_manager, manager, tmp_path):
        """Closed tabs from one mindspace are not offered after leaving it."""
        registry = _registry(tab_manager, manager, tmp_path)
        cid = registry.open(
            context_type="conversation", path=str(tmp_path / "conv.json"), title="conv"
        )
        tab_manager.close_tab_by_id(cid)
        assert tab_manager._closed_tab_history

        tab_manager._unsubscribe_from_registry()

        assert not tab_manager._closed_tab_history
