"""Tests for reopening closed tabs through the TabManager."""
# pylint: disable=protected-access

import os

import pytest

# pylint: disable=wrong-import-position
from context.context_registry import ContextRegistry
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

    def test_reopen_with_empty_history_does_nothing(self, tab_manager, manager, tmp_path):
        """Reopening with nothing in the history leaves the workspace untouched."""
        _registry(tab_manager, manager, tmp_path)

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
        assert registry.peek_last_closed_context().path == first_path

    def test_reopen_works_when_no_tabs_are_open(self, tab_manager, manager, tmp_path):
        """The last tab closed is the one most likely to be reopened."""
        registry = _registry(tab_manager, manager, tmp_path)
        transcript = str(tmp_path / "conv.json")
        cid = registry.open(context_type="conversation", path=transcript, title="conv")
        tab_manager.close_tab_by_id(cid)
        assert not tab_manager._tabs

        tab_manager.reopen_last_closed_tab()

        assert [tab.path() for tab in tab_manager._tabs.values()] == [transcript]

    def test_a_close_that_is_cancelled_is_not_recorded(self, tab_manager, manager, tmp_path):
        """A tab that refuses to close stays open and is not offered for reopening."""
        registry = _registry(tab_manager, manager, tmp_path)
        cid = registry.open(context_type="conversation", path=str(tmp_path / "conv.json"), title="conv")
        tab = tab_manager._tabs[cid]
        tab.can_close_tab = lambda: False

        tab_manager.close_tab_by_id(cid)

        assert cid in tab_manager._tabs
        assert not registry.has_closed_contexts()


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

    def test_content_state_is_visible_before_the_tab_is_created(
        self, tab_manager, manager, tmp_path, commands_seen
    ):
        """The content state is readable while the tab factory is still running."""
        registry = _registry(tab_manager, manager, tmp_path)
        cid = registry.open(
            context_type="terminal", path=str(tmp_path / "term.json"), title="Terminal"
        )
        registry.register_model(cid, ModelWithCommand("npm run dev"))

        tab_manager.close_tab_by_id(cid)
        tab_manager.reopen_last_closed_tab()

        assert commands_seen == [None, "npm run dev"]


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

        assert not registry.has_closed_contexts()

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

    def test_reopening_a_singleton_type_already_open_focuses_it(self, tab_manager, manager, tmp_path):
        """A context type with no path is a singleton, so it is never duplicated."""
        registry = _registry(tab_manager, manager, tmp_path)
        # The tab widget needs a path to construct, but the context does not:
        # the duplicate check keys on the context type when the path is empty.
        transcript = str(tmp_path / "log.json")
        tab_manager.register_context_factory(
            "log", lambda info, _registry, parent: ConversationTab(info.context_id, transcript, parent)
        )
        cid = registry.open(context_type="log", title="Mindspace Log")

        tab_manager.close_tab_by_id(cid)
        reopened_by_other_means = registry.open(context_type="log", title="Mindspace Log")

        tab_manager.reopen_last_closed_tab()

        assert list(tab_manager._tabs) == [reopened_by_other_means]


class TestMindspaceChange:
    def test_the_history_does_not_outlive_the_mindspace(self, tab_manager, manager, tmp_path):
        """Closed tabs from one mindspace are not offered after leaving it."""
        registry = _registry(tab_manager, manager, tmp_path)
        cid = registry.open(
            context_type="conversation", path=str(tmp_path / "conv.json"), title="conv"
        )
        tab_manager.close_tab_by_id(cid)
        assert registry.has_closed_contexts()

        manager.close_mindspace()

        assert not tab_manager._tabs
        assert not manager.mindspace().contexts().has_closed_contexts()


class TestRegistryOwnsTheHistory:
    def test_a_context_closed_outside_the_frontend_is_reopenable(
        self, tab_manager, manager, tmp_path
    ):
        """A close that does not go through the tab manager is still recorded."""
        registry = _registry(tab_manager, manager, tmp_path)
        path = str(tmp_path / "conv.json")
        cid = registry.open(context_type="conversation", path=path, title="conv")

        registry.close(cid)

        assert not tab_manager._tabs
        assert registry.has_closed_contexts()
        assert isinstance(registry, ContextRegistry)
