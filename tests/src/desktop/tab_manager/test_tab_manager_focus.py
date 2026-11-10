"""Tests for which open and restore decide where focus goes."""

import os

import pytest

# pylint: disable=wrong-import-position
from PySide6.QtWidgets import QApplication, QLineEdit, QVBoxLayout

from desktop.tab.tab_base import TabBase
from desktop.tab_manager.tab_manager import TabManager


class StubTab(TabBase):
    """A tab that satisfies the manager's contract without building tab content.

    These tests are about where the manager puts focus, not about what a tab
    renders, so the tab is kept free of content widgets.
    """

    def tool_name(self) -> str:
        """Report the context type the manager registered this factory under."""
        return "conversation"

    def set_path(self, path: str) -> None:
        """Record the path the tab belongs to."""
        self._path = path

    def can_close_tab(self) -> bool:
        """Allow the manager to close the tab."""
        return True

    def close_tab(self) -> None:
        """Release nothing, because the tab owns nothing."""

    def update_status(self) -> None:
        """Report no status, because the tab has none."""

    def capture_view_state(self) -> dict:
        """Report no view state, because the tab has none."""
        return {}


@pytest.fixture
def tab_manager(qapp, mindspace_sandbox, host):
    """Create a subscribed TabManager whose tabs are stubs."""
    tab_manager = TabManager(lambda _payload: None, host)
    tab_manager.register_context_factory(
        "conversation",
        lambda info, _registry, parent: StubTab(info.context_id, parent),
    )
    tab_manager._subscribe_to_registry()  # pylint: disable=protected-access

    yield tab_manager

    tab_manager._unsubscribe_from_registry()  # pylint: disable=protected-access
    for tab_id in list(tab_manager._tabs):  # pylint: disable=protected-access
        tab_manager.close_tab_by_id(tab_id, force_close=True)

    qapp.processEvents()
    host.close()


@pytest.fixture
def elsewhere(qapp, host):
    """A widget outside the tab manager that holds focus, standing in for a sidebar."""
    layout = host.layout() or QVBoxLayout(host)
    field = QLineEdit()
    layout.addWidget(field)
    host.show()
    qapp.processEvents()
    field.setFocus()
    qapp.processEvents()
    return field


def _open(sandbox, message: str, requester_id: str) -> str:
    """Open a context, optionally on another context's behalf."""
    return sandbox.manager.mindspace().contexts().open(
        context_type="conversation",
        path=os.path.join(sandbox.path, f"{message}.conv"),
        title=message,
        requester_id=requester_id,
    )


class TestFocusOnOpen:
    """Tests that an open decides focus once, and only when the user asked for it."""

    def test_open_for_another_context_leaves_focus_alone(
        self, mindspace_sandbox, tab_manager, elsewhere
    ) -> None:
        """A tab opened for another context does not take focus from the user."""
        _open(mindspace_sandbox, "delegated", "requesting-context")

        assert QApplication.focusWidget() is elsewhere

    def test_open_for_another_context_does_not_schedule_activation(
        self, qapp, mindspace_sandbox, tab_manager, elsewhere
    ) -> None:
        """A tab opened for another context is never activated behind the user's back."""
        _open(mindspace_sandbox, "delegated", "requesting-context")

        for _ in range(5):
            qapp.processEvents()

        assert QApplication.focusWidget() is elsewhere

    def test_open_for_another_context_does_not_claim_focus_in_the_registry(
        self, mindspace_sandbox, tab_manager, elsewhere
    ) -> None:
        """The registry's focused context is unchanged by an open for another context."""
        registry = mindspace_sandbox.manager.mindspace().contexts()

        _open(mindspace_sandbox, "delegated", "requesting-context")

        assert registry.current_context_id() is None

    def test_user_initiated_open_claims_focus(
        self, qapp, mindspace_sandbox, tab_manager, elsewhere
    ) -> None:
        """A tab the user opened becomes the focused context and takes focus."""
        registry = mindspace_sandbox.manager.mindspace().contexts()

        tab_id = _open(mindspace_sandbox, "mine", "")

        for _ in range(5):
            qapp.processEvents()

        assert registry.current_context_id() == tab_id
        assert QApplication.focusWidget() is tab_manager._tabs[tab_id]  # pylint: disable=protected-access


class TestFocusOnRestore:
    """Tests that a restore keeps the focus its saved state names."""

    def test_restore_keeps_the_focused_context_from_the_saved_state(
        self, qapp, mindspace_sandbox, tab_manager, elsewhere
    ) -> None:
        """Restoring focuses the context the session had focused, not the last rebuilt."""
        registry = mindspace_sandbox.manager.mindspace().contexts()
        focused_id = _open(mindspace_sandbox, "first", "")
        _open(mindspace_sandbox, "second", "")
        registry.focus(focused_id)
        state = registry.save_state(current_by_column=tab_manager.current_tab_by_column())

        tab_manager._restoring = True  # pylint: disable=protected-access
        try:
            registry.restore_state(state)

        finally:
            tab_manager._restoring = False  # pylint: disable=protected-access

        tab_manager._restore_current_tabs(registry.get_current_by_column())  # pylint: disable=protected-access
        qapp.processEvents()

        assert registry.current_context_id() == focused_id

    def test_restoring_a_visible_tab_does_not_decide_focus(
        self, qapp, mindspace_sandbox, tab_manager, elsewhere
    ) -> None:
        """Making a restored tab visible again does not rewrite the focused context."""
        registry = mindspace_sandbox.manager.mindspace().contexts()
        focused_id = _open(mindspace_sandbox, "first", "")
        other_id = _open(mindspace_sandbox, "second", "")
        registry.focus(focused_id)

        tab_manager._restore_current_tabs({0: other_id})  # pylint: disable=protected-access
        qapp.processEvents()

        assert registry.current_context_id() == focused_id
