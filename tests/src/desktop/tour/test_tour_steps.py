"""Tests for onboarding tour step target resolution."""
# pylint: disable=missing-class-docstring, missing-function-docstring, protected-access

import tempfile

from desktop.conversation_sidebar.conversation_sidebar import ConversationSidebar
from desktop.tour.tour_steps import _resolve_open_conversation


class _FakeSidebarManager:
    """Stand-in for SidebarManager exposing just what the resolver needs."""

    def __init__(self, panel):
        self._panel = panel
        self.rail_button_requests: list[str] = []

    def get_panel(self, panel_id):
        return self._panel if panel_id == "conversations" else None

    def panel_button(self, panel_id):
        self.rail_button_requests.append(panel_id)
        return "RAIL_BUTTON_FALLBACK"


class _FakeMainWindow:
    def __init__(self, sidebar_manager):
        self._sidebar_manager = sidebar_manager

    def sidebar_manager(self):
        return self._sidebar_manager


class TestResolveOpenConversation:
    def test_falls_back_to_rail_button_before_mindspace_is_set(self, qapp):  # pylint: disable=unused-argument
        panel = ConversationSidebar()
        sidebar_manager = _FakeSidebarManager(panel)
        main_window = _FakeMainWindow(sidebar_manager)

        target = _resolve_open_conversation(main_window)

        assert target == "RAIL_BUTTON_FALLBACK"
        assert sidebar_manager.rail_button_requests == ["conversations"]
        panel.deleteLater()

    def test_spotlights_header_once_mindspace_is_set(self, qapp):  # pylint: disable=unused-argument
        panel = ConversationSidebar()
        panel.show()
        panel.set_mindspace(tempfile.mkdtemp())
        sidebar_manager = _FakeSidebarManager(panel)
        main_window = _FakeMainWindow(sidebar_manager)

        target = _resolve_open_conversation(main_window)

        assert target is panel.header()
        assert sidebar_manager.rail_button_requests == []
        panel.deleteLater()

    def test_falls_back_to_rail_button_when_conversations_panel_is_missing(self, qapp):  # pylint: disable=unused-argument
        sidebar_manager = _FakeSidebarManager(None)
        main_window = _FakeMainWindow(sidebar_manager)

        target = _resolve_open_conversation(main_window)

        assert target == "RAIL_BUTTON_FALLBACK"
