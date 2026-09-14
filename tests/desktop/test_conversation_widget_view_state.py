"""Tests for the conversation widget's view-state codec."""

import os

import pytest

# pylint: disable=wrong-import-position
from ai import AIMessage, AIMessageSource
from ai_transcript_conversation import AITranscriptConversation
from desktop.conversation_tab.conversation_widget import ConversationWidget
from desktop.mindspace.mindspace_manager import MindspaceManager
from desktop.user.user_manager import UserManager


@pytest.fixture
def conv_env(qapp, tmp_path, monkeypatch):
    """Open a real mindspace in a sandboxed HOME and return (mindspace_manager, transcript_path)."""
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

    transcript_path = os.path.join(mgr.mindspace().conversations_dir(), "test.conv")

    yield mgr, transcript_path

    MindspaceManager._instance = None
    UserManager._instance = None


def _make_widget(transcript_path: str) -> ConversationWidget:
    """Create a ConversationWidget backed by a fresh transcript."""
    transcript = AITranscriptConversation(transcript_path)
    return ConversationWidget(transcript_path, ai_transcript_conversation=transcript)


class TestConversationViewState:
    """Tests for ConversationWidget.create_view_state and restore_view_state."""

    def test_view_state_includes_input_draft(self, conv_env) -> None:
        """The unsaved input draft is part of the view state."""
        _mgr, transcript_path = conv_env
        widget = _make_widget(transcript_path)
        widget.set_input_text("a half-written prompt")

        state = widget.create_view_state()

        assert state["content"] == "a half-written prompt"

    def test_restore_view_state_restores_input_draft(self, conv_env) -> None:
        """A saved input draft is reapplied to the input box."""
        _mgr, transcript_path = conv_env
        widget = _make_widget(transcript_path)

        widget.restore_view_state({"content": "restored draft"})

        assert widget._input.to_plain_text() == "restored draft"  # pylint: disable=protected-access

    def test_restore_view_state_applies_draft_during_pending_load(self, conv_env) -> None:
        """The draft is restored immediately even while a batch load is in progress."""
        _mgr, transcript_path = conv_env
        widget = _make_widget(transcript_path)

        # Simulate a batch load in progress by populating the load queue.
        widget._load_queue = [AIMessage.create(AIMessageSource.USER, "queued")]  # pylint: disable=protected-access

        widget.restore_view_state({"content": "visible now"})

        assert widget._input.to_plain_text() == "visible now"  # pylint: disable=protected-access

    def test_restore_view_state_defers_layout_dependent_state(self, conv_env) -> None:
        """Layout-dependent state is deferred until the batch load completes."""
        _mgr, transcript_path = conv_env
        widget = _make_widget(transcript_path)

        widget._load_queue = [AIMessage.create(AIMessageSource.USER, "queued")]  # pylint: disable=protected-access

        widget.restore_view_state({"content": "visible now", "message_expansion": [True]})

        assert widget._load_pending_state is not None  # pylint: disable=protected-access
        assert widget._load_pending_state["message_expansion"] == [True]  # pylint: disable=protected-access
