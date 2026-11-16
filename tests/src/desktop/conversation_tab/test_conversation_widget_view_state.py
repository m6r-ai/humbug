"""Tests for the conversation widget's view-state codec."""

import os

import pytest

# pylint: disable=wrong-import-position
from ai import AIMessage, AIMessageSource
from ai_transcript_conversation import AITranscriptConversation
from desktop.conversation_tab.conversation_widget import ConversationWidget


def _make_widget(transcript_path: str) -> ConversationWidget:
    """Create a ConversationWidget backed by a fresh transcript."""
    transcript = AITranscriptConversation(transcript_path)
    return ConversationWidget(transcript_path, ai_transcript_conversation=transcript)


def _make_two_turn_widget(transcript_path: str) -> ConversationWidget:
    """Create a ConversationWidget holding two prompts and a response to each."""
    history = AITranscriptConversation(transcript_path).read()
    messages = [
        AIMessage.create(AIMessageSource.USER, "first prompt"),
        AIMessage.create(AIMessageSource.AI, "first response"),
        AIMessage.create(AIMessageSource.USER, "second prompt"),
        AIMessage.create(AIMessageSource.AI, "second response"),
    ]

    for message in messages:
        history.add_message(message)

    transcript = AITranscriptConversation(transcript_path)
    transcript.set_conversation_history(history)

    return ConversationWidget(transcript_path, ai_transcript_conversation=transcript)


@pytest.fixture
def conv_env(mindspace_sandbox):
    """The sandboxed mindspace, as (mindspace_manager, transcript_path)."""
    transcript_path = os.path.join(mindspace_sandbox.conversations_dir, "test.conv")

    yield mindspace_sandbox.manager, transcript_path


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

    def test_restore_view_state_does_not_defer_the_input_draft(self, conv_env) -> None:
        """
        The draft is applied once, at restore, and not again when loading finishes.

        Reapplying it at the end of the load would move focus into the input box
        seconds after the tab was opened, taking it from wherever the user had moved.
        """
        _mgr, transcript_path = conv_env
        widget = _make_widget(transcript_path)

        widget._load_queue = [AIMessage.create(AIMessageSource.USER, "queued")]  # pylint: disable=protected-access

        widget.restore_view_state({"content": "visible now", "message_expansion": [True]})

        assert "content" not in widget._load_pending_state  # pylint: disable=protected-access
        assert widget._input.hasFocus() is False  # pylint: disable=protected-access

        widget._load_pending_state = None  # pylint: disable=protected-access
        widget._finalise_load()  # pylint: disable=protected-access

        assert widget._input.to_plain_text() == "visible now"  # pylint: disable=protected-access
        assert widget._input.hasFocus() is False  # pylint: disable=protected-access


class TestMessageExpansionState:
    """Tests that each message's expansion state survives a view-state round trip."""

    def test_expansion_state_is_captured_for_each_message(self, conv_env) -> None:
        """The captured state records the expansion of every message shown."""
        _mgr, transcript_path = conv_env
        widget = _make_two_turn_widget(transcript_path)

        message_id = widget._message_ids[1]  # pylint: disable=protected-access
        widget._change_message_expansion(message_id, False)  # pylint: disable=protected-access
        state = widget.create_view_state()

        assert state["message_expansion"] == [True, False, True, True]
        assert state["message_ids"] == widget._message_ids  # pylint: disable=protected-access

    def test_expansion_state_is_restored_to_the_same_messages(self, conv_env) -> None:
        """A collapsed message is still collapsed after the view is rebuilt."""
        _mgr, transcript_path = conv_env
        source = _make_two_turn_widget(transcript_path)
        source._change_message_expansion(source._message_ids[1], False)  # pylint: disable=protected-access
        state = source.create_view_state()

        restored = _make_widget(transcript_path)
        restored.restore_view_state(state)  # pylint: disable=protected-access

        widgets = restored._ordered_widgets()  # pylint: disable=protected-access

        assert [w.is_expanded() for w in widgets] == [True, False, True, True]
