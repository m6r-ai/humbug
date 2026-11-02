"""Tests for how a loaded conversation is revealed."""

import os

import pytest

from PySide6.QtWidgets import QApplication

# pylint: disable=wrong-import-position
from ai import AIMessage, AIMessageSource
from ai_transcript_conversation import AITranscriptConversation
from desktop.conversation_tab import conversation_widget as conversation_widget_module
from desktop.conversation_tab.conversation_widget import ConversationWidget

MESSAGE_COUNT = 120


def _make_widget(transcript_path: str) -> ConversationWidget:
    """Create a ConversationWidget backed by a transcript holding many messages."""
    history = AITranscriptConversation(transcript_path).read()
    for index in range(MESSAGE_COUNT):
        source = AIMessageSource.USER if index % 2 == 0 else AIMessageSource.AI
        history.add_message(AIMessage.create(source, f"message {index}"))

    transcript = AITranscriptConversation(transcript_path)
    transcript.set_conversation_history(history)

    widget = ConversationWidget(transcript_path, ai_transcript_conversation=transcript)
    widget.resize(1000, 800)
    widget.show()

    return widget


def _visible_count(widget: ConversationWidget) -> int:
    """Return how many message widgets the conversation is currently showing."""
    return sum(1 for message in widget._ordered_widgets() if message.isVisible())  # pylint: disable=protected-access


def _pump_until_settled(widget: ConversationWidget, qapp: QApplication) -> list[int]:
    """
    Process events until the conversation has finished loading and revealing.

    Args:
        widget: The conversation widget to settle.
        qapp: The application delivering events.

    Returns:
        The number of visible messages after each event-loop turn.
    """
    counts: list[int] = []
    quiet = 0
    for _ in range(20000):
        qapp.processEvents()
        counts.append(_visible_count(widget))
        loaded = (not widget._load_queue and not widget._reveal_queue  # pylint: disable=protected-access
                  and not widget._reveal_complete_pending)  # pylint: disable=protected-access
        quiet = quiet + 1 if loaded else 0
        if quiet > 5:
            return counts

    raise AssertionError("conversation did not settle")


@pytest.fixture
def transcript_path(mindspace_sandbox) -> str:
    """A path in the sandbox for the conversation transcript."""
    return os.path.join(mindspace_sandbox.conversations_dir, "test.conv")


class TestReveal:
    def test_messages_are_revealed_over_several_turns(self, transcript_path, qapp, monkeypatch) -> None:
        """A loaded history becomes visible gradually rather than in a single turn."""
        monkeypatch.setattr(conversation_widget_module, "_REVEAL_SLICE_SECONDS", 0.001)
        monkeypatch.setattr(conversation_widget_module, "_REVEAL_INTERVAL_SECONDS", 0.0)
        widget = _make_widget(transcript_path)

        counts = _pump_until_settled(widget, qapp)

        growth_turns = sum(1 for before, after in zip(counts, counts[1:]) if after > before)
        assert growth_turns > 1

    def test_every_message_is_shown_once_the_reveal_completes(self, transcript_path, qapp) -> None:
        """Every message with content is visible when loading has finished."""
        widget = _make_widget(transcript_path)

        _pump_until_settled(widget, qapp)

        assert _visible_count(widget) == MESSAGE_COUNT
