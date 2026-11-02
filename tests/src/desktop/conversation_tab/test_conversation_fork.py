"""Tests for forking a conversation from one of its messages."""

import os

import pytest

# pylint: disable=wrong-import-position
from ai import AIMessage, AIMessageSource
from ai_transcript_conversation import AITranscriptConversation
from desktop.conversation_tab.conversation_widget import ConversationWidget

# pylint: disable=protected-access


@pytest.fixture
def conversation(qapp, mindspace_sandbox):
    """A conversation widget holding two prompts and a response to each."""
    transcript_path = os.path.join(mindspace_sandbox.conversations_dir, "test.conv")
    transcript = AITranscriptConversation(transcript_path)
    widget = ConversationWidget(transcript_path, ai_transcript_conversation=transcript)

    history = widget._ai_conversation.get_conversation_history()
    messages = [
        AIMessage.create(AIMessageSource.USER, "first prompt"),
        AIMessage.create(AIMessageSource.AI, "first response"),
        AIMessage.create(AIMessageSource.USER, "second prompt"),
        AIMessage.create(AIMessageSource.AI, "second response"),
    ]

    for message in messages:
        history.add_message(message)

    widget._load_message_history(
        history.get_messages(), True, attachments=history.attachments()
    )
    widget.resize(1000, 700)
    widget.show()
    qapp.processEvents()

    yield widget, messages

    widget.close_widget()
    widget.deleteLater()
    qapp.processEvents()


def _request_fork(widget: ConversationWidget, message_widget) -> list[int]:
    """Emit a fork request from the given message widget and return what the view asked for."""
    requested: list[int] = []
    widget.fork_from_index_requested.connect(requested.append)
    message_widget.fork_requested.emit()

    return requested


class TestForkFromMessage:
    """Tests for the fork point the view reports when a message is forked."""

    def test_forking_from_a_prompt_forks_everything_before_it(self, conversation) -> None:
        """Forking from the second prompt carries the fork point that precedes it."""
        widget, _messages = conversation

        requested = _request_fork(widget, widget._ordered_widgets()[2])

        assert requested == [2]
        forked = widget._ai_conversation.fork_history(requested[0])
        assert [m.content for m in forked.get_messages()] == ["first prompt", "first response"]

    def test_forking_from_the_first_prompt_forks_an_empty_conversation(self, conversation) -> None:
        """Forking from the very first prompt leaves nothing before it."""
        widget, _messages = conversation

        requested = _request_fork(widget, widget._ordered_widgets()[0])

        assert requested == [0]
        assert widget._ai_conversation.fork_history(requested[0]).get_messages() == []

    def test_forking_from_a_later_message_includes_all_the_earlier_ones(self, conversation) -> None:
        """Forking from the last message carries everything that precedes it."""
        widget, messages = conversation

        requested = _request_fork(widget, widget._ordered_widgets()[3])

        assert requested == [3]
        forked = widget._ai_conversation.fork_history(requested[0])
        assert [m.id for m in forked.get_messages()] == [m.id for m in messages[:3]]
