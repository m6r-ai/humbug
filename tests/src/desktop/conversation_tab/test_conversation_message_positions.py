"""Tests for the difference between a message's position in the view and in the transcript."""

import os

import pytest

# pylint: disable=wrong-import-position
from ai import AIMessage, AIMessageSource
from ai_transcript_conversation import AITranscriptConversation
from desktop.conversation_tab.conversation_widget import ConversationWidget

# pylint: disable=protected-access


@pytest.fixture
def conversation(qapp, mindspace_sandbox):
    """A laid-out conversation widget holding two prompts and a response to each."""
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


class TestTranscriptPosition:
    """Tests for locating a message within the conversation transcript."""

    def test_a_transcribed_message_reports_its_position_in_the_transcript(self, conversation) -> None:
        """Each message on show reports the position at which it appears in the transcript."""
        widget, messages = conversation

        assert [widget._history_index(message.id) for message in messages] == [0, 1, 2, 3]

    def test_an_untranscribed_message_has_no_transcript_position(self, conversation) -> None:
        """A prompt that is displayed but not yet written to the transcript has no position."""
        widget, _messages = conversation
        queued = AIMessage.create(AIMessageSource.USER_QUEUED, "and another thing")

        widget._add_message(queued)

        assert widget._history_index(queued.id) is None

    def test_an_unknown_message_has_no_transcript_position(self, conversation) -> None:
        """A message id that is not in the conversation has no position."""
        widget, _messages = conversation

        assert widget._history_index("not-a-real-id") is None

    def test_view_and_transcript_positions_agree_while_every_message_is_transcribed(
        self, conversation
    ) -> None:
        """With nothing queued, a message's view position and transcript position coincide."""
        widget, messages = conversation

        for message in messages:
            assert widget._message_index(message.id) == widget._history_index(message.id)


class TestScrollToMessagePosition:
    """Tests for the index accepted by the scroll operation."""

    def test_the_index_is_a_transcript_position(self, conversation) -> None:
        """Requesting a position selects the message the transcript holds at that position."""
        widget, messages = conversation

        result = widget.scroll_to_message_by_id_or_index(message_index=1)

        assert result["success"]
        assert result["message_id"] == messages[1].id
        assert result["target_index"] == 1

    def test_a_position_past_the_end_of_the_transcript_is_rejected(self, conversation) -> None:
        """A prompt displayed ahead of being transcribed does not extend the valid positions."""
        widget, messages = conversation
        widget._add_message(AIMessage.create(AIMessageSource.USER_QUEUED, "and another thing"))

        result = widget.scroll_to_message_by_id_or_index(message_index=len(messages))

        assert not result["success"]
        assert f"Invalid message index: {len(messages)}" == result["error"]

    def test_the_reported_positions_are_transcript_positions(self, conversation) -> None:
        """The positions reported back are the transcript's, matching the other operations."""
        widget, messages = conversation

        result = widget.scroll_to_message_by_id_or_index(message_id=messages[3].id)

        assert result["success"]
        assert result["target_index"] == messages.index(messages[3])
        assert result["actual_index"] == messages.index(messages[3])
