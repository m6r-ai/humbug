"""Tests for the correspondence between message widgets and transcript messages."""

import os

import pytest

# pylint: disable=wrong-import-position
from ai import AIMessage, AIMessageSource
from ai_transcript_conversation import AITranscriptConversation
from desktop.conversation_tab.conversation_widget import ConversationWidget

# pylint: disable=protected-access


@pytest.fixture
def conv_env(mindspace_sandbox):
    """The sandboxed mindspace, as (mindspace_manager, transcript_path)."""
    transcript_path = os.path.join(mindspace_sandbox.conversations_dir, "test.conv")

    yield mindspace_sandbox.manager, transcript_path


def _make_widget(transcript_path: str) -> ConversationWidget:
    """Create a ConversationWidget backed by a fresh transcript."""
    transcript = AITranscriptConversation(transcript_path)
    return ConversationWidget(transcript_path, ai_transcript_conversation=transcript)


def _two_turn_messages() -> list[AIMessage]:
    """Return two prompts with a response to each."""
    return [
        AIMessage.create(AIMessageSource.USER, "first prompt"),
        AIMessage.create(AIMessageSource.AI, "first response"),
        AIMessage.create(AIMessageSource.USER, "second prompt"),
        AIMessage.create(AIMessageSource.AI, "second response"),
    ]


def _load(widget: ConversationWidget, messages: list[AIMessage]) -> None:
    """Replay messages into the widget, as opening a transcript does."""
    history = widget._ai_conversation.get_conversation_history()
    for message in messages:
        history.add_message(message)

    widget._load_message_history(
        history.get_messages(), True, attachments=history.attachments()
    )


def _widget_ids(widget: ConversationWidget) -> list[str]:
    """Return the message ids of the widgets on show, in display order."""
    return [message.message_id() for message in widget._ordered_widgets()]


def _transcript_ids(widget: ConversationWidget) -> list[str]:
    """Return the message ids of the transcript, in transcript order."""
    history = widget._ai_conversation.get_conversation_history()
    return [message.id for message in history.get_messages()]


class TestWidgetTranscriptCorrespondence:
    """Tests that message widgets and transcript messages correspond one to one."""

    def test_each_transcript_message_has_exactly_one_widget(self, conv_env) -> None:
        """A replayed transcript produces one widget per message, in the same order."""
        _mgr, transcript_path = conv_env
        widget = _make_widget(transcript_path)
        messages = _two_turn_messages()

        _load(widget, messages)

        assert _widget_ids(widget) == [message.id for message in messages]

    def test_widgets_are_shown_in_transcript_order(self, conv_env) -> None:
        """The widgets are ordered top to bottom as the messages appear in the transcript."""
        _mgr, transcript_path = conv_env
        widget = _make_widget(transcript_path)

        _load(widget, _two_turn_messages())

        assert [w.message_content() for w in widget._ordered_widgets()] == [
            "first prompt",
            "first response",
            "second prompt",
            "second response",
        ]

    def test_queued_prompt_is_shown_without_a_transcript_entry(self, conv_env) -> None:
        """A prompt queued mid-turn is displayed although it is not yet in the transcript."""
        _mgr, transcript_path = conv_env
        widget = _make_widget(transcript_path)
        _load(widget, _two_turn_messages())

        queued = AIMessage.create(AIMessageSource.USER_QUEUED, "and another thing")
        widget._add_message(queued)

        assert queued.id in _widget_ids(widget)
        assert queued.id not in _transcript_ids(widget)

    def test_queued_prompt_is_removed_when_the_real_prompt_arrives(self, conv_env) -> None:
        """The queued placeholder gives way to the submitted prompt, leaving one widget per message."""
        _mgr, transcript_path = conv_env
        widget = _make_widget(transcript_path)
        _load(widget, _two_turn_messages())

        widget._add_message(AIMessage.create(AIMessageSource.USER_QUEUED, "and another thing"))

        history = widget._ai_conversation.get_conversation_history()
        submitted = AIMessage.create(AIMessageSource.USER, "and another thing")
        history.add_message(submitted)
        widget._add_message(submitted)

        assert _widget_ids(widget) == _transcript_ids(widget)
        assert not any(
            w.message_source() == AIMessageSource.USER_QUEUED for w in widget._ordered_widgets()
        )

    def test_a_transient_queued_prompt_does_not_move_the_other_messages(self, conv_env) -> None:
        """Displaying and then dropping a queued prompt leaves every other message where it was."""
        _mgr, transcript_path = conv_env
        widget = _make_widget(transcript_path)
        _load(widget, _two_turn_messages())
        positions = {
            message.message_id(): i
            for i, message in enumerate(widget._ordered_widgets())
        }

        widget._add_message(AIMessage.create(AIMessageSource.USER_QUEUED, "and another thing"))
        widget._add_message(AIMessage.create(AIMessageSource.USER, "and another thing"))

        for message_id, position in positions.items():
            assert _widget_ids(widget).index(message_id) == position

    def test_updating_a_message_targets_the_widget_with_that_id(self, conv_env) -> None:
        """An update is applied to the widget carrying the updated message's id."""
        _mgr, transcript_path = conv_env
        widget = _make_widget(transcript_path)
        messages = _two_turn_messages()
        _load(widget, messages)

        first_response = messages[1]
        revised = first_response.copy()
        revised.content = "first response revised"
        widget._update_last_message(revised)

        assert widget._response_reveal_widgets[first_response.id].message_id() == first_response.id
        assert (
            widget._response_reveal_widgets[first_response.id]
            is not widget._ordered_widgets()[3]
        )
