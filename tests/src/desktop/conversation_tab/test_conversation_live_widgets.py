"""
Tests for the widgets a message that arrives during a live conversation is given.

A live conversation produces the same kinds of message that history does which are never
shown: connection notices, the user-role wrappers that carry tool results, and empty
responses.  Unlike on load, where such a message is simply never given a widget, a live
message is given one and then found to have nothing to show.  These tests pin that the
widget is deleted at that point rather than left hidden in the layout, and that the
message still keeps its place in the display order.
"""

import asyncio
import os

import pytest

from ai import AIMessage, AIMessageSource
from ai_tool import AIToolResult
from ai_transcript_conversation import AITranscriptConversation

# pylint: disable=wrong-import-position
from desktop.conversation_tab.conversation_widget import ConversationWidget


@pytest.fixture
def widget(mindspace_sandbox) -> ConversationWidget:
    """A conversation widget backed by a fresh transcript."""
    transcript_path = os.path.join(mindspace_sandbox.conversations_dir, "test.conv")
    transcript = AITranscriptConversation(transcript_path)

    return ConversationWidget(transcript_path, ai_transcript_conversation=transcript)


def _add(widget: ConversationWidget, message: AIMessage) -> None:
    """Show a message that has just been added to the conversation."""
    widget._add_message(message)  # pylint: disable=protected-access


def _complete(widget: ConversationWidget, message: AIMessage) -> None:
    """Deliver a message that has just been completed."""
    asyncio.run(widget._on_message_completed(message))  # pylint: disable=protected-access


def _tool_round_trip(widget: ConversationWidget, prompt: str) -> None:
    """Run the messages one tool round trip adds through the widget."""
    messages = [
        AIMessage.create(AIMessageSource.USER, prompt),
        AIMessage.create(AIMessageSource.TOOL_CALL, "reading a file"),
        AIMessage.create(AIMessageSource.TOOL_RESULT, "file contents"),
        AIMessage.create(
            AIMessageSource.USER, "", tool_results=[AIToolResult(id="r1", name="read_file", content="file contents")]
        ),
        AIMessage.create(AIMessageSource.AI_CONNECTED, "Thinking..."),
    ]

    for message in messages:
        _add(widget, message)
        if message.source != AIMessageSource.AI_CONNECTED:
            _complete(widget, message)


class TestLiveMessagesThatAreNeverShown:
    """Tests for the messages a live conversation shows and then discards."""

    def test_connection_notice_is_deleted_once_its_turn_begins(self, widget, qapp) -> None:
        """A connection notice is shown until the turn it announced starts, then deleted."""
        notice = AIMessage.create(AIMessageSource.AI_CONNECTED, "Thinking...")
        _add(widget, notice)
        assert notice.id in widget._message_widgets  # pylint: disable=protected-access

        _add(widget, AIMessage.create(AIMessageSource.TOOL_CALL, "reading a file"))
        qapp.processEvents()

        assert notice.id not in widget._message_widgets  # pylint: disable=protected-access
        assert notice.id in widget._message_ids  # pylint: disable=protected-access

    def test_response_that_completes_with_no_content_has_no_widget(self, widget) -> None:
        """A response with nothing in it is left with no widget once it completes."""
        response = AIMessage.create(AIMessageSource.AI, "")
        _add(widget, response)
        _complete(widget, response)

        assert response.id not in widget._message_widgets  # pylint: disable=protected-access
        assert response.id in widget._message_ids  # pylint: disable=protected-access

    def test_message_carrying_tool_results_has_no_widget(self, widget) -> None:
        """The internal message that carries tool results has nothing to show, so it keeps no widget."""
        carrier = AIMessage.create(
            AIMessageSource.USER, "", tool_results=[AIToolResult(id="r1", name="read_file", content="file contents")]
        )
        _add(widget, carrier)
        _complete(widget, carrier)

        assert carrier.id not in widget._message_widgets  # pylint: disable=protected-access
        assert carrier.id in widget._message_ids  # pylint: disable=protected-access

    def test_message_with_something_to_show_keeps_its_widget(self, widget) -> None:
        """A completed tool call has content to display, so its widget stays on show."""
        tool_call = AIMessage.create(AIMessageSource.TOOL_CALL, "reading a file")
        _add(widget, tool_call)
        _complete(widget, tool_call)

        assert widget._message_widgets[tool_call.id].is_rendered()  # pylint: disable=protected-access


def test_a_live_round_trip_leaves_no_hidden_widgets(widget, qapp) -> None:
    """Processing a tool round trip and starting the next turn leaves nothing hidden behind."""
    _tool_round_trip(widget, "first prompt")
    _tool_round_trip(widget, "second prompt")
    qapp.processEvents()

    shown = list(widget._message_widgets.values())  # pylint: disable=protected-access
    assert shown
    assert all(message.is_rendered() for message in shown)
