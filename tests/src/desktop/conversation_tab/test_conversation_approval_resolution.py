"""Tests for the approval UI when an approval is resolved outside the conversation tab."""

import asyncio
import os

import pytest

from ai import AIMessage, AIMessageSource
from ai_tool import AIToolApprovalKind, AIToolCall
from ai_transcript_conversation import AITranscriptConversation
from desktop.conversation_tab.conversation_widget import ConversationWidget


@pytest.fixture
def widget(mindspace_sandbox):
    """A conversation widget backed by a fresh transcript in the sandboxed mindspace."""
    transcript_path = os.path.join(mindspace_sandbox.conversations_dir, "approval.conv")
    transcript = AITranscriptConversation(transcript_path)
    conversation_widget = ConversationWidget(transcript_path, ai_transcript_conversation=transcript)
    yield conversation_widget
    conversation_widget.deleteLater()


def _add_tool_call_message(widget: ConversationWidget) -> AIMessage:
    """Add a tool call message to the conversation and display it."""
    message = AIMessage.create(AIMessageSource.TOOL_CALL, "{}", completed=False)
    history = widget._ai_conversation.get_conversation_history()  # pylint: disable=protected-access
    history.add_message(message)
    widget._load_message_history(history.get_messages(), True)  # pylint: disable=protected-access
    return message


def _show_approval(widget: ConversationWidget, message: AIMessage) -> None:
    """Show the approval UI for a tool call message, as an approval request does."""
    tool_call = AIToolCall(id="auth_filesystem", name="filesystem", arguments={"operation": "write_file"})
    asyncio.run(widget._on_tool_approval_required(  # pylint: disable=protected-access
        message, tool_call, "Write a file", None, True, AIToolApprovalKind.STANDARD
    ))


class TestApprovalResolvedElsewhere:
    """The tab's approval buttons go away when the approval is answered somewhere else."""

    def test_completion_of_the_pending_message_removes_the_approval_ui(self, widget) -> None:
        """When the tool call message completes, its approval buttons are removed."""
        message = _add_tool_call_message(widget)
        _show_approval(widget, message)
        pending = widget._pending_tool_call_approval  # pylint: disable=protected-access
        assert pending is not None
        assert pending.get_tool_approval_info() is not None

        message.completed = True
        asyncio.run(widget._on_message_completed(message))  # pylint: disable=protected-access

        assert widget._pending_tool_call_approval is None  # pylint: disable=protected-access
        assert pending.get_tool_approval_info() is None

    def test_completion_of_another_message_keeps_the_approval_ui(self, widget) -> None:
        """A different message completing leaves a pending approval untouched."""
        message = _add_tool_call_message(widget)
        _show_approval(widget, message)

        other = AIMessage.create(AIMessageSource.AI, "Still thinking", completed=True)
        asyncio.run(widget._on_message_completed(other))  # pylint: disable=protected-access

        pending = widget._pending_tool_call_approval  # pylint: disable=protected-access
        assert pending is not None
        assert pending.get_tool_approval_info() is not None
