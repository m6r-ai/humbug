"""Tests for the interruptible tool wait UI shown inside a tool-call message."""

import asyncio
import os

import pytest

from ai import AIMessage, AIMessageSource
from ai_transcript_conversation import AITranscriptConversation
from desktop.conversation_tab.conversation_message import ConversationMessage
from desktop.conversation_tab.conversation_widget import ConversationWidget


@pytest.fixture
def conv_env(mindspace_sandbox):
    """The sandboxed mindspace, as (mindspace_manager, transcript_path)."""
    transcript_path = os.path.join(mindspace_sandbox.conversations_dir, "test.conv")

    yield mindspace_sandbox.manager, transcript_path


def _make_widget(transcript_path: str) -> ConversationWidget:
    """Create a ConversationWidget backed by a fresh transcript."""
    transcript = AITranscriptConversation(transcript_path)

    return ConversationWidget(transcript_path, ai_transcript_conversation=transcript)


def _make_tool_call_message() -> ConversationMessage:
    """Create a tool-call message widget."""
    return ConversationMessage(AIMessageSource.TOOL_CALL, content="{}")


class TestConversationMessageWaitUi:
    """Tests for the interruptible wait UI on a message widget."""

    def test_show_adds_widget_and_cancel_emits_signal(self, qapp) -> None:
        """Showing the wait UI adds a widget and cancelling emits wait_cancelled."""
        message = _make_tool_call_message()
        cancelled: list[bool] = []
        message.wait_cancelled.connect(lambda: cancelled.append(True))

        message.show_interruptible_wait_ui(12.0)

        assert message._wait_widget is not None  # pylint: disable=protected-access
        assert message._wait_finish_button is not None  # pylint: disable=protected-access

        message._wait_finish_button.click()  # pylint: disable=protected-access

        assert cancelled == [True]
        assert message._wait_widget is None  # pylint: disable=protected-access

    def test_remove_clears_widget(self, qapp) -> None:
        """Removing the wait UI clears the widget and its button."""
        message = _make_tool_call_message()
        message.show_interruptible_wait_ui(12.0)

        message.remove_interruptible_wait_ui()

        assert message._wait_widget is None  # pylint: disable=protected-access
        assert message._wait_finish_button is None  # pylint: disable=protected-access
        assert message._wait_duration is None  # pylint: disable=protected-access

    def test_countdown_text_shows_remaining_time(self, qapp) -> None:
        """The wait text shows the total duration and the time remaining."""
        message = _make_tool_call_message()
        message.show_interruptible_wait_ui(12.0)

        initial = message._wait_text_edit.toPlainText()  # pylint: disable=protected-access
        assert "12 seconds" in initial
        assert "12 remaining" in initial

        message._on_wait_tick()  # pylint: disable=protected-access
        message._on_wait_tick()  # pylint: disable=protected-access

        updated = message._wait_text_edit.toPlainText()  # pylint: disable=protected-access
        assert "10 remaining" in updated
        assert "10.0" not in updated

        message.remove_interruptible_wait_ui()


class TestConversationWidgetWaitHandlers:
    """Tests for the conversation widget's interruptible wait handlers."""

    def test_started_shows_ui_on_matching_message(self, conv_env) -> None:
        """The started handler shows the wait UI on the matching message widget."""
        _mgr, transcript_path = conv_env
        widget = _make_widget(transcript_path)

        tool_call_message = AIMessage.create(AIMessageSource.TOOL_CALL, "{}")
        widget._ai_conversation.get_conversation_history().add_message(  # pylint: disable=protected-access
            tool_call_message
        )
        widget._load_message_history(  # pylint: disable=protected-access
            widget._ai_conversation.get_conversation_history().get_messages(),  # pylint: disable=protected-access
            True
        )

        asyncio.run(widget._on_interruptible_wait_started(tool_call_message, 12.0))  # pylint: disable=protected-access

        assert widget._pending_interruptible_wait is not None  # pylint: disable=protected-access
        assert widget._pending_interruptible_wait._wait_widget is not None  # pylint: disable=protected-access

        asyncio.run(widget._on_interruptible_wait_ended())  # pylint: disable=protected-access

        assert widget._pending_interruptible_wait is None  # pylint: disable=protected-access
