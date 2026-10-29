"""Tests for the conversation's entries in the mindspace interaction log."""

import os

from ai import AIMessage, AIMessageSource
from ai_tool import AIToolCall, AIToolResult
from ai_transcript_conversation import AITranscriptConversation
from desktop.conversation_tab.conversation_widget import LOG_PREVIEW_CHARS, ConversationWidget
from mindspace.mindspace_message import MindspaceMessage


def _make_widget(mindspace_sandbox) -> tuple[ConversationWidget, str]:
    """Return a conversation widget backed by a fresh transcript, and that transcript's path."""
    transcript_path = os.path.join(mindspace_sandbox.conversations_dir, "test.conv")
    transcript = AITranscriptConversation(transcript_path)

    return (
        ConversationWidget(transcript_path, ai_transcript_conversation=transcript),
        transcript_path,
    )


def _latest(mindspace_sandbox) -> MindspaceMessage:
    """Return the newest entry in the mindspace's interaction log."""
    return mindspace_sandbox.manager.interactions().newest(1)[0]


class TestCompletedMessageLogging:
    """Tests for the entries a completed conversation message produces."""

    def test_human_prompt_is_recorded_against_the_conversation(self, mindspace_sandbox) -> None:
        """A human prompt is logged and attributed to the conversation that submitted it."""
        widget, transcript_path = _make_widget(mindspace_sandbox)
        message = AIMessage.create(AIMessageSource.USER, "How do I open a file?")

        widget._log_completed_message(message)  # pylint: disable=protected-access
        entry = _latest(mindspace_sandbox)

        assert entry.content == "Human submitted prompt: 'How do I open a file?'"
        assert entry.context[MindspaceMessage.CONVERSATION_CONTEXT] == os.path.relpath(
            transcript_path, mindspace_sandbox.path
        )

    def test_prompt_attachments_are_counted(self, mindspace_sandbox) -> None:
        """A prompt carrying attachments records how many were attached."""
        widget, _ = _make_widget(mindspace_sandbox)
        message = AIMessage.create(
            AIMessageSource.USER, "Summarise this", attachments=["a-guid", "b-guid"]
        )

        widget._log_completed_message(message)  # pylint: disable=protected-access

        assert _latest(mindspace_sandbox).content == (
            "Human submitted prompt: 'Summarise this'\nattachments: 2"
        )

    def test_prompt_from_another_ai_names_the_requester(self, mindspace_sandbox) -> None:
        """A prompt submitted by another AI is recorded as that AI's prompt, not a human's."""
        widget, _ = _make_widget(mindspace_sandbox)
        message = AIMessage.create(
            AIMessageSource.USER, "Draft a plan", user_name="Claude Sonnet 4.5"
        )

        widget._log_completed_message(message)  # pylint: disable=protected-access

        assert _latest(mindspace_sandbox).content == (
            "AI 'Claude Sonnet 4.5' submitted prompt: 'Draft a plan'"
        )

    def test_queued_prompt_is_recorded_as_queued(self, mindspace_sandbox) -> None:
        """A prompt submitted mid-turn is recorded as queued."""
        widget, _ = _make_widget(mindspace_sandbox)
        message = AIMessage.create(AIMessageSource.USER_QUEUED, "And another thing")

        widget._log_completed_message(message)  # pylint: disable=protected-access

        assert _latest(mindspace_sandbox).content == "Human queued prompt: 'And another thing'"

    def test_ai_response_is_recorded(self, mindspace_sandbox) -> None:
        """A completed AI response is recorded against its conversation."""
        widget, _ = _make_widget(mindspace_sandbox)
        message = AIMessage.create(AIMessageSource.AI, "Open the file from the Files panel.")

        widget._log_completed_message(message)  # pylint: disable=protected-access
        entry = _latest(mindspace_sandbox)

        assert entry.content == "AI response: 'Open the file from the Files panel.'"
        assert MindspaceMessage.CONVERSATION_CONTEXT in entry.context

    def test_ai_reasoning_is_recorded_separately(self, mindspace_sandbox) -> None:
        """Reasoning is recorded as reasoning, not as a response."""
        widget, _ = _make_widget(mindspace_sandbox)
        message = AIMessage.create(AIMessageSource.REASONING, "The user wants the files panel.")

        widget._log_completed_message(message)  # pylint: disable=protected-access

        assert _latest(mindspace_sandbox).content == (
            "AI reasoning: 'The user wants the files panel.'"
        )

    def test_response_with_only_tool_calls_names_the_tools(self, mindspace_sandbox) -> None:
        """A response that consists only of tool calls records the tools it asked for."""
        widget, _ = _make_widget(mindspace_sandbox)
        message = AIMessage.create(
            AIMessageSource.AI,
            "",
            tool_calls=[
                AIToolCall(id="1", name="filesystem", arguments={"operation": "read_file"}),
                AIToolCall(id="2", name="clock", arguments={}),
            ],
        )

        widget._log_completed_message(message)  # pylint: disable=protected-access

        assert _latest(mindspace_sandbox).content == (
            "AI requested tool calls: filesystem.read_file, clock"
        )

    def test_empty_messages_produce_no_entry(self, mindspace_sandbox) -> None:
        """A message with nothing in it is not worth an audit entry."""
        widget, _ = _make_widget(mindspace_sandbox)
        before = mindspace_sandbox.manager.interactions().count()

        widget._log_completed_message(  # pylint: disable=protected-access
            AIMessage.create(AIMessageSource.REASONING, "")
        )
        widget._log_completed_message(  # pylint: disable=protected-access
            AIMessage.create(AIMessageSource.AI, "")
        )

        assert mindspace_sandbox.manager.interactions().count() == before

    def test_tool_results_are_not_recorded_as_prompts(self, mindspace_sandbox) -> None:
        """The internal message carrying tool results is not a prompt and is not logged."""
        widget, _ = _make_widget(mindspace_sandbox)
        before = mindspace_sandbox.manager.interactions().count()
        message = AIMessage.create(
            AIMessageSource.USER,
            "",
            tool_results=[AIToolResult(id="1", name="filesystem", content="ok")],
        )

        widget._log_completed_message(message)  # pylint: disable=protected-access

        assert mindspace_sandbox.manager.interactions().count() == before

    def test_long_content_is_shortened_to_one_line(self, mindspace_sandbox) -> None:
        """Long or multi-line content is shortened to a single readable excerpt."""
        widget, _ = _make_widget(mindspace_sandbox)
        flat = "first line " + ("x" * 200)

        widget._log_completed_message(  # pylint: disable=protected-access
            AIMessage.create(AIMessageSource.AI, "first line\n" + ("x" * 200))
        )

        assert _latest(mindspace_sandbox).content == (
            f"AI response: '{flat[:LOG_PREVIEW_CHARS]}...'"
        )
