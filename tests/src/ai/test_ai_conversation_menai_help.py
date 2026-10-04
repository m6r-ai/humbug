"""Tests for AIConversation.menai_help_read history-derived behaviour."""

from ai import AIConversation
from ai.ai_message import AIMessage, AIMessageSource
from ai_tool import AIToolCall, AIToolResult


def _add_user_message(conversation: AIConversation, content: str) -> AIMessage:
    """Add a user message directly to the conversation history."""
    msg = AIMessage.create(AIMessageSource.USER, content)
    conversation.get_conversation_history().add_message(msg)

    return msg


def _add_tool_call_message(
    conversation: AIConversation,
    tool_call: AIToolCall,
) -> AIMessage:
    """Add a tool call message directly to the conversation history."""
    import json

    content = f"```json\n{json.dumps(tool_call.to_dict(), indent=2)}\n```"
    msg = AIMessage.create(
        AIMessageSource.TOOL_CALL,
        content,
        tool_calls=[tool_call],
    )
    conversation.get_conversation_history().add_message(msg)

    return msg


def _add_tool_result_message(
    conversation: AIConversation,
    tool_result: AIToolResult,
) -> AIMessage:
    """Add a tool result message directly to the conversation history."""
    import json

    content = f"```json\n{json.dumps(tool_result.to_dict(), indent=2)}\n```"
    msg = AIMessage.create(
        AIMessageSource.TOOL_RESULT,
        content,
        tool_results=[tool_result],
    )
    conversation.get_conversation_history().add_message(msg)

    return msg


def _make_help_call(tool_name: str) -> AIToolCall:
    """Create a help tool call targeting the given tool."""
    return AIToolCall(
        id="help-call-1",
        name="help",
        arguments={"operation": "get_help", "tool_name": tool_name},
    )


class TestMenaiHelpRead:
    """Tests for AIConversation.menai_help_read."""

    def test_no_history_returns_false(self):
        """menai_help_read returns False for a fresh conversation."""
        conv = AIConversation()

        assert conv.menai_help_read() is False

    def test_non_menai_help_does_not_unlock(self):
        """A help call for a different tool does not unlock Menai."""
        conv = AIConversation()
        _add_tool_call_message(conv, _make_help_call("editor"))

        assert conv.menai_help_read() is False

    def test_menai_help_call_unlocks(self):
        """A help call for the menai tool unlocks Menai operations."""
        conv = AIConversation()
        _add_tool_call_message(conv, _make_help_call("menai"))

        assert conv.menai_help_read() is True

    def test_menai_help_unlocks_with_operation_name(self):
        """A help call for menai with an operation name still unlocks."""
        conv = AIConversation()
        call = AIToolCall(
            id="help-call-1",
            name="help",
            arguments={"operation": "get_help", "tool_name": "menai", "operation_name": "evaluate"},
        )
        _add_tool_call_message(conv, call)

        assert conv.menai_help_read() is True

    def test_truncation_removing_help_relocks(self):
        """Truncating at a user message before the help call relocks Menai."""
        conv = AIConversation()

        user1 = _add_user_message(conv, "hello")
        user2 = _add_user_message(conv, "do something with menai")
        _add_tool_call_message(conv, _make_help_call("menai"))
        _add_tool_result_message(
            conv,
            AIToolResult(id="help-call-1", name="help", content="Menai docs"),
        )

        # Help is present, Menai is unlocked
        assert conv.menai_help_read() is True

        # Truncate to user2 — removes user2 and everything after (including the help call)
        conv.truncate_to_message(user2.id)

        assert conv.menai_help_read() is False

    def test_truncation_preserving_help_stays_unlocked(self):
        """Truncating after the Menai help call keeps Menai unlocked."""
        conv = AIConversation()

        user1 = _add_user_message(conv, "hello")
        _add_tool_call_message(conv, _make_help_call("menai"))
        _add_tool_result_message(
            conv,
            AIToolResult(id="help-call-1", name="help", content="Menai docs"),
        )

        user2 = _add_user_message(conv, "do something")

        # Truncate to user2 — the help call before user2 is preserved
        conv.truncate_to_message(user2.id)

        assert conv.menai_help_read() is True

    def test_load_history_with_help_unlocks(self):
        """Loading a history containing a Menai help call unlocks Menai."""
        conv = AIConversation()

        # Build a history with a menai help call
        history = conv.get_conversation_history()
        history.add_message(
            AIMessage.create(
                AIMessageSource.TOOL_CALL,
                "tool call",
                tool_calls=[_make_help_call("menai")],
            )
        )

        # Create a fresh conversation and load the history
        conv2 = AIConversation()
        conv2.load_history(history)

        assert conv2.menai_help_read() is True

    def test_load_history_without_help_stays_locked(self):
        """Loading a history without a Menai help call keeps Menai locked."""
        conv = AIConversation()
        history = conv.get_conversation_history()
        history.add_message(AIMessage.create(AIMessageSource.USER, "hello"))

        conv2 = AIConversation()
        conv2.load_history(history)

        assert conv2.menai_help_read() is False

    def test_non_tool_call_messages_ignored(self):
        """User and AI messages do not affect the menai_help_read result."""
        conv = AIConversation()
        history = conv.get_conversation_history()
        history.add_message(AIMessage.create(AIMessageSource.USER, "help with menai"))
        history.add_message(AIMessage.create(AIMessageSource.AI, "here is some help"))

        assert conv.menai_help_read() is False

    def test_help_call_without_tool_name_argument(self):
        """A help call missing the tool_name argument does not unlock Menai."""
        conv = AIConversation()
        call = AIToolCall(
            id="help-call-1",
            name="help",
            arguments={"operation": "list_tools"},
        )
        _add_tool_call_message(conv, call)

        assert conv.menai_help_read() is False
