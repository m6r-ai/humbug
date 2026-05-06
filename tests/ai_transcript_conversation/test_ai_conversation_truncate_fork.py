"""Tests for AIConversation.truncate_to_message and fork_history_to_index."""

import pytest

from ai import AIConversation, AIConversationHistory
from ai.ai_message import AIMessage, AIMessageSource


def _add_user_message(conversation: AIConversation, content: str, attachments=None) -> AIMessage:
    """Add a user message directly to the conversation history."""
    history = conversation.get_conversation_history()
    msg = AIMessage.create(AIMessageSource.USER, content, attachments=attachments)
    history.add_message(msg)
    return msg


def _add_ai_message(conversation: AIConversation, content: str) -> AIMessage:
    """Add an AI response directly to the conversation history."""
    history = conversation.get_conversation_history()
    msg = AIMessage.create(AIMessageSource.AI, content)
    history.add_message(msg)
    return msg


class TestTruncateToMessage:
    """Tests for AIConversation.truncate_to_message."""

    def test_truncate_removes_message_and_everything_after(self):
        """Truncating at a user message removes that message and all subsequent messages."""
        conv = AIConversation()
        _add_user_message(conv, "first")
        _add_ai_message(conv, "response to first")
        msg2 = _add_user_message(conv, "second")
        _add_ai_message(conv, "response to second")

        result = conv.truncate_to_message(msg2.id)

        messages = conv.get_conversation_history().get_messages()
        assert len(messages) == 2
        assert messages[0].content == "first"
        assert messages[1].content == "response to first"

    def test_truncate_returns_content_of_truncated_message(self):
        """truncate_to_message returns the content of the removed message."""
        conv = AIConversation()
        _add_user_message(conv, "first")
        msg2 = _add_user_message(conv, "the message to delete")

        result = conv.truncate_to_message(msg2.id)
        assert result == "the message to delete"

    def test_truncate_at_first_message_leaves_empty_history(self):
        """Truncating at the first message leaves an empty history."""
        conv = AIConversation()
        msg = _add_user_message(conv, "only message")
        _add_ai_message(conv, "response")

        conv.truncate_to_message(msg.id)
        assert conv.get_conversation_history().get_messages() == []

    def test_truncate_unknown_id_returns_none(self):
        """truncate_to_message returns None for an unknown message ID."""
        conv = AIConversation()
        _add_user_message(conv, "hello")
        result = conv.truncate_to_message("no-such-id")
        assert result is None

    def test_truncate_non_user_message_returns_none(self):
        """truncate_to_message returns None when the target is not a user message."""
        conv = AIConversation()
        _add_user_message(conv, "question")
        ai_msg = _add_ai_message(conv, "answer")

        result = conv.truncate_to_message(ai_msg.id)
        assert result is None

    def test_truncate_preserves_referenced_attachments(self):
        """Attachments referenced by preserved messages are kept."""
        conv = AIConversation()
        history = conv.get_conversation_history()
        guid = history.add_attachment("# blueprint", "blueprint.md", "blueprint")

        msg1 = AIMessage.create(AIMessageSource.USER, "first", attachments=[guid])
        history.add_message(msg1)
        msg2 = AIMessage.create(AIMessageSource.USER, "second")
        history.add_message(msg2)
        _add_ai_message(conv, "response")

        conv.truncate_to_message(msg2.id)

        remaining_attachments = conv.get_conversation_history().attachments()
        assert guid in remaining_attachments

    def test_truncate_removes_unreferenced_attachments(self):
        """Attachments only referenced by removed messages are pruned."""
        conv = AIConversation()
        history = conv.get_conversation_history()

        guid_kept = history.add_attachment("kept", "kept.py", "file")
        guid_pruned = history.add_attachment("pruned", "pruned.py", "file")

        msg1 = AIMessage.create(AIMessageSource.USER, "first", attachments=[guid_kept])
        history.add_message(msg1)
        msg2 = AIMessage.create(AIMessageSource.USER, "second", attachments=[guid_pruned])
        history.add_message(msg2)

        conv.truncate_to_message(msg2.id)

        remaining = conv.get_conversation_history().attachments()
        assert guid_kept in remaining
        assert guid_pruned not in remaining

    def test_truncate_history_with_no_attachments(self):
        """Truncation works correctly when there are no attachments at all."""
        conv = AIConversation()
        _add_user_message(conv, "one")
        msg2 = _add_user_message(conv, "two")
        _add_ai_message(conv, "reply")

        result = conv.truncate_to_message(msg2.id)
        assert result == "two"
        assert len(conv.get_conversation_history().get_messages()) == 1

    def test_truncate_does_not_affect_history_when_returning_none(self):
        """When truncate_to_message returns None, the history is unchanged."""
        conv = AIConversation()
        msg = _add_user_message(conv, "hello")
        _add_ai_message(conv, "world")

        original_count = len(conv.get_conversation_history().get_messages())
        conv.truncate_to_message("bad-id")
        assert len(conv.get_conversation_history().get_messages()) == original_count


class TestForkHistoryToIndex:
    """Tests for AIConversation.fork_history_to_index."""

    def test_fork_returns_correct_message_slice(self):
        """fork_history_to_index returns messages up to (not including) the given index."""
        conv = AIConversation()
        _add_user_message(conv, "one")
        _add_ai_message(conv, "two")
        _add_user_message(conv, "three")
        _add_ai_message(conv, "four")

        forked = conv.fork_history_to_index(2)
        messages = forked.get_messages()
        assert len(messages) == 2
        assert messages[0].content == "one"
        assert messages[1].content == "two"

    def test_fork_zero_returns_empty_history(self):
        """fork_history_to_index(0) returns an empty history."""
        conv = AIConversation()
        _add_user_message(conv, "hello")
        forked = conv.fork_history_to_index(0)
        assert forked.get_messages() == []

    def test_fork_full_length_returns_all_messages(self):
        """fork_history_to_index(len) returns all messages."""
        conv = AIConversation()
        _add_user_message(conv, "a")
        _add_ai_message(conv, "b")
        _add_user_message(conv, "c")

        all_messages = conv.get_conversation_history().get_messages()
        forked = conv.fork_history_to_index(len(all_messages))
        assert len(forked.get_messages()) == 3

    def test_fork_does_not_modify_original(self):
        """fork_history_to_index does not modify the original conversation."""
        conv = AIConversation()
        _add_user_message(conv, "one")
        _add_user_message(conv, "two")
        _add_user_message(conv, "three")

        conv.fork_history_to_index(1)
        assert len(conv.get_conversation_history().get_messages()) == 3

    def test_fork_includes_only_referenced_attachments(self):
        """Forked history includes only attachments referenced by the forked messages."""
        conv = AIConversation()
        history = conv.get_conversation_history()

        guid_included = history.add_attachment("included", "a.py", "file")
        guid_excluded = history.add_attachment("excluded", "b.py", "file")

        msg1 = AIMessage.create(AIMessageSource.USER, "first", attachments=[guid_included])
        history.add_message(msg1)
        msg2 = AIMessage.create(AIMessageSource.USER, "second", attachments=[guid_excluded])
        history.add_message(msg2)

        forked = conv.fork_history_to_index(1)

        assert guid_included in forked.attachments()
        assert guid_excluded not in forked.attachments()

    def test_fork_with_no_attachments(self):
        """Forking a history with no attachments produces an empty attachment store."""
        conv = AIConversation()
        _add_user_message(conv, "one")
        _add_user_message(conv, "two")

        forked = conv.fork_history_to_index(1)
        assert forked.attachments() == {}

    def test_fork_returns_aiconversationhistory(self):
        """fork_history_to_index returns an AIConversationHistory instance."""
        conv = AIConversation()
        _add_user_message(conv, "hello")
        forked = conv.fork_history_to_index(1)
        assert isinstance(forked, AIConversationHistory)

    def test_fork_preserves_shared_attachments(self):
        """An attachment referenced by both forked and non-forked messages is included."""
        conv = AIConversation()
        history = conv.get_conversation_history()

        shared_guid = history.add_attachment("shared", "shared.py", "file")
        msg1 = AIMessage.create(AIMessageSource.USER, "first", attachments=[shared_guid])
        history.add_message(msg1)
        msg2 = AIMessage.create(AIMessageSource.USER, "second", attachments=[shared_guid])
        history.add_message(msg2)

        # Fork includes only msg1, but the shared attachment should be in the fork
        forked = conv.fork_history_to_index(1)
        assert shared_guid in forked.attachments()
