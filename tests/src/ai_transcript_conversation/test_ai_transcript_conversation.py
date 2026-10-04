"""Tests for AITranscriptConversation."""

import json
import os
import tempfile

import pytest

from ai import AIConversationHistory
from ai.ai_message import AIMessage, AIMessageSource
from ai_transcript_conversation import AITranscriptConversation


@pytest.fixture
def transcript_path():
    """Provide a temporary transcript file path."""
    with tempfile.NamedTemporaryFile(suffix=".conv", delete=False) as f:
        path = f.name

    os.unlink(path)  # Remove so AITranscriptConversation initialises it fresh
    yield path

    if os.path.exists(path):
        os.unlink(path)


@pytest.fixture
def fork_path():
    """Provide a second temporary path for fork targets."""
    with tempfile.NamedTemporaryFile(suffix=".conv", delete=False) as f:
        path = f.name

    os.unlink(path)  # Remove so AITranscriptConversation initialises it fresh
    yield path

    if os.path.exists(path):
        os.unlink(path)


def _add_user_message(tc: AITranscriptConversation, content: str, attachments=None) -> AIMessage:
    """Add a user message directly to the inner history."""
    history = tc.get_conversation_history()
    msg = AIMessage.create(AIMessageSource.USER, content, attachments=attachments)
    history.add_message(msg)
    return msg


def _add_ai_message(tc: AITranscriptConversation, content: str) -> AIMessage:
    """Add an AI message directly to the inner history."""
    history = tc.get_conversation_history()
    msg = AIMessage.create(AIMessageSource.AI, content)
    history.add_message(msg)
    return msg


class TestAITranscriptConversationLifecycle:
    """Tests for creation, path management, and basic delegation."""

    def test_creates_transcript_file_on_init(self, transcript_path):
        """Constructing AITranscriptConversation creates the transcript file."""
        assert not os.path.exists(transcript_path)  # fixture already removed it
        AITranscriptConversation(transcript_path)
        assert os.path.exists(transcript_path)

    def test_path_returns_transcript_path(self, transcript_path):
        """path() returns the transcript file path."""
        tc = AITranscriptConversation(transcript_path)
        assert tc.path() == transcript_path

    def test_set_path_updates_path(self, transcript_path, fork_path):
        """set_path() updates the path returned by path()."""
        tc = AITranscriptConversation(transcript_path)
        tc.set_path(fork_path)
        assert tc.path() == fork_path

    def test_inner_conversation_returns_ai_conversation(self, transcript_path):
        """inner_conversation() returns the underlying AIConversation."""
        from ai import AIConversation
        tc = AITranscriptConversation(transcript_path)
        assert isinstance(tc.inner_conversation(), AIConversation)

    def test_read_returns_empty_history_for_new_file(self, transcript_path):
        """read() on a fresh transcript returns an empty history."""
        tc = AITranscriptConversation(transcript_path)
        history = tc.read()
        assert history.get_messages() == []
        assert history.attachments() == {}

    def test_write_transcript_persists_history(self, transcript_path):
        """write_transcript() writes current history to disk."""
        tc = AITranscriptConversation(transcript_path)
        _add_user_message(tc, "hello")
        tc.write_transcript()

        # Read back independently
        tc2 = AITranscriptConversation(transcript_path)
        history = tc2.read()
        assert len(history.get_messages()) == 1
        assert history.get_messages()[0].content == "hello"

    def test_conversation_settings_delegation(self, transcript_path):
        """conversation_settings() delegates to the inner AIConversation."""
        tc = AITranscriptConversation(transcript_path)
        settings = tc.conversation_settings()
        assert settings is not None

    def test_get_token_counts_delegation(self, transcript_path):
        """get_token_counts() delegates to the inner AIConversation."""
        tc = AITranscriptConversation(transcript_path)
        counts = tc.get_token_counts()
        assert isinstance(counts, dict)

    def test_is_streaming_false_when_idle(self, transcript_path):
        """is_streaming() returns False when not actively streaming."""
        tc = AITranscriptConversation(transcript_path)
        assert tc.is_streaming() is False


class TestAITranscriptConversationTruncate:
    """Tests for truncate_to_message on AITranscriptConversation."""

    def test_truncate_returns_content(self, transcript_path):
        """truncate_to_message returns the content of the removed message."""
        tc = AITranscriptConversation(transcript_path)
        _add_user_message(tc, "keep this")
        msg2 = _add_user_message(tc, "delete this")

        result = tc.truncate_to_message(msg2.id)
        assert result == "delete this"

    def test_truncate_removes_messages_from_history(self, transcript_path):
        """truncate_to_message removes the target and subsequent messages."""
        tc = AITranscriptConversation(transcript_path)
        _add_user_message(tc, "keep")
        _add_ai_message(tc, "keep reply")
        msg3 = _add_user_message(tc, "remove")
        _add_ai_message(tc, "remove reply")

        tc.truncate_to_message(msg3.id)

        messages = tc.get_conversation_history().get_messages()
        assert len(messages) == 2
        assert all(m.content in ("keep", "keep reply") for m in messages)

    def test_truncate_writes_transcript(self, transcript_path):
        """truncate_to_message persists the truncated history to disk."""
        tc = AITranscriptConversation(transcript_path)
        _add_user_message(tc, "keep")
        msg2 = _add_user_message(tc, "remove")
        tc.truncate_to_message(msg2.id)

        # Read back from disk independently
        with open(transcript_path, encoding="utf-8") as f:
            data = json.load(f)

        assert len(data["conversation"]) == 1
        assert data["conversation"][0]["content"] == "keep"

    def test_truncate_unknown_id_does_not_write(self, transcript_path):
        """truncate_to_message with unknown ID returns None and does not write."""
        tc = AITranscriptConversation(transcript_path)
        msg = _add_user_message(tc, "hello")
        tc.write_transcript()

        # Get file modification time
        mtime_before = os.path.getmtime(transcript_path)

        result = tc.truncate_to_message("no-such-id")
        assert result is None

        # File should not have been rewritten
        mtime_after = os.path.getmtime(transcript_path)
        assert mtime_after == mtime_before

    def test_truncate_preserves_attachments_for_remaining_messages(self, transcript_path):
        """Attachments referenced by preserved messages survive truncation on disk."""
        tc = AITranscriptConversation(transcript_path)
        history = tc.get_conversation_history()
        guid = history.add_attachment("# content", "blueprint.md", "blueprint")

        msg1 = AIMessage.create(AIMessageSource.USER, "with attachment", attachments=[guid])
        history.add_message(msg1)
        msg2 = AIMessage.create(AIMessageSource.USER, "without attachment")
        history.add_message(msg2)

        tc.truncate_to_message(msg2.id)

        # Reload from disk and verify attachment is present
        with open(transcript_path, encoding="utf-8") as f:
            data = json.load(f)

        assert guid in data["attachments"]
        assert data["attachments"][guid]["content"] == "# content"

    def test_truncate_prunes_attachments_for_removed_messages(self, transcript_path):
        """Attachments only referenced by removed messages are pruned from disk."""
        tc = AITranscriptConversation(transcript_path)
        history = tc.get_conversation_history()
        guid = history.add_attachment("removed content", "removed.py", "file")

        msg1 = AIMessage.create(AIMessageSource.USER, "no attachment")
        history.add_message(msg1)
        msg2 = AIMessage.create(AIMessageSource.USER, "with attachment", attachments=[guid])
        history.add_message(msg2)

        tc.truncate_to_message(msg2.id)

        with open(transcript_path, encoding="utf-8") as f:
            data = json.load(f)

        assert guid not in data["attachments"]


class TestAITranscriptConversationFork:
    """Tests for fork_history on AITranscriptConversation."""

    def test_fork_returns_aiconversationhistory(self, transcript_path):
        """fork_history returns an AIConversationHistory."""
        tc = AITranscriptConversation(transcript_path)
        _add_user_message(tc, "hello")
        forked = tc.fork_history(1)
        assert isinstance(forked, AIConversationHistory)

    def test_fork_contains_correct_messages(self, transcript_path):
        """fork_history returns messages up to the given index."""
        tc = AITranscriptConversation(transcript_path)
        _add_user_message(tc, "one")
        _add_ai_message(tc, "two")
        _add_user_message(tc, "three")

        forked = tc.fork_history(2)
        messages = forked.get_messages()
        assert len(messages) == 2
        assert messages[0].content == "one"
        assert messages[1].content == "two"

    def test_fork_does_not_modify_original(self, transcript_path):
        """fork_history does not change the original conversation."""
        tc = AITranscriptConversation(transcript_path)
        _add_user_message(tc, "one")
        _add_user_message(tc, "two")
        _add_user_message(tc, "three")

        tc.fork_history(1)
        assert len(tc.get_conversation_history().get_messages()) == 3

    def test_fork_includes_correct_attachments(self, transcript_path):
        """fork_history includes only attachments referenced by forked messages."""
        tc = AITranscriptConversation(transcript_path)
        history = tc.get_conversation_history()

        guid_in = history.add_attachment("in fork", "a.py", "file")
        guid_out = history.add_attachment("not in fork", "b.py", "file")

        msg1 = AIMessage.create(AIMessageSource.USER, "first", attachments=[guid_in])
        history.add_message(msg1)
        msg2 = AIMessage.create(AIMessageSource.USER, "second", attachments=[guid_out])
        history.add_message(msg2)

        forked = tc.fork_history(1)
        assert guid_in in forked.attachments()
        assert guid_out not in forked.attachments()

    def test_fork_zero_returns_empty_history(self, transcript_path):
        """fork_history(0) returns an empty history."""
        tc = AITranscriptConversation(transcript_path)
        _add_user_message(tc, "hello")
        forked = tc.fork_history(0)
        assert forked.get_messages() == []
        assert forked.attachments() == {}


class TestAITranscriptConversationSetHistory:
    """Tests for set_conversation_history on AITranscriptConversation."""

    def test_set_history_writes_to_disk(self, transcript_path, fork_path):
        """set_conversation_history writes the new history to the transcript file."""
        # Create a source conversation with some content
        source = AITranscriptConversation(transcript_path)
        _add_user_message(source, "original")
        forked_history = source.fork_history(1)

        # Apply to a new transcript conversation
        target = AITranscriptConversation(fork_path)
        target.set_conversation_history(forked_history)

        with open(fork_path, encoding="utf-8") as f:
            data = json.load(f)

        assert len(data["conversation"]) == 1
        assert data["conversation"][0]["content"] == "original"

    def test_set_history_with_attachments_persists_them(self, transcript_path, fork_path):
        """set_conversation_history preserves attachments in the written transcript."""
        source = AITranscriptConversation(transcript_path)
        history = source.get_conversation_history()
        guid = history.add_attachment("data", "data.py", "file")
        msg = AIMessage.create(AIMessageSource.USER, "hello", attachments=[guid])
        history.add_message(msg)
        forked = source.fork_history(1)

        target = AITranscriptConversation(fork_path)
        target.set_conversation_history(forked)

        with open(fork_path, encoding="utf-8") as f:
            data = json.load(f)

        assert guid in data["attachments"]

    def test_set_history_restores_attachments_in_memory(self, transcript_path, fork_path):
        """set_conversation_history makes attachments resolvable in the in-memory history."""
        source = AITranscriptConversation(transcript_path)
        history = source.get_conversation_history()
        guid = history.add_attachment("print('hi')", "script.py", "file")
        msg = AIMessage.create(AIMessageSource.USER, "review", attachments=[guid])
        history.add_message(msg)
        forked = source.fork_history(1)

        target = AITranscriptConversation(fork_path)
        target.set_conversation_history(forked)

        # The attachment must be resolvable in the target's in-memory history
        resolved = target.get_conversation_history().get_attachment(guid)
        assert resolved is not None
        assert resolved["content"] == "print('hi')"

    def test_set_history_attachment_available_for_request(self, transcript_path, fork_path):
        """After set_conversation_history, get_attachment_content_for_request works."""
        source = AITranscriptConversation(transcript_path)
        history = source.get_conversation_history()
        guid = history.add_attachment("# blueprint", "blueprint.md", "blueprint")
        msg = AIMessage.create(AIMessageSource.USER, "go", attachments=[guid])
        history.add_message(msg)
        forked = source.fork_history(1)

        target = AITranscriptConversation(fork_path)
        target.set_conversation_history(forked)

        result = target.get_conversation_history().get_attachment_content_for_request(guid)
        assert result is not None
        filename, content = result
        assert filename == "blueprint.md"
        assert content == "# blueprint"


class TestAITranscriptConversationRestart:
    """Tests that attachments survive a simulated application restart.

    These tests cover the scenario where a conversation is written to disk,
    the application restarts, and the conversation is reloaded from the
    transcript.  Attachment GUIDs in messages must still resolve after reload.
    """

    def test_attachments_survive_reload(self, transcript_path):
        """Attachments written to disk are resolvable after reloading the transcript."""
        # Write a conversation with an attachment
        tc = AITranscriptConversation(transcript_path)
        history = tc.get_conversation_history()
        guid = history.add_attachment("print('hello')", "script.py", "file")
        msg = AIMessage.create(AIMessageSource.USER, "review this", attachments=[guid])
        history.add_message(msg)
        tc.write_transcript()
        # Simulate restart: create a fresh instance from the same path
        reloaded = AITranscriptConversation(transcript_path)
        reloaded_history = reloaded.read()
        reloaded.load_history(reloaded_history)

        resolved = reloaded.get_conversation_history().get_attachment(guid)
        assert resolved is not None
        assert resolved["content"] == "print('hello')"

    def test_attachment_content_for_request_after_reload(self, transcript_path):
        """get_attachment_content_for_request works after reloading from disk."""
        tc = AITranscriptConversation(transcript_path)
        history = tc.get_conversation_history()
        guid = history.add_attachment("# blueprint content", "blueprint.md", "blueprint")
        msg = AIMessage.create(AIMessageSource.USER, "use this", attachments=[guid])
        history.add_message(msg)
        tc.write_transcript()

        reloaded = AITranscriptConversation(transcript_path)
        reloaded_history = reloaded.read()
        reloaded.load_history(reloaded_history)

        result = reloaded.get_conversation_history().get_attachment_content_for_request(guid)
        assert result is not None
        filename, content = result
        assert filename == "blueprint.md"
        assert content == "# blueprint content"
