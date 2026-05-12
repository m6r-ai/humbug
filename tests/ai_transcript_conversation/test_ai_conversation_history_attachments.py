"""Tests for AIConversationHistory attachment store."""

import pytest

from ai import AIConversationHistory
from ai.ai_message import AIMessage, AIMessageSource


def _make_user_message(content: str = "hello", attachment_guids=None) -> AIMessage:
    msg = AIMessage.create(AIMessageSource.USER, content)
    msg.attachments = attachment_guids
    return msg


class TestAttachmentStore:
    """Tests for the attachment store on AIConversationHistory."""

    def test_add_attachment_returns_guid(self):
        """add_attachment returns a non-empty GUID string."""
        history = AIConversationHistory()
        guid = history.add_attachment("content", "file.py", "file")
        assert isinstance(guid, str)
        assert len(guid) > 0

    def test_add_attachment_returns_unique_guids(self):
        """Each call to add_attachment returns a different GUID."""
        history = AIConversationHistory()
        guid1 = history.add_attachment("content1", "a.py", "file")
        guid2 = history.add_attachment("content2", "b.py", "file")
        assert guid1 != guid2

    def test_get_attachment_returns_stored_data(self):
        """get_attachment returns the filename, type, and content stored."""
        history = AIConversationHistory()
        guid = history.add_attachment("hello world", "readme.md", "blueprint")
        result = history.get_attachment(guid)
        assert result is not None
        assert result["filename"] == "readme.md"
        assert result["type"] == "blueprint"
        assert result["content"] == "hello world"

    def test_get_attachment_unknown_guid_returns_none(self):
        """get_attachment returns None for an unknown GUID."""
        history = AIConversationHistory()
        assert history.get_attachment("no-such-guid") is None

    def test_attachments_returns_copy(self):
        """attachments() returns a copy — mutating it does not affect the store."""
        history = AIConversationHistory()
        guid = history.add_attachment("data", "f.txt", "file")
        copy = history.attachments()
        copy["injected"] = {"filename": "x", "type": "file", "content": "y"}
        assert "injected" not in history.attachments()
        assert guid in history.attachments()

    def test_clear_removes_attachments(self):
        """clear() empties the attachment store."""
        history = AIConversationHistory()
        history.add_attachment("data", "f.txt", "file")
        history.clear()
        assert history.attachments() == {}

    def test_restore_attachments_replaces_store(self):
        """restore_attachments replaces the existing store entirely."""
        history = AIConversationHistory()
        old_guid = history.add_attachment("old", "old.txt", "file")

        new_store = {
            "new-guid-1": {"filename": "new.py", "type": "file", "content": "new content"}
        }
        history.restore_attachments(new_store)

        assert history.get_attachment(old_guid) is None
        assert history.get_attachment("new-guid-1") is not None
        assert history.get_attachment("new-guid-1")["content"] == "new content"

    def test_restore_attachments_with_empty_dict_clears_store(self):
        """restore_attachments({}) leaves the store empty."""
        history = AIConversationHistory()
        history.add_attachment("data", "f.txt", "file")
        history.restore_attachments({})
        assert history.attachments() == {}

    def test_get_attachment_content_for_request_returns_tuple(self):
        """get_attachment_content_for_request returns (filename, content)."""
        history = AIConversationHistory()
        guid = history.add_attachment("print('hi')", "script.py", "file")
        result = history.get_attachment_content_for_request(guid)
        assert result is not None
        filename, content = result
        assert filename == "script.py"
        assert content == "print('hi')"

    def test_get_attachment_content_for_request_unknown_returns_none(self):
        """get_attachment_content_for_request returns None for unknown GUID."""
        history = AIConversationHistory()
        assert history.get_attachment_content_for_request("ghost") is None

    def test_constructor_accepts_attachments(self):
        """AIConversationHistory can be initialised with a pre-built attachment store."""
        store = {
            "guid-a": {"filename": "a.py", "type": "file", "content": "# a"}
        }
        history = AIConversationHistory(attachments=store)
        result = history.get_attachment("guid-a")
        assert result is not None
        assert result["content"] == "# a"


class TestAIMessageAttachments:
    """Tests for the attachments field on AIMessage."""

    def test_message_without_attachments_serialises_cleanly(self):
        """A message with no attachments omits the field from the transcript dict."""
        msg = AIMessage.create(AIMessageSource.USER, "hello")
        d = msg.to_transcript_dict()
        assert "attachments" not in d

    def test_message_with_attachments_serialises_guid_list(self):
        """A message with attachments serialises the GUID list."""
        msg = AIMessage.create(AIMessageSource.USER, "hello", attachments=["g1", "g2"])
        d = msg.to_transcript_dict()
        assert d["attachments"] == ["g1", "g2"]

    def test_round_trip_with_attachments(self):
        """A message with attachments survives a to_transcript_dict / from_transcript_dict round-trip."""
        original = AIMessage.create(AIMessageSource.USER, "test", attachments=["abc", "def"])
        d = original.to_transcript_dict()
        restored = AIMessage.from_transcript_dict(d)
        assert restored.attachments == ["abc", "def"]

    def test_round_trip_without_attachments(self):
        """A message without attachments restores with attachments=None."""
        original = AIMessage.create(AIMessageSource.USER, "test")
        d = original.to_transcript_dict()
        restored = AIMessage.from_transcript_dict(d)
        assert restored.attachments is None

    def test_copy_preserves_attachments(self):
        """copy() preserves the attachments list."""
        msg = AIMessage.create(AIMessageSource.USER, "x", attachments=["g1"])
        copy = msg.copy()
        assert copy.attachments == ["g1"]

    def test_copy_attachments_is_independent(self):
        """Mutating the copy's attachments list does not affect the original."""
        msg = AIMessage.create(AIMessageSource.USER, "x", attachments=["g1"])
        copy = msg.copy()
        copy.attachments.append("g2")
        assert msg.attachments == ["g1"]
