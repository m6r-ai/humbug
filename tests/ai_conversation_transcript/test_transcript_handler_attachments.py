"""Tests for AIConversationTranscriptHandler attachment persistence."""

import json
import tempfile
import os

import pytest

from ai import AIConversationHistory
from ai.ai_message import AIMessage, AIMessageSource
from ai_conversation_transcript import (
    AIConversationTranscriptHandler,
    AIConversationTranscriptFormatError
)


@pytest.fixture
def transcript_file():
    """Provide a temporary transcript file path, cleaned up after the test."""
    with tempfile.NamedTemporaryFile(suffix=".conv", delete=False) as f:
        path = f.name

    os.unlink(path)  # Remove so AIConversationTranscriptHandler initialises it fresh
    yield path

    if os.path.exists(path):
        os.unlink(path)


def _make_history_with_attachment() -> tuple[AIConversationHistory, str]:
    """Return a history with one user message that references one attachment."""
    history = AIConversationHistory()
    guid = history.add_attachment("print('hello')", "script.py", "file")
    msg = AIMessage.create(AIMessageSource.USER, "review this", attachments=[guid])
    history.add_message(msg)
    return history, guid


class TestTranscriptHandlerAttachments:
    """Tests for attachment read/write in AIConversationTranscriptHandler."""

    def test_write_and_read_attachment(self, transcript_file):
        """Attachments written to disk are faithfully restored on read."""
        handler = AIConversationTranscriptHandler(transcript_file)
        history, guid = _make_history_with_attachment()
        handler.write(history)

        restored = handler.read()
        attachment = restored.get_attachment(guid)
        assert attachment is not None
        assert attachment["filename"] == "script.py"
        assert attachment["type"] == "file"
        assert attachment["content"] == "print('hello')"

    def test_message_attachment_guids_survive_round_trip(self, transcript_file):
        """Message attachment GUID lists are preserved through write/read."""
        handler = AIConversationTranscriptHandler(transcript_file)
        history, guid = _make_history_with_attachment()
        handler.write(history)

        restored = handler.read()
        messages = restored.get_messages()
        assert len(messages) == 1
        assert messages[0].attachments == [guid]

    def test_multiple_attachments_round_trip(self, transcript_file):
        """Multiple attachments on multiple messages all survive round-trip."""
        handler = AIConversationTranscriptHandler(transcript_file)
        history = AIConversationHistory()
        guid1 = history.add_attachment("content1", "a.py", "file")
        guid2 = history.add_attachment("content2", "b.md", "blueprint")
        msg1 = AIMessage.create(AIMessageSource.USER, "first", attachments=[guid1])
        msg2 = AIMessage.create(AIMessageSource.USER, "second", attachments=[guid2])
        history.add_message(msg1)
        history.add_message(msg2)
        handler.write(history)

        restored = handler.read()
        assert restored.get_attachment(guid1)["filename"] == "a.py"
        assert restored.get_attachment(guid2)["filename"] == "b.md"
        assert restored.get_messages()[0].attachments == [guid1]
        assert restored.get_messages()[1].attachments == [guid2]

    def test_new_transcript_has_empty_attachments(self, transcript_file):
        """A freshly created transcript file has an empty attachments section."""
        handler = AIConversationTranscriptHandler(transcript_file)
        restored = handler.read()
        assert restored.attachments() == {}

    def test_attachments_section_present_in_written_file(self, transcript_file):
        """The written JSON file contains an 'attachments' key at the top level."""
        handler = AIConversationTranscriptHandler(transcript_file)
        history, _ = _make_history_with_attachment()
        handler.write(history)

        with open(transcript_file, encoding="utf-8") as f:
            data = json.load(f)

        assert "attachments" in data
        assert isinstance(data["attachments"], dict)

    def test_backwards_compatibility_no_attachments_key(self, transcript_file):
        """Old transcripts without an 'attachments' key load successfully with empty store."""
        old_format = {
            "metadata": {"version": "0.1", "parent": None},
            "conversation": [
                {
                    "id": "msg-1",
                    "timestamp": "2025-01-01T00:00:00+00:00",
                    "type": "user_message",
                    "content": "hello",
                    "completed": True
                }
            ]
        }
        with open(transcript_file, "w", encoding="utf-8") as f:
            json.dump(old_format, f)

        handler = AIConversationTranscriptHandler(transcript_file)
        history = handler.read()
        assert history.attachments() == {}
        assert len(history.get_messages()) == 1

    def test_invalid_attachments_not_dict_raises(self, transcript_file):
        """A transcript with attachments as a non-dict raises a format error."""
        bad_format = {
            "metadata": {"version": "0.1", "parent": None},
            "attachments": ["not", "a", "dict"],
            "conversation": []
        }
        with open(transcript_file, "w", encoding="utf-8") as f:
            json.dump(bad_format, f)

        handler = AIConversationTranscriptHandler(transcript_file)
        with pytest.raises(AIConversationTranscriptFormatError):
            handler.read()

    def test_attachment_missing_required_field_raises(self, transcript_file):
        """An attachment entry missing a required field raises a format error."""
        bad_format = {
            "metadata": {"version": "0.1", "parent": None},
            "attachments": {
                "guid-1": {"filename": "f.py", "type": "file"}  # missing 'content'
            },
            "conversation": []
        }
        with open(transcript_file, "w", encoding="utf-8") as f:
            json.dump(bad_format, f)

        handler = AIConversationTranscriptHandler(transcript_file)
        with pytest.raises(AIConversationTranscriptFormatError):
            handler.read()

    def test_attachment_not_dict_raises(self, transcript_file):
        """An attachment entry that is not a dict raises a format error."""
        bad_format = {
            "metadata": {"version": "0.1", "parent": None},
            "attachments": {
                "guid-1": "just a string"
            },
            "conversation": []
        }
        with open(transcript_file, "w", encoding="utf-8") as f:
            json.dump(bad_format, f)

        handler = AIConversationTranscriptHandler(transcript_file)
        with pytest.raises(AIConversationTranscriptFormatError):
            handler.read()

    def test_write_no_attachments_produces_empty_dict(self, transcript_file):
        """Writing a history with no attachments produces an empty attachments dict."""
        handler = AIConversationTranscriptHandler(transcript_file)
        history = AIConversationHistory()
        history.add_message(AIMessage.create(AIMessageSource.USER, "plain"))
        handler.write(history)

        with open(transcript_file, encoding="utf-8") as f:
            data = json.load(f)

        assert data["attachments"] == {}
