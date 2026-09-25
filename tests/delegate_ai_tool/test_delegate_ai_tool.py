"""Tests for the delegate AI tool's session ownership checks."""

import json
import os

import pytest

from ai_tool import AIToolExecutionError
from delegate_ai_tool import DelegateAITool
from mindspace.mindspace import Mindspace
from mindspace.mindspace_settings import MindspaceSettings


def _make_mindspace(tmp_path, monkeypatch) -> Mindspace:
    """Construct a Mindspace with an open mindspace at a temp path."""
    mindspace = Mindspace(
        on_settings_changed=lambda: None,
        on_interactions_updated=lambda: None,
    )
    path = os.path.join(str(tmp_path), "ms")
    os.makedirs(os.path.join(path, ".humbug"))
    monkeypatch.setattr(mindspace, "_path", path)
    monkeypatch.setattr(mindspace, "_settings", MindspaceSettings(enabled_tools={}))
    return mindspace


def _write_conv(path: str, message_ids: list[str], parent: dict | None = None) -> None:
    """Write a minimal .conv file with the given message IDs and parent metadata."""
    data = {
        "metadata": {"version": "0.1", "parent": parent},
        "conversation": [{"id": mid, "source": "user"} for mid in message_ids],
    }
    with open(path, "w", encoding="utf-8") as f:
        json.dump(data, f)


class _FakeConversation:
    """Minimal stand-in for AIConversation exposing a conversation identity."""

    def __init__(self, conversation_id: str) -> None:
        self._conversation_id = conversation_id

    def conversation_id(self) -> str:
        return self._conversation_id


class TestResolveSessionPath:
    """Tests for _resolve_session_path ownership enforcement."""

    def test_missing_session_is_rejected(self, tmp_path, monkeypatch):
        """An empty session_id is rejected."""
        tool = DelegateAITool(_make_mindspace(tmp_path, monkeypatch))

        with pytest.raises(AIToolExecutionError, match="must not be empty"):
            tool._resolve_session_path("", _FakeConversation(""))  # pylint: disable=protected-access

    def test_path_traversal_is_rejected(self, tmp_path, monkeypatch):
        """A session_id attempting path traversal is rejected."""
        tool = DelegateAITool(_make_mindspace(tmp_path, monkeypatch))

        with pytest.raises(AIToolExecutionError, match="path traversal"):
            tool._resolve_session_path("../secret.conv", _FakeConversation(""))  # pylint: disable=protected-access

    def test_nonexistent_file_is_rejected(self, tmp_path, monkeypatch):
        """A session_id that does not refer to an existing file is rejected."""
        tool = DelegateAITool(_make_mindspace(tmp_path, monkeypatch))

        with pytest.raises(AIToolExecutionError, match="does not refer to an existing file"):
            tool._resolve_session_path(  # pylint: disable=protected-access
                ".humbug/conversations/missing.conv", _FakeConversation("")
            )

    def test_session_delegated_by_requester_is_accepted(self, tmp_path, monkeypatch):
        """A session delegated by the requesting conversation is accepted."""
        mindspace = _make_mindspace(tmp_path, monkeypatch)
        conv_dir = mindspace.conversations_dir()
        parent_path = os.path.join(conv_dir, "parent.conv")
        child_path = os.path.join(conv_dir, "child.conv")
        _write_conv(parent_path, ["m1", "m2"])
        _write_conv(child_path, ["m3"], parent={"message_id": "m2", "tool_call_id": "tc1"})

        tool = DelegateAITool(mindspace)
        requester = _FakeConversation(parent_path)

        resolved = tool._resolve_session_path(  # pylint: disable=protected-access
            ".humbug/conversations/child.conv", requester
        )

        assert resolved == child_path

    def test_session_delegated_by_another_conversation_is_rejected(self, tmp_path, monkeypatch):
        """A session delegated by a different conversation is rejected."""
        mindspace = _make_mindspace(tmp_path, monkeypatch)
        conv_dir = mindspace.conversations_dir()
        parent_path = os.path.join(conv_dir, "parent.conv")
        other_path = os.path.join(conv_dir, "other.conv")
        child_path = os.path.join(conv_dir, "child.conv")
        _write_conv(parent_path, ["m1", "m2"])
        _write_conv(other_path, ["m7"])
        _write_conv(child_path, ["m3"], parent={"message_id": "m2", "tool_call_id": "tc1"})

        tool = DelegateAITool(mindspace)
        requester = _FakeConversation(other_path)

        with pytest.raises(AIToolExecutionError, match="was not delegated by this conversation"):
            tool._resolve_session_path(  # pylint: disable=protected-access
                ".humbug/conversations/child.conv", requester
            )

    def test_root_conversation_is_rejected(self, tmp_path, monkeypatch):
        """A session with no delegation parent is rejected even if it exists."""
        mindspace = _make_mindspace(tmp_path, monkeypatch)
        conv_dir = mindspace.conversations_dir()
        root_path = os.path.join(conv_dir, "root.conv")
        requester_path = os.path.join(conv_dir, "requester.conv")
        _write_conv(root_path, ["m1"])
        _write_conv(requester_path, ["m2"])

        tool = DelegateAITool(mindspace)
        requester = _FakeConversation(requester_path)

        with pytest.raises(AIToolExecutionError, match="was not delegated by this conversation"):
            tool._resolve_session_path(  # pylint: disable=protected-access
                ".humbug/conversations/root.conv", requester
            )

    def test_requester_without_identity_is_rejected(self, tmp_path, monkeypatch):
        """A requester with no conversation identity cannot resume any session."""
        mindspace = _make_mindspace(tmp_path, monkeypatch)
        conv_dir = mindspace.conversations_dir()
        parent_path = os.path.join(conv_dir, "parent.conv")
        child_path = os.path.join(conv_dir, "child.conv")
        _write_conv(parent_path, ["m1", "m2"])
        _write_conv(child_path, ["m3"], parent={"message_id": "m2", "tool_call_id": "tc1"})

        tool = DelegateAITool(mindspace)
        requester = _FakeConversation("")

        with pytest.raises(AIToolExecutionError, match="was not delegated by this conversation"):
            tool._resolve_session_path(  # pylint: disable=protected-access
                ".humbug/conversations/child.conv", requester
            )
