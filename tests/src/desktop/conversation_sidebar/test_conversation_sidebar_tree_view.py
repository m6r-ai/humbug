"""Tests for the tooltip text the Conversations tree view shows for its items."""

import json
import os

import pytest
from PySide6.QtCore import QModelIndex

# pylint: disable=wrong-import-position
from desktop.conversation_sidebar.conversation_sidebar_dag_model import ConversationSidebarDAGModel
from desktop.conversation_sidebar.conversation_sidebar_index import ConversationSidebarIndex
from desktop.conversation_sidebar.conversation_sidebar_tree_view import ConversationSidebarTreeView
from desktop.sidebar.conversation_prompt_preview import (
    PROMPT_PREVIEW_CHARACTERS,
    PROMPT_PREVIEW_LINE_CHARACTERS,
    PROMPT_PREVIEW_LINES,
)
from desktop.sidebar.sidebar_tree_icon_provider import SidebarTreeIconProvider
from mindspace.mindspace import Mindspace


def _relative_conversations_path(name: str) -> str:
    """Return the path of an item in the conversations directory, relative to the mindspace root."""
    return os.path.join(Mindspace.MINDSPACE_DIR, Mindspace.CONVERSATIONS_DIR, name)


def _message(message_id: str, message_type: str, content: str) -> dict:
    """Build a transcript message of the given type."""
    return {
        "id": message_id,
        "timestamp": "2024-01-01T00:00:00+00:00",
        "type": message_type,
        "content": content,
    }


def _write_conv_file(path: str, messages: list[dict]) -> None:
    """Write a root .conv file holding the given messages."""
    with open(path, 'w', encoding='utf-8') as f:
        json.dump({"metadata": {"version": "0.1", "parent": None}, "conversation": messages}, f)


@pytest.fixture
def conv_dir(mindspace_sandbox):
    """The sandboxed mindspace's conversations directory."""
    yield mindspace_sandbox.conversations_dir


@pytest.fixture
def make_view(conv_dir, qapp):
    """Factory for Conversations tree views over the conversations directory as it stands now."""
    widgets = []

    def _make() -> ConversationSidebarTreeView:
        index = ConversationSidebarIndex()
        index.set_conversations_dir(conv_dir)
        widget = ConversationSidebarTreeView()
        widget.setModel(ConversationSidebarDAGModel(index, SidebarTreeIconProvider()))
        widgets.append(widget)
        return widget

    yield _make

    for widget in widgets:
        widget.deleteLater()

    qapp.processEvents()


class TestTooltipText:
    """A conversation's tooltip shows the prompt that opened it, above its path."""

    def test_tooltip_shows_prompt_above_its_mindspace_relative_path(self, conv_dir, make_view):
        """The tooltip holds the opening prompt, then where the file sits in the mindspace."""
        chat = os.path.join(conv_dir, "chat.conv")
        _write_conv_file(chat, [_message("m1", "user_message", "Explain the build")])
        view = make_view()

        tooltip = view.get_tooltip_text(view.model().index_for_path(chat))

        assert tooltip == f"Explain the build\n\n{_relative_conversations_path('chat.conv')}"

    def test_prompt_whitespace_is_collapsed(self, conv_dir, make_view):
        """Newlines and repeated spaces in the prompt become single spaces."""
        chat = os.path.join(conv_dir, "chat.conv")
        _write_conv_file(chat, [_message("m1", "user_message", "Explain\n\nthe   build")])
        view = make_view()

        tooltip = view.get_tooltip_text(view.model().index_for_path(chat))

        assert tooltip.startswith("Explain the build\n\n")

    def test_long_prompt_is_wrapped_and_truncated(self, conv_dir, make_view):
        """An unbroken prompt fills whole preview lines and ends with an ellipsis."""
        prompt = "a" * (PROMPT_PREVIEW_CHARACTERS + 100)
        chat = os.path.join(conv_dir, "chat.conv")
        _write_conv_file(chat, [_message("m1", "user_message", prompt)])
        view = make_view()

        tooltip = view.get_tooltip_text(view.model().index_for_path(chat))

        line = "a" * PROMPT_PREVIEW_LINE_CHARACTERS
        expected_lines = [line] * PROMPT_PREVIEW_LINES
        expected_lines[-1] += "…"
        expected_preview = "\n".join(expected_lines)

        assert tooltip == f"{expected_preview}\n\n{_relative_conversations_path('chat.conv')}"

    def test_wrapping_never_exceeds_the_line_limit(self, conv_dir, make_view):
        """A prompt of short words is cut to the preview's line count, not just its character count."""
        prompt = "word " * 80
        chat = os.path.join(conv_dir, "chat.conv")
        _write_conv_file(chat, [_message("m1", "user_message", prompt)])
        view = make_view()

        tooltip = view.get_tooltip_text(view.model().index_for_path(chat))

        preview, path = tooltip.split("\n\n")
        lines = preview.split("\n")
        assert len(lines) == PROMPT_PREVIEW_LINES
        assert lines[-1].endswith("…")
        assert all(
            len(line) <= PROMPT_PREVIEW_LINE_CHARACTERS + 1
            for line in lines
        )
        assert path == _relative_conversations_path("chat.conv")

    def test_conversation_without_a_prompt_shows_only_its_path(self, conv_dir, make_view):
        """A conversation with no user message yet is described by its path alone."""
        chat = os.path.join(conv_dir, "chat.conv")
        _write_conv_file(chat, [])
        view = make_view()

        tooltip = view.get_tooltip_text(view.model().index_for_path(chat))

        assert tooltip == _relative_conversations_path("chat.conv")

    def test_folder_shows_only_its_mindspace_relative_path(self, conv_dir, make_view):
        """A folder has no prompt, so where it sits in the mindspace is the whole tooltip."""
        folder = os.path.join(conv_dir, "folder")
        os.makedirs(folder)
        view = make_view()

        tooltip = view.get_tooltip_text(view.model().index_for_path(folder))

        assert tooltip == _relative_conversations_path("folder")

    def test_no_tooltip_for_an_invalid_index(self, make_view):
        """An index that names no item produces no tooltip."""
        view = make_view()

        assert view.get_tooltip_text(QModelIndex()) == ""
