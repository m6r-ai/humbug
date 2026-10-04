"""Tests for editing from a message and deleting from a message."""

import os

import pytest

from ai import AIMessage, AIMessageSource
from ai_transcript_conversation import AITranscriptConversation
from desktop.conversation_tab.conversation_widget import ConversationWidget
from desktop.mindspace.mindspace_manager import MindspaceManager
from desktop.user.user_manager import UserManager


@pytest.fixture
def conv_env(qapp, tmp_path, monkeypatch):
    """Open a real mindspace in a sandboxed HOME and return (mindspace_manager, transcript_path)."""
    home_dir = tmp_path / "home"
    home_dir.mkdir()
    monkeypatch.setenv("HOME", str(home_dir))

    MindspaceManager._instance = None
    UserManager._instance = None

    mgr = MindspaceManager()
    mgr._home_config = str(tmp_path / "mindspace.json")  # pylint: disable=protected-access
    ms_path = str(tmp_path / "mindspace")
    mgr.create_mindspace(ms_path, [])
    mgr.open_mindspace(ms_path)

    transcript_path = os.path.join(mgr.mindspace().conversations_dir(), "test.conv")

    yield mgr, transcript_path

    MindspaceManager._instance = None
    UserManager._instance = None


def _make_widget(transcript_path: str) -> ConversationWidget:
    """Create a ConversationWidget backed by a fresh transcript."""
    transcript = AITranscriptConversation(transcript_path)
    return ConversationWidget(transcript_path, ai_transcript_conversation=transcript)


def _populate(widget: ConversationWidget) -> None:
    """Populate the conversation with two user prompts and two AI responses."""
    history = widget._ai_conversation.get_conversation_history()  # pylint: disable=protected-access
    history.add_message(AIMessage.create(AIMessageSource.USER, "first prompt"))
    history.add_message(AIMessage.create(AIMessageSource.AI, "first response"))
    history.add_message(AIMessage.create(AIMessageSource.USER, "second prompt"))
    history.add_message(AIMessage.create(AIMessageSource.AI, "second response"))
    widget._load_message_history(  # pylint: disable=protected-access
        history.get_messages(), True, attachments=history.attachments()
    )


def _user_widgets(widget: ConversationWidget) -> list:
    """Return the user message widgets in the conversation."""
    return [w for w in widget._messages if w.message_source() == AIMessageSource.USER]  # pylint: disable=protected-access


class TestEditFromHere:
    """Tests for editing from a message."""

    def test_begin_edit_loads_text_and_marks_later_messages(self, conv_env) -> None:
        """Editing from a message loads its text and marks later messages as pending removal."""
        _mgr, transcript_path = conv_env
        widget = _make_widget(transcript_path)
        _populate(widget)

        target = _user_widgets(widget)[1]
        target.edit_from_here_requested.emit()

        assert widget._input.to_plain_text() == "second prompt"  # pylint: disable=protected-access

        later = widget._messages[widget._messages.index(target) + 1:]  # pylint: disable=protected-access
        assert later
        assert all(w.is_pending_removal() for w in later)

    def test_begin_edit_does_not_remove_messages(self, conv_env) -> None:
        """Beginning an edit leaves the transcript untouched."""
        _mgr, transcript_path = conv_env
        widget = _make_widget(transcript_path)
        _populate(widget)

        before = len(widget._messages)  # pylint: disable=protected-access
        _user_widgets(widget)[1].edit_from_here_requested.emit()

        assert len(widget._messages) == before  # pylint: disable=protected-access
        assert len(widget._ai_conversation.get_conversation_history().get_messages()) == 4  # pylint: disable=protected-access

    def test_cancel_restores_messages(self, conv_env) -> None:
        """Cancelling an edit restores the messages marked for removal."""
        _mgr, transcript_path = conv_env
        widget = _make_widget(transcript_path)
        _populate(widget)

        target = _user_widgets(widget)[1]
        target.edit_from_here_requested.emit()
        widget._on_cancel_edit_requested()  # pylint: disable=protected-access

        assert not any(w.is_pending_removal() for w in widget._messages)  # pylint: disable=protected-access
        assert widget._input.to_plain_text() == ""  # pylint: disable=protected-access
        assert len(widget._ai_conversation.get_conversation_history().get_messages()) == 4  # pylint: disable=protected-access

    def test_clearing_input_keeps_edit_active(self, conv_env) -> None:
        """Emptying the input box keeps the edit active so the whole message can be replaced."""
        _mgr, transcript_path = conv_env
        widget = _make_widget(transcript_path)
        _populate(widget)

        target = _user_widgets(widget)[1]
        target.edit_from_here_requested.emit()
        widget._input.set_plain_text("")  # pylint: disable=protected-access

        assert widget._pending_edit_message_id is not None  # pylint: disable=protected-access
        assert any(w.is_pending_removal() for w in widget._messages)  # pylint: disable=protected-access

    def test_escape_cancels_edit(self, conv_env) -> None:
        """Pressing Esc while editing cancels the edit and restores the messages."""
        _mgr, transcript_path = conv_env
        widget = _make_widget(transcript_path)
        _populate(widget)

        target = _user_widgets(widget)[1]
        target.edit_from_here_requested.emit()

        handled = widget.handle_esc_key()

        assert handled
        assert widget._pending_edit_message_id is None  # pylint: disable=protected-access
        assert not any(w.is_pending_removal() for w in widget._messages)  # pylint: disable=protected-access

    def test_edited_message_is_marked(self, conv_env) -> None:
        """The message being edited is marked as such and unmarked when the edit ends."""
        _mgr, transcript_path = conv_env
        widget = _make_widget(transcript_path)
        _populate(widget)

        target = _user_widgets(widget)[1]
        target.edit_from_here_requested.emit()

        assert target.is_being_edited()
        assert not any(w.is_being_edited() for w in widget._messages if w is not target)  # pylint: disable=protected-access

        widget._on_cancel_edit_requested()  # pylint: disable=protected-access

        assert not any(w.is_being_edited() for w in widget._messages)  # pylint: disable=protected-access

    def test_begin_edit_does_not_scroll(self, conv_env) -> None:
        """Beginning an edit leaves the scroll position untouched."""
        _mgr, transcript_path = conv_env
        widget = _make_widget(transcript_path)
        _populate(widget)

        scrollbar = widget._scroll_area.verticalScrollBar()  # pylint: disable=protected-access
        scrollbar.setValue(scrollbar.maximum())
        widget._auto_scroll = False  # pylint: disable=protected-access
        before = scrollbar.value()

        _user_widgets(widget)[1].edit_from_here_requested.emit()

        assert scrollbar.value() == before
        assert widget._auto_scroll is False  # pylint: disable=protected-access

    def test_submit_truncates_and_sends_revised_text(self, conv_env) -> None:
        """Submitting a pending edit removes the edited message and all later messages."""
        _mgr, transcript_path = conv_env
        widget = _make_widget(transcript_path)
        _populate(widget)

        target = _user_widgets(widget)[1]
        target.edit_from_here_requested.emit()
        widget._input.set_plain_text("second prompt revised")  # pylint: disable=protected-access

        widget._commit_pending_edit()  # pylint: disable=protected-access

        messages = widget._ai_conversation.get_conversation_history().get_messages()  # pylint: disable=protected-access
        assert [m.content for m in messages] == ["first prompt", "first response"]
        assert not any(w.is_pending_removal() for w in widget._messages)  # pylint: disable=protected-access

    def test_retargeting_edit_moves_pending_removal(self, conv_env) -> None:
        """Editing from a different message moves the pending-removal marking."""
        _mgr, transcript_path = conv_env
        widget = _make_widget(transcript_path)
        _populate(widget)

        users = _user_widgets(widget)
        users[1].edit_from_here_requested.emit()
        first_target = users[0]
        first_target.edit_from_here_requested.emit()

        assert widget._input.to_plain_text() == "first prompt"  # pylint: disable=protected-access
        assert not any(w.is_pending_removal() for w in widget._messages[:1])  # pylint: disable=protected-access
        assert all(w.is_pending_removal() for w in widget._messages[1:])  # pylint: disable=protected-access


class TestDeleteFromHere:
    """Tests for deleting from a message."""

    def test_delete_removes_message_and_later_messages(self, conv_env) -> None:
        """Deleting from a message removes it and everything after it."""
        _mgr, transcript_path = conv_env
        widget = _make_widget(transcript_path)
        _populate(widget)

        target = _user_widgets(widget)[1]
        target.delete_requested.emit()

        messages = widget._ai_conversation.get_conversation_history().get_messages()  # pylint: disable=protected-access
        assert [m.content for m in messages] == ["first prompt", "first response"]

    def test_delete_does_not_restore_text_to_input(self, conv_env) -> None:
        """Deleting from a message does not load its text into the input box."""
        _mgr, transcript_path = conv_env
        widget = _make_widget(transcript_path)
        _populate(widget)

        _user_widgets(widget)[1].delete_requested.emit()

        assert widget._input.to_plain_text() == ""  # pylint: disable=protected-access

    def test_delete_supersedes_pending_edit(self, conv_env) -> None:
        """Deleting while a pending edit is active abandons the edit."""
        _mgr, transcript_path = conv_env
        widget = _make_widget(transcript_path)
        _populate(widget)

        users = _user_widgets(widget)
        users[1].edit_from_here_requested.emit()
        users[1].delete_requested.emit()

        assert widget._pending_edit_message_id is None  # pylint: disable=protected-access
        assert not any(w.is_pending_removal() for w in widget._messages)  # pylint: disable=protected-access
