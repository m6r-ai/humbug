"""
Tests for how a loaded conversation is turned into message widgets.

History holds messages that are never shown — connection notices, the user-role wrappers
that carry tool results, and empty or placeholder responses.  They still occupy positions
in the display order, because tools and view state address messages by position, but they
are not given widgets.  These tests pin that, and pin that the operations which address
messages still work when a run of them has no widget.
"""

import os

import pytest

from PySide6.QtWidgets import QApplication

# pylint: disable=wrong-import-position
from ai import AIMessage, AIMessageSource
from ai_tool import AIToolResult
from ai_transcript_conversation import AITranscriptConversation
from desktop.conversation_tab.conversation_message import ConversationMessage
from desktop.conversation_tab.conversation_widget import ConversationWidget


def _write_history(transcript_path: str, messages: list[AIMessage]) -> None:
    """Write messages to a transcript file."""
    history = AITranscriptConversation(transcript_path).read()
    for message in messages:
        history.add_message(message)

    AITranscriptConversation(transcript_path).set_conversation_history(history)


def _make_widget(transcript_path: str) -> ConversationWidget:
    """Create a shown ConversationWidget backed by the transcript on disk."""
    transcript = AITranscriptConversation(transcript_path)
    transcript.set_conversation_history(AITranscriptConversation(transcript_path).read())

    widget = ConversationWidget(transcript_path, ai_transcript_conversation=transcript)
    widget.resize(1000, 800)
    widget.show()

    return widget


def _pump_until_loaded(widget: ConversationWidget, qapp: QApplication) -> None:
    """Process events until the conversation has finished loading and revealing."""
    quiet = 0
    for _ in range(20000):
        qapp.processEvents()
        loaded = (not widget._load_queue and not widget._reveal_queue  # pylint: disable=protected-access
                  and not widget._reveal_complete_pending)  # pylint: disable=protected-access
        quiet = quiet + 1 if loaded else 0
        if quiet > 5:
            return

    raise AssertionError("conversation did not settle")


def _tool_round_trip(prompt: str) -> list[AIMessage]:
    """Build the messages one tool round trip adds: call, result, carrier, connection."""
    call = AIMessage.create(AIMessageSource.TOOL_CALL, "reading a file")
    result = AIMessage.create(AIMessageSource.TOOL_RESULT, "file contents")
    carrier = AIMessage.create(
        AIMessageSource.USER, "", tool_results=[AIToolResult(id="r1", name="read_file", content="file contents")]
    )
    connected = AIMessage.create(AIMessageSource.AI_CONNECTED, "...")

    return [AIMessage.create(AIMessageSource.USER, prompt), call, result, carrier, connected]


@pytest.fixture
def transcript_path(mindspace_sandbox) -> str:
    """A path in the sandbox for the conversation transcript."""
    return os.path.join(mindspace_sandbox.conversations_dir, "test.conv")


class TestLoadedWidgets:
    def test_messages_that_are_never_shown_get_no_widget(self, transcript_path, qapp) -> None:
        """Connection notices, tool carriers and empty responses are not built."""
        messages = _tool_round_trip("a prompt")
        messages.append(AIMessage.create(AIMessageSource.AI, ""))
        _write_history(transcript_path, messages)

        widget = _make_widget(transcript_path)
        _pump_until_loaded(widget, qapp)

        sources = [message.message_source() for message in widget._ordered_widgets()]  # pylint: disable=protected-access
        assert AIMessageSource.AI_CONNECTED not in sources
        assert AIMessageSource.USER in sources
        assert len(widget._ordered_widgets()) == 3  # pylint: disable=protected-access

    def test_every_message_still_occupies_a_display_position(self, transcript_path, qapp) -> None:
        """The display order keeps one entry per transcript message, widget or not."""
        messages = _tool_round_trip("a prompt")
        _write_history(transcript_path, messages)

        widget = _make_widget(transcript_path)
        _pump_until_loaded(widget, qapp)

        assert widget._message_ids == [message.id for message in messages]  # pylint: disable=protected-access

    def test_a_user_message_with_only_attachments_is_shown(self, transcript_path, qapp) -> None:
        """An attachment-only prompt has something to display, so it keeps its widget."""
        _write_history(
            transcript_path,
            [AIMessage.create(AIMessageSource.USER, "", attachments=["attachment-id"])],
        )

        widget = _make_widget(transcript_path)
        _pump_until_loaded(widget, qapp)

        assert len(widget._ordered_widgets()) == 1  # pylint: disable=protected-access

    def test_a_streamed_connection_notice_is_still_shown(self, transcript_path, qapp) -> None:
        """A connection notice is displayed while a response is arriving."""
        _write_history(transcript_path, [AIMessage.create(AIMessageSource.USER, "a prompt")])

        widget = _make_widget(transcript_path)
        _pump_until_loaded(widget, qapp)

        widget._add_message(AIMessage.create(AIMessageSource.AI_CONNECTED, "..."))  # pylint: disable=protected-access
        qapp.processEvents()

        sources = [message.message_source() for message in widget._ordered_widgets()]  # pylint: disable=protected-access
        assert AIMessageSource.AI_CONNECTED in sources


class TestOperationsAcrossWidgetlessMessages:
    def test_forking_addresses_shown_messages_by_transcript_position(self, transcript_path, qapp) -> None:
        """Every shown message resolves to its own transcript position, gaps and all."""
        messages = _tool_round_trip("a prompt")
        messages.append(AIMessage.create(AIMessageSource.AI, "a response"))
        _write_history(transcript_path, messages)

        widget = _make_widget(transcript_path)
        _pump_until_loaded(widget, qapp)

        transcript_ids = [message.id for message in messages]
        for message in widget._ordered_widgets():  # pylint: disable=protected-access
            assert widget._history_index(message.message_id()) == transcript_ids.index(message.message_id())  # pylint: disable=protected-access

        assert widget._history_index(messages[-1].id) == len(messages) - 1  # pylint: disable=protected-access

    def test_deleting_from_a_message_removes_widgetless_entries(self, transcript_path, qapp) -> None:
        """Deleting from a message copes with the messages after it having no widget."""
        messages = _tool_round_trip("first prompt")
        messages.extend(_tool_round_trip("second prompt")[1:])
        _write_history(transcript_path, messages)

        widget = _make_widget(transcript_path)
        _pump_until_loaded(widget, qapp)

        before = len(widget._message_ids)  # pylint: disable=protected-access
        assert before == len(messages)

        index = widget._message_index(messages[-1].id)  # pylint: disable=protected-access
        widget._remove_message_widgets_from(index)  # pylint: disable=protected-access
        qapp.processEvents()

        expected = [message.id for message in messages[:-1]]
        assert widget._message_ids == expected  # pylint: disable=protected-access
        assert len(widget._message_ids) == before - 1  # pylint: disable=protected-access

    def test_editing_marks_every_later_message_for_removal(self, transcript_path, qapp) -> None:
        """A pending edit dims every message after the one being edited, gaps and all."""
        messages = _tool_round_trip("first prompt")
        messages.extend(_tool_round_trip("second prompt")[1:])
        _write_history(transcript_path, messages)

        widget = _make_widget(transcript_path)
        _pump_until_loaded(widget, qapp)

        widget._begin_pending_edit(messages[0].id, "first prompt", [])  # pylint: disable=protected-access

        shown = widget._ordered_widgets()  # pylint: disable=protected-access
        assert shown[0].is_being_edited()
        assert all(message.is_pending_removal() for message in shown[1:])

        widget._clear_pending_edit()  # pylint: disable=protected-access
        assert not any(message.is_pending_removal() for message in shown)

    def test_navigation_visits_every_shown_message_in_order(self, transcript_path, qapp) -> None:
        """Stepping through messages by position skips the ones with no widget."""
        messages = _tool_round_trip("first prompt")
        messages.extend(_tool_round_trip("second prompt")[1:])
        _write_history(transcript_path, messages)

        widget = _make_widget(transcript_path)
        _pump_until_loaded(widget, qapp)

        shown_positions = [
            index for index, message_id in enumerate(widget._message_ids)  # pylint: disable=protected-access
            if message_id in widget._message_widgets  # pylint: disable=protected-access
        ]
        assert len(shown_positions) < len(messages)

        visited = [widget._find_first_visible_message()]  # pylint: disable=protected-access
        while True:
            following = widget._find_next_visible_message(visited[-1])  # pylint: disable=protected-access
            if following < 0:
                break

            visited.append(following)

        assert visited == shown_positions
        assert widget._find_last_visible_message() == shown_positions[-1]  # pylint: disable=protected-access

        user_positions = [
            position for position in shown_positions
            if widget._widget_at(position).message_source() == AIMessageSource.USER  # pylint: disable=protected-access
        ]
        assert user_positions

        for position in shown_positions:
            expected = next((candidate for candidate in user_positions if candidate > position), -1)
            actual = widget._find_next_visible_user_message(position)  # pylint: disable=protected-access
            assert actual == expected

    def test_expansion_state_survives_messages_without_widgets(self, transcript_path, qapp) -> None:
        """Expansion state round trips by message id, not by position."""
        messages = _tool_round_trip("a prompt")
        messages.append(AIMessage.create(AIMessageSource.AI, "a response"))
        _write_history(transcript_path, messages)

        source = _make_widget(transcript_path)
        _pump_until_loaded(source, qapp)

        shown = source._ordered_widgets()  # pylint: disable=protected-access
        collapsed_id = shown[-1].message_id()
        source._change_message_expansion(collapsed_id, False)  # pylint: disable=protected-access
        state = source.create_view_state()

        assert len(state["message_expansion"]) == len(messages)
        assert state["message_ids"] == [message.id for message in messages]

        restored = _make_widget(transcript_path)
        _pump_until_loaded(restored, qapp)
        restored.restore_view_state(state)

        by_id = {
            widget.message_id(): widget.is_expanded()
            for widget in restored._ordered_widgets()  # pylint: disable=protected-access
        }
        assert by_id[collapsed_id] is False
        assert by_id[messages[0].id] is True

    def test_expansion_state_saved_without_ids_is_matched_by_position(self, transcript_path, qapp) -> None:
        """State written before ids were recorded still applies to the messages it names."""
        messages = _tool_round_trip("a prompt")
        messages.append(AIMessage.create(AIMessageSource.AI, "a response"))
        _write_history(transcript_path, messages)

        widget = _make_widget(transcript_path)
        _pump_until_loaded(widget, qapp)

        legacy_state = {"message_expansion": [True, True, True, True, True, False]}
        widget.restore_view_state(legacy_state)

        shown = widget._ordered_widgets()  # pylint: disable=protected-access
        assert shown[-1].is_expanded() is False
        assert all(message.is_expanded() for message in shown[:-1])


def test_loaded_widget_count_is_smaller_than_the_transcript(transcript_path, qapp) -> None:
    """A history full of tool round trips builds far fewer widgets than it holds messages."""
    messages: list[AIMessage] = []
    for index in range(20):
        messages.extend(_tool_round_trip(f"prompt {index}"))

    _write_history(transcript_path, messages)

    widget = _make_widget(transcript_path)
    _pump_until_loaded(widget, qapp)

    built = len(widget.findChildren(ConversationMessage))
    assert len(messages) == 100
    assert built == len(widget._ordered_widgets()) + 1  # pylint: disable=protected-access
    # Prompt, tool call and tool result are shown; the carrier and connection notice are not.
    assert built == 20 * 3 + 1
