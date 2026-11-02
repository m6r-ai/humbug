"""Tests for navigating, locating and searching within a conversation view."""

import os

import pytest

# pylint: disable=wrong-import-position
from ai import AIMessage, AIMessageSource
from ai_transcript_conversation import AITranscriptConversation
from desktop.conversation_tab.conversation_widget import ConversationWidget

# pylint: disable=protected-access


@pytest.fixture
def conversation(qapp, mindspace_sandbox):
    """A laid-out conversation widget holding two prompts and a response to each."""
    transcript_path = os.path.join(mindspace_sandbox.conversations_dir, "test.conv")
    transcript = AITranscriptConversation(transcript_path)
    widget = ConversationWidget(transcript_path, ai_transcript_conversation=transcript)

    history = widget._ai_conversation.get_conversation_history()
    messages = [
        AIMessage.create(AIMessageSource.USER, "first prompt"),
        AIMessage.create(AIMessageSource.AI, "first response"),
        AIMessage.create(AIMessageSource.USER, "second prompt"),
        AIMessage.create(AIMessageSource.AI, "second response"),
    ]

    for message in messages:
        history.add_message(message)

    widget._load_message_history(
        history.get_messages(), True, attachments=history.attachments()
    )
    widget.resize(1000, 700)
    widget.show()
    qapp.processEvents()

    yield widget, messages

    widget.close_widget()
    widget.deleteLater()
    qapp.processEvents()


def _markers(widget: ConversationWidget) -> list[tuple[int, float, str]]:
    """Return the minimap's markers after refreshing them from the current layout."""
    widget._update_prompt_minimap()
    return widget._prompt_minimap._markers


class TestPromptMinimap:
    """Tests for the markers the prompt minimap draws."""

    def test_a_marker_is_drawn_for_each_prompt(self, conversation) -> None:
        """Every user prompt in the conversation gets one marker."""
        widget, _messages = conversation

        markers = _markers(widget)

        assert [marker[2] for marker in markers] == ["first prompt", "second prompt"]

    def test_markers_identify_the_message_they_belong_to(self, conversation) -> None:
        """Each marker carries the position of the prompt it represents."""
        widget, messages = conversation

        markers = _markers(widget)

        assert [marker[0] for marker in markers] == [0, 2]
        assert [messages[marker[0]].content for marker in markers] == [
            "first prompt",
            "second prompt",
        ]

    def test_markers_are_ordered_down_the_conversation(self, conversation) -> None:
        """Markers run top to bottom in conversation order."""
        widget, _messages = conversation

        positions = [marker[1] for marker in _markers(widget)]

        assert positions == sorted(positions)


class TestPromptNavigation:
    """Tests for navigating to a prompt by its position in the conversation."""

    def test_navigating_to_a_prompt_spotlights_it(self, conversation) -> None:
        """Navigating to a prompt spotlights that prompt and no other message."""
        widget, _messages = conversation

        widget.navigate_to_prompt(2)

        assert widget._spotlighted_message_index == 2

        widgets = widget._ordered_widgets()
        assert widgets[2].is_spotlighted()
        assert not any(
            message.is_spotlighted()
            for message in widgets
            if message is not widgets[2]
        )

    def test_navigating_to_a_non_prompt_does_nothing(self, conversation) -> None:
        """A position that does not hold a user prompt is not navigable."""
        widget, _messages = conversation

        widget.navigate_to_prompt(1)

        assert widget._spotlighted_message_index == -1
        assert not any(message.is_spotlighted() for message in widget._ordered_widgets())


class TestScrollToMessage:
    """Tests for scrolling the view to a message."""

    def test_scrolling_to_a_message_by_id_reaches_that_message(self, conversation) -> None:
        """A message id resolves to the message it identifies."""
        widget, messages = conversation

        result = widget.scroll_to_message_by_id_or_index(message_id=messages[3].id)

        assert result["success"]
        assert result["actual_index"] == 3
        assert result["message_id"] == messages[3].id
        assert not result["substituted"]

    def test_scrolling_to_a_message_by_index_reaches_that_message(self, conversation) -> None:
        """A position in the conversation resolves to the message at that position."""
        widget, messages = conversation

        result = widget.scroll_to_message_by_id_or_index(message_index=2)

        assert result["success"]
        assert result["target_index"] == 2
        assert result["actual_index"] == 2
        assert result["message_id"] == messages[2].id

    def test_an_unknown_message_id_is_rejected(self, conversation) -> None:
        """A message id that is not in the conversation cannot be scrolled to."""
        widget, _messages = conversation

        result = widget.scroll_to_message_by_id_or_index(message_id="not-a-real-id")

        assert not result["success"]
        assert "not-a-real-id" in result["error"]

    def test_a_position_beyond_the_conversation_is_rejected(self, conversation) -> None:
        """A position past the end of the conversation is not a message position."""
        widget, _messages = conversation

        result = widget.scroll_to_message_by_id_or_index(message_index=99)

        assert not result["success"]
        assert "99" in result["error"]


class TestFindWithinConversation:
    """Tests for searching the conversation's messages."""

    def test_matches_are_found_across_messages(self, conversation) -> None:
        """A search reports matches from every message that contains one."""
        widget, _messages = conversation

        current, total, truncated = widget.find_text("prompt")

        assert total == 2
        assert current == 1
        assert not truncated

    def test_a_match_anywhere_in_the_conversation_is_found(self, conversation) -> None:
        """A message in the middle of the conversation is searched, not just the ends."""
        widget, _messages = conversation

        _current, total, _truncated = widget.find_text("second")

        assert total == 2

    def test_match_status_advances_through_the_matches(self, conversation) -> None:
        """Stepping forward moves through each match in turn."""
        widget, _messages = conversation

        widget.find_text("prompt")
        current, total, _truncated = widget.find_text("prompt", forward=True)

        assert (current, total) == (2, 2)
