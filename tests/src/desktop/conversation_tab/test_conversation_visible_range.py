"""
Tests for the cached visible message range.

Scrolling used to walk every message in the conversation on every tick, which is what
made a long conversation scroll slowly.  The range is now cached and moved a message at
a time, so these tests pin the two things that makes it depend on: the range it reports
is the range a full scan would find, and scrolling alone does not rebuild the minimap's
markers.
"""

import os

import pytest

from PySide6.QtWidgets import QApplication

# pylint: disable=wrong-import-position
from ai import AIMessage, AIMessageSource
from ai_transcript_conversation import AITranscriptConversation
from desktop.conversation_tab.conversation_widget import ConversationWidget

MESSAGE_COUNT = 160


def _make_widget(transcript_path: str) -> ConversationWidget:
    """Create a shown ConversationWidget holding more messages than fit on screen."""
    history = AITranscriptConversation(transcript_path).read()
    for index in range(MESSAGE_COUNT):
        source = AIMessageSource.USER if index % 2 == 0 else AIMessageSource.AI
        history.add_message(AIMessage.create(source, f"message {index} " + "text " * 40))

    transcript = AITranscriptConversation(transcript_path)
    transcript.set_conversation_history(history)

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


def _scanned_range(widget: ConversationWidget) -> tuple[int, int]:
    """Return the visible range by walking every message."""
    scrollbar = widget.scroll_area().verticalScrollBar()
    top = scrollbar.value()
    bottom = top + widget.scroll_area().viewport().height()

    hits = [
        index for index, message in enumerate(widget._ordered_widgets())  # pylint: disable=protected-access
        if message.is_rendered() and message.y() + message.height() > top and message.y() < bottom
    ]

    return (hits[0], hits[-1]) if hits else (0, -1)


@pytest.fixture
def transcript_path(mindspace_sandbox) -> str:
    """A path in the sandbox for the conversation transcript."""
    return os.path.join(mindspace_sandbox.conversations_dir, "test.conv")


class TestVisibleMessageRange:
    def test_range_matches_a_full_scan_while_scrolling(self, transcript_path, qapp) -> None:
        """The cached range agrees with a full scan at every scroll position it reaches."""
        widget = _make_widget(transcript_path)
        _pump_until_loaded(widget, qapp)

        scrollbar = widget.scroll_area().verticalScrollBar()
        maximum = scrollbar.maximum()
        assert maximum > 0

        positions = [maximum * step // 16 for step in range(17)]
        positions += [maximum * step // 16 + 17 for step in range(16)]

        for position in positions:
            scrollbar.setValue(position)
            qapp.processEvents()

            assert widget._visible_message_range() == _scanned_range(widget)  # pylint: disable=protected-access

    def test_range_covers_every_message_touching_the_viewport(self, transcript_path, qapp) -> None:
        """No message overlapping the viewport falls outside the reported range."""
        widget = _make_widget(transcript_path)
        _pump_until_loaded(widget, qapp)

        scrollbar = widget.scroll_area().verticalScrollBar()
        scrollbar.setValue(scrollbar.maximum() // 2)
        qapp.processEvents()

        first, last = widget._visible_message_range()  # pylint: disable=protected-access
        scanned_first, scanned_last = _scanned_range(widget)

        assert (first, last) == (scanned_first, scanned_last)


class TestMinimapMarkers:
    def test_scrolling_does_not_rebuild_the_markers(self, transcript_path, qapp) -> None:
        """Markers are content-relative, so scrolling alone leaves them untouched."""
        widget = _make_widget(transcript_path)
        _pump_until_loaded(widget, qapp)

        markers = widget._minimap_markers  # pylint: disable=protected-access
        assert markers

        widget.scroll_area().verticalScrollBar().setValue(
            widget.scroll_area().verticalScrollBar().maximum() // 2
        )
        qapp.processEvents()

        assert widget._minimap_markers is markers  # pylint: disable=protected-access
