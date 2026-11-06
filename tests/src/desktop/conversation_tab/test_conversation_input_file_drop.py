"""
Tests for a file dropped on the conversation input box.

A file dragged from a sidebar panel can be dropped anywhere in the tab area, where it
opens in a tab, or on the input box, where it is attached to the message being composed.
These tests pin the second of those.

Qt resolves the widget a live drag is delivered to, so the handlers are called here
directly rather than a drag being synthesized.  A drag event does not own its mime
data, so every event is built from a mime data object the test holds for as long as
the event lives.
"""

import os

import pytest

# pylint: disable=wrong-import-position
from PySide6.QtCore import QMimeData, QPoint, QPointF, Qt
from PySide6.QtGui import QColor, QDragEnterEvent, QDragLeaveEvent, QDropEvent

from ai_transcript_conversation import AITranscriptConversation
from desktop.color_role import ColorRole
from desktop.conversation_tab.conversation_widget import ConversationWidget
from desktop.drop_payload import PATH_FORMAT, SOURCE_FORMAT, journal_mime_data
from desktop.style_manager import StyleManager

# pylint: disable=protected-access


@pytest.fixture
def conversation(qapp, mindspace_sandbox):
    """A laid-out conversation widget holding an input box, and its mindspace root."""
    transcript_path = os.path.join(mindspace_sandbox.conversations_dir, "test.conv")
    transcript = AITranscriptConversation(transcript_path)
    widget = ConversationWidget(transcript_path, ai_transcript_conversation=transcript)
    widget.resize(1000, 700)
    widget.show()
    qapp.processEvents()

    yield widget, mindspace_sandbox.path

    widget.close_widget()
    widget.deleteLater()
    qapp.processEvents()


def _file_mime_data(path: str) -> QMimeData:
    """Drag data naming a path, as the sidebar views produce it."""
    mime_data = QMimeData()
    mime_data.setData(PATH_FORMAT, path.encode())
    mime_data.setData(SOURCE_FORMAT, b"files")
    return mime_data


def _drag_enter(mime_data: QMimeData) -> QDragEnterEvent:
    """A drag entering a widget."""
    return QDragEnterEvent(
        QPoint(5, 5),
        Qt.DropAction.CopyAction,
        mime_data,
        Qt.MouseButton.LeftButton,
        Qt.KeyboardModifier.NoModifier,
    )


def _drop(mime_data: QMimeData) -> QDropEvent:
    """A drop on a widget."""
    return QDropEvent(
        QPointF(5, 5),
        Qt.DropAction.CopyAction,
        mime_data,
        Qt.MouseButton.LeftButton,
        Qt.KeyboardModifier.NoModifier,
    )


def _write_file(directory: str, name: str, content: str) -> str:
    """Write a file in a directory and return its path."""
    path = os.path.join(directory, name)
    with open(path, "w", encoding="utf-8") as f:
        f.write(content)

    return path


def _top_border_colour(widget: ConversationWidget) -> QColor:
    """The colour painted along the top edge of the input box."""
    image = widget._input.grab().toImage()
    return image.pixelColor(image.width() // 2, 1)


class TestFileDropAttaches:
    """Tests for what a file dropped on the input box does."""

    def test_a_file_dropped_on_the_input_is_attached(self, conversation, qapp) -> None:
        """A dropped file becomes an attachment of the message being composed."""
        widget, mindspace_path = conversation
        mime_data = _file_mime_data(_write_file(mindspace_path, "notes.txt", "a note"))
        event = _drop(mime_data)

        widget._input.dropEvent(event)
        qapp.processEvents()

        assert event.isAccepted()
        assert widget._input.get_attachments() == [("notes.txt", "a note")]

    def test_a_file_dragged_over_the_text_area_is_attached(self, conversation, qapp) -> None:
        """The text area covers the input box, so a drop on it attaches the file too."""
        widget, mindspace_path = conversation
        mime_data = _file_mime_data(_write_file(mindspace_path, "notes.txt", "a note"))
        event = _drop(mime_data)

        handled = widget._input.eventFilter(widget._input.text_area(), event)
        qapp.processEvents()

        assert handled
        assert event.isAccepted()
        assert widget._input.get_attachments() == [("notes.txt", "a note")]

    def test_a_folder_is_refused(self, conversation, qapp) -> None:
        """Only a file has contents to attach, so a dropped folder is left alone."""
        widget, mindspace_path = conversation
        folder = os.path.join(mindspace_path, "a_folder")
        os.mkdir(folder)
        mime_data = _file_mime_data(folder)
        event = _drop(mime_data)

        widget._input.dropEvent(event)
        qapp.processEvents()

        assert not event.isAccepted()
        assert widget._input.get_attachments() == []

    def test_a_dropped_change_is_refused(self, conversation, qapp) -> None:
        """A drag naming a recorded change names no file, so it is not an attachment."""
        widget, _mindspace_path = conversation
        mime_data = journal_mime_data(1)
        event = _drop(mime_data)

        widget._input.dropEvent(event)
        qapp.processEvents()

        assert not event.isAccepted()
        assert widget._input.get_attachments() == []

    def test_a_dropped_humbug_file_is_refused(self, conversation, qapp) -> None:
        """A file under `.humbug` is Humbug's own state, which the AI never reads."""
        widget, mindspace_path = conversation
        mime_data = _file_mime_data(
            _write_file(os.path.join(mindspace_path, ".humbug"), "notes.txt", "internal state")
        )
        event = _drop(mime_data)

        widget._input.dropEvent(event)
        qapp.processEvents()

        assert not event.isAccepted()
        assert widget._input.get_attachments() == []


class TestDropHighlight:
    """Tests for the input showing itself as the place a dragged file will attach."""

    def test_the_input_is_highlighted_while_a_file_is_dragged_over_it(self, conversation, qapp) -> None:
        """The input's border takes the drop-target colour as a file is dragged onto it."""
        widget, mindspace_path = conversation
        mime_data = _file_mime_data(_write_file(mindspace_path, "notes.txt", "a note"))
        highlight = QColor(StyleManager().get_color_str(ColorRole.DROP_TARGET_HIGHLIGHT))

        widget._input.dragEnterEvent(_drag_enter(mime_data))
        qapp.processEvents()

        assert _top_border_colour(widget) == highlight

    def test_the_highlight_is_cleared_when_the_drag_leaves(self, conversation, qapp) -> None:
        """A drag that leaves the input leaves no highlight behind."""
        widget, mindspace_path = conversation
        mime_data = _file_mime_data(_write_file(mindspace_path, "notes.txt", "a note"))
        highlight = QColor(StyleManager().get_color_str(ColorRole.DROP_TARGET_HIGHLIGHT))
        widget._input.dragEnterEvent(_drag_enter(mime_data))
        qapp.processEvents()

        widget._input.dragLeaveEvent(QDragLeaveEvent())
        qapp.processEvents()

        assert _top_border_colour(widget) != highlight

    def test_a_dragged_file_leaves_no_highlight_once_dropped(self, conversation, qapp) -> None:
        """Once the file has been attached, the input no longer marks itself as a target."""
        widget, mindspace_path = conversation
        mime_data = _file_mime_data(_write_file(mindspace_path, "notes.txt", "a note"))
        highlight = QColor(StyleManager().get_color_str(ColorRole.DROP_TARGET_HIGHLIGHT))
        widget._input.dragEnterEvent(_drag_enter(mime_data))

        widget._input.dropEvent(_drop(mime_data))
        qapp.processEvents()

        assert _top_border_colour(widget) != highlight
