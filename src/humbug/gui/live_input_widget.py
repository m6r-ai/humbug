"""Input widget that matches history message styling."""

import sys

from PySide6.QtCore import Signal, Qt, QMimeData, QRect, QEvent
from PySide6.QtGui import QKeyEvent

from humbug.gui.message_widget import MessageWidget
from humbug.gui.conversation_text_edit import ConversationTextEdit
from humbug.gui.color_role import ColorRole
from humbug.language.language_manager import LanguageManager


class LiveInputWidget(MessageWidget):
    """Widget for live message input that matches history message styling."""

    # Forward text cursor signals from the input area
    cursorPositionChanged = Signal()
    pageScrollRequested = Signal()
    cancelRequested = Signal()

    def __init__(self, parent=None):
        """Initialize the live input widget."""
        self._is_streaming = False
        super().__init__(parent, is_input=True)

        # Connect text cursor signals
        self._text_area.cursorPositionChanged.connect(self.cursorPositionChanged)
        self._text_area.pageScrollRequested.connect(self.pageScrollRequested)

        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._handle_language_changed)

        self._update_header_text()

        # Install an event filter so we can capture clicks anywhere and redirect them to the input box
        self.installEventFilter(self)

    def _handle_language_changed(self):
        self._update_header_text()

    def set_streaming(self, streaming: bool):
        """Update the streaming state and header text."""
        self._is_streaming = streaming
        self._update_header_text()

    def _get_submit_key_text(self):
        """Get the appropriate submit key text based on the platform."""
        if sys.platform == "darwin":
            return "⌘J"

        return "Ctrl+J"

    def _update_header_text(self):
        """Update the header text based on current state."""
        print("update header text")
        strings = self._language_manager.strings
        if self._is_streaming:
            print("streaming")
            self._role_label.setText(strings.processing_message)
        else:
            print("not streaming")
            submit_key = self._get_submit_key_text()
            self._role_label.setText(strings.input_prompt.format(key=submit_key))

        self._set_role_style()

    def _set_role_style(self):
        """Set the role label color."""
        colour = ColorRole.TEXT_DISABLED if self._is_streaming else ColorRole.MESSAGE_USER

        # WARNING: This needs to stay in sync with MessageWidget
        self._role_label.setStyleSheet(f"""
            QLabel {{
                font-weight: bold;
                color: {self._style_manager.get_color_str(colour)};
                margin: 0;
                padding: 0;
                background-color: {self._style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
            }}
        """)

    def _create_text_area(self) -> ConversationTextEdit:
        """Create and configure the input text area."""
        text_area = super()._create_text_area()
        # Override paste behavior to strip formatting
        text_area.insertFromMimeData = self._insert_from_mime_data
        return text_area

    def _insert_from_mime_data(self, source: QMimeData) -> None:
        """Override default paste behavior to insert only plain text."""
        if source.hasText():
            cursor = self._text_area.textCursor()
            cursor.insertText(source.text())

    def eventFilter(self, obj, event) -> bool:
        """Handle window activation and mouse events to redirect focus to our input box."""
        if event.type() in (QEvent.MouseButtonPress, QEvent.FocusIn):
            self._text_area.setFocus()
            return False  # Don't consume the event

        return super().eventFilter(obj, event)

    def keyPressEvent(self, event: QKeyEvent):
        """Handle special key events."""
        if event.key() == Qt.Key_J and event.modifiers() == Qt.ControlModifier:
            # Just emit the signal - let parent decide what to do
            self.cancelRequested.emit()
            event.accept()
            return

        super().keyPressEvent(event)

    def clear(self):
        """Clear the input area."""
        self._text_area.clear()

    def toPlainText(self) -> str:
        """Get the current input text."""
        return self._text_area.toPlainText()

    def setPlainText(self, text: str):
        """Set the input text."""
        self._text_area.setPlainText(text)

    def cursorRect(self):
        """Get the cursor rectangle from the input area."""
        text_cursor = self._text_area.cursorRect()
        offset = self._header.height()
        cursor = QRect(text_cursor.x(), offset + text_cursor.y(), text_cursor.width(), text_cursor.height())
        return cursor

    def setFocus(self):
        """Set focus to the input area."""
        self._text_area.setFocus()

    def hasFocus(self) -> bool:
        """Check if the input area has focus."""
        return self._text_area.hasFocus()

    def document(self):
        """Get the document from the input area."""
        return self._text_area.document()

    def textCursor(self):
        """Get the text cursor from the input area."""
        return self._text_area.textCursor()

    def undo(self):
        """Undo the last edit operation."""
        self._text_area.undo()

    def redo(self):
        """Redo the last undone edit operation."""
        self._text_area.redo()

    def cut(self):
        """Cut selected text to clipboard."""
        self._text_area.cut()

    def copy(self):
        """Copy selected text to clipboard."""
        self._text_area.copy()

    def paste(self):
        """Paste text from clipboard."""
        self._text_area.paste()
