"""Input widget that matches history message styling."""

from PySide6.QtCore import Signal, Qt, QMimeData, QRect
from PySide6.QtGui import QKeyEvent

from humbug.gui.message_widget import MessageWidget
from humbug.gui.chat_text_edit import ChatTextEdit


class LiveInputWidget(MessageWidget):
    """Widget for live message input that matches history message styling."""

    # Forward text cursor signals from the input area
    cursorPositionChanged = Signal()

    def __init__(self, parent=None):
        """Initialize the live input widget."""
        super().__init__(parent, is_input=True)

        # Set up the header initial text
        self.set_content("", "user")

        # Connect text cursor signals
        self._text_area.cursorPositionChanged.connect(self.cursorPositionChanged)

        # Initialize input history
        self._input_history = []
        self._history_index = -1
        self._current_index = ""

    def _create_text_area(self) -> ChatTextEdit:
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

    def keyPressEvent(self, event: QKeyEvent):
        """Handle special key events."""
#        print(f"event: {event.key()}, {event.modifiers()}")
        if event.key() == Qt.Key_J and event.modifiers() == Qt.ControlModifier:
            text = self._text_area.toPlainText().strip()
            if text:
                if text not in self._input_history:
                    self._input_history.append(text)
                self._history_index = -1
                self.clear()
            return

        if self._text_area.textCursor().atStart() and not self._text_area.textCursor().hasSelection():
            if event.key() == Qt.Key_Up and self._input_history:
                if self._history_index == -1:
                    self._current_index = self._text_area.toPlainText()
                self._history_index = min(len(self._input_history) - 1,
                                       self._history_index + 1)
                self._text_area.setPlainText(self._input_history[-self._history_index - 1])
                return

            if event.key() == Qt.Key_Down:
                if self._history_index > 0:
                    self._history_index -= 1
                    self._text_area.setPlainText(self._input_history[-self._history_index - 1])
                elif self._history_index == 0:
                    self._history_index = -1
                    self._text_area.setPlainText(self._current_index)
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
        cursor = QRect(text_cursor.x(), offset + text_cursor.y(), text_cursor. width(), text_cursor.height())
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
