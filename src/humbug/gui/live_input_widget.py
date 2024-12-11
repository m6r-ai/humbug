"""Input widget that matches history message styling."""

from PySide6.QtCore import Signal, Qt, QMimeData, QRect
from PySide6.QtGui import QKeyEvent

from humbug.gui.message_widget import MessageWidget
from humbug.gui.dynamic_text_edit import DynamicTextEdit


class LiveInputWidget(MessageWidget):
    """Widget for live message input that matches history message styling."""

    submitted = Signal(str)
    # Forward text cursor signals from the input area
    cursorPositionChanged = Signal()

    def __init__(self, parent=None):
        """Initialize the live input widget."""
        super().__init__(parent, is_input=True)

        # Set up the header initial text
        self.set_content("", "user")

        # Connect text cursor signals
        self.text_area.cursorPositionChanged.connect(self.cursorPositionChanged)

        # Initialize input history
        self.input_history = []
        self.history_index = -1
        self.current_input = ""

    def _create_text_area(self) -> DynamicTextEdit:
        """Create and configure the input text area."""
        text_area = super()._create_text_area()
        # Override paste behavior to strip formatting
        text_area.insertFromMimeData = self._insert_from_mime_data
        return text_area

    def _insert_from_mime_data(self, source: QMimeData) -> None:
        """Override default paste behavior to insert only plain text."""
        if source.hasText():
            cursor = self.text_area.textCursor()
            cursor.insertText(source.text())

    def keyPressEvent(self, event: QKeyEvent):
        """Handle special key events."""
        if event.key() == Qt.Key_J and event.modifiers() == Qt.ControlModifier:
            text = self.text_area.toPlainText().strip()
            if text:
                self.submitted.emit(text)
                if text not in self.input_history:
                    self.input_history.append(text)
                self.history_index = -1
                self.clear()
            return

        if self.text_area.textCursor().atStart() and not self.text_area.textCursor().hasSelection():
            if event.key() == Qt.Key_Up and self.input_history:
                if self.history_index == -1:
                    self.current_input = self.text_area.toPlainText()
                self.history_index = min(len(self.input_history) - 1,
                                       self.history_index + 1)
                self.text_area.setPlainText(self.input_history[-self.history_index - 1])
                return

            if event.key() == Qt.Key_Down:
                if self.history_index > 0:
                    self.history_index -= 1
                    self.text_area.setPlainText(self.input_history[-self.history_index - 1])
                elif self.history_index == 0:
                    self.history_index = -1
                    self.text_area.setPlainText(self.current_input)
                return

        super().keyPressEvent(event)

    # Delegate methods to text_area
    def clear(self):
        """Clear the input area."""
        self.text_area.clear()

    def toPlainText(self) -> str:
        """Get the current input text."""
        return self.text_area.toPlainText()

    def setPlainText(self, text: str):
        """Set the input text."""
        self.text_area.setPlainText(text)

    def cursorRect(self):
        """Get the cursor rectangle from the input area."""
        text_cursor = self.text_area.cursorRect()
        offset = self.header.height()
        cursor = QRect(text_cursor.x(), offset + text_cursor.y(), text_cursor. width(), text_cursor.height())
        return cursor

    def setFocus(self):
        """Set focus to the input area."""
        self.text_area.setFocus()

    def hasFocus(self) -> bool:
        """Check if the input area has focus."""
        return self.text_area.hasFocus()

    def document(self):
        """Get the document from the input area."""
        return self.text_area.document()

    def textCursor(self):
        """Get the text cursor from the input area."""
        return self.text_area.textCursor()

    def undo(self):
        """Undo the last edit operation."""
        self.text_area.undo()

    def redo(self):
        """Redo the last undone edit operation."""
        self.text_area.redo()

    def cut(self):
        """Cut selected text to clipboard."""
        self.text_area.cut()

    def copy(self):
        """Copy selected text to clipboard."""
        self.text_area.copy()

    def paste(self):
        """Paste text from clipboard."""
        self.text_area.paste()
