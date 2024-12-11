"""Input widget that matches history message styling."""

from PySide6.QtWidgets import QWidget, QVBoxLayout
from PySide6.QtCore import Signal, Qt, QMimeData
from PySide6.QtGui import QKeyEvent

from humbug.gui.message_widget import MessageWidget
from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager


class LiveInputWidget(QWidget):
    """Widget for live message input that matches history message styling."""

    submitted = Signal(str)
    # Forward text cursor signals from the input area
    cursorPositionChanged = Signal()

    def __init__(self, parent=None):
        """Initialize the live input widget."""
        super().__init__(parent)
        self.setContentsMargins(10, 0, 10, 10)  # Match history margin but no top margin

        # Create layout
        layout = QVBoxLayout(self)
        layout.setSpacing(0)
        layout.setContentsMargins(0, 0, 0, 0)

        # Create message widget for input
        self.message_widget = MessageWidget(self)

        # Set up the header and initial styling
        self.message_widget.header.setText("You")

        # Get the text area from message widget and make it editable
        self.input_area = self.message_widget.text_area
        self.input_area.setReadOnly(False)

        # Connect text cursor signals
        self.input_area.cursorPositionChanged.connect(self.cursorPositionChanged)

        # Initialize styling
        self._setup_styling()

        # Override paste behavior to strip formatting
        self.input_area.insertFromMimeData = self._insert_from_mime_data

        # Initialize input history
        self.input_history = []
        self.history_index = -1
        self.current_input = ""

        layout.addWidget(self.message_widget)

    def _setup_styling(self):
        """Set up the initial styling for the input area."""
        style_manager = StyleManager()

        # Style the header
        self.message_widget.header.setStyleSheet(f"""
            QLabel {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_HEADER)};
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
            }}
        """)

        # Style the content area
        content_color = style_manager.get_color_str(ColorRole.MESSAGE_USER)
        self.input_area.setStyleSheet(f"""
            QTextEdit {{
                background-color: {content_color};
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                selection-background-color: {style_manager.get_color_str(ColorRole.SELECTED_TEXT)};
                border: none;
                padding: 8px 8px 8px 8px;
            }}
        """)

        # Style the frame
        self.message_widget.setStyleSheet(f"""
            QFrame {{
                border: 1px solid {style_manager.get_color_str(ColorRole.MESSAGE_HEADER)};
                margin: 0;
            }}
        """)

    def _insert_from_mime_data(self, source: QMimeData) -> None:
        """Override default paste behavior to insert only plain text."""
        if source.hasText():
            cursor = self.input_area.textCursor()
            cursor.insertText(source.text())

    def keyPressEvent(self, event: QKeyEvent):
        """Handle special key events."""
        if event.key() == Qt.Key_J and event.modifiers() == Qt.ControlModifier:
            text = self.input_area.toPlainText().strip()
            if text:
                self.submitted.emit(text)
                if text not in self.input_history:
                    self.input_history.append(text)
                self.history_index = -1
                self.clear()
            return

        if self.input_area.textCursor().atStart() and not self.input_area.textCursor().hasSelection():
            if event.key() == Qt.Key_Up and self.input_history:
                if self.history_index == -1:
                    self.current_input = self.input_area.toPlainText()
                self.history_index = min(len(self.input_history) - 1,
                                       self.history_index + 1)
                self.input_area.setPlainText(self.input_history[-self.history_index - 1])
                return

            if event.key() == Qt.Key_Down:
                if self.history_index > 0:
                    self.history_index -= 1
                    self.input_area.setPlainText(self.input_history[-self.history_index - 1])
                elif self.history_index == 0:
                    self.history_index = -1
                    self.input_area.setPlainText(self.current_input)
                return

        super().keyPressEvent(event)

    def clear(self):
        """Clear the input area."""
        self.input_area.clear()

    def toPlainText(self) -> str:
        """Get the current input text."""
        return self.input_area.toPlainText()

    def setPlainText(self, text: str):
        """Set the input text."""
        self.input_area.setPlainText(text)

    def cursorRect(self):
        """Get the cursor rectangle from the input area."""
        return self.input_area.cursorRect()

    def setFocus(self):
        """Set focus to the input area."""
        self.input_area.setFocus()

    def hasFocus(self) -> bool:
        """Check if the input area has focus."""
        return self.input_area.hasFocus()

    def document(self):
        """Get the document from the input area."""
        return self.input_area.document()

    def textCursor(self):
        """Get the text cursor from the input area."""
        return self.input_area.textCursor()

    def undo(self):
        """Undo the last edit operation."""
        self.input_area.undo()

    def redo(self):
        """Redo the last undone edit operation."""
        self.input_area.redo()

    def cut(self):
        """Cut selected text to clipboard."""
        self.input_area.cut()

    def copy(self):
        """Copy selected text to clipboard."""
        self.input_area.copy()

    def paste(self):
        """Paste text from clipboard."""
        self.input_area.paste()
