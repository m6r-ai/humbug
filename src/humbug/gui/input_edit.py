"""Input edit widget."""

from PySide6.QtWidgets import QFrame, QTextEdit, QSizePolicy
from PySide6.QtCore import Qt, QSize, Signal
from PySide6.QtGui import QKeyEvent


class InputEdit(QTextEdit):
    """Editable input area for user messages."""

    submitted = Signal(str)

    def __init__(self, parent=None):
        """Initialize the input edit area."""
        super().__init__(parent)
        self.setFrameStyle(QFrame.NoFrame)
        self.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)

        # Set minimum height for input area
#        self.setMinimumHeight(40)

        # Set size policy to expand horizontally but be minimum vertically
        self.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Minimum)

        # Input history
        self.input_history = []
        self.history_index = -1
        self.current_input = ""

        # Watch for document changes
        self.document().contentsChanged.connect(self._on_content_changed)

        self.setStyleSheet("""
            QTextEdit {
                background-color: black;
                color: white;
                selection-background-color: #404040;
                border: none;
            }
            QTextEdit:focus {
                background-color: #404040;
            }
        """)

    def _on_content_changed(self):
        self.updateGeometry()

    def minimumSizeHint(self) -> QSize:
        """Calculate the total size needed for both widgets."""
        height = max(int(self.document().size().height()), 40)
        width = self.width() if self.width() > 0 else self.minimumWidth()

        return QSize(width, height)

    def sizeHint(self) -> QSize:
        """Calculate the total size needed for both widgets."""
        return self.minimumSizeHint()

    def keyPressEvent(self, event: QKeyEvent):
        """Handle special key events."""
        if event.key() == Qt.Key_J and event.modifiers() == Qt.ControlModifier:
            text = self.toPlainText().strip()
            if text:
                self.submitted.emit(text)
                if text not in self.input_history:
                    self.input_history.append(text)
                self.history_index = -1
                self.clear()
            return

        if self.textCursor().atStart() and not self.textCursor().hasSelection():
            if event.key() == Qt.Key_Up and self.input_history:
                if self.history_index == -1:
                    self.current_input = self.toPlainText()
                self.history_index = min(len(self.input_history) - 1,
                                       self.history_index + 1)
                self.setPlainText(self.input_history[-self.history_index - 1])
                return

            if event.key() == Qt.Key_Down:
                if self.history_index > 0:
                    self.history_index -= 1
                    self.setPlainText(self.input_history[-self.history_index - 1])
                elif self.history_index == 0:
                    self.history_index = -1
                    self.setPlainText(self.current_input)
                return

        super().keyPressEvent(event)
