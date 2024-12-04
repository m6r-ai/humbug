"""Chat history view widget."""

from typing import Optional

from PySide6.QtWidgets import QFrame, QTextEdit, QSizePolicy
from PySide6.QtCore import Qt, QSize
from PySide6.QtGui import QTextCursor, QColor, QTextCharFormat


class HistoryView(QTextEdit):
    """Read-only view for chat history."""

    def __init__(self, parent=None):
        """Initialize the history view."""
        super().__init__(parent)
        self.setReadOnly(True)
        self.setFrameStyle(QFrame.NoFrame)
        self.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        self.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.MinimumExpanding)

        # Style formats for different message types
        self.formats = {
            'user': self._create_format('white'),
            'ai': self._create_format('yellow'),
            'system': self._create_format('green'),
            'error': self._create_format('red')
        }

        # Track AI response position for updates
        self._ai_response_start: Optional[int] = None
        self._ai_response_length: int = 0

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

        # Watch for document changes
        self.document().contentsChanged.connect(self._on_content_changed)

    def _on_content_changed(self):
        """Handle document content changes."""
        self.updateGeometry()

    def minimumSizeHint(self) -> QSize:
        """Calculate the total size needed for both widgets."""
        height = max(int(self.document().size().height()), 100)
        width = self.width() if self.width() > 0 else self.minimumWidth()

        return QSize(width, height)

    def _create_format(self, color: str) -> QTextCharFormat:
        """Create a text format with the specified color."""
        fmt = QTextCharFormat()
        fmt.setForeground(QColor(color))
        return fmt

    def append_message(self, message: str, style: str):
        """Append a message with the specified style."""
        cursor = QTextCursor(self.document())
        cursor.movePosition(QTextCursor.End)

        if not cursor.atStart():
            cursor.insertBlock()

        cursor.insertText(message, self.formats.get(style, self.formats['user']))

        if style == 'ai':
            self._ai_response_start = cursor.position() - len(message)
            self._ai_response_length = len(message)
        else:
            self._ai_response_start = None
            self._ai_response_length = 0

        self.setTextCursor(cursor)

    def update_last_ai_response(self, content: str):
        """Update the last AI response in the history."""
        if self._ai_response_start is None:
            self.append_message(f"AI: {content}", 'ai')
            return

        cursor = QTextCursor(self.document())
        cursor.setPosition(self._ai_response_start)
        cursor.movePosition(QTextCursor.Right, QTextCursor.KeepAnchor,
                          self._ai_response_length)
        cursor.insertText(f"AI: {content}", self.formats['ai'])
        self._ai_response_length = len(f"AI: {content}")

        self.setTextCursor(cursor)

    def finish_ai_response(self):
        """Mark the current AI response as complete."""
        self._ai_response_start = None
        self._ai_response_length = 0
