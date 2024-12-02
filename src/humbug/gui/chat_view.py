"""Unified chat view implementation with correct scrolling and input expansion."""

from typing import Optional
from PySide6.QtWidgets import (QFrame, QVBoxLayout, QWidget, QScrollArea,
                             QTextEdit, QLabel, QSizePolicy)
from PySide6.QtCore import Qt, Signal, QEvent, QSize, QRect
from PySide6.QtGui import (QTextCursor, QColor, QTextCharFormat, QKeyEvent,
                          QResizeEvent, QWheelEvent, QPalette, QTextDocument)


class HistoryView(QTextEdit):
    """Read-only view for chat history."""

    def __init__(self, parent=None):
        """Initialize the history view."""
        super().__init__(parent)
        self.setReadOnly(True)
        self.setFrameStyle(QFrame.NoFrame)
        self.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)

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
        self.ensureCursorVisible()

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
        self.ensureCursorVisible()

    def finish_ai_response(self):
        """Mark the current AI response as complete."""
        self._ai_response_start = None
        self._ai_response_length = 0


class InputEdit(QTextEdit):
    """Editable input area for user messages."""

    submitted = Signal(str)
    height_changed = Signal(int, int)  # New height, height difference

    def __init__(self, parent=None):
        """Initialize the input edit area."""
        super().__init__(parent)
        self.setFrameStyle(QFrame.NoFrame)
        self.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)

        # Only set minimum height, no maximum
        self.setMinimumHeight(40)

        self.document().documentLayout().documentSizeChanged.connect(
            self._on_document_size_changed)

        self._current_height = 40

        # Input history
        self.input_history = []
        self.history_index = -1
        self.current_input = ""

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

    def _on_document_size_changed(self, new_size):
        """Handle document size changes."""
        # Calculate required height for content
        doc_height = new_size.height()
        margin = self.document().documentMargin()
        new_height = doc_height + 2 * margin

        # Ensure minimum height but no maximum
        new_height = max(40, new_height)

        # If height has changed, emit signal with new height and difference
        if new_height != self._current_height:
            height_diff = new_height - self._current_height
            self._current_height = new_height
            self.height_changed.emit(new_height, height_diff)

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


class ChatView(QFrame):
    """Unified chat view implementing single-window feel with distinct regions."""

    def __init__(self, parent=None):
        """Initialize the unified chat view."""
        super().__init__(parent)
        self.setup_ui()

    def setup_ui(self):
        """Set up the user interface."""
        # Main layout
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        # Create content widget that will be inside the scroll area
        self.content_widget = QWidget()
        self.content_layout = QVBoxLayout(self.content_widget)
        self.content_layout.setContentsMargins(0, 0, 0, 0)
        self.content_layout.setSpacing(0)

        # Create history and input
        self.history = HistoryView()
        self.input = InputEdit()

        # Set size policies to allow proper expansion
        self.history.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Expanding)
        self.input.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Minimum)

        # Add widgets to content layout
        self.content_layout.addWidget(self.history, 1)
        self.content_layout.addWidget(self.input, 0)

        # Create scroll area
        self.scroll_area = QScrollArea()
        self.scroll_area.setWidget(self.content_widget)
        self.scroll_area.setWidgetResizable(True)
        self.scroll_area.setFrameStyle(QFrame.NoFrame)
        self.scroll_area.setVerticalScrollBarPolicy(Qt.ScrollBarAsNeeded)
        self.scroll_area.setHorizontalScrollBarPolicy(Qt.ScrollBarAsNeeded)

        # Status bar
        self.status_bar = QLabel()
        self.status_bar.setStyleSheet("""
            QLabel {
                background-color: #d3d3d3;
                color: black;
                padding: 2px 5px;
            }
        """)

        # Add to main layout
        layout.addWidget(self.scroll_area)
        layout.addWidget(self.status_bar)

        # Connect signals
        self.input.height_changed.connect(self._handle_input_height_change)

        # Install event filters
        self.history.installEventFilter(self)
        self.input.installEventFilter(self)

    def _handle_input_height_change(self, new_height: int, height_diff: int):
        """Handle changes in input area height."""
        scrollbar = self.scroll_area.verticalScrollBar()

        # Store the current scroll position relative to the bottom
        max_scroll = scrollbar.maximum()
        current_scroll = scrollbar.value()
        distance_from_bottom = max_scroll - current_scroll

        # Update input height
        self.input.setFixedHeight(new_height)

        # After layout updates, adjust scroll position to maintain relative position
        self.scroll_area.verticalScrollBar().setValue(
            self.scroll_area.verticalScrollBar().maximum() - distance_from_bottom
        )

        # If we're near bottom, scroll to show new content
        if height_diff > 0 and distance_from_bottom < height_diff:
            self.scroll_area.verticalScrollBar().setValue(
                self.scroll_area.verticalScrollBar().maximum()
            )

    def eventFilter(self, obj: QWidget, event: QEvent) -> bool:
        """Handle focus changes for proper background colors."""
        if event.type() == QEvent.FocusIn:
            if obj in (self.history, self.input):
                obj.setStyleSheet("""
                    QTextEdit {
                        background-color: #404040;
                        color: white;
                        selection-background-color: #606060;
                        border: none;
                    }
                """)
        elif event.type() == QEvent.FocusOut:
            if obj in (self.history, self.input):
                obj.setStyleSheet("""
                    QTextEdit {
                        background-color: black;
                        color: white;
                        selection-background-color: #404040;
                        border: none;
                    }
                """)
        return super().eventFilter(obj, event)

    def wheelEvent(self, event: QWheelEvent):
        """Handle wheel events for smooth scrolling."""
        if event.angleDelta().y() != 0:
            scrollbar = self.scroll_area.verticalScrollBar()
            scrollbar.setValue(scrollbar.value() - event.angleDelta().y() // 2)

    def add_message(self, message: str, style: str):
        """Add a message to history with appropriate styling."""
        if style == 'ai' and message.startswith("AI: "):
            self.history.update_last_ai_response(message[4:])
        else:
            self.history.append_message(message, style)

    def get_input_text(self) -> str:
        """Get the current input text."""
        return self.input.toPlainText()

    def set_input_text(self, text: str):
        """Set the input text."""
        self.input.setPlainText(text)

    def clear_input(self):
        """Clear the input area."""
        self.input.clear()

    def finish_ai_response(self):
        """Mark the current AI response as complete."""
        self.history.finish_ai_response()

    def update_status(self, input_tokens: int, output_tokens: int):
        """Update the status bar with token counts."""
        self.status_bar.setText(
            f"Input tokens: {input_tokens} | Output tokens: {output_tokens}")
