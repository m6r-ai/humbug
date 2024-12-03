"""Unified chat view implementation with correct scrolling and input expansion."""

from typing import Optional

from PySide6.QtWidgets import (
    QFrame, QVBoxLayout, QWidget, QScrollArea, QTextEdit, QSizePolicy
)
from PySide6.QtCore import Qt, Signal, QEvent, QSize, QTimer
from PySide6.QtGui import (
    QTextCursor, QColor, QTextCharFormat, QKeyEvent, QResizeEvent
)


class HistoryView(QTextEdit):
    """Read-only view for chat history."""

    def __init__(self, parent=None):
        """Initialize the history view."""
        super().__init__(parent)
        self.setReadOnly(True)
        self.setFrameStyle(QFrame.NoFrame)
        self.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)

        # Set minimum height and size policy
        self.setMinimumHeight(100)
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
        height = max(int(self.document().size().height()), 100)
        self.setMinimumHeight(height)

        # Debug sizes after content change
        if isinstance(self.parent(), ChatContainer):
            self.parent().handle_input_change()

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
        self.setMinimumHeight(40)

        # Set size policy to expand horizontally but be minimum vertically
        self.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Minimum)

        # Input history
        self.input_history = []
        self.history_index = -1
        self.current_input = ""

        # Watch for document changes
        self.document().contentsChanged.connect(self._on_content_changed)

        # Watch of cursor position changes
        self.cursorPositionChanged.connect(self._ensure_cursor_visible)

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
        """Handle document content changes."""
        height = max(int(self.document().size().height()), 40)
        self.setMinimumHeight(height)

        # Debug sizes after content change
        if isinstance(self.parent(), ChatContainer):
            self.parent().handle_input_change()

    def _ensure_cursor_visible(self):
        """Ensure the cursor remains visible when it moves."""
        cursor = self.textCursor()
        self.ensureCursorVisible()

        # Find the chat view to handle container scrolling if needed
        chat_view = self.parent()
        while chat_view and not isinstance(chat_view, ChatView):
            chat_view = chat_view.parent()

        if chat_view:
            # Calculate cursor position in viewport coordinates
            cursor_rect = self.cursorRect()
            global_pos = self.mapTo(chat_view, cursor_rect.center())

            # Get the visible area of the chat view
            scroll_area = chat_view.scroll_area
            visible_rect = scroll_area.viewport().rect()

            # If cursor would be outside visible area, scroll to make it visible
            if global_pos.y() < visible_rect.top() or global_pos.y() > visible_rect.bottom():
                # Convert cursor position to scroll area coordinates
                scroll_pos = scroll_area.widget().mapFrom(chat_view, global_pos)
                scroll_area.ensureVisible(scroll_pos.x(), scroll_pos.y(), 0, 50)

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


class ChatContainer(QWidget):
    """Container widget that manages the history and input views."""

    def __init__(self, parent=None):
        """Initialize the container widget."""
        super().__init__(parent)

        # Create child widgets
        self.history = HistoryView(self)
        self.input = InputEdit(self)

        # Set size policy for container
        self.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Minimum)
        self.setMinimumWidth(200)

    def handle_input_change(self):
        """Handle input changes and ensure visibility."""
        was_at_bottom = False
        chat_view = self.parent()
        while chat_view and not isinstance(chat_view, ChatView):
            chat_view = chat_view.parent()

        if chat_view and chat_view.isScrolledToBottom():
            was_at_bottom = True

        # Update container geometry
        width = self.width() if self.width() > 0 else self.minimumWidth()

        # Get current height of each widget based on document size
        history_height = self.history.minimumHeight()
        input_height = self.input.minimumHeight()

        # Position and size history
        self.history.setGeometry(0, 0, width, history_height)

        # Position and size input directly below history
        self.input.setGeometry(0, history_height, width, input_height)

        self.adjustSize()

        # If we found the ChatView and it's scrolled near bottom, scroll to bottom
        if chat_view and was_at_bottom:
            QTimer.singleShot(0, chat_view.scrollToBottom)

    def sizeHint(self) -> QSize:
        """Calculate the total size needed for both widgets."""
        history_height = self.history.minimumHeight()
        input_height = self.input.minimumHeight()

        # Use parent width if available, otherwise minimum width
        width = self.width() if self.width() > 0 else self.minimumWidth()

        return QSize(width, history_height + input_height)


class ChatView(QFrame):
    """Unified chat view implementing single-window feel with distinct regions."""

    def __init__(self, parent=None):
        """Initialize the unified chat view."""
        super().__init__(parent)
        self.setup_ui()

    @property
    def input(self):
        """Provide access to input widget."""
        return self.container.input

    @property
    def history(self):
        """Provide access to history widget."""
        return self.container.history

    def setup_ui(self):
        """Set up the user interface."""
        # Main layout
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        # Create scroll area
        self.scroll_area = QScrollArea(self)
#        self.scroll_area.setVerticalScrollBarPolicy(Qt.ScrollBarAsNeeded)
#        self.scroll_area.setHorizontalScrollBarPolicy(Qt.ScrollBarAsNeeded)

        # Create and set the container widget
        self.container = ChatContainer()
        self.scroll_area.setWidget(self.container)
        self.scroll_area.setWidgetResizable(True)

        # Set size policy for scroll area
        self.scroll_area.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Expanding)

        # Style the scroll area
        self.scroll_area.setStyleSheet("""
            QScrollBar:vertical {
                background: #2d2d2d;
                width: 12px;
            }
            QScrollBar::handle:vertical {
                background: #404040;
                min-height: 20px;
            }
            QScrollBar::add-line:vertical, QScrollBar::sub-line:vertical {
                height: 0px;
            }
        """)

        # Add scroll area to main layout
        layout.addWidget(self.scroll_area)

        # Set initial focus to input area
        QTimer.singleShot(0, self._set_initial_focus)

    def _set_initial_focus(self):
        """Set initial focus to input area."""
        self.container.input.setFocus()
        # Explicitly set the focused style
        self.container.input.setStyleSheet("""
            QTextEdit {
                background-color: #404040;
                color: white;
                selection-background-color: #606060;
                border: none;
            }
        """)

    def eventFilter(self, obj: QWidget, event: QEvent) -> bool:
        """Handle focus changes for proper background colors."""
        if event.type() == QEvent.FocusIn:
            if isinstance(obj, (HistoryView, InputEdit)):
                obj.setStyleSheet("""
                    QTextEdit {
                        background-color: #404040;
                        color: white;
                        selection-background-color: #606060;
                        border: none;
                    }
                """)
        elif event.type() == QEvent.FocusOut:
            if isinstance(obj, (HistoryView, InputEdit)):
                obj.setStyleSheet("""
                    QTextEdit {
                        background-color: black;
                        color: white;
                        selection-background-color: #404040;
                        border: none;
                    }
                """)
        return super().eventFilter(obj, event)

    def isScrolledToBottom(self) -> bool:
        """Check if scroll area is at the bottom."""
        scrollbar = self.scroll_area.verticalScrollBar()
        print(f"scrollbar {scrollbar.value()} out of {scrollbar.maximum()}")
        return scrollbar.value() >= scrollbar.maximum() - 10  # Small threshold for "close to bottom"

    def scrollToBottom(self):
        """Scroll to the bottom of the content."""
        scrollbar = self.scroll_area.verticalScrollBar()
        scrollbar.setValue(scrollbar.maximum())

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
        self.parent().parent().statusBar().showMessage(
            f"Input tokens: {input_tokens} | Output tokens: {output_tokens}")
