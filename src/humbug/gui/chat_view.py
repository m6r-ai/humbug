"""Unified chat view implementation with correct scrolling and input expansion."""

from PySide6.QtWidgets import (
    QFrame, QVBoxLayout, QWidget, QScrollArea, QSizePolicy
)
from PySide6.QtCore import QEvent, QSize, QTimer
from PySide6.QtGui import QResizeEvent

from humbug.gui.history_view import HistoryView
from humbug.gui.input_edit import InputEdit


class ChatContainer(QWidget):
    """Container widget that manages the history and input views."""

    def __init__(self, parent=None):
        """Initialize the container widget."""
        super().__init__(parent)

        # Create child widgets
        self.history = HistoryView(self)
        self.input = InputEdit(self)

        vbox = QVBoxLayout()
        vbox.setSpacing(0)
        vbox.setContentsMargins(0, 0, 0, 0)
        vbox.addWidget(self.history)
        vbox.addWidget(self.input)
        self.setLayout(vbox)

        # Set size policy for container
        self.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.MinimumExpanding)
        self.setMinimumWidth(200)

    def resizeEvent(self, event: QResizeEvent):
        """Handle widget resize events."""
        # Get the old and new sizes
        old_size: QSize = event.oldSize()

        # Do whatever you need with the size information
        was_at_bottom = False
        chat_view = self.parent()
        while chat_view and not isinstance(chat_view, ChatView):
            chat_view = chat_view.parent()

        if chat_view and chat_view.is_scrolled_to_bottom(old_size.height() - chat_view.scroll_area.viewport().height()):
            was_at_bottom = True

        if chat_view and was_at_bottom:
            QTimer.singleShot(0, chat_view.scroll_to_bottom)

        # Don't forget to call the parent class's resizeEvent
        super().resizeEvent(event)


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

        # Watch of cursor position changes
        self.container.input.cursorPositionChanged.connect(self._ensure_cursor_visible)

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

    def is_scrolled_to_bottom(self, old_maximum) -> bool:
        """Check if scroll area is at the bottom."""
        scrollbar = self.scroll_area.verticalScrollBar()
        return scrollbar.value() >= old_maximum - 20

    def scroll_to_bottom(self):
        """Scroll to the bottom of the content."""
        scrollbar = self.scroll_area.verticalScrollBar()
        scrollbar.setValue(scrollbar.maximum())

    def add_message(self, message: str, style: str):
        """Add a message to history with appropriate styling."""
        if style == 'ai' and message.startswith("AI: "):
            self.history.update_last_ai_response(message[4:])
        else:
            self.history.append_message(message, style)

        QTimer.singleShot(0, self.scroll_to_bottom)

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

    def _ensure_cursor_visible(self):
        """Ensure the cursor remains visible when it moves."""
        cursor_rect = self.container.input.cursorRect()

        self.scroll_area.ensureVisible(cursor_rect.left(), cursor_rect.top() + self.container.history.height(), 0, 50)
