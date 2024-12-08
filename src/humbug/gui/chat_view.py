"""Unified chat view implementation with correct scrolling and input expansion."""

from typing import Dict, List, Optional

from PySide6.QtWidgets import (
    QFrame, QLabel, QVBoxLayout, QWidget, QScrollArea, QSizePolicy
)
from PySide6.QtCore import QSize, QTimer, Signal
from PySide6.QtGui import QResizeEvent

from humbug.ai.conversation_settings import ConversationSettings
from humbug.conversation import ConversationHistory, Message, MessageSource, Usage
from humbug.gui.history_view import HistoryView
from humbug.gui.input_edit import InputEdit


class ChatContainer(QWidget):
    """Container widget that manages the history and input views."""

    def __init__(self, parent: Optional[QWidget] = None) -> None:
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

    def resizeEvent(self, event: QResizeEvent) -> None:
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

    # Signal emitted when the tab should be closed
    close_requested = Signal(str)  # Emits conversation_id

    def __init__(self, conversation_id: str, parent: Optional[QWidget] = None) -> None:
        """Initialize the unified chat view."""
        super().__init__(parent)
        self._conversation_id = conversation_id
        self._conversation = ConversationHistory(conversation_id)
        self._settings = ConversationSettings()
        self._current_ai_message = None
        self._setup_ui()

    @property
    def conversation_id(self) -> str:
        """Get the conversation ID for this view."""
        return self._conversation_id

    @property
    def input(self) -> InputEdit:
        """Provide access to input widget."""
        return self.container.input

    @property
    def history(self) -> HistoryView:
        """Provide access to history widget."""
        return self.container.history

    def get_settings(self) -> ConversationSettings:
        """Get current conversation settings."""
        return ConversationSettings(
            model=self._settings.model,
            temperature=self._settings.temperature
        )

    def get_message_context(self) -> List[str]:
        """Get messages formatted for AI context."""
        return self._conversation.get_messages_for_context()

    def update_settings(self, settings: ConversationSettings) -> None:
        """Update conversation settings."""
        self._settings = settings
        # Update the status bar to reflect new settings
        self._update_status_display()

    def _setup_ui(self):
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

        # Add status bar
        self.status_bar = QLabel()
        self.status_bar.setStyleSheet("""
            QLabel {
                background-color: #d3d3d3;
                color: black;
                padding: 2px 2px;
            }
        """)
        layout.addWidget(self.status_bar)

        self.update_status(0, 0)

        # Watch for cursor position changes
        self.container.input.cursorPositionChanged.connect(self._ensure_cursor_visible)

        # Set initial focus to input area
        QTimer.singleShot(0, self._set_initial_focus)

    async def update_streaming_response(self, content: str, usage: Optional[Usage] = None,
                                    error: Optional[Dict] = None, completed: bool = False) -> Optional[Message]:
        """Update the current AI response in the conversation."""
        if error:
            error_msg = f"Error: {error['message']}"
            self.history.update_last_ai_response(error_msg)
            error_message = Message.create(
                self._conversation_id,
                MessageSource.SYSTEM,
                error_msg,
                error=error
            )
            self._conversation.add_message(error_message)
            return error_message

        # Update display
        self.history.update_last_ai_response(content)

        # Update or create AI message in conversation
        settings = self.get_settings()
        if not self._current_ai_message:
            # Create and add initial message
            message = Message.create(
                self._conversation_id,
                MessageSource.AI,
                content,
                model=settings.model,
                temperature=settings.temperature,
                completed=False
            )
            self._conversation.add_message(message)
            self._current_ai_message = message
        else:
            # Update existing message
            message = self._conversation.update_message(
                self._current_ai_message.id,
                content,
                usage=usage,
                completed=(usage is not None or completed)
            )
            if not message:
                return None

        if usage:
            self._update_status_display()
            self._current_ai_message = None
            return message

        if completed:
            self.finish_ai_response()
            self._current_ai_message = None
            return message

        return message

    def add_user_message(self, content: str) -> Message:
        """Add a user message to the conversation and return the message object."""
        self.add_message(f"You: {content}", "user")
        message = Message.create(
            self._conversation_id,
            MessageSource.USER,
            content
        )
        self._conversation.add_message(message)
        return message

    def add_system_message(self, content: str, error: Optional[Dict] = None) -> Message:
        """Add a system message to the conversation and return the message object."""
        self.add_message(content, "system")
        message = Message.create(
            self._conversation_id,
            MessageSource.SYSTEM,
            content,
            error=error
        )
        self._conversation.add_message(message)
        return message

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
            QTextEdit:focus {
                background-color: #404040;
            }
        """)

    def is_scrolled_to_bottom(self, old_maximum) -> bool:
        """Check if scroll area is at the bottom."""
        scrollbar = self.scroll_area.verticalScrollBar()
        return scrollbar.value() >= old_maximum - 20

    def scroll_to_bottom(self) -> None:
        """Scroll to the bottom of the content."""
        scrollbar = self.scroll_area.verticalScrollBar()
        scrollbar.setValue(scrollbar.maximum())

    def add_message(self, message: str, style: str) -> None:
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

    def _update_status_display(self):
        """Update status bar with current settings and token counts."""
        counts = self._conversation.get_token_counts()
        self.update_status(
            counts['input'],
            counts['output']
        )

    def update_status(self, input_tokens: int, output_tokens: int):
        """Update the status bar with token counts and settings."""
        temp_display = f"Temp: {self._settings.temperature:.1f}" if self._settings.temperature is not None else "Temp: N/A"
        self.status_bar.setText(
            f"Model: {self._settings.model} | "
            f"{temp_display} | "
            f"Input tokens: {input_tokens} | "
            f"Output tokens: {output_tokens}"
        )

    def _ensure_cursor_visible(self):
        """Ensure the cursor remains visible when it moves."""
        cursor_rect = self.container.input.cursorRect()
        self.scroll_area.ensureVisible(
            cursor_rect.left(),
            cursor_rect.top() + self.container.history.height(),
            0,
            50
        )
