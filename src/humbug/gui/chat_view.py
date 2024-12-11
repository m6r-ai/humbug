"""Unified chat view implementation with correct scrolling and input expansion."""

from typing import Dict, List, Optional

from PySide6.QtWidgets import (
    QFrame, QLabel, QVBoxLayout, QWidget, QScrollArea, QSizePolicy
)
from PySide6.QtCore import QTimer, Signal, QSize

from humbug.ai.conversation_settings import ConversationSettings
from humbug.conversation.conversation_history import ConversationHistory
from humbug.conversation.message import Message
from humbug.conversation.message_source import MessageSource
from humbug.conversation.usage import Usage
from humbug.gui.chat_container import ChatContainer
from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager


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
        layout.setContentsMargins(0, 1, 0, 0)
        layout.setSpacing(0)

        # Create scroll area
        self.scroll_area = QScrollArea(self)
        self.scroll_area.setFrameStyle(0)

        # Create and set the container widget
        self.container = ChatContainer()
        self.scroll_area.setWidget(self.container)
        self.scroll_area.setWidgetResizable(True)

        # Connect the scroll request signal
        self.container.scroll_requested.connect(self._handle_scroll_request)

        # Set size policy for scroll area
        self.scroll_area.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Expanding)

        style_manager = StyleManager()
        self.setStyleSheet(f"""
            QFrame {{
                background-color: {style_manager.get_color_str(ColorRole.TAB_ACTIVE)};
                border-top: 1px solid {style_manager.get_color_str(ColorRole.TAB_ACTIVE)};
            }}
            #container {{
                background-color: {style_manager.get_color_str(ColorRole.TAB_ACTIVE)};
            }}
        """)

        # Style the scroll area
        self.scroll_area.setStyleSheet(f"""
            QScrollBar:vertical {{
                background: {style_manager.get_color_str(ColorRole.SCROLLBAR_BACKGROUND)};
                width: 12px;
            }}
            QScrollBar::handle:vertical {{
                background: {style_manager.get_color_str(ColorRole.SCROLLBAR_HANDLE)};
                min-height: 20px;
            }}
            QScrollBar::add-line:vertical, QScrollBar::sub-line:vertical {{
                height: 0px;
            }}
        """)

        # Add scroll area to main layout
        layout.addWidget(self.scroll_area)

        # Add status bar
        self.status_bar = QLabel()
        self.status_bar.setStyleSheet(f"""
            QLabel {{
                background-color: {style_manager.get_color_str(ColorRole.STATUS_BAR)};
                color: black;
                padding: 2px 2px;
            }}
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
            self.container.history.update_last_ai_response(error_msg)
            error_message = Message.create(
                self._conversation_id,
                MessageSource.SYSTEM,
                error_msg,
                error=error
            )
            self._conversation.add_message(error_message)
            return error_message

        # Update display
        self.container.history.update_last_ai_response(content)

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
        self.add_message(content, "user")
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

    def _is_scrolled_to_bottom(self, old_maximum) -> bool:
        """Check if scroll area is at the bottom."""
        scrollbar = self.scroll_area.verticalScrollBar()
        return scrollbar.value() >= old_maximum - 20

    def _scroll_to_bottom(self) -> None:
        """Scroll to the bottom of the content."""
        scrollbar = self.scroll_area.verticalScrollBar()
        scrollbar.setValue(scrollbar.maximum())

    def _handle_scroll_request(self, old_size: QSize) -> None:
        # Do whatever you need with the size information
        if self._is_scrolled_to_bottom(old_size.height() - self.scroll_area.viewport().height()):
            QTimer.singleShot(0, self._scroll_to_bottom)

    def add_message(self, message: str, style: str) -> None:
        """Add a message to history with appropriate styling."""
        if style == 'ai':
            self.container.history.update_last_ai_response(message[4:])
        else:
            self.container.history.append_message(message, style)

        QTimer.singleShot(0, self._scroll_to_bottom)

    def get_input_text(self) -> str:
        """Get the current input text."""
        return self.container.input.toPlainText()

    def set_input_text(self, text: str):
        """Set the input text."""
        self.container.input.setPlainText(text)

    def clear_input(self):
        """Clear the input area."""
        self.container.input.clear()

    def finish_ai_response(self):
        """Mark the current AI response as complete."""
        self.container.history.finish_ai_response()

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

    def can_undo(self) -> bool:
        """Is undo currently available?"""
        return self.container.input.document().isUndoAvailable()

    def undo(self):
        """Undo the last edit operation."""
        self.container.input.text_area.undo()

    def can_redo(self) -> bool:
        """Is redo currently available?"""
        return self.container.input.document().isRedoAvailable()

    def redo(self):
        """Redo the last undone edit operation."""
        self.container.input.text_area.redo()

    def can_cut(self) -> bool:
        """Is cut currently available?"""
        input_focused = self.container.input.hasFocus()
        has_input_selection = self.container.input.textCursor().hasSelection()
        return input_focused and has_input_selection

    def cut(self):
        """Cut selected text to clipboard."""
        if self.container.input.hasFocus():
            self.container.input.text_area.cut()

    def can_copy(self) -> bool:
        """Is copy currently available?"""
        has_input_selection = self.container.input.textCursor().hasSelection()
        has_history_selection = self.container.history.has_selection()
        return has_history_selection or has_input_selection

    def copy(self):
        """Copy selected text to clipboard."""
        if self.container.input.hasFocus():
            self.container.input.copy()
        else:
            self.container.history.copy_selection()

    def can_paste(self) -> bool:
        """Is paste currently available?"""
        input_focused = self.container.input.hasFocus()
        return input_focused

    def paste(self):
        """Paste text from clipboard."""
        self.container.input.paste()
