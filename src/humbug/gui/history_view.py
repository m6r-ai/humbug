"""Chat history view widget."""

from typing import List, Optional

from PySide6.QtWidgets import QFrame, QVBoxLayout, QWidget, QSizePolicy
from PySide6.QtCore import QSize

from humbug.gui.color_role import ColorRole
from humbug.gui.message_widget import MessageWidget
from humbug.gui.live_input_widget import LiveInputWidget
from humbug.gui.style_manager import StyleManager


class HistoryView(QFrame):
    """View for chat history with integrated input area."""

    def __init__(self, parent=None):
        """Initialize the history view."""
        super().__init__(parent)
        self.setFrameStyle(QFrame.NoFrame)

        # Create main layout
        self.layout = QVBoxLayout(self)
        self.layout.setSpacing(10)
        self.layout.setContentsMargins(10, 10, 10, 10)

        # Create message container
        self.message_container = QWidget(self)
        self.message_container.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.MinimumExpanding)

        # Create message container layout
        self.message_layout = QVBoxLayout(self.message_container)
        self.message_layout.setSpacing(10)
        self.message_layout.setContentsMargins(0, 0, 0, 0)
        self.message_layout.addStretch()  # Push messages to the top

        # Create input widget
        self.input = LiveInputWidget(self)
        self.input.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Fixed)

        # Add widgets to main layout
        self.layout.addWidget(self.message_container)
        self.layout.addWidget(self.input)

        # Track messages and current AI response
        self.messages: List[MessageWidget] = []
        self._ai_response_widget: Optional[MessageWidget] = None
        self._message_with_selection: Optional[MessageWidget] = None

        # Style the widget
        style_manager = StyleManager()
        self.setStyleSheet(f"""
            QFrame {{
                background-color: {style_manager.get_color_str(ColorRole.TAB_ACTIVE)};
                border: none;
            }}
            QWidget#message_container {{
                background-color: {style_manager.get_color_str(ColorRole.TAB_ACTIVE)};
            }}
        """)
        self.message_container.setObjectName("message_container")

    def append_message(self, message: str, style: str):
        """Append a message with the specified style."""
        msg_widget = MessageWidget(self)
        msg_widget.selectionChanged.connect(
            lambda has_selection: self._handle_selection_changed(msg_widget, has_selection)
        )
        msg_widget.set_content(message, style)
        msg_widget.setFixedWidth(self.width() - 20)  # Account for margins

        # Add widget before the stretch spacer
        self.message_layout.insertWidget(self.message_layout.count() - 1, msg_widget)
        self.messages.append(msg_widget)

        if style == 'ai':
            self._ai_response_widget = msg_widget
        else:
            self._ai_response_widget = None

    def update_last_ai_response(self, content: str):
        """Update the last AI response in the history."""
        if self._ai_response_widget:
            self._ai_response_widget.set_content(content, 'ai')
        else:
            self.append_message(content, 'ai')

    def finish_ai_response(self):
        """Mark the current AI response as complete."""
        self._ai_response_widget = None

    def _handle_selection_changed(self, message_widget: MessageWidget, has_selection: bool):
        """Handle selection changes in message widgets."""
        if has_selection:
            if self._message_with_selection and self._message_with_selection != message_widget:
                old_cursor = self._message_with_selection.text_area.textCursor()
                old_cursor.clearSelection()
                self._message_with_selection.text_area.setTextCursor(old_cursor)
            self._message_with_selection = message_widget
        elif message_widget == self._message_with_selection:
            self._message_with_selection = None

    def has_selection(self) -> bool:
        """Check if any message has selected text."""
        return self._message_with_selection is not None and self._message_with_selection.has_selection()

    def copy_selection(self):
        """Copy selected text to clipboard."""
        if self._message_with_selection:
            self._message_with_selection.copy_selection()

    def resizeEvent(self, event):
        """Handle resize events."""
        super().resizeEvent(event)
        new_width = self.width() - 20  # Account for margins
        # Update all message widgets
        for message in self.messages:
            message.setFixedWidth(new_width)
        # Update input widget
        self.input.setFixedWidth(new_width)

    def sizeHint(self) -> QSize:
        """Calculate size based on content."""
        return QSize(
            self.width(),
            self.message_container.sizeHint().height() + self.input.sizeHint().height() + 20
        )

    def minimumSizeHint(self) -> QSize:
        """Minimum size is the same as size hint."""
        return self.sizeHint()
