"""Chat history view widget."""

from typing import List, Optional

from PySide6.QtWidgets import QFrame, QVBoxLayout, QSizePolicy
from PySide6.QtCore import QSize, Signal
from PySide6.QtGui import QResizeEvent

from humbug.gui.color_role import ColorRole
from humbug.gui.message_widget import MessageWidget
from humbug.gui.live_input_widget import LiveInputWidget
from humbug.gui.style_manager import StyleManager


class HistoryView(QFrame):
    """View for chat history with integrated input area."""

    # Signal to notify when container needs scroll adjustment
    scroll_requested = Signal(QSize)

    def __init__(self, parent=None):
        """Initialize the history view."""
        super().__init__(parent)
        self.setFrameStyle(QFrame.NoFrame)

        # Create main layout
        self.layout = QVBoxLayout(self)
        self.layout.setSpacing(10)
        self.layout.setContentsMargins(10, 10, 10, 10)

        # Add stretcher at the top to push messages down
        self.layout.addStretch()

        # Create input widget
        self._input = LiveInputWidget(self)
        self._input.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Fixed)
        self._input.setFixedWidth(self.width() - 20)  # Account for margins

        # Add input widget at the bottom
        self.layout.addWidget(self._input)

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
        """)

    def append_message(self, message: str, style: str):
        """Append a message with the specified style."""
        msg_widget = MessageWidget(self)
        msg_widget.selectionChanged.connect(
            lambda has_selection: self._handle_selection_changed(msg_widget, has_selection)
        )
        msg_widget.set_content(message, style)
        msg_widget.setFixedWidth(self.width() - 20)  # Account for margins

        # Add widget after the stretch spacer but before the input
        self.layout.insertWidget(self.layout.count() - 1, msg_widget)
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

    def resizeEvent(self, event: QResizeEvent) -> None:
        """Handle resize events."""
        super().resizeEvent(event)
        new_width = self.width() - 20  # Account for margins
        # Update all message widgets
        for message in self.messages:
            message.setFixedWidth(new_width)

        # Update input widget
        self._input.setFixedWidth(new_width)

        old_size: QSize = event.oldSize()
        self.scroll_requested.emit(old_size)

    def sizeHint(self) -> QSize:
        """Calculate size based on content."""
        total_height = sum(msg.sizeHint().height() + self.layout.spacing() for msg in self.messages)
        total_height += self._input.sizeHint().height() + (2 * self.layout.contentsMargins().top())
        return QSize(self.width(), total_height)

    def minimumSizeHint(self) -> QSize:
        """Minimum size is the same as size hint."""
        return self.sizeHint()

    def get_input_text(self) -> str:
        """Get the current input text."""
        return self._input.toPlainText()

    def set_input_text(self, text: str) -> None:
        """Set the input text."""
        self._input.setPlainText(text)

    def clear_input(self) -> None:
        """Clear the input area."""
        self._input.clear()

    def set_input_focus(self) -> None:
        """Set focus to the input area."""
        self._input.setFocus()

    def has_input_focus(self) -> bool:
        """Check if input area has focus."""
        return self._input.hasFocus()

    def get_input_cursor_rect(self):
        """Get the cursor rectangle from the input area."""
        return self._input.cursorRect()

    def can_undo(self) -> bool:
        """Check if undo is available in input area."""
        return self._input.document().isUndoAvailable()

    def undo(self) -> None:
        """Undo the last edit operation in input area."""
        self._input.undo()

    def can_redo(self) -> bool:
        """Check if redo is available in input area."""
        return self._input.document().isRedoAvailable()

    def redo(self) -> None:
        """Redo the last undone edit operation in input area."""
        self._input.redo()

    def can_cut(self) -> bool:
        """Check if cut is available in input area."""
        return self._input.hasFocus() and self._input.textCursor().hasSelection()

    def cut(self) -> None:
        """Cut selected text from input area to clipboard."""
        self._input.cut()

    def can_copy(self) -> bool:
        """Check if copy is available."""
        return self.has_selection() or self._input.textCursor().hasSelection()

    def copy(self):
        """Copy selected text to clipboard."""
        if self._input.hasFocus():
            self._input.copy()
        else:
            self.copy_selection()

    def can_paste(self) -> bool:
        """Check if paste is available in input area."""
        return self._input.hasFocus()

    def paste(self) -> None:
        """Paste text from clipboard to input area."""
        self._input.paste()

    def connect_input_cursor_changed(self, slot):
        """Connect a slot to input cursor position changes."""
        self._input.cursorPositionChanged.connect(slot)
