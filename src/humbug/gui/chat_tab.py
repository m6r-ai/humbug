"""Unified chat tab implementation with correct scrolling and input expansion."""

from typing import Dict, List, Optional

from PySide6.QtWidgets import (
    QLabel, QVBoxLayout, QWidget, QScrollArea, QSizePolicy
)
from PySide6.QtCore import QTimer, Signal, QPoint, Qt, Slot
from PySide6.QtGui import QCursor, QResizeEvent

from humbug.ai.conversation_settings import ConversationSettings
from humbug.conversation.conversation_history import ConversationHistory
from humbug.conversation.message import Message
from humbug.conversation.message_source import MessageSource
from humbug.conversation.usage import Usage
from humbug.gui.color_role import ColorRole
from humbug.gui.message_widget import MessageWidget
from humbug.gui.live_input_widget import LiveInputWidget
from humbug.gui.style_manager import StyleManager
from humbug.gui.tab_base import TabBase


class ChatTab(TabBase):
    """Unified chat tab implementing single-window feel with distinct regions."""

    # Signal emitted when the tab should be closed
    submitted = Signal(str)  # Emits message text when submitted

    def __init__(self, tab_id: str, parent: Optional[QWidget] = None) -> None:
        """Initialize the unified chat tab."""
        super().__init__(tab_id, parent)
        self._conversation = ConversationHistory(tab_id)
        self._settings = ConversationSettings()
        self._current_ai_message = None
        self._messages: List[MessageWidget] = []
        self._message_with_selection: Optional[MessageWidget] = None

        chat_layout = QVBoxLayout(self)
        self.setLayout(chat_layout)

        self._scroll_area = QScrollArea()

        self._messages_container = QWidget()

        self._messages_layout = QVBoxLayout(self._messages_container)
        self._messages_container.setLayout(self._messages_layout)

        # Set up the input box
        self._input = LiveInputWidget(self._messages_container)
        self._input.cursorPositionChanged.connect(self._ensure_cursor_visible)
        self._input.submitted.connect(self._handle_submit)
        self._input.selectionChanged.connect(
            lambda has_selection: self._handle_selection_changed(self._input, has_selection)
        )
        self._input.scrollRequested.connect(self._handle_selection_scroll)
        self._input.mouseReleased.connect(self._stop_scroll)

        self._messages_layout.setSpacing(10)
        self._messages_layout.setContentsMargins(10, 10, 10, 10)
        self._messages_layout.addStretch()
        self._messages_layout.addWidget(self._input)

        self._style_manager = StyleManager()

        self._messages_container.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Expanding)

        # Set up the scroll area
        self._scroll_area.setFrameStyle(0)
        self._scroll_area.setWidget(self._messages_container)
        self._scroll_area.setWidgetResizable(True)
        self._scroll_area.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Expanding)
        self._scroll_area.setVerticalScrollBarPolicy(Qt.ScrollBarAsNeeded)
        self._scroll_area.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)

        # Set up the status bar
        self._status_bar = QLabel()

        # Main layout
        chat_layout.setContentsMargins(0, 1, 0, 0)
        chat_layout.setSpacing(0)
        chat_layout.addWidget(self._scroll_area)
        chat_layout.addWidget(self._status_bar)

        zoom_factor = self._style_manager.zoom_factor

        self.update_status(0, 0)

        self._style_manager.style_changed.connect(self._handle_style_changed)
        self._handle_style_changed(zoom_factor)

        # Create timer for smooth scrolling
        self._scroll_timer = QTimer(self)
        self._scroll_timer.setInterval(16)  # ~60fps
        self._scroll_timer.timeout.connect(self._update_scroll)
        self._last_mouse_pos = None

        # Initialize tracking variables
        self._auto_scroll = True
        self._last_scroll_maximum = 0
        self._last_insertion_point = 0

        # Connect to the vertical scrollbar's change signals
        self._scroll_area.verticalScrollBar().valueChanged.connect(self._on_scroll_value_changed)
        self._scroll_area.verticalScrollBar().rangeChanged.connect(self._on_scroll_range_changed)

        # Set initial focus to input area
        QTimer.singleShot(0, self._set_initial_focus)

    def _handle_selection_scroll(self, mouse_pos: QPoint):
        """Begin scroll handling for selection drag."""
        viewport_pos = self._scroll_area.viewport().mapFromGlobal(mouse_pos)

        if not self._scroll_timer.isActive():
            self._scroll_timer.start()

        self._last_mouse_pos = viewport_pos

    def _stop_scroll(self):
        """Stop any ongoing selection scrolling."""
        if self._scroll_timer.isActive():
            self._scroll_timer.stop()

        self._last_mouse_pos = None

    def _update_scroll(self):
        """Update scroll position based on mouse position."""
        if not self._last_mouse_pos:
            self._scroll_timer.stop()
            return

        viewport = self._scroll_area.viewport()
        scrollbar = self._scroll_area.verticalScrollBar()
        current_val = scrollbar.value()
        viewport_height = viewport.height()

        # Calculate scroll amount based on distance from viewport edges
        if self._last_mouse_pos.y() < 0:
            # Above viewport
            distance_out = -self._last_mouse_pos.y()
            if distance_out > viewport_height * 2:
                scrollbar.setValue(scrollbar.minimum())
            else:
                scroll_amount = min(50, max(10, distance_out // 5))
                new_val = max(scrollbar.minimum(), current_val - scroll_amount)
                scrollbar.setValue(new_val)

        elif self._last_mouse_pos.y() > viewport_height:
            # Below viewport
            distance_out = self._last_mouse_pos.y() - viewport_height
            if distance_out > viewport_height * 2:
                scrollbar.setValue(scrollbar.maximum())
            else:
                scroll_amount = min(50, max(10, distance_out // 5))
                new_val = min(scrollbar.maximum(), current_val + scroll_amount)
                scrollbar.setValue(new_val)

        # Update mouse position
        self._last_mouse_pos = self._scroll_area.viewport().mapFromGlobal(QCursor.pos())

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
        self._update_status_display()


    def _handle_submit(self):
        """Handle message submission from input widget."""
        text = self.get_input_text().strip()
        if text:
            self.submitted.emit(text)

    @Slot(int)
    def _on_scroll_value_changed(self, value: int):
        """
        Handle scroll value changes to detect user scrolling.

        Args:
            value (int): The new scroll value
        """
        # Get the vertical scrollbar
        vbar = self._scroll_area.verticalScrollBar()

        # Check if we're at the bottom
        at_bottom = value == vbar.maximum()

        # If user scrolls up, disable auto-scroll
        if not at_bottom:
            self._auto_scroll = False

        # If user scrolls to bottom, re-enable auto-scroll
        if at_bottom:
            self._auto_scroll = True

    @Slot(int, int)
    def _on_scroll_range_changed(self, _minimum, _maximum):
        """Handle the scroll range changing."""
        # If we're set to auto-scroll then do so now
        if self._auto_scroll:
            self._scroll_to_bottom()

        current_pos = self._scroll_area.verticalScrollBar().value()

        # Work out what we're supposed to do about scrolling.
        vbar_maximum = self._scroll_area.verticalScrollBar().maximum()

        if current_pos > self._last_insertion_point:
            if self._last_scroll_maximum != vbar_maximum:
                max_diff = vbar_maximum - self._last_scroll_maximum
                self._scroll_area.verticalScrollBar().setValue(current_pos + max_diff)

        self._last_scroll_maximum = vbar_maximum

    def _scroll_to_bottom(self) -> None:
        """Scroll to the bottom of the content."""
        scrollbar = self._scroll_area.verticalScrollBar()
        scrollbar.setValue(scrollbar.maximum())

    def _add_message(self, message: str, style: str) -> None:
        """Add a message to history with appropriate styling."""
        msg_widget = MessageWidget(self)
        msg_widget.selectionChanged.connect(
            lambda has_selection: self._handle_selection_changed(msg_widget, has_selection)
        )
        msg_widget.scrollRequested.connect(self._handle_selection_scroll)
        msg_widget.mouseReleased.connect(self._stop_scroll)
        msg_widget.set_content(message, style)

        # Add widget before input
        self._messages_layout.insertWidget(self._messages_layout.count() - 1, msg_widget)
        self._messages.append(msg_widget)

        # When we call this we should always scroll to the bottom and restore auto-scrolling
        self._auto_scroll = True
        self._scroll_to_bottom()

    def _update_last_ai_response(self, content: str):
        """Update the last AI response in the history."""
        # If our last message was not from the AI then create a new one.
        if not self._messages or not self._messages[-1].is_ai:
            self._add_message(content, 'ai')
            return

        # Store current scroll position before appending.  If this insertion triggers a change
        # in scrolling state then we'll get a signal and will adjust the scrollbar state based
        # on this.
        self._last_insertion_point = self._get_insertion_point()

        # Update our message
        self._messages[-1].set_content(content, 'ai')

    def finish_ai_response(self):
        """Mark the current AI response as complete."""
        self._current_ai_message = None

    def _handle_selection_changed(self, message_widget: MessageWidget, has_selection: bool):
        """Handle selection changes in message widgets."""
        if has_selection:
            if self._message_with_selection and self._message_with_selection != message_widget:
                self._message_with_selection.clear_selection()
            self._message_with_selection = message_widget
        elif message_widget == self._message_with_selection:
            self._message_with_selection = None

    def clear_selection(self):
        """Clear all message selections."""
        if self._message_with_selection:
            self._message_with_selection.clear_selection()
            self._message_with_selection = None

    def has_selection(self) -> bool:
        """Check if any message has selected text."""
        return self._message_with_selection is not None and self._message_with_selection.has_selection()

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
        self._status_bar.setText(
            f"Model: {self._settings.model} | "
            f"{temp_display} | "
            f"Input tokens: {input_tokens} | "
            f"Output tokens: {output_tokens}"
        )

    def _get_insertion_point(self):
        """Determine where streaming responses will insert in the scroll area"""
        total_height = self._messages_container.height()
        input_height = self._input.height()
        return total_height - input_height - 2 * self._messages_layout.spacing()

    def _ensure_cursor_visible(self):
        """Ensure the cursor remains visible when it moves."""
        total_height = sum(msg.sizeHint().height() + self._messages_layout.spacing() for msg in self._messages)
        input_cursor = self._input.cursorRect()

        self._scroll_area.ensureVisible(
            input_cursor.x(),
            total_height + input_cursor.y(),
            1,
            50
        )

    def get_input_text(self) -> str:
        """Get the current input text."""
        return self._input.toPlainText()

    def set_input_text(self, text: str):
        """Set the input text."""
        self._input.setPlainText(text)

    def clear_input(self):
        """Clear the input area."""
        self._input.clear()

    def _set_initial_focus(self):
        """Set initial focus to input area."""
        self._input.setFocus()

    async def update_streaming_response(self, content: str, usage: Optional[Usage] = None,
                                     error: Optional[Dict] = None, completed: bool = False) -> Optional[Message]:
        """Update the current AI response in the conversation."""
        if error:
            error_msg = f"Error: {error['message']}"
            self._update_last_ai_response(error_msg)
            error_message = Message.create(
                self._tab_id,
                MessageSource.SYSTEM,
                error_msg,
                error=error
            )
            self._conversation.add_message(error_message)
            return error_message

        # Update display
        self._update_last_ai_response(content)

        # Update or create AI message in conversation
        settings = self.get_settings()
        if not self._current_ai_message:
            # Create and add initial message
            message = Message.create(
                self._tab_id,
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
        """Add a user message to the conversation."""
        self._add_message(content, "user")
        message = Message.create(
            self._tab_id,
            MessageSource.USER,
            content
        )
        self._conversation.add_message(message)
        return message

    def add_system_message(self, content: str, error: Optional[Dict] = None) -> Message:
        """Add a system message to the conversation."""
        self._add_message(content, "system")
        message = Message.create(
            self._tab_id,
            MessageSource.SYSTEM,
            content,
            error=error
        )
        self._conversation.add_message(message)
        return message

    def resizeEvent(self, event: QResizeEvent) -> None:
        """Handle resize events."""
        super().resizeEvent(event)

        if self._auto_scroll:
            self._scroll_to_bottom()

    def _handle_style_changed(self, factor: float) -> None:
        font = self.font()
        base_font_size = self._style_manager.base_font_size
        font.setPointSizeF(base_font_size * factor)
        self.setFont(font)

        self._messages_container.setStyleSheet(f"""
            QWidget {{
                background-color: {self._style_manager.get_color_str(ColorRole.TAB_ACTIVE)};
            }}
        """)
        self._scroll_area.setStyleSheet(f"""
            QScrollArea {{
                border: none;
            }}
            QScrollBar:vertical {{
                background-color: {self._style_manager.get_color_str(ColorRole.SCROLLBAR_BACKGROUND)};
                width: 12px;
            }}
            QScrollBar::handle:vertical {{
                background-color: {self._style_manager.get_color_str(ColorRole.SCROLLBAR_HANDLE)};
                min-height: 20px;
            }}
            QScrollBar::add-line:vertical, QScrollBar::sub-line:vertical {{
                height: 0px;
            }}
        """)
        self._status_bar.setStyleSheet(f"""
            QLabel {{
                background-color: {self._style_manager.get_color_str(ColorRole.STATUS_BAR)};
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                padding: 2px 2px;
            }}
        """)
        self.setStyleSheet(f"""
            QFrame {{
                background-color: {self._style_manager.get_color_str(ColorRole.TAB_ACTIVE)};
                border-top: 1px solid {self._style_manager.get_color_str(ColorRole.TAB_ACTIVE)};
            }}
        """)

        # Apply to all widgets in the hierarchy
        for widget in self.findChildren(QWidget):
            widget.setFont(font)

        for widget in self.findChildren(MessageWidget):
            widget.handle_style_changed()

        self.updateGeometry()

    def can_close(self) -> bool:
        return True

    def can_save(self) -> bool:
        return False

    def save(self) -> bool:
        return True

    def can_save_as(self) -> bool:
        return False

    def save_as(self) -> bool:
        return True

    def can_undo(self) -> bool:
        """Check if undo is available."""
        return self._input.document().isUndoAvailable()

    def undo(self):
        """Undo the last edit operation."""
        self._input.undo()

    def can_redo(self) -> bool:
        """Check if redo is available."""
        return self._input.document().isRedoAvailable()

    def redo(self):
        """Redo the last undone edit operation."""
        self._input.redo()

    def can_cut(self) -> bool:
        """Check if cut is available."""
        return self._input.hasFocus() and self._input.textCursor().hasSelection()

    def cut(self):
        """Cut selected text to clipboard."""
        self._input.cut()

    def can_copy(self) -> bool:
        """Check if copy is available."""
        return self.has_selection() or (self._input.hasFocus() and self._input.textCursor().hasSelection())

    def copy(self):
        """Copy selected text to clipboard."""
        if self._input.hasFocus():
            self._input.copy()
        elif self._message_with_selection:
            self._message_with_selection.copy_selection()

    def can_paste(self) -> bool:
        """Check if paste is available."""
        return self._input.hasFocus()

    def paste(self):
        """Paste text from clipboard."""
        self._input.paste()

    def can_submit(self) -> bool:
        return True
