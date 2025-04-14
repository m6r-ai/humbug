"""System widget implementation."""

import logging
import time
from typing import Dict, List, Tuple, Any, Set, cast

from PySide6.QtWidgets import (
    QWidget, QVBoxLayout, QScrollArea, QSizePolicy, QMenu
)
from PySide6.QtCore import QTimer, QPoint, Qt, Signal, QEvent, QObject
from PySide6.QtGui import QCursor, QResizeEvent

from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager
from humbug.gui.tab.system.system_input import SystemInput
from humbug.gui.tab.system.system_message import SystemMessage
from humbug.language.language_manager import LanguageManager
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.user.user_manager import UserManager


class SystemWidgetEventFilter(QObject):
    """Event filter to track activation events from child widgets."""

    widget_activated = Signal(object)
    widget_deactivated = Signal(object)

    def __init__(self, parent: QWidget | None = None) -> None:
        """Initialize the event filter."""
        super().__init__(parent)

    def eventFilter(self, obj: QObject, event: QEvent) -> bool:
        """
        Filter events to detect widget activation.

        Args:
            obj: The object that received the event
            event: The event that was received

        Returns:
            True if event was handled, False to pass to the target object
        """
        if event.type() in (QEvent.Type.MouseButtonPress, QEvent.Type.FocusIn):
            # Simply emit the signal with the object that received the event
            self.widget_activated.emit(obj)
            return False  # Don't consume the event

        if event.type() == QEvent.Type.FocusOut:
            # Emit a widget deactivated signal
            self.widget_deactivated.emit(obj)
            return False  # Don't consume the event

        return super().eventFilter(obj, event)


class SystemWidget(QWidget):
    """Widget for displaying system with message history and input."""

    # Signal to notify tab of status changes
    status_updated = Signal()

    # Signal to request scrolling to a specific widget and position
    scrollRequested = Signal(QWidget, int)  # Widget to scroll to, position within widget

    # Emits when parent should be activated by user interaction
    activated = Signal()

    def __init__(
        self,
        parent: QWidget | None = None,
    ) -> None:
        """
        Initialize the system widget.

        Args:
            path: Full path to transcript file
            timestamp: ISO format timestamp for the system
            parent: Optional parent widget
            use_existing_ai_system: Will we use an existing AI system?
        """
        super().__init__(parent)
        self._logger = logging.getLogger("SystemWidget")

        self._user_manager = UserManager()

        self._mindspace_manager = MindspaceManager()

        self._last_update_time: float = 0  # Timestamp of last UI update
        self._update_timer = QTimer(self)  # Timer for throttled updates
        self._update_timer.setSingleShot(True)
        self._update_timer.timeout.connect(self._process_pending_update)
        self._pending_message = None  # Store the most recent pending message

            # Widget tracking
        self._messages: List[SystemMessage] = []
        self._message_with_selection: SystemMessage | None = None
        self._is_streaming = False

        # Initialize tracking variables
        self._auto_scroll = True
        self._last_scroll_maximum = 0

        # Create layout
        system_layout = QVBoxLayout(self)
        self.setLayout(system_layout)
        system_layout.setContentsMargins(0, 0, 0, 0)
        system_layout.setSpacing(0)

        # Set up the scroll area
        self._scroll_area = QScrollArea()
        self._scroll_area.setFrameStyle(0)
        self._scroll_area.setWidgetResizable(True)
        self._scroll_area.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)
        self._scroll_area.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAsNeeded)
        self._scroll_area.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)

        # Create messages container widget
        self._messages_container = QWidget()
        self._messages_layout = QVBoxLayout(self._messages_container)
        self._messages_container.setLayout(self._messages_layout)

        # Set up the input box
        self._input = SystemInput(self._messages_container)
        self._input.cursorPositionChanged.connect(self._ensure_cursor_visible)
        self._input.selectionChanged.connect(
            lambda has_selection: self._handle_selection_changed(self._input, has_selection)
        )
        self._input.pageScrollRequested.connect(self._handle_edit_page_scroll)
        self._input.scrollRequested.connect(self._handle_selection_scroll)
        self._input.mouseReleased.connect(self._stop_scroll)

        self._messages_layout.setSpacing(10)
        self._messages_layout.setContentsMargins(10, 10, 10, 10)
        self._messages_layout.addStretch()
        self._messages_layout.addWidget(self._input)

        self._messages_container.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)
        self._scroll_area.setWidget(self._messages_container)

        # Add the scroll area to the main layout
        system_layout.addWidget(self._scroll_area)

        # Setup signals for search highlights
        self._search_highlights: Dict[SystemMessage, List[Tuple[int, int, int]]] = {}

        self._style_manager = StyleManager()

        # Tracking for focused message
        self._focused_message_index = -1

        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._handle_language_changed)

        # Create timer for smooth scrolling
        self._scroll_timer = QTimer(self)
        self._scroll_timer.setInterval(16)  # ~60fps
        self._scroll_timer.timeout.connect(self._update_scroll)
        self._last_mouse_pos: QPoint | None = None

        # Setup context menu
        self.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.customContextMenuRequested.connect(self._show_system_context_menu)

        # Connect to the vertical scrollbar's change signals
        self._scroll_area.verticalScrollBar().valueChanged.connect(self._on_scroll_value_changed)
        self._scroll_area.verticalScrollBar().rangeChanged.connect(self._on_scroll_range_changed)

        # Set initial focus to input area
        QTimer.singleShot(0, self._set_initial_focus)

        self._style_manager.style_changed.connect(self._handle_style_changed)
        self._handle_style_changed()

        # Find functionality
        self._matches: List[Tuple[SystemMessage, List[Tuple[int, int, int]]]] = []
        self._current_widget_index = -1
        self._current_match_index = -1
        self._last_search = ""
        self._highlighted_widgets: Set[SystemMessage] = set()

        # Set up activation tracking
        self._event_filter = SystemWidgetEventFilter(self)
        self._event_filter.widget_activated.connect(self._handle_widget_activation)
        self._event_filter.widget_deactivated.connect(self._handle_widget_deactivation)
        self._install_activation_tracking(self._input)

    async def add_message(self, message: AIMessage) -> None:
        """
        Add a new message to the system view.

        Args:
            message: The message that was added
        """
        msg_widget = SystemMessage(self)
        msg_widget.selectionChanged.connect(
            lambda has_selection: self._handle_selection_changed(msg_widget, has_selection)
        )
        # Add bookmark-specific signal
        msg_widget.scrollRequested.connect(self._handle_selection_scroll)
        msg_widget.mouseReleased.connect(self._stop_scroll)
        msg_widget.set_content(message.content, message.source, message.timestamp, message.model)

        # Add widget before input
        self._messages_layout.insertWidget(self._messages_layout.count() - 1, msg_widget)
        self._messages.append(msg_widget)

        self._install_activation_tracking(msg_widget)

    async def _on_request_error(self, retries_exhausted: bool, message: AIMessage) -> None:
        """
        Handle errors in AI responses.

        Args:
            message: The error that occurred
        """
        await self.add_message(message)

        if retries_exhausted:
            self._is_streaming = False
            self._input.set_streaming(False)
            self.status_updated.emit()

        # When we call this we should always scroll to the bottom and restore auto-scrolling
        self._auto_scroll = True
        self._scroll_to_bottom()

    async def _on_message_added(self, message: AIMessage) -> None:
        """
        Handle a new message being added to the system.

        Args:
            message: The message that was added
        """
        await self.add_message(message)

        # When we call this we should always scroll to the bottom and restore auto-scrolling
        self._auto_scroll = True
        self._scroll_to_bottom()

    async def _update_message(self, message: AIMessage) -> None:
        # Find the message widget that corresponds to the updated message
        # This is a simple approach - in practice you'd want to associate message IDs with widgets
        for i, widget in enumerate(self._messages):
            if (i == len(self._messages) - 1 and
                    message.source in (AIMessageSource.AI, AIMessageSource.REASONING)):
                widget.set_content(message.content, message.source, message.timestamp, message.model)
                break

        # Scroll to bottom if auto-scrolling is enabled
        if self._auto_scroll:
            self._scroll_to_bottom()

    async def _process_pending_update(self) -> None:
        """Process any pending message update."""
        if not self._pending_message:
            return

        await self._update_message(self._pending_message)
        self._pending_message = None
        self._last_update_time = time.time() * 1000

    async def _on_message_updated(self, message: AIMessage) -> None:
        """
        Handle a message being updated with throttling.

        The first update is processed immediately, subsequent updates
        are throttled to once every 100ms.

        Args:
            message: The message that was updated
        """
        # Make a deep copy of the message to prevent any reference issues
        message_copy = message.copy()

        # If no pending message exists, process immediately
        if self._pending_message is None:
            await self._update_message(message_copy)
            self._last_update_time = time.time() * 1000  # Current time in ms
            return

        # Store the pending message (overwrite any existing pending message)
        self._pending_message = message_copy

        # If the timer is not active, start it
        if not self._update_timer.isActive():
            # Calculate time until next update (aim for 100ms between updates)
            current_time = time.time() * 1000
            elapsed = current_time - self._last_update_time
            delay = max(0, 100 - elapsed)

            self._update_timer.start(int(delay))

    async def _on_message_completed(self, message: AIMessage) -> None:
        """
        Handle a message being completed.

        This cancels any pending updates and immediately updates with
        the completed message.

        Args:
            message: The message that was completed
        """
        # Cancel any pending update
        if self._update_timer.isActive():
            self._update_timer.stop()

        # Clear the pending message state
        self._pending_message = None

        # Update with the completed message immediately
        await self._update_message(message)

    async def _on_request_completed(self) -> None:
        """
        Handle completed AI request.
        """
        # Update status bar with token counts
        self._is_streaming = False
        self._input.set_streaming(False)

        # Reset message update throttling state
        self._pending_message = None
        if self._update_timer.isActive():
            self._update_timer.stop()

        self.status_updated.emit()

    def _handle_language_changed(self) -> None:
        """Update language-specific elements when language changes."""
        # Update input widget streaming state text
        self._input.set_streaming(self._is_streaming)

        # Emit signal for status update
        self.status_updated.emit()

    def _handle_selection_scroll(self, mouse_pos: QPoint) -> None:
        """Begin scroll handling for selection drag."""
        viewport_pos = self._scroll_area.viewport().mapFromGlobal(mouse_pos)

        if not self._scroll_timer.isActive():
            self._scroll_timer.start()

        self._last_mouse_pos = viewport_pos

    def _stop_scroll(self) -> None:
        """Stop any ongoing selection scrolling."""
        if self._scroll_timer.isActive():
            self._scroll_timer.stop()

        self._last_mouse_pos = None

    def _update_scroll(self) -> None:
        """Update scroll position based on mouse position."""
        if self._last_mouse_pos is None:
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

    def _on_scroll_value_changed(self, value: int) -> None:
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

    def _on_scroll_range_changed(self, _minimum: int, maximum: int) -> None:
        """Handle the scroll range changing."""
        # If we're set to auto-scroll then do so now
        total_height = self._messages_container.height()
        input_height = self._input.height()
        last_insertion_point = total_height - input_height - 2 * self._messages_layout.spacing()

        current_pos = self._scroll_area.verticalScrollBar().value()

        if self._auto_scroll:
            self._scroll_to_bottom()

        elif current_pos > last_insertion_point:
            if self._last_scroll_maximum != maximum:
                max_diff = maximum - self._last_scroll_maximum
                self._scroll_area.verticalScrollBar().setValue(current_pos + max_diff)

        self._last_scroll_maximum = maximum

    def _scroll_to_bottom(self) -> None:
        """Scroll to the bottom of the content."""
        scrollbar = self._scroll_area.verticalScrollBar()
        scrollbar.setValue(scrollbar.maximum())

    def _install_activation_tracking(self, widget: QWidget) -> None:
        """
        Install event filter on widget and all its children recursively.

        Call this for any new widgets added to the system widget.

        Args:
            widget: Widget to track for activation events
        """
        widget.installEventFilter(self._event_filter)
        child: QWidget
        for child in widget.findChildren(QWidget):
            cast(QWidget, child).installEventFilter(self._event_filter)

    def _handle_widget_activation(self, widget: QWidget) -> None:
        """
        Handle activation of a widget, focusing the associated message.

        Args:
            widget: The widget that was activated
        """
        # Emit activated signal to let the tab know this system was clicked
        self.activated.emit()

        # Find the SystemMessage that contains this widget
        message_widget = self._find_system_message(widget)
        if message_widget is None:
            return

        if message_widget.is_focused():
            return

        # Set focus on the new message
        if message_widget in self._messages:
            self._focused_message_index = self._messages.index(message_widget)
            message_widget.set_focused(True)

        else:
            self._focused_message_index = -1
            self._input.set_focused(True)

    def _handle_widget_deactivation(self, widget: QWidget) -> None:
        """
        Handle deactivation of a widget, checking if focus is leaving the associated message.

        Args:
            widget: The widget that lost focus
        """
        # Find the SystemMessage that contains this widget
        message_widget = self._find_system_message(widget)
        if message_widget is None:
            return

        # Remove focus from the currently focused message
        if self._focused_message_index != -1:
            self._messages[self._focused_message_index].set_focused(False)

        else:
            self._input.set_focused(False)

    def _find_system_message(self, widget: QWidget) -> SystemMessage | None:
        """
        Find the SystemMessage that contains the given widget.

        Args:
            widget: The widget to find the containing SystemMessage for

        Returns:
            The SystemMessage containing the widget, or None if not found
        """
        current: QObject = widget
        while current:
            if isinstance(current, SystemMessage):
                return current

            current = current.parent()

        return None

    def navigate_to_next_message(self) -> bool:
        """Navigate to the next message or input box if possible."""
        # If input box is focused, do nothing
        if self._focused_message_index == -1:
            return False

        # If we're at the last message, move to input box
        if self._focused_message_index == len(self._messages) - 1:
            self._messages[self._focused_message_index].set_focused(False)
            self._focused_message_index = -1
            self._focus_message()
            return True

        # Otherwise move to next message
        if self._focused_message_index < len(self._messages) - 1:
            self._messages[self._focused_message_index].set_focused(False)
            self._focused_message_index += 1
            self._focus_message()
            return True

        return False

    def navigate_to_previous_message(self) -> bool:
        """Navigate to the previous message if possible."""
        # If input box is focused, move to the last message
        if self._focused_message_index == -1:
            if self._messages:
                self._input.set_focused(False)
                self._focused_message_index = len(self._messages) - 1
                self._focus_message()
                return True

            return False

        # Otherwise move to previous message if possible
        if self._focused_message_index > 0:
            self._messages[self._focused_message_index].set_focused(False)
            self._focused_message_index -= 1
            self._focus_message()
            return True

        return False

    def _focus_message(self) -> None:
        """Focus and highlight the specified message."""
        index = self._focused_message_index
        if 0 <= index < len(self._messages):
            self._messages[index].set_focused(True)
            self._scroll_to_message(self._messages[index])
            return

        self._input.set_focused(True)
        self._scroll_to_message(self._input)

    def _scroll_to_message(self, message: SystemMessage) -> None:
        """Ensure the message is visible in the scroll area."""
        # Get the position of the message in the scroll area
        message_pos = message.mapTo(self._messages_container, QPoint(0, 0))

        # Calculate the visible region
        scroll_value = self._scroll_area.verticalScrollBar().value()
        viewport_height = self._scroll_area.viewport().height()

        delta = message_pos.y() - scroll_value

        # Determine if scrolling is needed
        if delta < 0:
            # Message is above visible area
            self._scroll_area.verticalScrollBar().setValue(message_pos.y())

        elif delta + message.height() > viewport_height:
            # Message is below visible area
            if message.height() > viewport_height:
                self._scroll_area.verticalScrollBar().setValue(message_pos.y())

            else:
                self._scroll_area.verticalScrollBar().setValue(message_pos.y() + message.height() - viewport_height + 10)

    def can_navigate_next_message(self) -> bool:
        """Check if navigation to next message is possible."""
        return not self._input.hasFocus()

    def can_navigate_previous_message(self) -> bool:
        """Check if navigation to previous message is possible."""
        return (
            self._input.hasFocus() or
            (0 <= self._focused_message_index < len(self._messages) and self._focused_message_index > 0)
        )

    def _handle_selection_changed(self, message_widget: SystemMessage, has_selection: bool) -> None:
        """Handle selection changes in message widgets."""
        if not has_selection:
            if self._message_with_selection:
                msg = self._message_with_selection
                self._message_with_selection = None
                msg.clear_selection()

            return

        if self._message_with_selection and self._message_with_selection != message_widget:
            self._message_with_selection.clear_selection()

        if message_widget == self._input:
            self._message_with_selection = None
            return

        self._message_with_selection = message_widget

    def has_selection(self) -> bool:
        """Check if any message has selected text."""
        return self._message_with_selection is not None and self._message_with_selection.has_selection()

    def _handle_edit_page_scroll(self) -> None:
        """
        Handle page up/down scroll requests.
        """
        # Input cursor has already moved - just ensure it's visible
        self._ensure_cursor_visible()

    def _ensure_cursor_visible(self) -> None:
        """Ensure the cursor remains visible when it moves."""
        total_height = sum(msg.sizeHint().height() + self._messages_layout.spacing() for msg in self._messages)
        input_cursor = self._input.cursor_rect()

        # Use scroll area's ensureVisible method which handles visibility calculations for us
        self._scroll_area.ensureVisible(
            input_cursor.x(),
            total_height + input_cursor.y(),
            1,
            50
        )

    def handle_find_scroll(self, widget: SystemMessage, section_num: int, position: int) -> None:
        """
        Handle scroll requests from find operations.

        Args:
            widget: Widget to scroll to
            section_num: Section number within the widget
            position: Text position within the section
        """
        # Get position relative to the message widget
        pos_in_message = widget.select_and_scroll_to_position(section_num, position)
        if pos_in_message == QPoint(0, 0) and section_num > 0:
            # Handle case where position wasn't found
            return

        # Map position from message widget to the scroll area's coordinate system
        # This is safe because we know the relationship between these widgets
        pos_in_scroll_area = widget.mapTo(self._scroll_area.widget(), pos_in_message)

        # Ensure the point is visible in the scroll area
        self._scroll_area.ensureVisible(
            pos_in_scroll_area.x(),  # x
            pos_in_scroll_area.y(),  # y
            10,  # xmargin
            50   # ymargin - provide some context around the match
        )

    def set_input_text(self, text: str) -> None:
        """Set the input text."""
        self._input.set_plain_text(text)
        self._input.setFocus()

    def _set_initial_focus(self) -> None:
        """Set initial focus to input area."""
        self._input.setFocus()

    def resizeEvent(self, event: QResizeEvent) -> None:
        """Handle resize events."""
        super().resizeEvent(event)

        if self._auto_scroll:
            self._scroll_to_bottom()

    def _handle_style_changed(self) -> None:
        factor = self._style_manager.zoom_factor()
        font = self.font()
        base_font_size = self._style_manager.base_font_size()
        font.setPointSizeF(base_font_size * factor)
        self.setFont(font)

        self._messages_container.setStyleSheet(f"""
            QWidget {{
                background-color: {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
                border: none;
            }}
        """)
        self._scroll_area.setStyleSheet(f"""
            QScrollArea {{
                background-color: {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
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
            QScrollBar::add-page:vertical, QScrollBar::sub-page:vertical {{
                background: none;
            }}
            QScrollBar::add-line:vertical, QScrollBar::sub-line:vertical {{
                height: 0px;
            }}
        """)

    def _show_system_context_menu(self, pos: QPoint) -> None:
        """
        Create and show the context menu at the given position.

        Args:
            pos: Local coordinates for menu position
        """
        menu = QMenu(self)
        strings = self._language_manager.strings()

        # Copy action
        copy_action = menu.addAction(strings.copy)
        copy_action.setEnabled(self.has_selection())
        copy_action.triggered.connect(self.copy)

        # Paste action
        paste_action = menu.addAction(strings.paste)
        paste_action.setEnabled(True)
        paste_action.triggered.connect(self.paste)
        menu.addSeparator()

        # Show menu at click position
        menu.exec_(self.mapToGlobal(pos))

    def can_cut(self) -> bool:
        """Check if cut operation is available."""
        return self._input.hasFocus() and self._input.text_cursor().hasSelection()

    def cut(self) -> None:
        """Cut selected text to clipboard."""
        if self._input.hasFocus():
            self._input.cut()

    def can_copy(self) -> bool:
        """Check if copy is available."""
        return (self._input.hasFocus() and self._input.text_cursor().hasSelection()) or self.has_selection()

    def copy(self) -> None:
        """Copy selected text to clipboard."""
        if self._input.hasFocus():
            self._input.copy()
        elif self._message_with_selection:
            self._message_with_selection.copy_selection()

    def can_paste(self) -> bool:
        """Check if paste is available."""
        return self._input.hasFocus()

    def paste(self) -> None:
        """Paste text from clipboard."""
        self._input.paste()

    def can_submit(self) -> bool:
        """Check if the current input can be submitted."""
        has_text = bool(self._input.to_plain_text())
        return has_text and not self._is_streaming

    def submit(self) -> None:
        """Submit current input text."""
        content = self._input.to_plain_text().strip()
        if not content:
            return

        if self._is_streaming:
            return

        self._input.clear()
        self._input.set_streaming(True)
        self._is_streaming = True
        self.status_updated.emit()

        message = AIMessage.create(AIMessageSource.USER, content)

    def create_state_metadata(self, temp_state: bool) -> Dict[str, Any]:
        """
        Create metadata dictionary capturing current widget state.

        Returns:
            Dictionary containing system state metadata
        """
        metadata: Dict[str, Any] = {}

        # Store current input content
        metadata["content"] = self._input.to_plain_text()
        metadata['cursor'] = self._get_cursor_position()

        return metadata

    def restore_from_metadata(self, metadata: Dict[str, Any]) -> None:
        """
        Restore widget state from metadata.

        Args:
            metadata: Dictionary containing state metadata
        """
        if not metadata:
            return

        # Restore input content if specified
        if "content" in metadata:
            self.set_input_text(metadata["content"])

        if "cursor" in metadata:
            self._set_cursor_position(metadata["cursor"])

        # Update our status
        self.status_updated.emit()

    def _set_cursor_position(self, position: Dict[str, int]) -> None:
        """Set cursor position in input area.

        Args:
            position: Dictionary with 'line' and 'column' keys
        """
        if not position:
            return

        self._input.set_cursor_position(position)

    def _get_cursor_position(self) -> Dict[str, int]:
        """Get current cursor position from input area.

        Returns:
            Dictionary with 'line' and 'column' keys
        """
        return self._input.get_cursor_position()

    def get_token_counts(self) -> Dict[str, int] | None:
        """
        Get the current token counts for status display.

        Returns:
            Dictionary with token count information
        """
        if self._ai_system is None:
            return None

        return self._ai_system.get_token_counts()

    def get_selected_text(self) -> str:
        """
        Get current selected text if any.

        Returns:
            The selected text or empty string
        """
        if self._message_with_selection:
            return self._message_with_selection.get_selected_text()

        if self._input.hasFocus():
            cursor = self._input.text_cursor()
            if cursor.hasSelection():
                text = cursor.selectedText()
                if '\u2029' not in text:
                    return text

                return text.replace('\u2029', '\n')

        return ""

    def find_text(self, text: str, forward: bool = True) -> Tuple[int, int]:
        """
        Find all instances of text and highlight them.

        Args:
            text: Text to search for
            forward: Whether to search forward from current position

        Returns:
            Tuple of (current_match, total_matches)
        """
        # Get searchable widgets
        widgets = self._messages + [self._input]

        # Clear existing highlights if search text changed
        if text != self._last_search:
            self._clear_highlights()
            self._matches = []
            self._current_widget_index = -1
            self._current_match_index = -1
            self._last_search = text

        # Find all matches if this is a new search
        if not self._matches and text:
            for widget in widgets:
                widget_matches = widget.find_text(text)
                if widget_matches:
                    self._matches.append((widget, widget_matches))

        if not self._matches:
            return 0, 0

        # Move to next/previous match
        if self._current_widget_index == -1:
            # First search - start at beginning or end depending on direction
            if forward:
                self._current_widget_index = 0
                self._current_match_index = 0

            else:
                self._current_widget_index = len(self._matches) - 1
                self._current_match_index = len(self._matches[self._current_widget_index][1]) - 1

        else:
            # Move to next/previous match
            if forward:
                self._current_match_index += 1
                # If we've reached the end of matches in current widget
                if self._current_match_index >= len(self._matches[self._current_widget_index][1]):
                    self._current_widget_index += 1
                    # If we've reached the end of widgets, wrap around
                    if self._current_widget_index >= len(self._matches):
                        self._current_widget_index = 0

                    self._current_match_index = 0

            else:
                self._current_match_index -= 1
                # If we've reached the start of matches in current widget
                if self._current_match_index < 0:
                    self._current_widget_index -= 1
                    # If we've reached the start of widgets, wrap around
                    if self._current_widget_index < 0:
                        self._current_widget_index = len(self._matches) - 1

                    self._current_match_index = len(self._matches[self._current_widget_index][1]) - 1

        # Highlight all matches
        self._highlight_matches()

        # Scroll to current match
        self._scroll_to_current_match()

        # Return current match status
        return self.get_match_status()

    def _highlight_matches(self) -> None:
        """Update the highlighting of all matches."""
        self._clear_highlights()

        if not self._matches:
            return

        # Get colors from style manager
        highlight_color = self._style_manager.get_color(ColorRole.TEXT_FOUND)
        dim_highlight_color = self._style_manager.get_color(ColorRole.TEXT_FOUND_DIM)

        # Highlight matches in each widget
        for widget_idx, (widget, matches) in enumerate(self._matches):
            # Set current_match_index to highlight the current match
            current_match_idx = self._current_match_index if widget_idx == self._current_widget_index else -1

            # Highlight matches in this widget
            widget.highlight_matches(
                matches,
                current_match_idx,
                highlight_color,
                dim_highlight_color
            )

            # Track highlighted widgets
            self._highlighted_widgets.add(widget)

    def _scroll_to_current_match(self) -> None:
        """Request scroll to ensure the current match is visible."""
        if not self._matches:
            return

        widget, matches = self._matches[self._current_widget_index]
        section_num, start, _ = matches[self._current_match_index]

        # Trigger scrolling to this position
        self.handle_find_scroll(widget, section_num, start)

    def _clear_highlights(self) -> None:
        """Clear all search highlights."""
        # Clear highlights from all tracked widgets
        for widget in self._highlighted_widgets:
            widget.clear_highlights()

        self._highlighted_widgets.clear()

    def get_match_status(self) -> Tuple[int, int]:
        """
        Get the current match status.

        Returns:
            Tuple of (current_match, total_matches)
        """
        if not self._matches:
            return 0, 0

        total_matches = sum(len(matches) for _, matches in self._matches)
        if self._current_widget_index == -1:
            return 0, total_matches

        current_match = sum(len(matches) for _, matches in self._matches[:self._current_widget_index])
        current_match += self._current_match_index + 1

        return current_match, total_matches

    def clear_find(self) -> None:
        """Clear all find state."""
        self._clear_highlights()
        self._matches = []
        self._current_widget_index = -1
        self._current_match_index = -1
        self._last_search = ""
