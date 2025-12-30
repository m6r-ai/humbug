"""Shell widget implementation for displaying shell command history."""

import logging
from typing import Dict, List, Tuple, Any, Set, cast

from PySide6.QtWidgets import (
    QWidget, QVBoxLayout, QScrollArea, QSizePolicy, QMenu
)
from PySide6.QtCore import QTimer, QPoint, Qt, Signal, QEvent, QObject
from PySide6.QtGui import QCursor, QResizeEvent

from humbug.color_role import ColorRole
from humbug.language.language_manager import LanguageManager
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.style_manager import StyleManager
from humbug.tabs.shell.shell_command_processor import ShellCommandProcessor
from humbug.tabs.shell.shell_command_registry import ShellCommandRegistry
from humbug.tabs.shell.shell_event import ShellEvent
from humbug.tabs.shell.shell_history_manager import ShellHistoryManager
from humbug.tabs.shell.shell_input import ShellInput
from humbug.tabs.shell.shell_message import ShellMessage


class ShellWidgetEventFilter(QObject):
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
            return False

        if event.type() == QEvent.Type.FocusOut:
            # Emit a widget deactivated signal
            self.widget_deactivated.emit(obj)
            return False

        return super().eventFilter(obj, event)


class ShellWidget(QWidget):
    """Widget for displaying shell with message history and input."""

    # Signal to notify tab of status changes
    status_updated = Signal()

    # Signal to request scrolling to a specific widget and position
    scroll_requested = Signal(QWidget, int)  # Widget to scroll to, position within widget

    # Emits when parent should be activated by user interaction
    activated = Signal()

    def __init__(
        self,
        parent: QWidget | None = None,
    ) -> None:
        """
        Initialize the shell widget.

        Args:
            parent: Optional parent widget
        """
        super().__init__(parent)
        self._logger = logging.getLogger("ShellWidget")

        # Create shell history manager
        self._command_registry = ShellCommandRegistry()
        self._history_manager = ShellHistoryManager()
        self._history_manager.history_updated.connect(self.load_messages)

        self._mindspace_manager = MindspaceManager()
        self._style_manager = StyleManager()

        # Widget tracking
        self._messages: List[ShellMessage] = []
        self._message_with_selection: ShellMessage | None = None

        # Initialize tracking variables
        self._auto_scroll = True
        self._last_scroll_maximum = 0

        # Create layout
        shell_layout = QVBoxLayout(self)
        self.setLayout(shell_layout)
        shell_layout.setContentsMargins(0, 0, 0, 0)
        shell_layout.setSpacing(0)

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
        self._input = ShellInput(self._messages_container)
        self._input.cursor_position_changed.connect(self._ensure_cursor_visible)
        self._input.selection_changed.connect(
            lambda has_selection: self._on_selection_changed(self._input, has_selection)
        )
        self._input.page_key_scroll_requested.connect(self._on_page_key_scroll_requested)
        self._input.scroll_requested.connect(self._on_scroll_requested)
        self._input.mouse_released.connect(self._stop_scroll)

        # Connect input to command handling
        self._input.command_submitted.connect(self._process_command)
        self._input.tab_completion_requested.connect(self._on_tab_completion_requested)

        # Create command processor
        self._command_processor = ShellCommandProcessor()

        spacing = int(self._style_manager.message_bubble_spacing())
        self._messages_layout.setSpacing(spacing)
        self._messages_layout.setContentsMargins(spacing, spacing, spacing, spacing)
        self._messages_layout.addStretch()
        self._messages_layout.addWidget(self._input)

        self._messages_container.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)
        self._scroll_area.setWidget(self._messages_container)

        # Add the scroll area to the main layout
        shell_layout.addWidget(self._scroll_area)

        # Setup signals for search highlights
        self._search_highlights: Dict[ShellMessage, List[Tuple[int, int]]] = {}

        # Tracking for spotlighted message
        self._spotlighted_message_index = -1

        self._language_manager = LanguageManager()

        # Create timer for scrolling
        self._scroll_timer = QTimer(self)
        self._scroll_timer.setInterval(16)  # ~60fps
        self._scroll_timer.timeout.connect(self._update_scroll)
        self._last_mouse_pos: QPoint | None = None

        # Timer for smooth animated scrolling
        self._smooth_scroll_timer = QTimer(self)
        self._smooth_scroll_timer.setInterval(16)  # ~60fps
        self._smooth_scroll_timer.timeout.connect(self._update_smooth_scroll)
        self._smooth_scroll_target: int = 0
        self._smooth_scroll_start: int = 0
        self._smooth_scroll_distance: int = 0
        self._smooth_scroll_duration: int = 500  # ms
        self._smooth_scroll_time: int = 0

        # Setup context menu
        self.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.customContextMenuRequested.connect(self._show_shell_context_menu)

        # Connect to the vertical scrollbar's change signals
        self._scroll_area.verticalScrollBar().valueChanged.connect(self._on_scroll_value_changed)
        self._scroll_area.verticalScrollBar().rangeChanged.connect(self._on_scroll_range_changed)

        self._style_manager.style_changed.connect(self._on_style_changed)
        self._on_style_changed()

        # Find functionality
        self._matches: List[Tuple[ShellMessage, List[Tuple[int, int]]]] = []
        self._current_widget_index = -1
        self._current_match_index = -1
        self._last_search = ""
        self._highlighted_widgets: Set[ShellMessage] = set()

        # Set up activation tracking
        self._event_filter = ShellWidgetEventFilter(self)
        self._event_filter.widget_activated.connect(self._on_widget_activated)
        self._event_filter.widget_deactivated.connect(self._on_widget_deactivated)
        self._install_activation_tracking(self._input)
        self._install_activation_tracking(self._messages_container)

        # Load shell messages when initialized
        self.load_messages()

    def _on_tab_completion_requested(
        self,
        current_text: str,
        is_continuation: bool,
        move_foward: bool,
        cursor_position: int
    ) -> None:
        """
        Handle tab completion request for the current input.

        Args:
            current_text: Current input text
            is_continuation: Whether this is a continuation of previous tab presses
            move_foward: Whether to move forward or backward in the completion list
            cursor_position: Current cursor position
        """
        # Ask command processor for completion
        result = self._command_processor.handle_tab_completion(current_text, is_continuation, move_foward, cursor_position)
        if result.success and result.replacement is not None:
            # Apply the completion
            self._input.apply_completion(result)

    def _update_command_history(self) -> None:
        """
        Update command history from shell messages.

        Extracts all user messages from shell history and adds them
        to the command input history for up/down arrow navigation.
        """
        # Get user commands from shell history (newest first)
        user_commands = self._history_manager.get_user_commands()

        # Limit to a reasonable size (e.g., 100 items)
        if len(user_commands) > 100:
            user_commands = user_commands[:100]

        # Set the command history in the input widget
        self._input.set_command_history(user_commands)

    def load_messages(self) -> None:
        """
        Load shell messages from history, optimizing to only remove
        early messages and add new messages at the end.

        This optimized implementation assumes messages are only ever removed from the beginning
        of the list or added to the end of the list, and are never reordered.
        """
        # Get shell messages from history manager
        shell_messages = self._history_manager.get_messages()

        # Handle empty cases
        if not shell_messages:
            # Clear all existing messages if there are none from the shell
            for msg in self._messages:
                self._messages_layout.removeWidget(msg)
                msg.deleteLater()

            self._messages.clear()
            return

        # Handle case where we don't have any messages yet
        if not self._messages:
            # Simply add all messages
            for message in shell_messages:
                self._add_shell_message(message)

            self._auto_scroll = True
            self._scroll_to_bottom()

            # Update command history
            self._update_command_history()
            return

        # Find matching messages between existing UI messages and new shell messages
        # First, check if first shell message exists in our UI message list
        first_shell_msg_id = shell_messages[0].message_id

        # Look for the first matching message ID in our existing messages
        first_match_index = -1
        for i, msg in enumerate(self._messages):
            if msg.message_id() == first_shell_msg_id:
                first_match_index = i
                break

        # If found, remove any UI messages before this match (older messages removed from shell)
        if first_match_index > 0:
            # Remove messages that are no longer at the beginning of the shell messages
            for i in range(first_match_index):
                msg = self._messages[0]  # Always remove the first message
                self._messages_layout.removeWidget(msg)
                msg.deleteLater()
                self._messages.pop(0)  # Remove from list

        # Find how many existing messages we have that match shell messages
        matching_count = min(len(self._messages), len(shell_messages))

        # Check if we need to add new messages at the end
        if len(shell_messages) > matching_count:
            # Add only the new messages that don't exist yet (at the end)
            for message in shell_messages[matching_count:]:
                self._add_shell_message(message)

        # Scroll to the latest message if we added new ones
        if len(shell_messages) > matching_count:
            self._auto_scroll = True
            self._scroll_to_bottom()

        # Update command history after loading messages
        self._update_command_history()

    def clear_history(self) -> None:
        """Clear both the message display and command input history."""
        # Clear shell history
        self._history_manager.clear_history()

        # Clear command history in input widget
        self._input.clear_command_history()

        # Clear message display (message widgets will be cleared when
        # load_messages is called with empty history)
        for msg in self._messages:
            self._messages_layout.removeWidget(msg)
            msg.deleteLater()

        self._messages.clear()

    def _add_shell_message(self, message: ShellEvent) -> None:
        """Add a message from the shell message history."""
        msg_widget = ShellMessage(self)
        msg_widget.selection_changed.connect(
            lambda has_selection: self._on_selection_changed(msg_widget, has_selection)
        )
        msg_widget.scroll_requested.connect(self._on_scroll_requested)
        msg_widget.mouse_released.connect(self._stop_scroll)

        # Set content using fields from ShellEvent model
        msg_widget.set_content(
            message.content,
            message.source,
            message.timestamp,
            message.message_id
        )

        # Add widget before input and the stretch
        count = self._messages_layout.count()
        self._messages_layout.insertWidget(count - 2, msg_widget)
        self._messages.append(msg_widget)

        self._install_activation_tracking(msg_widget)

    def _process_command(self, command_text: str) -> None:
        """
        Process a command entered by the user.

        Args:
            command_text: The command text to process
        """
        # Process the command (this will add the user message and any responses to shell history)
        self._command_processor.process_command(command_text)

        # Refresh the messages display
        self.load_messages()

    def submit(self) -> None:
        """Submit current input text as a user shell command."""
        content = self._input.to_plain_text().strip()
        if not content:
            return

        # Process as a command
        self._input.clear()
        self._process_command(content)

    def _on_scroll_requested(self, mouse_pos: QPoint) -> None:
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

    def _start_smooth_scroll(self, target_value: int) -> None:
        """
        Start smooth scrolling animation to target value.

        Args:
            target_value: Target scroll position
        """
        scrollbar = self._scroll_area.verticalScrollBar()

        # If we're already scrolling, stop the current animation
        if self._smooth_scroll_timer.isActive():
            self._smooth_scroll_timer.stop()

        # Set up the animation parameters
        self._smooth_scroll_start = scrollbar.value()
        self._smooth_scroll_target = target_value
        self._smooth_scroll_distance = target_value - self._smooth_scroll_start
        self._smooth_scroll_time = 0

        # Start the animation timer
        self._smooth_scroll_timer.start()

    def _update_smooth_scroll(self) -> None:
        """Update the smooth scrolling animation."""
        self._smooth_scroll_time += self._smooth_scroll_timer.interval()

        # Calculate progress (0 to 1)
        progress = min(1.0, self._smooth_scroll_time / self._smooth_scroll_duration)

        # Apply easing function (ease out cubic)
        t = 1 - (1 - progress) ** 3

        # Calculate new position
        new_position = self._smooth_scroll_start + int(self._smooth_scroll_distance * t)

        # Update scrollbar position
        scrollbar = self._scroll_area.verticalScrollBar()
        scrollbar.setValue(new_position)

        # Stop the timer when animation is complete
        if progress >= 1.0:
            self._smooth_scroll_timer.stop()

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

    def activate(self) -> None:
        """Activate the shell widget."""
        # If we have a spotlighted message then spotlight it
        if self._spotlighted_message_index != -1:
            self._messages[self._spotlighted_message_index].set_spotlighted(True)
            self._messages[self._spotlighted_message_index].setFocus()
            return

        self._input.set_spotlighted(True)
        self._input.setFocus()

    def _install_activation_tracking(self, widget: QWidget) -> None:
        """
        Install event filter on widget and all its children recursively.

        Call this for any new widgets added to the shell widget.

        Args:
            widget: Widget to track for activation events
        """
        widget.installEventFilter(self._event_filter)
        child: QWidget
        for child in widget.findChildren(QWidget):
            cast(QWidget, child).installEventFilter(self._event_filter)

    def _on_widget_activated(self, widget: QWidget) -> None:
        """
        Handle activation of a widget, spotlighting the associated message.

        Args:
            widget: The widget that was activated
        """
        # Emit activated signal to let the tab know this shell was clicked
        self.activated.emit()

        # If we are clicking the messages container, spotlight the last spotlighted message or input
        if widget == self._messages_container:
            self.activate()
            return

        # Find the ShellMessage that contains this widget
        message_widget = self._find_shell_message(widget)
        if message_widget is None:
            return

        if message_widget.is_spotlighted():
            return

        # Set spotlight on the new message
        if message_widget in self._messages:
            self._spotlighted_message_index = self._messages.index(message_widget)
            message_widget.set_spotlighted(True)
            return

        self._spotlighted_message_index = -1
        self._input.set_spotlighted(True)

    def _on_widget_deactivated(self, widget: QWidget) -> None:
        """
        Handle deactivation of a widget, checking if spotlight is leaving the associated message.

        Args:
            widget: The widget that lost focus
        """
        # Find the ShellMessage that contains this widget
        message_widget = self._find_shell_message(widget)
        if message_widget is None:
            return

        # Remove spotlight from the currently spotlighted message
        if self._spotlighted_message_index != -1:
            self._messages[self._spotlighted_message_index].set_spotlighted(False)

        else:
            self._input.set_spotlighted(False)

    def _find_shell_message(self, widget: QWidget) -> ShellMessage | None:
        """
        Find the ShellMessage that contains the given widget.

        Args:
            widget: The widget to find the containing ShellMessage for

        Returns:
            The ShellMessage containing the widget, or None if not found
        """
        current: QObject = widget
        while current:
            if isinstance(current, ShellMessage):
                return current

            current = current.parent()

        return None

    def navigate_to_next_message(self) -> bool:
        """Navigate to the next message or input box if possible."""
        # If input box is spotlighted, do nothing
        if self._spotlighted_message_index == -1:
            return False

        # If we're at the last message, move to input box
        if self._spotlighted_message_index == len(self._messages) - 1:
            self._messages[self._spotlighted_message_index].set_spotlighted(False)
            self._spotlighted_message_index = -1
            self._spotlight_message()
            return True

        # Otherwise move to next message
        if self._spotlighted_message_index < len(self._messages) - 1:
            self._messages[self._spotlighted_message_index].set_spotlighted(False)
            self._spotlighted_message_index += 1
            self._spotlight_message()
            return True

        return False

    def navigate_to_previous_message(self) -> bool:
        """Navigate to the previous message if possible."""
        # If input box is spotlighted, move to the last message
        if self._spotlighted_message_index == -1:
            if self._messages:
                self._input.set_spotlighted(False)
                self._spotlighted_message_index = len(self._messages) - 1
                self._spotlight_message()
                return True

            return False

        # Otherwise move to previous message if possible
        if self._spotlighted_message_index > 0:
            self._messages[self._spotlighted_message_index].set_spotlighted(False)
            self._spotlighted_message_index -= 1
            self._spotlight_message()
            return True

        return False

    def _spotlight_message(self) -> None:
        """Spotlight the specified message."""
        index = self._spotlighted_message_index
        if 0 <= index < len(self._messages):
            self._messages[index].set_spotlighted(True)
            self._messages[index].setFocus()
            self._scroll_to_message(self._messages[index])
            return

        self._input.set_spotlighted(True)
        self._input.setFocus()
        self._scroll_to_message(self._input)

    def _perform_scroll_to_position(self, message: ShellMessage, y_offset: int) -> None:
        """
        Scroll to position a message at a specific Y offset from the top of viewport.

        This is a low-level helper that performs the actual scrolling operation.

        Args:
            message: Message widget to scroll to
            y_offset: Offset from top of viewport (positive values move message down from top)
        """
        message_pos = message.mapTo(self._messages_container, QPoint(0, 0))
        scroll_value = message_pos.y() - y_offset

        # Clamp to valid range
        scrollbar = self._scroll_area.verticalScrollBar()
        scroll_value = max(scrollbar.minimum(), min(scrollbar.maximum(), scroll_value))

        self._start_smooth_scroll(scroll_value)

    def _scroll_to_message(self, message: ShellMessage) -> None:
        """Ensure the message is visible in the scroll area."""
        # Get the position of the message in the scroll area
        message_pos = message.mapTo(self._messages_container, QPoint(0, 0))

        # Calculate the visible region
        scroll_value = self._scroll_area.verticalScrollBar().value()
        viewport_height = self._scroll_area.viewport().height()

        delta = message_pos.y() - scroll_value

        message_spacing = int(self._style_manager.message_bubble_spacing())

        # Determine if scrolling is needed
        if delta < 0:
            # Message is above visible area
            self._perform_scroll_to_position(message, message_spacing)

        elif delta + message.height() > viewport_height:
            # Message is below visible area
            if message.height() > viewport_height:
                # Message is taller than viewport, position at top
                self._perform_scroll_to_position(message, message_spacing)

            else:
                # Message fits in viewport, position at bottom
                bottom_offset = viewport_height - message.height() - message_spacing
                self._perform_scroll_to_position(message, bottom_offset)

    def can_navigate_next_message(self) -> bool:
        """Check if navigation to next message is possible."""
        return not self._input.hasFocus()

    def can_navigate_previous_message(self) -> bool:
        """Check if navigation to previous message is possible."""
        return (
            self._input.hasFocus() or
            (0 <= self._spotlighted_message_index < len(self._messages) and self._spotlighted_message_index > 0)
        )

    def _on_selection_changed(self, message_widget: ShellMessage, has_selection: bool) -> None:
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

    def _on_page_key_scroll_requested(self) -> None:
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

    def set_input_text(self, text: str) -> None:
        """Set the input text."""
        self._input.set_plain_text(text)
        self._input.setFocus()

    def resizeEvent(self, event: QResizeEvent) -> None:
        """Handle resize events."""
        super().resizeEvent(event)

        if self._auto_scroll:
            self._scroll_to_bottom()

    def _build_widget_style(self) -> str:
        """Build styles for the log widget."""

        return f"""
            QWidget {{
                background-color: {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
            }}

            {self._style_manager.get_menu_stylesheet()}

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
        """

    def _build_shell_message_widget_styles(self) -> str:
        """Build styles for the main message frame."""
        style_manager = self._style_manager
        border_radius = int(self._style_manager.message_bubble_spacing())

        return f"""
            #ShellMessage {{
                margin: 0;
                border-radius: {border_radius}px;
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
                border: 1px solid {style_manager.get_color_str(ColorRole.MESSAGE_BORDER)};
            }}
            #ShellMessage[message_source="user"] {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_USER_BACKGROUND)};
                border: 1px solid {style_manager.get_color_str(ColorRole.MESSAGE_USER_BORDER)};
            }}
            #ShellMessage[border="spotlighted"] {{
                border: 2px solid {style_manager.get_color_str(ColorRole.MESSAGE_SPOTLIGHTED)};
            }}

            #ShellMessage #_header {{
                background-color: transparent;
                border: none;
                border-radius: 0;
                padding: 0;
                margin: 0;
            }}

            #ShellMessage #_role_label {{
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                margin: 0;
                padding: 0;
                border: none;
                background-color: transparent;
            }}
            #ShellMessage #_role_label[message_source="user"] {{
                color: {style_manager.get_color_str(ColorRole.MESSAGE_USER)};
            }}
            #ShellMessage #_role_label[message_source="success"] {{
                color: {style_manager.get_color_str(ColorRole.MESSAGE_SYSTEM_SUCCESS)};
            }}
            #ShellMessage #_role_label[message_source="error"] {{
                color: {style_manager.get_color_str(ColorRole.MESSAGE_SYSTEM_ERROR)};
            }}

            #ShellMessage #_text_area {{
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                selection-background-color: {style_manager.get_color_str(ColorRole.TEXT_SELECTED)};
                border: none;
                border-radius: 0;
                padding: 0;
                margin: 0;
                background-color: transparent;
            }}

            #ShellMessage #_text_area QScrollBar:horizontal {{
                height: 12px;
                background: {style_manager.get_color_str(ColorRole.SCROLLBAR_BACKGROUND)};
            }}
            #ShellMessage #_text_area QScrollBar::handle:horizontal {{
                background: {style_manager.get_color_str(ColorRole.SCROLLBAR_HANDLE)};
                min-width: 20px;
            }}
            #ShellMessage #_text_area QScrollBar::add-page:horizontal,
            #ShellMessage #_text_area QScrollBar::sub-page:horizontal {{
                background: none;
            }}
            #ShellMessage #_text_area QScrollBar::add-line:horizontal,
            #ShellMessage #_text_area QScrollBar::sub-line:horizontal {{
                width: 0px;
            }}
        """

    def _on_style_changed(self) -> None:
        """Handle style changes by updating fonts and stylesheets."""
        factor = self._style_manager.zoom_factor()
        font = self.font()
        base_font_size = self._style_manager.base_font_size()
        font.setPointSizeF(base_font_size * factor)
        self.setFont(font)

        for message in self._messages:
            message.apply_style()

        stylesheet_parts = [
            self._build_widget_style(),
            self._build_shell_message_widget_styles()
        ]

        shared_stylesheet = "\n".join(stylesheet_parts)
        self.setStyleSheet(shared_stylesheet)

    def _show_shell_context_menu(self, pos: QPoint) -> None:
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
            return

        if self._message_with_selection:
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
        return has_text and self._mindspace_manager.has_mindspace()

    def create_state_metadata(self, _temp_state: bool) -> Dict[str, Any]:
        """
        Create metadata dictionary capturing current widget state.

        Returns:
            Dictionary containing shell state metadata
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

        # Refresh messages
        self.load_messages()

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

        self._highlight_matches()
        self._scroll_to_current_match()
        return self.get_match_status()

    def _highlight_matches(self) -> None:
        """Update the highlighting of all matches."""
        self._clear_highlights()

        # Highlight matches in each widget
        for widget_idx, (widget, matches) in enumerate(self._matches):
            # Set current_match_index to highlight the current match
            current_match_idx = self._current_match_index if widget_idx == self._current_widget_index else -1

            # Highlight matches in this widget
            widget.highlight_matches(matches, current_match_idx)

            # Track highlighted widgets
            self._highlighted_widgets.add(widget)

    def _handle_find_scroll(self, widget: ShellMessage, position: int) -> None:
        """
        Handle scroll requests from find operations.

        Args:
            widget: Widget to scroll to
            position: Text position within the section
        """
        # Get position relative to the message widget
        pos_in_message = widget.select_and_scroll_to_position(position)

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

    def _scroll_to_current_match(self) -> None:
        """Request scroll to ensure the current match is visible."""
        widget, matches = self._matches[self._current_widget_index]
        start, _ = matches[self._current_match_index]

        # Trigger scrolling to this position
        self._handle_find_scroll(widget, start)

    def _clear_highlights(self) -> None:
        """Clear all highlights from all widgets."""
        for widget in self._highlighted_widgets:
            widget.clear_highlights()

        self._highlighted_widgets.clear()

    def clear_highlights(self) -> None:
        """Public method to clear highlights."""
        self._clear_highlights()
        self._matches = []
        self._current_widget_index = -1
        self._current_match_index = -1
        self._last_search = ""

    def get_match_status(self) -> Tuple[int, int]:
        """
        Get current match status.

        Returns:
            Tuple of (current_match, total_matches)
        """
        # Calculate total matches
        total_matches = sum(len(m[1]) for m in self._matches)

        # Calculate current match
        if self._current_widget_index == -1 or self._current_match_index == -1:
            return 0, total_matches

        current_global_match = sum(len(m[1]) for m in self._matches[:self._current_widget_index]) + self._current_match_index + 1

        return current_global_match, total_matches
