"""Log widget implementation for displaying mindspace message log."""

import logging
from typing import Dict, List, Tuple, Any, Set

from PySide6.QtWidgets import (
    QWidget, QVBoxLayout, QScrollArea, QSizePolicy, QMenu
)
from PySide6.QtCore import QTimer, QPoint, Qt, Signal, QObject
from PySide6.QtGui import QCursor, QResizeEvent

from humbug.color_role import ColorRole
from humbug.language.language_manager import LanguageManager
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.mindspace.mindspace_message import MindspaceMessage
from humbug.style_manager import StyleManager
from humbug.tabs.log.log_message import LogMessage


class LogWidget(QWidget):
    """Widget for displaying mindspace message log with message history."""

    # Signal to notify tab of status changes
    status_updated = Signal()

    # Signal to request scrolling to a specific widget and position
    scroll_requested = Signal(QWidget, int)  # Widget to scroll to, position within widget

    # Signal to notify tab when content is updated while user is scrolled up
    update_label = Signal()

    # Emits when the has-seen-latest-update state changes
    has_seen_latest_update_changed = Signal(bool)

    def __init__(
        self,
        parent: QWidget | None = None,
    ) -> None:
        """
        Initialize the log widget.

        Args:
            parent: Optional parent widget
        """
        super().__init__(parent)
        self._logger = logging.getLogger("LogWidget")

        self._mindspace_manager = MindspaceManager()
        self._mindspace_manager.interactions_updated.connect(self.load_messages)

        self._style_manager = StyleManager()

        # Widget tracking
        self._messages: List[LogMessage] = []
        self._message_with_selection: LogMessage | None = None

        # Initialize tracking variables
        self._auto_scroll = True
        self._last_scroll_maximum = 0

        # Create layout
        log_layout = QVBoxLayout(self)
        self.setLayout(log_layout)
        log_layout.setContentsMargins(0, 0, 0, 0)
        log_layout.setSpacing(0)

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

        spacing = int(self._style_manager.message_bubble_spacing())
        self._messages_layout.setSpacing(spacing)
        self._messages_layout.setContentsMargins(spacing, spacing, spacing, spacing)
        self._messages_layout.addStretch()

        self._messages_container.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)
        self._scroll_area.setWidget(self._messages_container)

        # Add the scroll area to the main layout
        log_layout.addWidget(self._scroll_area)

        # Setup signals for search highlights
        self._search_highlights: Dict[LogMessage, List[Tuple[int, int]]] = {}

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
        self.customContextMenuRequested.connect(self._show_log_context_menu)

        # Connect to the vertical scrollbar's change signals
        self._scroll_area.verticalScrollBar().valueChanged.connect(self._on_scroll_value_changed)
        self._scroll_area.verticalScrollBar().rangeChanged.connect(self._on_scroll_range_changed)

        self._style_manager.style_changed.connect(self._on_style_changed)
        self._on_style_changed()

        # Find functionality
        self._matches: List[Tuple[LogMessage, List[Tuple[int, int]]]] = []
        self._current_widget_index = -1
        self._current_match_index = -1
        self._last_search = ""
        self._highlighted_widgets: Set[LogMessage] = set()

        # Load messages when initialized
        if self._mindspace_manager.has_mindspace():
            self.load_messages()

    def load_messages(self) -> None:
        """
        Load mindspace messages, optimizing to only remove early messages 
        and add new messages at the end.

        This optimized implementation assumes messages are only ever removed from the beginning
        of the list or added to the end of the list, and are never reordered.
        """
        if not self._mindspace_manager.has_mindspace():
            return

        # Get messages from mindspace manager
        mindspace_messages = self._mindspace_manager.get_interactions()

        # Handle empty cases
        if not mindspace_messages:
            # Clear all existing messages if there are none from the system
            for msg in self._messages:
                self._messages_layout.removeWidget(msg)
                msg.deleteLater()

            self._messages.clear()
            return

        # Handle case where we don't have any messages yet
        if not self._messages:
            # Simply add all messages
            for message in mindspace_messages:
                self._add_log_message(message)

            self._auto_scroll = True
            self._scroll_to_bottom()
            return

        # Find matching messages between existing UI messages and new mindspace messages
        # First, check if first mindspace message exists in our UI message list
        first_mindspace_msg_id = mindspace_messages[0].message_id

        # Look for the first matching message ID in our existing messages
        first_match_index = -1
        for i, msg in enumerate(self._messages):
            if msg.message_id() == first_mindspace_msg_id:
                first_match_index = i
                break

        # If found, remove any UI messages before this match (older messages removed from system)
        if first_match_index > 0:
            # Remove messages that are no longer at the beginning of the mindspace interactions
            for i in range(first_match_index):
                msg = self._messages[0]  # Always remove the first message
                self._messages_layout.removeWidget(msg)
                msg.deleteLater()
                self._messages.pop(0)  # Remove from list

        # Find how many existing messages we have that match mindspace messages
        matching_count = min(len(self._messages), len(mindspace_messages))

        # Check if we need to add new messages at the end
        if len(mindspace_messages) > matching_count:
            # Add only the new messages that don't exist yet (at the end)
            for message in mindspace_messages[matching_count:]:
                self._add_log_message(message)

        # Scroll to bottom if in auto-scroll mode, otherwise mark tab as updated
        if len(mindspace_messages) > matching_count:
            if self._auto_scroll:
                self._scroll_to_bottom()
            else:
                # User is scrolled up, notify them there's new content below
                self.update_label.emit()

    def _add_log_message(self, message: MindspaceMessage) -> None:
        """Add a message from the mindspace message log."""
        msg_widget = LogMessage(
            message.content,
            message.level,
            message.timestamp,
            message.message_id,
            self
        )
        msg_widget.selection_changed.connect(
            lambda has_selection: self._on_selection_changed(msg_widget, has_selection)
        )
        msg_widget.scroll_requested.connect(self._on_scroll_requested)
        msg_widget.mouse_released.connect(self._stop_scroll)

        # Add widget before the stretch
        count = self._messages_layout.count()
        self._messages_layout.insertWidget(count - 1, msg_widget)
        self._messages.append(msg_widget)

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

        self.has_seen_latest_update_changed.emit(at_bottom)

    def _on_scroll_range_changed(self, _minimum: int, maximum: int) -> None:
        """Handle the scroll range changing."""
        # If we're set to auto-scroll then do so now
        current_pos = self._scroll_area.verticalScrollBar().value()

        if self._auto_scroll:
            self._scroll_to_bottom()

        elif current_pos < maximum:
            if self._last_scroll_maximum != maximum:
                max_diff = maximum - self._last_scroll_maximum
                self._scroll_area.verticalScrollBar().setValue(current_pos + max_diff)

        self._last_scroll_maximum = maximum

    def _scroll_to_bottom(self) -> None:
        """Scroll to the bottom of the content."""
        scrollbar = self._scroll_area.verticalScrollBar()
        scrollbar.setValue(scrollbar.maximum())

    def set_active(self, widget: QWidget, active: bool) -> None:
        """
        Handle tab activation state changes.

        Args:
            widget: The widget that triggered the activation change
            active: True if the tab is now active, False otherwise
        """
        if not active:
            self._deactivate_widget(widget)
            return

        self._activate_widget(widget)

    def _activate_widget(self, widget: QWidget) -> None:
        """
        Handle activation of a widget, spotlighting the associated message.

        Args:
            widget: The widget that was activated
        """
        # Find the LogMessage that contains this widget
        message_widget = self._find_log_message(widget)
        if message_widget is None:
            # We couldn't find it so active the last spotlighted message or input
            if self._spotlighted_message_index != -1:
                self._messages[self._spotlighted_message_index].set_spotlighted(True)
                self._messages[self._spotlighted_message_index].setFocus()
                return

            # Otherwise spotlight the first message if available
            if self._messages:
                self._spotlighted_message_index = 0
                self._messages[0].set_spotlighted(True)
                self._messages[0].setFocus()

            return

        if message_widget.is_spotlighted():
            return

        # Remove spotlight from the currently spotlighted message
        if self._spotlighted_message_index != -1:
            self._messages[self._spotlighted_message_index].set_spotlighted(False)

        # Set spotlight on the new message
        if message_widget in self._messages:
            self._spotlighted_message_index = self._messages.index(message_widget)
            message_widget.set_spotlighted(True)

    def _deactivate_widget(self, widget: QWidget) -> None:
        """
        Handle deactivation of a widget, checking if spotlight is leaving the associated message.

        Args:
            widget: The widget that lost focus
        """
        # Find the LogMessage that contains this widget
        message_widget = self._find_log_message(widget)
        if message_widget is None:
            return

        # Remove spotlight from the currently spotlighted message
        if self._spotlighted_message_index != -1:
            self._messages[self._spotlighted_message_index].set_spotlighted(False)

    def _find_log_message(self, widget: QWidget) -> LogMessage | None:
        """
        Find the LogMessage that contains the given widget.

        Args:
            widget: The widget to find the containing LogMessage for

        Returns:
            The LogMessage containing the widget, or None if not found
        """
        current: QObject = widget
        while current:
            if isinstance(current, LogMessage):
                return current

            current = current.parent()

        return None

    def navigate_to_next_message(self) -> bool:
        """Navigate to the next message if possible."""
        if not self._messages:
            return False

        # If no message is spotlighted, spotlight the first one
        if self._spotlighted_message_index == -1:
            self._spotlighted_message_index = 0
            self._spotlight_message()
            return True

        # Otherwise move to next message if possible
        if self._spotlighted_message_index < len(self._messages) - 1:
            self._messages[self._spotlighted_message_index].set_spotlighted(False)
            self._spotlighted_message_index += 1
            self._spotlight_message()
            return True

        return False

    def navigate_to_previous_message(self) -> bool:
        """Navigate to the previous message if possible."""
        if not self._messages:
            return False

        # If no message is spotlighted, spotlight the last one
        if self._spotlighted_message_index == -1:
            self._spotlighted_message_index = len(self._messages) - 1
            self._spotlight_message()
            return True

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

    def _perform_scroll_to_position(self, message: LogMessage, y_offset: int) -> None:
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

    def _scroll_to_message(self, message: LogMessage) -> None:
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
        return (
            len(self._messages) > 0 and
            (self._spotlighted_message_index == -1 or self._spotlighted_message_index < len(self._messages) - 1)
        )

    def can_navigate_previous_message(self) -> bool:
        """Check if navigation to previous message is possible."""
        return (
            len(self._messages) > 0 and
            (self._spotlighted_message_index == -1 or self._spotlighted_message_index > 0)
        )

    def _on_selection_changed(self, message_widget: LogMessage, has_selection: bool) -> None:
        """Handle selection changes in message widgets."""
        if not has_selection:
            if self._message_with_selection:
                msg = self._message_with_selection
                self._message_with_selection = None
                msg.clear_selection()

            return

        if self._message_with_selection and self._message_with_selection != message_widget:
            self._message_with_selection.clear_selection()

        self._message_with_selection = message_widget

    def has_selection(self) -> bool:
        """Check if any message has selected text."""
        return self._message_with_selection is not None and self._message_with_selection.has_selection()

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

    def _build_log_message_styles(self) -> str:
        """Build styles for the main message frame."""
        style_manager = self._style_manager
        border_radius = int(style_manager.message_bubble_spacing())

        return f"""
            #LogMessage {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
                margin: 0;
                border-radius: {border_radius}px;
                border: 1px solid {style_manager.get_color_str(ColorRole.MESSAGE_BORDER)};
            }}
            #LogMessage[border="spotlighted"] {{
                border: 2px solid {style_manager.get_color_str(ColorRole.MESSAGE_SPOTLIGHTED)};
            }}

            #LogMessage #_header {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
                border: none;
                border-radius: 0;
                padding: 0;
                margin: 0;
            }}

            #LogMessage #_level_label {{
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                margin: 0;
                padding: 0;
                border: none;
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
            }}

            #LogMessage #_level_label[log_level="trace"] {{
                color: {style_manager.get_color_str(ColorRole.MESSAGE_TRACE)};
            }}
            #LogMessage #_level_label[log_level="info"] {{
                color: {style_manager.get_color_str(ColorRole.MESSAGE_INFORMATION)};
            }}
            #LogMessage #_level_label[log_level="warn"] {{
                color: {style_manager.get_color_str(ColorRole.MESSAGE_WARNING)};
            }}
            #LogMessage #_level_label[log_level="error"] {{
                color: {style_manager.get_color_str(ColorRole.MESSAGE_ERROR)};
            }}

            #LogMessage #_text_area {{
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                selection-background-color: {style_manager.get_color_str(ColorRole.TEXT_SELECTED)};
                border: none;
                border-radius: 0;
                padding: 0;
                margin: 0;
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
            }}

            #LogMessage #_text_area QScrollBar:horizontal {{
                height: 12px;
                background: {style_manager.get_color_str(ColorRole.SCROLLBAR_BACKGROUND)};
            }}

            #LogMessage #_text_area QScrollBar::handle:horizontal {{
                background: {style_manager.get_color_str(ColorRole.SCROLLBAR_HANDLE)};
                min-width: 20px;
            }}

            #LogMessage #_text_area QScrollBar::add-page:horizontal,
            #LogMessage #_text_area QScrollBar::sub-page:horizontal {{
                background: none;
            }}

            #LogMessage #_text_area QScrollBar::add-line:horizontal,
            #LogMessage #_text_area QScrollBar::sub-line:horizontal {{
                width: 0px;
            }}
        """

    def _on_style_changed(self) -> None:
        factor = self._style_manager.zoom_factor()
        font = self.font()
        base_font_size = self._style_manager.base_font_size()
        font.setPointSizeF(base_font_size * factor)
        self.setFont(font)

        for message in self._messages:
            message.apply_style()

        stylesheet_parts = [
            self._build_widget_style(),
            self._build_log_message_styles()
        ]

        shared_stylesheet = "\n".join(stylesheet_parts)
        self.setStyleSheet(shared_stylesheet)

    def _show_log_context_menu(self, pos: QPoint) -> None:
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

        # Show menu at click position
        menu.exec_(self.mapToGlobal(pos))

    def can_copy(self) -> bool:
        """Check if copy is available."""
        return self.has_selection()

    def copy(self) -> None:
        """Copy selected text to clipboard."""
        if self._message_with_selection:
            self._message_with_selection.copy_selection()

    def create_state_metadata(self, _temp_state: bool) -> Dict[str, Any]:
        """
        Create metadata dictionary capturing current widget state.

        Returns:
            Dictionary containing log state metadata
        """
        metadata: Dict[str, Any] = {}

        # Store current scroll position
        metadata["scroll_position"] = self._scroll_area.verticalScrollBar().value()

        # Store spotlighted message index
        metadata["spotlighted_message_index"] = self._spotlighted_message_index

        return metadata

    def restore_from_metadata(self, metadata: Dict[str, Any]) -> None:
        """
        Restore widget state from metadata.

        Args:
            metadata: Dictionary containing state metadata
        """
        if not metadata:
            return

        # Refresh messages if we have a mindspace
        if self._mindspace_manager.has_mindspace():
            self.load_messages()

        # Restore scroll position if specified
        if "scroll_position" in metadata:
            self._scroll_area.verticalScrollBar().setValue(metadata["scroll_position"])

        # Restore spotlighted message index if specified
        if "spotlighted_message_index" in metadata:
            self._spotlighted_message_index = metadata["spotlighted_message_index"]
            if 0 <= self._spotlighted_message_index < len(self._messages):
                self._messages[self._spotlighted_message_index].set_spotlighted(True)
                self._messages[self._spotlighted_message_index].setFocus()

    def get_selected_text(self) -> str:
        """
        Get current selected text if any.

        Returns:
            The selected text or empty string
        """
        if self._message_with_selection:
            return self._message_with_selection.get_selected_text()

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
        widgets = self._messages

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

    def _handle_find_scroll(self, widget: LogMessage, position: int) -> None:
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

    def get_log_info(self) -> Dict[str, Any]:
        """
        Get high-level metadata about the log.

        Returns:
            Dictionary containing log metadata
        """
        if not self._mindspace_manager.has_mindspace():
            raise ValueError("No mindspace available")

        messages = self._mindspace_manager.get_interactions()

        if not messages:
            return {
                "message_count": 0,
                "first_message_timestamp": None,
                "last_message_timestamp": None,
                "level_distribution": {
                    "trace": 0,
                    "info": 0,
                    "warn": 0,
                    "error": 0
                }
            }

        # Calculate level distribution
        level_distribution = {
            "trace": 0,
            "info": 0,
            "warn": 0,
            "error": 0
        }

        for msg in messages:
            level_str = msg.level.value
            if level_str in level_distribution:
                level_distribution[level_str] += 1

        return {
            "message_count": len(messages),
            "first_message_timestamp": messages[0].timestamp.isoformat(),
            "last_message_timestamp": messages[-1].timestamp.isoformat(),
            "level_distribution": level_distribution
        }

    def read_messages(
        self,
        start_index: int | None = None,
        end_index: int | None = None,
        levels: List[str] | None = None,
        limit: int | None = None,
        include_content: bool = True
    ) -> Dict[str, Any]:
        """
        Read log messages with filtering and pagination.

        Args:
            start_index: Starting message index (0-based, inclusive)
            end_index: Ending message index (0-based, inclusive)
            levels: List of log levels to include (trace, info, warn, error)
            limit: Maximum number of messages to return
            include_content: Include full message content

        Returns:
            Dictionary containing messages and metadata
        """
        if not self._mindspace_manager.has_mindspace():
            raise ValueError("No mindspace available")

        messages = self._mindspace_manager.get_interactions()

        # Apply range filtering
        if start_index is not None:
            start_index = max(0, start_index)

        else:
            start_index = 0

        if end_index is not None:
            end_index = min(len(messages) - 1, end_index)

        else:
            end_index = len(messages) - 1

        # Get messages in range
        filtered_messages = messages[start_index:end_index + 1]

        # Apply level filtering
        if levels:
            filtered_messages = [
                msg for msg in filtered_messages
                if msg.level.value in levels
            ]

        # Apply limit
        if limit and limit > 0:
            filtered_messages = filtered_messages[:limit]

        # Convert to dictionaries
        result_messages = []
        for msg in filtered_messages:
            msg_dict = {
                "index": messages.index(msg),  # Original index in full log
                "message_id": msg.message_id,
                "level": msg.level.value,
                "timestamp": msg.timestamp.isoformat()
            }

            # Optionally include content
            if include_content:
                msg_dict["content"] = msg.content

            result_messages.append(msg_dict)

        return {
            "total_messages": len(messages),
            "returned_count": len(result_messages),
            "start_index": start_index,
            "end_index": end_index,
            "messages": result_messages
        }

    def get_message_by_id_or_index(
        self,
        message_id: str | None = None,
        message_index: int | None = None
    ) -> Dict[str, Any] | None:
        """
        Get a specific log message by ID or index.

        Args:
            message_id: Message UUID
            message_index: Message index (0-based)

        Returns:
            Message dictionary or None if not found
        """
        if not self._mindspace_manager.has_mindspace():
            raise ValueError("No mindspace available")

        if message_id is None and message_index is None:
            raise ValueError("Must provide either message_id or message_index")

        messages = self._mindspace_manager.get_interactions()

        # Find by index
        if message_index is not None:
            if 0 <= message_index < len(messages):
                msg = messages[message_index]
                return {
                    "index": message_index,
                    "message_id": msg.message_id,
                    "level": msg.level.value,
                    "content": msg.content,
                    "timestamp": msg.timestamp.isoformat()
                }
            return None

        # Find by ID
        for idx, msg in enumerate(messages):
            if msg.message_id == message_id:
                return {
                    "index": idx,
                    "message_id": msg.message_id,
                    "level": msg.level.value,
                    "content": msg.content,
                    "timestamp": msg.timestamp.isoformat()
                }

        return None

    def search_messages(
        self,
        search_text: str,
        case_sensitive: bool = False,
        levels: List[str] | None = None,
        max_results: int = 50
    ) -> Dict[str, Any]:
        """
        Search for text across all log messages.

        Args:
            search_text: Text to search for
            case_sensitive: Case-sensitive search
            levels: Filter to specific log levels
            max_results: Maximum results to return

        Returns:
            Dictionary containing search results
        """
        if not self._mindspace_manager.has_mindspace():
            raise ValueError("No mindspace available")

        if not search_text:
            return {
                "search_text": search_text,
                "case_sensitive": case_sensitive,
                "total_matches": 0,
                "returned_count": 0,
                "matches": []
            }

        messages = self._mindspace_manager.get_interactions()

        # Prepare search
        search_str = search_text if case_sensitive else search_text.lower()
        matches = []

        for idx, msg in enumerate(messages):
            # Apply level filter
            if levels:
                if msg.level.value not in levels:
                    continue

            # Search in content
            content = msg.content if case_sensitive else msg.content.lower()
            pos = 0

            while True:
                pos = content.find(search_str, pos)
                if pos == -1:
                    break

                # Extract context (50 chars before and after)
                context_start = max(0, pos - 50)
                context_end = min(len(msg.content), pos + len(search_text) + 50)

                match_info = {
                    "message_index": idx,
                    "message_id": msg.message_id,
                    "level": msg.level.value,
                    "timestamp": msg.timestamp.isoformat(),
                    "match_position": pos,
                    "context_before": msg.content[context_start:pos],
                    "match_text": msg.content[pos:pos + len(search_text)],
                    "context_after": msg.content[pos + len(search_text):context_end]
                }

                matches.append(match_info)

                # Check if we've hit the limit
                if len(matches) >= max_results:
                    break

                pos += 1

            if len(matches) >= max_results:
                break

        return {
            "search_text": search_text,
            "case_sensitive": case_sensitive,
            "total_matches": len(matches),
            "returned_count": len(matches),
            "matches": matches
        }

    def scroll_to_message_by_id_or_index(
        self,
        message_id: str | None = None,
        message_index: int | None = None
    ) -> bool:
        """
        Scroll to a specific log message.

        Args:
            message_id: Message UUID
            message_index: Message index (0-based)

        Returns:
            True if successful, False if message not found
        """
        if message_id is None and message_index is None:
            raise ValueError("Must provide either message_id or message_index")

        # Find message index if ID provided
        if message_id is not None:
            messages = self._mindspace_manager.get_interactions()
            message_index = None
            for idx, msg in enumerate(messages):
                if msg.message_id == message_id:
                    message_index = idx
                    break

            if message_index is None:
                return False

        # Validate index
        if message_index is None or message_index < 0 or message_index >= len(self._messages):
            return False

        # Get the message widget
        message_widget = self._messages[message_index]

        # Scroll so message is at top of viewport with spacing
        bubble_spacing = self._style_manager.message_bubble_spacing()
        self._perform_scroll_to_position(message_widget, int(bubble_spacing))

        return True
