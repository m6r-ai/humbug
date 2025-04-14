"""System widget implementation for displaying system interaction history."""

import logging
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
from humbug.mindspace.system.system_message import SystemMessage as SystemMessageModel
from humbug.mindspace.system.system_message_source import SystemMessageSource


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
            parent: Optional parent widget
        """
        super().__init__(parent)
        self._logger = logging.getLogger("SystemWidget")

        self._mindspace_manager = MindspaceManager()
        self._mindspace_manager.system_interactions_updated.connect(self.load_system_interactions)

        # Widget tracking
        self._messages: List[SystemMessage] = []
        self._message_with_selection: SystemMessage | None = None

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
        self._search_highlights: Dict[SystemMessage, List[Tuple[int, int]]] = {}

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
        self._matches: List[Tuple[SystemMessage, List[Tuple[int, int]]]] = []
        self._current_widget_index = -1
        self._current_match_index = -1
        self._last_search = ""
        self._highlighted_widgets: Set[SystemMessage] = set()

        # Set up activation tracking
        self._event_filter = SystemWidgetEventFilter(self)
        self._event_filter.widget_activated.connect(self._handle_widget_activation)
        self._event_filter.widget_deactivated.connect(self._handle_widget_deactivation)
        self._install_activation_tracking(self._input)

        # Load system interactions when initialized
        if self._mindspace_manager.has_mindspace():
            self.load_system_interactions()

    def load_system_interactions(self) -> None:
        """
        Load system interaction messages from mindspace, optimizing to only remove
        early messages and add new messages at the end.

        This optimized implementation assumes messages are only ever removed from the beginning
        of the list or added to the end of the list, and are never reordered.
        """
        if not self._mindspace_manager.has_mindspace():
            return

        # Get system messages from mindspace manager
        system_messages = self._mindspace_manager.get_system_interactions()

        # Handle empty cases
        if not system_messages:
            # Clear all existing messages if there are none from the system
            for msg in self._messages:
                self._messages_layout.removeWidget(msg)
                msg.deleteLater()

            self._messages.clear()

            # Update status and return
            self.status_updated.emit()
            return

        # Handle case where we don't have any messages yet
        if not self._messages:
            # Simply add all messages
            for message in system_messages:
                self._add_system_message(message)

            # Scroll and update status
            self._auto_scroll = True
            self._scroll_to_bottom()
            self.status_updated.emit()
            return

        # Find matching messages between existing UI messages and new system messages
        # First, check if first system message exists in our UI message list
        first_system_msg_id = system_messages[0].message_id

        # Look for the first matching message ID in our existing messages
        first_match_index = -1
        for i, msg in enumerate(self._messages):
            if msg._message_id == first_system_msg_id:
                first_match_index = i
                break

        # If found, remove any UI messages before this match (older messages removed from system)
        if first_match_index > 0:
            # Remove messages that are no longer at the beginning of the system interactions
            for i in range(first_match_index):
                msg = self._messages[0]  # Always remove the first message
                self._messages_layout.removeWidget(msg)
                msg.deleteLater()
                self._messages.pop(0)  # Remove from list

        # Find how many existing messages we have that match system messages
        matching_count = min(len(self._messages), len(system_messages))

        # Check if we need to add new messages at the end
        if len(system_messages) > matching_count:
            # Add only the new messages that don't exist yet (at the end)
            for message in system_messages[matching_count:]:
                self._add_system_message(message)

        # Scroll to the latest message if we added new ones
        if len(system_messages) > matching_count:
            self._auto_scroll = True
            self._scroll_to_bottom()

        # Update status
        self.status_updated.emit()

    def _add_system_message(self, message: SystemMessageModel) -> None:
        """Add a message from the system message history."""
        msg_widget = SystemMessage(self)
        msg_widget.selectionChanged.connect(
            lambda has_selection: self._handle_selection_changed(msg_widget, has_selection)
        )
        msg_widget.scrollRequested.connect(self._handle_selection_scroll)
        msg_widget.mouseReleased.connect(self._stop_scroll)

        # Set content using fields from SystemMessage model
        msg_widget.set_content(
            message.content,
            message.source,
            message.timestamp,
            message.message_id
        )

        # Add widget before input and stretch
        count = self._messages_layout.count()
        self._messages_layout.insertWidget(count - 2, msg_widget)  # Insert before stretch and input
        self._messages.append(msg_widget)

        self._install_activation_tracking(msg_widget)

    def submit(self) -> None:
        """Submit current input text as a user system message."""
        content = self._input.to_plain_text().strip()
        if not content:
            return

        # Add user message directly to system interactions
        self._mindspace_manager.add_system_interaction(
            SystemMessageSource.USER,
            content
        )

        # Clear input
        self._input.clear()

        # Refresh the messages display
        self.load_system_interactions()

    def _handle_language_changed(self) -> None:
        """Update language-specific elements when language changes."""
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

    def handle_find_scroll(self, widget: SystemMessage, position: int) -> None:
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
        return has_text and self._mindspace_manager.has_mindspace()

    def create_state_metadata(self, _temp_state: bool) -> Dict[str, Any]:
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

        # Refresh messages if we have a mindspace
        if self._mindspace_manager.has_mindspace():
            self.load_system_interactions()

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

        # Highlight matches in each widget
        for widget_idx, (widget, matches) in enumerate(self._matches):
            # Set current_match_index to highlight the current match
            current_match_idx = self._current_match_index if widget_idx == self._current_widget_index else -1

            # Highlight matches in this widget
            widget.highlight_matches(matches, current_match_idx)

            # Track highlighted widgets
            self._highlighted_widgets.add(widget)

    def _scroll_to_current_match(self) -> None:
        """Request scroll to ensure the current match is visible."""
        if not self._matches:
            return

        widget, matches = self._matches[self._current_widget_index]
        start, _ = matches[self._current_match_index]

        # Trigger scrolling to this position
        self.handle_find_scroll(widget, start)

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
        if not self._matches:
            return 0, 0

        # Calculate total matches
        total_matches = sum(len(m[1]) for m in self._matches)

        # Calculate current match
        if self._current_widget_index == -1 or self._current_match_index == -1:
            return 0, total_matches

        current_global_match = sum(len(m[1]) for m in self._matches[:self._current_widget_index]) + self._current_match_index + 1

        return current_global_match, total_matches
