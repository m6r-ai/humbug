"""Conversation widget implementation."""

import asyncio
from dataclasses import dataclass
import logging
import time
from typing import Dict, List, Tuple, Any, Set, cast

from PySide6.QtWidgets import (
    QWidget, QVBoxLayout, QScrollArea, QSizePolicy, QMenu
)
from PySide6.QtCore import QTimer, QPoint, Qt, Signal, QEvent, QObject
from PySide6.QtGui import QCursor, QResizeEvent

from ai.ai_conversation import AIConversation, AIConversationEvent
from ai.ai_conversation_history import AIConversationHistory
from ai.ai_conversation_settings import AIConversationSettings, ReasoningCapability
from ai.ai_message import AIMessage
from ai.ai_message_source import AIMessageSource
from ai_tool.ai_tool_manager import AIToolCall

from humbug.color_role import ColorRole
from humbug.language.language_manager import LanguageManager
from humbug.message_box import MessageBox, MessageBoxType
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.style_manager import StyleManager
from humbug.tabs.conversation.conversation_input import ConversationInput
from humbug.tabs.conversation.conversation_message import ConversationMessage
from humbug.tabs.conversation.conversation_transcript_error import ConversationTranscriptError
from humbug.tabs.conversation.conversation_transcript_handler import ConversationTranscriptHandler


@dataclass
class BookmarkData:
    """Data associated with a bookmarked message."""
    widget: 'ConversationMessage'
    scroll_position: int

    def __init__(self, widget: 'ConversationMessage', scroll_position: int):
        """
        Initialize bookmark data.

        Args:
            widget: The bookmarked message widget
            scroll_position: Vertical scroll position when bookmarked
        """
        self.widget = widget
        self.scroll_position = scroll_position


class ConversationWidgetEventFilter(QObject):
    """Event filter to track activation events from child widgets."""

    widget_activated = Signal(object)
    widget_deactivated = Signal(object)

    def __init__(self, parent: QWidget | None = None) -> None:
        """Initialize the event filter."""
        super().__init__(parent)

    def eventFilter(self, watched: QObject, event: QEvent) -> bool:
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
            self.widget_activated.emit(watched)
            return False  # Don't consume the event

        if event.type() == QEvent.Type.FocusOut:
            # Emit a widget deactivated signal
            self.widget_deactivated.emit(watched)
            return False  # Don't consume the event

        return super().eventFilter(watched, event)


class ConversationWidget(QWidget):
    """Widget for displaying conversation with message history and input."""

    # Signal to notify tab of status changes
    status_updated = Signal()

    # Signals for tab to handle forking a conversation
    forkRequested = Signal()  # Signal to fork the conversation
    forkFromIndexRequested = Signal(int)  # Signal to fork from a specific message index

    # Signal for bookmark navigation
    bookmarkNavigationRequested = Signal(bool)  # True for next, False for previous

    # Signal to request scrolling to a specific widget and position
    scrollRequested = Signal(QWidget, int)  # Widget to scroll to, position within widget

    # Emits when parent should be activated by user interaction
    activated = Signal()

    # Emits when a submitted message has finished processing
    submit_finished = Signal()

    # Emits when the conversation is modified by the user
    conversation_modified = Signal()

    def __init__(
        self,
        path: str,
        parent: QWidget | None = None,
        use_existing_ai_conversation: bool = False
    ) -> None:
        """
        Initialize the conversation widget.

        Args:
            path: Full path to transcript file
            parent: Optional parent widget
            use_existing_ai_conversation: Will we use an existing AI conversation?
        """
        super().__init__(parent)
        self._logger = logging.getLogger("ConversationWidget")

        self._bookmarked_messages: Dict[ConversationMessage, BookmarkData] = {}
        self._current_bookmark_index: int | None = None

        # Create transcript handler with provided filename
        self._transcript_handler = ConversationTranscriptHandler(path)

        self._mindspace_manager = MindspaceManager()

        self._style_manager = StyleManager()

        self._ai_conversation = None
        if not use_existing_ai_conversation:
            self._ai_conversation = AIConversation()

            # Register callbacks for AIConversation events
            self._register_ai_conversation_callbacks()

        self._last_submitted_message: str = ""

        # We need to track any unfinished message because that won't appear in the transcript until
        # it completes.  If we move a conversation to a new tab, we need to ensure it doesn't get lost.
        self._current_unfinished_message: AIMessage | None = None

        self._last_update_time: float = 0  # Timestamp of last UI update
        self._update_timer = QTimer(self)  # Timer for throttled updates
        self._update_timer.setSingleShot(True)
        self._update_timer.timeout.connect(self._process_pending_update)
        self._pending_message = None  # Store the most recent pending message

            # Widget tracking
        self._messages: List[ConversationMessage] = []
        self._message_with_selection: ConversationMessage | None = None
        self._is_streaming = False

        # Initialize tracking variables
        self._auto_scroll = True
        self._last_scroll_maximum = 0

        # Create layout
        conversation_layout = QVBoxLayout(self)
        self.setLayout(conversation_layout)
        conversation_layout.setContentsMargins(0, 0, 0, 0)
        conversation_layout.setSpacing(0)

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
        self._input = ConversationInput(self._messages_container)
        self._input.cursorPositionChanged.connect(self._ensure_cursor_visible)
        self._input.selectionChanged.connect(
            lambda has_selection: self._handle_selection_changed(self._input, has_selection)
        )
        self._input.page_key_scroll_requested.connect(self._handle_edit_page_scroll)
        self._input.scrollRequested.connect(self._handle_selection_scroll)
        self._input.mouseReleased.connect(self._stop_scroll)
        self._input.forkRequested.connect(self._fork_from_message)
        self._input.submit_requested.connect(self.submit)
        self._input.stop_requested.connect(self._handle_stop_request)
        self._input.modified.connect(self.conversation_modified.emit)

        spacing = int(self._style_manager.message_bubble_spacing())
        self._messages_layout.setSpacing(spacing)
        self._messages_layout.setContentsMargins(spacing, spacing, spacing, spacing)
        self._messages_layout.addStretch()
        self._messages_layout.addWidget(self._input)

        self._messages_container.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)
        self._scroll_area.setWidget(self._messages_container)

        # Add the scroll area to the main layout
        conversation_layout.addWidget(self._scroll_area)

        # Setup signals for search highlights
        self._search_highlights: Dict[ConversationMessage, List[Tuple[int, int, int]]] = {}

        # Tracking for focused message
        self._focused_message_index = -1

        # Add bookmark status
        self._is_bookmarked = False

        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._handle_language_changed)

        # Create timer for smooth scrolling
        self._scroll_timer = QTimer(self)
        self._scroll_timer.setInterval(16)  # ~60fps
        self._scroll_timer.timeout.connect(self._update_scroll)
        self._last_mouse_pos: QPoint | None = None

        # Setup context menu
        self.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.customContextMenuRequested.connect(self._show_conversation_context_menu)

        # Connect to the vertical scrollbar's change signals
        self._scroll_area.verticalScrollBar().valueChanged.connect(self._on_scroll_value_changed)
        self._scroll_area.verticalScrollBar().rangeChanged.connect(self._on_scroll_range_changed)

        self._style_manager.style_changed.connect(self._handle_style_changed)
        self._handle_style_changed()

        # Find functionality
        self._matches: List[Tuple[ConversationMessage, List[Tuple[int, int, int]]]] = []
        self._current_widget_index = -1
        self._current_match_index = -1
        self._last_search = ""
        self._highlighted_widgets: Set[ConversationMessage] = set()

        # Set up activation tracking
        self._event_filter = ConversationWidgetEventFilter(self)
        self._event_filter.widget_activated.connect(self._handle_widget_activation)
        self._event_filter.widget_deactivated.connect(self._handle_widget_deactivation)
        self._install_activation_tracking(self._input)
        self._install_activation_tracking(self._messages_container)

    async def add_message(self, message: AIMessage) -> None:
        """
        Add a new message to the conversation view.

        Args:
            message: The message that was added
        """
        msg_widget = ConversationMessage(self)
        msg_widget.selectionChanged.connect(
            lambda has_selection: self._handle_selection_changed(msg_widget, has_selection)
        )
        # Add bookmark-specific signal
        msg_widget.scrollRequested.connect(self._handle_selection_scroll)
        msg_widget.mouseReleased.connect(self._stop_scroll)
        msg_widget.forkRequested.connect(self._fork_from_message)
        msg_widget.deleteRequested.connect(self._delete_from_message)
        msg_widget.expandRequested.connect(self._on_message_expanded)

        # Connect tool approval signals
        msg_widget.toolCallApproved.connect(self._handle_tool_call_approved)
        msg_widget.toolCallRejected.connect(self._handle_tool_call_rejected)

        msg_widget.set_content(message.content, message.source, message.timestamp, message.model or "", message.id)

        # Add widget before input and the stretch
        self._messages_layout.insertWidget(self._messages_layout.count() - 2, msg_widget)
        self._messages.append(msg_widget)

        self._install_activation_tracking(msg_widget)

    async def _change_message_expansion(self, message_index: int, expanded: bool) -> None:
        """
        Change the expansion state of a message.

        Args:
            message_index: Index of the message to change
            expanded: Whether the message should be expanded
        """
        if message_index >= len(self._messages):
            return

        self._messages[message_index].set_expanded(expanded)

    def _unregister_ai_conversation_callbacks(self) -> None:
        """Unregister all callbacks from the AIConversation object."""
        ai_conversation = cast(AIConversation, self._ai_conversation)
        ai_conversation.unregister_callback(
            AIConversationEvent.ERROR, self._on_request_error
        )
        ai_conversation.unregister_callback(
            AIConversationEvent.MESSAGE_ADDED, self._on_message_added
        )
        ai_conversation.unregister_callback(
            AIConversationEvent.MESSAGE_UPDATED, self._on_message_updated
        )
        ai_conversation.unregister_callback(
            AIConversationEvent.MESSAGE_COMPLETED, self._on_message_completed
        )
        ai_conversation.unregister_callback(
            AIConversationEvent.TOOL_USED, self._on_tool_used
        )
        ai_conversation.unregister_callback(
            AIConversationEvent.COMPLETED, self._on_request_completed
        )
        ai_conversation.unregister_callback(
            AIConversationEvent.TOOL_APPROVAL_REQUIRED, self._on_tool_approval_required
        )
        ai_conversation.unregister_callback(
            AIConversationEvent.STREAMING_UPDATE, self._on_streaming_update
        )

    def _register_ai_conversation_callbacks(self) -> None:
        """Register callbacks for AIConversation events."""
        ai_conversation = cast(AIConversation, self._ai_conversation)
        ai_conversation.register_callback(
            AIConversationEvent.ERROR, self._on_request_error
        )
        ai_conversation.register_callback(
            AIConversationEvent.MESSAGE_ADDED, self._on_message_added
        )
        ai_conversation.register_callback(
            AIConversationEvent.MESSAGE_UPDATED, self._on_message_updated
        )
        ai_conversation.register_callback(
            AIConversationEvent.MESSAGE_COMPLETED, self._on_message_completed
        )
        ai_conversation.register_callback(
            AIConversationEvent.TOOL_USED, self._on_tool_used
        )
        ai_conversation.register_callback(
            AIConversationEvent.COMPLETED, self._on_request_completed
        )
        ai_conversation.register_callback(
            AIConversationEvent.TOOL_APPROVAL_REQUIRED, self._on_tool_approval_required
        )
        ai_conversation.register_callback(
            AIConversationEvent.STREAMING_UPDATE, self._on_streaming_update
        )

    async def _on_streaming_update(self) -> None:
        """
        Handle streaming update events from AI conversation.

        This triggers the visual feedback animation in the input widget.
        """
        self._input.trigger_streaming_animation()

    async def _on_request_error(self, retries_exhausted: bool, message: AIMessage) -> None:
        """
        Handle errors in AI responses.

        Args:
            message: The error that occurred
        """
        await self.add_message(message)
        await self.write_transcript(message)

        if retries_exhausted:
            self._is_streaming = False
            self._input.set_streaming(False)
            self.submit_finished.emit()
            self.status_updated.emit()

            if self._last_submitted_message:
                self._input.set_plain_text(self._last_submitted_message)
                self._last_submitted_message = ""
                self._input.setFocus()

        # When we call this we should always scroll to the bottom and restore auto-scrolling
        self._auto_scroll = True
        self._scroll_to_bottom()

    async def _on_tool_used(self, message: AIMessage) -> None:
        """
        Handle a tool being used in the conversation.

        Args:
            message: The tool call message
        """
        await self.add_message(message)

        # Write the tool call to the transcript
        await self.write_transcript(message)

    async def _on_tool_approval_required(
        self,
        message: AIMessage,
        tool_call: AIToolCall,
        reason: str,
        destructive: bool
    ) -> None:
        """
        Handle tool approval requirement.

        Args:
            message: The tool call message
            tool_call: Tool call requiring approval
            reason: Reason for the tool call
            destructive: Whether the tool calls are considered destructive
        """
        # Find the message widget that corresponds to this tool call message
        for msg_widget in self._messages:
            if msg_widget.message_id() == message.id:
                # Add approval UI to this message
                msg_widget.show_tool_approval_ui(tool_call, reason, destructive)
                break

    def _handle_tool_call_approved(self, _tool_call: AIToolCall) -> None:
        """Handle user approval of tool calls."""
        ai_conversation = cast(AIConversation, self._ai_conversation)
        loop = asyncio.get_event_loop()
        loop.create_task(ai_conversation.approve_pending_tool_calls())

    def _handle_tool_call_rejected(self, reason: str) -> None:
        """Handle user rejection of tool calls."""
        ai_conversation = cast(AIConversation, self._ai_conversation)
        loop = asyncio.get_event_loop()
        loop.create_task(ai_conversation.reject_pending_tool_calls(reason))

    def _on_message_expanded(self, expanded: bool) -> None:
        """
        Handle the change in scroll behaviour when a message is expanded or collapsed.
        Args:
            expanded: Whether the message is expanded or not
        """
        if expanded:
            self._auto_scroll = False

    async def _on_message_added(self, message: AIMessage) -> None:
        """
        Handle a new message being added to the conversation.

        Args:
            message: The message that was added
        """
        self._current_unfinished_message = message
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
                widget.set_content(message.content, message.source, message.timestamp, message.model or "", message.id)
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
        self._current_unfinished_message = None

        # Cancel any pending update
        if self._update_timer.isActive():
            self._update_timer.stop()

        # Clear the pending message state
        self._pending_message = None

        # Update with the completed message immediately
        await self._update_message(message)
        await self.write_transcript(message)
        self.status_updated.emit()

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

        self.submit_finished.emit()
        self.status_updated.emit()

    def _handle_language_changed(self) -> None:
        """Update language-specific elements when language changes."""
        # Update input widget streaming state text
        self._input.set_streaming(self._is_streaming)

        # Emit signal for status update
        self.status_updated.emit()

    async def write_transcript(self, message: AIMessage) -> None:
        """
        Write messages to transcript file.

        Args:
            message: AIMessage to write to transcript
        """
        try:
            await self._transcript_handler.write([message.to_transcript_dict()])

        except ConversationTranscriptError:
            self._logger.exception("Failed to write to transcript")

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

    def update_conversation_settings(self, new_settings: AIConversationSettings) -> None:
        """Update conversation settings and associated backend."""
        ai_conversation = cast(AIConversation, self._ai_conversation)
        ai_conversation.update_conversation_settings(new_settings)
        self.status_updated.emit()
        self._input.set_model(new_settings.model)

    def conversation_settings(self) -> AIConversationSettings:
        """
        Get current conversation settings.

        Returns:
            Current conversation settings
        """
        ai_conversation = cast(AIConversation, self._ai_conversation)
        return ai_conversation.conversation_settings()

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
        # If we have a focus message then focus it
        if self._focused_message_index != -1:
            self._messages[self._focused_message_index].set_focused(True)
            return

        self._input.set_focused(True)

    def _install_activation_tracking(self, widget: QWidget) -> None:
        """
        Install event filter on widget and all its children recursively.

        Call this for any new widgets added to the conversation widget.

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
         # Emit activated signal to let the tab know this conversation was clicked
        self.activated.emit()

        # If we are clicking the messages container, focus the last focused message or input
        if widget == self._messages_container:
            self.activate()
            return

        # Find the ConversationMessage that contains this widget
        message_widget = self._find_conversation_message(widget)
        if message_widget is None:
            return

        if message_widget.is_focused():
            return

        # Set focus on the new message
        if message_widget in self._messages:
            self._focused_message_index = self._messages.index(message_widget)
            message_widget.set_focused(True)
            return

        self._focused_message_index = -1
        self._input.set_focused(True)

    def _handle_widget_deactivation(self, widget: QWidget) -> None:
        """
        Handle deactivation of a widget, checking if focus is leaving the associated message.

        Args:
            widget: The widget that lost focus
        """
        # Find the ConversationMessage that contains this widget
        message_widget = self._find_conversation_message(widget)
        if message_widget is None:
            return

        # Remove focus from the currently focused message
        if self._focused_message_index != -1:
            self._messages[self._focused_message_index].set_focused(False)

        else:
            self._input.set_focused(False)

    def _find_conversation_message(self, widget: QWidget) -> ConversationMessage | None:
        """
        Find the ConversationMessage that contains the given widget.

        Args:
            widget: The widget to find the containing ConversationMessage for

        Returns:
            The ConversationMessage containing the widget, or None if not found
        """
        current: QObject = widget
        while current:
            if isinstance(current, ConversationMessage):
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

    def _scroll_to_message(self, message: ConversationMessage) -> None:
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

    def _toggle_message_bookmark(self, message_widget: ConversationMessage) -> None:
        """Toggle bookmark status for a message."""
        if message_widget in self._bookmarked_messages:
            # Remove bookmark
            del self._bookmarked_messages[message_widget]
            message_widget.set_bookmarked(False)

        else:
            # Add bookmark with current scroll position
            scroll_position = self._scroll_area.verticalScrollBar().value()
            self._bookmarked_messages[message_widget] = BookmarkData(
                widget=message_widget,
                scroll_position=scroll_position
            )
            message_widget.set_bookmarked(True)

        # Reset bookmark index when bookmarks change
        self._current_bookmark_index = None

    def navigate_bookmarks(self, forward: bool = True) -> None:
        """Navigate between bookmarked messages."""
        if not self._bookmarked_messages:
            return

        # Convert to list for ordered access while maintaining BookmarkData
        bookmarked_items = list(self._bookmarked_messages.items())

        # Initialize bookmark index if not set
        if self._current_bookmark_index is None:
            # Start from the beginning or end based on navigation direction
            self._current_bookmark_index = -1 if forward else len(bookmarked_items)

        # Calculate next bookmark index
        if forward:
            self._current_bookmark_index = (self._current_bookmark_index + 1) % len(bookmarked_items)

        else:
            self._current_bookmark_index = (self._current_bookmark_index - 1) % len(bookmarked_items)

        # Get the bookmarked message and its data
        _message_widget, bookmark_data = bookmarked_items[self._current_bookmark_index]

        # Restore the scroll position
        self._scroll_area.verticalScrollBar().setValue(bookmark_data.scroll_position)

    def _handle_selection_changed(self, message_widget: ConversationMessage, has_selection: bool) -> None:
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

    def set_path(self, new_path: str) -> None:
        """
        Set the conversation file path.

        Args:
            new_path: New path for the conversation file
        """
        self._transcript_handler.set_path(new_path)

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

    def set_input_text(self, text: str) -> None:
        """Set the input text."""
        self._input.set_plain_text(text)
        self._input.setFocus()

    def load_message_history(self, messages: List[AIMessage], reuse_ai_conversation: bool) -> None:
        """
        Load existing message history from transcript.

        Args:
            messages: List of AIMessage objects to load
            reuse_ai_conversation: True if we are reusing an existing AI conversation
        """
        # Establish a baseline for conversation settings
        if not reuse_ai_conversation:
            settings = self._mindspace_manager.settings()
            if settings is None:
                self._logger.error("Failed to load conversation settings.")
                return

            default_settings = AIConversationSettings(
                model=settings.model,
                temperature=settings.temperature,
                reasoning=settings.reasoning
            )

            ai_conversation = cast(AIConversation, self._ai_conversation)
            ai_conversation.update_conversation_settings(default_settings)
            ai_conversation.load_message_history(messages)
            conversation_settings = ai_conversation.conversation_settings()
            self._input.set_model(conversation_settings.model)

        # Add messages to this widget.
        loop = asyncio.get_event_loop()
        for message in messages:
            loop.create_task(self.add_message(message))

        # Ensure we're scrolled to the end
        self._auto_scroll = True
        self._scroll_to_bottom()

        self.status_updated.emit()

    def resizeEvent(self, event: QResizeEvent) -> None:
        """Handle resize events."""
        super().resizeEvent(event)

        if self._auto_scroll:
            self._scroll_to_bottom()

    def cancel_current_tasks(self) -> None:
        """Cancel any ongoing AI response tasks."""
        ai_conversation = cast(AIConversation, self._ai_conversation)
        ai_conversation.cancel_current_tasks()

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

    def _show_conversation_context_menu(self, pos: QPoint) -> None:
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

        fork_action = menu.addAction(strings.fork_conversation)
        fork_action.triggered.connect(self.forkRequested)
        menu.addSeparator()

        toggle_bookmark_action = menu.addAction(strings.bookmark_section)
        toggle_bookmark_action.setEnabled(self.can_toggle_bookmark())
        toggle_bookmark_action.setCheckable(True)
        toggle_bookmark_action.setChecked(self.is_checked_bookmark())
        toggle_bookmark_action.triggered.connect(self.toggle_bookmark)

        next_bookmark_action = menu.addAction(strings.next_bookmark)
        next_bookmark_action.setEnabled(self.can_navigate_next_bookmark())
        next_bookmark_action.triggered.connect(self.navigate_next_bookmark)

        prev_bookmark_action = menu.addAction(strings.previous_bookmark)
        prev_bookmark_action.setEnabled(self.can_navigate_previous_bookmark())
        prev_bookmark_action.triggered.connect(self.navigate_previous_bookmark)

        # Show menu at click position
        menu.exec_(self.mapToGlobal(pos))

    def _find_fork_end_index(self, start_index: int) -> int:
        """
        Find the last message index to include when forking from start_index.

        This scans forward from start_index to include any hidden messages
        that follow, stopping at the next visible message.

        Args:
            start_index: Index of the message to fork from

        Returns:
            Index of the last message to include in the fork
        """
        if start_index < 0 or start_index >= len(self._messages):
            return start_index

        # Start from the message after the fork point
        current_index = start_index + 1

        # Scan forward while we have hidden messages
        while current_index < len(self._messages):
            message_widget = self._messages[current_index]

            # If we hit a visible message, stop (don't include it)
            if message_widget.isVisible():
                break

            # This is a hidden message, include it and continue
            current_index += 1

        # Return the index of the last message to include
        # (current_index - 1 because we stopped at the first visible message)
        return current_index - 1

    def _fork_from_message(self) -> None:
        """
        Fork the conversation from the specified message.

        This will include the specified message and any hidden messages
        that immediately follow it until the next visible message.
        """
        # Find the index of the message in our list
        message = self.sender()
        if not isinstance(message, ConversationMessage):
            return

        if message not in self._messages:
            # For the input widget, fork at current position
            self.forkRequested.emit()
            return

        message_index = self._messages.index(message)

        # Find the actual end index including hidden messages
        fork_end_index = self._find_fork_end_index(message_index)

        # Emit signal with the end index (inclusive)
        self.forkFromIndexRequested.emit(fork_end_index)

    def _delete_from_message(self) -> None:
        """Handle request to delete conversation from a message onwards."""
        # Identify which message widget triggered the request
        sender = self.sender()
        if not isinstance(sender, ConversationMessage):
            return

        # Find the index of the message in our list
        if sender in self._messages:
            index = self._messages.index(sender)
            loop = asyncio.get_event_loop()
            if loop.is_running():
                loop.create_task(self.delete_messages_from_index(index))

    async def delete_messages_from_index(self, index: int) -> None:
        """
        Delete the message at the specified index and all subsequent messages.

        Args:
            index: Index of the first message to delete
        """
        if index < 0 or index >= len(self._messages):
            return

        # Store all messages up to but not including the specified index
        preserved_messages = self._messages[:index]

        # Remove message widgets from the layout and delete them
        for i in range(len(self._messages) - 1, index - 1, -1):
            message_widget = self._messages[i]
            self._messages_layout.removeWidget(message_widget)
            message_widget.deleteLater()

        # Update the _messages list to only include preserved messages
        self._messages = preserved_messages

        # Update the underlying AI conversation history
        ai_conversation = cast(AIConversation, self._ai_conversation)
        history = ai_conversation.get_conversation_history()

        # Get all messages from history
        all_messages = history.get_messages()

        # Keep only the messages up to the specified index
        preserved_history_messages = all_messages[:index]

        # Update the AI conversation history
        ai_conversation.load_message_history(preserved_history_messages)

        # Work out what the conversation settings now are - they may have changed
        conversation_settings = ai_conversation.conversation_settings()
        self._input.set_model(conversation_settings.model)

        # Update the transcript file by rewriting it with only the preserved messages
        transcript_messages = [msg.to_transcript_dict() for msg in preserved_history_messages]

        try:
            await self._transcript_handler.replace_messages(transcript_messages)

            # Reset bookmarks and selection state
            self._bookmarked_messages = {}
            self._current_bookmark_index = None
            self._message_with_selection = None

            # Emit status update
            self.status_updated.emit()

            # Scroll to bottom
            self._auto_scroll = True
            self._scroll_to_bottom()

        except Exception as e:
            self._logger.error("Failed to update transcript after deletion: %s", str(e))
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                self._language_manager.strings().error_title_rename,
                f"Failed to update transcript after deletion: {str(e)}"
            )

    def can_toggle_bookmark(self) -> bool:
        """Can we toggle bookmarks?"""
        focus_widget: QWidget | None = self.focusWidget()
        if not focus_widget:
            return False

        while focus_widget and not isinstance(focus_widget, ConversationMessage):
            focus_widget = focus_widget.parentWidget()
            if isinstance(focus_widget, ConversationWidget):
                return False

        if isinstance(focus_widget, ConversationInput):
            return False

        return True

    def is_checked_bookmark(self) -> bool:
        """Is the current bookmark set (checked)?"""
        focus_widget: QWidget | None = self.focusWidget()
        if not focus_widget:
            return False

        while focus_widget and not isinstance(focus_widget, ConversationMessage):
            focus_widget = focus_widget.parentWidget()
            if isinstance(focus_widget, ConversationWidget):
                return False

        if isinstance(focus_widget, ConversationInput):
            return False

        return cast(ConversationMessage, focus_widget).is_bookmarked()

    def toggle_bookmark(self) -> None:
        """Toggle a bookmark at the current message."""
        focus_widget: QWidget | None = self.focusWidget()
        if not focus_widget:
            return

        while focus_widget and not isinstance(focus_widget, ConversationMessage):
            focus_widget = focus_widget.parentWidget()
            if isinstance(focus_widget, ConversationWidget):
                return

        if isinstance(focus_widget, ConversationInput):
            return

        self._toggle_message_bookmark(cast(ConversationMessage, focus_widget))

    def can_navigate_next_bookmark(self) -> bool:
        """Can we go to a next bookmark?"""
        return bool(self._bookmarked_messages)

    def navigate_next_bookmark(self) -> None:
        """Move to the next bookmark."""
        self.navigate_bookmarks(forward=True)

    def can_navigate_previous_bookmark(self) -> bool:
        """Can we go to a previous bookmark?"""
        return bool(self._bookmarked_messages)

    def navigate_previous_bookmark(self) -> None:
        """Move to the previous bookmark."""
        self.navigate_bookmarks(forward=False)

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
        return has_text and not self._is_streaming

    def _sanitize_input(self, text: str) -> str:
        """
        Strip control characters from input text, preserving newlines and tabs.

        Args:
            text: Text to sanitize

        Returns:
            Sanitized text
        """
        return ''.join(char for char in text if char == '\n' or char == '\t' or (ord(char) >= 32 and ord(char) != 127))

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

        sanitized_content = self._sanitize_input(content)
        message = AIMessage.create(AIMessageSource.USER, sanitized_content)

        self._last_submitted_message = content

        # Submit the message to the AIConversation instance
        loop = asyncio.get_event_loop()
        if not loop.is_running():
            return

        loop.create_task(self.add_message(message))
        ai_conversation = cast(AIConversation, self._ai_conversation)
        loop.create_task(ai_conversation.submit_message(message))
        loop.create_task(self.write_transcript(message))

        # When we call this we should always scroll to the bottom and restore auto-scrolling
        self._auto_scroll = True
        self._scroll_to_bottom()

    def _handle_stop_request(self) -> None:
        """Handle stop request from input widget."""
        self.cancel_current_tasks()

    def get_conversation_history(self) -> AIConversationHistory:
        """Get the conversation history object."""
        ai_conversation = cast(AIConversation, self._ai_conversation)
        return ai_conversation.get_conversation_history()

    def create_state_metadata(self, temp_state: bool) -> Dict[str, Any]:
        """
        Create metadata dictionary capturing current widget state.

        Returns:
            Dictionary containing conversation state metadata
        """
        metadata: Dict[str, Any] = {}

        # Store current input content
        metadata["content"] = self._input.to_plain_text()
        metadata['cursor'] = self._get_cursor_position()

        # Store message expansion states
        expansion_states = []
        for message_widget in self._messages:
            expansion_states.append(message_widget.is_expanded())

        metadata["message_expansion"] = expansion_states

        # Store bookmarks
        bookmark_data = []
        for message_widget, data in self._bookmarked_messages.items():
            if message_widget in self._messages:
                bookmark_data.append({
                    'index': self._messages.index(message_widget),
                    'scroll_position': data.scroll_position
                })

        metadata['bookmarks'] = bookmark_data

        # Store current settings
        ai_conversation = cast(AIConversation, self._ai_conversation)
        settings = ai_conversation.conversation_settings()
        metadata["settings"] = {
            "model": settings.model,
            "temperature": settings.temperature,
            "reasoning": settings.reasoning.value
        }

        # If we've been asked for temporary state it means we're going to move this
        # widget so prep for moving our conversation state directly.
        if temp_state:
            # Unregister callbacks from the current widget
            self._unregister_ai_conversation_callbacks()

            # Store AIConversation reference in metadata
            metadata["ai_conversation_ref"] = self._ai_conversation
            metadata["is_streaming"] = self._is_streaming
            metadata["current_unfinished_message"] = self._current_unfinished_message

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

        # Restore message expansion states if specified
        if "message_expansion" in metadata:
            expansion_states = metadata["message_expansion"]

            # Our messages list has already been queued to be added but they're not yet there.
            # Add our expansion states updates to the same event loop so they get processed
            # after the messages are added.
            loop = asyncio.get_event_loop()
            for i, is_expanded in enumerate(expansion_states):
                loop.create_task(self._change_message_expansion(i, is_expanded))

        # Restore bookmarks if specified
        if 'bookmarks' in metadata:
            bookmark_data = metadata['bookmarks']
            for data in bookmark_data:
                index = data['index']
                scroll_position = data['scroll_position']
                if 0 <= index < len(self._messages):
                    msg_widget = self._messages[index]

                    # Add bookmark with stored scroll position
                    self._bookmarked_messages[msg_widget] = BookmarkData(
                        widget=msg_widget,
                        scroll_position=scroll_position
                    )
                    msg_widget.set_bookmarked(True)

        # If we have a conversation reference then we're going to take that over!
        if "ai_conversation_ref" in metadata:
            ai_conversation: AIConversation = metadata["ai_conversation_ref"]
            self._ai_conversation = ai_conversation
            self._register_ai_conversation_callbacks()

            # Update streaming state if the AI conversation is already streaming
            self._is_streaming = metadata["is_streaming"]
            self._input.set_streaming(self._is_streaming)
            conversation_settings = ai_conversation.conversation_settings()
            self._input.set_model(conversation_settings.model)

            current_unfinished_message = metadata.get("current_unfinished_message")
            if current_unfinished_message:
                loop = asyncio.get_event_loop()
                loop.create_task(self.add_message(current_unfinished_message))
                self._current_unfinished_message = current_unfinished_message

        else:
            # Restore settings
            if "settings" in metadata:
                settings = AIConversationSettings(
                    model=metadata["settings"].get("model"),
                    temperature=metadata["settings"].get("temperature"),
                    reasoning=ReasoningCapability(metadata["settings"].get("reasoning", ReasoningCapability.NO_REASONING.value))
                )
                self.update_conversation_settings(settings)

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
        if self._ai_conversation is None:
            return None

        return self._ai_conversation.get_token_counts()

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

    def _handle_find_scroll(self, widget: ConversationMessage, section_num: int, position: int) -> None:
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

    def _scroll_to_current_match(self) -> None:
        """Request scroll to ensure the current match is visible."""
        widget, matches = self._matches[self._current_widget_index]
        section_num, start, _ = matches[self._current_match_index]

        # Trigger scrolling to this position
        self._handle_find_scroll(widget, section_num, start)

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
