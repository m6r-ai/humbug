"""Conversation widget implementation."""

import asyncio
import logging
import os
import time
from typing import Dict, List, Tuple, Any, Set, cast

from PySide6.QtWidgets import (
    QWidget, QVBoxLayout, QScrollArea, QSizePolicy, QMenu
)
from PySide6.QtCore import QTimer, QPoint, Qt, Signal, QObject, QRect
from PySide6.QtGui import QCursor, QResizeEvent, QShowEvent

from ai import (
    AIConversation, AIConversationEvent, AIConversationHistory,
    AIConversationSettings, AIReasoningCapability, AIMessage, AIMessageSource
)
from ai_conversation_transcript import AIConversationTranscriptError, AIConversationTranscriptHandler
from ai_tool import AIToolCall

from humbug.color_role import ColorRole
from humbug.language.language_manager import LanguageManager
from humbug.message_box import MessageBox, MessageBoxType, MessageBoxButton
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.style_manager import StyleManager
from humbug.tabs.conversation.conversation_error import ConversationError
from humbug.tabs.conversation.conversation_input import ConversationInput
from humbug.tabs.conversation.conversation_message import ConversationMessage
from humbug.tabs.conversation.conversation_widget_event_filter import ConversationWidgetEventFilter


class ConversationWidget(QWidget):
    """Widget for displaying conversation with message history and input."""

    # Signal to notify tab of status changes
    status_updated = Signal()

    # Signals for tab to handle forking a conversation
    fork_requested = Signal()  # Signal to fork the conversation
    fork_from_index_requested = Signal(int)  # Signal to fork from a specific message index

    # Emits when parent should be activated by user interaction
    activated = Signal()

    # Emits when the conversation label should be updated
    update_label = Signal()

    # Emits when a submitted message has finished processing
    submit_finished = Signal(dict)

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

        self.setObjectName("ConversationWidget")

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

        # Message border animation state (moved from ConversationInput)
        self._animated_message: ConversationMessage | None = None
        self._animation_frame = 0
        self._fade_direction = 1
        self._is_animating = False

        # Animation parameters for smooth fade
        self._animation_steps = 64

        # Timer intervals
        self._slow_interval_ms = int(3000 / self._animation_steps)
        self._debounce_interval_ms = int(750 / self._animation_steps)

        # Slow timer - always running during animation to provide regular updates
        self._slow_timer = QTimer()
        self._slow_timer.setInterval(self._slow_interval_ms)
        self._slow_timer.timeout.connect(self._on_slow_timer)

        # Pending message flag and counter for smooth transition
        self._pending_animation_message = False
        self._no_message_counter = 0
        self._max_no_message_cycles = 16  # Number of cycles before disabling debounce timer

        # Debounce timer for message notifications
        self._debounce_timer = QTimer()
        self._debounce_timer.setSingleShot(True)
        self._debounce_timer.timeout.connect(self._on_debounce_timeout)
        self._debounce_timer.setInterval(self._debounce_interval_ms)

        # Initialize tracking variables
        self._auto_scroll = True
        self._last_scroll_maximum = 0

        # Layout stabilization tracking for lazy syntax highlighting.  We don't have a signal for when
        # the layout has finalized so we watch resize events and wait for them to stop firing.
        self._initial_layout_complete = False
        self._initial_scroll_position: int | None = None
        self._initial_auto_scroll = True
        self._layout_stabilization_timer = QTimer(self)
        self._layout_stabilization_timer.setSingleShot(True)
        self._layout_stabilization_timer.timeout.connect(self._on_initial_layout_stabilized)

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
        self._input = ConversationInput(AIMessageSource.USER, self._messages_container)
        self._input.cursor_position_changed.connect(self._ensure_cursor_visible)
        self._input.selection_changed.connect(
            lambda has_selection: self._on_selection_changed(self._input, has_selection)
        )
        self._input.page_key_scroll_requested.connect(self._on_page_key_scroll_requested)
        self._input.scroll_requested.connect(self._on_scroll_requested)
        self._input.mouse_released.connect(self._stop_scroll)
        self._input.fork_requested.connect(self._on_message_fork_requested)
        self._input.submit_requested.connect(self.submit)
        self._input.stop_requested.connect(self._on_stop_requested)
        self._input.modified.connect(self.conversation_modified)

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

        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._on_language_changed)

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

        self._style_manager.style_changed.connect(self._on_style_changed)
        self._on_style_changed()

        # Find functionality
        self._matches: List[Tuple[ConversationMessage, List[Tuple[int, int, int]]]] = []
        self._current_widget_index = -1
        self._current_match_index = -1
        self._last_search = ""
        self._highlighted_widgets: Set[ConversationMessage] = set()

        # Set up activation tracking
        self._event_filter = ConversationWidgetEventFilter(self)
        self._event_filter.widget_activated.connect(self._on_widget_activated)
        self._event_filter.widget_deactivated.connect(self._on_widget_deactivated)
        self._install_activation_tracking(self._input)
        self._install_activation_tracking(self._messages_container)

        # Create transcript handler with provided filename, then load the transcript data
        self._transcript_handler = AIConversationTranscriptHandler(path)
        conversation_history = self._transcript_handler.read()
        self._load_message_history(conversation_history.get_messages(), use_existing_ai_conversation)
        self._set_delegated_conversation_mode(os.path.basename(path).startswith("dAI-"))

        # Any active tool approval
        self._pending_tool_call_approval: ConversationMessage | None = None

    def _set_delegated_conversation_mode(self, enabled: bool) -> None:
        """
        Enable or disable delegated conversation mode.

        In delegated conversation mode, the user input is hidden to prevent
        manual message submission.

        Args:
            enabled: True to enable delegated conversation mode, False to disable
        """
        self._is_delegated_conversation = enabled
        self._input.setVisible(not enabled)

    def _create_completion_result(self) -> Dict[str, Any]:
        """
        Create completion result for delegated conversation.

        Returns:
            Dictionary containing completion result
        """
        ai_conversation = cast(AIConversation, self._ai_conversation)
        messages = ai_conversation.get_conversation_history().get_messages()

        if not messages:
            return {"success": False, "error": "No messages in conversation"}

        last_message = messages[-1]

        if not last_message.completed:
            return {"success": False, "error": "AI response was terminated early"}

        if last_message.source == AIMessageSource.AI:
            result = {
                "success": True,
                "content": last_message.content,
                "model": last_message.model,
            }
            if last_message.usage:
                result["usage"] = last_message.usage.to_dict()

            return result

        if last_message.source == AIMessageSource.SYSTEM:
            return {
                "success": False,
                "error": last_message.content,
                "details": last_message.error
            }

        return {"success": False, "error": "Conversation ended unexpectedly"}

    def _add_message(self, message: AIMessage) -> None:
        """
        Add a new message to the conversation view.

        Args:
            message: The message that was added
        """
        self._hide_last_ai_connected_message()

        msg_widget = ConversationMessage(
            message.source, message.timestamp, message.model or "", message.id, message.user_name, message.content, self
        )
        msg_widget.selection_changed.connect(
            lambda has_selection: self._on_selection_changed(msg_widget, has_selection)
        )
        msg_widget.scroll_requested.connect(self._on_scroll_requested)
        msg_widget.mouse_released.connect(self._stop_scroll)
        msg_widget.fork_requested.connect(self._on_message_fork_requested)
        msg_widget.delete_requested.connect(self._on_message_delete_requested)
        msg_widget.expand_requested.connect(self._on_message_expand_requested)
        msg_widget.tool_call_approved.connect(self._on_tool_call_approved)
        msg_widget.tool_call_rejected.connect(self._on_tool_call_rejected)

        # Add widget before input and the stretch
        self._messages_layout.insertWidget(self._messages_layout.count() - 2, msg_widget)
        self._messages.append(msg_widget)

        self._install_activation_tracking(msg_widget)

        # If we're currently animating, transfer animation to this new message
        # but only if the new message is not hidden
        if self._is_animating and msg_widget.is_rendered():
            self._transfer_animation_to_message(msg_widget)

        # If we're animating but current animated message is no longer visible,
        # find a new visible message to animate
        elif self._is_animating and self._animated_message and not self._animated_message.is_rendered():
            self._update_animated_message()

    def _hide_last_ai_connected_message(self) -> None:
        """
        Hide the last AI_CONNECTED message from the UI if it is present.

        This keeps the message in the transcript for analytics but removes it from
        the visual conversation flow.
        """
        if not self._messages:
            return

        last_message_widget = self._messages[-1]
        if last_message_widget.message_source() != AIMessageSource.AI_CONNECTED:
            return

        # Remove from UI
        last_message_widget.set_rendered(False)

        if self._message_with_selection == last_message_widget:
            self._message_with_selection = None

        # If this was the animated message, stop animation (new message will start it)
        if self._animated_message == last_message_widget:
            self._animated_message = None

    def _change_message_expansion(self, message_index: int, expanded: bool) -> None:
        """
        Change the expansion state of a message.

        Args:
            message_index: Index of the message to change
            expanded: Whether the message should be expanded
        """
        if message_index >= len(self._messages):
            return

        message_widget = self._messages[message_index]
        message_widget.set_expanded(expanded)

        # If we're animating and the animated message visibility changed, update animation
        if self._is_animating:
            self._update_animated_message()

    def _lazy_update_visible_sections(self) -> None:
        """Ensure all visible code sections have highlighters created."""
        viewport = self._scroll_area.viewport()
        viewport_rect = viewport.rect()
        scroll_offset = self._scroll_area.verticalScrollBar().value()

        # Create viewport rect in scroll area content coordinates
        visible_rect = QRect(0, scroll_offset, viewport_rect.width(), viewport_rect.height())

        for i, message in enumerate(self._messages):
            if not message.is_rendered():
                continue

            # Get message position relative to scroll area content
            message_pos = message.mapTo(self._messages_container, QPoint(0, 0))
            message_rect = QRect(message_pos, message.size())

            # Only check sections if message intersects with viewport
            if message_rect.intersects(visible_rect):
                is_streaming = self._is_streaming and i == len(self._messages) - 1
                message.lazy_update(visible_rect, self._messages_container, self._event_filter, is_streaming)

    def _on_initial_layout_stabilized(self) -> None:
        """Handle the initial layout stabilization - do the first visibility check."""
        self._initial_layout_complete = True
        self._lazy_update_visible_sections()

        # If we have an initial scroll position, set it now
        if self._initial_scroll_position is not None:
            self._auto_scroll = self._initial_auto_scroll
            self._scroll_area.verticalScrollBar().setValue(self._initial_scroll_position)
            self._initial_scroll_position = None

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
        ai_conversation.unregister_callback(
            AIConversationEvent.AI_CONNECTED, self._on_ai_connected
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
        ai_conversation.register_callback(
            AIConversationEvent.AI_CONNECTED, self._on_ai_connected
        )

    async def _on_ai_connected(self, message: AIMessage) -> None:
        """
        Handle AI connected event.

        Args:
            message: The AI_CONNECTED message
        """
        # Our "thinking" message is empty when we receive it.  We populate it here.
        strings = self._language_manager.strings()
        message.content = strings.ai_thinking

        self._add_message(message)
        self._append_message_to_transcript(message)

        # Start animation if not already animating
        if not self._is_animating:
            self._start_message_border_animation()

        # When we call this we should always scroll to the bottom and restore auto-scrolling
        self._auto_scroll = True
        self._scroll_to_bottom()

    async def _on_streaming_update(self) -> None:
        """
        Handle streaming update events from AI conversation.

        This triggers the visual feedback animation for the last visible message.
        """
        self.trigger_message_animation()

    def trigger_message_animation(self) -> None:
        """
        Trigger animation update due to network message received.

        This method implements debouncing - if the debounce timer is not active,
        it triggers an immediate animation update and starts the debounce timer.
        If the debounce timer is already active, it sets a pending flag to
        indicate another message was received during the debounce period.

        When a new message is received, the no-message counter is reset to zero.
        """
        if not self._is_animating:
            return

        # Reset the no-message counter since we received a message
        self._no_message_counter = 0

        if self._debounce_timer.isActive():
            self._pending_animation_message = True
            return

        # No debounce timer running - trigger immediate update
        self._update_border_animation()
        self._debounce_timer.start()
        self._pending_animation_message = False

    def _on_slow_timer(self) -> None:
        """Handle slow timer timeout - provides regular animation updates."""
        if not self._is_animating:
            return

        if self._debounce_timer.isActive():
            return

        self._update_border_animation()

    def _on_debounce_timeout(self) -> None:
        """
        Handle debounce timer timeout.

        If there was a pending message during the debounce period, immediately
        trigger another animation update and restart the debounce timer.

        If there was no pending message, increment the no-message counter.
        If the counter reaches the maximum, disable the debounce timer.
        Otherwise, treat it as if we saw a message (for smooth transition).
        """
        if self._pending_animation_message:
            # There was a message during debounce - trigger update and restart
            self._update_border_animation()
            self._debounce_timer.start()
            self._pending_animation_message = False
            return

        # No message during debounce period
        self._no_message_counter += 1

        if self._no_message_counter >= self._max_no_message_cycles:
            # Reached maximum cycles - stop debounce timer
            # Animation will continue with slow timer only
            self._no_message_counter = 0
            return

        # Continue fast animation for smooth transition
        self._update_border_animation()
        self._debounce_timer.start()

    def _start_message_border_animation(self) -> None:
        """Start animating the last visible message."""
        last_message = self._find_last_visible_message_widget()
        if not last_message:
            return

        self._animated_message = last_message
        self._animation_frame = 0
        self._fade_direction = 1
        self._is_animating = True
        self._pending_animation_message = False
        self._no_message_counter = 0

        # Start animation on the message
        last_message.set_border_animation(True, self._animation_frame, self._animation_steps)

        # Start the slow timer - this runs continuously
        self._slow_timer.start()

    def _transfer_animation_to_message(self, new_message: ConversationMessage) -> None:
        """Transfer animation to a new message."""
        # Stop current animation
        if self._animated_message:
            self._animated_message.set_border_animation(False)

        # Start new animation (reset frame)
        self._animated_message = new_message
        self._animation_frame = 0
        self._fade_direction = 1
        new_message.set_border_animation(True, self._animation_frame, self._animation_steps)

    def _update_animated_message(self) -> None:
        """Update which message is being animated based on visibility."""
        if not self._is_animating:
            return

        # Find the last visible message
        last_visible = self._find_last_visible_message_widget()

        # If no visible messages, stop animation
        if not last_visible:
            self._stop_message_border_animation()
            return

        # If the currently animated message is different from last visible, transfer
        if self._animated_message != last_visible:
            self._transfer_animation_to_message(last_visible)

    def _stop_message_border_animation(self) -> None:
        """Stop all message border animation."""
        if self._animated_message:
            self._animated_message.set_border_animation(False)
            self._animated_message = None

        self._is_animating = False
        self._slow_timer.stop()
        self._debounce_timer.stop()
        self._animation_frame = 0
        self._pending_animation_message = False
        self._no_message_counter = 0

    def _find_last_visible_message_widget(self) -> ConversationMessage | None:
        """Find the last visible message widget."""
        for message in reversed(self._messages):
            if message.is_rendered():
                return message
        return None

    def _update_border_animation(self) -> None:
        """Update the border animation frame."""
        if not self._is_animating or not self._animated_message:
            return

        # Check if animated message is still visible
        if not self._animated_message.is_rendered():
            self._update_animated_message()
            return

        # Update animation frame with direction
        self._animation_frame += self._fade_direction

        # Reverse direction at the extremes for full cycle (start→mid→start)
        if self._animation_frame >= self._animation_steps:
            self._animation_frame = self._animation_steps - 2
            self._fade_direction = -1

        elif self._animation_frame < 0:
            self._animation_frame = 1
            self._fade_direction = 1

        # Update the animated message
        self._animated_message.set_border_animation(True, self._animation_frame, self._animation_steps)

    async def _on_request_error(self, retries_exhausted: bool, message: AIMessage) -> None:
        """
        Handle errors in AI responses.

        Args:
            message: The error that occurred
        """
        self._add_message(message)
        self._append_message_to_transcript(message)

        if retries_exhausted:
            self._is_streaming = False
            self._input.set_streaming(False)
            self._stop_message_border_animation()
            self.status_updated.emit()

            result = self._create_completion_result()
            self.submit_finished.emit(result)
            self.update_label.emit()

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
        self._add_message(message)
        self._append_message_to_transcript(message)

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
                self._pending_tool_call_approval = msg_widget
                msg_widget.show_tool_approval_ui(tool_call, reason, destructive)
                self.update_label.emit()
                break

    def _on_tool_call_approved(self, _tool_call: AIToolCall) -> None:
        """Handle user approval of tool calls."""
        self._pending_tool_call_approval = None
        ai_conversation = cast(AIConversation, self._ai_conversation)
        loop = asyncio.get_event_loop()
        loop.create_task(ai_conversation.approve_pending_tool_calls())

    def _on_tool_call_rejected(self, reason: str) -> None:
        """Handle user rejection of tool calls."""
        self._pending_tool_call_approval = None
        ai_conversation = cast(AIConversation, self._ai_conversation)
        loop = asyncio.get_event_loop()
        loop.create_task(ai_conversation.reject_pending_tool_calls(reason))

    def _on_message_expand_requested(self, expanded: bool) -> None:
        """
        Handle the change in scroll behaviour when a message is expanded or collapsed.
        Args:
            expanded: Whether the message is expanded or not
        """
        if expanded:
            self._auto_scroll = False

            # When a message is expanded, ensure its sections are highlighted if layout is complete
            if self._initial_layout_complete:
                self._lazy_update_visible_sections()

        # Update animation target if visibility changed
        if self._is_animating:
            self._update_animated_message()

    async def _on_message_added(self, message: AIMessage) -> None:
        """
        Handle a new message being added to the conversation.

        Args:
            message: The message that was added
        """
        self._current_unfinished_message = message
        self._add_message(message)

        # Start animation if not already animating
        if not self._is_animating:
            self._start_message_border_animation()

        # When we call this we should always scroll to the bottom and restore auto-scrolling
        self._auto_scroll = True
        self._scroll_to_bottom()

    def _update_last_message(self, message: AIMessage) -> None:
        # Update the last message
        if not self._messages:
            self._logger.warning("No messages to update with the last message.")
            return

        if message.source not in (AIMessageSource.AI, AIMessageSource.REASONING):
            return

        for i in range(len(self._messages) - 1, -1, -1):
            if self._messages[i].message_id() == message.id:
                self._messages[i].set_content(message.content)

        # Scroll to bottom if auto-scrolling is enabled
        if self._auto_scroll:
            self._scroll_to_bottom()

    async def _process_pending_update(self) -> None:
        """Process any pending message update."""
        if not self._pending_message:
            return

        self._update_last_message(self._pending_message)
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
            self._update_last_message(message_copy)
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
        self._update_last_message(message)
        self._append_message_to_transcript(message)
        self.status_updated.emit()

    async def _on_request_completed(self) -> None:
        """
        Handle completed AI request.
        """
        # Update status bar with token counts
        self._is_streaming = False
        self._input.set_streaming(False)
        self._stop_message_border_animation()

        # Reset message update throttling state
        self._pending_message = None
        if self._update_timer.isActive():
            self._update_timer.stop()

        self.status_updated.emit()

        result = self._create_completion_result()
        self.submit_finished.emit(result)
        self.update_label.emit()
        if self._initial_layout_complete:
            self._lazy_update_visible_sections()

    def _on_language_changed(self) -> None:
        """Update language-specific elements when language changes."""
        # Update input widget streaming state text
        self._input.set_streaming(self._is_streaming)

        # Emit signal for status update
        self.status_updated.emit()

    def _append_message_to_transcript(self, message: AIMessage) -> None:
        """
        Append messages to transcript file.

        Args:
            message: AIMessage to append to transcript
        """
        try:
            self._transcript_handler.append_message(message.to_transcript_dict())

        except AIConversationTranscriptError:
            self._logger.exception("Failed to write to transcript")

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

        # Check for newly visible sections that need highlighting (only after initial layout is complete)
        if self._initial_layout_complete:
            self._lazy_update_visible_sections()

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
        self._lazy_update_visible_sections()

    def activate(self) -> None:
        """Activate the conversation widget."""
        # If we have a focus message then focus it
        if self._focused_message_index != -1:
            self._messages[self._focused_message_index].set_focused(True)
            return

        # If our input box is hidden then focus the last message.
        if self._input.isHidden():
            last_visible_index = self._find_last_visible_message()
            self._focused_message_index = last_visible_index
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

    def _on_widget_activated(self, widget: QWidget) -> None:
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

    def _on_widget_deactivated(self, widget: QWidget) -> None:
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

    def _find_next_visible_message(self, start_index: int) -> int:
        """
        Find the next visible message starting from start_index + 1.

        Args:
            start_index: Index to start searching from (exclusive)

        Returns:
            Index of next visible message, or -1 if none found
        """
        for i in range(start_index + 1, len(self._messages)):
            if self._messages[i].is_rendered():
                return i

        return -1

    def _find_previous_visible_message(self, start_index: int) -> int:
        """
        Find the previous visible message starting from start_index - 1.

        Args:
            start_index: Index to start searching from (exclusive)

        Returns:
            Index of previous visible message, or -1 if none found
        """
        for i in range(start_index - 1, -1, -1):
            if self._messages[i].is_rendered():
                return i

        return -1

    def _find_last_visible_message(self) -> int:
        """
        Find the index of the last visible message.

        Returns:
            Index of last visible message, or -1 if none found
        """
        for i in range(len(self._messages) - 1, -1, -1):
            if self._messages[i].is_rendered():
                return i

        return -1

    def _find_first_visible_message(self) -> int:
        """
        Find the index of the first visible message.

        Returns:
            Index of first visible message, or -1 if none found
        """
        for i, message in enumerate(self._messages):
            if message.is_rendered():
                return i

        return -1

    def navigate_to_next_message(self) -> bool:
        """Navigate to the next visible message or input box if possible."""
        # If input box is focused, can't navigate further forward
        if self._focused_message_index == -1:
            return False

        # Find the next visible message
        next_visible_index = self._find_next_visible_message(self._focused_message_index)
        if next_visible_index != -1:
            # Move to next visible message
            self._messages[self._focused_message_index].set_focused(False)
            self._focused_message_index = next_visible_index
            self._focus_message()
            return True

        # No more visible messages - try to move to input if it's visible
        if self._input.isVisible():
            self._messages[self._focused_message_index].set_focused(False)
            self._focused_message_index = -1
            self._focus_message()
            return True

        # Input is not visible, can't navigate further
        return False

    def navigate_to_previous_message(self) -> bool:
        """Navigate to the previous visible message if possible."""
        # If input box is focused, move to the last visible message
        if self._focused_message_index == -1:
            last_visible_index = self._find_last_visible_message()
            if last_visible_index != -1:
                self._input.set_focused(False)
                self._focused_message_index = last_visible_index
                self._focus_message()
                return True
            return False

        # Find the previous visible message
        prev_visible_index = self._find_previous_visible_message(self._focused_message_index)
        if prev_visible_index != -1:
            # Move to previous visible message
            self._messages[self._focused_message_index].set_focused(False)
            self._focused_message_index = prev_visible_index
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

        message_spacing = int(self._style_manager.message_bubble_spacing())
        message_y = message_pos.y()

        # Determine if scrolling is needed
        if delta < 0:
            # Message is above visible area
            y = max(0, message_y - message_spacing)
            self._scroll_area.verticalScrollBar().setValue(y)

        elif delta + message.height() > viewport_height:
            # Message is below visible area
            if message.height() > viewport_height:
                y = max(0, message_y - message_spacing)
                self._scroll_area.verticalScrollBar().setValue(y)

            else:
                y = message_y + message.height() - viewport_height + message_spacing
                self._scroll_area.verticalScrollBar().setValue(y)

    def can_navigate_next_message(self) -> bool:
        """Check if navigation to next visible message is possible."""
        # If input is focused, can't navigate further forward
        if self._focused_message_index == -1:
            return False

        # If on a message, check if there are visible messages after current position
        # or if input is visible (can move to input)
        next_visible_index = self._find_next_visible_message(self._focused_message_index)
        return next_visible_index != -1 or self._input.isVisible()

    def can_navigate_previous_message(self) -> bool:
        """Check if navigation to previous visible message is possible."""
        # If input is focused, check if there are any visible messages to go back to
        if self._focused_message_index == -1:
            return self._find_last_visible_message() != -1

        # If on a message, check if there are visible messages before current position
        return self._find_previous_visible_message(self._focused_message_index) != -1

    def _on_selection_changed(self, message_widget: ConversationMessage, has_selection: bool) -> None:
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

    def path(self) -> str:
        """
        Get the conversation file path.

        Returns:
            str: Full path to the conversation file
        """
        return self._transcript_handler.get_path()

    def set_path(self, new_path: str) -> None:
        """
        Set the conversation file path.

        Args:
            new_path: New path for the conversation file
        """
        self._transcript_handler.set_path(new_path)

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

    def set_conversation_history(self, history: AIConversationHistory) -> None:
        """
        Set the conversation history for this widget.

        Args:
            history: AIConversationHistory object containing messages
        """
        try:
            # Write history to new transcript file
            self._transcript_handler.write(history)

            # Load messages into the new tab
            self._load_message_history(history.get_messages(), False)

        except Exception as e:
            raise ConversationError(f"Failed to write transcript for new history: {str(e)}") from e

    def _load_message_history(self, messages: List[AIMessage], reuse_ai_conversation: bool) -> None:
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

        for message in messages:
            self._add_message(message)

        # Ensure we're scrolled to the end
        self._auto_scroll = True
        self._scroll_to_bottom()

        self.status_updated.emit()

    def _delete_empty_transcript_file(self) -> None:
        """
        Delete the transcript file if the conversation doesn't have any AI messages.

        A conversation is considered empty if it has no messages with source AI or REASONING.
        """
        try:
            # Get all messages from the conversation
            messages = self.get_conversation_history().get_messages()

            # Check if there are any AI or REASONING messages
            has_ai_messages = any(
                msg.source in (AIMessageSource.AI, AIMessageSource.REASONING)
                for msg in messages
            )

            # If there are no AI messages and the file exists, delete it
            path = self._transcript_handler.get_path()
            if not has_ai_messages and os.path.exists(path):
                self._logger.info("Deleting empty conversation transcript: %s", path)
                os.remove(path)

        except Exception as e:
            self._logger.exception("Failed to delete empty conversation transcript: %s", e)

    def can_close(self) -> bool:
        """Check if the conversation can be closed, handling active streaming."""
        if not self._is_streaming:
            return True

        strings = self._language_manager.strings()
        result = MessageBox.show_message(
            self,
            MessageBoxType.QUESTION,
            strings.cancel_conversation_title,
            strings.cancel_conversation,
            [MessageBoxButton.YES, MessageBoxButton.NO],
            destructive=True
        )

        if result == MessageBoxButton.YES:
            return True

        return False

    def close_widget(self) -> None:
        """Close the conversation."""
        # If this is a delegated conversation, we need to ensure we notify the parent
        if self._is_delegated_conversation and self._is_streaming:
            result = self._create_completion_result()
            self.submit_finished.emit(result)

        self._unregister_ai_conversation_callbacks()
        self._stop_message_border_animation()
        self._delete_empty_transcript_file()

    def resizeEvent(self, event: QResizeEvent) -> None:
        """Handle resize events to detect layout stabilization and trigger lazy updating."""
        super().resizeEvent(event)

        if self._auto_scroll:
            self._scroll_to_bottom()

    def showEvent(self, event: QShowEvent) -> None:
        """Ensure visible sections are highlighted when widget becomes visible."""
        super().showEvent(event)

        # Only check visibility if initial layout is complete
        if not self._initial_layout_complete:
            # Reset the timer each time we get a resize during initial loading
            self._layout_stabilization_timer.start(100)

        else:
            # After initial layout is complete, check for visibility changes after resize
            self._lazy_update_visible_sections()

    def cancel_current_tasks(self) -> None:
        """Cancel any ongoing AI response tasks."""
        # First remove any active tool approval UI
        if self._pending_tool_call_approval:
            self._pending_tool_call_approval.remove_tool_approval_ui()
            self._pending_tool_call_approval = None

        ai_conversation = cast(AIConversation, self._ai_conversation)
        ai_conversation.cancel_current_tasks()

    def _build_widget_style(self) -> str:
        """Build styles for the conversation widget."""

        return f"""
            QWidget {{
                background-color: {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
            }}

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
            QScrollBar::add-page:vertical,
            QScrollBar::sub-page:vertical {{
                background: none;
            }}
            QScrollBar::add-line:vertical,
            QScrollBar::sub-line:vertical {{
                height: 0px;
            }}
        """

    def _build_conversation_message_styles(self) -> str:
        """Build styles for the main message frame."""
        style_manager = self._style_manager
        border_radius = int(self._style_manager.message_bubble_spacing())

        return f"""
            #ConversationMessage {{
                margin: 0;
                border-radius: {border_radius}px;
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
                border: 2px solid {style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
            }}
            #ConversationMessage[message_source="user"] {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_USER_BACKGROUND)};
                border: 2px solid {style_manager.get_color_str(ColorRole.MESSAGE_USER_BACKGROUND)};
            }}

            #ConversationMessage #_header,
            #ConversationMessage #_sections_container {{
                background-color: transparent;
                border: none;
                border-radius: 0;
                padding: 0;
                margin: 0;
            }}

            #ConversationMessage #_role_label {{
                margin: 0;
                padding: 0;
                border: none;
                background-color: transparent;
            }}
            #ConversationMessage #_role_label[message_source="user"] {{
                color: {style_manager.get_color_str(ColorRole.MESSAGE_USER)};
            }}
            #ConversationMessage #_role_label[message_source="ai_connected"] {{
                color: {style_manager.get_color_str(ColorRole.MESSAGE_STREAMING)};
            }}
            #ConversationMessage #_role_label[message_source="ai"] {{
                color: {style_manager.get_color_str(ColorRole.MESSAGE_AI)};
            }}
            #ConversationMessage #_role_label[message_source="reasoning"] {{
                color: {style_manager.get_color_str(ColorRole.MESSAGE_REASONING)};
            }}
            #ConversationMessage #_role_label[message_source="tool_call"] {{
                color: {style_manager.get_color_str(ColorRole.MESSAGE_TOOL_CALL)};
            }}
            #ConversationMessage #_role_label[message_source="tool_result"] {{
                color: {style_manager.get_color_str(ColorRole.MESSAGE_TOOL_RESULT)};
            }}
            #ConversationMessage #_role_label[message_source="system"] {{
                color: {style_manager.get_color_str(ColorRole.MESSAGE_SYSTEM_ERROR)};
            }}

            #ConversationMessage #_expand_button,
            #ConversationMessage #_copy_button,
            #ConversationMessage #_save_button,
            #ConversationMessage #_fork_button,
            #ConversationMessage #_delete_button {{
                background-color: transparent;
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: none;
                padding: 0px;
                margin: 0px;
            }}

            #ConversationMessage #_copy_button:hover,
            #ConversationMessage #_save_button:hover,
            #ConversationMessage #_fork_button:hover,
            #ConversationMessage #_delete_button:hover {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_HOVER)};
            }}

            #ConversationMessage #_copy_button:pressed,
            #ConversationMessage #_save_button:pressed,
            #ConversationMessage #_fork_button:pressed,
            #ConversationMessage #_delete_button:pressed {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_PRESSED)};
            }}

            #ConversationMessage #_approval_widget {{
                background-color: transparent;
                border: none;
            }}

            #ConversationMessage #_approval_text_edit {{
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                background-color: transparent;
                border: none;
                border-radius: 0px;
                padding: 0;
                margin: 0;
            }}

            #ConversationMessage #_approval_approve_button[recommended="true"] {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_RECOMMENDED)};
                color: {style_manager.get_color_str(ColorRole.TEXT_RECOMMENDED)};
                border-radius: 4px;
            }}
            #ConversationMessage #_approval_approve_button[recommended="true"]:hover {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_RECOMMENDED_HOVER)};
            }}
            #ConversationMessage #_approval_approve_button[recommended="true"]:pressed {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_RECOMMENDED_PRESSED)};
            }}
            #ConversationMessage #_approval_approve_button[recommended="false"] {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE)};
                color: {style_manager.get_color_str(ColorRole.TEXT_RECOMMENDED)};
                border-radius: 4px;
            }}
            #ConversationMessage #_approval_approve_button[recommended="false"]:hover {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE_HOVER)};
            }}
            #ConversationMessage #_approval_approve_button[recommended="false"]:pressed {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE_PRESSED)};
            }}

            #ConversationMessage #_approval_reject_button {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_SECONDARY_BACKGROUND)};
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                border-radius: 4px;
            }}
            #ConversationMessage #_approval_reject_button:hover {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_SECONDARY_BACKGROUND_HOVER)};
            }}
            #ConversationMessage #_approval_reject_button:pressed {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_SECONDARY_BACKGROUND_PRESSED)};
            }}
        """

    def _build_conversation_message_section_styles(self) -> str:
        """Build styles for message sections."""
        style_manager = self._style_manager
        border_radius = int(style_manager.message_bubble_spacing() / 2)
        return f"""
            #ConversationMessage #ConversationMessageSection[section_style="text-system"] {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
                margin: 0;
                border-radius: {border_radius}px;
                border: 0;
            }}
            #ConversationMessage #ConversationMessageSection[section_style="text-user"] {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_USER_BACKGROUND)};
                margin: 0;
                border-radius: {border_radius}px;
                border: 0;
            }}
            #ConversationMessage #ConversationMessageSection[section_style="code-system"] {{
                background-color: {style_manager.get_color_str(ColorRole.BACKGROUND_TERTIARY)};
                margin: 0;
                border-radius: {border_radius}px;
                border: 0;
            }}
            #ConversationMessage #ConversationMessageSection[section_style="code-user"] {{
                background-color: {style_manager.get_color_str(ColorRole.BACKGROUND_TERTIARY)};
                margin: 0;
                border-radius: {border_radius}px;
                border: 0;
            }}

            #ConversationMessage #ConversationMessageSection #_header_container {{
                background-color: transparent;
                border: none;
                border-radius: 0;
                padding: 0;
                margin: 0;
            }}

            /* Text areas within message sections */
            #ConversationMessage #ConversationMessageSection QTextEdit {{
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                background-color: transparent;
                border: none;
                padding: 0;
                margin: 0;
                selection-background-color: {style_manager.get_color_str(ColorRole.TEXT_SELECTED)};
            }}

            /* Labels (language headers) within message sections */
            #ConversationMessage #ConversationMessageSection QLabel {{
                color: {style_manager.get_color_str(ColorRole.MESSAGE_LANGUAGE)};
                background-color: transparent;
                margin: 0;
                padding: 0;
            }}

            /* Header containers within message sections */
            #ConversationMessage #ConversationMessageSection QWidget {{
                background-color: transparent;
                margin: 0;
                padding: 0;
            }}

            /* Buttons within message sections */
            #ConversationMessage #ConversationMessageSection QToolButton {{
                background-color: transparent;
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: none;
                padding: 0px;
            }}
            #ConversationMessage #ConversationMessageSection QToolButton:hover {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_HOVER)};
            }}
            #ConversationMessage #ConversationMessageSection QToolButton:pressed {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_PRESSED)};
            }}

            /* Scrollbars within message sections */
            #ConversationMessage #ConversationMessageSection QScrollBar:horizontal {{
                height: 12px;
                background: {style_manager.get_color_str(ColorRole.SCROLLBAR_BACKGROUND)};
            }}
            #ConversationMessage #ConversationMessageSection QScrollBar::handle:horizontal {{
                background: {style_manager.get_color_str(ColorRole.SCROLLBAR_HANDLE)};
                min-width: 20px;
            }}
            #ConversationMessage #ConversationMessageSection QScrollBar::add-page:horizontal,
            #ConversationMessage #ConversationMessageSection QScrollBar::sub-page:horizontal {{
                background: none;
            }}
            #ConversationMessage #ConversationMessageSection QScrollBar::add-line:horizontal,
            #ConversationMessage #ConversationMessageSection QScrollBar::sub-line:horizontal {{
                width: 0px;
            }}
        """

    def _on_style_changed(self) -> None:
        """Update styles when the application style changes."""
        factor = self._style_manager.zoom_factor()
        font = self.font()
        base_font_size = self._style_manager.base_font_size()
        font.setPointSizeF(base_font_size * factor)
        self.setFont(font)

        stylesheet_parts = [
            self._build_widget_style(),
            self._build_conversation_message_styles(),
            self._build_conversation_message_section_styles()
        ]

        shared_stylesheet = "\n".join(stylesheet_parts)
        self.setStyleSheet(shared_stylesheet)

        if self._initial_layout_complete:
            self._initial_layout_complete = False
            self._layout_stabilization_timer.start(100)

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
        fork_action.triggered.connect(self.fork_requested)
        menu.addSeparator()

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
            if message_widget.is_rendered():
                break

            # This is a hidden message, include it and continue
            current_index += 1

        # Return the index of the last message to include
        # (current_index - 1 because we stopped at the first visible message)
        return current_index - 1

    def _on_message_fork_requested(self) -> None:
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
            self.fork_requested.emit()
            return

        message_index = self._messages.index(message)

        # Find the actual end index including hidden messages
        fork_end_index = self._find_fork_end_index(message_index)

        # Emit signal with the end index (inclusive)
        self.fork_from_index_requested.emit(fork_end_index)

    def _on_message_delete_requested(self) -> None:
        """Handle request to delete conversation from a message onwards."""
        # Identify which message widget triggered the request
        sender = self.sender()
        if not isinstance(sender, ConversationMessage):
            return

        index = self._messages.index(sender)
        if index < 0 or index >= len(self._messages):
            return

        assert self._messages[index].message_source() == AIMessageSource.USER, "Only user messages can be deleted."

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

        # Capture the prompt from the first message we're deleting.
        assert all_messages[index].source == AIMessageSource.USER, "Only user messages can be deleted."
        prompt = all_messages[index].content

        # Keep only the messages up to the specified index
        preserved_history_messages = all_messages[:index]

        # Update the AI conversation history
        ai_conversation.load_message_history(preserved_history_messages)

        # Work out what the conversation settings now are - they may have changed
        conversation_settings = ai_conversation.conversation_settings()
        self._input.set_model(conversation_settings.model)

        # Update the transcript file by rewriting it with only the preserved messages
        preserved_history = AIConversationHistory(preserved_history_messages, history.version(), history.parent())

        try:
            self._transcript_handler.write(preserved_history)

            # Stop any animation on deleted messages
            if self._animated_message and self._animated_message not in preserved_messages:
                self._stop_message_border_animation()

            # Emit status update
            self.status_updated.emit()

            # Put the focus back to the input
            self._focused_message_index = -1
            self._input.set_content(prompt)
            self._input.set_focused(True)

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

    def submit(
        self,
        requester: str | None = None,
        model: str | None = None,
        temperature: float | None = None,
        reasoning_capability: AIReasoningCapability | None = None
    ) -> None:
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

        ai_conversation = cast(AIConversation, self._ai_conversation)
        settings = ai_conversation.conversation_settings()
        if model is None:
            model = settings.model

        if temperature is None:
            temperature = settings.temperature

        if reasoning_capability is None:
            reasoning_capability = settings.reasoning

        sanitized_content = self._sanitize_input(content)
        message = AIMessage.create(
            AIMessageSource.USER, sanitized_content, user_name=requester,
            model=model, temperature=temperature, reasoning_capability=reasoning_capability
        )

        self._last_submitted_message = content
        self._add_message(message)

        # Submit the message to the AIConversation instance
        loop = asyncio.get_event_loop()
        if not loop.is_running():
            return

        loop.create_task(ai_conversation.submit_message(message))
        self._append_message_to_transcript(message)

        # Start animation if not already animating
        if not self._is_animating:
            self._start_message_border_animation()

        # When we call this we should always scroll to the bottom and restore auto-scrolling
        self._auto_scroll = True
        self._scroll_to_bottom()

    def _on_stop_requested(self) -> None:
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

        # Is this a conversation or a delegated conversation?
        metadata["delegated_conversation"] = self._is_delegated_conversation

        # Store current input content
        metadata["content"] = self._input.to_plain_text()
        metadata['cursor'] = self._get_cursor_position()

        metadata["auto_scroll"] = self._auto_scroll
        metadata["vertical_scroll"] = self._scroll_area.verticalScrollBar().value()

        # Store message expansion states
        expansion_states = []
        for message_widget in self._messages:
            expansion_states.append(message_widget.is_expanded())

        metadata["message_expansion"] = expansion_states

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

        delegated_conversation = False
        if "delegated_conversation" in metadata:
            delegated_conversation = metadata["delegated_conversation"]

        self._set_delegated_conversation_mode(delegated_conversation)

        # Restore input content if specified
        if "content" in metadata:
            content = metadata["content"]
            if content:
                self.set_input_text(metadata["content"])

        if "cursor" in metadata:
            self._set_cursor_position(metadata["cursor"])

        # Restore vertical scroll position if specified
        if "auto_scroll" in metadata:
            self._initial_auto_scroll = metadata["auto_scroll"]

        if "vertical_scroll" in metadata:
            self._initial_scroll_position = metadata["vertical_scroll"]

        # Restore message expansion states if specified
        if "message_expansion" in metadata:
            expansion_states = metadata["message_expansion"]
            for i, is_expanded in enumerate(expansion_states):
                self._change_message_expansion(i, is_expanded)

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
                self._add_message(current_unfinished_message)
                self._current_unfinished_message = current_unfinished_message

            # Start animation if we're streaming
            if self._is_streaming and not self._is_animating:
                self._start_message_border_animation()

        else:
            # Restore settings
            if "settings" in metadata:
                settings = AIConversationSettings(
                    model=metadata["settings"].get("model"),
                    temperature=metadata["settings"].get("temperature"),
                    reasoning=AIReasoningCapability(metadata["settings"].get("reasoning", AIReasoningCapability.NO_REASONING.value))
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
