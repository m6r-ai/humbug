"""Conversation widget implementation."""

import asyncio
import logging
import os
from typing import Dict, List, Tuple, Any, Set, cast

from PySide6.QtWidgets import (
    QWidget, QVBoxLayout, QScrollArea, QSizePolicy, QMenu
)
from PySide6.QtCore import QTimer, QPoint, Qt, Signal, QObject
from PySide6.QtGui import QCursor, QResizeEvent

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


class ConversationWidget(QWidget):
    """Widget for displaying conversation with message history and input."""

    # Signal to notify tab of status changes
    status_updated = Signal()

    # Signals for tab to handle forking a conversation
    fork_requested = Signal()  # Signal to fork the conversation
    fork_from_index_requested = Signal(int)  # Signal to fork from a specific message index

    # Emits when conversation settings are requested
    conversation_settings_requested = Signal()

    # Emits when the conversation label should be updated
    update_label = Signal()

    # Emits when the has-seen-latest-update state changes
    has_seen_latest_update_changed = Signal(bool)

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

        style_manager = StyleManager()
        self._style_manager = style_manager

        self._ai_conversation = None
        if not use_existing_ai_conversation:
            self._ai_conversation = AIConversation()

            # Register callbacks for AIConversation events
            self._register_ai_conversation_callbacks()

        self._last_submitted_message: str = ""

        # We need to track any unfinished message because it won't appear in the transcript until
        # it completes.  If we move a conversation to a new tab, we need to ensure it doesn't get lost.
        self._current_unfinished_message: AIMessage | None = None

        self._update_timer = QTimer(self)  # Timer for throttled updates
        self._update_timer.setSingleShot(True)
        self._update_timer.timeout.connect(self._process_pending_update)
        self._pending_messages: Dict[str, AIMessage] = {}  # Store pending messages by message ID

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

        # Timer for debouncing container visibility to eliminate jitter
        self._container_show_timer = QTimer(self)
        self._container_show_timer.setSingleShot(True)
        self._container_show_timer.timeout.connect(self._enable_messages_container_updates)

        # Create layout
        conversation_layout = QVBoxLayout(self)
        self.setLayout(conversation_layout)
        conversation_layout.setContentsMargins(0, 0, 0, 0)
        conversation_layout.setSpacing(0)

        # Set up the scroll area
        self._scroll_area = QScrollArea()
        self._scroll_area.setObjectName("ConversationScrollArea")
        self._scroll_area.setFrameStyle(0)
        self._scroll_area.setWidgetResizable(True)
        self._scroll_area.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)
        self._scroll_area.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAsNeeded)
        self._scroll_area.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)

        # Connect to the vertical scrollbar's change signals
        self._scroll_area.verticalScrollBar().valueChanged.connect(self._on_scroll_value_changed)
        self._scroll_area.verticalScrollBar().rangeChanged.connect(self._on_scroll_range_changed)

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
        self._input.settings_requested.connect(self._on_input_settings_requested)
        self._input.modified.connect(self.conversation_modified)

        style_manager.style_changed.connect(self._on_style_changed)
        self._on_style_changed()

        self._messages_layout.addStretch()
        self._messages_layout.addWidget(self._input)

        self._messages_container.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Minimum)
        self._scroll_area.setWidget(self._messages_container)

        # Add the scroll area to the main layout
        conversation_layout.addWidget(self._scroll_area)

        # Setup signals for search highlights
        self._search_highlights: Dict[ConversationMessage, List[Tuple[int, int, int]]] = {}

        # Tracking for spotlighted message
        self._spotlighted_message_index = -1

        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._on_language_changed)

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
        self.customContextMenuRequested.connect(self._show_conversation_context_menu)

        # Find functionality
        self._matches: List[Tuple[ConversationMessage, List[Tuple[int, int, int]]]] = []
        self._current_widget_index = -1
        self._current_match_index = -1
        self._last_search = ""
        self._highlighted_widgets: Set[ConversationMessage] = set()

        # Create transcript handler with provided filename, then load the transcript data
        self._transcript_handler = AIConversationTranscriptHandler(path)
        try:
            conversation_history = self._transcript_handler.read()

        except AIConversationTranscriptError as e:
            raise ConversationError(f"Failed to read conversation transcript: {str(e)}") from e

        self._load_message_history(conversation_history.get_messages(), use_existing_ai_conversation)
        self._set_delegated_conversation_mode(os.path.basename(path).startswith("dAI-"))

        # Any active tool approval
        self._pending_tool_call_approval: ConversationMessage | None = None

    def _activate_widget(self, widget: QWidget) -> None:
        """
        Handle activation of a widget, spotlighting the associated message.

        Args:
            widget: The widget that was activated
        """
        # Find the ConversationMessage that contains this widget
        message_widget = self._find_conversation_message(widget)
        if message_widget is None:
            # We couldn't find it so active the last spotlighted message or input
            if self._spotlighted_message_index != -1:
                self._messages[self._spotlighted_message_index].set_spotlighted(True)
                self._messages[self._spotlighted_message_index].setFocus()
                return

            # If our input box is hidden then spotlight the last message.
            if self._input.isHidden():
                last_visible_index = self._find_last_visible_message()
                if last_visible_index == -1:
                    return

                self._spotlighted_message_index = last_visible_index
                self._messages[self._spotlighted_message_index].set_spotlighted(True)
                self._messages[self._spotlighted_message_index].setFocus()
                return

            self._input.set_spotlighted(True)
            self._input.setFocus()
            return

        if message_widget.is_spotlighted():
            return

        # Remove focus from the currently spotlighted message
        if self._spotlighted_message_index != -1:
            self._messages[self._spotlighted_message_index].set_spotlighted(False)

        else:
            self._input.set_spotlighted(False)

        # Set spotlight on the new message
        if message_widget in self._messages:
            self._spotlighted_message_index = self._messages.index(message_widget)
            message_widget.set_spotlighted(True)
            return

        self._spotlighted_message_index = -1
        self._input.set_spotlighted(True)

    def _deactivate_widget(self, widget: QWidget) -> None:
        """
        Handle deactivation of a widget, checking if spotlight is leaving the associated message.

        Args:
            widget: The widget that lost focus
        """
        # Find the ConversationMessage that contains this widget
        message_widget = self._find_conversation_message(widget)
        if message_widget is None:
            return

        # Remove focus from the currently spotlighted message
        if self._spotlighted_message_index != -1:
            self._messages[self._spotlighted_message_index].set_spotlighted(False)

        else:
            self._input.set_spotlighted(False)

    def set_active(self, widget: QWidget, active: bool) -> None:
        """
        Set the active state of the conversation widget.

        Args:
            widget: The widget that triggered the activation change
            active: True if the conversation is now active, False otherwise
        """
        # Propagate to input box
        if active:
            self._activate_widget(widget)
            return

        self._deactivate_widget(widget)

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

    def _add_message_core(self, message: AIMessage) -> ConversationMessage:
        """Core of the _add_message method that avoid unecessary UI updates."""
        msg_widget = ConversationMessage(
            message.source,
            message.timestamp,
            message.model or "",
            message.id,
            message.user_name,
            message.content,
            message.tool_call_context
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
        msg_widget.tool_call_i_am_unsure.connect(self._on_tool_call_i_am_unsure)
        msg_widget.tool_call_rejected.connect(self._on_tool_call_rejected)

        self._messages.append(msg_widget)

        # Add widget before input and the stretch
        self._messages_layout.insertWidget(self._messages_layout.count() - 2, msg_widget)
        return msg_widget

    def _add_message(self, message: AIMessage) -> None:
        """
        Add a new message to the conversation view.

        Args:
            message: The message that was added
        """
        self._hide_last_ai_connected_message()

        # If this is a USER message, delete any previous USER_QUEUED messages
        if message.source == AIMessageSource.USER:
            self._delete_user_queued_messages()

        # If we're not auto-scrolling we want to disable updates during insertion to prevent jitter
        if not self._auto_scroll:
            # Cancel any pending show timer and hide container during insertion
            if self._container_show_timer.isActive():
                self._container_show_timer.stop()

            self._messages_container.setUpdatesEnabled(False)

        msg_widget = self._add_message_core(message)
        msg_widget.apply_style()

        if not self._auto_scroll:
            self._container_show_timer.start(5)

        # If we're not animating then we've done everything we need to.
        if not self._is_animating:
            return

        # We're currently animating, transfer animation to this new message
        # but only if the new message is not hidden
        if msg_widget.is_rendered():
            self._transfer_animation_to_message(msg_widget)

        # We're animating but current animated message is no longer visible,
        # find a new visible message to animate
        elif self._animated_message and not self._animated_message.is_rendered():
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

    def _delete_user_queued_messages(self) -> None:
        """
        Delete all USER_QUEUED messages from the UI.

        This is called when a USER message is added, indicating that any
        queued messages have been consumed to create the actual user prompt.
        These messages are temporary UI-only widgets and should be completely
        removed when no longer needed.
        """
        for i in range(len(self._messages) - 1, -1, -1):
            if self._messages[i].message_source() != AIMessageSource.USER_QUEUED:
                continue

            # Remove widgets
            message_widget = self._messages[i]

            # Clear selection if this message was selected
            if self._message_with_selection == message_widget:
                self._message_with_selection = None

            # Remove from layout
            self._messages_layout.removeWidget(message_widget)

            # Delete the widget
            message_widget.deleteLater()

            # Remove from list
            del self._messages[i]

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

    def _enable_messages_container_updates(self) -> None:
        """Re-enable updates for the messages container after layout has settled."""
        self._messages_container.setUpdatesEnabled(True)

        # Only unpolish/polish the specific widget that changed, not the entire container
        self._messages_container.style().unpolish(self._messages_container)
        self._messages_container.style().polish(self._messages_container)

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
            AIConversationEvent.MESSAGE_ADDED_AND_COMPLETED, self._on_message_added_and_completed
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
            AIConversationEvent.MESSAGE_ADDED_AND_COMPLETED, self._on_message_added_and_completed
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
        try:
            self._transcript_handler.write(self.get_conversation_history())

        except AIConversationTranscriptError:
            self._logger.exception("Failed to write to transcript")

        # Start animation if not already animating
        if not self._is_animating:
            self._start_message_border_animation()

        # Scroll to bottom if in auto-scroll mode, otherwise mark tab as updated
        if self._auto_scroll:
            self._scroll_to_bottom()

        else:
            # User is scrolled up, notify them there's new content below
            self.update_label.emit()

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
        try:
            self._transcript_handler.write(self.get_conversation_history())

        except AIConversationTranscriptError:
            self._logger.exception("Failed to write to transcript")

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

        # Scroll to bottom if in auto-scroll mode, otherwise mark tab as updated
        if self._auto_scroll:
            self._scroll_to_bottom()

        else:
            # User is scrolled up, notify them there's new content below
            self.update_label.emit()

    async def _on_tool_approval_required(
        self,
        message: AIMessage,
        tool_call: AIToolCall,
        reason: str,
        context: str | None,
        destructive: bool
    ) -> None:
        """
        Handle tool approval requirement.

        Args:
            message: The tool call message
            tool_call: Tool call requiring approval
            reason: Reason for the tool call
            context: Additional context for the tool call
            destructive: Whether the tool calls are considered destructive
        """
        # Find the message widget that corresponds to this tool call message
        for msg_widget in self._messages:
            if msg_widget.message_id() == message.id:
                # Add approval UI to this message
                self._pending_tool_call_approval = msg_widget
                msg_widget.show_tool_approval_ui(tool_call, reason, context, destructive)
                self.update_label.emit()
                break

    def _on_tool_call_approved(self, _tool_call: AIToolCall) -> None:
        """Handle user approval of tool calls."""
        self._pending_tool_call_approval = None
        ai_conversation = cast(AIConversation, self._ai_conversation)
        loop = asyncio.get_event_loop()
        loop.create_task(ai_conversation.approve_pending_tool_calls())

    def _on_tool_call_i_am_unsure(self) -> None:
        """Handle user indicating uncertainty about tool calls."""
        # Clean up the tool approval UI
        if self._pending_tool_call_approval:
            self._pending_tool_call_approval.remove_tool_approval_ui()
            self._pending_tool_call_approval = None

        # Get the AI conversation instance
        ai_conversation = cast(AIConversation, self._ai_conversation)

        # Reject the pending tool calls with explanation
        loop = asyncio.get_event_loop()
        loop.create_task(ai_conversation.reject_pending_tool_calls(
            "User is unsure about this tool request and wants to discuss"
        ))

        # Submit the user's message to continue the conversation
        loop.create_task(ai_conversation.submit_message(
            None, "I'm not sure about this tool request. Let's discuss."))

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

        # Scroll to bottom if in auto-scroll mode, otherwise mark tab as updated
        if self._auto_scroll:
            self._scroll_to_bottom()

        else:
            # User is scrolled up, notify them there's new content below
            self.update_label.emit()

    def _update_last_message(self, message: AIMessage) -> None:
        # Update the last message
        if not self._messages:
            self._logger.warning("No messages to update with the last message.")
            return

        if message.source not in (AIMessageSource.AI, AIMessageSource.REASONING):
            return

        # If we're not auto-scrolling we want to disable updates during insertion to prevent jitter
        if not self._auto_scroll:
            # Cancel any pending show timer and hide container during content update
            if self._container_show_timer.isActive():
                self._container_show_timer.stop()

            self._messages_container.setUpdatesEnabled(False)

        for i in range(len(self._messages) - 1, -1, -1):
            if self._messages[i].message_id() == message.id:
                self._messages[i].set_content(message.content)
                break

        if not self._auto_scroll:
            # Defer re-enabling updates until layout settles (after all resize events)
            self._container_show_timer.start(5)

        else:
            # Scroll to bottom if auto-scrolling is enabled
            self._scroll_to_bottom()

    def _process_pending_update(self) -> None:
        """Process all pending message updates."""
        if not self._pending_messages:
            return

        for message in self._pending_messages.values():
            self._update_last_message(message)

        self._pending_messages.clear()

    async def _on_message_updated(self, message: AIMessage) -> None:
        """
        Handle a message being updated with throttling.

        Updates are batched and processed together every 20ms to avoid
        excessive UI updates while supporting multiple concurrent messages.

        Args:
            message: The message that was updated
        """
        # Store the message update (will overwrite if same message updates multiple times)
        self._pending_messages[message.id] = message.copy()

        if not self._update_timer.isActive():
            self._update_timer.start(20)

    async def _on_message_completed(self, message: AIMessage) -> None:
        """
        Handle a message being completed.

        This processes any pending updates for this message and immediately
        updates with the completed message.

        Args:
            message: The message that was completed
        """
        self._current_unfinished_message = None

        # Remove this message from pending updates if present
        self._pending_messages.pop(message.id, None)

        # Stop timer if no more pending messages
        if self._update_timer.isActive():
            if not self._pending_messages:
                self._update_timer.stop()

        # Update with the completed message immediately
        self._update_last_message(message)
        try:
            self._transcript_handler.write(self.get_conversation_history())

        except AIConversationTranscriptError:
            self._logger.exception("Failed to write to transcript")

        self.status_updated.emit()

    async def _on_message_added_and_completed(self, message: AIMessage) -> None:
        """
        Handle a message being added and completed in the conversation.

        Args:
            message: The message that was added and completed
        """
        await self._on_message_added(message)
        await self._on_message_completed(message)

    async def _on_request_completed(self) -> None:
        """
        Handle completed AI request.
        """
        # Update status bar with token counts
        self._is_streaming = False
        self._input.set_streaming(False)
        self._stop_message_border_animation()

        # Reset message update throttling state
        self._pending_messages.clear()
        if self._update_timer.isActive():
            self._update_timer.stop()

        self.status_updated.emit()

        result = self._create_completion_result()
        self.submit_finished.emit(result)
        self.update_label.emit()

    def _on_language_changed(self) -> None:
        """Update language-specific elements when language changes."""
        # Update input widget streaming state text
        self._input.set_streaming(self._is_streaming)

        # Emit signal for status update
        self.status_updated.emit()

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
        self._auto_scroll = at_bottom

        self.has_seen_latest_update_changed.emit(at_bottom)

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
        # If input box is spotlighted, can't navigate further forward
        if self._spotlighted_message_index == -1:
            return False

        # Find the next visible message
        next_visible_index = self._find_next_visible_message(self._spotlighted_message_index)
        if next_visible_index != -1:
            # Move to next visible message
            self._messages[self._spotlighted_message_index].set_spotlighted(False)
            self._spotlighted_message_index = next_visible_index
            self._spotlight_message()
            return True

        # No more visible messages - try to move to input if it's visible
        if self._input.isVisible():
            self._messages[self._spotlighted_message_index].set_spotlighted(False)
            self._spotlighted_message_index = -1
            self._spotlight_message()
            return True

        # Input is not visible, can't navigate further
        return False

    def navigate_to_previous_message(self) -> bool:
        """Navigate to the previous visible message if possible."""
        # If input box is spotlighted, move to the last visible message
        if self._spotlighted_message_index == -1:
            last_visible_index = self._find_last_visible_message()
            if last_visible_index != -1:
                self._input.set_spotlighted(False)
                self._spotlighted_message_index = last_visible_index
                self._spotlight_message()
                return True
            return False

        # Find the previous visible message
        prev_visible_index = self._find_previous_visible_message(self._spotlighted_message_index)
        if prev_visible_index != -1:
            # Move to previous visible message
            self._messages[self._spotlighted_message_index].set_spotlighted(False)
            self._spotlighted_message_index = prev_visible_index
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

    def _perform_scroll_to_position(self, message: ConversationMessage, y_offset: int) -> None:
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
        scroll_value = max(0, min(scroll_value, scrollbar.maximum()))

        self._start_smooth_scroll(scroll_value)

    def _scroll_to_message(self, message: ConversationMessage) -> None:
        """Ensure the message is visible in the scroll area."""
        # Get the position of the message in the scroll area
        message_pos = message.mapTo(self._messages_container, QPoint(0, 0))

        # Calculate the visible region
        scroll_value = self._scroll_area.verticalScrollBar().value()
        viewport_height = self._scroll_area.viewport().height()

        delta = message_pos.y() - scroll_value

        zoom_factor = self._style_manager.zoom_factor()
        message_spacing = int(self._style_manager.message_bubble_spacing() * zoom_factor)

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
        """Check if navigation to next visible message is possible."""
        # If input is spotlighted, can't navigate further forward
        if self._spotlighted_message_index == -1:
            return False

        # If on a message, check if there are visible messages after current position
        # or if input is visible (can move to input)
        next_visible_index = self._find_next_visible_message(self._spotlighted_message_index)
        return next_visible_index != -1 or self._input.isVisible()

    def can_navigate_previous_message(self) -> bool:
        """Check if navigation to previous visible message is possible."""
        # If input is spotlighted, check if there are any visible messages to go back to
        if self._spotlighted_message_index == -1:
            return self._find_last_visible_message() != -1

        # If on a message, check if there are visible messages before current position
        return self._find_previous_visible_message(self._spotlighted_message_index) != -1

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

        except AIConversationTranscriptError as e:
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
            message_widget = self._add_message_core(message)

            # Filter messages that shouldn't be shown in the UI
            if message_widget.message_source() in (AIMessageSource.USER_QUEUED, AIMessageSource.AI_CONNECTED):
                message_widget.set_rendered(False)

            else:
                message_widget.apply_style()

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

    def cancel_current_tasks(self, notify: bool = True) -> None:
        """Cancel any ongoing AI response tasks."""
        # First remove any active tool approval UI
        if self._pending_tool_call_approval:
            self._pending_tool_call_approval.remove_tool_approval_ui()
            self._pending_tool_call_approval = None

        ai_conversation = cast(AIConversation, self._ai_conversation)
        ai_conversation.cancel_current_tasks(notify)

    def _build_widget_style(self) -> str:
        """Build styles for the conversation widget."""
        style_manager = self._style_manager

        return f"""
            QWidget {{
                background-color: {style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
            }}

            {style_manager.get_menu_stylesheet()}

            QScrollArea {{
                background-color: {style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
                border: none;
            }}

            QScrollBar:vertical {{
                background-color: {style_manager.get_color_str(ColorRole.SCROLLBAR_BACKGROUND)};
                width: 12px;
            }}
            QScrollBar::handle:vertical {{
                background-color: {style_manager.get_color_str(ColorRole.SCROLLBAR_HANDLE)};
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
        zoom_factor = style_manager.zoom_factor()
        border_radius = int(style_manager.message_bubble_spacing() * zoom_factor)

        # The -2px padding above is to offset the 2px border so that the content area remains the same size
        return f"""
            #ConversationMessage {{
                margin: 0;
                border-radius: {border_radius}px;
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
                border: 2px solid {style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
                padding: -2px;
            }}
            #ConversationMessage[message_source="user"],
            #ConversationMessage[message_source="ai_streaming"] {{
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
            #ConversationMessage[message_source="user"] #_role_label {{
                color: {style_manager.get_color_str(ColorRole.MESSAGE_USER)};
            }}
            #ConversationMessage[message_source="ai_connected"] #_role_label,
            #ConversationMessage[message_source="ai_streaming"] #_role_label {{
                color: {style_manager.get_color_str(ColorRole.MESSAGE_STREAMING)};
            }}
            #ConversationMessage[message_source="ai"] #_role_label {{
                color: {style_manager.get_color_str(ColorRole.MESSAGE_AI)};
            }}
            #ConversationMessage[message_source="reasoning"] #_role_label {{
                color: {style_manager.get_color_str(ColorRole.MESSAGE_REASONING)};
            }}
            #ConversationMessage[message_source="tool_call"] #_role_label {{
                color: {style_manager.get_color_str(ColorRole.MESSAGE_TOOL_CALL)};
            }}
            #ConversationMessage[message_source="tool_result"] #_role_label {{
                color: {style_manager.get_color_str(ColorRole.MESSAGE_TOOL_RESULT)};
            }}
            #ConversationMessage[message_source="system"] #_role_label {{
                color: {style_manager.get_color_str(ColorRole.MESSAGE_SYSTEM_ERROR)};
            }}
            #ConversationMessage[message_source="user_queued"] #_role_label {{
                color: {style_manager.get_color_str(ColorRole.MESSAGE_USER_QUEUED)};
            }}

            #ConversationMessage #_expand_button,
            #ConversationMessage #_copy_button,
            #ConversationMessage #_save_button,
            #ConversationMessage #_fork_button,
            #ConversationMessage #_delete_button,
            #ConversationMessage #_stop_button,
            #ConversationMessage #_submit_button,
            #ConversationMessage #_settings_button {{
                background-color: transparent;
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: none;
                padding: 0px;
                margin: 0px;
            }}

            #ConversationMessage #_copy_button:hover,
            #ConversationMessage #_save_button:hover,
            #ConversationMessage #_fork_button:hover,
            #ConversationMessage #_delete_button:hover,
            #ConversationMessage #_stop_button:hover,
            #ConversationMessage #_submit_button:hover,
            #ConversationMessage #_settings_button:hover {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND_HOVER)};
            }}

            #ConversationMessage #_copy_button:pressed,
            #ConversationMessage #_save_button:pressed,
            #ConversationMessage #_fork_button:pressed,
            #ConversationMessage #_delete_button:pressed,
            #ConversationMessage #_stop_button:presse,
            #ConversationMessage #_submit_button:pressed,
            #ConversationMessage #_settings_button:pressed {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND_PRESSED)};
            }}

            #ConversationMessage #_stop_button:disabled,
            #ConversationMessage #_submit_button:disabled {{
                color: {style_manager.get_color_str(ColorRole.TEXT_DISABLED)};
                background-color: transparent;
            }}

            #ConversationMessage[message_source="user"] #_copy_button:hover,
            #ConversationMessage[message_source="user"] #_save_button:hover,
            #ConversationMessage[message_source="user"] #_fork_button:hover,
            #ConversationMessage[message_source="user"] #_delete_button:hover {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_USER_BACKGROUND_HOVER)};
            }}

            #ConversationMessage[message_source="user"] #_copy_button:pressed,
            #ConversationMessage[message_source="user"] #_save_button:pressed,
            #ConversationMessage[message_source="user"] #_fork_button:pressed,
            #ConversationMessage[message_source="user"] #_delete_button:pressed {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_USER_BACKGROUND_PRESSED)};
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

            #ConversationMessage #_approval_context_widget {{
                background-color: {style_manager.get_color_str(ColorRole.BACKGROUND_TERTIARY)};
                margin: 0;
                padding: 0;
                border-radius: {border_radius // 2}px;
                border: 1px solid {style_manager.get_color_str(ColorRole.CODE_BORDER)};
            }}

            #ConversationMessage #_approval_context_widget #_approval_context_text_edit {{
                background-color: transparent;
            }}

            /* Scrollbars within approval contexts */
            #ConversationMessage #_approval_context_widget #_approval_context_text_edit QScrollBar:horizontal {{
                height: 12px;
                background: {style_manager.get_color_str(ColorRole.SCROLLBAR_BACKGROUND)};
            }}
            #ConversationMessage #_approval_context_widget #_approval_context_text_edit QScrollBar::handle:horizontal {{
                background: {style_manager.get_color_str(ColorRole.SCROLLBAR_HANDLE)};
                min-width: 20px;
            }}
            #ConversationMessage #_approval_context_widget #_approval_context_text_edit QScrollBar::add-page:horizontal,
            #ConversationMessage #_approval_context_widget #_approval_context_text_edit QScrollBar::sub-page:horizontal {{
                background: none;
            }}
            #ConversationMessage #_approval_context_widget #_approval_context_text_edit QScrollBar::add-line:horizontal,
            #ConversationMessage #_approval_context_widget #_approval_context_text_edit QScrollBar::sub-line:horizontal {{
                width: 0px;
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

            #ConversationMessage #_approval_i_am_unsure_button {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_SECONDARY_BACKGROUND)};
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                border-radius: 4px;
            }}
            #ConversationMessage #_approval_i_am_unsure_button:hover {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_SECONDARY_BACKGROUND_HOVER)};
            }}
            #ConversationMessage #_approval_i_am_unsure_button:pressed {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_SECONDARY_BACKGROUND_PRESSED)};
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
        zoom_factor = style_manager.zoom_factor()
        border_radius = int(style_manager.message_bubble_spacing() * zoom_factor / 2)
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
                border: 1px solid {style_manager.get_color_str(ColorRole.CODE_BORDER)};
            }}
            #ConversationMessage #ConversationMessageSection[section_style="code-user"] {{
                background-color: {style_manager.get_color_str(ColorRole.BACKGROUND_TERTIARY)};
                margin: 0;
                border-radius: {border_radius}px;
                border: 1px solid {style_manager.get_color_str(ColorRole.CODE_BORDER)};
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
            #ConversationMessage #ConversationMessageSection[section_style="text-system"] QToolButton:hover {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND_HOVER)};
            }}
            #ConversationMessage #ConversationMessageSection[section_style="text-system"] QToolButton:pressed {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND_PRESSED)};
            }}
            #ConversationMessage #ConversationMessageSection[section_style="text-user"] QToolButton:hover {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_USER_BACKGROUND_HOVER)};
            }}
            #ConversationMessage #ConversationMessageSection[section_style="text-user"] QToolButton:pressed {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_USER_BACKGROUND_PRESSED)};
            }}
            #ConversationMessage #ConversationMessageSection[section_style="code-system"] QToolButton:hover {{
                background-color: {style_manager.get_color_str(ColorRole.BACKGROUND_TERTIARY_HOVER)};
            }}
            #ConversationMessage #ConversationMessageSection[section_style="code-system"] QToolButton:pressed {{
                background-color: {style_manager.get_color_str(ColorRole.BACKGROUND_TERTIARY_PRESSED)};
            }}
            #ConversationMessage #ConversationMessageSection[section_style="code-user"] QToolButton:hover {{
                background-color: {style_manager.get_color_str(ColorRole.BACKGROUND_TERTIARY_HOVER)};
            }}
            #ConversationMessage #ConversationMessageSection[section_style="code-user"] QToolButton:pressed {{
                background-color: {style_manager.get_color_str(ColorRole.BACKGROUND_TERTIARY_PRESSED)};
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
        style_manager = self._style_manager
        zoom_factor = style_manager.zoom_factor()
        spacing = int(style_manager.message_bubble_spacing() * zoom_factor)
        self._messages_layout.setSpacing(spacing)
        self._messages_layout.setContentsMargins(spacing, spacing, spacing, spacing)

        font = self.font()
        base_font_size = style_manager.base_font_size()
        font.setPointSizeF(base_font_size * zoom_factor)
        self.setFont(font)

        stylesheet_parts = [
            self._build_widget_style(),
            self._build_conversation_message_styles(),
            self._build_conversation_message_section_styles()
        ]

        shared_stylesheet = "\n".join(stylesheet_parts)
        self.setStyleSheet(shared_stylesheet)

        for message in self._messages:
            if message.is_rendered():
                message.apply_style()

        self._input.apply_style()

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

        # If we're currently streaming, cancel the AI interaction first
        if self._is_streaming:
            self.cancel_current_tasks(False)
            self._is_streaming = False
            self._input.set_streaming(False)
            self._stop_message_border_animation()

            # Clear any pending message updates
            self._pending_messages.clear()
            if self._update_timer.isActive():
                self._update_timer.stop()

            self.status_updated.emit()

        # Update the underlying AI conversation history
        ai_conversation = cast(AIConversation, self._ai_conversation)
        history = ai_conversation.get_conversation_history()

        # Get all messages from history
        all_messages = history.get_messages()

        assert all_messages[index].source == AIMessageSource.USER, "Only user messages can be deleted."
        prompt = all_messages[index].content

        # Keep only the messages up to the specified index
        preserved_history_messages = all_messages[:index]

        # Update the AI conversation history
        ai_conversation.load_message_history(preserved_history_messages)

        # Store all messages up to but not including the specified index
        preserved_messages = self._messages[:index]

        # Remove message widgets from the layout and delete them
        for i in range(len(self._messages) - 1, index - 1, -1):
            message_widget = self._messages[i]
            if self._message_with_selection == message_widget:
                self._message_with_selection = None

            self._messages_layout.removeWidget(message_widget)
            message_widget.deleteLater()

        # Update the _messages list to only include preserved messages
        self._messages = preserved_messages

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

            # Put the spotlight back to the input
            self._spotlighted_message_index = -1
            self._input.set_content(prompt)
            self._input.set_spotlighted(True)
            self._input.setFocus()

            # Scroll to bottom
            self._auto_scroll = True
            self._scroll_to_bottom()

        except AIConversationTranscriptError as e:
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
        return has_text

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
        requester: str | None = None
    ) -> None:
        """Submit current input text."""
        content = self._input.to_plain_text().strip()
        if not content:
            return

        ai_conversation = cast(AIConversation, self._ai_conversation)
        sanitized_content = self._sanitize_input(content)
        self._input.clear()

        # We need to decide if we're already streaming or if this is a new message.
        if self._is_streaming:
            # We're streaming, so auto-reject any pending tool approval
            if self._pending_tool_call_approval:
                self._pending_tool_call_approval.remove_tool_approval_ui()
                self._pending_tool_call_approval = None

                loop = asyncio.get_event_loop()
                loop.create_task(ai_conversation.reject_pending_tool_calls("User interrupted with new message"))

        else:
            # We're not streaming, so mark that we are now
            self._input.set_streaming(True)
            self._is_streaming = True
            self.status_updated.emit()

            # Remember the last submitted message in case we need to restore it after an error or user cancellation
            self._last_submitted_message = content

            # Scroll to the bottom and restore auto-scrolling
            self._auto_scroll = True
            self._scroll_to_bottom()

        # Submit the message to the AIConversation instance
        loop = asyncio.get_event_loop()
        if not loop.is_running():
            return

        loop.create_task(ai_conversation.submit_message(requester, sanitized_content))

    def _on_stop_requested(self) -> None:
        """Handle stop request from input widget."""
        self.cancel_current_tasks()

    def _on_input_settings_requested(self) -> None:
        """Handle settings request from input widget."""
        self.conversation_settings_requested.emit()

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
            self._auto_scroll = metadata["auto_scroll"]

        if "vertical_scroll" in metadata:
            # Use a timer to ensure the scroll happens after layout is complete
            QTimer.singleShot(0, lambda: self._scroll_area.verticalScrollBar().setValue(metadata["vertical_scroll"]))

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
        """
        Request scroll to ensure the current match is visible.

        If the match is in a collapsed message, expands it first and re-searches
        to get accurate section positions after rendering.
        """
        widget, matches = self._matches[self._current_widget_index]
        section_num, start, _ = matches[self._current_match_index]

        # If this is a ConversationMessage (not input), check if it needs expansion
        if isinstance(widget, ConversationMessage) and not widget.is_expanded():
            # Expand the message first
            widget.set_expanded(True)

            # Re-search this specific message to get accurate section positions
            # after rendering (the raw text positions won't map correctly to sections)
            search_text = self._last_search
            new_matches = widget.find_text(search_text)

            # Update the matches for this widget
            self._matches[self._current_widget_index] = (widget, new_matches)
            matches = new_matches

            # Re-highlight all matches with updated positions
            self._highlight_matches()

            # Use the same match index if possible, otherwise use first match
            if self._current_match_index >= len(new_matches):
                self._current_match_index = 0

            section_num, start, _ = new_matches[self._current_match_index]

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

    # AI Tool Support Methods

    def get_conversation_info(self) -> Dict[str, Any]:
        """
        Get high-level metadata about the conversation.

        Returns:
            Dictionary containing conversation metadata
        """
        if self._ai_conversation is None:
            raise ValueError("No conversation available")

        history = self.get_conversation_history()
        messages = history.get_messages()

        if not messages:
            return {
                "message_count": 0,
                "first_message_timestamp": None,
                "last_message_timestamp": None,
                "models_used": [],
                "total_tokens": history.get_token_counts(),
                "parent": history.parent(),
                "version": history.version()
            }

        # Collect unique models
        models_used = list(set(msg.model for msg in messages if msg.model))

        return {
            "message_count": len(messages),
            "first_message_timestamp": messages[0].timestamp.isoformat(),
            "last_message_timestamp": messages[-1].timestamp.isoformat(),
            "models_used": models_used,
            "total_tokens": history.get_token_counts(),
            "parent": history.parent(),
            "version": history.version()
        }

    def read_messages(
        self,
        start_index: int | None = None,
        end_index: int | None = None,
        message_types: List[str] | None = None,
        limit: int | None = None
    ) -> Dict[str, Any]:
        """
        Read messages with filtering and pagination.

        Args:
            start_index: Starting message index (0-based, inclusive)
            end_index: Ending message index (0-based, inclusive)
            message_types: List of message types to include
            limit: Maximum number of messages to return

        Returns:
            Dictionary containing messages and metadata
        """
        if self._ai_conversation is None:
            raise ValueError("No conversation available")

        # Assert message types are valid (should be validated by caller)
        if message_types is not None:
            valid_types = AIMessage.get_message_types()
            invalid_types = set(message_types) - valid_types
            assert not invalid_types, (
                f"Invalid message types: {', '.join(sorted(invalid_types))}. "
                f"Valid types: {', '.join(sorted(valid_types))}"
            )

        history = self.get_conversation_history()
        messages = history.get_messages()

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

        # Apply type filtering
        if message_types:
            filtered_messages = [
                msg for msg in filtered_messages
                if msg.source_str() in message_types
            ]

        # Apply limit
        if limit and limit > 0:
            filtered_messages = filtered_messages[:limit]

        # Convert to dictionaries
        result_messages = []
        for msg in filtered_messages:
            msg_dict = msg.to_transcript_dict()
            msg_dict['index'] = messages.index(msg)  # Original index in full conversation
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
        Get a specific message by ID or index.

        Args:
            message_id: Message UUID
            message_index: Message index (0-based)

        Returns:
            Message dictionary or None if not found
        """
        if self._ai_conversation is None:
            raise ValueError("No conversation available")

        if message_id is None and message_index is None:
            raise ValueError("Must provide either message_id or message_index")

        history = self.get_conversation_history()
        messages = history.get_messages()

        # Find by index
        if message_index is not None:
            if 0 <= message_index < len(messages):
                msg = messages[message_index]
                msg_dict = msg.to_transcript_dict()
                msg_dict['index'] = message_index
                return msg_dict
            return None

        # Find by ID
        for idx, msg in enumerate(messages):
            if msg.id == message_id:
                msg_dict = msg.to_transcript_dict()
                msg_dict['index'] = idx
                return msg_dict

        return None

    def search_messages(
        self,
        search_text: str,
        case_sensitive: bool = False,
        message_types: List[str] | None = None,
        max_results: int = 50
    ) -> Dict[str, Any]:
        """
        Search for text across all messages.

        Args:
            search_text: Text to search for
            case_sensitive: Case-sensitive search
            message_types: Filter to specific message types
            max_results: Maximum results to return

        Returns:
            Dictionary containing search results
        """
        if self._ai_conversation is None:
            raise ValueError("No conversation available")

        # Assert message types are valid (should be validated by caller)
        if message_types is not None:
            valid_types = AIMessage.get_message_types()
            invalid_types = set(message_types) - valid_types
            assert not invalid_types, (
                f"Invalid message types: {', '.join(sorted(invalid_types))}. "
                f"Valid types: {', '.join(sorted(valid_types))}"
            )

        if not search_text:
            return {
                "search_text": search_text,
                "case_sensitive": case_sensitive,
                "total_matches": 0,
                "returned_count": 0,
                "matches": []
            }

        history = self.get_conversation_history()
        messages = history.get_messages()

        # Prepare search
        search_str = search_text if case_sensitive else search_text.lower()
        matches = []

        for idx, msg in enumerate(messages):
            # Apply type filter
            if message_types:
                msg_type = msg.source_str()
                if msg_type not in message_types:
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
                    "message_id": msg.id,
                    "message_type": msg.source_str(),
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
    ) -> Dict[str, Any]:
        """
        Scroll to a specific message, substituting with nearest visible message if hidden.

        If the target message is hidden, this will search forward for the next visible
        message, then backward if no forward message exists.

        Args:
            message_id: Message UUID
            message_index: Message index (0-based)

        Returns:
            Dictionary containing:
                - success (bool): Whether scroll succeeded
                - target_index (int): Original requested index
                - actual_index (int): Index actually scrolled to
                - substituted (bool): Whether a visible message was substituted
                - message_id (str): ID of message scrolled to
                - error (str): Error message if success is False
        """
        if message_id is None and message_index is None:
            return {
                "success": False,
                "error": "Must provide either message_id or message_index"
            }

        # Store original message_id for result
        original_message_id = message_id
        target_index = message_index

        # Find message index if ID provided
        if message_id is not None:
            history = self.get_conversation_history()
            messages = history.get_messages()
            message_index = None
            for idx, msg in enumerate(messages):
                if msg.id == message_id:
                    message_index = idx
                    target_index = idx
                    break

            if message_index is None:
                return {
                    "success": False,
                    "error": f"Message not found with ID: {original_message_id}"
                }

        # Validate index
        if message_index is None or message_index < 0 or message_index >= len(self._messages):
            return {
                "success": False,
                "error": f"Invalid message index: {message_index}"
            }

        # Check if target message is visible
        target_message = self._messages[message_index]
        actual_index = message_index
        substituted = False

        if not target_message.is_rendered():
            # Target is hidden, find next visible message
            next_visible = self._find_next_visible_message(message_index)

            if next_visible != -1:
                # Found visible message after target
                actual_index = next_visible
                substituted = True

            else:
                # No visible message after, search backward
                prev_visible = self._find_previous_visible_message(message_index)

                if prev_visible != -1:
                    # Found visible message before target
                    actual_index = prev_visible
                    substituted = True

                else:
                    # No visible messages at all
                    return {
                        "success": False,
                        "error": "No visible messages in conversation"
                    }

        # Get the message widget to scroll to
        message_widget = self._messages[actual_index]

        # Scroll so message header is at top of viewport with spacing
        bubble_spacing = self._style_manager.message_bubble_spacing()
        self._perform_scroll_to_position(message_widget, int(bubble_spacing))

        # Get the actual message ID
        history = self.get_conversation_history()
        messages = history.get_messages()
        actual_message_id = messages[actual_index].id

        return {
            "success": True,
            "target_index": target_index,
            "actual_index": actual_index,
            "substituted": substituted,
            "message_id": actual_message_id
        }
