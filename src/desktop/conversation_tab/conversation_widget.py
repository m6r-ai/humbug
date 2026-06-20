"""Conversation widget implementation."""

import asyncio
import logging
import os
import re
import time
from typing import Callable, cast, Dict, List, Tuple, Any, Set

from PySide6.QtWidgets import (
    QWidget, QApplication, QVBoxLayout, QScrollArea, QSizePolicy, QFileDialog
)
from PySide6.QtCore import QTimer, QPoint, Qt, Signal, QObject, QEvent, QSize
from PySide6.QtGui import QCursor, QFont, QGuiApplication, QIcon, QResizeEvent

from ai import (
    AIConversationEvent, AIConversationHistory, AIConversationSettings,
    AIReasoningCapability, AIMessage, AIMessageSource
)
from ai_tool import AIToolCall
from ai_transcript_conversation import AITranscriptConversation
from docx import DocxError, DocxUnsupportedError, extract_text as extract_docx_text
from pdf import PDFUnsupportedError, PDFError, extract_text, parse as parse_pdf
from syntax.programming_language_utils import ProgrammingLanguageUtils

from desktop.color_role import ColorRole
from desktop.conversation_tab.conversation_error import ConversationError
from desktop.conversation_tab.conversation_input import ConversationInput
from desktop.conversation_tab.conversation_message import ConversationMessage
from desktop.conversation_tab.conversation_message_style import ConversationMessageStyle
from desktop.language.language_manager import LanguageManager
from desktop.message_box import MessageBox, MessageBoxType, MessageBoxButton
from desktop.mindspace.mindspace_manager import MindspaceManager
from desktop.style_manager import StyleManager
from desktop.widgets import SMOOTH_SCROLL_DURATION_MS, SMOOTH_SCROLL_INTERVAL_MS
from desktop.markdown import MarkdownTextEdit
from mindspace.mindspace_log_level import MindspaceLogLevel


class ConversationWidget(QWidget):
    """Widget for displaying conversation with message history and input."""

    # Signal to notify tab of status changes
    status_updated = Signal()

    # Signals for tab to handle forking a conversation
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
        ai_transcript_conversation: AITranscriptConversation | None = None
    ) -> None:
        """
        Initialize the conversation widget.

        Args:
            path: Full path to transcript file
            parent: Optional parent widget
            ai_transcript_conversation: An existing AITranscriptConversation to adopt,
                or None to create a new one
        """
        super().__init__(parent)
        self._logger = logging.getLogger("ConversationWidget")

        self.setObjectName("ConversationWidget")

        self._mindspace_manager = MindspaceManager()

        style_manager = StyleManager()
        self._style_manager = style_manager
        self._message_style = self._build_message_style()

        if ai_transcript_conversation is not None:
            self._ai_conversation = ai_transcript_conversation

        else:
            self._ai_conversation = AITranscriptConversation(path)

        # Register UI callbacks on the inner AIConversation
        self._register_ai_conversation_callbacks()

        self._last_submitted_message: str = ""

        # We need to track any unfinished message because it won't appear in the transcript until
        # it completes.  If we move a conversation to a new tab, we need to ensure it doesn't get lost.
        self._current_unfinished_message: AIMessage | None = None

        self._response_reveal_timer = QTimer(self)
        self._response_reveal_timer.setInterval(24)
        self._response_reveal_timer.timeout.connect(self._advance_response_reveal)
        self._response_reveal_targets: Dict[str, str] = {}
        self._response_reveal_rendered: Dict[str, str] = {}
        self._response_reveal_widgets: Dict[str, ConversationMessage] = {}
        self._response_reveal_completed: Set[str] = set()
        self._response_reveal_last_render: Dict[str, float] = {}

        # Widget tracking
        self._messages: List[ConversationMessage] = []
        self._message_with_selection: ConversationMessage | None = None
        self._is_streaming = False

        # Batched message loading state
        self._load_queue: List[AIMessage] = []
        self._load_pending_metadata: Dict[str, Any] | None = None
        self._load_scroll_offset: int | None = None
        self._load_batch_size: int = 2
        self._load_tail_size: int = 80  # Weirdly we might get 80 message in view!
        self._load_generation: int = 0
        self._load_head_insert_pos: int = 0

        self._load_batch_timer = QTimer(self)
        self._load_batch_timer.setSingleShot(True)
        self._load_batch_timer.setInterval(0)
        self._load_batch_timer_slot: Callable[..., Any] | None = None

        self._deferred_scroll_timer = QTimer(self)
        self._deferred_scroll_timer.setSingleShot(True)
        self._deferred_scroll_timer.setInterval(0)
        self._deferred_scroll_timer_slot: Callable[..., Any] | None = None

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
        self._slow_timer = QTimer(self)
        self._slow_timer.setInterval(self._slow_interval_ms)
        self._slow_timer.timeout.connect(self._on_slow_timer)

        # Pending message flag and counter for smooth transition
        self._pending_animation_message = False
        self._no_message_counter = 0
        self._max_no_message_cycles = 16  # Number of cycles before disabling debounce timer

        # Debounce timer for message notifications
        self._debounce_timer = QTimer(self)
        self._debounce_timer.setSingleShot(True)
        self._debounce_timer.timeout.connect(self._on_debounce_timeout)
        self._debounce_timer.setInterval(self._debounce_interval_ms)

        # Initialize tracking variables
        self._auto_scroll = True
        self._input_spacer: QWidget | None = None
        self._sticky_update_pending = False

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
        self._scroll_area.setFocusPolicy(Qt.FocusPolicy.NoFocus)
        self._scroll_area.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)
        self._scroll_area.setAlignment(Qt.AlignmentFlag.AlignHCenter | Qt.AlignmentFlag.AlignTop)
        self._scroll_area.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOn)
        self._scroll_area.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)

        # Connect to the vertical scrollbar's change signals
        self._scroll_area.verticalScrollBar().valueChanged.connect(self._on_scroll_value_changed)
        self._scroll_area.verticalScrollBar().rangeChanged.connect(self._on_scroll_range_changed)

        # Create messages container widget
        self._messages_container = QWidget()
        self._messages_container.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)
        self._messages_layout = QVBoxLayout(self._messages_container)
        self._messages_container.setLayout(self._messages_layout)

        # Set up the input box as a floating overlay, parented to this widget
        self._input = ConversationInput(AIMessageSource.USER, self)
        self._input.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)
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
        self._input.attach_requested.connect(self._on_attach_requested)
        self._input.modified.connect(self.conversation_modified)

        self.apply_style()

        # Invisible spacer that reserves the same height as the floating input
        # at the bottom of the scroll area so scrolling to the end doesn't hide
        # content behind the input overlay.
        self._input_spacer = QWidget(self._messages_container)
        self._input_spacer.setStyleSheet("background: transparent;")
        self._input_spacer.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)

        self._messages_layout.addStretch()
        self._messages_layout.addWidget(self._input_spacer)

        self._scroll_area.setWidget(self._messages_container)

        # Add the scroll area to the main layout
        conversation_layout.addWidget(self._scroll_area)

        input_text_area = cast(MarkdownTextEdit, self._input._text_area)
        input_text_area.size_hint_changed.connect(self._on_input_size_hint_changed)
        input_text_area.set_allow_vertical_scroll(True)
        self._input.chrome_height_changed.connect(self._on_input_size_hint_changed)
        self._scroll_area.viewport().installEventFilter(self)
        self._messages_container.installEventFilter(self)
        self._input.raise_()

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
        self._smooth_scroll_timer.setInterval(SMOOTH_SCROLL_INTERVAL_MS)
        self._smooth_scroll_timer.timeout.connect(self._update_smooth_scroll)
        self._smooth_scroll_target: int = 0
        self._smooth_scroll_start: int = 0
        self._smooth_scroll_distance: int = 0
        self._smooth_scroll_duration: int = SMOOTH_SCROLL_DURATION_MS
        self._smooth_scroll_time: int = 0

        # Setup context menu
        self.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.customContextMenuRequested.connect(self._show_conversation_context_menu)

        # Find functionality
        self._matches: List[Tuple[ConversationMessage, List[Tuple[int, int, int]]]] = []
        self._current_widget_index = -1
        self._current_match_index = -1
        self._last_search: tuple = ("", False, False)
        self._highlighted_widgets: Set[ConversationMessage] = set()

        try:
            conversation_history = self._ai_conversation.read()

        except Exception as e:
            raise ConversationError(f"Failed to read conversation transcript: {str(e)}") from e

        self._load_message_history(
            conversation_history.get_messages(), ai_transcript_conversation is not None,
            attachments=conversation_history.attachments()
        )

        # Restore parent metadata from the transcript onto the ai_conversation history.
        # load_message_history calls clear() which discards it, so we reapply it here.
        transcript_parent = conversation_history.parent()
        if transcript_parent is not None:
            self._ai_conversation.get_conversation_history().set_parent(transcript_parent)

        self._is_delegated_conversation = os.path.basename(self._ai_conversation.path()).startswith("dAI-")

        # Any active tool approval
        self._pending_tool_call_approval: ConversationMessage | None = None

        # The last SYSTEM error message widget that has a retry button showing
        self._last_error_message_widget: ConversationMessage | None = None

    def _activate_widget(self, widget: QWidget) -> None:
        """
        Handle activation of a widget, spotlighting the associated message.

        Args:
            widget: The widget that was activated
        """
        # If the content is short enough that no scrolling is needed, the last message
        # is already fully visible, so treat activation as "seen".
        if self._scroll_area.verticalScrollBar().maximum() == 0:
            self.has_seen_latest_update_changed.emit(True)

        # Find the ConversationMessage that contains this widget
        message_widget = self._find_conversation_message(widget)
        if message_widget is None:
            # We couldn't find a ConversationMessage parent. Only redirect focus if
            # focus is not already somewhere inside this widget - redirecting while
            # focus is inside a child (e.g. an approval button mid-click) would
            # interrupt the in-flight mouse event on Windows.
            focus_widget = QApplication.focusWidget()
            current: QObject | None = focus_widget
            while current is not None:
                if current is self:
                    return

                current = current.parent()

            if self._spotlighted_message_index != -1:
                self._messages[self._spotlighted_message_index].set_spotlighted(True)
                self._messages[self._spotlighted_message_index].setFocus()
                return

            # If our input box is hidden then spotlight the last user message.
            if self._input.isHidden():
                last_visible_index = self._find_last_visible_user_message()
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

        # Only spotlight user and user_queued messages; clicking other message
        # types clears any existing spotlight but does not set a new one.
        if message_widget in self._messages:
            source = message_widget.message_source()
            if source in (AIMessageSource.USER, AIMessageSource.USER_QUEUED):
                self._spotlighted_message_index = self._messages.index(message_widget)
                message_widget.set_spotlighted(True)
                message_widget.setFocus(Qt.FocusReason.MouseFocusReason)

            else:
                self._spotlighted_message_index = -1
                self._input.set_spotlighted(True)

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

    def _create_completion_result(self) -> Dict[str, Any]:
        """
        Create completion result for delegated conversation.

        Returns:
            Dictionary containing completion result
        """
        messages = self._ai_conversation.get_conversation_history().get_messages()

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

    def _add_message_core(
        self,
        message: AIMessage,
        layout_pos: int | None = None,
        apply_style: bool = True
    ) -> ConversationMessage:
        """
        Core of the _add_message method that avoids unnecessary UI updates.

        Args:
            message: The message to add.
            layout_pos: If given, insert the widget at this layout position and at
                the corresponding index in self._messages.  If None (default),
                append before the input widget at the end of the layout.
            apply_style: If True, pass the current message style into the widget
                so sections are styled correctly on first render.
        """
        resolved_attachments: list[tuple[str, str]] | None = None
        if message.attachments:
            history = self._ai_conversation.get_conversation_history()
            resolved = []
            for guid in message.attachments:
                attachment = history.get_attachment(guid)
                if attachment is not None:
                    resolved.append((attachment["filename"], attachment["content"]))

            if resolved:
                resolved_attachments = resolved

        msg_widget = ConversationMessage(
            message.source,
            message.timestamp,
            AIConversationSettings.get_display_name(message.model or "", message.provider or ""),
            message.id,
            message.user_name,
            message.content,
            message.tool_call_context,
            attachments=resolved_attachments,
            message_style=self._message_style if apply_style else None
        )
        msg_widget.selection_changed.connect(
            lambda has_selection: self._on_selection_changed(msg_widget, has_selection)
        )
        msg_widget.scroll_requested.connect(self._on_scroll_requested)
        msg_widget.mouse_released.connect(self._stop_scroll)
        msg_widget.fork_requested.connect(self._on_message_fork_requested)
        msg_widget.edit_confirmed.connect(self._on_message_edit_confirmed)
        msg_widget.delete_requested.connect(self._on_message_delete_requested)
        msg_widget.expand_requested.connect(self._on_message_expand_requested)
        msg_widget.tool_call_approved.connect(self._on_tool_call_approved)
        msg_widget.tool_call_i_am_unsure.connect(self._on_tool_call_i_am_unsure)
        msg_widget.tool_call_rejected.connect(self._on_tool_call_rejected)
        msg_widget.retry_requested.connect(self._on_retry_requested)

        if layout_pos is None:
            self._messages.append(msg_widget)

            # Add widget before input and the stretch.
            self._messages_layout.insertWidget(self._messages_layout.count() - 2, msg_widget)

        else:
            # Insert at the requested position in both the list and the layout.
            self._messages.insert(layout_pos, msg_widget)
            self._messages_layout.insertWidget(layout_pos, msg_widget)

        return msg_widget

    def _add_message(self, message: AIMessage) -> None:
        """
        Add a new message to the conversation view.

        Args:
            message: The message that was added
        """
        self._hide_last_ai_connected_message()
        self._remove_last_error_retry_ui()

        # If this is a USER message, delete any previous USER_QUEUED messages
        if message.source == AIMessageSource.USER:
            self._delete_user_queued_messages()

        msg_widget = self._add_message_core(message)

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
            self._remove_response_reveal(message_widget)
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

    def _unregister_ai_conversation_callbacks(self) -> None:
        """Unregister all UI callbacks from the inner AIConversation."""
        self._ai_conversation.unregister_callback(
            AIConversationEvent.ERROR, self._on_request_error
        )
        self._ai_conversation.unregister_callback(
            AIConversationEvent.MESSAGE_ADDED, self._on_message_added
        )
        self._ai_conversation.unregister_callback(
            AIConversationEvent.MESSAGE_UPDATED, self._on_message_updated
        )
        self._ai_conversation.unregister_callback(
            AIConversationEvent.MESSAGE_COMPLETED, self._on_message_completed
        )
        self._ai_conversation.unregister_callback(
            AIConversationEvent.MESSAGE_ADDED_AND_COMPLETED, self._on_message_added_and_completed
        )
        self._ai_conversation.unregister_callback(
            AIConversationEvent.COMPLETED, self._on_request_completed
        )
        self._ai_conversation.unregister_callback(
            AIConversationEvent.TOOL_APPROVAL_REQUIRED, self._on_tool_approval_required
        )
        self._ai_conversation.unregister_callback(
            AIConversationEvent.STREAMING_UPDATE, self._on_streaming_update
        )
        self._ai_conversation.unregister_callback(
            AIConversationEvent.AI_CONNECTED, self._on_ai_connected
        )

    def _register_ai_conversation_callbacks(self) -> None:
        """Register UI callbacks on the inner AIConversation."""
        self._ai_conversation.register_callback(
            AIConversationEvent.ERROR, self._on_request_error
        )
        self._ai_conversation.register_callback(
            AIConversationEvent.MESSAGE_ADDED, self._on_message_added
        )
        self._ai_conversation.register_callback(
            AIConversationEvent.MESSAGE_UPDATED, self._on_message_updated
        )
        self._ai_conversation.register_callback(
            AIConversationEvent.MESSAGE_COMPLETED, self._on_message_completed
        )
        self._ai_conversation.register_callback(
            AIConversationEvent.MESSAGE_ADDED_AND_COMPLETED, self._on_message_added_and_completed
        )
        self._ai_conversation.register_callback(
            AIConversationEvent.COMPLETED, self._on_request_completed
        )
        self._ai_conversation.register_callback(
            AIConversationEvent.TOOL_APPROVAL_REQUIRED, self._on_tool_approval_required
        )
        self._ai_conversation.register_callback(
            AIConversationEvent.STREAMING_UPDATE, self._on_streaming_update
        )
        self._ai_conversation.register_callback(
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

            # Show retry button on the error message widget
            if self._messages:
                self._last_error_message_widget = self._messages[-1]
                self._last_error_message_widget.show_retry_ui()

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
        if self._pending_tool_call_approval and self._mindspace_manager.has_mindspace():
            info = self._pending_tool_call_approval.get_tool_approval_info()
            if info and info.get("tool_call"):
                tc = info["tool_call"]
                op = tc.arguments.get("operation", "")
                label = f"{tc.name}.{op}" if op else tc.name
                self._mindspace_manager.add_interaction(
                    MindspaceLogLevel.INFO,
                    f"Human approved tool call: {label}"
                )

        self._pending_tool_call_approval = None
        loop = asyncio.get_event_loop()
        loop.create_task(self._ai_conversation.approve_pending_tool_calls())

    def _on_tool_call_i_am_unsure(self) -> None:
        """Handle user indicating uncertainty about tool calls."""
        # Clean up the tool approval UI
        if self._pending_tool_call_approval:
            if self._mindspace_manager.has_mindspace():
                info = self._pending_tool_call_approval.get_tool_approval_info()
                if info and info.get("tool_call"):
                    tc = info["tool_call"]
                    op = tc.arguments.get("operation", "")
                    label = f"{tc.name}.{op}" if op else tc.name
                    self._mindspace_manager.add_interaction(
                        MindspaceLogLevel.INFO,
                        f"Human indicated uncertainty about tool call: {label}"
                    )
            self._pending_tool_call_approval.remove_tool_approval_ui()
            self._pending_tool_call_approval = None

        # Reject the pending tool calls with explanation
        loop = asyncio.get_event_loop()
        loop.create_task(self._ai_conversation.reject_pending_tool_calls(
            "User is unsure about this tool request and wants to discuss"
        ))

        # Submit the user's message to continue the conversation
        loop.create_task(self._ai_conversation.submit_message(
            None, "I'm not sure about this tool request. Let's discuss."))

    def _on_tool_call_rejected(self, reason: str) -> None:
        """Handle user rejection of tool calls."""
        if self._pending_tool_call_approval and self._mindspace_manager.has_mindspace():
            info = self._pending_tool_call_approval.get_tool_approval_info()
            if info and info.get("tool_call"):
                tc = info["tool_call"]
                op = tc.arguments.get("operation", "")
                label = f"{tc.name}.{op}" if op else tc.name
                self._mindspace_manager.add_interaction(
                    MindspaceLogLevel.INFO,
                    f"Human denied tool call: {label}\nreason: {reason}"
                )

        self._pending_tool_call_approval = None
        loop = asyncio.get_event_loop()
        loop.create_task(self._ai_conversation.reject_pending_tool_calls(reason))

    def _remove_last_error_retry_ui(self) -> None:
        """Remove the retry button from the last error message widget, if present."""
        if self._last_error_message_widget:
            self._last_error_message_widget.remove_retry_ui()
            self._last_error_message_widget = None

    def _on_retry_requested(self) -> None:
        """Handle the user clicking Retry on a SYSTEM error message."""
        self._last_error_message_widget = None

        # Remove all widgets from the error message back to (but not including) the last
        # USER or TOOL_RESULT widget, mirroring what retry_last_request does to the history.
        retry_sources = {AIMessageSource.USER, AIMessageSource.TOOL_RESULT}
        while self._messages and self._messages[-1].message_source() not in retry_sources:
            widget = self._messages.pop()
            if self._message_with_selection == widget:
                self._message_with_selection = None

            if self._animated_message == widget:
                self._animated_message = None

            self._remove_response_reveal(widget)
            self._messages_layout.removeWidget(widget)
            widget.deleteLater()

        # If the spotlight index now points beyond the end of the list, reset it
        if self._spotlighted_message_index >= len(self._messages):
            self._spotlighted_message_index = -1

        # Clear the input box - the text was restored there by _on_request_error so the
        # user could edit and resubmit manually, but retry resubmits directly from history.
        self._input.clear()

        # Re-enter streaming state so the UI reflects that a request is in flight
        self._is_streaming = True
        self._input.set_streaming(True)
        self.status_updated.emit()

        # Start animating immediately so there's visual feedback during the connection wait
        if not self._is_animating:
            self._start_message_border_animation()

        loop = asyncio.get_event_loop()
        loop.create_task(self._ai_conversation.retry_last_request())

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

        display_message = message
        should_reveal_from_empty = message.source in (AIMessageSource.AI, AIMessageSource.REASONING) and bool(message.content)
        if should_reveal_from_empty:
            display_message = message.copy()
            display_message.content = ""

        self._add_message(display_message)

        if should_reveal_from_empty:
            self._update_last_message(message)

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

        for i in range(len(self._messages) - 1, -1, -1):
            if self._messages[i].message_id() == message.id:
                self._queue_response_reveal(self._messages[i], message, completed=message.completed)
                break

        if self._auto_scroll:
            self._scroll_to_bottom()

    def _queue_response_reveal(self, widget: ConversationMessage, message: AIMessage, completed: bool = False) -> None:
        """Queue streamed response text for smooth incremental rendering."""
        message_id = message.id
        target = message.content
        rendered = self._response_reveal_rendered.get(message_id)

        if rendered is None or not target.startswith(rendered):
            rendered = ""
            self._response_reveal_rendered[message_id] = rendered
            if target:
                widget.set_content("")

        self._response_reveal_targets[message_id] = target
        self._response_reveal_widgets[message_id] = widget

        if completed:
            self._response_reveal_completed.add(message_id)

        if not self._response_reveal_timer.isActive():
            self._response_reveal_timer.start()

    def _advance_response_reveal(self) -> None:
        """Reveal queued response text in small chunks for smoother streaming."""
        if not self._response_reveal_targets:
            self._response_reveal_timer.stop()
            return

        caught_up_ids: list[str] = []
        final_render_ids: list[str] = []
        did_render = False
        newly_visible_widget: ConversationMessage | None = None
        rendered_widgets: list[ConversationMessage] = []
        for message_id, target in list(self._response_reveal_targets.items()):
            widget = self._response_reveal_widgets.get(message_id)
            if widget is None:
                caught_up_ids.append(message_id)
                continue

            rendered = self._response_reveal_rendered.get(message_id, "")
            if rendered == target:
                if message_id in self._response_reveal_completed:
                    final_render_ids.append(message_id)

                caught_up_ids.append(message_id)
                continue

            # When scrolled away, throttle renders to ~500ms intervals to reduce
            # layout churn, but always render immediately when the stream completes.
            if not self._auto_scroll and message_id not in self._response_reveal_completed:
                last_render = self._response_reveal_last_render.get(message_id, 0.0)
                if time.monotonic() - last_render < 0.5:
                    continue

            remaining = len(target) - len(rendered)
            if remaining <= 0 or not target.startswith(rendered) or not self._auto_scroll:
                next_text = target

            else:
                chunk_size = self._response_reveal_chunk_size(remaining, message_id in self._response_reveal_completed)
                next_text = target[:len(rendered) + chunk_size]

                # Snap the cut point back to the nearest word boundary so we
                # never expose a lone block marker like "# " or "* " at the
                # trailing edge of the revealed text.
                if next_text != target:
                    new_portion = next_text[len(rendered):]
                    last_break = max(new_portion.rfind('\n'), new_portion.rfind(' '))
                    if last_break > 0:
                        next_text = rendered + new_portion[:last_break + 1]

            widget.set_content(next_text)
            if not self._auto_scroll:
                rendered_widgets.append(widget)

            self._response_reveal_last_render[message_id] = time.monotonic()
            if next_text and not rendered and widget.is_rendered():
                newly_visible_widget = widget

            self._response_reveal_rendered[message_id] = next_text
            did_render = True

            if next_text == target and message_id in self._response_reveal_completed:
                final_render_ids.append(message_id)
                caught_up_ids.append(message_id)

        for message_id in final_render_ids:
            widget = self._response_reveal_widgets.get(message_id)
            final_target = self._response_reveal_targets.get(message_id)
            if widget is not None and final_target is not None:
                if not self._auto_scroll and widget not in rendered_widgets:
                    rendered_widgets.append(widget)
                    widget.setUpdatesEnabled(False)

                widget.set_content(final_target)

        for message_id in caught_up_ids:
            self._response_reveal_targets.pop(message_id, None)
            if message_id in self._response_reveal_completed:
                self._response_reveal_rendered.pop(message_id, None)
                self._response_reveal_widgets.pop(message_id, None)
                self._response_reveal_completed.discard(message_id)
                self._response_reveal_last_render.pop(message_id, None)

        if did_render and self._auto_scroll:
            self._scroll_to_bottom()

        if newly_visible_widget is not None and self._is_animating and newly_visible_widget != self._animated_message:
            self._transfer_animation_to_message(newly_visible_widget)

        if did_render:
            self.trigger_message_animation()

        if not self._response_reveal_targets:
            self._response_reveal_timer.stop()

        if rendered_widgets:
            def _re_enable_updates(widgets: list) -> None:
                for w in widgets:
                    w.setUpdatesEnabled(True)

            QTimer.singleShot(5, lambda: _re_enable_updates(rendered_widgets))

    def _response_reveal_chunk_size(self, remaining: int, completed: bool) -> int:
        """Choose a reveal chunk size that stays smooth but catches up quickly."""
        if completed:
            return min(200, max(20, remaining // 5))

        return min(20, max(2, remaining // 18))

    def _remove_response_reveal(self, widget: ConversationMessage) -> None:
        """Remove pending reveal state for a widget that is leaving the layout."""
        message_id = widget.message_id()
        if message_id is None:
            return

        self._response_reveal_targets.pop(message_id, None)
        self._response_reveal_rendered.pop(message_id, None)
        self._response_reveal_widgets.pop(message_id, None)
        self._response_reveal_completed.discard(message_id)
        self._response_reveal_last_render.pop(message_id, None)

        if not self._response_reveal_targets:
            self._response_reveal_timer.stop()

    async def _on_message_updated(self, message: AIMessage) -> None:
        """
        Handle a message being updated with throttling.

        Updates are passed directly to the reveal system which handles rate-limiting.

        Args:
            message: The message that was updated
        """
        self._update_last_message(message)

    async def _on_message_completed(self, message: AIMessage) -> None:
        """
        Handle a message being completed.

        This processes any pending updates for this message and immediately
        updates with the completed message.

        Args:
            message: The message that was completed
        """
        self._current_unfinished_message = None

        # Update with the completed message immediately
        self._update_last_message(message)
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
                screen = QGuiApplication.screenAt(QCursor.pos())
                if screen is None or QCursor.pos().y() <= screen.availableGeometry().top() + 4:
                    distance_out = 250

                scroll_amount = min(50, max(10, distance_out // 5))
                new_val = max(scrollbar.minimum(), current_val - scroll_amount)
                scrollbar.setValue(new_val)

        elif self._last_mouse_pos.y() > viewport_height:
            # Below viewport
            distance_out = self._last_mouse_pos.y() - viewport_height
            if distance_out > viewport_height * 2:
                scrollbar.setValue(scrollbar.maximum())

            else:
                screen = QGuiApplication.screenAt(QCursor.pos())
                if screen is None or QCursor.pos().y() >= screen.availableGeometry().bottom() - 4:
                    distance_out = 250

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
        progress = min(1.0, self._smooth_scroll_time / self._smooth_scroll_duration)
        t = 1 - (1 - progress) ** 3

        # Add 0.5 lines of bias so that int() truncation crosses each pixel boundary
        # slightly early, avoiding a visible "jump" at the very end of the animation
        # where the easing curve decelerates so slowly that the final line is only
        # reached on the last tick, well after the scroll appears to have stopped.
        new_position = min(
            self._smooth_scroll_target,
            self._smooth_scroll_start + int(self._smooth_scroll_distance * t + 0.5),
        ) if self._smooth_scroll_distance > 0 else max(
            self._smooth_scroll_target,
            self._smooth_scroll_start + int(self._smooth_scroll_distance * t - 0.5),
        )
        scrollbar = self._scroll_area.verticalScrollBar()
        scrollbar.setValue(new_position)
        if progress >= 1.0 or new_position == self._smooth_scroll_target:
            self._smooth_scroll_timer.stop()

    def update_conversation_settings(self, new_settings: AIConversationSettings) -> None:
        """Update conversation settings and associated backend."""
        self._ai_conversation.update_conversation_settings(new_settings)
        self.status_updated.emit()
        self._input.set_model(AIConversationSettings.get_display_name(new_settings.model, new_settings.provider))

    def ai_conversation(self) -> AITranscriptConversation:
        """
        Get the AIConversation instance.

        Returns:
            The AIConversation instance
        """
        return self._ai_conversation

    def conversation_settings(self) -> AIConversationSettings:
        """
        Get current conversation settings.

        Returns:
            Current conversation settings
        """
        return self._ai_conversation.conversation_settings()

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

        self._update_sticky_banners()

    def _on_scroll_range_changed(self, _minimum: int, _maximum: int) -> None:
        """Handle the scroll range changing."""
        if self._auto_scroll:
            self._scroll_to_bottom()

        self._update_sticky_banners()

    def _schedule_sticky_update(self) -> None:
        """
        Coalesce a sticky-banner recompute onto the next event-loop turn.

        Used for resize-driven updates: the message widgets are re-laid-out via posted
        layout events, so recomputing immediately would read stale geometry. Deferring
        lets the layout settle first, giving precise final banner positions.
        """
        if self._sticky_update_pending:
            return

        self._sticky_update_pending = True
        QTimer.singleShot(0, self._run_deferred_sticky_update)

    def _run_deferred_sticky_update(self) -> None:
        """Run a coalesced sticky-banner recompute scheduled by _schedule_sticky_update."""
        self._sticky_update_pending = False
        self._update_sticky_banners()

    def _update_sticky_banners(self) -> None:
        """
        Keep each message's banner pinned to the top of the viewport while the message
        body scrolls beneath it. The banner returns to its natural position once the
        message top is back within view, and rides off the top once the message has
        almost fully scrolled past.
        """
        if not self._messages:
            return

        viewport = self._scroll_area.viewport()
        viewport_height = viewport.height()
        for message in self._messages:
            if not message.is_rendered():
                continue

            # The viewport top edge, expressed in the message's local coordinates.
            top_in_viewport = message.mapTo(viewport, QPoint(0, 0)).y()

            # Skip messages that lie entirely below the viewport: their banners are
            # already at their natural position and nothing is scrolling past them yet.
            if top_in_viewport >= viewport_height:
                continue

            # Messages entirely above the viewport keep their banner parked off-screen
            # from the last update, so there's no need to reposition them either.
            if top_in_viewport + message.height() <= 0:
                continue

            message.update_sticky_banner(-top_in_viewport)

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

    def _is_user_message(self, index: int) -> bool:
        """
        Return True if the message at index has a user or user_queued source.

        Args:
            index: Index into self._messages

        Returns:
            True if the message is a user or user_queued message
        """
        source = self._messages[index].message_source()
        return source in (AIMessageSource.USER, AIMessageSource.USER_QUEUED)

    def _find_next_visible_user_message(self, start_index: int) -> int:
        """
        Find the next visible user/user_queued message starting from start_index + 1.

        Args:
            start_index: Index to start searching from (exclusive)

        Returns:
            Index of next visible user message, or -1 if none found
        """
        for i in range(start_index + 1, len(self._messages)):
            if self._messages[i].is_rendered() and self._is_user_message(i):
                return i

        return -1

    def _find_previous_visible_user_message(self, start_index: int) -> int:
        """
        Find the previous visible user/user_queued message starting from start_index - 1.

        Args:
            start_index: Index to start searching from (exclusive)

        Returns:
            Index of previous visible user message, or -1 if none found
        """
        for i in range(start_index - 1, -1, -1):
            if self._messages[i].is_rendered() and self._is_user_message(i):
                return i

        return -1

    def _find_last_visible_user_message(self) -> int:
        """
        Find the index of the last visible user/user_queued message.

        Returns:
            Index of last visible user message, or -1 if none found
        """
        for i in range(len(self._messages) - 1, -1, -1):
            if self._messages[i].is_rendered() and self._is_user_message(i):
                return i

        return -1

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

        # Find the next visible user message
        next_visible_index = self._find_next_visible_user_message(self._spotlighted_message_index)
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
            last_visible_index = self._find_last_visible_user_message()
            if last_visible_index != -1:
                self._input.set_spotlighted(False)
                self._spotlighted_message_index = last_visible_index
                self._spotlight_message()
                return True

            return False

        # Find the previous visible user message
        prev_visible_index = self._find_previous_visible_user_message(self._spotlighted_message_index)
        if prev_visible_index != -1:
            # Move to previous visible message
            self._messages[self._spotlighted_message_index].set_spotlighted(False)
            self._spotlighted_message_index = prev_visible_index
            self._spotlight_message()
            return True

        return False

    def _spotlight_message(self) -> None:
        """Spotlight the specified message, or the input box if index is -1."""
        index = self._spotlighted_message_index
        if 0 <= index < len(self._messages):
            self._messages[index].set_spotlighted(True)
            self._messages[index].setFocus()

            # Always position the spotlighted message at the top of the viewport
            # so it is never obscured by the floating input box.
            message_spacing = int(self._style_manager.message_bubble_spacing())
            self._perform_scroll_to_position(self._messages[index], message_spacing)
            return

        self._input.set_spotlighted(True)
        self._input.setFocus()

        # Scroll unconditionally to the end of the conversation so the full
        # context before the input box is visible.
        scrollbar = self._scroll_area.verticalScrollBar()
        self._start_smooth_scroll(scrollbar.maximum())

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
        """Check if navigation to next visible message is possible."""
        # If input is spotlighted, can't navigate further forward
        if self._spotlighted_message_index == -1:
            return False

        # If on a message, check if there are visible messages after current position
        # or if input is visible (can move to input)
        next_visible_index = self._find_next_visible_user_message(self._spotlighted_message_index)
        return next_visible_index != -1 or self._input.isVisible()

    def can_navigate_previous_message(self) -> bool:
        """Check if navigation to previous visible message is possible."""
        # If input is spotlighted, check if there are any visible messages to go back to
        if self._spotlighted_message_index == -1:
            return self._find_last_visible_user_message() != -1

        # If on a message, check if there are visible messages before current position
        return self._find_previous_visible_user_message(self._spotlighted_message_index) != -1

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
        return self._ai_conversation.path()

    def set_path(self, new_path: str) -> None:
        """
        Set the conversation file path.

        Args:
            new_path: New path for the conversation file
        """
        self._ai_conversation.set_path(new_path)

    def _on_page_key_scroll_requested(self) -> None:
        """
        Handle page up/down scroll requests.
        """

    def eventFilter(self, obj: QObject, event: QEvent) -> bool:
        """Reposition the floating input when the viewport or messages container is resized."""
        if event.type() == QEvent.Type.Resize and obj in (
            self._scroll_area.viewport(), self._messages_container
        ):
            self._on_input_size_hint_changed()
            # Defer so message relayouts settle before we read geometry; the per-message
            # banner move filter keeps things pinned (flicker-free) in the meantime.
            self._schedule_sticky_update()

        return super().eventFilter(obj, event)

    def _on_input_size_hint_changed(self) -> None:
        """Update the spacer and floating input height when the input content changes."""
        text_area = self._input.text_area()
        max_height = self._scroll_area.viewport().height() // 2
        chrome_height = self._input.chrome_height()
        uncapped_height = text_area.sizeHint().height() + chrome_height
        capped = uncapped_height > max_height
        new_height = max_height if capped else uncapped_height
        text_area.set_height_cap(max_height - chrome_height if capped else None)

        assert self._input_spacer is not None
        self._input_spacer.setFixedHeight(new_height)
        self._update_input_width()
        self._update_input_position(new_height)

    def _update_input_width(self) -> None:
        """Set the floating input's width to match the viewport content width."""
        style_manager = self._style_manager
        spacing = int(style_manager.message_bubble_spacing())
        zoom_factor = style_manager.zoom_factor()
        max_content_width = int(style_manager.nice_tab_width() * zoom_factor)
        input_width = min(self._scroll_area.viewport().width(), max_content_width) - 2 * spacing + 2
        self._input.resize(input_width, self._input.height())

    def _update_input_position(self, input_height: int | None = None) -> None:
        """Position the floating input at the bottom of the visible viewport."""
        style_manager = self._style_manager
        spacing = int(style_manager.message_bubble_spacing())
        zoom_factor = style_manager.zoom_factor()
        max_content_width = int(style_manager.nice_tab_width() * zoom_factor)
        viewport_width = self._scroll_area.viewport().width()
        container_width = min(viewport_width, max_content_width) + 2
        container_offset = (viewport_width - container_width) // 2
        input_x = container_offset + spacing

        if input_height is None:
            assert self._input_spacer is not None
            input_height = self._input_spacer.height()

        input_y = self.height() - input_height - spacing + 1

        self._input.setFixedHeight(input_height)
        self._input.move(input_x, input_y)
        self._input.raise_()

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
            self._ai_conversation.set_conversation_history(history)
            self._load_message_history(history.get_messages(), True)

        except Exception as e:
            raise ConversationError(f"Failed to set conversation history: {str(e)}") from e

    def _load_message_history(
        self, messages: List[AIMessage], reuse_ai_conversation: bool,
        attachments: Dict[str, Dict] | None = None
    ) -> None:
        """
        Load existing message history from transcript.

        Args:
            messages: List of AIMessage objects to load
            reuse_ai_conversation: True if we are reusing an existing AI conversation
            attachments: Optional attachment store to restore alongside the messages
        """
        # Cancel any in-progress batch load before starting a new one.
        self._load_queue.clear()
        self._load_generation += 1

        # Establish a baseline for conversation settings.
        if not reuse_ai_conversation:
            settings = self._mindspace_manager.settings()
            if settings is None:
                self._logger.error("Failed to load conversation settings.")
                return

            default_settings = AIConversationSettings(
                model=settings.model,
                provider=settings.provider,
                temperature=settings.temperature,
                reasoning=settings.reasoning,
                reasoning_effort=settings.reasoning_effort,
            )

            self._ai_conversation.update_conversation_settings(default_settings)
            self._ai_conversation.load_message_history(messages)
            if attachments:
                self._ai_conversation.get_conversation_history().restore_attachments(attachments)

        cs = self._ai_conversation.conversation_settings()
        self._input.set_model(AIConversationSettings.get_display_name(cs.model, cs.provider))

        # Split into a tail loaded synchronously (so the visible end of the
        # conversation appears immediately without glitching) and a head that
        # is loaded in background batches above the already-visible tail.
        tail = messages[-self._load_tail_size:]
        head = messages[:-self._load_tail_size]

        for message in tail:
            skip_style = message.source in (AIMessageSource.USER_QUEUED, AIMessageSource.AI_CONNECTED)
            message_widget = self._add_message_core(message, apply_style=not skip_style)
            if skip_style:
                message_widget.set_rendered(False)

        self._auto_scroll = True
        if self._deferred_scroll_timer_slot is not None:
            self._deferred_scroll_timer.timeout.disconnect(self._deferred_scroll_timer_slot)

        self._deferred_scroll_timer_slot = self._scroll_to_bottom
        self._deferred_scroll_timer.timeout.connect(self._deferred_scroll_timer_slot)
        self._deferred_scroll_timer.start()

        # The head messages will be prepended one batch at a time.
        self._load_head_insert_pos = 0
        self._load_queue = list(head)
        self._load_next_batch(self._load_generation)

    def _load_next_batch(self, generation: int) -> None:
        """
        Process the next batch of queued messages and schedule the following batch.

        Args:
            generation: The load generation this batch belongs to. If the current
                generation has advanced (due to close_widget or a second
                _load_message_history call), the batch exits without completing.
        """
        if generation != self._load_generation:
            return

        for _ in range(self._load_batch_size):
            if not self._load_queue:
                break

            message = self._load_queue.pop(0)
            skip_style = message.source in (AIMessageSource.USER_QUEUED, AIMessageSource.AI_CONNECTED)
            message_widget = self._add_message_core(message, self._load_head_insert_pos, apply_style=not skip_style)
            message_widget.set_rendered(False)

            self._load_head_insert_pos += 1

        if self._load_batch_timer_slot is not None:
            self._load_batch_timer.timeout.disconnect(self._load_batch_timer_slot)

        if self._load_queue:
            self._load_batch_timer_slot = lambda: self._load_next_batch(generation)
            self._load_batch_timer.timeout.connect(self._load_batch_timer_slot)
            self._load_batch_timer.setInterval(0)
            self._load_batch_timer.start()
            return

        self._on_load_complete()

    def _on_load_complete(self) -> None:
        """Finalise a completed batch load."""
        # Run through all the messages and ensure any that should be visible are now visible.
        for message in self._messages:
            if message.message_source() not in (AIMessageSource.USER_QUEUED, AIMessageSource.AI_CONNECTED):
                message.set_rendered(True)

        # If the last visible message is a SYSTEM error, restore the retry button.
        if self._messages:
            last_widget = self._messages[-1]
            last_msg_source = last_widget.message_source()
            if last_msg_source == AIMessageSource.SYSTEM:
                history = self.get_conversation_history()
                msgs = history.get_messages()
                if msgs and msgs[-1].error:
                    self._last_error_message_widget = last_widget
                    last_widget.show_retry_ui()

        self.status_updated.emit()

        pending = self._load_pending_metadata
        self._load_pending_metadata = None

        if not self._auto_scroll:
            # The user scrolled during loading — record the current offset from
            # the bottom so _on_scroll_range_changed can restore it once the
            # range has expanded to accommodate the revealed head widgets.
            vbar = self._scroll_area.verticalScrollBar()
            self._load_scroll_offset = vbar.maximum() - vbar.value()

            # Restore non-scroll metadata fields so they don't get lost.
            if pending is not None:
                pending.pop("auto_scroll", None)
                pending.pop("vertical_scroll", None)
                pending.pop("scroll_maximum", None)
                self.restore_from_metadata(pending)

        elif pending is not None:
            # User hasn't scrolled — check whether the saved position was
            # non-bottom, and if so convert it to an offset for range-change
            # restoration; otherwise let auto-scroll keep us at the bottom.
            saved_auto_scroll = pending.get("auto_scroll", True)
            if not saved_auto_scroll:
                saved_scroll = pending.get("vertical_scroll", 0)
                saved_maximum = pending.get("scroll_maximum", saved_scroll)
                self._load_scroll_offset = saved_maximum - saved_scroll

            # Restore all non-scroll metadata fields.
            pending.pop("auto_scroll", None)
            pending.pop("vertical_scroll", None)
            pending.pop("scroll_maximum", None)
            self.restore_from_metadata(pending)

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
            path = self._ai_conversation.path()
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
        # Advance the generation so any pending timer callbacks from a batch load
        # recognise they have been superseded and exit without touching the widget.
        self._load_generation += 1
        self._load_pending_metadata = None

        # If this is a delegated conversation, we need to ensure we notify the parent
        if self._is_delegated_conversation and self._is_streaming:
            result = self._create_completion_result()
            self.submit_finished.emit(result)

        self._unregister_ai_conversation_callbacks()
        self._ai_conversation.close()
        self._stop_message_border_animation()
        self._delete_empty_transcript_file()

    def resizeEvent(self, event: QResizeEvent) -> None:
        """Handle resize events to detect layout stabilization and trigger lazy updating."""
        super().resizeEvent(event)
        if self._input_spacer is None:
            return

        self._update_input_position()

        if self._auto_scroll:
            self._scroll_to_bottom()

    def cancel_current_tasks(self, notify: bool = True) -> None:
        """Cancel any ongoing AI response tasks."""
        # First remove any active tool approval UI
        if self._pending_tool_call_approval:
            self._pending_tool_call_approval.remove_tool_approval_ui()
            self._pending_tool_call_approval = None

        self._ai_conversation.cancel_current_tasks(notify)

    def handle_esc_key(self) -> bool:
        """
        Handle Esc key press with confirmation for active streaming or pending tool approval.

        If the conversation is actively streaming or has a pending tool approval,
        shows a confirmation dialog before canceling. The dialog warns that this
        is a dangerous operation.

        Returns:
            bool: True if the Esc key was handled (even if user declined), False otherwise
        """
        # If not streaming and no pending tool approval, nothing to cancel
        if not self._is_streaming and not self._pending_tool_call_approval:
            return False

        # Show confirmation dialog for dangerous operation
        strings = self._language_manager.strings()
        result = MessageBox.show_message(
            self,
            MessageBoxType.QUESTION,
            strings.cancel_conversation_title,
            strings.cancel_conversation,
            [MessageBoxButton.YES, MessageBoxButton.NO],
            destructive=True
        )

        # Only cancel if user confirmed
        if result == MessageBoxButton.YES:
            self.cancel_current_tasks()

        # Return True even if user declined - we handled the Esc key
        return True

    def _build_widget_style(self) -> str:
        """Build styles for the conversation widget."""
        style_manager = self._style_manager

        return f"""
            QWidget {{
                background-color: {style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
            }}

            {style_manager.get_scrollbar_stylesheet()}
        """

    def _build_conversation_message_styles(self) -> str:
        """Build styles for the main message frame."""
        style_manager = self._style_manager
        border_radius = int(style_manager.message_bubble_spacing())

        # The -2px padding above is to offset the 2px border so that the content area remains the same size
        return f"""
            #ConversationMessage {{
                margin: 0;
                border-radius: {border_radius}px;
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
                border: 2px solid {style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
                padding: -2px;
            }}
            #ConversationMessage[message_source="user"] {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_USER_BACKGROUND)};
                border: 2px solid {style_manager.get_color_str(ColorRole.MESSAGE_USER_BACKGROUND)};
            }}
            #ConversationMessage[message_source="user_input"],
            #ConversationMessage[message_source="ai_streaming"] {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_INPUT_BACKGROUND)};
                border: 2px solid {style_manager.get_color_str(ColorRole.MESSAGE_INPUT_BACKGROUND)};
            }}

            #ConversationMessage #_sections_container {{
                background-color: transparent;
                border: none;
                border-radius: 0;
                padding: 0;
                margin: 0;
            }}

            /* The banner is opaque (matching its message background) so it can be pinned
               as a sticky header with content scrolling beneath it. */
            #ConversationMessage #_banner {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
                border: none;
                border-top: 1px solid {style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
                border-top-left-radius: {border_radius}px;
                border-top-right-radius: {border_radius}px;
                border-bottom-left-radius: 0;
                border-bottom-right-radius: 0;
                padding: 0;
                margin: 0;
            }}
            #ConversationMessage #_banner[sticky="true"] {{
                border-top: 1px solid {style_manager.get_color_str(ColorRole.MESSAGE_BORDER)};
                border-top-left-radius: 0;
                border-top-right-radius: 0;
            }}
            #ConversationMessage[message_source="user"] #_banner {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_USER_BACKGROUND)};
                border-top: 1px solid {style_manager.get_color_str(ColorRole.MESSAGE_USER_BACKGROUND)};
            }}
            #ConversationMessage[message_source="user"] #_banner[sticky="true"] {{
                border-top: 1px solid {style_manager.get_color_str(ColorRole.MESSAGE_USER_BORDER)};
            }}
            #ConversationMessage[message_source="user_input"] #_banner,
            #ConversationMessage[message_source="ai_streaming"] #_banner {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_INPUT_BACKGROUND)};
                border-top: none;
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
            #ConversationMessage[message_source="user_input"] #_role_label,
            #ConversationMessage[message_source="user_queued"] #_role_label {{
                color: {style_manager.get_color_str(ColorRole.MESSAGE_USER_QUEUED)};
            }}

            #ConversationMessage #_expand_button,
            #ConversationMessage #_copy_button,
            #ConversationMessage #_save_button,
            #ConversationMessage #_fork_button,
            #ConversationMessage #_edit_button,
            #ConversationMessage #_delete_button,
            #ConversationMessage #_stop_button,
            #ConversationMessage #_submit_button,
            #ConversationMessage #_settings_button,
            #ConversationMessage #_attach_button,
            #ConversationMessage #_attachments_button {{
                background-color: transparent;
                color: {style_manager.get_color_str(ColorRole.TEXT_INACTIVE)};
                border: none;
                border-radius: {max(4, int(4 * style_manager.zoom_factor()))}px;
                padding: 0px;
                margin: 0px;
            }}

            #ConversationMessage #_expand_button:hover,
            #ConversationMessage #_copy_button:hover,
            #ConversationMessage #_save_button:hover,
            #ConversationMessage #_fork_button:hover,
            #ConversationMessage #_edit_button:hover,
            #ConversationMessage #_delete_button:hover,
            #ConversationMessage #_stop_button:hover,
            #ConversationMessage #_submit_button:hover,
            #ConversationMessage #_settings_button:hover,
            #ConversationMessage #_attach_button:hover,
            #ConversationMessage #_attachments_button:hover {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND_HOVER)};
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
            }}

            #ConversationMessage #_expand_button:pressed,
            #ConversationMessage #_copy_button:pressed,
            #ConversationMessage #_save_button:pressed,
            #ConversationMessage #_fork_button:pressed,
            #ConversationMessage #_edit_button:pressed,
            #ConversationMessage #_delete_button:pressed,
            #ConversationMessage #_stop_button:pressed,
            #ConversationMessage #_submit_button:pressed,
            #ConversationMessage #_settings_button:pressed,
            #ConversationMessage #_attach_button:pressed,
            #ConversationMessage #_attachments_button:pressed {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND_PRESSED)};
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
            }}

            #ConversationMessage #_stop_button:disabled,
            #ConversationMessage #_submit_button:disabled {{
                color: {style_manager.get_color_str(ColorRole.TEXT_DISABLED)};
                background-color: transparent;
            }}

            #ConversationMessage[message_source="user"] #_expand_button:hover,
            #ConversationMessage[message_source="user"] #_copy_button:hover,
            #ConversationMessage[message_source="user"] #_save_button:hover,
            #ConversationMessage[message_source="user"] #_fork_button:hover,
            #ConversationMessage[message_source="user"] #_edit_button:hover,
            #ConversationMessage[message_source="user"] #_delete_button:hover,
            #ConversationMessage[message_source="user"] #_attachments_button:hover {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_USER_BACKGROUND_HOVER)};
            }}

            #ConversationMessage[message_source="user"] #_expand_button:pressed,
            #ConversationMessage[message_source="user"] #_copy_button:pressed,
            #ConversationMessage[message_source="user"] #_save_button:pressed,
            #ConversationMessage[message_source="user"] #_fork_button:pressed,
            #ConversationMessage[message_source="user"] #_edit_button:pressed,
            #ConversationMessage[message_source="user"] #_delete_button:pressed,
            #ConversationMessage[message_source="user"] #_attachments_button:pressed {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_USER_BACKGROUND_PRESSED)};
            }}

            #ConversationMessage #_attachments_container {{
                background-color: transparent;
                border: none;
            }}

            #ConversationMessage #_chips_bar {{
                background-color: transparent;
                border: none;
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

            #ConversationMessage #_edit_area {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_USER_BACKGROUND)};
                border-radius: {border_radius}px;
                border: 0;
            }}

            #ConversationMessage #_edit_text_edit {{
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                background-color: transparent;
                border: none;
                padding: 0;
                margin: 0 0 {border_radius}px 0;
                selection-background-color: {style_manager.get_color_str(ColorRole.TEXT_SELECTED)};
            }}

            #ConversationMessage #_edit_btn_row {{
                background-color: transparent;
            }}

            #ConversationMessage #_attachments_bar {{
                background-color: transparent;
                border: none;
            }}

            #ConversationMessage #_attachment_widget {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_ATTACHMENT_BACKGROUND)};
                border: 1px solid {style_manager.get_color_str(ColorRole.MESSAGE_USER_BORDER)};
                border-radius: 4px;
            }}

            #ConversationMessage #_attachment_label {{
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                background-color: transparent;
            }}

            #ConversationMessage #_attachment_remove {{
                background-color: transparent;
                border: none;
                padding: 0;
                margin: 0;
            }}

            #ConversationMessage #_attachment_remove:hover {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_USER_BACKGROUND_HOVER)};
            }}

            #ConversationMessage #_attachment_remove:pressed {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_USER_BACKGROUND_PRESSED)};
            }}

            #ConversationMessage #_edit_confirm_button {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_EDIT)};
                color: {style_manager.get_color_str(ColorRole.TEXT_RECOMMENDED)};
                border: none;
                border-radius: 4px;
                padding: 4px 12px;
            }}

            #ConversationMessage #_edit_confirm_button:hover {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_EDIT_HOVER)};
            }}

            #ConversationMessage #_edit_confirm_button:pressed {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_EDIT_PRESSED)};
            }}

            #ConversationMessage #_edit_cancel_button {{
                background-color: transparent;
                color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE)};
                border: 1px solid {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE)};
                border-radius: 4px;
                padding: 4px 12px;
            }}

            #ConversationMessage #_edit_cancel_button:hover {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND_HOVER)};
            }}

            #ConversationMessage #_edit_cancel_button:pressed {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND_HOVER)};
                border-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE_PRESSED)};
                color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE_PRESSED)};
            }}

            {style_manager.get_scrollbar_stylesheet(
                "#ConversationMessage #_approval_context_widget #_approval_context_text_edit QScrollBar"
            )}

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

            #ConversationMessage #_retry_widget {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
            }}
            #ConversationMessage #_retry_button {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_RECOMMENDED)};
                color: {style_manager.get_color_str(ColorRole.TEXT_RECOMMENDED)};
                border-radius: 4px;
                padding: 4px 48px;
            }}
            #ConversationMessage #_retry_button:hover {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_RECOMMENDED_HOVER)};
            }}
            #ConversationMessage #_retry_button:pressed {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_RECOMMENDED_PRESSED)};
            }}
        """

    def _build_conversation_message_section_styles(self) -> str:
        """Build styles for message sections."""
        style_manager = self._style_manager
        border_radius = int(style_manager.message_bubble_spacing() / 2)
        return f"""
            #ConversationMessage #ConversationMessageSection[section_style="text-system"] {{
                background-color: transparent;
                margin: 0;
                border-radius: {border_radius}px;
                border: 0;
            }}
            #ConversationMessage #ConversationMessageSection[section_style="text-user"] {{
                background-color: transparent;
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

            #ConversationMessage #ConversationMessageSection #_banner_container {{
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

            /* Labels (syntax headers) within message sections */
            #ConversationMessage #ConversationMessageSection QLabel {{
                color: {style_manager.get_color_str(ColorRole.MESSAGE_SYNTAX)};
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

            {style_manager.get_scrollbar_stylesheet("#ConversationMessage #ConversationMessageSection QScrollBar")}
        """

    def apply_style(self) -> None:
        """Apply current style settings."""
        style_manager = self._style_manager
        spacing = int(style_manager.message_bubble_spacing())
        self._messages_layout.setSpacing(spacing)
        zoom_factor = style_manager.zoom_factor()
        self._messages_container.setMaximumWidth(int(style_manager.nice_tab_width() * zoom_factor))

        font = self.font()
        base_font_size = style_manager.base_font_size()
        font.setPointSizeF(base_font_size * style_manager.zoom_factor())
        self.setFont(font)

        new_stylesheet = "\n".join([
            self._build_widget_style(),
            self._build_conversation_message_styles(),
            self._build_conversation_message_section_styles()
        ])

        # Style sheet changes are very expensive.  Don't do them unless we must.
        if new_stylesheet != self.styleSheet():
            self.setStyleSheet(new_stylesheet)

        self._message_style = self._build_message_style()
        for message in self._messages:
            if message.is_rendered():
                message.apply_style(self._message_style)

        self._input.apply_style(self._message_style)
        if self._input_spacer is not None:
            QTimer.singleShot(0, self._on_input_size_hint_changed)

    def _build_message_style(self) -> ConversationMessageStyle:
        """Build the shared style object for all ConversationMessage instances."""
        style_manager = self._style_manager
        zoom_factor = style_manager.zoom_factor()
        base_font_size = style_manager.base_font_size()
        icon_base_size = 14
        icon_scaled_size = int(icon_base_size * zoom_factor)

        font = QFont(self.font())
        font.setPointSizeF(base_font_size * zoom_factor)

        chip_font = QFont(self.font())
        chip_font.setPointSizeF(base_font_size * zoom_factor * 0.8)

        icon_size = QSize(icon_scaled_size, icon_scaled_size)

        return ConversationMessageStyle(
            font=font,
            chip_font=chip_font,
            spacing=int(style_manager.message_bubble_spacing()),
            icon_size=icon_size,
            copy_icon=QIcon(style_manager.scale_icon("copy", icon_base_size)),
            save_icon=QIcon(style_manager.scale_icon("save", icon_base_size)),
            fork_icon=QIcon(style_manager.scale_icon("fork", icon_base_size)),
            edit_icon=QIcon(style_manager.scale_icon("edit", icon_base_size)),
            delete_icon=QIcon(style_manager.scale_icon("delete", icon_base_size)),
            paperclip_icon=QIcon(style_manager.scale_icon("paperclip", icon_base_size)),
            expand_down_icon=QIcon(style_manager.scale_icon("expand-down", icon_base_size)),
            expand_right_icon=QIcon(style_manager.scale_icon("expand-right", icon_base_size)),
            expand_left_icon=QIcon(style_manager.scale_icon("expand-left", icon_base_size)),
        )

    def _show_conversation_context_menu(self, pos: QPoint) -> None:
        """
        Create and show the context menu at the given position.

        Args:
            pos: Local coordinates for menu position
        """
        menu = self._style_manager.create_menu(self)
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

    def _on_message_fork_requested(self) -> None:
        """Fork the conversation from the specified message."""
        # Find the index of the message in our list
        message = self.sender()
        if not isinstance(message, ConversationMessage):
            return

        message_index = self._messages.index(message)

        # Emit signal with the end index (inclusive)
        self.fork_from_index_requested.emit(message_index)

    def _on_message_edit_confirmed(self, new_text: str) -> None:
        """Handle confirmed inline edit: truncate from that message onward and resubmit."""
        sender = self.sender()
        if not isinstance(sender, ConversationMessage):
            return

        # Guard: only handle widgets that belong to THIS conversation
        if sender not in self._messages:
            return

        widget_index = self._messages.index(sender)
        if widget_index < 0 or widget_index >= len(self._messages):
            return

        if self._messages[widget_index].message_source() != AIMessageSource.USER:
            return

        if self._is_streaming:
            self.cancel_current_tasks(False)
            self._is_streaming = False
            self._input.set_streaming(False)
            self._stop_message_border_animation()

            self.status_updated.emit()

        # Truncate history via the wrapper — this also writes the transcript
        message_id = sender.message_id()
        if message_id is None:
            return

        if self._ai_conversation.truncate_to_message(message_id) is None:
            self._logger.error("Failed to truncate conversation at message %s", message_id)
            return

        preserved_messages = self._messages[:widget_index]
        for i in range(len(self._messages) - 1, widget_index - 1, -1):
            message_widget = self._messages[i]
            if self._message_with_selection == message_widget:
                self._message_with_selection = None

            if self._last_error_message_widget == message_widget:
                self._last_error_message_widget = None

            self._remove_response_reveal(message_widget)
            self._messages_layout.removeWidget(message_widget)
            message_widget.deleteLater()

        self._messages = preserved_messages

        conversation_settings = self._ai_conversation.conversation_settings()
        self._input.set_model(AIConversationSettings.get_display_name(conversation_settings.model, conversation_settings.provider))

        if self._animated_message and self._animated_message not in preserved_messages:
            self._stop_message_border_animation()

        self.status_updated.emit()
        self._spotlighted_message_index = -1
        self._auto_scroll = True
        self._input.set_plain_text(new_text.strip())
        self.submit()

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

            self.status_updated.emit()

        # Truncate history via the wrapper — this also writes the transcript
        message_id = sender.message_id()
        if message_id is None:
            return

        prompt = self._ai_conversation.truncate_to_message(message_id)
        if prompt is None:
            self._logger.error("Failed to truncate conversation at message %s", message_id)
            return

        # Remove message widgets from the layout
        preserved_messages = self._messages[:index]
        for i in range(len(self._messages) - 1, index - 1, -1):
            message_widget = self._messages[i]
            if self._message_with_selection == message_widget:
                self._message_with_selection = None

            if self._last_error_message_widget == message_widget:
                self._last_error_message_widget = None

            self._remove_response_reveal(message_widget)
            self._messages_layout.removeWidget(message_widget)
            message_widget.deleteLater()

        self._messages = preserved_messages

        conversation_settings = self._ai_conversation.conversation_settings()
        self._input.set_model(AIConversationSettings.get_display_name(conversation_settings.model, conversation_settings.provider))

        if self._animated_message and self._animated_message not in preserved_messages:
            self._stop_message_border_animation()

        self.status_updated.emit()
        self._spotlighted_message_index = -1
        self._input.set_content(prompt)
        self._input.set_spotlighted(True)
        self._input.setFocus()
        self._auto_scroll = True
        self._scroll_to_bottom()

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
        attachments = self._input.get_attachments()
        if not content and not attachments:
            return

        # Store each attachment in the conversation history and collect GUIDs
        history = self._ai_conversation.get_conversation_history()
        attachment_guids = []
        for filename, file_content in attachments:
            guid = history.add_attachment(file_content, filename, "file")
            attachment_guids.append(guid)

        sanitized_content = self._sanitize_input(content)
        self._input.clear()
        self._input.clear_attachments()

        # We need to decide if we're already streaming or if this is a new message.
        if self._is_streaming:
            # We're streaming, so auto-reject any pending tool approval
            if self._pending_tool_call_approval:
                self._pending_tool_call_approval.remove_tool_approval_ui()
                self._pending_tool_call_approval = None

                loop = asyncio.get_event_loop()
                loop.create_task(self._ai_conversation.reject_pending_tool_calls("User interrupted with new message"))

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

        if requester is None and self._mindspace_manager.has_mindspace():
            preview = sanitized_content[:120].replace('\n', ' ')
            attachment_note = f"\nattachments: {len(attachment_guids)}" if attachment_guids else ""
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"Human submitted prompt: '{preview}{'...' if len(sanitized_content) > 120 else ''}'"
                f"{attachment_note}"
            )

        loop.create_task(self._ai_conversation.submit_message(
            requester,
            sanitized_content,
            attachment_guids=attachment_guids if attachment_guids else None
        ))

    def _on_stop_requested(self) -> None:
        """Handle stop request from input widget."""
        self.cancel_current_tasks()

    def _on_input_settings_requested(self) -> None:
        """Handle settings request from input widget."""
        self.conversation_settings_requested.emit()

    def _on_attach_requested(self) -> None:
        """Handle attach file request: open file dialog and attach selected file."""
        strings = self._language_manager.strings()
        extensions = ProgrammingLanguageUtils.get_supported_file_extensions()
        ext_pattern = " ".join(f"*{ext}" for ext in sorted(extensions))
        file_filter = f"Text and code files ({ext_pattern} *.docx *.pdf);;All files (*)"
        path, _ = QFileDialog.getOpenFileName(self, strings.file_dialog_attach_file, "", file_filter)
        if not path:
            return

        filename = os.path.basename(path)

        if path.lower().endswith(".pdf"):
            content = self._extract_pdf_text(path)
            if content is None:
                return

            size_kb = len(content) // 1024
            if len(content) > 100 * 1024:
                result = MessageBox.show_message(
                    self,
                    MessageBoxType.WARNING,
                    strings.file_error_title,
                    strings.warning_file_too_large.format(filename=filename, size_kb=size_kb),
                    [MessageBoxButton.YES, MessageBoxButton.NO]
                )

                if result != MessageBoxButton.YES:
                    return

        elif path.lower().endswith(".docx"):
            content = self._extract_docx_text(path)
            if content is None:
                return

            size_kb = len(content) // 1024
            if len(content) > 100 * 1024:
                result = MessageBox.show_message(
                    self,
                    MessageBoxType.WARNING,
                    strings.file_error_title,
                    strings.warning_file_too_large.format(filename=filename, size_kb=size_kb),
                    [MessageBoxButton.YES, MessageBoxButton.NO]
                )
                if result != MessageBoxButton.YES:
                    return

        else:
            file_size = os.path.getsize(path)
            size_kb = file_size // 1024
            if file_size > 100 * 1024:
                result = MessageBox.show_message(
                    self,
                    MessageBoxType.WARNING,
                    strings.file_error_title,
                    strings.warning_file_too_large.format(filename=filename, size_kb=size_kb),
                    [MessageBoxButton.YES, MessageBoxButton.NO]
                )
                if result != MessageBoxButton.YES:
                    return

            try:
                with open(path, encoding='utf-8', errors='replace') as f:
                    content = f.read()

            except Exception as e:
                self._logger.warning("Failed to read attachment %s: %s", path, str(e))
                return

        self._input.add_attachment(filename, content)
        if self._mindspace_manager.has_mindspace():
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"Human attached file to conversation: '{path}'"
            )

    def _extract_pdf_text(self, path: str) -> str | None:
        """Extract text from a PDF file, showing an error dialog on failure.

        Returns the extracted text, or None if extraction failed or was unsupported.
        """
        strings = self._language_manager.strings()
        filename = os.path.basename(path)
        try:
            with open(path, "rb") as f:
                data = f.read()

            doc = parse_pdf(data)
            return extract_text(doc)

        except PDFUnsupportedError as e:
            MessageBox.show_message(
                self, MessageBoxType.WARNING, strings.file_error_title,
                strings.error_pdf_unsupported.format(filename=filename, reason=str(e))
            )
            self._logger.warning("PDF not supported %s: %s", path, str(e))
            return None

        except PDFError as e:
            MessageBox.show_message(
                self, MessageBoxType.WARNING, strings.file_error_title,
                strings.error_pdf_extraction_failed.format(filename=filename, reason=str(e))
            )
            self._logger.warning("PDF extraction failed %s: %s", path, str(e))
            return None

        except Exception as e:
            MessageBox.show_message(
                self, MessageBoxType.WARNING, strings.file_error_title,
                strings.error_pdf_extraction_failed.format(filename=filename, reason=str(e))
            )
            self._logger.warning("Failed to read PDF %s: %s", path, str(e))
            return None

    def _extract_docx_text(self, path: str) -> str | None:
        """Extract text from a DOCX file, showing an error dialog on failure.

        Returns the extracted text, or None if extraction failed or was unsupported.
        """
        strings = self._language_manager.strings()
        filename = os.path.basename(path)
        try:
            with open(path, "rb") as f:
                data = f.read()
            return extract_docx_text(data)

        except DocxUnsupportedError as e:
            MessageBox.show_message(
                self, MessageBoxType.WARNING, strings.file_error_title,
                strings.error_docx_unsupported.format(filename=filename, reason=str(e))
            )
            self._logger.warning("DOCX not supported %s: %s", path, str(e))
            return None

        except DocxError as e:
            MessageBox.show_message(
                self, MessageBoxType.WARNING, strings.file_error_title,
                strings.error_docx_extraction_failed.format(filename=filename, reason=str(e))
            )
            self._logger.warning("DOCX extraction failed %s: %s", path, str(e))
            return None

        except Exception as e:
            MessageBox.show_message(
                self, MessageBoxType.WARNING, strings.file_error_title,
                strings.error_docx_extraction_failed.format(filename=filename, reason=str(e))
            )
            self._logger.warning("Failed to read DOCX %s: %s", path, str(e))
            return None

    def get_conversation_history(self) -> AIConversationHistory:
        """Get the conversation history object."""
        return self._ai_conversation.get_conversation_history()

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
        metadata["scroll_maximum"] = self._scroll_area.verticalScrollBar().maximum()

        # Store message expansion states
        expansion_states = []
        for message_widget in self._messages:
            expansion_states.append(message_widget.is_expanded())

        metadata["message_expansion"] = expansion_states

        # Store current settings
        settings = self._ai_conversation.conversation_settings()
        metadata["settings"] = {
            "model": settings.model,
            "provider": settings.provider,
            "temperature": settings.temperature,
            "reasoning": settings.reasoning.value,
            "reasoning_effort": settings.reasoning_effort,
        }

        # If we've been asked for temporary state it means we're going to move this
        # widget so prep for moving our conversation state directly.
        if temp_state:
            # Capture tool approval state
            if self._pending_tool_call_approval:
                # Find the message index
                message_index = self._messages.index(self._pending_tool_call_approval)

                # Get the message ID for robust lookup
                message_id = self._pending_tool_call_approval.message_id()

                # Get the tool approval info from the message widget
                tool_approval_info = self._pending_tool_call_approval.get_tool_approval_info()

                if tool_approval_info and tool_approval_info["tool_call"]:
                    metadata["pending_tool_approval"] = {
                        "message_index": message_index,
                        "message_id": message_id,
                        "tool_call": tool_approval_info["tool_call"].to_dict(),
                        "reason": tool_approval_info["reason"],
                        "context": tool_approval_info["context"],
                        "destructive": tool_approval_info["destructive"]
                    }

                    # Clear the approval UI from the old widget
                    self._pending_tool_call_approval.remove_tool_approval_ui()
                    self._pending_tool_call_approval = None

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

        # If a batch load is still in progress, defer restoration until it
        # completes so that self._messages is fully populated when we apply it.
        if self._load_queue:
            self._load_pending_metadata = metadata
            return

        delegated_conversation = False
        if "delegated_conversation" in metadata:
            delegated_conversation = metadata["delegated_conversation"]

        self._is_delegated_conversation = delegated_conversation

        # Restore input content if specified
        if "content" in metadata:
            self.set_input_text(metadata["content"])

        if "cursor" in metadata:
            self._set_cursor_position(metadata["cursor"])

        # Restore vertical scroll position if specified
        if "auto_scroll" in metadata:
            self._auto_scroll = metadata["auto_scroll"]

        if "vertical_scroll" in metadata:
            # Defer so Qt has one event-loop cycle to finish layout geometry
            # before we set the scroll position.
            if self._deferred_scroll_timer_slot is not None:
                self._deferred_scroll_timer.timeout.disconnect(self._deferred_scroll_timer_slot)

            self._deferred_scroll_timer_slot = (
                lambda v=metadata["vertical_scroll"]: self._scroll_area.verticalScrollBar().setValue(v)
            )
            self._deferred_scroll_timer.timeout.connect(self._deferred_scroll_timer_slot)
            self._deferred_scroll_timer.start()

        # Restore message expansion states if specified
        if "message_expansion" in metadata:
            expansion_states = metadata["message_expansion"]
            for i, is_expanded in enumerate(expansion_states):
                self._change_message_expansion(i, is_expanded)

        if "is_streaming" in metadata:
            # Restore streaming state from a tab move
            self._is_streaming = metadata["is_streaming"]
            self._input.set_streaming(self._is_streaming)
            cs = self._ai_conversation.conversation_settings()
            self._input.set_model(AIConversationSettings.get_display_name(cs.model, cs.provider))

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
                reasoning_options = AIReasoningCapability(
                    metadata["settings"].get("reasoning", AIReasoningCapability.NO_REASONING.value)
                )
                settings = AIConversationSettings(
                    model=metadata["settings"].get("model"),
                    provider=metadata["settings"].get("provider", ""),
                    temperature=metadata["settings"].get("temperature"),
                    reasoning=reasoning_options,
                    reasoning_effort=metadata["settings"].get("reasoning_effort"),
                )
                self.update_conversation_settings(settings)

        # Restore tool approval state if present
        if "pending_tool_approval" in metadata:
            approval_info = metadata["pending_tool_approval"]

            # Find the message widget by ID (more robust than index)
            message_id = approval_info["message_id"]
            message_widget = None

            for msg_widget in self._messages:
                if msg_widget.message_id() == message_id:
                    message_widget = msg_widget
                    break

            if message_widget:
                # Recreate the tool call object
                tool_call = AIToolCall.from_dict(approval_info["tool_call"])

                # Show the approval UI
                message_widget.show_tool_approval_ui(
                    tool_call,
                    approval_info["reason"],
                    approval_info["context"],
                    approval_info["destructive"]
                )

                # Update our tracking reference
                self._pending_tool_call_approval = message_widget

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

    def find_text(
        self, text: str, forward: bool = True, case_sensitive: bool = False, regexp: bool = False
    ) -> Tuple[int, int, bool]:
        """
        Find all instances of text and highlight them.

        Args:
            text: Text to search for
            forward: Whether to search forward from current position
            case_sensitive: If True, match case exactly.
            regexp: If True, treat text as a regular expression.

        Returns:
            Tuple of (current_match, total_matches)
        """
        # Get searchable widgets
        widgets = self._messages + [self._input]

        # Clear existing highlights if search text changed
        if (text, case_sensitive, regexp) != self._last_search:
            self._clear_highlights()
            self._matches = []
            self._current_widget_index = -1
            self._current_match_index = -1
            self._last_search = (text, case_sensitive, regexp)

        # Find all matches if this is a new search
        if not self._matches and text:
            total_so_far = 0
            for widget in widgets:
                widget_matches = widget.find_text(text, case_sensitive, regexp)
                if widget_matches:
                    remaining = 500 - total_so_far
                    widget_matches = widget_matches[:remaining]
                    self._matches.append((widget, widget_matches))
                    total_so_far += len(widget_matches)
                    if total_so_far >= 500:
                        break

        if not self._matches:
            return 0, 0, False

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

    def find_text_at_message(
        self,
        text: str,
        message_id: str,
        case_sensitive: bool = False,
        regexp: bool = False,
    ) -> Tuple[int, int, bool]:
        """Highlight all matches and scroll to the first match in the message with the given ID.

        Falls back to the first match in the conversation if the message is not found or has no matches.

        Args:
            text: Text to search for
            message_id: ID of the target message widget
            case_sensitive: If True, match case exactly.
            regexp: If True, treat text as a regular expression.

        Returns:
            Tuple of (current_match, total_matches)
        """
        self._ensure_matches(text, case_sensitive=case_sensitive, regexp=regexp)
        if not self._matches:
            return 0, 0, False

        for widget_index, (widget, widget_matches) in enumerate(self._matches):
            if isinstance(widget, ConversationMessage) and widget.message_id() == message_id:
                if self._current_widget_index == widget_index:
                    self._current_match_index = (self._current_match_index + 1) % len(widget_matches)
                else:
                    self._current_widget_index = widget_index
                    self._current_match_index = 0
                self._highlight_matches()
                self._scroll_to_current_match()
                return self.get_match_status()

        return self.get_match_status()

    def _ensure_matches(self, text: str, case_sensitive: bool = False, regexp: bool = False) -> None:
        """Populate _matches for the given search without navigating.

        If the search parameters have changed, clears existing state and re-scans.
        If they are unchanged and matches are already populated, does nothing.
        """
        widgets = self._messages + [self._input]

        if (text, case_sensitive, regexp) != self._last_search:
            self._clear_highlights()
            self._matches = []
            self._current_widget_index = -1
            self._current_match_index = -1
            self._last_search = (text, case_sensitive, regexp)

        if not self._matches and text:
            for widget in widgets:
                widget_matches = widget.find_text(text, case_sensitive, regexp)
                if widget_matches:
                    self._matches.append((widget, widget_matches))

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

        # Map the match position to the messages container coordinate system and
        # smooth-scroll so the match sits roughly a quarter of the way down the viewport
        pos_in_container = widget.mapTo(self._messages_container, pos_in_message)
        viewport_height = self._scroll_area.viewport().height()
        target = pos_in_container.y() - viewport_height // 4
        scrollbar = self._scroll_area.verticalScrollBar()
        target = max(scrollbar.minimum(), min(scrollbar.maximum(), target))
        self._start_smooth_scroll(target)

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
            search_text, case_sensitive, regexp = self._last_search
            new_matches = widget.find_text(search_text, case_sensitive, regexp)

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

    def get_match_status(self) -> Tuple[int, int, bool]:
        """
        Get the current match status.

        Returns:
            Tuple of (current_match, total_matches, truncated)
        """
        total_matches = sum(len(matches) for _, matches in self._matches)
        if self._current_widget_index == -1:
            return 0, total_matches, total_matches == 500

        current_match = sum(len(matches) for _, matches in self._matches[:self._current_widget_index])
        current_match += self._current_match_index + 1

        return current_match, total_matches, total_matches == 500

    def clear_find(self) -> None:
        """Clear all find state."""
        self._clear_highlights()
        self._matches = []
        self._current_widget_index = -1
        self._current_match_index = -1
        self._last_search = ("", False, False)

    def clear_highlights(self) -> None:
        """Remove find highlights without resetting match state."""
        self._clear_highlights()

    # AI Tool Support Methods

    def _serialize_parent(self, parent: Any) -> Any:
        """
        Serialize an AIConversationParent to a JSON-safe dict, or return None.

        Args:
            parent: AIConversationParent instance or None

        Returns:
            Dict with message_id and tool_call_id, or None
        """
        if parent is None:
            return None

        return {"message_id": parent.message_id, "tool_call_id": parent.tool_call_id}

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
                "parent": self._serialize_parent(history.parent()),
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
            "parent": self._serialize_parent(history.parent()),
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
        max_results: int = 50,
        regexp: bool = False
    ) -> Dict[str, Any]:
        """
        Search for text across all messages.

        Args:
            search_text: Text to search for
            case_sensitive: Case-sensitive search
            message_types: Filter to specific message types
            max_results: Maximum results to return
            regexp: If True, treat search_text as a regular expression.

        Returns:
            Dictionary containing search results

        Raises:
            ValueError: If regexp is True and search_text is not a valid regular expression.
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

        matches = []

        if regexp:
            flags = 0 if case_sensitive else re.IGNORECASE
            try:
                pattern = re.compile(search_text, flags)

            except re.error as e:
                raise ValueError(f"Invalid regular expression: {e}") from e

        else:
            pattern = None

        for idx, msg in enumerate(messages):
            # Apply type filter
            if message_types:
                msg_type = msg.source_str()
                if msg_type not in message_types:
                    continue

            # Search in content
            content = msg.content

            if pattern is not None:
                for m in pattern.finditer(content):
                    pos = m.start()
                    matched = m.group()
                    context_start = max(0, pos - 50)
                    context_end = min(len(content), m.end() + 50)
                    matches.append({
                        "message_index": idx,
                        "message_id": msg.id,
                        "message_type": msg.source_str(),
                        "timestamp": msg.timestamp.isoformat(),
                        "match_position": pos,
                        "context_before": content[context_start:pos],
                        "match_text": matched,
                        "context_after": content[m.end():context_end]
                    })
                    if len(matches) >= max_results:
                        break

            else:
                search_str = search_text if case_sensitive else search_text.lower()
                haystack = content if case_sensitive else content.lower()
                pos = 0
                while True:
                    pos = haystack.find(search_str, pos)
                    if pos == -1:
                        break

                    context_start = max(0, pos - 50)
                    context_end = min(len(content), pos + len(search_text) + 50)
                    matches.append({
                        "message_index": idx,
                        "message_id": msg.id,
                        "message_type": msg.source_str(),
                        "timestamp": msg.timestamp.isoformat(),
                        "match_position": pos,
                        "context_before": content[context_start:pos],
                        "match_text": content[pos:pos + len(search_text)],
                        "context_after": content[pos + len(search_text):context_end]
                    })
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
