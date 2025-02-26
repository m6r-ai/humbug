"""Conversation widget implementation."""

import asyncio
from dataclasses import dataclass
from datetime import datetime
import logging
from typing import Dict, List, Optional, Tuple, Any

from PySide6.QtWidgets import (
    QWidget, QVBoxLayout, QScrollArea, QSizePolicy, QMenu
)
from PySide6.QtCore import QTimer, QPoint, Qt, Slot, Signal
from PySide6.QtGui import QCursor, QResizeEvent, QTextCursor

from humbug.ai.ai_backend import AIBackend
from humbug.ai.ai_response import AIError
from humbug.ai.conversation_settings import ConversationSettings
from humbug.conversation.conversation_history import ConversationHistory
from humbug.conversation.message import Message
from humbug.conversation.message_source import MessageSource
from humbug.conversation.usage import Usage
from humbug.gui.color_role import ColorRole
from humbug.gui.conversation.conversation_input_widget import ConversationInputWidget
from humbug.gui.conversation.message_widget import MessageWidget
from humbug.gui.style_manager import StyleManager
from humbug.language.language_manager import LanguageManager
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.transcript.transcript_error import TranscriptError
from humbug.transcript.transcript_handler import TranscriptHandler


@dataclass
class BookmarkData:
    """Data associated with a bookmarked message."""
    widget: 'MessageWidget'
    scroll_position: int

    def __init__(self, widget: 'MessageWidget', scroll_position: int):
        """
        Initialize bookmark data.

        Args:
            widget: The bookmarked message widget
            scroll_position: Vertical scroll position when bookmarked
        """
        self.widget = widget
        self.scroll_position = scroll_position


class ConversationWidget(QWidget):
    """Widget for displaying conversation with message history and input."""

    # Signal to notify tab of status changes
    status_message = Signal(str)

    # Signal for tab to handle forking a conversation
    forkRequested = Signal()

    # Signal for bookmark navigation
    bookmarkNavigationRequested = Signal(bool)  # True for next, False for previous

    def __init__(
        self,
        conversation_id: str,
        path: str,
        timestamp: datetime,
        ai_backends: Dict[str, AIBackend],
        parent: Optional[QWidget] = None
    ) -> None:
        """
        Initialize the conversation widget.

        Args:
            conversation_id: Unique identifier for this conversation
            path: Full path to transcript file
            timestamp: ISO format timestamp for the conversation
            ai_backends: AI backend map
            parent: Optional parent widget
        """
        super().__init__(parent)
        self._logger = logging.getLogger("ConversationWidget")
        self._conversation_id = conversation_id
        self._path = path
        self._timestamp = timestamp
        self._ai_backends = ai_backends
        self._current_tasks: List[asyncio.Task] = []
        self._last_submitted_message = None

        self._bookmarked_messages: Dict[MessageWidget, BookmarkData] = {}
        self._current_bookmark_index: Optional[int] = None

        # Create transcript handler with provided filename
        self._transcript_handler = TranscriptHandler(
            path,
            timestamp
        )

        self._mindspace_manager = MindspaceManager()

        self._conversation = ConversationHistory(conversation_id)
        self._settings = ConversationSettings()
        self._current_ai_message = None
        self._current_reasoning_message = None
        self._messages: List[MessageWidget] = []
        self._message_with_selection: Optional[MessageWidget] = None
        self._is_streaming = False

        # Initialize tracking variables
        self._auto_scroll = True
        self._last_scroll_maximum = 0
        self._last_insertion_point = 0

        # Create layout
        conversation_layout = QVBoxLayout(self)
        self.setLayout(conversation_layout)
        conversation_layout.setContentsMargins(0, 0, 0, 0)
        conversation_layout.setSpacing(0)

        # Set up the scroll area
        self._scroll_area = QScrollArea()
        self._scroll_area.setFrameStyle(0)
        self._scroll_area.setWidgetResizable(True)
        self._scroll_area.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Expanding)
        self._scroll_area.setVerticalScrollBarPolicy(Qt.ScrollBarAsNeeded)
        self._scroll_area.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)

        # Create messages container widget
        self._messages_container = QWidget()
        self._messages_layout = QVBoxLayout(self._messages_container)
        self._messages_container.setLayout(self._messages_layout)

        # Set up the input box
        self._input = ConversationInputWidget(self._messages_container)
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

        self._messages_container.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Expanding)
        self._scroll_area.setWidget(self._messages_container)

        # Add the scroll area to the main layout
        conversation_layout.addWidget(self._scroll_area)

        # Setup signals for search highlights
        self._search_highlights = {}

        self._style_manager = StyleManager()
        self._init_colour_mode = self._style_manager.color_mode

        # Add bookmark status
        self._is_bookmarked = False

        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._handle_language_changed)

        # Create timer for smooth scrolling
        self._scroll_timer = QTimer(self)
        self._scroll_timer.setInterval(16)  # ~60fps
        self._scroll_timer.timeout.connect(self._update_scroll)
        self._last_mouse_pos = None

        # Setup context menu
        self.setContextMenuPolicy(Qt.CustomContextMenu)
        self.customContextMenuRequested.connect(self._show_conversation_context_menu)

        # Connect to the vertical scrollbar's change signals
        self._scroll_area.verticalScrollBar().valueChanged.connect(self._on_scroll_value_changed)
        self._scroll_area.verticalScrollBar().rangeChanged.connect(self._on_scroll_range_changed)

        # Set initial focus to input area
        QTimer.singleShot(0, self._set_initial_focus)

        self._style_manager.style_changed.connect(self._handle_style_changed)
        self._handle_style_changed()

    def _handle_language_changed(self) -> None:
        """Update language-specific elements when language changes."""
        # Update input widget streaming state text
        self._input.set_streaming(self._is_streaming)

        # Emit signal for status update
        self._update_status()

    @property
    def conversation_id(self) -> str:
        """Get the conversation ID."""
        return self._conversation_id

    @property
    def path(self) -> str:
        """Get the path to the conversation file."""
        return self._path

    @property
    def timestamp(self) -> datetime:
        """Get the timestamp of the conversation."""
        return self._timestamp

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
            temperature=self._settings.temperature,
            reasoning=self._settings.reasoning
        )

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

    def _add_message(self, message: Message) -> None:
        """
        Add a message to history with appropriate styling.

        Args:
            message: The message text
        """
        msg_widget = MessageWidget(self)
        msg_widget.selectionChanged.connect(
            lambda has_selection: self._handle_selection_changed(msg_widget, has_selection)
        )
        # Add bookmark-specific signal
        msg_widget.scrollRequested.connect(self._handle_selection_scroll)
        msg_widget.mouseReleased.connect(self._stop_scroll)
        msg_widget.set_content(message.content, message.source, message.timestamp)

        # Add widget before input
        self._messages_layout.insertWidget(self._messages_layout.count() - 1, msg_widget)
        self._messages.append(msg_widget)

        # When we call this we should always scroll to the bottom and restore auto-scrolling
        self._auto_scroll = True
        self._scroll_to_bottom()

        self._conversation.add_message(message)

    def _toggle_message_bookmark(self, message_widget: MessageWidget):
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

    def navigate_bookmarks(self, forward: bool = True):
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

    def _handle_selection_changed(self, message_widget: MessageWidget, has_selection: bool):
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
        else:
            self._message_with_selection = message_widget

    def _has_selection(self) -> bool:
        """Check if any message has selected text."""
        return self._message_with_selection is not None and self._message_with_selection.has_selection()

    def update_path(self, new_id: str, new_path: str):
        """Update the conversation file path.

        Args:
            new_id: New conversation ID
            new_path: New path for the conversation file
        """
        self._path = new_path
        self._conversation_id = new_id
        self._transcript_handler.update_path(new_path)

    def _update_status(self) -> None:
        """Update status information and emit signal."""
        counts = self._conversation.get_token_counts()
        strings = self._language_manager.strings

        # Temperature display depends on whether it's available
        if ConversationSettings.supports_temperature(self._settings.model):
            temp_display = strings.conversation_status_temperature.format(
                temperature=self._settings.temperature
            )
        else:
            temp_display = strings.conversation_status_no_temperature

        status = strings.conversation_status.format(
            model=self._settings.model,
            temperature=temp_display,
            input_tokens=counts['input'],
            max_input_tokens=self._settings.context_window,
            output_tokens=counts['output'],
            max_output_tokens=self._settings.max_output_tokens
        )

        self.status_message.emit(status)

    def _handle_edit_page_scroll(self) -> None:
        """
        Handle page up/down scroll requests.
        """
        # Input cursor has already moved - just ensure it's visible
        self._ensure_cursor_visible()

    def _ensure_cursor_visible(self):
        """Ensure the cursor remains visible when it moves."""
        total_height = sum(msg.sizeHint().height() + self._messages_layout.spacing() for msg in self._messages)
        input_cursor = self._input.cursorRect()

        # Use scroll area's ensureVisible method which handles visibility calculations for us
        self._scroll_area.ensureVisible(
            input_cursor.x(),
            total_height + input_cursor.y(),
            1,
            50
        )

    def _handle_find_scroll(self, widget: QWidget, position: int) -> None:
        """
        Handle scroll requests from find operations.

        Args:
            widget: Widget to scroll to
            position: Text position within the widget
        """
        # Get text edit cursor rect for the position
        text_edit = widget._text_area
        cursor = text_edit.textCursor()
        cursor.setPosition(position)
        text_edit.setTextCursor(cursor)
        cursor_rect = text_edit.cursorRect(cursor)

        # Convert cursor position to global coordinates
        global_pos = text_edit.mapTo(self._messages_container, cursor_rect.topLeft())

        # Use ensureVisible for consistent scrolling behavior
        self._scroll_area.ensureVisible(
            0,  # x
            global_pos.y(),  # y
            0,  # xmargin
            50  # ymargin - provide some context around the match
        )

    def set_input_text(self, text: str):
        """Set the input text."""
        self._input.setPlainText(text)
        self._input.setFocus()

    def _set_initial_focus(self):
        """Set initial focus to input area."""
        self._input.setFocus()

    async def update_streaming_response(
        self,
        reasoning: str,
        content: str,
        usage: Optional[Usage] = None,
        error: Optional[AIError] = None
    ):
        """Update the current AI response in the conversation."""
        if error:
            # Only stop streaming if retries are exhausted
            if error.retries_exhausted:
                self._is_streaming = False
                self._input.set_streaming(False)

                # For cancellation, preserve the partial response first
                if self._current_reasoning_message:
                    message = self._conversation.update_message(
                        self._current_reasoning_message.id,
                        content=self._current_reasoning_message.content,
                        completed=False
                    )
                    if message:
                        await self._write_transcript(message)

                    self._current_reasoning_message = None

                if self._current_ai_message:
                    message = self._conversation.update_message(
                        self._current_ai_message.id,
                        content=self._current_ai_message.content,
                        completed=False
                    )
                    if message:
                        await self._write_transcript(message)

                    self._current_ai_message = None

            error_msg = error.message
            error_message = Message.create(
                MessageSource.SYSTEM,
                error_msg,
                error={"code": error.code, "message": error.message, "details": error.details}
            )
            self._add_message(error_message)
            asyncio.create_task(self._write_transcript(error_message))

            # For cancellation, don't log as warning since it's user-initiated
            if error.code == "cancelled":
                self._logger.debug("AI response cancelled by user")
            else:
                self._logger.warning("AI response error: %s", error.message)
            return

        if not self._is_streaming:
            self._is_streaming = True
            self._input.set_streaming(True)

        # Is our message an AI response or is it the AI reasoning?
        if content:
            # We're handling a response.  Is this the first time we're seeing it?
            if not self._current_ai_message:
                # If we previously had reasoning from the AI then close that out
                if self._current_reasoning_message:
                    message = self._conversation.update_message(
                        self._current_reasoning_message.id,
                        reasoning,
                        usage=usage,
                        completed=True
                    )

                    await self._write_transcript(message)
                    self._current_reasoning_message = None

                # Create and add initial AI response message
                settings = self.get_settings()
                message = Message.create(
                    MessageSource.AI,
                    content,
                    model=settings.model,
                    temperature=settings.temperature,
                    completed=False
                )
                self._add_message(message)
                self._current_ai_message = message
            else:
                # Store current scroll position before appending.  If this insertion triggers a change
                # in scrolling state then we'll get a signal and will adjust the scrollbar state based
                # on this.
                total_height = self._messages_container.height()
                input_height = self._input.height()
                self._last_insertion_point = total_height - input_height - 2 * self._messages_layout.spacing()

                # Update our message
                self._messages[-1].set_content(content, MessageSource.AI, self._current_ai_message.timestamp)

                # Update existing message
                message = self._conversation.update_message(
                    self._current_ai_message.id,
                    content,
                    usage=usage,
                    completed=(usage is not None)
                )
                if not message:
                    return
            if usage:
                self._is_streaming = False
                self._input.set_streaming(False)
                self._update_status()
                self._current_reasoning_message = None
                self._current_ai_message = None
                await self._write_transcript(message)
                return

            return

        if reasoning:
            # We're handling reasoning from our AI.  Is it the first time we're seeting this?
            if not self._current_reasoning_message:
                # Create and add initial message
                settings = self.get_settings()
                message = Message.create(
                    MessageSource.REASONING,
                    reasoning,
                    model=settings.model,
                    temperature=settings.temperature,
                    completed=False
                )
                self._add_message(message)
                self._current_reasoning_message = message
            else:
                # Store current scroll position before appending.  If this insertion triggers a change
                # in scrolling state then we'll get a signal and will adjust the scrollbar state based
                # on this.
                total_height = self._messages_container.height()
                input_height = self._input.height()
                self._last_insertion_point = total_height - input_height - 2 * self._messages_layout.spacing()

                # Update our message
                self._messages[-1].set_content(reasoning, MessageSource.REASONING, self._current_reasoning_message.timestamp)

                # Update existing message
                message = self._conversation.update_message(
                    self._current_reasoning_message.id,
                    reasoning,
                    usage=usage,
                    completed=False
                )
                if not message:
                    return

    def load_message_history(self, messages: List[Message]):
        """
        Load existing message history from transcript.

        Args:
            messages: List of Message objects to load
        """
        # Establish a baseline for conversation settings
        self.update_conversation_settings(ConversationSettings(
            model=self._mindspace_manager.settings.model,
            temperature=self._mindspace_manager.settings.temperature
        ))

        # Iterate over the messages
        for message in messages:
            self._add_message(message)

            # Update settings and status if AI message
            if message.source == MessageSource.AI:
                if message.usage:
                    self._conversation.update_last_tokens(
                        message.usage.prompt_tokens,
                        message.usage.completion_tokens
                    )
                if message.model:
                    self.update_conversation_settings(ConversationSettings(
                        model=message.model,
                        temperature=message.temperature
                    ))

        # Update display with final state
        self._update_status()

        # Ensure we're scrolled to the end
        self._auto_scroll = True
        self._scroll_to_bottom()

    def resizeEvent(self, event: QResizeEvent) -> None:
        """Handle resize events."""
        super().resizeEvent(event)

        if self._auto_scroll:
            self._scroll_to_bottom()

    def _sanitize_input(self, text: str) -> str:
        """Strip control characters from input text, preserving newlines and tabs."""
        return ''.join(char for char in text if char == '\n' or char == '\t' or (ord(char) >= 32 and ord(char) != 127))

    async def _write_transcript(self, message: Message) -> None:
        """
        Write messages to transcript file.

        Args:
            messages: List of message dictionaries to write

        Raises:
            IOError: If writing to transcript file fails
        """
        try:
            await self._transcript_handler.write([message.to_transcript_dict()])
        except TranscriptError as e:
            self._logger.error("Failed to write to transcript: %s", e)

    async def _start_ai(self):
        """Submit the message to the AI and process the response."""
        stream = None
        try:
            self._logger.debug("=== Starting new AI response ===")

            # Get appropriate backend for conversation
            settings = self.get_settings()
            provider = ConversationSettings.get_provider(settings.model)
            backend = self._ai_backends.get(provider)

            if not backend:
                error_msg = f"No backend available for provider: {provider}"
                self._logger.error(error_msg)
                error_message = Message.create(
                    MessageSource.SYSTEM,
                    error_msg,
                    {"code": "backend_error", "message": error_msg}
                )
                self._add_message(error_message)
                asyncio.create_task(self._write_transcript(error_message))

                self._restore_last_message()
                self._is_streaming = False
                self._input.set_streaming(False)
                return

            stream = backend.stream_message(
                self._conversation.get_messages_for_context(),
                self._conversation_id
            )

            async for response in stream:
                await self.update_streaming_response(
                    reasoning=response.reasoning,
                    content=response.content,
                    usage=response.usage,
                    error=response.error
                )

                if response.error:
                    if response.error.retries_exhausted:
                        self._restore_last_message()
                        return

                    # We're retrying - continue to the next attempt
                    continue

            # If we get here and are still marked as streaming then we failed to get a
            # complete response before giving up.  This is a failure and should be handled as such.
            if self._is_streaming:
                self._logger.debug("AI response failed (likely timeout)")
                await self.update_streaming_response(
                    reasoning="",
                    content="",
                    error=AIError(
                        code="cancelled",
                        message="Server failed to complete response",
                        retries_exhausted=True,
                        details={"type": "CancelledError"}
                    )
                )
                self._restore_last_message()
                return

        except asyncio.CancelledError:
            self._logger.debug("AI response cancelled")
            await self.update_streaming_response(
                reasoning="",
                content="",
                error=AIError(
                    code="cancelled",
                    message="Request cancelled by user",
                    retries_exhausted=True,
                    details={"type": "CancelledError"}
                )
            )
            self._restore_last_message()
            return

        except Exception as e:
            self._logger.exception(
                "Error processing AI response with model %s: %s",
                settings.model,
                str(e)
            )
            await self.update_streaming_response(
                reasoning="",
                content="",
                error=AIError(
                    code="process_error",
                    message=str(e),
                    retries_exhausted=True,
                    details={"type": type(e).__name__}
                )
            )
            self._restore_last_message()

        finally:
            self._logger.debug("=== Finished AI response ===")

            # Properly close the async generator if it exists
            if stream is not None:
                try:
                    await stream.aclose()
                except Exception as e:
                    # Log but don't propagate generator cleanup errors
                    self._logger.debug("Error during generator cleanup: %s", str(e))

    def _restore_last_message(self):
        """Restore the last submitted message to the input box."""
        if self._last_submitted_message is not None:
            self._input.setPlainText(self._last_submitted_message)
            self._last_submitted_message = None

    def cancel_current_tasks(self):
        """Cancel any ongoing AI response tasks."""
        for task in self._current_tasks:
            if not task.done():
                task.cancel()

    def update_conversation_settings(self, new_settings: ConversationSettings):
        """Update conversation settings and associated backend."""
        self._settings = new_settings
        self._update_status()
        provider = ConversationSettings.get_provider(new_settings.model)
        backend = self._ai_backends.get(provider)
        if backend:
            backend.update_conversation_settings(self._conversation_id, new_settings)

    def _handle_style_changed(self) -> None:
        factor = self._style_manager.zoom_factor
        font = self.font()
        base_font_size = self._style_manager.base_font_size
        font.setPointSizeF(base_font_size * factor)
        self.setFont(font)

        self._messages_container.setStyleSheet(f"""
            QWidget {{
                background-color: {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
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

    def set_search_highlights(self, row: int, highlights: List[Tuple[int, int, bool]]) -> None:
        """Set search highlights for a given row.

        Args:
            row: Row to set highlights for
            highlights: List of (start_col, end_col, is_current) highlight ranges
        """
        if highlights:
            self._search_highlights[row] = highlights
        else:
            self._search_highlights.pop(row, None)

        self.viewport().update()

    def clear_search_highlights(self) -> None:
        """Clear all search highlights."""
        self._search_highlights.clear()

    def scroll_to_match(self, widget: MessageWidget) -> None:
        """Scroll to ensure a specific message widget is visible."""
        if widget not in self._messages:
            return

        # Find widget position in the container
        pos = widget.mapTo(self._messages_container, widget.rect().topLeft())

        # Use ensureVisible for consistent scrolling
        self._scroll_area.ensureVisible(
            pos.x(),
            pos.y(),
            0,
            50  # Provide context above the widget
        )

    def _show_conversation_context_menu(self, pos) -> None:
        """
        Create and show the context menu at the given position.

        Args:
            pos: Local coordinates for menu position
        """
        menu = QMenu(self)

        # Copy action
        copy_action = menu.addAction(self._language_manager.strings.copy)
        copy_action.setEnabled(self._has_selection())
        copy_action.triggered.connect(self.copy)

        # Paste action
        paste_action = menu.addAction(self._language_manager.strings.paste)
        paste_action.setEnabled(True)
        paste_action.triggered.connect(self.paste)
        menu.addSeparator()

        fork_action = menu.addAction(self._language_manager.strings.fork_conversation)
        fork_action.triggered.connect(self.forkRequested)
        menu.addSeparator()

        toggle_bookmark_action = menu.addAction(self._language_manager.strings.bookmark_section)
        toggle_bookmark_action.setEnabled(self.can_toggle_bookmark())
        toggle_bookmark_action.setCheckable(True)
        toggle_bookmark_action.setChecked(self.is_checked_bookmark())
        toggle_bookmark_action.triggered.connect(self.toggle_bookmark)

        next_bookmark_action = menu.addAction(self._language_manager.strings.next_bookmark)
        next_bookmark_action.setEnabled(self.can_next_bookmark())
        next_bookmark_action.triggered.connect(self.next_bookmark)

        prev_bookmark_action = menu.addAction(self._language_manager.strings.previous_bookmark)
        prev_bookmark_action.setEnabled(self.can_previous_bookmark())
        prev_bookmark_action.triggered.connect(self.previous_bookmark)

        # Show menu at click position
        menu.exec_(self.mapToGlobal(pos))

    def can_toggle_bookmark(self) -> bool:
        """Can we toggle bookmarks?"""
        focus_widget = self.focusWidget()
        if not focus_widget:
            return False

        while focus_widget and not isinstance(focus_widget, MessageWidget):
            focus_widget = focus_widget.parentWidget()
            if isinstance(focus_widget, ConversationWidget):
                return False

        if isinstance(focus_widget, ConversationInputWidget):
            return False

        return True

    def is_checked_bookmark(self) -> bool:
        """Is the current bookmark set (checked)?"""
        focus_widget = self.focusWidget()
        if not focus_widget:
            return False

        while focus_widget and not isinstance(focus_widget, MessageWidget):
            focus_widget = focus_widget.parentWidget()
            if isinstance(focus_widget, ConversationWidget):
                return False

        if isinstance(focus_widget, ConversationInputWidget):
            return False

        return focus_widget.is_bookmarked()

    def toggle_bookmark(self) -> None:
        """Toggle a bookmark at the current message."""
        focus_widget = self.focusWidget()
        if not focus_widget:
            return

        while focus_widget and not isinstance(focus_widget, MessageWidget):
            focus_widget = focus_widget.parentWidget()
            if isinstance(focus_widget, ConversationWidget):
                return

        if isinstance(focus_widget, ConversationInputWidget):
            return

        self._toggle_message_bookmark(focus_widget)

    def can_next_bookmark(self) -> bool:
        """Can we go to a next bookmark?"""
        return bool(self._bookmarked_messages)

    def next_bookmark(self) -> None:
        """Move to the next bookmark."""
        self.navigate_bookmarks(forward=True)

    def can_previous_bookmark(self) -> bool:
        """Can we go to a previous bookmark?"""
        return bool(self._bookmarked_messages)

    def previous_bookmark(self) -> None:
        """Move to the previous bookmark."""
        self.navigate_bookmarks(forward=False)

    def can_cut(self) -> bool:
        """Check if cut operation is available."""
        return self._input.hasFocus() and self._input.textCursor().hasSelection()

    def cut(self) -> None:
        """Cut selected text to clipboard."""
        if self._input.hasFocus():
            self._input.cut()

    def can_copy(self) -> bool:
        """Check if copy is available."""
        return (self._input.hasFocus() and self._input.textCursor().hasSelection()) or self._has_selection()

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
        has_text = bool(self._input.toPlainText())
        return has_text and not self._is_streaming

    def submit(self):
        """Submit current input text."""
        content = self._sanitize_input(self._input.toPlainText().strip())
        if not content:
            return

        self._last_submitted_message = content
        self._input.clear()
        self._input.set_streaming(True)

        # Add the user message to the conversation
        message = Message.create(MessageSource.USER, content)
        self._add_message(message)
        asyncio.create_task(self._write_transcript(message))

        # Start AI response
        task = asyncio.create_task(self._start_ai())
        self._current_tasks.append(task)

        def task_done_callback(task):
            try:
                self._current_tasks.remove(task)
            except ValueError:
                self._logger.debug("Task already removed")

        task.add_done_callback(task_done_callback)

    def get_conversation_history(self) -> ConversationHistory:
        """Get the conversation history object."""
        return self._conversation

    def create_state_metadata(self) -> Dict[str, Any]:
        """
        Create metadata dictionary capturing current widget state.

        Returns:
            Dictionary containing conversation state metadata
        """
        metadata = {}

        # Store bookmarks
        bookmark_data = []
        for message_widget, data in self._bookmarked_messages.items():
            if message_widget in self._messages:
                bookmark_data.append({
                    'index': self._messages.index(message_widget),
                    'scroll_position': data.scroll_position
                })
        metadata['bookmarks'] = bookmark_data

        # Store current input content
        metadata["content"] = self._input.toPlainText()

        # Store current settings
        settings = self.get_settings()
        metadata["settings"] = {
            "model": settings.model,
            "temperature": settings.temperature,
            "reasoning": settings.reasoning
        }

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

        # Restore settings if specified
        if "settings" in metadata:
            settings = ConversationSettings(
                model=metadata["settings"].get("model"),
                temperature=metadata["settings"].get("temperature"),
                reasoning=metadata["settings"].get("reasoning", None)
            )
            self.update_conversation_settings(settings)

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

    def set_cursor_position(self, position: Dict[str, int]) -> None:
        """Set cursor position in input area.

        Args:
            position: Dictionary with 'line' and 'column' keys
        """
        if not position:
            return

        cursor = self._input.textCursor()
        cursor.movePosition(QTextCursor.Start)

        # Move cursor to specified position
        for _ in range(position.get("line", 0)):
            cursor.movePosition(QTextCursor.NextBlock)

        cursor.movePosition(
            QTextCursor.Right,
            QTextCursor.MoveAnchor,
            position.get("column", 0)
        )

        self._input.setTextCursor(cursor)
        self._input.ensureCursorVisible()

    def get_cursor_position(self) -> Dict[str, int]:
        """Get current cursor position from input area.

        Returns:
            Dictionary with 'line' and 'column' keys
        """
        cursor = self._input.textCursor()
        return {
            "line": cursor.blockNumber(),
            "column": cursor.columnNumber()
        }
