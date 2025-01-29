"""Unified conversation tab implementation with correct scrolling and input expansion."""

import asyncio
from datetime import datetime
import logging
import os
from typing import Dict, List, Optional

from PySide6.QtWidgets import (
    QVBoxLayout, QWidget, QScrollArea, QSizePolicy
)
from PySide6.QtCore import QTimer, QPoint, Qt, Slot
from PySide6.QtGui import QCursor, QResizeEvent, QTextCursor

from humbug.ai.conversation_settings import ConversationSettings
from humbug.ai.ai_backend import AIBackend
from humbug.conversation.conversation_history import ConversationHistory
from humbug.conversation.message import Message
from humbug.conversation.message_source import MessageSource
from humbug.conversation.usage import Usage
from humbug.gui.conversation_error import ConversationError
from humbug.gui.color_role import ColorRole
from humbug.gui.conversation_find import ConversationFind
from humbug.gui.find_widget import FindWidget
from humbug.gui.message_widget import MessageWidget
from humbug.gui.live_input_widget import LiveInputWidget
from humbug.gui.status_message import StatusMessage
from humbug.gui.style_manager import StyleManager
from humbug.gui.tab_base import TabBase
from humbug.gui.tab_state import TabState
from humbug.gui.tab_type import TabType
from humbug.transcript.transcript_error import (
    TranscriptError, TranscriptFormatError, TranscriptIOError
)
from humbug.transcript.transcript_handler import TranscriptHandler


class ConversationTab(TabBase):
    """Unified conversation tab implementing single-window feel with distinct regions."""

    def __init__(
        self,
        tab_id: str,
        path: str,
        timestamp: datetime,
        ai_backends: Dict[str, AIBackend],
        parent: Optional[QWidget] = None
    ) -> None:
        """
        Initialize the unified conversation tab.

        Args:
            tab_id: Unique identifier for this tab
            path: Full path to transcript file
            timestamp: ISO format timestamp for the conversation
            ai_backends: AI backend map
            parent: Optional parent widget
        """
        super().__init__(tab_id, parent)
        self._logger = logging.getLogger("ConversationTab")
        self._path = path
        self._timestamp = timestamp
        self._ai_backends = ai_backends
        self._current_tasks: List[asyncio.Task] = []

        # Create transcript handler with provided filename
        self._transcript_handler = TranscriptHandler(
            path,
            timestamp
        )

        self._conversation = ConversationHistory(tab_id)
        self._settings = ConversationSettings()
        self._current_ai_message = None
        self._messages: List[MessageWidget] = []
        self._message_with_selection: Optional[MessageWidget] = None
        self._is_streaming = False

        # Initialize tracking variables
        self._auto_scroll = True
        self._last_scroll_maximum = 0
        self._last_insertion_point = 0

        conversation_layout = QVBoxLayout(self)
        self.setLayout(conversation_layout)

        self._scroll_area = QScrollArea()

        # Add find widget at top (initially hidden)
        self._find_widget = FindWidget()
        self._find_widget.hide()
        self._find_widget.closed.connect(self._close_find)
        self._find_widget.find_next.connect(lambda: self._find_next(True))
        self._find_widget.find_previous.connect(lambda: self._find_next(False))
        conversation_layout.insertWidget(0, self._find_widget)

        self._messages_container = QWidget()

        self._messages_layout = QVBoxLayout(self._messages_container)
        self._messages_container.setLayout(self._messages_layout)

        # Set up the input box
        self._input = LiveInputWidget(self._messages_container)
        self._input.cursorPositionChanged.connect(self._ensure_cursor_visible)
        self._input.selectionChanged.connect(
            lambda has_selection: self._handle_selection_changed(self._input, has_selection)
        )
        self._input.scrollRequested.connect(self._handle_selection_scroll)
        self._input.mouseReleased.connect(self._stop_scroll)

        self._messages_layout.setSpacing(10)
        self._messages_layout.setContentsMargins(10, 10, 10, 10)
        self._messages_layout.addStretch()
        self._messages_layout.addWidget(self._input)

        self._install_activation_tracking(self._input)

        self._style_manager = StyleManager()

        self._messages_container.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Expanding)

        # Set up the scroll area
        self._scroll_area.setFrameStyle(0)
        self._scroll_area.setWidget(self._messages_container)
        self._scroll_area.setWidgetResizable(True)
        self._scroll_area.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Expanding)
        self._scroll_area.setVerticalScrollBarPolicy(Qt.ScrollBarAsNeeded)
        self._scroll_area.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)

        # Main layout
        conversation_layout.setContentsMargins(0, 1, 0, 0)
        conversation_layout.setSpacing(0)
        conversation_layout.addWidget(self._scroll_area)

        # Create find handler
        self._find_handler = ConversationFind()

        self.update_status()

        self._style_manager.style_changed.connect(self._handle_style_changed)
        self._handle_style_changed(self._style_manager.zoom_factor)

        # Create timer for smooth scrolling
        self._scroll_timer = QTimer(self)
        self._scroll_timer.setInterval(16)  # ~60fps
        self._scroll_timer.timeout.connect(self._update_scroll)
        self._last_mouse_pos = None

        # Connect to the vertical scrollbar's change signals
        self._scroll_area.verticalScrollBar().valueChanged.connect(self._on_scroll_value_changed)
        self._scroll_area.verticalScrollBar().rangeChanged.connect(self._on_scroll_range_changed)

        # Handle scrolling requests from input area
        self._input.pageScrollRequested.connect(self._handle_edit_page_scroll)
        self._find_handler.scrollRequested.connect(self._handle_find_scroll)

        # Set initial focus to input area
        QTimer.singleShot(0, self._set_initial_focus)

    async def fork_conversation(self) -> None:
        """Create a copy of this conversation with the same history."""
        # Generate new conversation ID using current time
        timestamp = datetime.utcnow()
        conversation_id = timestamp.strftime("%Y-%m-%d-%H-%M-%S-%f")[:23]

        # Create new file in same directory as current conversation
        base_dir = os.path.dirname(self._path)
        new_path = os.path.join(base_dir, f"{conversation_id}.conv")

        # Create new tab using same history
        forked_tab = ConversationTab(conversation_id, new_path, self._timestamp, self._ai_backends, self.parent())

        # Get all messages and write to new transcript
        messages = self._conversation.get_messages()
        transcript_messages = [msg.to_transcript_dict() for msg in messages]

        try:
            # Write history to new transcript file
            await forked_tab._transcript_handler.write(transcript_messages)
        except Exception as e:
            raise ConversationError(f"Failed to write transcript for forked conversation: {str(e)}") from e

        # Load messages into the new tab
        forked_tab._load_message_history(messages)

        return forked_tab

    @property
    def timestamp(self) -> datetime:
        """Get the timestamp of the conversation."""
        return self._timestamp

    def get_state(self, temp_state: bool=False) -> TabState:
        """Get serializable state for mindspace persistence.

        Returns:
            TabState containing conversation-specific state
        """
        metadata_state = {
        }

        if temp_state:
            metadata_state["content"] = self._input.toPlainText()
            metadata_state["settings"] = {
                "model": self._settings.model,
                "temperature": self._settings.temperature
            }

        return TabState(
            type=TabType.CONVERSATION,
            tab_id=self._tab_id,
            path=self._path,
            timestamp=self._timestamp,
            metadata=metadata_state
        )

    @classmethod
    def load_from_file(cls, path: str, ai_backends: Dict[str, AIBackend], parent=None) -> 'ConversationTab':
        """
        Load a conversation tab from a transcript file.

        Args:
            path: Path to transcript file
            ai_backends: Dictionary mapping provider names to AI backend instances
            parent: Optional parent widget

        Returns:
            Created ConversationTab instance

        Raises:
            ConversationError: If the conversation tab cannot be loaded
        """
        try:
            # Read transcript
            transcript = TranscriptHandler(path)
            transcript_data = transcript.read()

            conversation_id = os.path.splitext(os.path.basename(path))[0]
            timestamp = transcript_data.timestamp

            # Create conversation tab
            conversation_tab = cls(conversation_id, path, timestamp, ai_backends, parent)
            conversation_tab._load_message_history(transcript_data.messages)

            return conversation_tab

        except TranscriptFormatError as e:
            raise ConversationError(f"Failed to load conversation transcript: {str(e)}") from e
        except TranscriptIOError as e:
            raise ConversationError(f"Failed to read conversation transcript: {str(e)}") from e
        except Exception as e:
            raise ConversationError(f"Failed to create conversation tab: {str(e)}") from e

    @classmethod
    def restore_from_state(cls, state: TabState, parent=None, ai_backends: Dict[str, AIBackend] = None) -> 'ConversationTab':
        """Create and restore a conversation tab from serialized state.

        Args:
            state: TabState containing conversation-specific state
            ai_backends: Dictionary mapping provider names to AI backend instances
            parent: Optional parent widget

        Returns:
            Newly created and restored conversation tab

        Raises:
            ValueError: If state is invalid for conversation tab
        """
        if state.type != TabType.CONVERSATION:
            raise ConversationError(f"Invalid tab type for ConversationTab: {state.type}")

        if not state.timestamp:
            raise ConversationError("Conversation tab requires timestamp")

        # Create new tab instance
        tab = cls(state.tab_id, state.path, state.timestamp, ai_backends, parent)

        # Load conversation from transcript
        try:
            transcript = TranscriptHandler(state.path)
            transcript_data = transcript.read()

            # Validate timestamp matches state
            if state.timestamp != transcript_data.timestamp:
                raise ConversationError("Timestamp mismatch in transcript metadata")

            # Load the message history
            tab._load_message_history(transcript_data.messages)

            # Restore content if specified
            if state.metadata:
                if "content" in state.metadata:
                    tab._input.setPlainText(state.metadata["content"])

                if "settings" in state.metadata:
                    if "model" in state.metadata["settings"]:
                        tab._settings.model = state.metadata["settings"]["model"]

                    if "temperature" in state.metadata["settings"]:
                        tab._settings.temperature = state.metadata["settings"]["temperature"]

            return tab

        except ValueError as e:
            raise ConversationError(f"Failed to restore conversation tab: {str(e)}") from e
        except Exception as e:
            raise ValueError(f"Failed to restore conversation tab: {str(e)}") from e

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
            style: The style type ('user', 'ai', or 'system')
            timestamp: Optional ISO format timestamp string
        """
        msg_widget = MessageWidget(self)
        msg_widget.selectionChanged.connect(
            lambda has_selection: self._handle_selection_changed(msg_widget, has_selection)
        )
        msg_widget.scrollRequested.connect(self._handle_selection_scroll)
        msg_widget.mouseReleased.connect(self._stop_scroll)
        msg_widget.set_content(message.content, message.source, message.timestamp)

        # Add widget before input
        self._messages_layout.insertWidget(self._messages_layout.count() - 1, msg_widget)
        self._messages.append(msg_widget)

        self._install_activation_tracking(msg_widget)

        # When we call this we should always scroll to the bottom and restore auto-scrolling
        self._auto_scroll = True
        self._scroll_to_bottom()

        self._conversation.add_message(message)

    def _handle_selection_changed(self, message_widget: MessageWidget, has_selection: bool):
        """Handle selection changes in message widgets."""
        print(f"section changed {has_selection}")
        if not has_selection:
            if self._message_with_selection:
                self._message_with_selection.clear_selection()
                self._message_with_selection = None

            return

        if self._message_with_selection and self._message_with_selection != message_widget:
            self._message_with_selection.clear_selection()

        if message_widget == self._input:
            self._message_with_section = None
        else:
            self._message_with_selection = message_widget

    def _has_selection(self) -> bool:
        """Check if any message has selected text."""
        return self._message_with_selection is not None and self._message_with_selection.has_selection()

    def update_status(self) -> None:
        """Update status bar with token counts and settings."""
        counts = self._conversation.get_token_counts()
        temp_display = f"Temp: {self._settings.temperature:.1f}" if self._settings.temperature is not None else "Temp: N/A"
        message = StatusMessage(
            f"Model: {self._settings.model} | "
            f"{temp_display} | "
            f"Last response - Input: {counts['input']} ({self._settings.context_window}) | "
            f"Output: {counts['output']}"
        )
        self.status_message.emit(message)

    def _handle_edit_page_scroll(self) -> None:
        """
        Handle page up/down scroll requests.

        Args:
            direction: The direction to scroll (PAGE_UP or PAGE_DOWN)
        """
        # Input cursor has already moved - just ensure it's visible
        self._ensure_cursor_visible()

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

    def _handle_find_scroll(self, widget: QWidget, position: int) -> None:
        """
        Handle scroll requests from find operations.

        Args:
            widget: Widget to scroll to
            position: Text position within the widget
        """
        # Calculate widget's position in scroll area
        widget_pos = widget.mapTo(self._messages_container, QPoint(0, 0))

        # Get text edit cursor rect for the position
        text_edit = widget._text_area
        cursor = text_edit.textCursor()
        cursor.setPosition(position)
        text_edit.setTextCursor(cursor)
        cursor_rect = text_edit.cursorRect(cursor)

        # Convert cursor position to global coordinates
        global_pos = text_edit.mapTo(self._messages_container, cursor_rect.topLeft())

        # Ensure position is visible in scroll area
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
        content: str,
        usage: Optional[Usage] = None,
        error: Optional[Dict] = None
    ):
        """Update the current AI response in the conversation."""
        if error:
            self._is_streaming = False
            self._input.set_streaming(False)

            # For cancellation, preserve the partial response first
            if error.get("code") == "cancelled" and self._current_ai_message:
                message = self._conversation.update_message(
                    self._current_ai_message.id,
                    content=self._current_ai_message.content,
                    completed=False
                )
                if message:
                    await self._write_transcript(message)

                self._current_ai_message = None

            # Then add the error message
            error_msg = f"{error['message']}"
            error_message = Message.create(
                MessageSource.SYSTEM,
                error_msg,
                error=error
            )
            self._add_message(error_message)
            asyncio.create_task(self._write_transcript(error_message))
            self._logger.warning("AI response error: %s", error_msg)
            return

        if not self._is_streaming:
            self._is_streaming = True
            self._input.set_streaming(True)

        # If our last message was not from the AI then create a new one.
        if not self._current_ai_message:
            # Create and add initial message

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
            self._last_insertion_point =  total_height - input_height - 2 * self._messages_layout.spacing()

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
            self.update_status()
            self._current_ai_message = None
            await self._write_transcript(message)
            return

        return

    def _load_message_history(self, messages: List[Message]):
        """
        Load existing message history from transcript.

        Args:
            messages: List of Message objects to load
        """
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
        self.update_status()

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

    async def _process_ai_response(self, message: str):
        """Process AI response with streaming."""
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

                self._is_streaming = False
                self._input.set_streaming(False)
                return

            stream = backend.stream_message(
                message,
                self.get_message_context(),
                self.tab_id
            )

            async for response in stream:
                try:
                    await self.update_streaming_response(
                        content=response.content,
                        usage=response.usage,
                        error=response.error
                    )

                    # Handle retryable errors
                    if response.error:
                        if response.error['code'] in ['network_error', 'timeout']:
                            continue  # Continue to next retry attempt
                        return  # Non-retryable error

                except StopAsyncIteration:
                    break

        except (asyncio.CancelledError, GeneratorExit):
            self._logger.debug("AI response cancelled")
            await self.update_streaming_response(
                content="",
                error={
                    "code": "cancelled",
                    "message": "Request cancelled by user"
                }
            )
            return

        except Exception as e:
            self._logger.exception(
                "Error processing AI response with model %s: %s",
                settings.model,
                str(e)
            )
            error = {
                "code": "process_error",
                "message": str(e),
                "details": {"type": type(e).__name__}
            }
            await self.update_streaming_response(
                content="",
                error=error
            )

        finally:
            self._logger.debug("=== Finished AI response ===")

    def cancel_current_tasks(self):
        """Cancel any ongoing AI response tasks."""
        for task in self._current_tasks:
            if not task.done():
                task.cancel()

    def update_conversation_settings(self, new_settings: ConversationSettings):
        """Update conversation settings and associated backend."""
        self._settings = new_settings
        self.update_status()
        provider = ConversationSettings.get_provider(new_settings.model)
        backend = self._ai_backends.get(provider)
        if backend:
            backend.update_conversation_settings(self.tab_id, new_settings)

    def _handle_style_changed(self, factor: float) -> None:
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
        self.setStyleSheet(f"""
            QFrame {{
                background-color: {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
                border-top: 1px solid {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
            }}
        """)

    def can_close(self) -> bool:
        return True

    def close(self) -> None:
        pass

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
        return (self._input.hasFocus() and self._input.textCursor().hasSelection()) or self._has_selection()

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
        has_text = bool(self._input.toPlainText())
        return has_text and not self._is_streaming

    def submit(self):
        """Submit current input text."""
        content = self._sanitize_input(self._input.toPlainText().strip())
        if not content:
            return

        self._input.clear()
        self._input.set_streaming(True)

        # Add the user message to the conversation
        message = Message.create(MessageSource.USER, content)
        self._add_message(message)
        asyncio.create_task(self._write_transcript(message))

        # Start AI response
        task = asyncio.create_task(self._process_ai_response(content))
        self._current_tasks.append(task)

        def task_done_callback(task):
            try:
                self._current_tasks.remove(task)
            except ValueError:
                self._logger.debug("Task already removed")

        task.add_done_callback(task_done_callback)

    def show_find(self):
        """Show the find widget."""
        # Get selected text if any
        if self._message_with_selection:
            cursor = self._message_with_selection._text_area.textCursor()
            if cursor.hasSelection():
                text = cursor.selectedText()
                if '\u2029' not in text:  # Qt uses this for line breaks
                    self._find_widget.set_search_text(text)
                else:
                    self._find_widget.set_search_text("")
        elif self._input.hasFocus():
            cursor = self._input.textCursor()
            if cursor.hasSelection():
                text = cursor.selectedText()
                if '\u2029' not in text:
                    self._find_widget.set_search_text(text)
                else:
                    self._find_widget.set_search_text("")

        self._find_widget.show()

    def _close_find(self):
        """Close the find widget and clear search state."""
        self._find_widget.hide()
        self._find_handler.clear()

    def _find_next(self, forward: bool = True):
        """Find next/previous match."""
        text = self._find_widget.get_search_text()
        # Include both messages and input widget in search
        widgets = self._messages + [self._input]
        self._find_handler.find_text(text, widgets, forward)
        current, total = self._find_handler.get_match_status()
        self._find_widget.set_match_status(current, total)
