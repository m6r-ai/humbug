"""Unified conversation tab implementation."""

import asyncio
from datetime import datetime
import logging
import os
from typing import Dict, List, Optional

from PySide6.QtWidgets import (
    QDialog, QVBoxLayout, QWidget
)
from PySide6.QtCore import Signal

from humbug.ai.conversation_settings import ConversationSettings
from humbug.ai.ai_backend import AIBackend
from humbug.gui.color_role import ColorRole
from humbug.gui.conversation.conversation_error import ConversationError
from humbug.gui.conversation.conversation_find import ConversationFind
from humbug.gui.conversation.conversation_settings_dialog import ConversationSettingsDialog
from humbug.gui.conversation.conversation_widget import ConversationWidget
from humbug.gui.find_widget import FindWidget
from humbug.gui.status_message import StatusMessage
from humbug.gui.style_manager import StyleManager
from humbug.gui.tab_base import TabBase
from humbug.gui.tab_state import TabState
from humbug.gui.tab_type import TabType
from humbug.language.language_manager import LanguageManager
from humbug.transcript.transcript_error import TranscriptFormatError, TranscriptIOError
from humbug.transcript.transcript_handler import TranscriptHandler


class ConversationTab(TabBase):
    """Unified conversation tab."""

    forkRequested = Signal()
    bookmarkNavigationRequested = Signal(bool)  # True for next, False for previous

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

        # Create layout
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        # Add find widget at top (initially hidden)
        self._find_widget = FindWidget()
        self._find_widget.hide()
        self._find_widget.closed.connect(self._close_find)
        self._find_widget.find_next.connect(lambda: self._find_next(True))
        self._find_widget.find_previous.connect(lambda: self._find_next(False))
        layout.addWidget(self._find_widget)

        # Create conversation widget
        self._conversation_widget = ConversationWidget(
            tab_id, path, timestamp, ai_backends, self
        )
        self._conversation_widget.forkRequested.connect(self.forkRequested)
        self._conversation_widget.status_updated.connect(self.update_status)
        self._conversation_widget.bookmarkNavigationRequested.connect(self.bookmarkNavigationRequested)
        layout.addWidget(self._conversation_widget)

        # Create find handler
        self._find_handler = ConversationFind()
        self._find_handler.scrollRequested.connect(self._conversation_widget._handle_find_scroll)

        # Install activation tracking
        self._install_activation_tracking(self._conversation_widget)

        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._handle_language_changed)

        self._style_manager = StyleManager()
        self._style_manager.style_changed.connect(self._handle_style_changed)
        self._handle_style_changed()

    def _handle_language_changed(self) -> None:
        """Update language-specific elements when language changes."""
        # Update find widget text if visible
        if not self._find_widget.isHidden():
            current, total = self._find_handler.get_match_status()
            self._find_widget.set_match_status(current, total)

        # Update status bar
        self.update_status()

    async def fork_conversation(self) -> 'ConversationTab':
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
        messages = self._conversation_widget.get_conversation_history().get_messages()
        transcript_messages = [msg.to_transcript_dict() for msg in messages]

        try:
            # Write history to new transcript file
            handler = TranscriptHandler(new_path, timestamp)
            await handler.write(transcript_messages)
        except Exception as e:
            raise ConversationError(f"Failed to write transcript for forked conversation: {str(e)}") from e

        # Load messages into the new tab
        forked_tab._conversation_widget.load_message_history(messages)

        return forked_tab

    @property
    def timestamp(self) -> datetime:
        """Get the timestamp of the conversation."""
        return self._timestamp

    def get_state(self, temp_state: bool=False) -> TabState:
        """Get serializable state for mindspace persistence."""
        metadata_state = {}

        # Store command for both persistent and temporary state
        if temp_state:
            # Get widget-specific metadata
            metadata_state.update(self._conversation_widget.create_state_metadata())

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
            conversation_tab._conversation_widget.load_message_history(transcript_data.messages)

            return conversation_tab

        except TranscriptFormatError as e:
            raise ConversationError(f"Failed to load conversation transcript: {str(e)}") from e
        except TranscriptIOError as e:
            raise ConversationError(f"Failed to read conversation transcript: {str(e)}") from e
        except Exception as e:
            raise ConversationError(f"Failed to create conversation tab: {str(e)}") from e

    @classmethod
    def restore_from_state(cls, state: TabState, parent=None, ai_backends: Dict[str, AIBackend] = None) -> 'ConversationTab':
        """Create and restore a conversation tab from serialized state."""
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
            tab._conversation_widget.load_message_history(transcript_data.messages)

            # Restore widget-specific state if metadata present
            if state.metadata:
                tab._conversation_widget.restore_from_metadata(state.metadata)

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
        self._conversation_widget.set_cursor_position(position)

    def get_cursor_position(self) -> Dict[str, int]:
        """Get current cursor position from input area.

        Returns:
            Dictionary with 'line' and 'column' keys
        """
        return self._conversation_widget.get_cursor_position()

    def update_path(self, new_id: str, new_path: str):
        """Update the conversation file path.

        Args:
            new_id: New ID for the conversation
            new_path: New path for the conversation file
        """
        self._path = new_path
        self._tab_id = new_id
        self._conversation_widget.update_path(new_id, new_path)

    def update_conversation_settings(self, new_settings: ConversationSettings):
        """Update conversation settings and associated backend."""
        self._conversation_widget.update_conversation_settings(new_settings)

    def update_status(self) -> None:
        """Update status bar with token counts and settings."""
        counts = self._conversation_widget.get_token_counts()
        settings = self._conversation_widget.get_settings()
        strings = self._language_manager.strings

        # Temperature display depends on whether it's available
        if ConversationSettings.supports_temperature(settings.model):
            temp_display = strings.conversation_status_temperature.format(
                temperature=settings.temperature
            )
        else:
            temp_display = strings.conversation_status_no_temperature

        status = strings.conversation_status.format(
            model=settings.model,
            temperature=temp_display,
            input_tokens=counts['input'],
            max_input_tokens=settings.context_window,
            output_tokens=counts['output'],
            max_output_tokens=settings.max_output_tokens
        )

        self.status_message.emit(StatusMessage(status))

    def can_close(self) -> bool:
        """Check if terminal can be closed."""
        return True

    def close(self) -> None:
        """Close the terminal."""
        self.cancel_current_tasks()

    def can_save(self) -> bool:
        """Check if conversation can be saved (not applicable)."""
        return False

    def save(self) -> bool:
        """Save conversation (not applicable)."""
        return True

    def can_save_as(self) -> bool:
        """Check if conversation can be saved as (not applicable)."""
        return False

    def save_as(self) -> bool:
        """Save conversation as (not applicable)."""
        return True

    def can_undo(self) -> bool:
        """Check if undo is available."""
        return False

    def undo(self):
        """Undo not supported for conversations."""

    def can_redo(self) -> bool:
        """Check if redo is available."""
        return False

    def redo(self):
        """Redo not supported for conversations."""

    def can_cut(self) -> bool:
        """Check if cut is available."""
        return self._conversation_widget.can_cut()

    def cut(self):
        """Cut selected text to clipboard."""
        self._conversation_widget.cut()

    def can_copy(self) -> bool:
        """Check if copy is available."""
        return self._conversation_widget.can_copy()

    def copy(self):
        """Copy selected text to clipboard."""
        self._conversation_widget.copy()

    def can_paste(self) -> bool:
        """Check if paste is available."""
        return self._conversation_widget.can_paste()

    def paste(self):
        """Paste text from clipboard."""
        self._conversation_widget.paste()

    def can_submit(self) -> bool:
        """Check if message can be submitted."""
        return self._conversation_widget.can_submit()

    def submit(self):
        """Submit the current message."""
        self._conversation_widget.submit()

    def show_find(self):
        """Show the find widget."""
        # Get selected text if any
        selected_text = self._conversation_widget.get_selected_text()
        if selected_text:
            self._find_widget.set_search_text(selected_text)
        else:
            self._find_widget.set_search_text("")

        self._find_widget.show()
        self._find_widget.setFocus()

    def _close_find(self):
        """Close the find widget and clear search state."""
        self._find_widget.hide()
        self._find_handler.clear()
        # Use the existing method to clear search highlights
        self._conversation_widget.clear_search_highlights()

    def _find_next(self, forward: bool = True):
        """Find next/previous match."""
        text = self._find_widget.get_search_text()
        widgets = self._conversation_widget.get_searchable_widgets()
        self._find_handler.find_text(text, widgets, forward)
        current, total = self._find_handler.get_match_status()
        self._find_widget.set_match_status(current, total)

    def _handle_style_changed(self):
        """Handle style changes."""
        # Our styling is primarily handled by the widget
        self.setStyleSheet(f"""
            QWidget {{
                background-color: {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
                border: none;
            }}
        """)

    def cancel_current_tasks(self):
        """Cancel any ongoing AI response tasks."""
        self._conversation_widget.cancel_current_tasks()

    def show_conversation_settings_dialog(self) -> None:
        """Show the conversation settings dialog."""
        dialog = ConversationSettingsDialog(self._ai_backends, self)
        dialog.set_settings(self._conversation_widget.get_settings())

        if dialog.exec() == QDialog.Accepted:
            self._conversation_widget.update_conversation_settings(dialog.get_settings())

    def can_toggle_bookmark(self) -> bool:
        """Can we toggle bookmarks?"""
        return self._conversation_widget.can_toggle_bookmark()

    def is_checked_bookmark(self) -> bool:
        """Is the current bookmark set (checked)?"""
        return self._conversation_widget.is_checked_bookmark()

    def toggle_bookmark(self) -> None:
        """Toggle a bookmark at the current message."""
        self._conversation_widget.toggle_bookmark()

    def can_next_bookmark(self) -> bool:
        """Can we go to a next bookmark?"""
        return self._conversation_widget.can_next_bookmark()

    def next_bookmark(self) -> None:
        """Move to the next bookmark."""
        self._conversation_widget.next_bookmark()

    def can_previous_bookmark(self) -> bool:
        """Can we go to a previous bookmark?"""
        return self._conversation_widget.can_previous_bookmark()

    def previous_bookmark(self) -> None:
        """Move to the previous bookmark."""
        self._conversation_widget.previous_bookmark()

    def set_input_text(self, text: str):
        """Set the input text."""
        self._conversation_widget.set_input_text(text)
