"""Unified conversation tab implementation."""

import asyncio
from datetime import datetime
import logging
import os
from typing import List, Optional

from PySide6.QtWidgets import (
    QDialog, QVBoxLayout, QWidget
)
from PySide6.QtCore import Signal

from humbug.ai.ai_conversation_settings import AIConversationSettings
from humbug.gui.color_role import ColorRole
from humbug.gui.find_widget import FindWidget
from humbug.gui.status_message import StatusMessage
from humbug.gui.style_manager import StyleManager
from humbug.gui.tab.conversation.conversation_error import ConversationError
from humbug.gui.tab.conversation.conversation_settings_dialog import ConversationSettingsDialog
from humbug.gui.tab.conversation.conversation_transcript_error import (
    ConversationTranscriptFormatError, ConversationTranscriptIOError
)
from humbug.gui.tab.conversation.conversation_transcript_handler import ConversationTranscriptHandler
from humbug.gui.tab.conversation.conversation_widget import ConversationWidget
from humbug.gui.tab.tab_base import TabBase
from humbug.gui.tab.tab_state import TabState
from humbug.gui.tab.tab_type import TabType
from humbug.language.language_manager import LanguageManager


class ConversationTab(TabBase):
    """Unified conversation tab."""

    forkRequested = Signal()
    bookmarkNavigationRequested = Signal(bool)  # True for next, False for previous

    def __init__(
        self,
        tab_id: str,
        path: str,
        timestamp: datetime,
        parent: Optional[QWidget] = None
    ) -> None:
        """
        Initialize the unified conversation tab.

        Args:
            tab_id: Unique identifier for this tab
            path: Full path to transcript file
            timestamp: ISO format timestamp for the conversation
            parent: Optional parent widget
        """
        super().__init__(tab_id, parent)
        self._logger = logging.getLogger("ConversationTab")
        self._path = path
        self._timestamp = timestamp
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
            tab_id, path, timestamp, self
        )
        self._conversation_widget.forkRequested.connect(self.forkRequested)
        self._conversation_widget.status_updated.connect(self.update_status)
        self._conversation_widget.bookmarkNavigationRequested.connect(self.bookmarkNavigationRequested)
        layout.addWidget(self._conversation_widget)

        # Install activation tracking
        self._install_activation_tracking(self._conversation_widget)
        self._conversation_widget.activated.connect(self.activated)

        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._handle_language_changed)

        self._style_manager = StyleManager()
        self._style_manager.style_changed.connect(self._handle_style_changed)
        self._handle_style_changed()

    def _handle_language_changed(self) -> None:
        """Update language-specific elements when language changes."""
        # Update find widget text if visible
        if not self._find_widget.isHidden():
            current, total = self._conversation_widget.get_match_status()
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
        forked_tab = ConversationTab(conversation_id, new_path, self._timestamp, self.parent())

        # Get all messages and write to new transcript
        messages = self._conversation_widget.get_conversation_history().get_messages()
        transcript_messages = [msg.to_transcript_dict() for msg in messages]

        try:
            # Write history to new transcript file
            handler = ConversationTranscriptHandler(new_path, timestamp)
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

        # Get widget-specific metadata
        metadata_state.update(self._conversation_widget.create_state_metadata(temp_state))

        return TabState(
            type=TabType.CONVERSATION,
            tab_id=self._tab_id,
            path=self._path,
            timestamp=self._timestamp,
            metadata=metadata_state
        )

    @classmethod
    def load_from_file(cls, path: str, parent=None) -> 'ConversationTab':
        """
        Load a conversation tab from a transcript file.

        Args:
            path: Path to transcript file
            parent: Optional parent widget

        Returns:
            Created ConversationTab instance

        Raises:
            ConversationError: If the conversation tab cannot be loaded
        """
        try:
            # Read transcript
            transcript = ConversationTranscriptHandler(path)
            transcript_data = transcript.read()

            conversation_id = os.path.splitext(os.path.basename(path))[0]
            timestamp = transcript_data.timestamp

            # Create conversation tab
            conversation_tab = cls(conversation_id, path, timestamp, parent)
            conversation_tab._conversation_widget.load_message_history(transcript_data.messages)

            return conversation_tab

        except ConversationTranscriptFormatError as e:
            raise ConversationError(f"Failed to load conversation transcript: {str(e)}") from e

        except ConversationTranscriptIOError as e:
            raise ConversationError(f"Failed to read conversation transcript: {str(e)}") from e

        except Exception as e:
            raise ConversationError(f"Failed to create conversation tab: {str(e)}") from e

    @classmethod
    def restore_from_state(cls, state: TabState, parent=None) -> 'ConversationTab':
        """Create and restore a conversation tab from serialized state."""
        if state.type != TabType.CONVERSATION:
            raise ConversationError(f"Invalid tab type for ConversationTab: {state.type}")

        if not state.timestamp:
            raise ConversationError("Conversation tab requires timestamp")

        # Check if we have an AIConversation reference to reuse in metadata
        existing_ai_conversation = state.metadata.get("ai_conversation_ref") if state.metadata else None

        # Create new tab instance with or without the existing AIConversation
        if existing_ai_conversation:
            # We need to create a basic tab first
            tab = cls(state.tab_id, state.path, state.timestamp, parent)

            # Then replace its ConversationWidget with one that uses our existing AIConversation
            old_widget = tab._conversation_widget

            # Create new conversation widget with the existing AIConversation
            new_widget = ConversationWidget(
                state.tab_id,
                state.path,
                state.timestamp,
                tab,
                existing_ai_conversation=existing_ai_conversation
            )

            # Set up signals for the new widget
            new_widget.forkRequested.connect(tab.forkRequested)
            new_widget.status_updated.connect(tab.update_status)
            new_widget.bookmarkNavigationRequested.connect(tab.bookmarkNavigationRequested)

            # Install activation tracking
            tab._install_activation_tracking(new_widget)
            new_widget.activated.connect(tab.activated)

            # Replace the widget in the layout
            layout = tab.layout()
            layout.replaceWidget(old_widget, new_widget)
            old_widget.deleteLater()
            tab._conversation_widget = new_widget

            # Load messages from existing AIConversation history
            messages = existing_ai_conversation.get_conversation_history().get_messages()
            loop = asyncio.get_event_loop()
            for message in messages:
                loop.create_task(tab._conversation_widget._add_message(message))

            if state.metadata:
                tab._conversation_widget.restore_from_metadata(state.metadata)

            # Update display with final state
            tab.update_status()

            return tab

        # Standard restoration without AIConversation reuse
        tab = cls(state.tab_id, state.path, state.timestamp, parent)

        # Load conversation from transcript
        try:
            transcript = ConversationTranscriptHandler(state.path)
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

    def update_path(self, new_id: str, new_path: str):
        """Update the conversation file path.

        Args:
            new_id: New ID for the conversation
            new_path: New path for the conversation file
        """
        self._path = new_path
        self._tab_id = new_id
        self._conversation_widget.update_path(new_id, new_path)

    def update_conversation_settings(self, new_settings: AIConversationSettings):
        """Update conversation settings and associated backend."""
        self._conversation_widget.update_conversation_settings(new_settings)

    def update_status(self) -> None:
        """Update status bar with token counts and settings."""
        counts = self._conversation_widget.get_token_counts()
        settings = self._conversation_widget.get_settings()
        strings = self._language_manager.strings

        # Temperature display depends on whether it's available
        if AIConversationSettings.supports_temperature(settings.model):
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
        if self._conversation_widget.has_selection():
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
        self._conversation_widget.clear_find()

    def _find_next(self, forward: bool = True):
        """Find next/previous match."""
        text = self._find_widget.get_search_text()
        current, total = self._conversation_widget.find_text(text, forward)
        self._find_widget.set_match_status(current, total)

    def _handle_style_changed(self):
        """Handle style changes."""
        self.setStyleSheet(f"""
            QWidget {{
                background-color: {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
                border-top: 2px solid {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
            }}
        """)

    def cancel_current_tasks(self):
        """Cancel any ongoing AI response tasks."""
        self._conversation_widget.cancel_current_tasks()

    def show_conversation_settings_dialog(self) -> None:
        """Show the conversation settings dialog."""
        dialog = ConversationSettingsDialog(self)
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
