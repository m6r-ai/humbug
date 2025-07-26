"""Unified conversation tab implementation."""

import logging
import os
from typing import cast, Dict, Any

from PySide6.QtWidgets import (
    QDialog, QVBoxLayout, QWidget
)
from PySide6.QtCore import Signal

from ai import AIConversationHistory, AIConversationSettings

from humbug.color_role import ColorRole
from humbug.language.language_manager import LanguageManager
from humbug.status_message import StatusMessage
from humbug.style_manager import StyleManager
from humbug.tabs.conversation.conversation_settings_dialog import ConversationSettingsDialog
from humbug.tabs.conversation.conversation_widget import ConversationWidget
from humbug.tabs.find_widget import FindWidget
from humbug.tabs.tab_base import TabBase
from humbug.tabs.tab_state import TabState
from humbug.tabs.tab_type import TabType


class ConversationTab(TabBase):
    """Unified conversation tab."""

    fork_requested = Signal()
    fork_from_index_requested = Signal(int)
    conversation_completed = Signal(dict)  # Signal for conversation completion

    def __init__(
        self,
        tab_id: str,
        path: str,
        parent: QWidget | None = None,
        use_existing_ai_conversation: bool = False
    ) -> None:
        """
        Initialize the unified conversation tab.

        Args:
            tab_id: Unique identifier for this tab, or a UUID will be generated if not provided.
            path: Full path to transcript file
            parent: Optional parent widget
        """
        super().__init__(tab_id, parent)
        self._logger = logging.getLogger("ConversationTab")
        self._path = path

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
            path, self, use_existing_ai_conversation
        )
        self._conversation_widget.fork_requested.connect(self.fork_requested)
        self._conversation_widget.fork_from_index_requested.connect(self.fork_from_index_requested)
        self._conversation_widget.status_updated.connect(self.update_status)
        self._conversation_widget.submit_finished.connect(self._on_submit_finished)
        self._conversation_widget.update_label.connect(self._on_update_label)
        self._conversation_widget.conversation_modified.connect(self._on_conversation_modified)
        layout.addWidget(self._conversation_widget)

        # Install activation tracking
        self._conversation_widget.activated.connect(self.activated)

        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._on_language_changed)

        self._style_manager = StyleManager()
        self._style_manager.style_changed.connect(self._on_style_changed)
        self._on_style_changed()

    def activate(self) -> None:
        """Activate the tab."""
        self._conversation_widget.activate()

    def _on_conversation_modified(self) -> None:
        """Handle when the conversation is modified."""
        self._set_modified(True)

    def _on_language_changed(self) -> None:
        """Update language-specific elements when language changes."""
        # Update find widget text if visible
        if not self._find_widget.isHidden():
            current, total = self._conversation_widget.get_match_status()
            self._find_widget.set_match_status(current, total)

        # Update status bar
        self.update_status()

    def _get_fork_file_name(self, original_path: str) -> str:
        """
        Generate a unique fork name based on the original conversation file.

        Args:
            original_path: Path to the original conversation file

        Returns:
            New filename with " - fork" suffix that doesn't conflict
        """
        parent_path = os.path.dirname(original_path)
        original_filename = os.path.basename(original_path)

        # Split filename and extension (.conv)
        name, ext = os.path.splitext(original_filename)

        # Check if the name already ends with " - fork" or " - fork (n)"
        fork_suffix = " - fork"
        if name.endswith(fork_suffix):
            # Remove the existing " - fork" suffix to get the base name
            base_name = name[:-len(fork_suffix)]

        elif " - fork (" in name and name.endswith(")"):
            # Handle case like "filename - fork (2)" - extract base name
            fork_index = name.rfind(" - fork (")
            if fork_index != -1:
                base_name = name[:fork_index]

            else:
                base_name = name

        else:
            # No existing fork suffix
            base_name = name

        # Generate unique fork name
        counter = 1
        while True:
            if counter == 1:
                candidate_name = f"{base_name}{fork_suffix}{ext}"

            else:
                candidate_name = f"{base_name}{fork_suffix} ({counter}){ext}"

            full_path = os.path.join(parent_path, candidate_name)
            if not os.path.exists(full_path):
                return full_path

            counter += 1

    def set_conversation_history(self, history: AIConversationHistory) -> None:
        """
        Set the conversation history for this tab.

        Args:
            history: AIConversationHistory object containing conversation history
        """
        self._conversation_widget.set_conversation_history(history)

    # pylint: disable=protected-access
    def fork_conversation_from_index(self, message_index: int | None = None) -> 'ConversationTab':
        """
        Create a copy of this conversation with an option to only include selected history.

        Args:
            message_index: Index to fork at (None for full conversation)

        Returns:
            New ConversationTab with forked history

        Raises:
            ConversationError: If the fork operation fails
        """
        # Generate new file path using fork naming convention
        new_path = self._get_fork_file_name(self._path)
        forked_tab = ConversationTab("", new_path, cast(QWidget, self.parent()))
        self._conversation_widget.fork_conversation_from_index(forked_tab._conversation_widget, message_index)
        return forked_tab

    def get_state(self, temp_state: bool=False) -> TabState:
        """Get serializable state for mindspace persistence."""
        metadata = {}

        # Get widget-specific metadata
        metadata.update(self._conversation_widget.create_state_metadata(temp_state))

        if temp_state:
            metadata['temp_state'] = True

        return TabState(
            type=TabType.CONVERSATION,
            tab_id=self._tab_id,
            path=self._path,
            metadata=metadata,
            is_ephemeral=self._is_ephemeral
        )

    @classmethod
    def restore_from_state(cls, state: TabState, parent: QWidget) -> 'ConversationTab':
        """Create and restore a conversation tab from serialized state."""

        use_existing_ai_conversation = bool(state.metadata and state.metadata.get('temp_state'))
        tab = cls(state.tab_id, state.path, parent, use_existing_ai_conversation)
        if state.is_ephemeral:
            tab._is_ephemeral = True

        # Load conversation from transcript
        try:
            # Restore widget-specific state if metadata present
            if state.metadata:
                tab._conversation_widget.restore_from_metadata(state.metadata)

            return tab

        except Exception as e:
            raise ValueError(f"Failed to restore conversation tab: {str(e)}") from e

    def path(self) -> str:
        """Get the conversation file path."""
        return self._path

    def set_path(self, path: str) -> None:
        """
        Set the conversation file path.

        Args:
            new_path: New path for the conversation file
        """
        self._path = path
        self._conversation_widget.set_path(path)

    def conversation_settings(self) -> AIConversationSettings:
        """Get the current conversation settings."""
        return self._conversation_widget.conversation_settings()

    def update_conversation_settings(self, new_settings: AIConversationSettings) -> None:
        """Update conversation settings and associated backend."""
        self._conversation_widget.update_conversation_settings(new_settings)

    def _on_update_label(self) -> None:
        """
        Handle label updates for the conversation tab.
        """
        self.set_updated(True)

    def _on_submit_finished(self, result: Dict[str, Any]) -> None:
        """
        Handle when a submitted message finishes processing.
        """
        # Update the tab bar to indicate content has changed
        self.conversation_completed.emit(result)

    def update_status(self) -> None:
        """Update status bar with token counts and settings."""
        counts = self._conversation_widget.get_token_counts()
        if counts is None:
            return

        settings = self._conversation_widget.conversation_settings()
        if settings is None:
            return

        strings = self._language_manager.strings()

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
            total_input_tokens=counts['input_total'],
            output_tokens=counts['output'],
            max_output_tokens=settings.max_output_tokens,
            total_output_tokens=counts['output_total']
        )

        self.status_message.emit(StatusMessage(status))

    def can_close_tab(self) -> bool:
        """Check if conversation can be closed."""
        return True

    def close_tab(self) -> None:
        """Close the conversation."""
        self.cancel_current_tasks()
        self._conversation_widget.close_widget()

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

    def undo(self) -> None:
        """Undo not supported for conversations."""

    def can_redo(self) -> bool:
        """Check if redo is available."""
        return False

    def redo(self) -> None:
        """Redo not supported for conversations."""

    def can_cut(self) -> bool:
        """Check if cut is available."""
        return self._conversation_widget.can_cut()

    def cut(self) -> None:
        """Cut selected text to clipboard."""
        self._conversation_widget.cut()

    def can_copy(self) -> bool:
        """Check if copy is available."""
        return self._conversation_widget.can_copy()

    def copy(self) -> None:
        """Copy selected text to clipboard."""
        self._conversation_widget.copy()

    def can_paste(self) -> bool:
        """Check if paste is available."""
        return self._conversation_widget.can_paste()

    def paste(self) -> None:
        """Paste text from clipboard."""
        self._conversation_widget.paste()

    def can_submit(self) -> bool:
        """Check if message can be submitted."""
        return self._conversation_widget.can_submit()

    def submit(self) -> None:
        """Submit the current message."""
        self._conversation_widget.submit()

    def submit_with_requester(self, requester: str) -> None:
        """
        Submit the current message with a specific requester and model parameters.

        Args:
            requester: The AI model or user name submitting the message
        """
        self._conversation_widget.submit(requester)

    def show_find(self) -> None:
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

    def _close_find(self) -> None:
        """Close the find widget and clear search state."""
        self._find_widget.hide()
        self._conversation_widget.clear_find()

    def _find_next(self, forward: bool = True) -> None:
        """Find next/previous match."""
        text = self._find_widget.get_search_text()
        current, total = self._conversation_widget.find_text(text, forward)
        self._find_widget.set_match_status(current, total)

    def _on_style_changed(self) -> None:
        """Handle style changes."""
        self.setStyleSheet(f"""
            QWidget {{
                background-color: {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
                border-top: 2px solid {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
            }}
        """)

    def cancel_current_tasks(self) -> None:
        """Cancel any ongoing AI response tasks."""
        self._conversation_widget.cancel_current_tasks()

    def show_conversation_settings_dialog(self) -> None:
        """Show the conversation settings dialog."""
        dialog = ConversationSettingsDialog(self)
        dialog.set_settings(self._conversation_widget.conversation_settings())

        if dialog.exec() == QDialog.DialogCode.Accepted:
            self._conversation_widget.update_conversation_settings(dialog.get_settings())

    def can_navigate_next_message(self) -> bool:
        """Check if navigation to next message is possible."""
        return self._conversation_widget.can_navigate_next_message()

    def navigate_next_message(self) -> None:
        """Navigate to the next message."""
        self._conversation_widget.navigate_to_next_message()

    def can_navigate_previous_message(self) -> bool:
        """Check if navigation to previous message is possible."""
        return self._conversation_widget.can_navigate_previous_message()

    def navigate_previous_message(self) -> None:
        """Navigate to the previous message."""
        self._conversation_widget.navigate_to_previous_message()

    def can_toggle_bookmark(self) -> bool:
        """Can we toggle bookmarks?"""
        return self._conversation_widget.can_toggle_bookmark()

    def is_checked_bookmark(self) -> bool:
        """Is the current bookmark set (checked)?"""
        return self._conversation_widget.is_checked_bookmark()

    def toggle_bookmark(self) -> None:
        """Toggle a bookmark at the current message."""
        self._conversation_widget.toggle_bookmark()

    def can_navigate_next_bookmark(self) -> bool:
        """Can we go to a next bookmark?"""
        return self._conversation_widget.can_navigate_next_bookmark()

    def navigate_next_bookmark(self) -> None:
        """Move to the next bookmark."""
        self._conversation_widget.navigate_next_bookmark()

    def can_navigate_previous_bookmark(self) -> bool:
        """Can we go to a previous bookmark?"""
        return self._conversation_widget.can_navigate_previous_bookmark()

    def navigate_previous_bookmark(self) -> None:
        """Move to the previous bookmark."""
        self._conversation_widget.navigate_previous_bookmark()

    def set_input_text(self, text: str) -> None:
        """Set the input text."""
        self._conversation_widget.set_input_text(text)
