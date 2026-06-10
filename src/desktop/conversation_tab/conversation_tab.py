"""Unified conversation tab implementation."""

import logging
import os
from typing import Dict, Any

from PySide6.QtWidgets import (
    QApplication, QVBoxLayout, QWidget
)
from PySide6.QtCore import QObject, Signal, QRegularExpression

from ai import AIConversationHistory, AIConversationSettings
from ai_transcript_conversation import AITranscriptConversation
from context.context_registry import ContextRegistry
from conversation_context.conversation_context import ConversationContext

from desktop.ai_backend_display import get_backend_display_name
from desktop.conversation_tab.conversation_settings_dialog import ConversationSettingsDialog
from desktop.conversation_tab.conversation_widget import ConversationWidget
from desktop.language.language_manager import LanguageManager
from desktop.mindspace.mindspace_manager import MindspaceManager
from desktop.status_message import StatusMessage
from desktop.style_manager import StyleManager
from desktop.tab import TabBase, TabState
from desktop.widgets import FindWidget


class ConversationTab(TabBase):
    """Unified conversation tab."""

    conversation_completed = Signal(dict)  # Signal for conversation completion

    def __init__(
        self,
        tab_id: str,
        path: str,
        parent: QWidget | None = None,
        ai_transcript_conversation: AITranscriptConversation | None = None
    ) -> None:
        """
        Initialize the unified conversation tab.

        Args:
            tab_id: Unique identifier for this tab, or a UUID will be generated if not provided.
            path: Full path to transcript file
            parent: Optional parent widget
            ai_transcript_conversation: An existing AITranscriptConversation to adopt, or None
        """
        super().__init__(tab_id, parent)
        self._logger = logging.getLogger("ConversationTab")
        self._path = path
        self._conversation_settings_dialog: ConversationSettingsDialog | None = None

        # Create layout
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        # Add find widget at top (initially hidden)
        self._find_widget = FindWidget(self)
        self._find_widget.hide()
        self._find_widget.set_preferred_width(self.preferred_width)
        self._find_widget.closed.connect(self._close_find)
        self._find_widget.search_changed.connect(self._on_search_changed)
        self._find_widget.find_next.connect(lambda: self._find_next(True))
        self._find_widget.find_previous.connect(lambda: self._find_next(False))
        layout.addWidget(self._find_widget)

        # Create conversation widget
        self._conversation_widget = ConversationWidget(
            path, self,
            ai_transcript_conversation=ai_transcript_conversation
        )
        self._conversation_widget.fork_from_index_requested.connect(self.fork_from_index)
        self._conversation_widget.conversation_settings_requested.connect(
            self.show_conversation_settings_dialog
        )
        self._conversation_widget.status_updated.connect(self.update_status)
        self._conversation_widget.submit_finished.connect(self._on_submit_finished)
        self._conversation_widget.update_label.connect(self._on_update_label)
        self._conversation_widget.has_seen_latest_update_changed.connect(self._on_has_seen_latest_update_changed)
        self._conversation_widget.conversation_modified.connect(self._on_conversation_modified)
        layout.addWidget(self._conversation_widget)

        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._on_language_changed)

        self._start_file_watching(self._path)

    def tool_name(self) -> str:
        """Return the tool name for this tab type."""
        return "conversation"

    def tab_title_from_path(self) -> str:
        """Return the conversation title derived from the filename (without extension)."""
        return os.path.splitext(os.path.basename(self._path))[0] if self._path else ""

    def _get_fork_file_name(self) -> str:
        """Generate a unique fork filename based on this conversation's path."""
        parent_path = os.path.dirname(self._path)
        name, ext = os.path.splitext(os.path.basename(self._path))

        fork_suffix = " - fork"
        if name.endswith(fork_suffix):
            base_name = name[:-len(fork_suffix)]

        elif " - fork (" in name and name.endswith(")"):
            fork_index = name.rfind(" - fork (")
            base_name = name[:fork_index] if fork_index != -1 else name

        else:
            base_name = name

        counter = 1
        while True:
            if counter == 1:
                candidate = f"{base_name}{fork_suffix}{ext}"

            else:
                candidate = f"{base_name}{fork_suffix} ({counter}){ext}"

            if not os.path.exists(os.path.join(parent_path, candidate)):
                return os.path.join(parent_path, candidate)

            counter += 1

    def fork_from_index(self, message_index: int) -> None:
        """Fork this conversation from message_index into a new tab."""
        mindspace_manager = MindspaceManager()
        if not mindspace_manager.has_mindspace():
            return

        source_messages = self.conversation_history().get_messages()
        source_index = min(message_index, len(source_messages))
        forked_history = self.ai_conversation().fork_history(source_index)

        new_path = self._get_fork_file_name()
        new_ai_conversation = AITranscriptConversation(new_path)
        new_ai_conversation.set_conversation_history(forked_history)

        fork_title = os.path.splitext(os.path.basename(new_path))[0]
        mindspace_manager.mindspace().contexts().open(
            context_type="conversation",
            path=new_path,
            title=fork_title,
            initial_model=new_ai_conversation,
            requester_id=self._tab_id,
        )

    def on_modified_changed(self, modified: bool) -> None:
        """Conversation tabs become permanent on first modification; nothing extra needed here."""

    def on_path_renamed(self, new_path: str) -> None:
        """Update path and emit a new tab bar label after a conversation file rename."""
        self.set_path(new_path)
        title = os.path.splitext(os.path.basename(new_path))[0] if new_path else ""
        self.tab_label_changed.emit(self._tab_id, title)

    def register_context_models(self, registry: ContextRegistry) -> None:
        """Register the ConversationContext with the registry."""
        conv_context = ConversationContext(
            context_id=self._tab_id,
            ai_transcript_conversation=self.ai_conversation(),
            on_scroll_to_message=self.scroll_to_message,
        )
        registry.register_model(self._tab_id, conv_context)

    def set_active(self, widget: QWidget, active: bool) -> None:
        """
        Set the active state of the tab.

        Args:
            widget: The widget that triggered the activation change
            active: True if the tab is now active, False otherwise
        """
        if active and not self._find_widget.isHidden():
            focus_widget = QApplication.focusWidget()
            current: QObject | None = focus_widget
            while current is not None:
                if current is self._find_widget:
                    return

                current = current.parent()

        self._conversation_widget.set_active(widget, active)
        if active:
            self.activated.emit()

    def activate(self) -> None:
        """Activate the tab."""
        self._conversation_widget.set_active(self._conversation_widget, True)

    def _on_conversation_modified(self) -> None:
        """Handle when the conversation is modified."""
        self._set_modified(True)

    def _on_language_changed(self) -> None:
        """Update language-specific elements when language changes."""
        # Update find widget text if visible
        if not self._find_widget.isHidden():
            current, total, truncated = self._conversation_widget.get_match_status()
            self._find_widget.set_match_status(current, total, truncated)

        # Update status bar
        self.update_status()

    def conversation_history(self) -> AIConversationHistory:
        """Get the conversation history."""
        return self._conversation_widget.get_conversation_history()

    def set_conversation_history(self, history: AIConversationHistory) -> None:
        """
        Set the conversation history for this tab.

        Args:
            history: AIConversationHistory object containing conversation history
        """
        self._conversation_widget.set_conversation_history(history)

    def get_state(self, temp_state: bool=False) -> TabState:
        """Get serializable state for mindspace persistence."""
        metadata = {}

        # Get widget-specific metadata
        metadata.update(self._conversation_widget.create_state_metadata(temp_state))

        if temp_state:
            metadata['find_widget'] = self._find_widget.create_state_metadata()
            metadata['temp_state'] = True

        return TabState(
            type=self.tool_name(),
            tab_id=self._tab_id,
            path=self._path,
            metadata=metadata,
            is_ephemeral=self._is_ephemeral
        )

    @classmethod
    def restore_from_state(cls, state: TabState, parent: QWidget) -> 'ConversationTab':
        """Create and restore a conversation tab from serialized state."""

        ai_transcript_conversation = state.metadata.get('ai_conversation_ref') if state.metadata else None
        tab = cls(state.tab_id, state.path, parent, ai_transcript_conversation=ai_transcript_conversation)
        if state.is_ephemeral:
            tab._is_ephemeral = True

        # Load conversation from transcript
        try:
            # Restore widget-specific state if metadata present
            if state.metadata:
                tab._conversation_widget.restore_from_metadata(state.metadata)

                if 'find_widget' in state.metadata:
                    tab._find_widget.restore_from_metadata(state.metadata['find_widget'])

            return tab

        except Exception as e:
            raise ValueError(f"Failed to restore conversation tab: {str(e)}") from e

    def set_path(self, path: str) -> None:
        """
        Set the file being edited.

        Args:
            path: Path to file
        """
        # Stop watching old path
        if self._path != path:
            self.stop_file_watching()

        self._path = path
        self._conversation_widget.set_path(path)

        # Start watching new path
        if path:
            self._start_file_watching(path)

        self.update_status()

    def ai_conversation(self) -> AITranscriptConversation:
        """Get the AITranscriptConversation instance."""
        return self._conversation_widget.ai_conversation()

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

    def _on_has_seen_latest_update_changed(self, seen: bool) -> None:
        """
        Handle changes to the has-seen-latest-update state.
        """
        self.set_has_seen_latest_update(seen)

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

        if AIConversationSettings.supports_temperature(settings.model, settings.provider):
            temp_display = strings.conversation_status_temperature.format(
                temperature=settings.temperature
            )

        else:
            temp_display = strings.conversation_status_no_temperature

        if settings.reasoning_effort is not None:
            reasoning_display = strings.conversation_status_reasoning.format(
                effort=settings.reasoning_effort.title()
            )
        else:
            reasoning_display = strings.conversation_status_no_reasoning

        provider_display = get_backend_display_name(settings.provider, strings)

        status = strings.conversation_status.format(
            model=AIConversationSettings.get_display_name(settings.model, settings.provider),
            provider=provider_display,
            temperature=temp_display,
            reasoning=reasoning_display,
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
        return self._conversation_widget.can_close()

    def close_tab(self) -> None:
        """Close the conversation."""
        self.stop_file_watching()
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

    def preferred_width(self) -> int | None:
        """Return the preferred column width matching the conversation content max width."""
        style_manager = StyleManager()
        return int(style_manager.nice_tab_width() * style_manager.zoom_factor())

    def apply_style(self) -> None:
        """Apply current style settings to the tab's content widgets."""
        self._find_widget.apply_style()
        self._conversation_widget.apply_style()

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

    def apply_find_search(self, text: str, case_sensitive: bool = False, regexp: bool = False) -> None:
        """Apply a programmatic find/highlight request to the conversation."""
        self._find_widget.set_case_sensitive(case_sensitive)
        self._find_widget.set_regexp(regexp)
        self._find_widget.set_search_text(text)
        self._find_widget.show()
        current, total, truncated = self._conversation_widget.find_text(text, True, case_sensitive=case_sensitive, regexp=regexp)
        self._find_widget.set_match_status(current, total, truncated)
        self._find_widget.setFocus()

    def apply_search_highlight(self, text: str, case_sensitive: bool = False, regexp: bool = False) -> None:
        """Apply a transient search highlight without altering the local find widget."""
        self._conversation_widget.find_text(text, True, case_sensitive=case_sensitive, regexp=regexp)

    def navigate_to_search_match(
        self, text: str, line_number: int | None, message_id: str | None, case_sensitive: bool = False, regexp: bool = False
    ) -> None:
        """Close any active find, then highlight all matches and scroll to the match in the message with message_id."""
        self._close_find()
        if message_id is not None:
            self._conversation_widget.find_text_at_message(text, message_id, case_sensitive=case_sensitive, regexp=regexp)
        else:
            self._conversation_widget.find_text(text, True, case_sensitive=case_sensitive, regexp=regexp)

    def clear_search_highlight(self) -> None:
        """Clear transient search highlights."""
        self._conversation_widget.clear_highlights()

    def _close_find(self) -> None:
        """Close the find widget and clear search state."""
        self._find_widget.hide()
        self._conversation_widget.clear_highlights()

    def _find_next(self, forward: bool = True) -> None:
        """Find next/previous match."""
        text, case_sensitive, regexp = self._find_widget.current_search_request()
        if regexp:
            if text and not QRegularExpression(text).isValid():
                self._find_widget.set_invalid_regexp()
                return

        current, total, truncated = self._conversation_widget.find_text(text, forward, case_sensitive=case_sensitive, regexp=regexp)
        self._find_widget.set_match_status(current, total, truncated)

    def _on_search_changed(self) -> None:
        """Handle search text or mode changes - update matches without navigating."""
        text, case_sensitive, regexp = self._find_widget.current_search_request()
        if regexp:
            if text and not QRegularExpression(text).isValid():
                self._find_widget.set_invalid_regexp()
                return

        current, total, truncated = self._conversation_widget.find_text(text, True, case_sensitive=case_sensitive, regexp=regexp)
        self._find_widget.set_match_status(current, total, truncated)

    def cancel_current_tasks(self) -> None:
        """Cancel any ongoing AI response tasks."""
        self._conversation_widget.cancel_current_tasks()

    def handle_esc_key(self) -> bool:
        """
        Handle Esc key press with confirmation if streaming or pending tool approval.

        Returns:
            bool: True if the Esc key was handled, False otherwise
        """
        return self._conversation_widget.handle_esc_key()

    def can_show_conversation_settings_dialog(self) -> bool:
        """Return True — conversation tabs always support the settings dialog."""
        return True

    def show_conversation_settings_dialog(self) -> None:
        """Show the conversation settings dialog."""
        if self._conversation_settings_dialog and self._conversation_settings_dialog.isVisible():
            self._conversation_settings_dialog.raise_()
            self._conversation_settings_dialog.activateWindow()
            return

        dialog = ConversationSettingsDialog(self)
        self._conversation_settings_dialog = dialog
        dialog.set_settings(self._conversation_widget.conversation_settings())

        def _on_accepted() -> None:
            self._conversation_widget.update_conversation_settings(dialog.get_settings())

        def _on_finished(_result: int) -> None:
            self._conversation_settings_dialog = None

        dialog.accepted.connect(_on_accepted)
        dialog.finished.connect(_on_finished)
        dialog.show()
        dialog.raise_()
        dialog.activateWindow()

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

    def set_input_text(self, text: str) -> None:
        """Set the input text."""
        self._conversation_widget.set_input_text(text)

    # AI Tool Support Methods

    def get_conversation_info(self) -> Dict[str, Any]:
        """
        Get high-level metadata about the conversation.

        Returns:
            Dictionary containing conversation metadata
        """
        return self._conversation_widget.get_conversation_info()

    def read_messages(
        self,
        start_index: int | None = None,
        end_index: int | None = None,
        message_types: list[str] | None = None,
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
        return self._conversation_widget.read_messages(start_index, end_index, message_types, limit)

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
        return self._conversation_widget.get_message_by_id_or_index(message_id, message_index)

    def search_messages(
        self,
        search_text: str,
        case_sensitive: bool = False,
        message_types: list[str] | None = None,
        max_results: int = 50,
        regexp: bool = False
    ) -> Dict[str, Any]:
        """Search for text across all messages."""
        return self._conversation_widget.search_messages(
            search_text, case_sensitive, message_types, max_results, regexp
        )

    def scroll_to_message(
        self,
        message_id: str | None = None,
        message_index: int | None = None
    ) -> Dict[str, Any]:
        """
        Scroll to a specific message.

        Args:
            message_id: Message UUID
            message_index: Message index (0-based)

        Returns:
            Dictionary containing scroll result with success status and details
        """
        return self._conversation_widget.scroll_to_message_by_id_or_index(
            message_id, message_index
        )
