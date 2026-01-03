"""
Log tab implementation for Humbug application.

This tab provides a read-only view of the mindspace message log for viewing system interactions.
"""

import logging
from typing import Dict, Any

from PySide6.QtWidgets import QVBoxLayout, QWidget

from humbug.language.language_manager import LanguageManager
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.status_message import StatusMessage
from humbug.tabs.find_widget import FindWidget
from humbug.tabs.log.log_widget import LogWidget
from humbug.tabs.tab_base import TabBase
from humbug.tabs.tab_state import TabState
from humbug.tabs.tab_type import TabType


class LogTab(TabBase):
    """
    Log tab for Humbug application showing mindspace message log.

    This tab provides a read-only way to view the mindspace message log
    that tracks system interactions and events.
    """

    def __init__(self, tab_id: str, parent: QWidget | None = None) -> None:
        """
        Initialize the log tab.

        Args:
            tab_id: Unique identifier for this tab, or a UUID will be generated if not provided.
            parent: Optional parent widget
        """
        super().__init__(tab_id, parent)
        self._logger = logging.getLogger("LogTab")
        self._mindspace_manager = MindspaceManager()

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

        # Create log widget
        self._log_widget = LogWidget(self)
        self._log_widget.status_updated.connect(self.update_status)
        layout.addWidget(self._log_widget)
        self._log_widget.update_label.connect(self._on_update_label)
        self._log_widget.has_seen_latest_update_changed.connect(self._on_has_seen_latest_update_changed)

        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._on_language_changed)

    def set_active(self, widget: QWidget, active: bool) -> None:
        """
        Set the active state of the tab.

        Args:
            widget: The widget that triggered the activation change
            active: True if the tab is now active, False otherwise
        """
        self._log_widget.set_active(widget, active)
        if active:
            self.activated.emit()

    def activate(self) -> None:
        """Activate the tab."""
        self._log_widget.set_active(self._log_widget, True)

    def refresh(self) -> None:
        """Refresh log display."""
        self._log_widget.load_messages()

    def _on_language_changed(self) -> None:
        """Update language-specific elements when language changes."""
        # Update find widget text if visible
        if not self._find_widget.isHidden():
            current, total = self._log_widget.get_match_status()
            self._find_widget.set_match_status(current, total)

        # Update status bar
        self.update_status()

    def _on_update_label(self) -> None:
        """
        Handle label updates for the log tab.
        """
        self.set_updated(True)

    def _on_has_seen_latest_update_changed(self, seen: bool) -> None:
        """
        Handle changes to the has-seen-latest-update state.
        """
        self.set_has_seen_latest_update(seen)

    def get_state(self, temp_state: bool = False) -> TabState:
        """
        Get serializable state for persistence.

        Args:
            temp_state: True if we're saving temporary state to restore locally,
                False if we're persisting state

        Returns:
            TabState object containing serializable state
        """
        metadata = self._log_widget.create_state_metadata(temp_state)

        if temp_state:
            metadata['find_widget'] = self._find_widget.create_state_metadata()

        return TabState(
            type=TabType.LOG,
            tab_id=self._tab_id,
            path="",  # Log tab doesn't have a file path
            metadata=metadata
        )

    @classmethod
    def restore_from_state(cls, state: TabState, parent: QWidget) -> 'LogTab':
        """
        Create and restore a tab from serialized state.

        Args:
            state: TabState object containing serialized state
            parent: Parent widget

        Returns:
            Restored LogTab instance
        """
        tab = cls(state.tab_id, parent)

        if state.metadata:
            tab._log_widget.restore_from_metadata(state.metadata)

            if 'find_widget' in state.metadata:
                tab._find_widget.restore_from_metadata(state.metadata['find_widget'])

        return tab

    def set_path(self, path: str) -> None:
        """
        Set the path associated with this tab.

        Args:
            path: Path to associate with this tab
        """
        # Do nothing for log tabs

    def can_close_tab(self) -> bool:
        """Check if log tab can be closed."""
        return True

    def close_tab(self) -> None:
        """Handle tab closing."""

    def can_save(self) -> bool:
        """Check if the tab can be saved."""
        return False

    def save(self) -> bool:
        """Save is not applicable for log tab."""
        return True

    def can_save_as(self) -> bool:
        """Check if the tab can be saved as."""
        return False

    def save_as(self) -> bool:
        """Save as is not applicable for log tab."""
        return True

    def can_undo(self) -> bool:
        """Check if undo is available."""
        return False

    def undo(self) -> None:
        """Undo not supported for log tab."""

    def can_redo(self) -> bool:
        """Check if redo is available."""
        return False

    def redo(self) -> None:
        """Redo not supported for log tab."""

    def can_cut(self) -> bool:
        """Check if cut is available."""
        return False

    def cut(self) -> None:
        """Cut not supported for log tab."""

    def can_copy(self) -> bool:
        """Check if copy is available."""
        return self._log_widget.can_copy()

    def copy(self) -> None:
        """Copy selected text to clipboard."""
        self._log_widget.copy()

    def can_paste(self) -> bool:
        """Check if paste is available."""
        return False

    def paste(self) -> None:
        """Paste not supported for log tab."""

    def can_submit(self) -> bool:
        """Check if message can be submitted."""
        return False

    def submit(self) -> None:
        """Submit not supported for log tab."""

    def show_find(self) -> None:
        """Show the find widget."""
        # Get selected text if any
        if self._log_widget.has_selection():
            selected_text = self._log_widget.get_selected_text()
            if selected_text:
                self._find_widget.set_search_text(selected_text)

            else:
                self._find_widget.set_search_text("")

        self._find_widget.show()
        self._find_widget.setFocus()

    def _close_find(self) -> None:
        """Close the find widget and clear search state."""
        self._find_widget.hide()
        self._log_widget.clear_highlights()

    def _find_next(self, forward: bool = True) -> None:
        """Find next/previous match."""
        text = self._find_widget.get_search_text()
        current, total = self._log_widget.find_text(text, forward)
        self._find_widget.set_match_status(current, total)

    def can_navigate_next_message(self) -> bool:
        """Check if navigation to next message is possible."""
        return self._log_widget.can_navigate_next_message()

    def navigate_next_message(self) -> None:
        """Navigate to the next message."""
        self._log_widget.navigate_to_next_message()

    def can_navigate_previous_message(self) -> bool:
        """Check if navigation to previous message is possible."""
        return self._log_widget.can_navigate_previous_message()

    def navigate_previous_message(self) -> None:
        """Navigate to the previous message."""
        self._log_widget.navigate_to_previous_message()

    def update_status(self) -> None:
        """Update status bar with log tab information."""
        strings = self._language_manager.strings()
        message = StatusMessage(strings.log_status)
        self.status_message.emit(message)

    # AI Tool Support Methods

    def get_log_info(self) -> Dict[str, Any]:
        """
        Get high-level metadata about the log.

        Returns:
            Dictionary containing log metadata
        """
        return self._log_widget.get_log_info()

    def read_messages(
        self,
        start_index: int | None = None,
        end_index: int | None = None,
        levels: list[str] | None = None,
        limit: int | None = None,
        include_content: bool = True
    ) -> Dict[str, Any]:
        """
        Read log messages with filtering and pagination.

        Args:
            start_index: Starting message index (0-based, inclusive)
            end_index: Ending message index (0-based, inclusive)
            levels: List of log levels to include (trace, info, warn, error)
            limit: Maximum number of messages to return
            include_content: Include full message content

        Returns:
            Dictionary containing messages and metadata
        """
        return self._log_widget.read_messages(
            start_index, end_index, levels, limit, include_content
        )

    def get_message_by_id_or_index(
        self,
        message_id: str | None = None,
        message_index: int | None = None
    ) -> Dict[str, Any] | None:
        """
        Get a specific log message by ID or index.

        Args:
            message_id: Message UUID
            message_index: Message index (0-based)

        Returns:
            Message dictionary or None if not found
        """
        return self._log_widget.get_message_by_id_or_index(message_id, message_index)

    def search_messages(
        self,
        search_text: str,
        case_sensitive: bool = False,
        levels: list[str] | None = None,
        max_results: int = 50
    ) -> Dict[str, Any]:
        """Search for text across log messages."""
        return self._log_widget.search_messages(
            search_text, case_sensitive, levels, max_results
        )

    def scroll_to_message(
        self,
        message_id: str | None = None,
        message_index: int | None = None
    ) -> bool:
        """Scroll to a specific log message."""
        return self._log_widget.scroll_to_message_by_id_or_index(
            message_id, message_index
        )
