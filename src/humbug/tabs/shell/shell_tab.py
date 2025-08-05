"""
Shell tab implementation for Humbug application.

This tab provides a shell command interface with its own persistent history.
"""

import logging

from PySide6.QtWidgets import QVBoxLayout, QWidget

from humbug.language.language_manager import LanguageManager
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.status_message import StatusMessage
from humbug.tabs.find_widget import FindWidget
from humbug.tabs.shell.shell_widget import ShellWidget
from humbug.tabs.tab_base import TabBase
from humbug.tabs.tab_state import TabState
from humbug.tabs.tab_type import TabType


class ShellTab(TabBase):
    """
    Shell tab for Humbug application showing shell command history.

    This tab provides an interactive shell command interface with its own
    persistent history that is separate from the system-wide message log.
    """

    def __init__(self, tab_id: str, parent: QWidget | None = None) -> None:
        """
        Initialize the shell tab.

        Args:
            tab_id: Unique identifier for this tab, or a UUID will be generated if not provided.
            parent: Optional parent widget
        """
        super().__init__(tab_id, parent)
        self._logger = logging.getLogger("ShellTab")
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

        # Create shell widget
        self._shell_widget = ShellWidget(self)
        self._shell_widget.status_updated.connect(self.update_status)
        layout.addWidget(self._shell_widget)

        # Install activation tracking
        self._shell_widget.activated.connect(self.activated)

        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._on_language_changed)

    def activate(self) -> None:
        """Activate the tab."""
        self._shell_widget.activate()

    def refresh(self) -> None:
        """Refresh shell command history display."""
        self._shell_widget.load_messages()

    def _on_language_changed(self) -> None:
        """Update language-specific elements when language changes."""
        # Update find widget text if visible
        if not self._find_widget.isHidden():
            current, total = self._shell_widget.get_match_status()
            self._find_widget.set_match_status(current, total)

        # Update status bar
        self.update_status()

    def get_state(self, temp_state: bool = False) -> TabState:
        """
        Get serializable state for persistence.

        Args:
            temp_state: True if we're saving temporary state to restore locally,
                False if we're persisting state

        Returns:
            TabState object containing serializable state
        """
        metadata = self._shell_widget.create_state_metadata(temp_state)

        return TabState(
            type=TabType.SHELL,
            tab_id=self._tab_id,
            path="",  # Shell tab doesn't have a file path
            cursor_position=None,
            horizontal_scroll=None,
            vertical_scroll=None,
            metadata=metadata
        )

    @classmethod
    def restore_from_state(cls, state: TabState, parent: QWidget) -> 'ShellTab':
        """
        Create and restore a tab from serialized state.

        Args:
            state: TabState object containing serialized state
            parent: Parent widget

        Returns:
            Restored ShellTab instance
        """
        tab = cls(state.tab_id, parent)

        if state.metadata:
            tab._shell_widget.restore_from_metadata(state.metadata)

        return tab

    def set_path(self, path: str) -> None:
        """
        Set the path associated with this tab.

        Args:
            path: Path to associate with this tab
        """
        # Do nothing for shell tabs

    def can_close_tab(self) -> bool:
        """Check if shell tab can be closed."""
        return True

    def close_tab(self) -> None:
        """Handle tab closing."""

    def can_save(self) -> bool:
        """Check if the tab can be saved."""
        return False

    def save(self) -> bool:
        """Save is not applicable for shell tab."""
        return True

    def can_save_as(self) -> bool:
        """Check if the tab can be saved as."""
        return False

    def save_as(self) -> bool:
        """Save as is not applicable for shell tab."""
        return True

    def can_undo(self) -> bool:
        """Check if undo is available."""
        return False

    def undo(self) -> None:
        """Undo not supported for shell tab."""

    def can_redo(self) -> bool:
        """Check if redo is available."""
        return False

    def redo(self) -> None:
        """Redo not supported for shell tab."""

    def can_cut(self) -> bool:
        """Check if cut is available."""
        return self._shell_widget.can_cut()

    def cut(self) -> None:
        """Cut selected text to clipboard."""
        self._shell_widget.cut()

    def can_copy(self) -> bool:
        """Check if copy is available."""
        return self._shell_widget.can_copy()

    def copy(self) -> None:
        """Copy selected text to clipboard."""
        self._shell_widget.copy()

    def can_paste(self) -> bool:
        """Check if paste is available."""
        return self._shell_widget.can_paste()

    def paste(self) -> None:
        """Paste text from clipboard."""
        self._shell_widget.paste()

    def can_submit(self) -> bool:
        """Check if message can be submitted."""
        return self._shell_widget.can_submit()

    def submit(self) -> None:
        """Submit the current message."""
        self._shell_widget.submit()
        # After submission, update status
        self.update_status()

    def show_find(self) -> None:
        """Show the find widget."""
        # Get selected text if any
        if self._shell_widget.has_selection():
            selected_text = self._shell_widget.get_selected_text()
            if selected_text:
                self._find_widget.set_search_text(selected_text)
            else:
                self._find_widget.set_search_text("")

        self._find_widget.show()
        self._find_widget.setFocus()

    def _close_find(self) -> None:
        """Close the find widget and clear search state."""
        self._find_widget.hide()
        self._shell_widget.clear_highlights()

    def _find_next(self, forward: bool = True) -> None:
        """Find next/previous match."""
        text = self._find_widget.get_search_text()
        current, total = self._shell_widget.find_text(text, forward)
        self._find_widget.set_match_status(current, total)

    def can_navigate_next_message(self) -> bool:
        """Check if navigation to next message is possible."""
        return self._shell_widget.can_navigate_next_message()

    def navigate_next_message(self) -> None:
        """Navigate to the next message."""
        self._shell_widget.navigate_to_next_message()

    def can_navigate_previous_message(self) -> bool:
        """Check if navigation to previous message is possible."""
        return self._shell_widget.can_navigate_previous_message()

    def navigate_previous_message(self) -> None:
        """Navigate to the previous message."""
        self._shell_widget.navigate_to_previous_message()

    def set_input_text(self, text: str) -> None:
        """Set the input text."""
        self._shell_widget.set_input_text(text)

    def update_status(self) -> None:
        """Update status bar with shell tab information."""
        strings = self._language_manager.strings()
        message = StatusMessage(strings.system_status)  # Using system_status for now
        self.status_message.emit(message)

    def clear_history(self) -> None:
        """Clear the shell history including command history."""
        self._shell_widget.clear_history()
