"""
Log tab implementation for Humbug application.

This tab provides a read-only view of the mindspace message log for viewing system interactions.
"""

import logging

from PySide6.QtWidgets import QVBoxLayout, QWidget

from humbug.color_role import ColorRole
from humbug.find_widget import FindWidget
from humbug.language.language_manager import LanguageManager
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.status_message import StatusMessage
from humbug.style_manager import StyleManager
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

        # Install activation tracking
        self._log_widget.activated.connect(self.activated)

        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._handle_language_changed)

        self._style_manager = StyleManager()
        self._style_manager.style_changed.connect(self._handle_style_changed)
        self._handle_style_changed()

    def activate(self) -> None:
        """Activate the tab."""
        self._log_widget.activate()

    def refresh(self) -> None:
        """Refresh log display."""
        self._log_widget.load_messages()

    def _handle_language_changed(self) -> None:
        """Update language-specific elements when language changes."""
        # Update find widget text if visible
        if not self._find_widget.isHidden():
            current, total = self._log_widget.get_match_status()
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
        metadata = self._log_widget.create_state_metadata(temp_state)

        return TabState(
            type=TabType.LOG,
            tab_id=self._tab_id,
            path="",  # Log tab doesn't have a file path
            cursor_position=None,
            horizontal_scroll=None,
            vertical_scroll=None,
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
        pass

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
        pass

    def can_redo(self) -> bool:
        """Check if redo is available."""
        return False

    def redo(self) -> None:
        """Redo not supported for log tab."""
        pass

    def can_cut(self) -> bool:
        """Check if cut is available."""
        return False

    def cut(self) -> None:
        """Cut not supported for log tab."""
        pass

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
        pass

    def can_submit(self) -> bool:
        """Check if message can be submitted."""
        return False

    def submit(self) -> None:
        """Submit not supported for log tab."""
        pass

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

    def _handle_style_changed(self) -> None:
        """Handle style changes."""
        self.setStyleSheet(f"""
            QWidget {{
                background-color: {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
                border-top: 2px solid {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
            }}
        """)

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
