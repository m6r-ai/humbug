"""
System tab implementation for Humbug application.

This tab provides a system-level overview and functionality.
"""

import logging

from PySide6.QtWidgets import QVBoxLayout, QWidget

from humbug.gui.color_role import ColorRole
from humbug.gui.find_widget import FindWidget
from humbug.gui.status_message import StatusMessage
from humbug.gui.style_manager import StyleManager
from humbug.gui.tab.system.system_widget import SystemWidget
from humbug.gui.tab.tab_base import TabBase
from humbug.gui.tab.tab_state import TabState
from humbug.gui.tab.tab_type import TabType
from humbug.language.language_manager import LanguageManager


class SystemTab(TabBase):
    """
    System tab for Humbug application.

    This tab provides a system-level overview and functionality.
    """

    def __init__(self, tab_id: str, parent: QWidget | None = None) -> None:
        """
        Initialize the unified system tab.

        Args:
            tab_id: Unique identifier for this tab
            parent: Optional parent widget
        """
        super().__init__(tab_id, parent)
        self._logger = logging.getLogger("SystemTab")

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

        # Create system widget
        self._system_widget = SystemWidget(self)
        self._system_widget.status_updated.connect(self.update_status)
        layout.addWidget(self._system_widget)

        # Install activation tracking
        self._install_activation_tracking(self._system_widget)
        self._system_widget.activated.connect(self.activated)

        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._handle_language_changed)

        self._style_manager = StyleManager()
        self._style_manager.style_changed.connect(self._handle_style_changed)
        self._handle_style_changed()

    def _handle_language_changed(self) -> None:
        """Update language-specific elements when language changes."""
        # Update find widget text if visible
        if not self._find_widget.isHidden():
            current, total = self._system_widget.get_match_status()
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
        return TabState(
            type=TabType.SYSTEM,
            tab_id=self._tab_id,
            path="",
            cursor_position=None,
            horizontal_scroll=None,
            vertical_scroll=None,
            metadata=None
        )

    @classmethod
    def restore_from_state(cls, state: TabState, parent: QWidget | None = None) -> 'SystemTab':
        """
        Create and restore a tab from serialized state.

        Args:
            state: TabState object containing serialized state
            parent: Optional parent widget

        Returns:
            Restored SystemTab instance
        """
        return cls(state.tab_id, parent)

    def can_close_tab(self) -> bool:
        """Check if system can be closed."""
        return True

    def close_tab(self) -> None:
        """Handle tab closing."""

    def can_save(self) -> bool:
        """Check if the tab can be saved."""
        return False

    def save(self) -> bool:
        """Save is not applicable for system tab."""
        return True

    def can_save_as(self) -> bool:
        """Check if the tab can be saved as."""
        return False

    def save_as(self) -> bool:
        """Save as is not applicable for system tab."""
        return True

    def can_undo(self) -> bool:
        """Check if undo is available."""
        return False

    def undo(self) -> None:
        """Undo not supported for systems."""

    def can_redo(self) -> bool:
        """Check if redo is available."""
        return False

    def redo(self) -> None:
        """Redo not supported for systems."""

    def can_cut(self) -> bool:
        """Check if cut is available."""
        return self._system_widget.can_cut()

    def cut(self) -> None:
        """Cut selected text to clipboard."""
        self._system_widget.cut()

    def can_copy(self) -> bool:
        """Check if copy is available."""
        return self._system_widget.can_copy()

    def copy(self) -> None:
        """Copy selected text to clipboard."""
        self._system_widget.copy()

    def can_paste(self) -> bool:
        """Check if paste is available."""
        return self._system_widget.can_paste()

    def paste(self) -> None:
        """Paste text from clipboard."""
        self._system_widget.paste()

    def can_submit(self) -> bool:
        """Check if message can be submitted."""
        return self._system_widget.can_submit()

    def submit(self) -> None:
        """Submit the current message."""
        self._system_widget.submit()

    def show_find(self) -> None:
        """Show the find widget."""
        # Get selected text if any
        if self._system_widget.has_selection():
            selected_text = self._system_widget.get_selected_text()
            if selected_text:
                self._find_widget.set_search_text(selected_text)
            else:
                self._find_widget.set_search_text("")

        self._find_widget.show()
        self._find_widget.setFocus()

    def _close_find(self) -> None:
        """Close the find widget and clear search state."""
        self._find_widget.hide()
        self._system_widget.clear_find()

    def _find_next(self, forward: bool = True) -> None:
        """Find next/previous match."""
        text = self._find_widget.get_search_text()
        current, total = self._system_widget.find_text(text, forward)
        self._find_widget.set_match_status(current, total)

    def _handle_style_changed(self) -> None:
        """Handle style changes."""
        self.setStyleSheet(f"""
            QWidget {{
                background-color: {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
                border-top: 2px solid {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
            }}
        """)

    def cancel_current_tasks(self) -> None:
        """Cancel any ongoing AI response tasks."""
        self._system_widget.cancel_current_tasks()

    def can_navigate_next_message(self) -> bool:
        """Check if navigation to next message is possible."""
        return self._system_widget.can_navigate_next_message()

    def navigate_next_message(self) -> None:
        """Navigate to the next message."""
        self._system_widget.navigate_to_next_message()

    def can_navigate_previous_message(self) -> bool:
        """Check if navigation to previous message is possible."""
        return self._system_widget.can_navigate_previous_message()

    def navigate_previous_message(self) -> None:
        """Navigate to the previous message."""
        self._system_widget.navigate_to_previous_message()

    def set_input_text(self, text: str) -> None:
        """Set the input text."""
        self._system_widget.set_input_text(text)

    def update_status(self) -> None:
        """Update status bar with system tab information."""
        strings = self._language_manager.strings()
        message = StatusMessage(strings.system_status)
        self.status_message.emit(message)
