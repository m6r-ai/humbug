"""
System tab implementation for Humbug application.

This tab provides a system-level overview and functionality.
"""

from PySide6.QtWidgets import QVBoxLayout, QWidget

from humbug.gui.color_role import ColorRole
from humbug.gui.status_message import StatusMessage
from humbug.gui.style_manager import StyleManager
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
        Initialize a system tab.

        Args:
            tab_id: Unique identifier for this tab
            parent: Optional parent widget
        """
        super().__init__(tab_id, parent)

        self._style_manager = StyleManager()
        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._handle_language_changed)

        # Set up layout
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        # Create an empty container widget for now
        self._content_widget = QWidget()
        layout.addWidget(self._content_widget)

        # Connect to style changes
        self._style_manager.style_changed.connect(self._handle_style_changed)
        self._handle_style_changed()

        # Setup activation tracking
        self._install_activation_tracking(self._content_widget)

        self._handle_language_changed()

    def _handle_language_changed(self) -> None:
        """Update language-specific elements."""
        self.update_status()

    def _handle_style_changed(self) -> None:
        """Handle style changes from StyleManager."""
        self.setStyleSheet(f"""
            QWidget {{
                background-color: {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
                border-top: 2px solid {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
            }}
        """)

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
        """Check if the tab can be closed."""
        return True

    def close_tab(self) -> None:
        """Handle tab closing."""
        pass

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
        """Undo not applicable for system tab."""
        pass

    def can_redo(self) -> bool:
        """Check if redo is available."""
        return False

    def redo(self) -> None:
        """Redo not applicable for system tab."""
        pass

    def can_cut(self) -> bool:
        """Check if cut is available."""
        return False

    def cut(self) -> None:
        """Cut not applicable for system tab."""
        pass

    def can_copy(self) -> bool:
        """Check if copy is available."""
        return False

    def copy(self) -> None:
        """Copy not applicable for system tab."""
        pass

    def can_paste(self) -> bool:
        """Check if paste is available."""
        return False

    def paste(self) -> None:
        """Paste not applicable for system tab."""
        pass

    def show_find(self) -> None:
        """Show find not applicable for system tab."""
        pass

    def can_submit(self) -> bool:
        """Check if submit is available."""
        return False

    def submit(self) -> None:
        """Submit not applicable for system tab."""
        pass

    def update_status(self) -> None:
        """Update status bar with system tab information."""
        strings = self._language_manager.strings()
        message = StatusMessage(strings.system_status)
        self.status_message.emit(message)
