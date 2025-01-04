from typing import Dict, Optional

from PySide6.QtWidgets import QFrame
from PySide6.QtCore import Signal

from humbug.gui.tab_state import TabState


class TabBase(QFrame):
    """Base class for all tab content."""

    # Common signals that both chat and editor tabs will need
    close_requested = Signal(str)  # Emits tab_id
    title_changed = Signal(str, str)  # Emits (tab_id, new_title)
    modified_state_changed = Signal(str, bool)  # Emits (tab_id, is_modified)

    def __init__(self, tab_id: str, parent=None):
        """
        Initialize the base tab.

        Args:
            tab_id: Unique identifier for this tab
            parent: Optional parent widget
        """
        super().__init__(parent)
        self._tab_id = tab_id
        self._is_modified = False
        self._path: Optional[str] = None

    @property
    def tab_id(self) -> str:
        """Get the tab's unique identifier."""
        return self._tab_id

    @property
    def path(self) -> Optional[str]:
        """Get the tab's associated file path."""
        return self._path

    @property
    def is_modified(self) -> bool:
        """Check if the tab's content has unsaved modifications."""
        return self._is_modified

    def _set_modified(self, modified: bool) -> None:
        """Update the modified state and emit signal if changed."""
        if modified != self._is_modified:
            self._is_modified = modified
            self.modified_state_changed.emit(self._tab_id, modified)

    def get_state(self) -> TabState:
        """Get serializable state for workspace persistence.

        Must be implemented by subclasses to provide their specific state.

        Returns:
            TabState object containing serializable state
        """
        raise NotImplementedError("Subclasses must implement get_state")

    @classmethod
    def restore_from_state(cls, state: TabState, parent=None) -> 'TabBase':
        """Create and restore a tab from serialized state.

        Must be implemented by subclasses to handle their specific state.

        Args:
            state: TabState object containing serialized state
                (note: state.type will be string, not TabType enum)
            parent: Optional parent widget

        Returns:
            Newly created and restored tab instance

        Raises:
            ValueError: If state is invalid for this tab type
        """
        raise NotImplementedError("Subclasses must implement restore_from_state")

    def set_cursor_position(self, position: Dict[str, int]) -> None:
        """Set the cursor position in the tab's content.

        Args:
            position: Dictionary with cursor position information
        """
        raise NotImplementedError("Subclasses must implement set_cursor_position")

    def get_cursor_position(self) -> Dict[str, int]:
        """Get the current cursor position from the tab's content.

        Returns:
            Dictionary with cursor position information
        """
        raise NotImplementedError("Subclasses must implement get_cursor_position")

    def can_close(self) -> bool:
        """
        Check if the tab can be closed.

        Must be implemented by subclasses.
        """
        raise NotImplementedError("Subclasses must implement can_close")

    def close(self) -> None:
        """
        Close the tab's content if applicable.

        Must be implemented by subclasses.
        """
        raise NotImplementedError("Subclasses must implement save")

    def can_save(self) -> bool:
        """
        Check if the tab can be saved.

        Must be implemented by subclasses.
        """
        raise NotImplementedError("Subclasses must implement can_save")

    def save(self) -> bool:
        """
        Save the tab's content if applicable.

        Must be implemented by subclasses.

        Returns:
            bool: True if save was successful or not needed
        """
        raise NotImplementedError("Subclasses must implement save")

    def can_save_as(self) -> bool:
        """
        Check if the tab can be saved "as".

        Must be implemented by subclasses.
        """
        raise NotImplementedError("Subclasses must implement can_save_as")

    def save_as(self) -> bool:
        """
        Save the tab's content "as" if applicable.

        Must be implemented by subclasses.

        Returns:
            bool: True if save was successful or not needed
        """
        raise NotImplementedError("Subclasses must implement save_as")

    def can_undo(self) -> bool:
        """
        Check if undo operation is available.

        Must be implemented by subclasses.
        """
        raise NotImplementedError("Subclasses must implement can_undo")

    def undo(self) -> None:
        """
        Perform undo operation.

        Must be implemented by subclasses.
        """
        raise NotImplementedError("Subclasses must implement undo")

    def can_redo(self) -> bool:
        """
        Check if redo operation is available.

        Must be implemented by subclasses.
        """
        raise NotImplementedError("Subclasses must implement can_redo")

    def redo(self) -> None:
        """
        Perform redo operation.

        Must be implemented by subclasses.
        """
        raise NotImplementedError("Subclasses must implement redo")

    def can_cut(self) -> bool:
        """
        Check if cut operation is available.

        Must be implemented by subclasses.
        """
        raise NotImplementedError("Subclasses must implement can_cut")

    def cut(self) -> None:
        """
        Perform cut operation.

        Must be implemented by subclasses.
        """
        raise NotImplementedError("Subclasses must implement cut")

    def can_copy(self) -> bool:
        """
        Check if copy operation is available.

        Must be implemented by subclasses.
        """
        raise NotImplementedError("Subclasses must implement can_copy")

    def copy(self) -> None:
        """
        Perform copy operation.

        Must be implemented by subclasses.
        """
        raise NotImplementedError("Subclasses must implement copy")

    def can_paste(self) -> bool:
        """
        Check if paste operation is available.

        Must be implemented by subclasses.
        """
        raise NotImplementedError("Subclasses must implement can_paste")

    def paste(self) -> None:
        """
        Perform paste operation.

        Must be implemented by subclasses.
        """
        raise NotImplementedError("Subclasses must implement paste")

    def can_submit(self) -> bool:
        """
        Check if submit operation is available.

        Must be implemented by subclasses.
        """
        raise NotImplementedError("Subclasses must implement can_submit")
