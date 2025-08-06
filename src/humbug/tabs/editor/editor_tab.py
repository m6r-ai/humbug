import logging

from PySide6.QtWidgets import (
    QVBoxLayout, QWidget
)

from humbug.language.language_manager import LanguageManager
from humbug.status_message import StatusMessage
from humbug.tabs.editor.editor_widget import EditorWidget
from humbug.tabs.find_widget import FindWidget
from humbug.tabs.tab_base import TabBase
from humbug.tabs.tab_state import TabState
from humbug.tabs.tab_type import TabType


class EditorTab(TabBase):
    """Tab for editing text files."""

    def __init__(self, tab_id: str, path: str, untitled_number: int | None, parent: QWidget | None = None) -> None:
        """
        Initialize editor tab.

        Args:
            tab_id: Unique identifier for this tab, or a UUID will be generated if not provided
            path: File path to edit, or empty string for new file
            untitled_number: Optional untitled file number for new files
            parent: Optional parent widget
        """
        super().__init__(tab_id, parent)
        self._logger = logging.getLogger("EditorTab")
        self._untitled_number = untitled_number

        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._on_language_changed)

        # Set up layout
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

        # Create editor widget
        self._editor_widget = EditorWidget(path, untitled_number, self)
        self._editor_widget.content_modified.connect(self._on_content_modified)
        self._editor_widget.status_updated.connect(self.update_status)
        self._editor_widget.activated.connect(self.activated)
        self._editor_widget.file_saved.connect(self._on_file_saved)
        layout.addWidget(self._editor_widget)

        self.update_status()

    def activate(self) -> None:
        """Activate the tab."""
        self._editor_widget.setFocus()

    def _on_content_modified(self, modified: bool) -> None:
        """Handle content modification changes."""
        self._set_modified(modified)

    def _on_file_saved(self, path: str) -> None:
        """Handle file being saved."""
        self.set_path(path)
        self.update_status()

    def _on_language_changed(self) -> None:
        """Update language-specific elements."""
        # Update find widget text if visible
        if not self._find_widget.isHidden():
            current, total = self._editor_widget.get_match_status()
            self._find_widget.set_match_status(current, total)

        # Update status bar with translated terms
        self.update_status()

    def get_state(self, temp_state: bool=False) -> TabState:
        """Get serializable state for mindspace persistence."""
        metadata = self._editor_widget.create_state_metadata(temp_state)

        path = self._editor_widget.path()
        if not path and self._untitled_number:
            path = f"untitled-{self._untitled_number}"

        return TabState(
            type=TabType.EDITOR,
            tab_id=self._tab_id,
            path=path,
            metadata=metadata
        )

    @classmethod
    def restore_from_state(cls, state: TabState, parent: QWidget) -> 'EditorTab':
        """Create and restore an editor tab from serialized state."""
        # Set filename and load content
        number: int | None = None
        path = ""

        if state.path.startswith("untitled-"):
            number = int(state.path.split("-")[1])

        else:
            path = state.path

        # Create new tab instance
        tab = cls(state.tab_id, path, number, parent)

        if state.metadata:
            tab._editor_widget.restore_from_metadata(state.metadata)

        return tab

    def set_path(self, path: str) -> None:
        """
        Set the file being edited.

        Args:
            path: Path to file
        """
        self._path = path
        self.update_status()

    def update_status(self) -> None:
        """Update status bar with current cursor position."""
        status_info = self._editor_widget.get_status_info()
        strings = self._language_manager.strings()
        message = StatusMessage(
            strings.editor_status.format(**status_info)
        )
        self.status_message.emit(message)

    def can_close_tab(self) -> bool:
        """Check if the file can be closed."""
        return self._editor_widget.can_close()

    def close_tab(self) -> None:
        """Close the tab and clean up resources."""
        self._editor_widget.close_widget()

    def can_save(self) -> bool:
        """Check if the file can be saved."""
        return self._editor_widget.is_modified()

    def save(self) -> bool:
        """
        Save the current file.

        Returns:
            bool: True if save was successful
        """
        return self._editor_widget.save_file()

    def can_save_as(self) -> bool:
        """Check if the file can be saved as."""
        return True

    def save_as(self) -> bool:
        """
        Show save as dialog and save file.

        Returns:
            bool: True if save was successful
        """
        return self._editor_widget.save_file_as()

    def can_undo(self) -> bool:
        """Check if undo is available."""
        return self._editor_widget.can_undo()

    def undo(self) -> None:
        """Undo the last edit operation."""
        self._editor_widget.undo()

    def can_redo(self) -> bool:
        """Check if redo is available."""
        return self._editor_widget.can_redo()

    def redo(self) -> None:
        """Redo the last undone edit operation."""
        self._editor_widget.redo()

    def can_cut(self) -> bool:
        """Check if cut is available."""
        return self._editor_widget.can_cut()

    def cut(self) -> None:
        """Cut selected text to clipboard."""
        self._editor_widget.cut()

    def can_copy(self) -> bool:
        """Check if copy is available."""
        return self._editor_widget.can_copy()

    def copy(self) -> None:
        """Copy selected text to clipboard."""
        self._editor_widget.copy()

    def can_paste(self) -> bool:
        """Check if paste is available."""
        return self._editor_widget.can_paste()

    def paste(self) -> None:
        """Paste text from clipboard."""
        self._editor_widget.paste()

    def can_submit(self) -> bool:
        """Check if submit is available (not applicable for editor)."""
        return False

    def submit(self) -> None:
        """Submit operation (not applicable for editor)."""

    def show_find(self) -> None:
        """Show the find widget."""
        cursor = self._editor_widget.textCursor()
        if cursor.hasSelection():
            # Get the selected text
            text = cursor.selectedText()

            # Only use selection if it's on a single line
            if '\u2029' not in text:  # Qt uses this character for line breaks
                self._find_widget.set_search_text(text)
            else:
                self._find_widget.set_search_text("")

        self._find_widget.show()
        self._find_widget.setFocus()

    def _close_find(self) -> None:
        """Close the find widget and clear search state."""
        self._find_widget.hide()
        self._editor_widget.clear_find()

    def _find_next(self, forward: bool = True) -> None:
        """Find next/previous match."""
        text = self._find_widget.get_search_text()
        self._editor_widget.find_text(text, forward)
        current, total = self._editor_widget.get_match_status()
        self._find_widget.set_match_status(current, total)
