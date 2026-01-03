from typing import Any, Dict, List
import logging

from PySide6.QtWidgets import (
    QVBoxLayout, QWidget
)
from PySide6.QtCore import QTimer

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
        self._path = path
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
        self._editor_widget.text_changed.connect(self._on_text_changed)
        self._editor_widget.status_updated.connect(self.update_status)
        self._editor_widget.file_saved.connect(self._on_file_saved)
        layout.addWidget(self._editor_widget)

        # Set up debounced search update timer
        self._search_update_timer = QTimer(self)
        self._search_update_timer.setSingleShot(True)
        self._search_update_timer.timeout.connect(self._update_search_after_edit)
        self._search_update_debounce_ms = 100

        # Start file watching if we have a path
        if self._path:
            self._start_file_watching(self._path)

        self.update_status()

    def set_active(self, widget: QWidget, active: bool) -> None:
        """
        Set the active state of the tab.

        Args:
            widget: The widget that triggered the activation change
            active: True if the tab is now active, False otherwise
        """
        if active:
            self.activated.emit()

    def activate(self) -> None:
        """Activate the tab."""
        self._editor_widget.setFocus()

    def _on_content_modified(self, modified: bool) -> None:
        """Handle content modification state changes."""
        self._set_modified(modified)
        self.set_updated(True)

    def _on_text_changed(self) -> None:
        """Handle any text changes in the editor."""
        # If find widget is visible, schedule a search update
        if not self._find_widget.isHidden():
            self._search_update_timer.start(self._search_update_debounce_ms)

    def _update_search_after_edit(self) -> None:
        """Update search results after text has been modified."""
        # Only proceed if find widget is still visible
        if self._find_widget.isHidden():
            return

        search_text = self._find_widget.get_search_text()
        if not search_text:
            return

        # Store current match index before clearing
        current_match, _total_matches = self._editor_widget.get_match_status()
        previous_match_index = current_match - 1 if current_match > 0 else -1  # Convert to 0-based

        # Clear current search state and re-search without moving cursor
        self._editor_widget.clear_find()
        self._editor_widget.find_text(search_text, True, move_cursor=False)

        # Try to restore the same match index if possible
        new_current, new_total = self._editor_widget.get_match_status()
        if new_total > 0 and previous_match_index >= 0:
            # If the previous index is still valid, try to set it
            if previous_match_index < new_total:
                self._editor_widget.set_current_match_index(previous_match_index)
                new_current, new_total = self._editor_widget.get_match_status()

        # Update match count display
        self._find_widget.set_match_status(new_current, new_total)

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

    def _handle_file_changed(self, changed_path: str) -> None:
        """
        Handle notification that the watched file has changed.

        If the tab is not modified, refresh the content from disk and set the updated marker.

        Args:
            changed_path: Path of the file that changed
        """
        # First, let the parent handle file existence checking
        super()._handle_file_changed(changed_path)

        # If the tab is not modified, refresh the content
        if not self.is_modified():
            self._logger.debug("File changed and tab not modified, refreshing content: %s", changed_path)
            self._editor_widget.refresh_content()
            self.set_updated(True)

    def get_state(self, temp_state: bool=False) -> TabState:
        """Get serializable state for mindspace persistence."""
        metadata = self._editor_widget.create_state_metadata(temp_state)

        if temp_state:
            metadata['find_widget'] = self._find_widget.create_state_metadata()

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

            if 'find_widget' in state.metadata:
                tab._find_widget.restore_from_metadata(state.metadata['find_widget'])

        return tab

    def set_path(self, path: str) -> None:
        """
        Set the file being edited.

        Args:
            path: Path to file
        """
        # Stop watching old path
        if self._path != path:
            self._stop_file_watching()

        self._path = path
        self._editor_widget.set_path(path)

        # Start watching new path
        if path:
            self._start_file_watching(path)

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
        self._stop_file_watching()
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
        # Disable file watching during save to avoid false change notifications
        self._stop_file_watching()
        result = self._editor_widget.save_file()
        self._start_file_watching(self._path)
        return result

    def can_save_as(self) -> bool:
        """Check if the file can be saved as."""
        return True

    def save_as(self) -> bool:
        """
        Show save as dialog and save file.

        Returns:
            bool: True if save was successful
        """
        # Disable file watching during save to avoid false change notifications
        self._stop_file_watching()
        result = self._editor_widget.save_file_as()
        self._start_file_watching(self._path)
        return result

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
        self._editor_widget.find_text(text, forward)  # move_cursor defaults to True for user navigation
        current, total = self._editor_widget.get_match_status()
        self._find_widget.set_match_status(current, total)

    def get_text_range(self, start_line: int | None = None, end_line: int | None = None) -> str:
        """
        Get text from editor, optionally limited to line range.

        Args:
            start_line: Starting line number (1-indexed, inclusive), None for start
            end_line: Ending line number (1-indexed, inclusive), None for end

        Returns:
            Text content for the specified range
        """
        return self._editor_widget.get_text_range(start_line, end_line)

    def get_cursor_info(self) -> Dict[str, Any]:
        """Get cursor position and selection information."""
        return self._editor_widget.get_cursor_info()

    def get_editor_info(self) -> Dict[str, Any]:
        """Get editor metadata and document information."""
        return self._editor_widget.get_editor_info()

    def goto_line(self, line: int, column: int = 1) -> None:
        """
        Move cursor to specific line and column.

        Args:
            line: Target line number (1-indexed)
            column: Target column number (1-indexed, default 1)
        """
        self._editor_widget.goto_line(line, column)

    def find_all_occurrences(self, search_text: str, case_sensitive: bool = False) -> List[Dict[str, Any]]:
        """
        Find all occurrences of text in the document.

        Args:
            search_text: Text to search for
            case_sensitive: Whether search should be case-sensitive

        Returns:
            List of match information dictionaries
        """
        return self._editor_widget.find_all_occurrences(search_text, case_sensitive)

    def get_selected_text(self) -> str:
        """
        Get the currently selected text.

        Returns:
            Selected text, or empty string if no selection
        """
        return self._editor_widget.get_selected_text()

    def get_diff(self, context_lines: int = 3) -> str:
        """
        Get unified diff between saved file and current buffer.

        Args:
            context_lines: Number of context lines to include (default 3)

        Returns:
            Unified diff string showing changes, or empty string if no modifications
            exist or file has never been saved
        """
        return self._editor_widget.get_diff(context_lines)

    def apply_diff(self, diff_text: str) -> Dict[str, Any]:
        """
        Apply a unified diff to the editor content.

        Args:
            diff_text: Unified diff format text

        Returns:
            Dictionary with operation result
        """
        return self._editor_widget.apply_unified_diff(diff_text)
