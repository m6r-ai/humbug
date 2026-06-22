from typing import Any, Dict, List
import logging
import os

from PySide6.QtWidgets import (
    QHBoxLayout, QSizePolicy, QVBoxLayout, QWidget
)
from PySide6.QtCore import Qt, QTimer, QRegularExpression

from context.context_registry import ContextRegistry
from editor_context.editor_context import EditorContext

from desktop.editor_tab.editor_goto_line_dialog import EditorGotoLineDialog
from desktop.language.language_manager import LanguageManager
from desktop.status_message import StatusMessage
from desktop.style_manager import StyleManager
from desktop.color_role import ColorRole
from desktop.editor_tab.editor_widget import EditorWidget
from desktop.widgets import FindWidget
from desktop.tab import TabBase, TabState


class EditorTab(TabBase):
    """Tab for editing text files."""

    _untitled_counter: int = 0

    @classmethod
    def next_untitled_title(cls) -> str:
        """Allocate and return the next untitled tab title."""
        cls._untitled_counter += 1
        return f"Untitled-{cls._untitled_counter}"

    def __init__(self, tab_id: str, path: str, untitled_number: int | None = None, parent: QWidget | None = None) -> None:
        """
        Initialize editor tab.

        Args:
            tab_id: Unique identifier for this tab, or a UUID will be generated if not provided
            path: File path to edit, or empty string for new file
            untitled_number: Untitled file number for pathless tabs; only passed when restoring from state
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
        self._find_widget = FindWidget(self)
        self._find_widget.hide()
        self._find_widget.enable_replace_row()
        self._find_widget.closed.connect(self._close_find)
        self._find_widget.search_changed.connect(self._on_search_changed)
        self._find_widget.find_next.connect(lambda: self._find_next(True))
        self._find_widget.find_previous.connect(lambda: self._find_next(False))
        self._find_widget.replace_current.connect(self._replace_current)
        self._find_widget.replace_all.connect(self._replace_all)
        layout.addWidget(self._find_widget)

        # Create editor widget
        self._editor_widget = EditorWidget(path, untitled_number, self)
        self._editor_widget.content_modified.connect(self._on_content_modified)
        self._editor_widget.text_changed.connect(self._on_text_changed)
        self._editor_widget.status_updated.connect(self.update_status)
        self._editor_widget.file_saved.connect(self._on_file_saved)

        editor_container = QWidget()
        editor_container_layout = QHBoxLayout(editor_container)
        editor_container_layout.setContentsMargins(0, 0, 0, 0)
        editor_container_layout.setSpacing(0)
        editor_container_layout.setAlignment(Qt.AlignmentFlag.AlignHCenter)
        self._editor_widget.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)
        editor_container_layout.addWidget(self._editor_widget)
        layout.addWidget(editor_container)

        # Build the EditorContext backed by the widget's document
        self._editor_context: EditorContext | None = EditorContext(
            context_id=tab_id,
            document=self._editor_widget.document(),
            get_cursor_info_cb=self._editor_widget.get_cursor_info,
            get_selected_text_cb=self._editor_widget.get_selected_text,
            get_editor_info_cb=self._editor_widget.get_editor_info,
            save_cb=self._editor_widget.save_file,
            on_goto_line=self._editor_widget.goto_line,
            on_apply_diff=self._editor_widget.apply_unified_diff,
        )

        # Set up debounced search update timer
        self._search_update_timer = QTimer(self)
        self._search_update_timer.setSingleShot(True)
        self._search_update_timer.timeout.connect(self._update_search_after_edit)
        self._search_update_debounce_ms = 100

        # Start file watching if we have a path
        if self._path:
            self._start_file_watching(self._path)

        self.apply_style()
        self.update_status()

    def tool_name(self) -> str:
        """Return the tool name for this tab type."""
        return "editor"

    def on_modified_changed(self, modified: bool) -> None:
        """Update the tab bar label to show or hide the unsaved-changes marker."""
        self._update_tab_label()

    def on_path_renamed(self, new_path: str) -> None:
        """Update path and emit a new tab bar label after a file rename."""
        self.set_path(new_path)
        self._update_tab_label()

    def _update_tab_label(self) -> None:
        """Compute the current tab label from path and modified state, and emit tab_label_changed."""
        title = os.path.basename(self._path) if self._path else (
            f"Untitled-{self._untitled_number}" if self._untitled_number else "Untitled"
        )
        if self.is_modified():
            title += "*"

        self.tab_label_changed.emit(self._tab_id, title)

    def register_context_models(self, registry: ContextRegistry) -> None:
        """Register the EditorContext with the registry."""
        if self._editor_context is not None:
            registry.register_model(self._tab_id, self._editor_context)

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
        if self._find_widget.isHidden():
            return

        self._editor_widget.refresh_find()

        new_current, new_total, new_truncated = self._editor_widget.get_match_status()
        self._find_widget.set_match_status(new_current, new_total, new_truncated)

    def _on_file_saved(self, path: str) -> None:
        """Handle file being saved."""
        self.set_path(path)
        self._update_tab_label()

    def _on_language_changed(self) -> None:
        """Update language-specific elements."""
        # Update find widget text if visible
        if not self._find_widget.isHidden():
            current, total, truncated = self._editor_widget.get_match_status()
            self._find_widget.set_match_status(current, total, truncated)

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
            type=self.tool_name(),
            tab_id=self._tab_id,
            path=path,
            metadata=metadata
        )

    def get_editor_context(self) -> EditorContext | None:
        """
        Return the EditorContext for this tab.

        Returns:
            The EditorContext, or None if not yet initialised.
        """
        return self._editor_context

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
        # If the path is the same as current, do nothing
        if self._path == path:
            return

        # Stop watching old path
        self.stop_file_watching()

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
        self.stop_file_watching()
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
        self.stop_file_watching()
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
        self.stop_file_watching()
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

        self._find_widget.show_find()

    def can_show_find_replace(self) -> bool:
        """Return True since editor tabs support find-and-replace."""
        return True

    def show_find_replace(self) -> None:
        """Show the find widget with the replace row expanded."""
        cursor = self._editor_widget.textCursor()
        if cursor.hasSelection():
            text = cursor.selectedText()
            if '\u2029' not in text:
                self._find_widget.set_search_text(text)

            else:
                self._find_widget.set_search_text("")

        self._find_widget.show_replace()

    def can_show_goto_line(self) -> bool:
        """Return True since editor tabs support go-to-line."""
        return True

    def show_goto_line(self) -> None:
        """Show the go-to-line dialog and navigate if accepted."""
        cursor_info = self._editor_widget.get_cursor_info()
        current_line = cursor_info.get('line', 1)
        editor_info = self._editor_widget.get_editor_info()
        max_line = editor_info.get('line_count', 1)
        dialog = EditorGotoLineDialog(max_line, current_line, self)
        if dialog.exec() == EditorGotoLineDialog.DialogCode.Accepted:
            self._editor_widget.goto_line(dialog.line_number())

    def apply_find_search(self, text: str, case_sensitive: bool = False, regexp: bool = False) -> None:
        """Apply a programmatic find/highlight request to the editor."""
        self._find_widget.set_case_sensitive(case_sensitive)
        self._find_widget.set_regexp(regexp)
        self._find_widget.set_search_text(text)
        self._find_widget.show()
        self._on_search_changed()
        self._find_widget.setFocus()

    def apply_search_highlight(self, text: str, case_sensitive: bool = False, regexp: bool = False) -> None:
        """Apply a transient search highlight without altering the local find widget."""
        self._editor_widget.find_text(text, forward=True, move_cursor=False, case_sensitive=case_sensitive, regexp=regexp)

    def navigate_to_search_match(
        self, text: str, line_number: int | None, message_id: str | None, case_sensitive: bool = False, regexp: bool = False
    ) -> None:
        """Close any active find, then highlight all matches and scroll to the match at line_number."""
        self._close_find()
        if line_number is not None:
            self._editor_widget.find_text_at_line(text, line_number, case_sensitive=case_sensitive, regexp=regexp)
        else:
            self._editor_widget.find_text(text, forward=True, move_cursor=True, case_sensitive=case_sensitive, regexp=regexp)

    def clear_search_highlight(self) -> None:
        """Clear transient search highlights."""
        self._editor_widget.clear_highlights()

    def _close_find(self) -> None:
        """Close the find widget and clear search state."""
        self._find_widget.hide()
        self._editor_widget.clear_highlights()

    def _find_next(self, forward: bool = True) -> None:
        """Find next/previous match."""
        text, case_sensitive, regexp = self._find_widget.current_search_request()
        if regexp:
            if text and not QRegularExpression(text).isValid():
                self._find_widget.set_invalid_regexp()
                return

        self._editor_widget.find_text(text, forward, case_sensitive=case_sensitive, regexp=regexp)
        current, total, truncated = self._editor_widget.get_match_status()
        self._find_widget.set_match_status(current, total, truncated)

    def _on_search_changed(self) -> None:
        """Handle search text or mode changes - update matches without navigating."""
        text, case_sensitive, regexp = self._find_widget.current_search_request()
        if regexp:
            if text and not QRegularExpression(text).isValid():
                self._find_widget.set_invalid_regexp()
                return

        self._editor_widget.find_text(text, forward=True, move_cursor=False, case_sensitive=case_sensitive, regexp=regexp)
        current, total, truncated = self._editor_widget.get_match_status()
        self._find_widget.set_match_status(current, total, truncated)

    def _replace_current(self, replace_text: str) -> None:
        """Replace the current match and advance to the next."""
        self._editor_widget.replace_current(replace_text)
        self._search_update_timer.stop()
        current, total, truncated = self._editor_widget.get_match_status()
        self._find_widget.set_match_status(current, total, truncated)

    def _replace_all(self, replace_text: str) -> None:
        """Replace all matches and update the status display."""
        count = self._editor_widget.replace_all(replace_text)
        self._search_update_timer.stop()
        if count > 0:
            strings = self._language_manager.strings()
            status_text = strings.replace_count.format(count=count)
            self._find_widget.set_match_status(0, 0)
            self._find_widget.set_status_text(status_text)

        else:
            self._find_widget.set_match_status(0, 0)

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

    def find_all_occurrences(self, search_text: str, case_sensitive: bool = False, regexp: bool = False) -> List[Dict[str, Any]]:
        """
        Find all occurrences of text in the document.

        Args:
            search_text: Text to search for
            case_sensitive: Whether search should be case-sensitive
            regexp: If True, treat search_text as a regular expression.

        Returns:
            List of match information dictionaries
        """
        return self._editor_widget.find_all_occurrences(search_text, case_sensitive, regexp)

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

    def apply_style(self) -> None:
        """Apply current style settings to the tab's content widgets."""
        self._find_widget.apply_style()
        style_manager = StyleManager()
        self._editor_widget.setMaximumWidth(style_manager.scaled_tab_width())
        self._editor_widget.apply_style()

        new_stylesheet = self._build_stylesheet()
        if new_stylesheet != self.styleSheet():
            self.setStyleSheet(new_stylesheet)

    def _build_stylesheet(self) -> str:
        """Build the stylesheet for this tab."""
        style_manager = StyleManager()
        return f"""
            #EditorWidget {{
                background-color: {style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
            }}

            {style_manager.get_menu_stylesheet()}

            QPlainTextEdit {{
                border: none;
                selection-background-color: {style_manager.get_color_str(ColorRole.TEXT_SELECTED)};
                selection-color: none;
            }}

            {style_manager.get_scrollbar_stylesheet()}

            QAbstractScrollArea::corner {{
                background-color: {style_manager.get_color_str(ColorRole.SCROLLBAR_BACKGROUND)};
            }}
        """

    def preferred_width(self) -> int | None:
        """Return the preferred column width for the editor tab."""
        style_manager = StyleManager()
        return style_manager.scaled_tab_width()
