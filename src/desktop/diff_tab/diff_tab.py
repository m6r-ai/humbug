"""Diff tab implementation."""

import logging
import os

from PySide6.QtWidgets import QVBoxLayout, QWidget
from PySide6.QtCore import QRegularExpression

from editor_context.editor_context import EditorContext
from git import GitNotFoundError, GitNotRepositoryError, find_repo_root

from desktop.diff_tab.diff_widget import DiffWidget
from desktop.language.language_manager import LanguageManager
from desktop.mindspace.mindspace_manager import MindspaceManager
from desktop.mindspace.mindspace_vcs_poller import MindspaceVCSPoller
from desktop.status_message import StatusMessage
from desktop.tab import TabBase, TabState
from desktop.style_manager import StyleManager
from desktop.widgets import FindWidget


class DiffTab(TabBase):
    """Tab showing a side-by-side diff between the working tree and HEAD for one file."""

    def __init__(self, tab_id: str, path: str, parent: QWidget | None = None) -> None:
        """
        Initialise the diff tab.

        Args:
            tab_id: Unique identifier for this tab.  A UUID is generated if empty.
            path: Absolute path to the file to diff.
            parent: Optional parent widget.
        """
        super().__init__(tab_id, parent)
        self._logger = logging.getLogger("DiffTab")
        self._path = path

        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._on_language_changed)

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

        self._diff_widget = DiffWidget(path, self)
        self._diff_widget.status_updated.connect(self.update_status)
        self._diff_widget.open_in_editor_requested.connect(self._open_in_editor)
        self._diff_widget.open_in_preview_requested.connect(self._open_in_preview)
        layout.addWidget(self._diff_widget)

        if path:
            self._diff_widget.load_diff(initial_load=True)
            self._start_file_watching(path)

        self._vcs_poller = MindspaceVCSPoller()
        self._vcs_poller.status_changed.connect(self._on_vcs_status_changed)

        self.update_status()

    def _open_in_editor(self, line: int, column: int) -> None:
        """Open this tab's file in an editor tab, navigating to the given line and column."""
        mindspace_manager = MindspaceManager()
        if not mindspace_manager.has_mindspace():
            return

        contexts = mindspace_manager.mindspace().contexts()
        existing = contexts.get_by_path_and_type(self._path, "editor")
        if existing:
            contexts.focus(existing.context_id)
            editor_context = contexts.get_model(existing.context_id, EditorContext)
            if editor_context is not None:
                editor_context.goto_line(line, column)

        else:
            contexts.open(
                context_type="editor",
                path=self._path,
                title=os.path.basename(self._path),
                initial_model=(line, column),
            )

    def _open_in_preview(self) -> None:
        """Open this tab's file in a preview tab."""
        mindspace_manager = MindspaceManager()
        if not mindspace_manager.has_mindspace():
            return

        contexts = mindspace_manager.mindspace().contexts()
        existing = contexts.get_by_path_and_type(self._path, "preview")
        if existing:
            contexts.focus(existing.context_id)

        else:
            contexts.open(
                context_type="preview",
                path=self._path,
                title=os.path.basename(self._path),
            )

    def tool_name(self) -> str:
        """Return the tool name for this tab type."""
        return "diff"

    def set_active(self, widget: QWidget, active: bool) -> None:
        """
        Set the active state of the tab.

        Args:
            widget: Widget that triggered the change.
            active: True if the tab is now active.
        """
        if active:
            self.activated.emit()

    def activate(self) -> None:
        """Activate the tab."""
        self._diff_widget.setFocus()

    def _handle_file_changed(self, changed_path: str) -> None:
        """
        Refresh the diff when the watched file changes on disk.

        Args:
            changed_path: Path of the file that changed.
        """
        super()._handle_file_changed(changed_path)
        self._diff_widget.refresh()
        self.set_updated(True)

    def _on_vcs_status_changed(self, _status: list) -> None:
        """
        Refresh the diff when the VCS status of the repository changes.

        A commit advances HEAD without touching the working-tree file, so the
        file watcher never fires.  Subscribing to the VCS poller ensures we
        re-run the diff (and show the 'no differences' message) after a commit.

        Args:
            _status: The new list of VCSFileStatus entries (unused; we always
                refresh so the widget can re-evaluate the diff from scratch).
        """
        self._diff_widget.refresh()
        self.set_updated(True)

    def _on_language_changed(self) -> None:
        """Update the status bar when the UI language changes."""
        self.update_status()

    def set_path(self, path: str) -> None:
        """
        Change the file being diffed.

        Args:
            path: New absolute file path.
        """
        if self._path == path:
            return

        self.stop_file_watching()
        self._path = path

        if path:
            self._diff_widget.load_diff()
            self._start_file_watching(path)

        self.update_status()

    def update_status(self) -> None:
        """Emit a status-bar message reflecting the current diff state."""
        strings = self._language_manager.strings()
        message = StatusMessage(
            strings.diff_status.format(
                path=os.path.basename(self._path) if self._path else "",
                rows=self._diff_widget.row_count(),
            )
        )
        self.status_message.emit(message)

    def preferred_width(self) -> int | None:
        """Return the preferred column width: twice the default editor column width."""
        style_manager = StyleManager()
        return int(style_manager.nice_tab_width() * style_manager.zoom_factor() * 2)

    def get_state(self, temp_state: bool = False) -> TabState:
        """Return serialisable state for mindspace persistence."""
        return TabState(
            type=self.tool_name(),
            tab_id=self._tab_id,
            path=self._path,
            metadata=None,
            is_ephemeral=self._is_ephemeral,
        )

    @classmethod
    def restore_from_state(cls, state: TabState, parent: QWidget) -> 'DiffTab':
        """Create and restore a diff tab from serialised state."""
        return cls(state.tab_id, state.path, parent)

    @staticmethod
    def can_restore(path: str) -> bool:
        """
        Return True if a diff tab for *path* can meaningfully be restored.

        A diff tab is only useful when the file exists on disk and lives inside
        a git repository.  If either condition fails the tab would open showing
        nothing but an error message, so callers should skip it on restore.

        Args:
            path: Absolute path to the file that would be diffed.

        Returns:
            True if the file exists and is inside a git repository.
        """
        if not os.path.exists(path):
            return False

        try:
            find_repo_root(path)
            return True

        except (GitNotFoundError, GitNotRepositoryError):
            return False

    def can_close_tab(self) -> bool:
        """Diff tabs can always be closed."""
        return True

    def close_tab(self) -> None:
        """Stop file watching when the tab is closed."""
        self.stop_file_watching()
        self._vcs_poller.status_changed.disconnect(self._on_vcs_status_changed)

    def can_save(self) -> bool:
        return False

    def save(self) -> bool:
        return True

    def can_save_as(self) -> bool:
        return False

    def save_as(self) -> bool:
        return True

    def can_undo(self) -> bool:
        return False

    def undo(self) -> None:
        pass

    def can_redo(self) -> bool:
        return False

    def redo(self) -> None:
        pass

    def can_cut(self) -> bool:
        return False

    def cut(self) -> None:
        pass

    def can_copy(self) -> bool:
        return bool(self._diff_widget.get_selected_text())

    def copy(self) -> None:
        self._diff_widget.copy()

    def can_paste(self) -> bool:
        return False

    def paste(self) -> None:
        pass

    def can_submit(self) -> bool:
        return False

    def submit(self) -> None:
        pass

    def show_find(self) -> None:
        """Show the find widget."""
        text = self._diff_widget.get_selected_text()
        if text and '\n' not in text:
            self._find_widget.set_search_text(text)

        else:
            self._find_widget.set_search_text("")

        self._find_widget.show()
        self._find_widget.setFocus()

    def apply_find_search(self, text: str, case_sensitive: bool = False, regexp: bool = False) -> None:
        """Apply a programmatic find/highlight request to the diff tab."""
        self._find_widget.set_case_sensitive(case_sensitive)
        self._find_widget.set_regexp(regexp)
        self._find_widget.set_search_text(text)
        self._find_widget.show()
        self._find_next(True)
        self._find_widget.setFocus()

    def apply_search_highlight(self, text: str, case_sensitive: bool = False, regexp: bool = False) -> None:
        """Apply a transient search highlight without altering the local find widget."""
        self._diff_widget.find_text(text, True, case_sensitive=case_sensitive, regexp=regexp)

    def clear_search_highlight(self) -> None:
        """Clear transient search highlights."""
        self._diff_widget.clear_highlights()

    def _close_find(self) -> None:
        """Close the find widget and clear search highlights."""
        self._find_widget.hide()
        self._diff_widget.clear_highlights()

    def _find_next(self, forward: bool = True) -> None:
        """Find the next or previous match and update the status label.

        Args:
            forward: If True search forward; if False search backward.
        """
        text, case_sensitive, regexp = self._find_widget.current_search_request()
        if regexp:
            if text and not QRegularExpression(text).isValid():
                self._find_widget.set_invalid_regexp()
                return

        current, total, truncated = self._diff_widget.find_text(text, forward, case_sensitive=case_sensitive, regexp=regexp)
        self._find_widget.set_match_status(current, total, truncated)

    def _on_search_changed(self) -> None:
        """Handle search text or mode changes - update matches without navigating."""
        text, case_sensitive, regexp = self._find_widget.current_search_request()
        if regexp:
            if text and not QRegularExpression(text).isValid():
                self._find_widget.set_invalid_regexp()
                return

        current, total, truncated = self._diff_widget.find_text(text, True, case_sensitive=case_sensitive, regexp=regexp)
        self._find_widget.set_match_status(current, total, truncated)

    def is_navigating_as_hunks(self) -> bool:
        """Return True — diff tabs navigate by hunks rather than messages."""
        return True

    def can_navigate_next_message(self) -> bool:
        """Return True if there is a hunk after the current scroll position."""
        return self._diff_widget.can_navigate_next_hunk()

    def navigate_next_message(self) -> None:
        """Scroll to the next hunk."""
        self._diff_widget.navigate_next_hunk(forward=True)

    def can_navigate_previous_message(self) -> bool:
        """Return True if there is a hunk before the current scroll position."""
        return self._diff_widget.can_navigate_previous_hunk()

    def navigate_previous_message(self) -> None:
        """Scroll to the previous hunk."""
        self._diff_widget.navigate_next_hunk(forward=False)
