"""Diff tab implementation."""

import logging
import os

from PySide6.QtWidgets import QVBoxLayout, QWidget

from git import GitNotFoundError, GitNotRepositoryError, find_repo_root

from humbug.language.language_manager import LanguageManager
from humbug.status_message import StatusMessage
from humbug.tabs.tab_base import TabBase
from humbug.tabs.tab_state import TabState
from humbug.tabs.tab_type import TabType
from humbug.style_manager import StyleManager
from humbug.tabs.diff.diff_widget import DiffWidget


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

        self._diff_widget = DiffWidget(path, self)
        self._diff_widget.status_updated.connect(self.update_status)
        layout.addWidget(self._diff_widget)

        if path:
            self._diff_widget.load_diff()
            self._start_file_watching(path)

        self.update_status()

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
        """Return the preferred column width to comfortably display two 80-column panes."""
        style_manager = StyleManager()

        char_width = style_manager.get_space_width()

        # Gutter: (digits + 4) character widths, matching the editor formula.
        # Assume 4-digit line numbers as a reasonable estimate for sizing.
        gutter_width = int((4 + 4) * char_width)

        # Each pane: gutter + 80 columns
        pane_width = gutter_width + int(80 * char_width)

        # Total: two panes + splitter handle (1px) + shared scrollbar
        return 2 * pane_width + 1 + style_manager.get_scrollbar_size()

    def get_state(self, temp_state: bool = False) -> TabState:
        """Return serialisable state for mindspace persistence."""
        return TabState(
            type=TabType.DIFF,
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
        return False

    def copy(self) -> None:
        pass

    def can_paste(self) -> bool:
        return False

    def paste(self) -> None:
        pass

    def can_submit(self) -> bool:
        return False

    def submit(self) -> None:
        pass

    def show_find(self) -> None:
        pass
