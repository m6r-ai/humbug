"""Revision control status view for the mindspace sidebar."""

import os

from PySide6.QtCore import Qt, Signal
from PySide6.QtGui import QColor
from PySide6.QtWidgets import (
    QListWidget, QListWidgetItem, QVBoxLayout, QWidget, QLabel
)

from git import VcsFileStatus, VcsStatusCode

from humbug.color_role import ColorRole
from humbug.language.language_manager import LanguageManager
from humbug.mindspace.mindspace_collapsible_header import MindspaceCollapsibleHeader
from humbug.mindspace.vcs.mindspace_vcs_poller import MindspaceVcsPoller
from humbug.style_manager import StyleManager


_STATUS_LABELS: dict[VcsStatusCode, str] = {
    VcsStatusCode.MODIFIED: "M",
    VcsStatusCode.ADDED: "A",
    VcsStatusCode.DELETED: "D",
    VcsStatusCode.RENAMED: "R",
    VcsStatusCode.COPIED: "C",
    VcsStatusCode.UNTRACKED: "?",
    VcsStatusCode.UNKNOWN: "!",
}


class MindspaceVcsView(QWidget):
    """Sidebar panel showing VCS-modified files for the current mindspace."""

    file_opened_in_diff = Signal(str, bool)   # path, ephemeral
    toggled = Signal(bool)                    # expanded state

    def __init__(self, parent: QWidget | None = None) -> None:
        """Initialise the VCS view."""
        super().__init__(parent)

        self._style_manager = StyleManager()
        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._on_language_changed)

        self._mindspace_path: str = ""
        self._current_status: list[VcsFileStatus] = []

        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        self._header = MindspaceCollapsibleHeader(
            self._language_manager.strings().mindspace_vcs,
            self
        )
        self._header.setProperty("splitter", True)
        self._header.toggled.connect(self._on_header_toggled)
        layout.addWidget(self._header)

        self._no_repo_label = QLabel(
            self._language_manager.strings().mindspace_vcs_no_repo
        )
        self._no_repo_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self._no_repo_label.setObjectName("_no_repo_label")
        layout.addWidget(self._no_repo_label)

        self._list_widget = QListWidget()
        self._list_widget.setObjectName("_list_widget")
        self._list_widget.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self._list_widget.itemClicked.connect(self._on_item_clicked)
        self._list_widget.itemActivated.connect(self._on_item_activated)
        layout.addWidget(self._list_widget)

        self._poller = MindspaceVcsPoller()
        self._poller.repo_state_changed.connect(self._on_repo_state_changed)
        self._poller.status_changed.connect(self._on_status_changed)

        self._show_no_repo()

    def get_header_height(self) -> int:
        """Return the pixel height of the collapsible header."""
        return self._header.sizeHint().height()

    def is_expanded(self) -> bool:
        """Return True if the section is currently expanded."""
        return self._header.is_expanded()

    def set_mindspace(self, path: str) -> None:
        """
        Set the mindspace root to track.

        Args:
            path: Absolute path to the mindspace root, or empty string to clear.
        """
        self._mindspace_path = path
        self._current_status = []
        self._list_widget.clear()
        self._poller.set_mindspace(path)

        if not path:
            self._show_no_repo()

    def apply_style(self) -> None:
        """Reapply theme and zoom-dependent styling."""
        self._header.apply_style()

        zoom = self._style_manager.zoom_factor()
        base = self._style_manager.base_font_size()
        font = self.font()
        font.setPointSizeF(base * zoom)
        self.setFont(font)
        self._list_widget.setFont(font)
        self._no_repo_label.setFont(font)

        self._rebuild_list()
        self._apply_stylesheet()

    def _apply_stylesheet(self) -> None:
        """Build and apply the widget stylesheet."""
        bg = self._style_manager.get_color_str(ColorRole.MINDSPACE_BACKGROUND)
        fg = self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)
        disabled = self._style_manager.get_color_str(ColorRole.TEXT_DISABLED)
        selected = self._style_manager.get_color_str(ColorRole.TEXT_SELECTED)
        hover = self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_HOVER)

        self.setStyleSheet(f"""
            QListWidget#_list_widget {{
                background-color: {bg};
                border: none;
                outline: none;
            }}
            QListWidget#_list_widget::item {{
                color: {fg};
                padding: 2px 4px;
            }}
            QListWidget#_list_widget::item:selected {{
                background-color: {selected};
            }}
            QListWidget#_list_widget::item:hover {{
                background-color: {hover};
            }}
            QLabel#_no_repo_label {{
                color: {disabled};
                background-color: {bg};
                padding: 4px;
            }}
        """)

    def _on_header_toggled(self, expanded: bool) -> None:
        """Show or hide the content area when the header is toggled."""
        if expanded:
            self._restore_content_visibility()
        else:
            self._list_widget.hide()
            self._no_repo_label.hide()

        self.toggled.emit(expanded)

    def _restore_content_visibility(self) -> None:
        """Show the appropriate content widget based on current repo state."""
        if self._poller._has_repo:
            self._list_widget.show()
            self._no_repo_label.hide()

        else:
            self._list_widget.hide()
            self._no_repo_label.show()

    def _show_no_repo(self) -> None:
        """Display the 'no repository' placeholder."""
        self._list_widget.hide()
        if self._header.is_expanded():
            self._no_repo_label.show()

        else:
            self._no_repo_label.hide()

    def _show_list(self) -> None:
        """Switch to showing the file list."""
        self._no_repo_label.hide()
        if self._header.is_expanded():
            self._list_widget.show()

    def _on_repo_state_changed(self, has_repo: bool) -> None:
        """React to the repository appearing or disappearing."""
        if has_repo:
            self._show_list()

        else:
            self._current_status = []
            self._list_widget.clear()
            self._show_no_repo()

    def _on_status_changed(self, status: list[VcsFileStatus]) -> None:
        """Update the list when git status changes."""
        self._current_status = status
        self._rebuild_list()
        self._show_list()

    def _rebuild_list(self) -> None:
        """Rebuild the QListWidget from the current status list."""
        self._list_widget.clear()

        for entry in self._current_status:
            display_name = self._display_name(entry)
            badge = _STATUS_LABELS.get(entry.code, "?")
            item = QListWidgetItem(f"{badge}  {display_name}")
            item.setData(Qt.ItemDataRole.UserRole, entry.path)
            item.setToolTip(entry.path)
            color = self._color_for_code(entry.code)
            item.setForeground(color)
            self._list_widget.addItem(item)

    def _display_name(self, entry: VcsFileStatus) -> str:
        """
        Return a short display name for a status entry.

        For renames, shows "old → new".  For all other entries shows the
        path relative to the mindspace root.

        Args:
            entry: The VcsFileStatus entry to format.

        Returns:
            Human-readable display string.
        """
        rel = self._rel_path(entry.path)

        if entry.code in (VcsStatusCode.RENAMED, VcsStatusCode.COPIED) and entry.original_path:
            old_rel = self._rel_path(entry.original_path)
            return f"{old_rel} → {rel}"

        return rel

    def _rel_path(self, abs_path: str) -> str:
        """
        Convert an absolute path to mindspace-relative form.

        Args:
            abs_path: Absolute file path.

        Returns:
            Relative path string, or the basename if conversion fails.
        """
        if self._mindspace_path:
            try:
                return os.path.relpath(abs_path, self._mindspace_path)
            except ValueError:
                pass

        return os.path.basename(abs_path)

    def _color_for_code(self, code: VcsStatusCode) -> QColor:
        """
        Return the theme-appropriate foreground colour for a status code.

        Args:
            code: The VcsStatusCode to look up.

        Returns:
            QColor for the status entry foreground.
        """
        if code in (VcsStatusCode.ADDED, VcsStatusCode.UNTRACKED):
            return self._style_manager.get_color(ColorRole.DIFF_ADDED_FOREGROUND)

        if code == VcsStatusCode.DELETED:
            return self._style_manager.get_color(ColorRole.DIFF_REMOVED_FOREGROUND)

        if code in (VcsStatusCode.RENAMED, VcsStatusCode.COPIED):
            return self._style_manager.get_color(ColorRole.DIFF_HEADER_FOREGROUND)

        # MODIFIED, UNKNOWN, and anything else
        return self._style_manager.get_color(ColorRole.DIFF_CHANGED_FOREGROUND)

    def _on_item_clicked(self, item: QListWidgetItem) -> None:
        """Open a diff tab ephemerally on single click."""
        path = item.data(Qt.ItemDataRole.UserRole)
        if path:
            self.file_opened_in_diff.emit(path, True)

    def _on_item_activated(self, item: QListWidgetItem) -> None:
        """Open a persistent diff tab on double-click / keyboard activation."""
        path = item.data(Qt.ItemDataRole.UserRole)
        if path:
            self.file_opened_in_diff.emit(path, False)

    def _on_language_changed(self) -> None:
        """Update localised strings when the UI language changes."""
        self._header.set_title(self._language_manager.strings().mindspace_vcs)
        self._no_repo_label.setText(
            self._language_manager.strings().mindspace_vcs_no_repo
        )
        self.apply_style()
