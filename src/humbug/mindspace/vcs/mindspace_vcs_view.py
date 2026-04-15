"""Revision control status view for the mindspace sidebar."""

import os
import logging

from PySide6.QtCore import Qt, QMimeData, QPoint, Signal
from PySide6.QtGui import QColor, QDrag, QMouseEvent
from PySide6.QtWidgets import (
    QApplication, QListWidget, QListWidgetItem, QMenu, QVBoxLayout, QWidget
)

from git import VcsFileStatus, VcsStatusCode

from humbug.color_role import ColorRole
from humbug.language.language_manager import LanguageManager
from humbug.mindspace.mindspace_collapsible_header import MindspaceCollapsibleHeader
from humbug.message_box import MessageBox, MessageBoxButton, MessageBoxType
from humbug.mindspace.mindspace_log_level import MindspaceLogLevel
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.mindspace.vcs.mindspace_vcs_poller import MindspaceVcsPoller
from humbug.mindspace.vcs.mindspace_vcs_delegate import MindspaceVcsDelegate
from humbug.mindspace.mindspace_view_type import MindspaceViewType
from humbug.style_manager import StyleManager


_STATUS_LABELS: dict[VcsStatusCode, str] = {
    VcsStatusCode.MODIFIED:  "M",
    VcsStatusCode.ADDED:     "A",
    VcsStatusCode.DELETED:   "D",
    VcsStatusCode.RENAMED:   "R",
    VcsStatusCode.COPIED:    "C",
    VcsStatusCode.UNTRACKED: "?",
    VcsStatusCode.UNKNOWN:   "!",
}


class _VcsList(QListWidget):
    """QListWidget subclass that initiates Humbug path drags."""

    def __init__(self, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._drag_start_pos: QPoint | None = None
        self.setMouseTracking(True)
        self.setToolTipDuration(10000)

    def mousePressEvent(self, event: QMouseEvent) -> None:
        if event.button() == Qt.MouseButton.LeftButton:
            self._drag_start_pos = event.pos()

        super().mousePressEvent(event)

    def mouseMoveEvent(self, event: QMouseEvent) -> None:
        if not (event.buttons() & Qt.MouseButton.LeftButton):
            self._drag_start_pos = None
            super().mouseMoveEvent(event)
            return

        if self._drag_start_pos is None:
            super().mouseMoveEvent(event)
            return

        if (event.pos() - self._drag_start_pos).manhattanLength() < QApplication.startDragDistance():
            super().mouseMoveEvent(event)
            return

        item = self.itemAt(self._drag_start_pos)
        if item is None:
            super().mouseMoveEvent(event)
            return

        path = item.data(Qt.ItemDataRole.UserRole)
        if not path:
            super().mouseMoveEvent(event)
            return

        mime_data = QMimeData()
        mime_data.setData("application/x-humbug-path", path.encode())
        mime_data.setData("application/x-humbug-source", b"vcs")

        drag = QDrag(self)
        drag.setMimeData(mime_data)
        drag.setPixmap(self.viewport().grab(self.visualItemRect(item)))
        drag.setHotSpot(event.pos() - self._drag_start_pos)
        drag.exec_(Qt.DropAction.CopyAction)

        self._drag_start_pos = None


class MindspaceVcsView(QWidget):
    """Sidebar panel showing VCS-modified files for the current mindspace."""

    file_clicked = Signal(MindspaceViewType, str, bool)   # view type, path, ephemeral
    file_edited = Signal(str, bool)                        # path, ephemeral
    file_opened_in_preview = Signal(str, bool)             # path, ephemeral
    file_deleted = Signal(str)                             # path
    file_opened_in_diff = Signal(str, bool)                # path, ephemeral
    toggled = Signal(bool)                                 # expanded state
    repo_available = Signal(bool)                          # True = repo found, False = hidden

    def __init__(self, parent: QWidget | None = None) -> None:
        """Initialise the VCS view."""
        super().__init__(parent)

        self._style_manager = StyleManager()
        self._logger = logging.getLogger("MindspaceVcsView")
        self._mindspace_manager = MindspaceManager()
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

        self._list_widget = _VcsList()
        self._list_widget.setObjectName("_list_widget")
        self._list_widget.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self._list_widget.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self._list_widget.customContextMenuRequested.connect(self._show_context_menu)
        self._list_widget.itemClicked.connect(self._on_item_clicked)
        self._list_widget.itemActivated.connect(self._on_item_activated)
        self._list_widget.setItemDelegate(MindspaceVcsDelegate(self._list_widget))
        layout.addWidget(self._list_widget)

        self._poller = MindspaceVcsPoller()
        self._poller.repo_state_changed.connect(self._on_repo_state_changed)
        self._poller.status_changed.connect(self._on_status_changed)

        # Start hidden — only shown once a repo is confirmed present.
        self.hide()

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
        self.hide()
        self._poller.set_mindspace(path)

    def apply_style(self) -> None:
        """Reapply theme and zoom-dependent styling."""
        self._header.apply_style()

        zoom = self._style_manager.zoom_factor()
        base = self._style_manager.base_font_size()
        font = self.font()
        font.setPointSizeF(base * zoom)
        self.setFont(font)
        self._list_widget.setFont(font)

        self._rebuild_list()
        self._apply_stylesheet()

    def _apply_stylesheet(self) -> None:
        """Build and apply the widget stylesheet."""
        bg = self._style_manager.get_color_str(ColorRole.MINDSPACE_BACKGROUND)
        selected = self._style_manager.get_color_str(ColorRole.TEXT_SELECTED)
        hover = self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_HOVER)

        if self.layoutDirection() == Qt.LayoutDirection.LeftToRight:
            list_padding = "2px 0 0 5px"
        else:
            list_padding = "2px 5px 0 0"

        self.setStyleSheet(f"""
            QListWidget#_list_widget {{
                background-color: {bg};
                border: none;
                outline: none;
                padding: {list_padding};
            }}
            QListWidget#_list_widget::item {{
                padding: 2px 0 2px 0;
                margin: 0px;
            }}
            QListWidget#_list_widget::item:selected {{
                background-color: {selected};
            }}
            QListWidget#_list_widget::item:hover {{
                background-color: {hover};
            }}
        """)

    def _on_header_toggled(self, expanded: bool) -> None:
        """Show or hide the list when the header is toggled."""
        if expanded:
            self._list_widget.show()
        else:
            self._list_widget.hide()

        self.toggled.emit(expanded)

    def _on_repo_state_changed(self, has_repo: bool) -> None:
        """Show or hide the entire widget as the repository appears or disappears."""
        if has_repo:
            self.show()
        else:
            self._current_status = []
            self._list_widget.clear()
            self.hide()

        self.repo_available.emit(has_repo)

    def _on_status_changed(self, status: list[VcsFileStatus]) -> None:
        """Update the list when git status changes."""
        self._current_status = status
        self._rebuild_list()

    def _rebuild_list(self) -> None:
        """Rebuild the QListWidget from the current status list."""
        self._list_widget.clear()

        for entry in self._current_status:
            display_name = self._display_name(entry)
            badge = _STATUS_LABELS.get(entry.code, "?")
            item = QListWidgetItem(f"{badge}  {display_name}")
            item.setData(Qt.ItemDataRole.UserRole, entry.path)
            item.setToolTip(entry.path)
            item.setForeground(self._color_for_code(entry.code))
            self._list_widget.addItem(item)

    def _show_context_menu(self, position: QPoint) -> None:
        """Show a context menu for the item under the cursor.

        Args:
            position: Cursor position in list-widget viewport coordinates.
        """
        item = self._list_widget.itemAt(position)
        if item is None:
            return

        path = item.data(Qt.ItemDataRole.UserRole)
        if not path:
            return

        strings = self._language_manager.strings()
        menu = QMenu(self)

        diff_action = menu.addAction(strings.diff)
        diff_action.triggered.connect(lambda: self.file_opened_in_diff.emit(path, False))

        # Edit and preview are only meaningful for files that still exist on disk.
        if os.path.exists(path):
            edit_action = menu.addAction(strings.edit)
            edit_action.triggered.connect(lambda: self.file_edited.emit(path, False))

            preview_action = menu.addAction(strings.preview)
            preview_action.triggered.connect(lambda: self.file_opened_in_preview.emit(path, False))

            delete_action = menu.addAction(strings.delete)
            delete_action.triggered.connect(lambda: self._handle_delete_file(path))

        menu.exec_(self._list_widget.viewport().mapToGlobal(position))

    def _handle_delete_file(self, path: str) -> None:
        """Handle request to delete a file.

        Args:
            path: Path to the file to delete
        """
        strings = self._language_manager.strings()
        result = MessageBox.show_message(
            self,
            MessageBoxType.WARNING,
            strings.confirm_delete_title,
            strings.confirm_delete_item_message.format(os.path.basename(path)) + "\n\n" + strings.delete_warning_detail,
            [MessageBoxButton.YES, MessageBoxButton.NO],
            True
        )

        if result != MessageBoxButton.YES:
            return

        try:
            self.file_deleted.emit(path)
            os.remove(path)
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"User deleted file '{path}'"
            )

        except FileNotFoundError:
            pass

        except OSError as e:
            self._logger.error("Failed to delete file '%s': %s", path, str(e))
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.file_error_title,
                strings.error_deleting_file.format(str(e)),
                [MessageBoxButton.OK]
            )

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
            return self._style_manager.get_color(ColorRole.VCS_ADDED)

        if code == VcsStatusCode.DELETED:
            return self._style_manager.get_color(ColorRole.VCS_DELETED)

        if code in (VcsStatusCode.RENAMED, VcsStatusCode.COPIED):
            return self._style_manager.get_color(ColorRole.VCS_RENAMED)

        return self._style_manager.get_color(ColorRole.VCS_MODIFIED)

    def _on_item_clicked(self, item: QListWidgetItem) -> None:
        """Open an ephemeral diff tab on single click."""
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
        self.apply_style()
