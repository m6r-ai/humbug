"""Trash browser panel — lists items soft-deleted from the conversations tree."""

from datetime import datetime, timezone
import os

from PySide6.QtCore import QFileInfo, QMimeData, QModelIndex, QPersistentModelIndex, QPoint, QSize, Qt, Signal
from PySide6.QtGui import QDrag, QMouseEvent
from PySide6.QtWidgets import (
    QApplication, QFrame, QLabel, QSizePolicy, QSpacerItem, QStyledItemDelegate, QStyleOptionViewItem,
    QTreeWidget, QTreeWidgetItem, QVBoxLayout, QWidget
)

from mindspace.mindspace_error import MindspaceError
from mindspace.trash_manifest import TrashEntry, strip_trash_prefix

from desktop.color_role import ColorRole
from desktop.file_utils import is_conversation_file
from desktop.language.language_manager import LanguageManager
from desktop.message_box import MessageBox, MessageBoxButton, MessageBoxType
from desktop.mindspace.mindspace_manager import MindspaceManager
from desktop.sidebar.sidebar_base import SidebarBase
from desktop.sidebar.sidebar_pane_style import build_tree_pane_stylesheet
from desktop.sidebar.sidebar_section_header import SidebarSectionHeader
from desktop.sidebar.sidebar_tree_icon_provider import SidebarTreeIconProvider
from desktop.sidebar.sidebar_tree_style import SidebarTreeStyle
from desktop.style_manager import StyleManager

_TRASH_NAME_ROLE = Qt.ItemDataRole.UserRole + 1


class TrashSidebar(SidebarBase):
    """
    Durable browser for trashed chats/folders, independent of the undo/redo stack.

    Undo/redo (Ctrl+Z) is purely in-memory and lost on restart, so anything
    still sitting in the trash directory once that history is gone would
    otherwise be unrecoverable through the UI. This panel reads the trash
    directory (and its manifest of original locations) directly, so items
    remain visible and restorable across restarts.
    """

    visibility_requested = Signal(bool)
    file_clicked = Signal(str, str, bool)  # panel_id, path, ephemeral

    def __init__(self, parent: QWidget | None = None) -> None:
        super().__init__(parent)

        self._style_manager = StyleManager()
        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._on_language_changed)
        self._mindspace_manager = MindspaceManager()
        self._icon_provider = SidebarTreeIconProvider()
        self._mindspace_path = ""
        self._empty_state_spacer: QSpacerItem | None = None

        self._layout = QVBoxLayout(self)
        self._layout.setContentsMargins(0, 0, 0, 0)
        self._layout.setSpacing(0)

        self._header = SidebarSectionHeader(self._language_manager.strings().trash, self)
        self._layout.addWidget(self._header)

        self._status_label = QLabel(self)
        self._status_label.setObjectName("_status_label")
        self._layout.addWidget(self._status_label)

        self._tree = TrashTreeWidget(self)
        self._tree.setObjectName("TrashSidebarTree")
        self._tree_style = SidebarTreeStyle()
        self._tree.setStyle(self._tree_style)
        self._tree.setFrameShape(QFrame.Shape.NoFrame)
        self._tree.setHeaderHidden(True)
        self._tree.setRootIsDecorated(False)
        self._tree.setDragEnabled(True)
        self._tree.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self._tree.customContextMenuRequested.connect(self._show_context_menu)

        self._delegate = TrashTreeDelegate(self._tree, self._style_manager)
        self._tree.setItemDelegate(self._delegate)

        self._tree.itemClicked.connect(self._on_item_clicked)
        self._tree.itemDoubleClicked.connect(self._on_item_double_clicked)
        self._layout.addWidget(self._tree)

        self._on_language_changed()

    def set_mindspace(self, path: str) -> None:
        """Set the active mindspace root and refresh the trash listing."""
        self._mindspace_path = path
        self.refresh()

    def refresh(self) -> None:
        """Rebuild the trash listing from disk."""
        self._tree.clear()
        strings = self._language_manager.strings()

        if not self._mindspace_path:
            self._show_empty_state(strings.trash_no_mindspace_message)
            return

        entries = self._mindspace_manager.mindspace().list_trashed()
        if not entries:
            self._show_empty_state(strings.trash_empty_message)
            return

        self._status_label.hide()
        self._tree.show()
        if self._empty_state_spacer is not None:
            self._layout.removeItem(self._empty_state_spacer)
            self._empty_state_spacer = None

        for entry in entries:
            self._tree.addTopLevelItem(self._build_item(entry))

    def _show_empty_state(self, message: str) -> None:
        """Show message in place of the tree, without leaving an empty tree box behind."""
        self._status_label.setText(message)
        self._status_label.show()
        self._tree.hide()
        if self._empty_state_spacer is None:
            self._empty_state_spacer = QSpacerItem(
                0, 0, QSizePolicy.Policy.Minimum, QSizePolicy.Policy.Expanding
            )
            self._layout.addItem(self._empty_state_spacer)

    def _build_item(self, entry: TrashEntry) -> QTreeWidgetItem:
        """Build a single row for a trash entry."""
        strings = self._language_manager.strings()
        trash_path = os.path.join(self._mindspace_manager.mindspace().trash_dir(), entry.trash_name)
        name = os.path.basename(entry.original_path) if entry.original_path else _display_name(entry.trash_name)
        location = os.path.dirname(entry.original_path) if entry.original_path else strings.trash_unknown_location

        item = QTreeWidgetItem([f"{name}  —  {location}  ·  {self._format_deleted_at(entry.deleted_at)}"])
        item.setData(0, _TRASH_NAME_ROLE, entry.trash_name)
        item.setToolTip(0, entry.original_path or strings.trash_unknown_location)
        item.setIcon(0, self._icon_provider.icon(QFileInfo(trash_path)))
        return item

    def _format_deleted_at(self, deleted_at: str) -> str:
        """Format an ISO 8601 timestamp as a short relative time string."""
        strings = self._language_manager.strings()
        try:
            deleted = datetime.fromisoformat(deleted_at)

        except ValueError:
            return deleted_at

        elapsed = datetime.now(timezone.utc) - deleted
        minutes = int(elapsed.total_seconds() // 60)
        if minutes < 1:
            return strings.trash_time_just_now

        if minutes < 60:
            return strings.trash_time_minutes_ago.format(minutes)

        hours = minutes // 60
        if hours < 24:
            return strings.trash_time_hours_ago.format(hours)

        return strings.trash_time_days_ago.format(hours // 24)

    def _on_item_clicked(self, item: QTreeWidgetItem) -> None:
        """Open a trashed conversation file in a conversation tab (ephemeral, active column)."""
        trash_path = self._trash_path_for_item(item)
        if trash_path and is_conversation_file(trash_path):
            self.file_clicked.emit("trash", trash_path, True)

    def _on_item_double_clicked(self, item: QTreeWidgetItem) -> None:
        """Open a trashed conversation file in a conversation tab (non-ephemeral, active column)."""
        trash_path = self._trash_path_for_item(item)
        if trash_path and is_conversation_file(trash_path):
            self.file_clicked.emit("trash", trash_path, False)

    def _trash_path_for_item(self, item: QTreeWidgetItem) -> str:
        """Return the absolute trash-directory path for a tree widget item."""
        trash_name = item.data(0, _TRASH_NAME_ROLE)
        if not trash_name:
            return ""

        return os.path.join(self._mindspace_manager.mindspace().trash_dir(), trash_name)

    def _show_context_menu(self, position: QPoint) -> None:
        """Show the Restore/Delete Forever/Empty Trash context menu."""
        strings = self._language_manager.strings()
        menu = self._style_manager.create_menu(self)

        item = self._tree.itemAt(position)
        if item is not None:
            trash_name = item.data(0, _TRASH_NAME_ROLE)
            restore_action = menu.addAction(strings.trash_restore)
            restore_action.triggered.connect(lambda: self._restore(trash_name))
            delete_action = menu.addAction(strings.trash_delete_forever)
            delete_action.triggered.connect(lambda: self._delete_forever(trash_name))

        else:
            empty_action = menu.addAction(strings.trash_empty_trash_action)
            empty_action.triggered.connect(self._empty_trash)

        menu.exec_(self._tree.viewport().mapToGlobal(position))

    def _restore(self, trash_name: str) -> None:
        """Restore a trashed item to its original (or best-guess) location."""
        strings = self._language_manager.strings()
        try:
            self._mindspace_manager.mindspace().restore_trashed(trash_name)

        except MindspaceError as e:
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.trash_restore_failed_title,
                strings.trash_restore_failed_message.format(_display_name(trash_name), str(e)),
                [MessageBoxButton.OK]
            )

        self.refresh()

    def _delete_forever(self, trash_name: str) -> None:
        """Permanently delete a single trashed item, after confirmation."""
        strings = self._language_manager.strings()
        result = MessageBox.show_message(
            self,
            MessageBoxType.WARNING,
            strings.trash_confirm_delete_forever_title,
            strings.trash_confirm_delete_forever_message.format(_display_name(trash_name)),
            [MessageBoxButton.YES, MessageBoxButton.NO],
            True
        )
        if result != MessageBoxButton.YES:
            return

        trash_path = os.path.join(self._mindspace_manager.mindspace().trash_dir(), trash_name)
        self._mindspace_manager.mindspace().purge_trashed(trash_path)
        self.refresh()

    def _empty_trash(self) -> None:
        """Permanently delete everything in the trash, after confirmation."""
        strings = self._language_manager.strings()
        result = MessageBox.show_message(
            self,
            MessageBoxType.WARNING,
            strings.trash_confirm_empty_title,
            strings.trash_confirm_empty_message,
            [MessageBoxButton.YES, MessageBoxButton.NO],
            True
        )
        if result != MessageBoxButton.YES:
            return

        self._mindspace_manager.mindspace().empty_trash()
        self.refresh()

    def _on_language_changed(self) -> None:
        """Update all UI labels when the language changes."""
        self._header.set_title(self._language_manager.strings().trash)
        self.refresh()
        self.apply_style()

    def apply_style(self) -> None:
        """Apply current application style."""
        zoom_factor = self._style_manager.zoom_factor()
        base_font_size = self._style_manager.base_font_size()

        self._header.apply_style()
        self._icon_provider.update_icons()

        font = self.font()
        font.setPointSizeF(base_font_size * zoom_factor)
        self.setFont(font)
        self._status_label.setFont(font)
        self._tree.setFont(font)
        icon_size = round(16 * zoom_factor)
        self._tree.setIconSize(QSize(icon_size, icon_size))

        subtle_text = self._style_manager.get_color_str(ColorRole.TEXT_INACTIVE)

        self.setStyleSheet(
            build_tree_pane_stylesheet(
                self._style_manager,
                "TrashSidebar",
                "QTreeWidget#TrashSidebarTree",
                self.layoutDirection(),
                zoom_factor,
            )
            + f"""
            QLabel#_status_label {{
                color: {subtle_text};
                padding: {round(12 * zoom_factor)}px {round(6 * zoom_factor)}px;
            }}
            QTreeWidget#TrashSidebarTree {{
                border: none;
            }}
            """
        )


class TrashTreeWidget(QTreeWidget):
    """QTreeWidget subclass supporting drag initiation for trashed conversation files."""

    def __init__(self, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._drag_start_pos: QPoint | None = None

    def mousePressEvent(self, event: QMouseEvent) -> None:
        """Record the drag start position before delegating to the base class."""
        if event.button() & Qt.MouseButton.LeftButton:
            self._drag_start_pos = event.pos()

        super().mousePressEvent(event)

    def mouseMoveEvent(self, event: QMouseEvent) -> None:
        """Initiate a drag when the mouse moves far enough from the press point."""
        if not event.buttons() & Qt.MouseButton.LeftButton:
            return

        if not self._drag_start_pos:
            return

        if (event.pos() - self._drag_start_pos).manhattanLength() < QApplication.startDragDistance():
            return

        item = self.itemAt(self._drag_start_pos)
        if item is None:
            return

        trash_name = item.data(0, _TRASH_NAME_ROLE)
        if not trash_name:
            return

        trash_path = os.path.join(MindspaceManager().mindspace().trash_dir(), trash_name)

        mime_data = QMimeData()
        mime_data.setData("application/x-humbug-path", trash_path.encode())
        mime_data.setData("application/x-humbug-source", b"conversations")

        drag = QDrag(self)
        drag.setMimeData(mime_data)
        pixmap = self.viewport().grab(self.visualItemRect(item))
        drag.setPixmap(pixmap)
        drag.setHotSpot(event.pos() - self._drag_start_pos)
        drag.exec_(Qt.DropAction.MoveAction | Qt.DropAction.CopyAction)

        self._drag_start_pos = None


class TrashTreeDelegate(QStyledItemDelegate):
    """Item delegate for the trash tree widget, providing consistent row spacing."""

    def __init__(self, tree: QTreeWidget, style_manager: StyleManager) -> None:
        super().__init__(tree)
        self._style_manager = style_manager

    def sizeHint(self, option: QStyleOptionViewItem, _index: QModelIndex | QPersistentModelIndex) -> QSize:
        """Return a consistent, zoom-scaled row height matching the conversation sidebar."""
        zoom = self._style_manager.zoom_factor()
        fm = option.fontMetrics  # type: ignore
        line_height = fm.height()
        row_height = max(line_height + round(10 * zoom), round(24 * zoom))
        return QSize(super().sizeHint(option, _index).width(), row_height)


def _display_name(trash_name: str) -> str:
    """Return trash_name with its uuid8_ uniqueness prefix stripped, for display."""
    return strip_trash_prefix(trash_name)
