"""Spine tree widget showing the ancestor folder chain of the topmost visible tree item."""

from collections.abc import Callable
import os
from typing import Union

from PySide6.QtCore import Qt, QMimeData, QModelIndex, QPersistentModelIndex, QPoint, QRect, QSize
from PySide6.QtGui import (
    QContextMenuEvent, QDragEnterEvent, QDragLeaveEvent, QDragMoveEvent, QDropEvent,
    QIcon, QMouseEvent, QPen, QPainter, QWheelEvent,
    QStandardItem, QStandardItemModel,
)
from PySide6.QtWidgets import QFrame, QStyledItemDelegate, QStyleOptionViewItem, QTreeView, QWidget

from desktop.color_role import ColorRole
from desktop.sidebar.sidebar_tree_icon_provider import SidebarTreeIconProvider
from desktop.sidebar.sidebar_tree_style import SidebarTreeStyle
from desktop.style_manager import StyleManager


_PATH_ROLE = Qt.ItemDataRole.UserRole


class _BreadcrumbDelegate(QStyledItemDelegate):
    """Item delegate that matches the row height of the main sidebar tree."""

    def __init__(self, style_manager: StyleManager) -> None:
        super().__init__()
        self._style_manager = style_manager

    def sizeHint(self, option: QStyleOptionViewItem, index: QModelIndex | QPersistentModelIndex) -> QSize:
        """Return the same zoom-scaled row height as SidebarTreeDelegate."""
        zoom = self._style_manager.zoom_factor()
        fm = option.fontMetrics  # type: ignore
        line_height = fm.height()
        row_height = max(line_height + round(10 * zoom), round(24 * zoom))
        return QSize(super().sizeHint(option, index).width(), row_height)


class SidebarBreadcrumbBar(QTreeView):
    """
    A compact tree view showing the ancestor chain (spine) of the folder currently
    at the top of the main tree's viewport.

    The "." sentinel is always the first top-level row. Real ancestor directories
    nest beneath it.  For example, if src/humbug is at the top of the main tree:

        .
        └── src
            └── humbug

    The widget cannot be collapsed by the user and resizes itself to fit its content
    exactly.  Each row is a valid drag-and-drop target.  Clicking a row scrolls the
    main tree to that folder.
    """

    def __init__(self, parent: QWidget | None = None) -> None:
        """Initialize the spine tree."""
        super().__init__(parent)
        self.setFrameShape(QFrame.Shape.NoFrame)
        self.setHeaderHidden(True)
        self.setAnimated(False)
        self.setSortingEnabled(False)
        self.setEditTriggers(QTreeView.EditTrigger.NoEditTriggers)
        self.setSelectionMode(QTreeView.SelectionMode.NoSelection)
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.setContextMenuPolicy(Qt.ContextMenuPolicy.DefaultContextMenu)
        self.setAcceptDrops(True)
        self.setFocusPolicy(Qt.FocusPolicy.NoFocus)

        self._style_manager = StyleManager()
        self._icon_provider = SidebarTreeIconProvider()
        self._tree_style = SidebarTreeStyle()
        self.setStyle(self._tree_style)
        self._delegate = _BreadcrumbDelegate(self._style_manager)
        self.setItemDelegate(self._delegate)

        self._model = QStandardItemModel(self)
        self.setModel(self._model)

        self._root_path: str = ""
        self._dot_path: str = ""
        self._current_spine: list[str] = []
        self._drop_target_index: QModelIndex = QModelIndex()

        self._drop_handler: Callable[[str, str], None] | None = None
        self._scroll_handler: Callable[[str], None] | None = None
        self._collapse_handler: Callable[[str], None] | None = None
        self._context_menu_handler: Callable[[str, QPoint], None] | None = None
        self._double_click_handler: Callable[[str], None] | None = None

        self.clicked.connect(self._on_item_clicked)

    def set_root_path(self, root_path: str) -> None:
        """
        Set the root path for the spine.

        Args:
            root_path: Absolute path of the mindspace/conversations root folder.
        """
        self._root_path = root_path
        self._dot_path = (root_path.rstrip(os.sep) + os.sep + ".") if root_path else ""
        self._rebuild([self._dot_path] if self._dot_path else [])

    def set_drop_handler(self, handler: Callable[[str, str], None]) -> None:
        """
        Set the callable invoked when an item is dropped onto a spine row.

        Signature: handler(source_path, target_path) -> None

        Args:
            handler: Drop handler callable
        """
        self._drop_handler = handler

    def set_scroll_handler(self, handler: Callable[[str], None]) -> None:
        """
        Set the callable invoked when the user clicks a spine row.

        Signature: handler(path) -> None

        Args:
            handler: Scroll handler callable
        """
        self._scroll_handler = handler

    def set_collapse_handler(self, handler: Callable[[str], None]) -> None:
        """
        Set the callable invoked when the user clicks the collapse arrow on a spine row.

        Signature: handler(path) -> None

        Args:
            handler: Collapse handler callable
        """
        self._collapse_handler = handler

    def set_context_menu_handler(self, handler: Callable[[str, QPoint], None]) -> None:
        """
        Set the callable invoked when the user right-clicks a spine row.

        Signature: handler(path, global_pos) -> None

        The path is the absolute file system path stored on the clicked item
        (the root path for the "." sentinel).  global_pos is the screen
        position suitable for passing directly to QMenu.exec_().

        Args:
            handler: Context menu handler callable
        """
        self._context_menu_handler = handler

    def set_double_click_handler(self, handler: Callable[[str], None]) -> None:
        """
        Set the callable invoked when the user double-clicks a spine row.

        Signature: handler(path) -> None

        Args:
            handler: Double-click handler callable
        """
        self._double_click_handler = handler

    def update_from_path(self, visible_path: str) -> int:
        """
        Rebuild the spine to show the ancestor chain of visible_path.

        Args:
            visible_path: Absolute path of the topmost visible folder in the main tree.
        """
        if not visible_path or not self._root_path:
            paths = [self._dot_path] if self._dot_path else []
            self._rebuild(paths)
            return len(paths)

        spine = self._build_spine(visible_path)
        if spine != self._current_spine:
            self._rebuild(spine)

        return len(spine)

    def drop_target_index(self) -> QModelIndex:
        """Return the model index currently acting as drop target, or an invalid index."""
        return self._drop_target_index

    def collapse(self, index: QModelIndex) -> None:  # type: ignore[override]
        """Suppress collapse — the breadcrumb is always fully expanded."""

    def setExpanded(self, index: QModelIndex, expanded: bool) -> None:  # type: ignore[override]
        """Only allow expansion, never collapse."""
        if expanded:
            super().setExpanded(index, True)

    def drawRow(
        self,
        painter: QPainter,
        option: QStyleOptionViewItem,
        index: Union[QModelIndex, QPersistentModelIndex],
    ) -> None:
        """Draw the row, adding drop target highlighting when applicable."""
        super().drawRow(painter, option, index)

        if index == self._drop_target_index:
            painter.save()
            drop_bg = self._style_manager.get_color(ColorRole.BUTTON_BACKGROUND_HOVER)
            drop_border = self._style_manager.get_color(ColorRole.TEXT_SELECTED)
            drop_bg.setAlpha(128)
            rect = option.rect  # type: ignore[attr-defined]
            painter.fillRect(rect, drop_bg)
            painter.setPen(QPen(drop_border, 1, Qt.PenStyle.SolidLine))
            painter.drawRect(rect.adjusted(1, 1, -1, -1))
            painter.restore()

    def apply_style(self, font_size: float, zoom_factor: float) -> None:
        """
        Apply current style settings.

        Args:
            font_size: Base font size in points
            zoom_factor: Current zoom factor
        """
        self._icon_provider.update_icons()

        icon_size = round(16 * zoom_factor)
        self.setIconSize(QSize(icon_size, icon_size))
        self.setIndentation(icon_size)

        font = self.font()
        font.setPointSizeF(font_size * zoom_factor)
        self.setFont(font)

        tree_hover = self._style_manager.get_color_str(ColorRole.BACKGROUND_TERTIARY_HOVER)
        text = self._style_manager.get_color_str(ColorRole.TEXT_HEADING)
        tree_margin = round(6 * zoom_factor)
        branch_icon_size = round(12 * zoom_factor)
        layout_dir = self.layoutDirection()
        collapsed_icon = "arrow-right" if layout_dir == Qt.LayoutDirection.LeftToRight else "arrow-left"
        expanded_icon = "arrow-down"

        self.setStyleSheet(f"""
            SidebarBreadcrumbBar {{
                background: transparent;
                color: {text};
                outline: none;
                margin-left: {tree_margin}px;
            }}
            SidebarBreadcrumbBar::item {{
                color: {text};
                padding: 0px;
                margin: 0px;
            }}
            SidebarBreadcrumbBar::item:hover {{
                background-color: {tree_hover};
            }}
            SidebarBreadcrumbBar::branch:has-children:!has-siblings:closed,
            SidebarBreadcrumbBar::branch:closed:has-children:has-siblings {{
                image: url("{self._style_manager.get_icon_path(collapsed_icon)}");
                width: {branch_icon_size}px;
                height: {branch_icon_size}px;
            }}
            SidebarBreadcrumbBar::branch:open:has-children:!has-siblings,
            SidebarBreadcrumbBar::branch:open:has-children:has-siblings {{
                image: url("{self._style_manager.get_icon_path(expanded_icon)}");
                width: {branch_icon_size}px;
                height: {branch_icon_size}px;
            }}
        """)

        self._refresh_icons()

    def _build_spine(self, target_path: str) -> list[str]:
        """
        Build the ordered spine list for target_path.

        Returns:
            Ordered list: [dot_path] or [dot_path, child_of_root, ..., target]
        """
        target_norm = os.path.normpath(target_path)
        root_norm = os.path.normpath(self._root_path)

        if target_norm == root_norm or not target_norm.startswith(root_norm + os.sep):
            return [self._dot_path]

        chain: list[str] = []
        current = target_norm
        while current != root_norm:
            chain.append(current)
            parent = os.path.dirname(current)
            if parent == current:
                break
            current = parent

        chain.reverse()
        return [self._dot_path] + chain

    def _rebuild(self, spine: list[str]) -> None:
        """
        Rebuild the model to represent the given spine.

        Layout:
          invisibleRoot
            dot_item        ("." — top-level leaf, no chevron)
            first_real_dir  (top-level, sibling of ".")
              second_real_dir
                ...
        """
        self._current_spine = spine
        self._drop_target_index = QModelIndex()
        self._model.beginResetModel()
        self._model.removeRows(0, self._model.rowCount())

        if not spine:
            self._model.endResetModel()
            return

        root_icon = self._root_folder_icon()
        icon = self._folder_icon()

        dot_item = QStandardItem(".")
        dot_item.setData(self._root_path, _PATH_ROLE)
        dot_item.setEditable(False)
        dot_item.setIcon(root_icon)
        self._model.invisibleRootItem().appendRow(dot_item)

        if len(spine) > 1:
            parent_item = self._model.invisibleRootItem()
            for path in spine[1:]:
                item = QStandardItem(os.path.basename(path))
                item.setData(path, _PATH_ROLE)
                item.setEditable(False)
                item.setIcon(icon)
                parent_item.appendRow(item)
                parent_item = item

        def _add_placeholders(parent_item: QStandardItem) -> None:
            for row in range(parent_item.rowCount()):
                child = parent_item.child(row)
                if child and child.flags() != Qt.ItemFlag.NoItemFlags:
                    child_path = child.data(_PATH_ROLE)
                    if child_path and os.path.normpath(child_path) == os.path.normpath(self._root_path):
                        continue
                    placeholder = QStandardItem()
                    placeholder.setFlags(Qt.ItemFlag.NoItemFlags)
                    child.appendRow(placeholder)
                    _add_placeholders(child)

        _add_placeholders(self._model.invisibleRootItem())

        self._model.endResetModel()
        self._expand_real_items(QModelIndex())

    def _reset_and_expand(self) -> None:
        """No-op: retained for any external callers; expansion now happens in _rebuild."""

    def _expand_real_items(self, parent: QModelIndex) -> None:
        """Expand only real (non-placeholder) items in the model."""
        for row in range(self._model.rowCount(parent)):
            index = self._model.index(row, 0, parent)
            item = self._model.itemFromIndex(index)
            if item and item.flags() != Qt.ItemFlag.NoItemFlags:
                super().setExpanded(index, True)
                self._expand_real_items(index)

    def _refresh_icons(self) -> None:
        """Refresh folder icons in the model after an icon provider update."""
        root_icon = self._root_folder_icon()
        icon = self._folder_icon()

        def refresh_recursive(parent: QModelIndex) -> None:
            for row in range(self._model.rowCount(parent)):
                index = self._model.index(row, 0, parent)
                path = index.data(_PATH_ROLE)
                if path and self._root_path and os.path.normpath(path) == os.path.normpath(self._root_path):
                    self._model.setData(index, root_icon, Qt.ItemDataRole.DecorationRole)

                else:
                    self._model.setData(index, icon, Qt.ItemDataRole.DecorationRole)

                refresh_recursive(index)

        refresh_recursive(QModelIndex())

    def _folder_icon(self) -> QIcon:
        """Return the open folder icon in the breadcrumb tint colour."""
        return self._icon_provider.open_folder_breadcrumb_icon()

    def _root_folder_icon(self) -> QIcon:
        """Return the current root folder icon from the icon provider."""
        return self._icon_provider.root_folder_icon()

    def _path_for_index(self, index: QModelIndex) -> str | None:
        """
        Return the drop target path stored in a model index.

        Args:
            index: Model index to query

        Returns:
            Absolute path string, or None if the index is invalid
        """
        if not index.isValid():
            return None

        return index.data(_PATH_ROLE)

    def _on_item_clicked(self, index: QModelIndex) -> None:
        """Handle a click on a spine item by invoking the scroll handler."""
        path = self._path_for_index(index)
        if path and self._scroll_handler:
            self._scroll_handler(path)

    def _get_dragged_path(self, mime_data: QMimeData) -> str:
        """Extract the dragged path from mime data."""
        raw = mime_data.data("application/x-humbug-path").data()
        if not isinstance(raw, bytes):
            raw = bytes(raw)

        return raw.decode()

    def _is_valid_drop(self, dragged_path: str, target_path: str) -> bool:
        """
        Check whether dropping dragged_path onto target_path is valid.

        Args:
            dragged_path: Path of the item being dragged
            target_path: Path of the folder being dropped onto

        Returns:
            True if the drop is valid
        """
        if not dragged_path or not target_path:
            return False

        dragged_norm = os.path.normpath(dragged_path)
        target_norm = os.path.normpath(target_path)

        if dragged_norm == target_norm:
            return False

        if target_norm.startswith(dragged_norm + os.sep):
            return False

        if os.path.dirname(dragged_norm) == target_norm:
            return False

        return True

    def _set_drop_target(self, index: QModelIndex) -> None:
        """Update the highlighted drop target and repaint."""
        if index != self._drop_target_index:
            self._drop_target_index = index
            self.viewport().update()

    def _clear_drop_target(self) -> None:
        """Clear the drop target highlight."""
        if self._drop_target_index.isValid():
            self._drop_target_index = QModelIndex()
            self.viewport().update()

    def dragEnterEvent(self, event: QDragEnterEvent) -> None:
        """Accept drag entry if it carries a humbug path."""
        if not event.mimeData().hasFormat("application/x-humbug-path"):
            event.ignore()
            return

        event.acceptProposedAction()

    def dragMoveEvent(self, event: QDragMoveEvent) -> None:
        """Update the drop target highlight as the drag moves over spine rows."""
        if not event.mimeData().hasFormat("application/x-humbug-path"):
            event.ignore()
            self._clear_drop_target()
            return

        dragged_path = self._get_dragged_path(event.mimeData())
        index = self.indexAt(event.pos())
        target_path = self._path_for_index(index)

        if target_path and self._is_valid_drop(dragged_path, target_path):
            self._set_drop_target(index)
            event.acceptProposedAction()
        else:
            self._clear_drop_target()
            event.ignore()

    def dragLeaveEvent(self, event: QDragLeaveEvent) -> None:
        """Clear highlighting when the drag leaves the spine."""
        self._clear_drop_target()
        super().dragLeaveEvent(event)

    def dropEvent(self, event: QDropEvent) -> None:
        """Handle a drop onto a spine row."""
        self._clear_drop_target()

        if not event.mimeData().hasFormat("application/x-humbug-path"):
            event.ignore()
            return

        dragged_path = self._get_dragged_path(event.mimeData())
        index = self.indexAt(event.pos())
        target_path = self._path_for_index(index)

        if not target_path or not self._is_valid_drop(dragged_path, target_path):
            event.ignore()
            return

        if self._drop_handler:
            self._drop_handler(dragged_path, target_path)

        event.acceptProposedAction()

    def drawBranches(
        self,
        painter: QPainter,
        rect: QRect,
        index: Union[QModelIndex, QPersistentModelIndex],
    ) -> None:
        """Suppress branch indicator for the '.' row; draw normally for all others."""
        path = index.data(_PATH_ROLE)
        if path and self._root_path and os.path.normpath(path) == os.path.normpath(self._root_path):
            return

        super().drawBranches(painter, rect, index)


    def wheelEvent(self, event: QWheelEvent) -> None:
        """Ignore wheel events — the breadcrumb bar does not scroll."""
        event.ignore()

    def scrollContentsBy(self, dx: int, dy: int) -> None:
        """Suppress all internal scrolling — the breadcrumb is always fully visible."""

    def mousePressEvent(self, event: QMouseEvent) -> None:
        """Suppress branch-area clicks; fire collapse handler for branch-area left-clicks."""
        if event.button() == Qt.MouseButton.RightButton:
            # Accept the press so the matching release is delivered to us, which
            # allows contextMenuEvent to fire via the normal Qt mechanism.
            event.accept()
            return

        index = self.indexAt(event.pos())
        if index.isValid():
            item_rect = self.visualRect(index)
            if event.pos().x() < item_rect.left():
                path = self._path_for_index(index)
                if path and self._collapse_handler:
                    self._collapse_handler(path)
                return

        super().mousePressEvent(event)

    def mouseDoubleClickEvent(self, event: QMouseEvent) -> None:
        """Fire the double-click handler for the item under the cursor."""
        if event.button() == Qt.MouseButton.LeftButton:
            index = self.indexAt(event.pos())
            if index.isValid():
                item_rect = self.visualRect(index)
                if event.pos().x() >= item_rect.left():
                    path = self._path_for_index(index)
                    if path and self._double_click_handler:
                        self._double_click_handler(path)
                        return

        super().mouseDoubleClickEvent(event)

    def mouseReleaseEvent(self, event: QMouseEvent) -> None:
        """Suppress branch-area release events."""
        if event.button() == Qt.MouseButton.RightButton:
            event.accept()
            return

        index = self.indexAt(event.pos())
        if index.isValid():
            item_rect = self.visualRect(index)
            if event.pos().x() < item_rect.left():
                return

        super().mouseReleaseEvent(event)

    def contextMenuEvent(self, event: QContextMenuEvent) -> None:
        """Fire the context menu handler when the user right-clicks a spine row."""
        if not self._context_menu_handler:
            return

        index = self.indexAt(event.pos())
        path = self._path_for_index(index) if index.isValid() else self._root_path
        if not path:
            path = self._root_path

        if path:
            self._context_menu_handler(path, event.globalPos())
