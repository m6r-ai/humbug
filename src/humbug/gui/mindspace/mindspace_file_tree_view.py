"""File tree view implementation for mindspace files with drag and drop support."""

import os
from typing import cast

from PySide6.QtWidgets import QTreeView, QApplication, QWidget, QFileSystemModel
from PySide6.QtCore import Qt, QSortFilterProxyModel, QMimeData, QPoint, Signal, QModelIndex, QPersistentModelIndex
from PySide6.QtGui import QDrag, QMouseEvent, QDragEnterEvent, QDragMoveEvent, QDropEvent, QDragLeaveEvent


class MindspaceFileTreeView(QTreeView):
    """Custom tree view with drag and drop support."""

    file_dropped = Signal(str, str)  # dragged_path, target_path
    drop_target_changed = Signal(QModelIndex)  # current drop target index

    def __init__(self, parent: QWidget | None = None):
        """Initialize the tree view."""
        super().__init__(parent)
        self.setDragEnabled(True)
        self.setAcceptDrops(True)
        self.setDragDropMode(QTreeView.DragDropMode.DragDrop)
        self.setDefaultDropAction(Qt.DropAction.MoveAction)
        self._drag_start_pos: QPoint | None = None
        self._current_drop_target: QModelIndex | None = None

        self.setHeaderHidden(True)
        self.setAnimated(True)
        self.header().setSortIndicator(0, Qt.SortOrder.AscendingOrder)
        self.setSortingEnabled(True)
        self.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.setMouseTracking(True)
        self.setToolTipDuration(10000)

    def get_current_drop_target(self) -> QModelIndex | None:
        """Get the current drop target index."""
        return self._current_drop_target

    def clear_drop_target(self) -> None:
        """Clear the current drop target and emit signal."""
        if self._current_drop_target is not None:
            self._current_drop_target = None
            self.drop_target_changed.emit(QModelIndex())  # Invalid index signals clear

    def _set_drop_target(self, index: QModelIndex) -> None:
        """Set the current drop target and emit signal if changed."""
        if self._current_drop_target != index:
            self._current_drop_target = index
            self.drop_target_changed.emit(index)

    def mousePressEvent(self, event: QMouseEvent) -> None:
        """Handle mouse press events for drag initiation."""
        if event.button() & Qt.MouseButton.LeftButton:
            self._drag_start_pos = event.pos()

        super().mousePressEvent(event)

    def mouseMoveEvent(self, event: QMouseEvent) -> None:
        """Handle mouse move events."""
        # Get the file path from the source model
        source_model = cast(QSortFilterProxyModel, self.model())
        if not source_model:
            return

        index = self.indexAt(event.pos())
        source_index = source_model.mapToSource(index)
        file_model = cast(QFileSystemModel, source_model.sourceModel())
        if not file_model:
            return

        path = file_model.filePath(source_index)

        # Get the item under the mouse to work out tool tips.
        self.setToolTip(path if index.isValid() else "")

        if not event.buttons() & Qt.MouseButton.LeftButton:
            return

        if not self._drag_start_pos:
            return

        # Check if we've moved far enough to start a drag
        if (event.pos() - self._drag_start_pos).manhattanLength() < QApplication.startDragDistance():
            return

        # Get the item under the mouse
        drag_index = self.indexAt(self._drag_start_pos)
        if not drag_index.isValid():
            return

        # Create mime data with path
        mime_data = QMimeData()
        mime_data.setData("application/x-humbug-path", path.encode())

        # Create drag object
        drag = QDrag(self)
        drag.setMimeData(mime_data)

        # Create drag pixmap from the tree item
        pixmap = self.viewport().grab(self.visualRect(index))
        drag.setPixmap(pixmap)
        drag.setHotSpot(event.pos() - self._drag_start_pos)

        # Execute drag operation
        drag.exec_(Qt.DropAction.MoveAction | Qt.DropAction.CopyAction)

        # Clear drop target when drag ends
        self.clear_drop_target()
        self._drag_start_pos = None

    def dragEnterEvent(self, event: QDragEnterEvent) -> None:
        """Handle drag enter events."""
        if not event.mimeData().hasFormat("application/x-humbug-path"):
            event.ignore()
            return

        event.acceptProposedAction()

    def dragLeaveEvent(self, event: QDragLeaveEvent) -> None:
        """Handle drag leave events."""
        # Clear drop target when drag leaves the tree view
        self.clear_drop_target()
        super().dragLeaveEvent(event)

    def dragMoveEvent(self, event: QDragMoveEvent) -> None:
        """Handle drag move events to provide visual feedback."""
        if not event.mimeData().hasFormat("application/x-humbug-path"):
            event.ignore()
            self.clear_drop_target()
            return

        # Get the index at the current position
        index = self.indexAt(event.pos())
        if not index.isValid():
            event.ignore()
            self.clear_drop_target()
            return

        # Get the target path
        source_model = cast(QSortFilterProxyModel, self.model())
        if not source_model:
            event.ignore()
            self.clear_drop_target()
            return

        source_index = source_model.mapToSource(index)
        file_model = cast(QFileSystemModel, source_model.sourceModel())
        if not file_model:
            event.ignore()
            self.clear_drop_target()
            return

        target_path = file_model.filePath(source_index)

        # Get the dragged item path
        mime_data = event.mimeData().data("application/x-humbug-path").data()

        # Convert to bytes first if it's not already bytes
        if not isinstance(mime_data, bytes):
            mime_data = bytes(mime_data)

        dragged_path = mime_data.decode()

        # Can't drop a parent folder into one of its children
        if target_path.startswith(dragged_path + os.sep):
            event.ignore()
            self.clear_drop_target()
            return

        # If the target is not a directory, ignore the event
        if not os.path.isdir(target_path):
            event.ignore()
            self.clear_drop_target()
            return

        # If the dragged path is the same as the target, ignore the event
        if dragged_path == target_path:
            event.ignore()
            self.clear_drop_target()
            return

        # Check if this is a valid drag target
        source_basename = os.path.basename(dragged_path)
        if source_basename in ['.humbug', 'conversations', 'metaphor']:
            event.ignore()
            self.clear_drop_target()
            return

        # Set this index as the current drop target
        self._set_drop_target(index)

        print(f"Dragging {dragged_path} to {target_path}")
        event.acceptProposedAction()

    def dropEvent(self, event: QDropEvent) -> None:
        """Handle drop events."""
        # Clear drop target when drop completes
        self.clear_drop_target()

        if not event.mimeData().hasFormat("application/x-humbug-path"):
            event.ignore()
            return

        # Get the target index and path
        index = self.indexAt(event.pos())
        if not index.isValid():
            event.ignore()
            return

        source_model = cast(QSortFilterProxyModel, self.model())
        if not source_model:
            event.ignore()
            return

        source_index = source_model.mapToSource(index)
        file_model = cast(QFileSystemModel, source_model.sourceModel())
        if not file_model:
            event.ignore()
            return

        target_path = file_model.filePath(source_index)
        if not os.path.isdir(target_path):
            event.ignore()
            return

        # Get the dragged item path
        mime_data = event.mimeData().data("application/x-humbug-path").data()

        # Convert to bytes first if it's not already bytes
        if not isinstance(mime_data, bytes):
            mime_data = bytes(mime_data)

        dragged_path = mime_data.decode()

        # Can't drop a parent folder into one of its children
        if target_path.startswith(dragged_path + os.sep):
            event.ignore()
            return

        # If the target is not a directory, ignore the event
        if not os.path.isdir(target_path):
            event.ignore()
            return

        # If the dragged path is the same as the target, ignore the event
        if dragged_path == target_path:
            event.ignore()
            return

        # Signal the drop event
        self.file_dropped.emit(dragged_path, target_path)

        event.acceptProposedAction()
