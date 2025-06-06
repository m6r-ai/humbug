"""File tree view implementation for mindspace files with drag and drop support."""

import os
from typing import cast

from PySide6.QtWidgets import QTreeView, QApplication, QWidget, QFileSystemModel
from PySide6.QtCore import Qt, QSortFilterProxyModel, QMimeData, QPoint
from PySide6.QtGui import QDrag, QMouseEvent, QDragEnterEvent, QDragMoveEvent, QDropEvent


class MindspaceFileTreeView(QTreeView):
    """Custom tree view with drag and drop support."""

    def __init__(self, parent: QWidget | None = None):
        """Initialize the tree view."""
        super().__init__(parent)
        self.setDragEnabled(True)
        self.setAcceptDrops(True)
        self.setDragDropMode(QTreeView.DragDropMode.DragDrop)
        self.setDefaultDropAction(Qt.DropAction.MoveAction)
        self._drag_start_pos: QPoint | None = None

        self.setHeaderHidden(True)
        self.setAnimated(True)
        self.header().setSortIndicator(0, Qt.SortOrder.AscendingOrder)
        self.setSortingEnabled(True)
        self.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.setMouseTracking(True)
        self.setToolTipDuration(10000)

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

        self._drag_start_pos = None

    def dragEnterEvent(self, event: QDragEnterEvent) -> None:
        """Handle drag enter events."""
        if not event.mimeData().hasFormat("application/x-humbug-path"):
            event.ignore()
            return

        event.acceptProposedAction()

    def dragMoveEvent(self, event: QDragMoveEvent) -> None:
        """Handle drag move events to provide visual feedback."""
        if not event.mimeData().hasFormat("application/x-humbug-path"):
            event.ignore()
            return

        # Get the index at the current position
        index = self.indexAt(event.pos())
        if not index.isValid():
            event.ignore()
            return

        # Get the target path
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

        # Get the dragged item path
        mime_data = event.mimeData().data("application/x-humbug-path").data()

        # Convert to bytes first if it's not already bytes
        if not isinstance(mime_data, bytes):
            mime_data = bytes(mime_data)

        dragged_path = mime_data.decode()

        # Check if this is a valid drop target
        if not self._is_valid_drop_target(dragged_path, target_path):
            event.ignore()
            return

        event.acceptProposedAction()

    def dropEvent(self, event: QDropEvent) -> None:
        """Handle drop events."""
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

        # Get the dragged item path
        mime_data = event.mimeData().data("application/x-humbug-path").data()

        # Convert to bytes first if it's not already bytes
        if not isinstance(mime_data, bytes):
            mime_data = bytes(mime_data)

        dragged_path = mime_data.decode()

        # Validate the drop
        if not self._is_valid_drop_target(dragged_path, target_path):
            event.ignore()
            return

        # Emit a custom signal that the parent widget can handle
        # We'll add this signal handling in the parent MindspaceFileTree
        parent_widget = self.parent()
        if hasattr(parent_widget, '_handle_file_drop'):
            parent_widget._handle_file_drop(dragged_path, target_path)

        event.acceptProposedAction()

    def _is_valid_drop_target(self, source_path: str, target_path: str) -> bool:
        """
        Check if a drop operation is valid.

        Args:
            source_path: Path of the item being dragged
            target_path: Path of the drop target

        Returns:
            True if the drop is valid, False otherwise
        """
        # Can't drop on self
        if source_path == target_path:
            return False

        # Can't drop a parent folder into one of its children
        if target_path.startswith(source_path + os.sep):
            return False

        # Target must be a directory to accept drops
        if not os.path.isdir(target_path):
            return False

        # Check if source is a protected folder
        source_basename = os.path.basename(source_path)
        if source_basename in ['.humbug', 'conversations', 'metaphor']:
            return False

        return True
