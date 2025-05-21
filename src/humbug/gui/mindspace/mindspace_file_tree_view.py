"""File tree view implementation for mindspace files."""

from typing import cast

from PySide6.QtWidgets import QTreeView, QApplication, QWidget, QFileSystemModel
from PySide6.QtCore import Qt, QSortFilterProxyModel, QMimeData, QPoint
from PySide6.QtGui import QDrag, QMouseEvent


class MindspaceFileTreeView(QTreeView):
    """Custom tree view with drag support."""

    def __init__(self, parent: QWidget | None = None):
        """Initialize the tree view."""
        super().__init__(parent)
        self.setDragEnabled(True)
        self.setDragDropMode(QTreeView.DragDropMode.DragOnly)
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
        drag.exec_(Qt.DropAction.CopyAction)

        self._drag_start_pos = None
