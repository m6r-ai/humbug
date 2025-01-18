"""File tree view implementation for mindspace files."""

import os

from PySide6.QtWidgets import QTreeView, QApplication
from PySide6.QtCore import Qt, QSortFilterProxyModel, QMimeData
from PySide6.QtGui import QDrag


class FileTreeView(QTreeView):
    """Custom tree view with drag support."""

    def __init__(self, parent=None):
        """Initialize the tree view."""
        super().__init__(parent)
        self.setDragEnabled(True)
        self.setDragDropMode(QTreeView.DragOnly)
        self._drag_start_pos = None

        self.setHeaderHidden(True)
        self.setAnimated(True)
        self.header().setSortIndicator(0, Qt.AscendingOrder)
        self.setSortingEnabled(True)
        self.setContextMenuPolicy(Qt.CustomContextMenu)
        self.setMouseTracking(True)
        self.setToolTipDuration(10000)

    def mousePressEvent(self, event):
        """Handle mouse press events for drag initiation."""
        if event.button() == Qt.LeftButton:
            self._drag_start_pos = event.pos()

        super().mousePressEvent(event)

    def mouseMoveEvent(self, event):
        """Handle mouse move events."""
        # Get the item under the mouse to work out tool tips.
        index = self.indexAt(event.pos())
        if not index.isValid():
            self.setToolTip("")
        else:
            # Get the file path from the source model
            source_model = self.model()
            if source_model:
                # If using a proxy model, map to source
                if isinstance(source_model, QSortFilterProxyModel):
                    source_index = source_model.mapToSource(index)
                    file_model = source_model.sourceModel()
                    if file_model:
                        path = file_model.filePath(source_index)
                        self.setToolTip(path)

        if not (event.buttons() & Qt.LeftButton):
            return

        if not self._drag_start_pos:
            return

        # Check if we've moved far enough to start a drag
        if (event.pos() - self._drag_start_pos).manhattanLength() < QApplication.startDragDistance():
            return

        # Get the item under the mouse
        index = self.indexAt(self._drag_start_pos)
        if not index.isValid():
            return

        # Get the file path from the source model
        source_model = self.model()
        if not source_model:
            return

        # If using a proxy model, map to source
        if isinstance(source_model, QSortFilterProxyModel):
            source_index = source_model.mapToSource(index)
            file_model = source_model.sourceModel()
            if not file_model:
                return
            path = file_model.filePath(source_index)
        else:
            path = source_model.filePath(index)

        # Only allow dragging files, not directories
        if os.path.isdir(path):
            return

        # Create mime data with file path
        mime_data = QMimeData()
        mime_data.setData("application/x-humbug-file", path.encode())

        # Create drag object
        drag = QDrag(self)
        drag.setMimeData(mime_data)

        # Create drag pixmap from the tree item
        pixmap = self.viewport().grab(self.visualRect(index))
        drag.setPixmap(pixmap)
        drag.setHotSpot(event.pos() - self._drag_start_pos)

        # Execute drag operation
        drag.exec_(Qt.CopyAction)

        self._drag_start_pos = None
