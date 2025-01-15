"""File tree view implementation for workspace files."""

import os
from typing import Optional

from PySide6.QtWidgets import (
    QTreeView, QFileSystemModel, QWidget, QVBoxLayout, QApplication
)
from PySide6.QtCore import Signal, QModelIndex, Qt, QSortFilterProxyModel, QMimeData, QSize
from PySide6.QtGui import QDrag

from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager
from humbug.gui.workspace_file_model import WorkspaceFileModel
from humbug.gui.file_tree_icon_provider import FileTreeIconProvider


class FileTreeView(QTreeView):
    """Custom tree view with drag support."""

    def __init__(self, parent=None):
        """Initialize the tree view."""
        super().__init__(parent)
        self.setDragEnabled(True)
        self.setDragDropMode(QTreeView.DragOnly)
        self._drag_start_pos = None

    def mousePressEvent(self, event):
        """Handle mouse press events for drag initiation."""
        if event.button() == Qt.LeftButton:
            self._drag_start_pos = event.pos()

        super().mousePressEvent(event)

    def mouseMoveEvent(self, event):
        """Handle mouse move events to start drag operations."""
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


class WorkspaceFileTree(QWidget):
    """Tree view widget for displaying workspace files."""

    file_activated = Signal(str)  # Emits path when file is activated

    def __init__(self, parent=None):
        """Initialize the file tree widget."""
        super().__init__(parent)

        self._style_manager = StyleManager()

        # Create layout
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        # Create tree view
        self._tree_view = FileTreeView()
        self._tree_view.setHeaderHidden(True)
        self._tree_view.setAnimated(True)
        self._tree_view.header().setSortIndicator(0, Qt.AscendingOrder)
        self._tree_view.setSortingEnabled(True)

        # Create file system model
        self._icon_provider = FileTreeIconProvider()
        self._fs_model = QFileSystemModel()
        self._fs_model.setReadOnly(True)

        # Create filter model
        self._filter_model = WorkspaceFileModel()
        self._filter_model.setSourceModel(self._fs_model)

        # Set model on tree view
        self._tree_view.setModel(self._filter_model)

        # Connect signals
        self._tree_view.activated.connect(self._handle_activation)

        # Add to layout
        layout.addWidget(self._tree_view)

        # Hide horizontal scrollbar
        self._tree_view.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)

        # Apply styling
        self._handle_style_changed()

        # Track current workspace
        self._workspace_path: Optional[str] = None
        self._style_manager.style_changed.connect(self._handle_style_changed)

    def set_workspace(self, path: str):
        """Set the workspace root directory."""
        if not path:
            # Clear the model when no workspace is active
            self._fs_model.setRootPath("")
            self._filter_model.set_workspace_root(None)
            self._tree_view.setRootIndex(self._filter_model.mapFromSource(
                self._fs_model.index("")
            ))
            return

        self._fs_model.setRootPath(path)
        self._filter_model.set_workspace_root(path)

        # Set the root index through the proxy model
        root_index = self._filter_model.mapFromSource(
            self._fs_model.index(path)
        )
        self._tree_view.setRootIndex(root_index)

        # Hide size, type, and date columns
        self._tree_view.header().hideSection(1)  # Size
        self._tree_view.header().hideSection(2)  # Type
        self._tree_view.header().hideSection(3)  # Date

        # Expand first level
        self._tree_view.expandToDepth(0)

    def _handle_activation(self, index: QModelIndex):
        """Handle item activation (double-click or Enter)."""
        # Get the file path from the source model
        source_index = self._filter_model.mapToSource(index)
        path = self._fs_model.filePath(source_index)

        # Only emit for files, not directories
        if os.path.isfile(path):
            self.file_activated.emit(path)

    def _handle_style_changed(self):
        """Update styling when application style changes."""
        zoom_factor = self._style_manager.zoom_factor
        base_font_size = self._style_manager.base_font_size

        self._icon_provider.update_icons()
        self._fs_model.setIconProvider(self._icon_provider)
        self._tree_view.setIconSize(QSize(16 * zoom_factor, 16 * zoom_factor))

        # Update font size
        font = self.font()
        font.setPointSizeF(base_font_size * zoom_factor)
        self.setFont(font)
        self._tree_view.setFont(font)

        self.setStyleSheet(f"""
            QTreeView {{
                background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_SECONDARY)};
                border: none;
                padding: 4px;
            }}
            QTreeView::item {{
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                padding: 2px;
            }}
            QTreeView::item:selected {{
                background-color: {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
            }}
            QTreeView::item:hover {{
                background-color: {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_HOVER)};
            }}
            QTreeView::branch {{
                background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_SECONDARY)};
            }}
            QTreeView::branch:has-children:!has-siblings:closed,
            QTreeView::branch:closed:has-children:has-siblings {{
                image: url("{self._style_manager.get_icon_path("arrow-right")}");
                padding: 2px;
            }}
            QTreeView::branch:open:has-children:!has-siblings,
            QTreeView::branch:open:has-children:has-siblings {{
                image: url("{self._style_manager.get_icon_path("arrow-down")}");
                padding: 2px;
            }}
            QScrollBar:vertical {{
                background-color: {self._style_manager.get_color_str(ColorRole.SCROLLBAR_BACKGROUND)};
                width: 12px;
            }}
            QScrollBar::handle:vertical {{
                background-color: {self._style_manager.get_color_str(ColorRole.SCROLLBAR_HANDLE)};
                min-height: 20px;
            }}
            QScrollBar::add-page:vertical, QScrollBar::sub-page:vertical {{
                background: none;
            }}
            QScrollBar::add-line:vertical, QScrollBar::sub-line:vertical {{
                height: 0px;
            }}
        """)
