"""File tree view implementation for workspace files."""

import os
from typing import Optional

from PySide6.QtWidgets import (
    QTreeView, QFileSystemModel, QWidget, QVBoxLayout
)
from PySide6.QtCore import Signal, QModelIndex, QSortFilterProxyModel

from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager, ColorMode


class WorkspaceFileModel(QSortFilterProxyModel):
    """Filter model to hide .humbug directory and apply custom sorting."""

    def __init__(self, parent=None):
        """Initialize the filter model."""
        super().__init__(parent)
        self._workspace_root = None

    def set_workspace_root(self, path: str):
        """Set the workspace root path for relative path calculations."""
        self._workspace_root = path

    def filterAcceptsRow(self, source_row: int, source_parent: QModelIndex) -> bool:
        """Filter out .humbug directory and other hidden files."""
        # If no workspace is open, don't show any files
        if not self._workspace_root:
            return False

        source_model = self.sourceModel()
        if not source_model:
            return False

        index = source_model.index(source_row, 0, source_parent)
        file_path = source_model.filePath(index)

        # Always hide .humbug directory
        if os.path.basename(file_path) == ".humbug":
            return False

        # Hide other hidden files/directories
        if os.path.basename(file_path).startswith("."):
            return False

        return True

    def lessThan(self, left: QModelIndex, right: QModelIndex) -> bool:
        """Sort directories before files, then alphabetically."""
        source_model = self.sourceModel()

        # Get file info for both indexes
        left_info = source_model.fileInfo(left)
        right_info = source_model.fileInfo(right)

        # Directories come before files
        if left_info.isDir() and not right_info.isDir():
            return True
        if not left_info.isDir() and right_info.isDir():
            return False

        # Otherwise sort alphabetically
        return left_info.fileName().lower() < right_info.fileName().lower()


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
        self._tree_view = QTreeView()
        self._tree_view.setHeaderHidden(True)
        self._tree_view.setAnimated(True)
        self._tree_view.setSortingEnabled(True)

        # Create file system model
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
        icon_dir = os.path.expanduser("~/.humbug/icons")
        theme = "dark" if self._style_manager.color_mode == ColorMode.DARK else "light"

        self.setStyleSheet(f"""
            QTreeView {{
                background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_SECONDARY)};
                border: none;
            }}
            QTreeView::item {{
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                padding: 4px;
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
                image: url("{icon_dir}/arrow-collapsed-{theme}.svg");
            }}
            QTreeView::branch:open:has-children:!has-siblings,
            QTreeView::branch:open:has-children:has-siblings {{
                image: url("{icon_dir}/arrow-expanded-{theme}.svg");
            }}
            QScrollBar:vertical {{
                background-color: {self._style_manager.get_color_str(ColorRole.SCROLLBAR_BACKGROUND)};
                width: 12px;
            }}
            QScrollBar::handle:vertical {{
                background-color: {self._style_manager.get_color_str(ColorRole.SCROLLBAR_HANDLE)};
                min-height: 20px;
            }}
            QScrollBar::add-line:vertical, QScrollBar::sub-line:vertical {{
                height: 0px;
            }}
        """)
