"""File tree view implementation for mindspace files."""

import os
from typing import cast

from PySide6.QtCore import QModelIndex, QSortFilterProxyModel, QPersistentModelIndex
from PySide6.QtWidgets import QFileSystemModel


class MindspaceFileModel(QSortFilterProxyModel):
    """Filter model to hide .humbug directory and apply custom sorting."""

    def __init__(self, parent=None):
        """Initialize the filter model."""
        super().__init__(parent)
        self._mindspace_root = None

    def set_mindspace_root(self, path: str):
        """Set the mindspace root path for relative path calculations."""
        self._mindspace_root = path
        self.invalidateFilter()

    def filterAcceptsRow(self, source_row: int, source_parent: QModelIndex | QPersistentModelIndex) -> bool:
        """Filter out .humbug directory and other hidden files."""
        # If no mindspace is open, don't show any files
        if not self._mindspace_root:
            return False

        source_model = cast(QFileSystemModel, self.sourceModel())
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

    def lessThan(self, left: QModelIndex | QPersistentModelIndex, right: QModelIndex | QPersistentModelIndex) -> bool:
        """Sort directories before files, then alphabetically."""
        source_model = cast(QFileSystemModel, self.sourceModel())
        if not source_model:
            return False

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
