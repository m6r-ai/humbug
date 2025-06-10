"""File tree view implementation for mindspace files."""

import os
from typing import cast

from PySide6.QtCore import QModelIndex, QSortFilterProxyModel, QPersistentModelIndex
from PySide6.QtWidgets import QFileSystemModel, QWidget


class MindspaceFileModel(QSortFilterProxyModel):
    """Filter model to hide .humbug directory and apply custom sorting."""

    def __init__(self, parent: QWidget | None = None):
        """Initialize the filter model."""
        super().__init__(parent)
        self._mindspace_root = ""

    def set_mindspace_root(self, path: str) -> None:
        """Set the mindspace root path for relative path calculations."""
        self._mindspace_root = path
        self.invalidateFilter()

    def filterAcceptsRow(self, source_row: int, source_parent: QModelIndex | QPersistentModelIndex) -> bool:
        """Filter out .humbug directory."""
        # If no mindspace is open, don't show any files
        if not self._mindspace_root:
            return False

        source_model = cast(QFileSystemModel, self.sourceModel())
        if not source_model:
            return False

        index = source_model.index(source_row, 0, source_parent)
        file_path = source_model.filePath(index)

        # Get the parent directory path
        parent_path = source_model.filePath(source_parent) if source_parent.isValid() else ""

        # If we're at the root level (parent of mindspace), only show the mindspace directory
        mindspace_parent = os.path.dirname(self._mindspace_root)
        if parent_path == mindspace_parent:
            return file_path == self._mindspace_root

        # For items within the mindspace, apply normal filtering
        # Always hide .humbug directory
        if os.path.basename(file_path) == ".humbug":
            return False

        return True

    def lessThan(
        self,
        source_left: QModelIndex | QPersistentModelIndex,
        source_right: QModelIndex | QPersistentModelIndex
    ) -> bool:
        """Sort directories before files, then alphabetically."""
        source_model = cast(QFileSystemModel, self.sourceModel())
        if not source_model:
            return False

        # Get file info for both indexes
        left_info = source_model.fileInfo(source_left)
        right_info = source_model.fileInfo(source_right)

        # Directories come before files
        if left_info.isDir() and not right_info.isDir():
            return True

        if not left_info.isDir() and right_info.isDir():
            return False

        # Otherwise sort alphabetically
        return left_info.fileName().lower() < right_info.fileName().lower()
