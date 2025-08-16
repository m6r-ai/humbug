"""File tree view implementation for mindspace files."""

import os
from typing import cast

from PySide6.QtCore import QModelIndex, QSortFilterProxyModel, QPersistentModelIndex, Qt
from PySide6.QtWidgets import QFileSystemModel, QWidget


class MindspaceFilesModel(QSortFilterProxyModel):
    """Filter model to hide .humbug directory and apply custom sorting."""

    def __init__(self, parent: QWidget | None = None):
        """Initialize the filter model."""
        super().__init__(parent)
        self._mindspace_root = ""

    def set_mindspace_root(self, path: str) -> None:
        """Set the mindspace root path for relative path calculations."""
        self._mindspace_root = path
        self.invalidateFilter()

    def flags(self, index: QModelIndex | QPersistentModelIndex) -> Qt.ItemFlag:
        """Return the item flags for the given index, making all items editable."""
        base_flags = super().flags(index)

        # Make all items editable so Qt's editing system works
        if index.isValid():
            return base_flags | Qt.ItemFlag.ItemIsEditable

        return base_flags

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

        # Both are same type (both directories or both files), sort alphabetically
        return left_info.fileName().lower() < right_info.fileName().lower()
