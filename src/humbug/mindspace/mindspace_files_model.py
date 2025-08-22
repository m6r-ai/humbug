"""File tree view implementation for mindspace files."""

import os
from typing import cast

from PySide6.QtCore import QModelIndex, QSortFilterProxyModel, QPersistentModelIndex, Qt
from PySide6.QtWidgets import QFileSystemModel, QWidget

from humbug.user.user_file_sort_order import UserFileSortOrder
from humbug.user.user_manager import UserManager


class MindspaceFilesModel(QSortFilterProxyModel):
    """Filter model to hide .humbug directory and apply custom sorting."""

    def __init__(self, parent: QWidget | None = None):
        """Initialize the filter model."""
        super().__init__(parent)
        self._mindspace_root = ""
        self._user_manager = UserManager()
        self._user_manager.settings_changed.connect(self._on_user_settings_changed)

    def _on_user_settings_changed(self) -> None:
        """Handle user settings changes by re-sorting."""
        self.invalidate()  # This triggers a resort

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
        file_name = source_model.fileName(index)
        if file_name == ".":
            # Only show current directory at the mindspace root level
            # Check if the parent of this "." entry is the mindspace root
            parent_path = source_model.filePath(source_parent) if source_parent.isValid() else ""

            # Normalize paths for comparison
            normalized_parent = os.path.normpath(parent_path) if parent_path else ""
            normalized_mindspace = os.path.normpath(self._mindspace_root)

            # Only show "." if we're at the mindspace root level
            return normalized_parent == normalized_mindspace

        # Always hide .humbug directory
        file_path = source_model.filePath(index)
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

        # Get file names for both indexes
        left_name = source_model.fileName(source_left)
        right_name = source_model.fileName(source_right)

        # "." always sorts to the top
        if left_name == ".":
            return True

        if right_name == ".":
            return False

        # Get file info for both indexes
        left_info = source_model.fileInfo(source_left)
        right_info = source_model.fileInfo(source_right)

        # Check user sort preference
        sort_order = self._user_manager.settings().file_sort_order
        if sort_order == UserFileSortOrder.ALPHABETICAL:
            # Skip directory/file separation, just sort alphabetically
            return left_info.fileName().lower() < right_info.fileName().lower()

        # Default directories-first logic
        # Directories come before files
        if left_info.isDir() and not right_info.isDir():
            return True

        if not left_info.isDir() and right_info.isDir():
            return False

        # Both are same type (both directories or both files), sort alphabetically
        return left_info.fileName().lower() < right_info.fileName().lower()
