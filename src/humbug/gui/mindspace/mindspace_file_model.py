"""File tree view implementation for mindspace files."""

import os
from enum import Enum
from typing import cast

from PySide6.QtCore import QModelIndex, QSortFilterProxyModel, QPersistentModelIndex
from PySide6.QtWidgets import QFileSystemModel, QWidget


class MindspaceFileModel(QSortFilterProxyModel):
    """Filter model to hide .humbug directory and apply custom sorting."""

    class SortMode(Enum):
        """Sorting modes for conversation files."""
        NAME = "name"
        CREATION_TIME = "creation_time"

    def __init__(self, parent: QWidget | None = None):
        """Initialize the filter model."""
        super().__init__(parent)
        self._mindspace_root = ""
        self._conversation_sort_mode = self.SortMode.CREATION_TIME
        self._conversations_path = ""  # Cache conversations folder path

    def set_mindspace_root(self, path: str) -> None:
        """Set the mindspace root path for relative path calculations."""
        self._mindspace_root = path

        # Cache conversations folder path for efficient sorting checks
        if path:
            conversations_path = os.path.join(path, "conversations")
            self._conversations_path = conversations_path if os.path.exists(conversations_path) else ""

        else:
            self._conversations_path = ""

        self.invalidateFilter()

    def set_conversation_sort_mode(self, mode: SortMode) -> None:
        """
        Set sorting mode for conversation files.

        Args:
            mode: The sorting mode to apply to files in the conversations folder
        """
        if self._conversation_sort_mode != mode:
            self._conversation_sort_mode = mode
            self.invalidate()  # Trigger resort

    def get_conversation_sort_mode(self) -> SortMode:
        """
        Get current conversation sorting mode.

        Returns:
            Current sorting mode for conversation files
        """
        return self._conversation_sort_mode

    def _is_in_conversations_folder(self, file_path: str) -> bool:
        """
        Check if a file is directly in the conversations folder.

        Args:
            file_path: Path to check

        Returns:
            True if file is directly in conversations folder, False otherwise
        """
        if not self._conversations_path or not file_path:
            return False

        return os.path.dirname(os.path.normpath(file_path)) == os.path.normpath(self._conversations_path)

    def _get_file_creation_time(self, file_path: str) -> float:
        """
        Get file creation time, with platform-appropriate fallbacks.

        Args:
            file_path: Path to the file

        Returns:
            Creation time as timestamp, or 0.0 if unable to determine
        """
        try:
            stat_info = os.stat(file_path)

            # Use the most appropriate time field based on platform
            # st_birthtime (macOS) > st_ctime (Windows/Unix) > st_mtime (fallback)
            if hasattr(stat_info, 'st_birthtime') and stat_info.st_birthtime > 0:
                return stat_info.st_birthtime

            if hasattr(stat_info, 'st_ctime'):
                return stat_info.st_ctime

            return stat_info.st_mtime

        except OSError:
            # Return 0 if we can't get the time (file doesn't exist, permission error, etc.)
            return 0.0

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
        """Sort directories before files, then apply conversation-specific sorting or alphabetically."""
        source_model = cast(QFileSystemModel, self.sourceModel())
        if not source_model:
            return False

        # Get file info for both indexes
        left_info = source_model.fileInfo(source_left)
        right_info = source_model.fileInfo(source_right)
        left_path = source_model.filePath(source_left)
        right_path = source_model.filePath(source_right)

        # Check if both files are in conversations folder
        left_in_conversations = self._is_in_conversations_folder(left_path)
        right_in_conversations = self._is_in_conversations_folder(right_path)

        # Apply creation time sorting for conversation files (non-directories only)
        if (left_in_conversations and right_in_conversations and
            self._conversation_sort_mode == self.SortMode.CREATION_TIME and
            not left_info.isDir() and not right_info.isDir()):

            left_time = self._get_file_creation_time(left_path)
            right_time = self._get_file_creation_time(right_path)

            # Sort newest first (reverse chronological order)
            return left_time > right_time

        # Default sorting logic: directories come before files
        if left_info.isDir() and not right_info.isDir():
            return True

        if not left_info.isDir() and right_info.isDir():
            return False

        # Both are same type (both directories or both files), sort alphabetically
        return left_info.fileName().lower() < right_info.fileName().lower()
