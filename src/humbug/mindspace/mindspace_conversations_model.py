"""Conversations model implementation for mindspace conversations."""

from datetime import datetime
from enum import Enum
import json
import os
from typing import cast

from PySide6.QtCore import QModelIndex, QSortFilterProxyModel, QPersistentModelIndex, Qt
from PySide6.QtWidgets import QFileSystemModel, QWidget


class MindspaceConversationsModel(QSortFilterProxyModel):
    """Filter model for conversations directory with custom sorting."""

    class SortMode(Enum):
        """Sorting modes for conversation files."""
        NAME = "name"
        CREATION_TIME = "creation_time"

    def __init__(self, parent: QWidget | None = None):
        """Initialize the conversations model."""
        super().__init__(parent)
        self._conversations_root = ""
        self._conversation_sort_mode = self.SortMode.CREATION_TIME  # Default to creation time

        # Cache for file creation times: {file_path: (cached_timestamp, file_mtime)}
        self._creation_time_cache: dict[str, tuple[float, float]] = {}

    def set_conversations_root(self, path: str) -> None:
        """Set the conversations root path for filtering."""
        self._conversations_root = path

        # Clear cache when conversations root changes
        self._creation_time_cache.clear()
        self.invalidateFilter()

    def set_conversation_sort_mode(self, mode: SortMode) -> None:
        """
        Set sorting mode for conversation files.

        Args:
            mode: The sorting mode to apply to files
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

    def flags(self, index: QModelIndex | QPersistentModelIndex) -> Qt.ItemFlag:
        """Return the item flags for the given index, making all items editable."""
        base_flags = super().flags(index)

        # Make all items editable so Qt's editing system works
        if index.isValid():
            return base_flags | Qt.ItemFlag.ItemIsEditable

        return base_flags

    def clear_creation_time_cache(self) -> None:
        """
        Clear the creation time cache.

        This can be called when files are known to have changed externally
        or when a full refresh is needed.
        """
        self._creation_time_cache.clear()

    def _is_conversation_file(self, file_path: str) -> bool:
        """
        Check if a file appears to be a conversation file based on location and extension.

        Args:
            file_path: Path to check

        Returns:
            True if this appears to be a conversation file
        """
        if not self._conversations_root or not file_path:
            return False

        # Check for .conv extension or other conversation file patterns
        _, ext = os.path.splitext(file_path.lower())
        return ext in ['.conv', '.conversation', '.json']  # Add other extensions as needed

    def _get_conversation_timestamp(self, file_path: str) -> float:
        """
        Get timestamp from the first message in a conversation file.

        Args:
            file_path: Path to the conversation file

        Returns:
            Timestamp as float, or 0.0 if unable to parse
        """
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                data = json.load(f)

            # Look for conversation structure
            messages = []
            if isinstance(data, dict):
                # Check for 'conversation' key with list of messages
                if 'conversation' in data and isinstance(data['conversation'], list):
                    messages = data['conversation']

            # Check if the root is a list of messages
            elif isinstance(data, list):
                messages = data

            # Find the first message with a timestamp
            for message in messages:
                if isinstance(message, dict) and 'timestamp' in message:
                    timestamp_str = message['timestamp']
                    try:
                        # Parse ISO format timestamp
                        if timestamp_str.endswith('Z'):
                            timestamp_str = timestamp_str[:-1] + '+00:00'

                        dt = datetime.fromisoformat(timestamp_str)
                        return dt.timestamp()

                    except (ValueError, AttributeError):
                        continue

        except (OSError, json.JSONDecodeError, KeyError):
            pass

        return 0.0

    def _get_file_creation_time(self, file_path: str) -> float:
        """
        Get file creation time, with conversation-aware fallbacks and caching.

        Args:
            file_path: Path to the file

        Returns:
            Creation time as timestamp, or 0.0 if unable to determine
        """
        try:
            # Get current file modification time for cache validation
            stat_info = os.stat(file_path)
            current_mtime = stat_info.st_mtime

            # Check if we have a cached result and if it's still valid
            if file_path in self._creation_time_cache:
                cached_timestamp, cached_mtime = self._creation_time_cache[file_path]
                if cached_mtime == current_mtime:
                    # Cache is still valid, return cached result
                    return cached_timestamp

            # Cache miss or invalid - compute the creation time
            creation_time = 0.0

            # For conversation files, try to get timestamp from content first
            if self._is_conversation_file(file_path):
                conversation_time = self._get_conversation_timestamp(file_path)
                if conversation_time > 0:
                    creation_time = conversation_time

                else:
                    # Fall back to filesystem timestamps
                    creation_time = self._get_filesystem_creation_time(stat_info)

            else:
                # Non-conversation file - use filesystem timestamps
                creation_time = self._get_filesystem_creation_time(stat_info)

            # Cache the result
            self._creation_time_cache[file_path] = (creation_time, current_mtime)
            return creation_time

        except OSError:
            # Return 0 if we can't get the time (file doesn't exist, permission error, etc.)
            return 0.0

    def _get_filesystem_creation_time(self, stat_info: os.stat_result) -> float:
        """
        Get file creation time from filesystem stat information.

        Args:
            stat_info: Result from os.stat() call

        Returns:
            Creation time as timestamp
        """
        # Use the most appropriate time field based on platform
        # st_birthtime (macOS) > st_ctime (Windows/Unix) > st_mtime (fallback)
        if hasattr(stat_info, 'st_birthtime') and stat_info.st_birthtime > 0:  # type: ignore[attr-defined]
            return stat_info.st_birthtime  # type: ignore[attr-defined]

        if hasattr(stat_info, 'st_ctime'):  # type: ignore[attr-defined]
            return stat_info.st_ctime

        return stat_info.st_mtime

    def filterAcceptsRow(self, _source_row: int, _source_parent: QModelIndex | QPersistentModelIndex) -> bool:
        """Show all files within the conversations directory."""
        # If no conversations root is set, don't show any files
        if not self._conversations_root:
            return False

        # we can show all items (no special filtering needed)
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

        # Apply creation time sorting for conversation files (non-directories only)
        if (self._conversation_sort_mode == self.SortMode.CREATION_TIME and
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
