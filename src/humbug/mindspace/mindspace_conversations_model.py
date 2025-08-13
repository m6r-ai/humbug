"""Conversations model implementation for mindspace conversations."""

from datetime import datetime
from enum import Enum
import json
import os
from typing import cast

from PySide6.QtCore import QModelIndex, QSortFilterProxyModel, QPersistentModelIndex
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

    def set_conversations_root(self, path: str) -> None:
        """Set the conversations root path for filtering."""
        self._conversations_root = path
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
        Get file creation time, with conversation-aware fallbacks.

        Args:
            file_path: Path to the file

        Returns:
            Creation time as timestamp, or 0.0 if unable to determine
        """
        # For conversation files, try to get timestamp from content first
        if self._is_conversation_file(file_path):
            conversation_time = self._get_conversation_timestamp(file_path)
            if conversation_time > 0:
                return conversation_time

        # Fall back to filesystem timestamps
        try:
            stat_info = os.stat(file_path)

            # Use the most appropriate time field based on platform
            # st_birthtime (macOS) > st_ctime (Windows/Unix) > st_mtime (fallback)
            if hasattr(stat_info, 'st_birthtime') and stat_info.st_birthtime > 0:  # type: ignore[attr-defined]
                return stat_info.st_birthtime  # type: ignore[attr-defined]

            if hasattr(stat_info, 'st_ctime'):  # type: ignore[attr-defined]
                return stat_info.st_ctime

            return stat_info.st_mtime

        except OSError:
            # Return 0 if we can't get the time (file doesn't exist, permission error, etc.)
            return 0.0

    def filterAcceptsRow(self, source_row: int, source_parent: QModelIndex | QPersistentModelIndex) -> bool:
        """Filter to show conversations directory and its contents, following files view pattern."""
        # If no conversations root is set, don't show any files
        if not self._conversations_root:
            return False

        source_model = cast(QFileSystemModel, self.sourceModel())
        if not source_model:
            return False

        index = source_model.index(source_row, 0, source_parent)
        file_path = source_model.filePath(index)

        # Get the parent directory path
        parent_path = source_model.filePath(source_parent) if source_parent.isValid() else ""

        # If we're at the root level (parent of conversations), only show the conversations directory
        conversations_parent = os.path.dirname(self._conversations_root)
        if parent_path == conversations_parent:
            return file_path == self._conversations_root

        # For items within the conversations directory, show all items
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

        # Check if both files are in conversations hierarchy
        left_in_conversations = self._is_in_conversations_hierarchy(left_path)
        right_in_conversations = self._is_in_conversations_hierarchy(right_path)

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

    def _is_in_conversations_hierarchy(self, file_path: str) -> bool:
        """
        Check if the given path is anywhere within the conversations folder hierarchy.

        Args:
            file_path: Path to check

        Returns:
            True if this path is within the conversations folder hierarchy, False otherwise
        """
        if not self._conversations_root or not file_path:
            return False

        normalized_file = os.path.normpath(file_path)
        normalized_conversations = os.path.normpath(self._conversations_root)

        return normalized_file.startswith(normalized_conversations + os.sep) or normalized_file == normalized_conversations
