"""Specialized delegate for conversations tree view with conversation file extension handling."""

import os

from PySide6.QtCore import QModelIndex

from humbug.mindspace.mindspace_tree_delegate import MindspaceTreeDelegate


class MindspaceConversationsTreeDelegate(MindspaceTreeDelegate):
    """Specialized tree delegate for conversations that handles conversation file extension preservation."""

    def _is_conversation_file(self, file_path: str) -> bool:
        """
        Check if a file is a conversation file based on its extension.

        Args:
            file_path: Path to check

        Returns:
            True if this is a conversation file (.conv or .json)
        """
        if not file_path:
            return False

        if not os.path.isfile(file_path):
            return False

        _, ext = os.path.splitext(file_path.lower())
        return ext in ['.conv', '.json']

    def _get_original_extension(self, file_path: str) -> str:
        """
        Get the original extension from a conversation file.

        Args:
            file_path: Path to the conversation file

        Returns:
            The extension (including the dot) or empty string if not a conversation file
        """
        if not self._is_conversation_file(file_path):
            return ""

        _, ext = os.path.splitext(file_path)
        return ext

    def validate_new_name(self, index: QModelIndex, new_name: str) -> tuple[bool, str]:
        """
        Validate a new name for uniqueness, taking into account conversation file extension preservation.

        Args:
            index: Model index being edited
            new_name: Proposed new name

        Returns:
            Tuple of (is_valid, error_message)
        """
        try:
            # Get the file path from the tree view
            file_path = self._tree_view.get_path_from_index(index)
            if not file_path:
                return False, self._language_manager.strings().error_validation_failed

            # Get the directory
            directory = os.path.dirname(file_path)

            # Check if this is a conversation file that will have its extension preserved
            if self._is_conversation_file(file_path):
                original_extension = self._get_original_extension(file_path)
                # The final filename will have the original extension appended
                final_name = new_name + original_extension

            else:
                # Non-conversation file - use the name as-is
                final_name = new_name

            # Check if a file with the final name already exists
            new_path = os.path.join(directory, final_name)
            if os.path.exists(new_path) and not os.path.samefile(new_path, file_path):
                return False, self._language_manager.strings().rename_error_exists

            return True, ""

        except Exception:
            return False, self._language_manager.strings().error_validation_failed
