"""Custom item delegate for mindspace file tree with inline editing and drop target support."""

import os

from humbug.mindspace.mindspace_base_delegate import MindspaceBaseDelegate
from humbug.mindspace.mindspace_files_tree_view import MindspaceFilesTreeView
from humbug.style_manager import StyleManager


class MindspaceFilesDelegate(MindspaceBaseDelegate):
    """Custom item delegate for files tree view that provides visual feedback for drop targets and handles inline editing."""

    def __init__(self, tree_view: MindspaceFilesTreeView, style_manager: StyleManager):
        """
        Initialize the delegate.

        Args:
            tree_view: The tree view this delegate is attached to
            style_manager: Style manager for accessing theme colors
        """
        super().__init__(tree_view, style_manager)

    def get_tree_view_type(self) -> type:
        """
        Get the expected tree view type for type checking.

        Returns:
            The expected tree view class type
        """
        return MindspaceFilesTreeView

    def validate_new_name(self, index, new_name: str) -> tuple[bool, str]:
        """
        Validate a new name for uniqueness.

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

            # Check if a file with this name already exists
            new_path = os.path.join(directory, new_name)
            if os.path.exists(new_path) and not os.path.samefile(new_path, file_path):
                return False, self._language_manager.strings().rename_error_exists

            return True, ""

        except Exception:
            return False, self._language_manager.strings().error_validation_failed
