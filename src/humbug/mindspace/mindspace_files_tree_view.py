"""File tree view implementation for mindspace files with drag and drop support and inline editing."""

import os
from typing import cast, Callable

from PySide6.QtWidgets import QWidget, QFileSystemModel
from PySide6.QtCore import QSortFilterProxyModel, QDir

from humbug.mindspace.mindspace_base_tree_view import MindspaceBaseTreeView


class MindspaceFilesTreeView(MindspaceBaseTreeView):
    """Custom tree view with drag and drop support, auto-scroll, and inline editing."""

    def __init__(self, parent: QWidget | None = None):
        """Initialize the tree view."""
        super().__init__(parent)
        self._mindspace_path: str = ""

    def get_root_path(self) -> str:
        """
        Get the root path for this tree view.

        Returns:
            Mindspace root path string, or empty string if no mindspace is configured
        """
        return self._mindspace_path

    def is_valid_drag_source(self, path: str) -> bool:
        """
        Check if a path can be dragged from this tree view.

        Files tree view prevents dragging of special folders.

        Args:
            path: Path to check for drag validity

        Returns:
            True if the path can be dragged, False otherwise
        """
        # Check if this is a valid drag target
        source_basename = os.path.basename(path)
        if source_basename in ['.humbug', 'conversations', 'metaphor']:
            return False

        return True

    def configure_for_path(self, path: str) -> None:
        """
        Configure the tree view for the given mindspace path.

        Args:
            path: Mindspace path to configure the tree view for
        """
        self._mindspace_path = path

    def get_path_from_index(self, index) -> str | None:
        """
        Get the file system path from a model index.

        Args:
            index: The model index to get the path for

        Returns:
            File system path if valid, None otherwise
        """
        if not index.isValid():
            return None

        source_model = cast(QSortFilterProxyModel, self.model())
        if not source_model:
            return None

        source_index = source_model.mapToSource(index)
        file_model = cast(QFileSystemModel, source_model.sourceModel())
        if not file_model:
            return None

        return QDir.toNativeSeparators(file_model.filePath(source_index))
