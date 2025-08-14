"""Conversations tree view implementation for mindspace conversations with drag and drop support and inline editing."""

from typing import cast, Callable

from PySide6.QtWidgets import QWidget, QFileSystemModel
from PySide6.QtCore import QSortFilterProxyModel, QDir

from humbug.mindspace.mindspace_tree_view import MindspaceTreeView


class MindspaceConversationsTreeView(MindspaceTreeView):
    """Custom tree view for conversations with drag and drop support, auto-scroll, and inline editing."""

    def __init__(self, parent: QWidget | None = None):
        """Initialize the conversations tree view."""
        super().__init__(parent)
        self._conversations_path: str = ""

    def get_root_path(self) -> str:
        """
        Get the root path for this tree view.

        Returns:
            Conversations root path string, or empty string if no conversations path is configured
        """
        return self._conversations_path

    def is_valid_drag_source(self, path: str) -> bool:
        """
        Check if a path can be dragged from this tree view.

        Conversations tree view allows all items to be dragged.

        Args:
            path: Path to check for drag validity

        Returns:
            True if the path can be dragged, False otherwise
        """
        # All items in conversations can be dragged
        return True

    def configure_for_path(self, path: str) -> None:
        """
        Configure the tree view for the given conversations path.

        Args:
            path: Conversations path to configure the tree view for
        """
        self._conversations_path = path

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
