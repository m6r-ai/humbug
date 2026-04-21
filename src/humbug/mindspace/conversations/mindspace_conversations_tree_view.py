"""Conversations tree view implementation for mindspace conversations with drag and drop support and inline editing."""

import os
from typing import Callable, cast

from PySide6.QtCore import QSortFilterProxyModel, QModelIndex, QTimer
from PySide6.QtWidgets import QWidget, QTreeView

from humbug.mindspace.conversations.mindspace_conversations_hierarchy_model import (
    MindspaceConversationsHierarchyModel,
    MindspaceConversationsSortProxy,
)
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
        if os.path.basename(path) == ".":
            return False

        return True

    def configure_for_path(self, path: str) -> None:
        """
        Configure the tree view for the given conversations path.

        Args:
            path: Conversations path to configure the tree view for
        """
        self._conversations_path = path

    def get_view_type(self) -> str:
        """
        Get the type identifier for this view.

        Returns:
            String identifying this as the conversations view
        """
        return "conversations"

    def get_path_from_index(self, index: QModelIndex) -> str | None:
        """
        Get the file system path from a model index.

        Supports both the new MindspaceConversationsHierarchyModel (via
        MindspaceConversationsSortProxy) and the legacy QFileSystemModel stack.

        Args:
            index: The model index to get the path for

        Returns:
            File system path if valid, None otherwise
        """
        if not index.isValid():
            return None

        proxy = self.model()
        if not isinstance(proxy, QSortFilterProxyModel):
            return None

        source_index = proxy.mapToSource(index)
        if not source_index.isValid():
            return None

        source = proxy.sourceModel()
        if isinstance(source, MindspaceConversationsHierarchyModel):
            path = source.file_path(source_index)
            return path if path else None

        # Fallback: legacy QFileSystemModel path
        from PySide6.QtCore import QDir
        from PySide6.QtWidgets import QFileSystemModel
        file_model = cast(QFileSystemModel, source)
        return QDir.toNativeSeparators(file_model.filePath(source_index))

    def _get_hierarchy_model(self) -> MindspaceConversationsHierarchyModel | None:
        proxy = self.model()
        if not isinstance(proxy, QSortFilterProxyModel):
            return None
        source = proxy.sourceModel()
        return source if isinstance(source, MindspaceConversationsHierarchyModel) else None

    def scroll_to_and_ensure_visible(self, file_path: str, callback: Callable) -> None:
        """Override to support hierarchy model's index_for_path."""
        hierarchy = self._get_hierarchy_model()
        if hierarchy is None:
            super().scroll_to_and_ensure_visible(file_path, callback)
            return

        proxy = cast(QSortFilterProxyModel, self.model())
        source_index = hierarchy.index_for_path(file_path)
        if not source_index.isValid():
            return

        filter_index = proxy.mapFromSource(source_index)
        if not filter_index.isValid():
            return

        viewport_rect = self.viewport().rect()
        item_rect = self.visualRect(filter_index)

        margin = 40
        is_optimally_visible = (
            item_rect.top() >= margin
            and item_rect.bottom() <= viewport_rect.height() - margin
            and item_rect.left() >= 0
            and item_rect.right() <= viewport_rect.width()
        )
        if is_optimally_visible:
            callback()
            return

        self.scrollTo(filter_index, QTreeView.ScrollHint.PositionAtCenter)
        QTimer.singleShot(100, callback)

    def ensure_path_visible_for_editing(self, file_path: str, callback: Callable) -> None:
        """Override to support hierarchy model's index_for_path."""
        hierarchy = self._get_hierarchy_model()
        if hierarchy is None:
            super().ensure_path_visible_for_editing(file_path, callback)
            return

        proxy = cast(QSortFilterProxyModel, self.model())
        source_index = hierarchy.index_for_path(file_path)
        if not source_index.isValid():
            return

        filter_index = proxy.mapFromSource(source_index)
        if not filter_index.isValid():
            return

        # Expand all ancestors
        parents = []
        current = filter_index.parent()
        while current.isValid():
            parents.append(current)
            current = current.parent()

        for parent in reversed(parents):
            if not self.isExpanded(parent):
                self.expand(parent)

        QTimer.singleShot(200, lambda: self.scroll_to_and_ensure_visible(file_path, callback))
