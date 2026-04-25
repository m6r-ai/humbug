"""Conversations tree view implementation for mindspace conversations with drag and drop support and inline editing."""

import os
from typing import Callable

from PySide6.QtCore import QModelIndex, QTimer
from PySide6.QtWidgets import QWidget

from humbug.mindspace.conversations.mindspace_conversations_dag_model import MindspaceConversationsDAGModel
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

        Args:
            index: The model index to get the path for

        Returns:
            File system path if valid, None otherwise
        """
        if not index.isValid():
            return None

        dag_model = self.model()
        if not isinstance(dag_model, MindspaceConversationsDAGModel):
            return None

        return dag_model.path_for_index(index)

    def ensure_path_visible_for_editing(self, file_path: str, callback: Callable) -> None:
        """
        Ensure the specified file path is visible and optimally positioned for editing.

        Args:
            file_path: Absolute path to the file to make visible
            callback: Callback to execute after the item is visible
        """
        dag_model = self.model()
        if not isinstance(dag_model, MindspaceConversationsDAGModel):
            return

        index = dag_model.index_for_path(file_path)
        if not index.isValid():
            return

        # Expand all parents
        parent = index.parent()
        parents = []
        while parent.isValid():
            parents.append(parent)
            parent = parent.parent()

        for p in reversed(parents):
            if not self.isExpanded(p):
                self.expand(p)

        QTimer.singleShot(200, lambda: self._scroll_to_and_edit(index, file_path, callback))

    def _scroll_to_and_edit(self, index: QModelIndex, file_path: str, callback: Callable) -> None:
        """
        Scroll to index and invoke callback.

        Args:
            index: Model index to scroll to
            file_path: Path for viewport position check
            callback: Callback to invoke after scrolling
        """
        viewport_rect = self.viewport().rect()
        item_rect = self.visualRect(index)
        margin = 40
        is_visible = (
            item_rect.top() >= margin and
            item_rect.bottom() <= viewport_rect.height() - margin
        )
        if not is_visible:
            self.scrollTo(index, self.ScrollHint.PositionAtCenter)
            QTimer.singleShot(100, callback)
        else:
            callback()
