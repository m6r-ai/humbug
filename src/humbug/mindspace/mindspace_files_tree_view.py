"""File tree view implementation for mindspace files with drag and drop support and inline editing."""

import os
from typing import cast

from PySide6.QtCore import QSortFilterProxyModel, QDir, QModelIndex
from PySide6.QtWidgets import QWidget, QFileSystemModel

from humbug.mindspace.mindspace_tree_view import MindspaceTreeView


class MindspaceFilesTreeView(MindspaceTreeView):
    """Custom tree view with drag and drop support, auto-scroll, and inline editing."""

    def __init__(self, parent: QWidget | None = None):
        """Initialize the tree view."""
        super().__init__(parent)
        self._mindspace_path: str = ""

        # Connect to expansion/collapse signals to update size
        self.expanded.connect(self.update_size)
        self.collapsed.connect(self.update_size)

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

    def get_view_type(self) -> str:
        """
        Get the type identifier for this view.

        Returns:
            String identifying this as the files view
        """
        return "files"

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

        source_model = cast(QSortFilterProxyModel, self.model())
        if not source_model:
            return None

        source_index = source_model.mapToSource(index)
        file_model = cast(QFileSystemModel, source_model.sourceModel())
        if not file_model:
            return None

        return QDir.toNativeSeparators(file_model.filePath(source_index))

    def setModel(self, model):
        super().setModel(model)
        if model:
            model.dataChanged.connect(self.update_size)
            model.rowsInserted.connect(self.update_size)
            model.rowsRemoved.connect(self.update_size)

    def showEvent(self, event):
        """Handle the show event to update the size of the tree view."""
        super().showEvent(event)
        self.update_size()

    def update_size(self):
        """Update the fixed height of the tree view based on visible content."""
        if not self.model():
            return

        # Force the view to layout its items
        self.doItemsLayout()

        # Calculate total height
        total_height = 0
        model = self.model()

        # Simple calculation: row height * visible row count
        if model.rowCount() > 0:
            # Get the height of the first row as a reference
            first_index = model.index(0, 0)
            if first_index.isValid():
                row_height = self.sizeHintForIndex(first_index).height()
                visible_rows = self.get_visible_row_count()
                total_height = row_height * visible_rows

        # Add header height if visible
        if self.header().isVisible():
            total_height += self.header().height()

        # Set a minimum height to avoid completely collapsing
        min_height = 50  # Minimum height in pixels
        total_height = max(total_height, min_height)

        self.setFixedHeight(total_height)

    def get_visible_row_count(self):
        """Count all visible rows including expanded children."""
        if not self.model():
            return 0

        def count_visible(parent_index):
            """Recursively count visible rows under a parent index."""
            count = 0
            row_count = self.model().rowCount(parent_index)

            for row in range(row_count):
                count += 1  # Count this row
                index = self.model().index(row, 0, parent_index)

                # If this row is expanded, count its children too
                if self.isExpanded(index):
                    count += count_visible(index)

            return count

        return count_visible(self.rootIndex())
