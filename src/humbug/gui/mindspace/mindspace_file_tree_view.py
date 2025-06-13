"""File tree view implementation for mindspace files with drag and drop support and inline editing."""

import os
from typing import cast, Callable

from PySide6.QtWidgets import QTreeView, QApplication, QWidget, QFileSystemModel
from PySide6.QtCore import Qt, QSortFilterProxyModel, QMimeData, QPoint, Signal, QModelIndex, QPersistentModelIndex, QTimer
from PySide6.QtGui import QDrag, QMouseEvent, QDragEnterEvent, QDragMoveEvent, QDropEvent, QDragLeaveEvent, QCursor

from humbug.mindspace.mindspace_file_watcher import MindspaceFileWatcher


class MindspaceFileTreeView(QTreeView):
    """Custom tree view with drag and drop support, auto-scroll, and inline editing."""

    file_dropped = Signal(str, str)  # dragged_path, target_path
    drop_target_changed = Signal()
    rename_requested = Signal(QModelIndex, str)  # index, new_name
    style_updated = Signal()  # Emitted after the tree view has processed style changes

    def __init__(self, parent: QWidget | None = None):
        """Initialize the tree view."""
        super().__init__(parent)
        self.setDragEnabled(True)
        self.setAcceptDrops(True)
        self.setDragDropMode(QTreeView.DragDropMode.DragDrop)
        self.setDefaultDropAction(Qt.DropAction.MoveAction)
        self._drag_start_pos: QPoint | None = None
        self._current_drop_target: QModelIndex | None = None

        # Auto-expand timer for drag operations
        self._auto_expand_timer = QTimer()
        self._auto_expand_timer.setSingleShot(True)
        self._auto_expand_timer.timeout.connect(self._handle_auto_expand_timeout)
        self._auto_expand_timer.setInterval(500)  # 0.5 seconds
        self._pending_expand_index: QModelIndex | None = None
        self._auto_opened_folders: set[QPersistentModelIndex] = set()

        # Auto-scroll timer for drag operations
        self._auto_scroll_timer = QTimer()
        self._auto_scroll_timer.timeout.connect(self._handle_auto_scroll_timeout)
        self._auto_scroll_timer.setInterval(50)  # 50ms for smooth scrolling
        self._scroll_direction: int = 0  # -1 for up, 1 for down, 0 for no scroll
        self._scroll_speed: int = 0  # Pixels to scroll per timer interval

        # Auto-scroll configuration
        self._scroll_zone_size = 25  # Pixels from edge to trigger scrolling
        self._min_scroll_speed = 2   # Minimum scroll speed
        self._max_scroll_speed = 10  # Maximum scroll speed

        # Use file watcher for network filesystem support
        self._file_watcher = MindspaceFileWatcher()
        self._monitored_directories: set[str] = set()

        self.setHeaderHidden(True)
        self.setAnimated(True)
        self.header().setSortIndicator(0, Qt.SortOrder.AscendingOrder)
        self.setSortingEnabled(True)
        self.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.setMouseTracking(True)
        self.setToolTipDuration(10000)

        # Connect to expansion/collapse events
        self.expanded.connect(self._handle_item_expanded)
        self.collapsed.connect(self._handle_item_collapsed)

    def _handle_item_expanded(self, index: QModelIndex) -> None:
        """Handle when a directory is expanded - start monitoring it."""
        dir_path = self.get_path_from_index(index)
        if dir_path and os.path.isdir(dir_path) and dir_path not in self._monitored_directories:
            self._file_watcher.watch_file(dir_path, self._handle_directory_changed)
            self._monitored_directories.add(dir_path)

    def _handle_item_collapsed(self, index: QModelIndex) -> None:
        """Handle when a directory is collapsed - stop monitoring it."""
        dir_path = self.get_path_from_index(index)
        if dir_path and dir_path in self._monitored_directories:
            self._file_watcher.unwatch_file(dir_path, self._handle_directory_changed)
            self._monitored_directories.discard(dir_path)

    def _handle_directory_changed(self, dir_path: str) -> None:
        """Handle directory content changes by refreshing the model."""
        source_model = cast(QSortFilterProxyModel, self.model())
        if not source_model:
            return

        file_model = cast(QFileSystemModel, source_model.sourceModel())
        if not file_model:
            return

        # Get the source index for the changed directory
        source_index = file_model.index(dir_path)
        if not source_index.isValid():
            return

        # Emit dataChanged signal to notify that this directory may have changed
        bottom_right = file_model.index(
            source_index.row(),
            file_model.columnCount(source_index.parent()) - 1,
            source_index.parent()
        )
        file_model.dataChanged.emit(source_index, bottom_right)

        # Invalidate the filter model to ensure it updates
        source_model.invalidateFilter()

    def clear_directory_monitoring(self) -> None:
        """Clear all directory monitoring when mindspace changes."""
        for dir_path in self._monitored_directories.copy():
            self._file_watcher.unwatch_file(dir_path, self._handle_directory_changed)

        self._monitored_directories.clear()

    def notify_style_updated(self) -> None:
        """
        Notify that this tree view has finished processing style updates.
        This should be called after the tree view has updated its layout, icon sizes, etc.
        """
        self.style_updated.emit()

    def get_current_drop_target(self) -> QModelIndex | None:
        """Get the current drop target index."""
        return self._current_drop_target

    def clear_drop_target(self) -> None:
        """Clear the current drop target and emit signal."""
        if self._current_drop_target is not None:
            self._current_drop_target = None
            self.drop_target_changed.emit()

    def _set_drop_target(self, index: QModelIndex) -> None:
        """Set the current drop target and emit signal if changed."""
        if self._current_drop_target is None or self._current_drop_target != index:
            self._current_drop_target = index
            self.drop_target_changed.emit()

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

        return file_model.filePath(source_index)

    def scroll_to_and_ensure_visible(self, index: QModelIndex, callback: Callable) -> bool:
        """
        Scroll to the specified index and ensure it's optimally positioned for editing.

        Args:
            index: The model index to scroll to
            callback: Callback to execute after scrolling is complete

        Returns:
            True if the index is valid and scrolling was initiated, False otherwise

        Raises:
            ValueError: If the index is invalid
        """
        if not index.isValid():
            raise ValueError("Cannot scroll to invalid index")

        # Get the current viewport rect to determine optimal positioning
        viewport_rect = self.viewport().rect()
        item_rect = self.visualRect(index)

        # Check if item is already optimally visible (not at edges and fully visible).  If it is, we can
        # execute the callback immediately without scrolling.
        margin = 40  # Minimum margin from viewport edges
        is_optimally_visible = (
            item_rect.top() >= margin and
            item_rect.bottom() <= viewport_rect.height() - margin and
            item_rect.left() >= 0 and
            item_rect.right() <= viewport_rect.width()
        )
        if is_optimally_visible:
            callback()
            return True

        # Scroll to position the item optimally in the viewport
        # Use PositionAtCenter to ensure good visibility for editing
        self.scrollTo(index, QTreeView.ScrollHint.PositionAtCenter)

        # If we have a callback, execute it after a short delay to allow scrolling to complete
        QTimer.singleShot(50, callback)

        return True

    def ensure_path_visible_for_editing(self, file_path: str, callback: Callable) -> bool:
        """
        Ensure the specified file path is visible and optimally positioned for editing.

        Args:
            file_path: Absolute path to the file to make visible
            callback: Optional callback to execute after the item is visible

        Returns:
            True if the path was found and made visible, False otherwise
        """
        # Find the index for this path
        source_model = cast(QSortFilterProxyModel, self.model())
        if not source_model:
            return False

        # Get the source file system model
        file_model = cast(QFileSystemModel, source_model.sourceModel())
        if not file_model:
            return False

        # Get the source index for the file path
        source_index = file_model.index(file_path)
        if not source_index.isValid():
            return False

        # Map to the filter model
        filter_index = source_model.mapFromSource(source_index)
        if not filter_index.isValid():
            return False

        # Ensure parent directories are expanded
        self._ensure_parents_expanded(filter_index)

        # Scroll to make the item visible
        return self.scroll_to_and_ensure_visible(filter_index, callback)

    def _ensure_parents_expanded(self, index: QModelIndex) -> None:
        """
        Ensure all parent directories of the given index are expanded.

        Args:
            index: The model index whose parents should be expanded
        """
        if not index.isValid():
            return

        # Collect all parent indexes
        parents = []
        current = index.parent()
        while current.isValid():
            parents.append(current)
            current = current.parent()

        # Expand parents from root to leaf
        for parent in reversed(parents):
            if not self.isExpanded(parent):
                self.expand(parent)

    def _is_ancestor_path(self, potential_ancestor: str, path: str) -> bool:
        """
        Check if one path is an ancestor of another.

        Args:
            potential_ancestor: Path that might be an ancestor
            path: Path to check against

        Returns:
            True if potential_ancestor is an ancestor of path
        """
        if not potential_ancestor or not path:
            return False

        # Normalize paths to handle different separators and relative paths
        ancestor_norm = os.path.normpath(potential_ancestor)
        path_norm = os.path.normpath(path)

        # Add trailing separator to ancestor to ensure we match complete directory names
        if not ancestor_norm.endswith(os.sep):
            ancestor_norm += os.sep

        # Check if path starts with the ancestor path
        if not path_norm.endswith(os.sep):
            path_norm += os.sep

        return path_norm.startswith(ancestor_norm)

    def _is_parent_directory(self, potential_parent: str, child_path: str) -> bool:
        """
        Check if one path is the direct parent of another.

        Args:
            potential_parent: Path that might be the parent
            child_path: Path to check against

        Returns:
            True if potential_parent is the direct parent of child_path
        """
        if not potential_parent or not child_path:
            return False

        # Get the actual parent directory of the child path
        actual_parent = os.path.dirname(os.path.normpath(child_path))
        normalized_potential_parent = os.path.normpath(potential_parent)

        return actual_parent == normalized_potential_parent

    def _start_auto_expand_timer(self, index: QModelIndex) -> None:
        """
        Start the auto-expand timer for the specified folder.

        Args:
            index: The model index of the folder to potentially expand
        """
        # Stop any existing timer
        self._stop_auto_expand_timer()

        # Set the pending index and start timer
        self._pending_expand_index = index
        self._auto_expand_timer.start()

    def _stop_auto_expand_timer(self) -> None:
        """Stop the auto-expand timer and clear pending index."""
        if self._auto_expand_timer.isActive():
            self._auto_expand_timer.stop()

        self._pending_expand_index = None

    def _close_auto_opened_folders(self, current_target_path: str | None = None) -> None:
        """
        Close auto-opened folders that are not ancestors of the current target.

        Args:
            current_target_path: Current target directory path. Folders that are
                               ancestors of this path will remain open.
        """
        folders_to_close = []

        for persistent_index in self._auto_opened_folders:
            # Skip if the persistent index is no longer valid
            if not persistent_index.isValid():
                folders_to_close.append(persistent_index)
                continue

            # Get the path for this auto-opened folder
            folder_path = self.get_path_from_index(QModelIndex(cast(QModelIndex, persistent_index)))
            if not folder_path:
                folders_to_close.append(persistent_index)
                continue

            # If we have a current target, check if this folder is an ancestor
            should_keep_open = False
            if current_target_path:
                # Keep the folder open if it's an ancestor of the current target
                should_keep_open = self._is_ancestor_path(folder_path, current_target_path)
                # Also keep it open if it's the current target itself
                should_keep_open = should_keep_open or (folder_path == current_target_path)

            if not should_keep_open:
                # Close the folder
                self.collapse(QModelIndex(cast(QModelIndex, persistent_index)))
                folders_to_close.append(persistent_index)

        # Remove closed folders from our tracking set
        for folder in folders_to_close:
            self._auto_opened_folders.discard(folder)

    def _clear_auto_opened_folders(self) -> None:
        """Clear the set of auto-opened folders without closing them."""
        self._auto_opened_folders.clear()

    def _handle_auto_expand_timeout(self) -> None:
        """Handle auto-expand timer timeout by expanding the pending folder."""
        if not self._pending_expand_index or not self._pending_expand_index.isValid():
            return

        # Verify the index is still a valid collapsed folder
        target_path = self.get_path_from_index(self._pending_expand_index)
        if not target_path:
            return

        # Only expand if it's still a directory and not already expanded
        if os.path.isdir(target_path) and not self.isExpanded(self._pending_expand_index):
            self.expand(self._pending_expand_index)
            # Track this folder as auto-opened
            self._auto_opened_folders.add(QPersistentModelIndex(self._pending_expand_index))

        # Clear the pending index
        self._pending_expand_index = None

    def _should_auto_expand(self, index: QModelIndex) -> bool:
        """
        Check if the given index should trigger auto-expansion.

        Args:
            index: The model index to check

        Returns:
            True if this index represents a collapsed folder that should auto-expand
        """
        if not index.isValid():
            return False

        # Don't auto-expand if already expanded
        if self.isExpanded(index):
            return False

        # Check if it's a directory
        target_path = self.get_path_from_index(index)
        if not target_path:
            return False

        return os.path.isdir(target_path)

    def _get_scroll_direction_and_speed(self, global_pos: QPoint | None = None) -> tuple[int, int]:
        """
        Calculate scroll direction and speed based on mouse position.

        Args:
            global_pos: Global mouse position, if None uses current cursor position

        Returns:
            Tuple of (direction, speed) where direction is -1/0/1 and speed is pixels per scroll
        """
        if global_pos is None:
            global_pos = QCursor.pos()

        # Convert global position to viewport coordinates
        viewport_pos = self.viewport().mapFromGlobal(global_pos)
        viewport_rect = self.viewport().rect()

        # Calculate distance from viewport edges (can be negative if outside)
        distance_from_top = viewport_pos.y()
        distance_from_bottom = viewport_pos.y() - viewport_rect.height()

        # Check if we should scroll up (mouse above or near top of viewport)
        if distance_from_top < self._scroll_zone_size:
            distance_out = self._scroll_zone_size - distance_from_top
            # Scale speed based on distance outside viewport
            if distance_from_top < 0:
                # Mouse is above viewport - increase speed based on distance
                distance_out = min(distance_out, viewport_rect.height() * 2)

            speed_factor = distance_out / self._scroll_zone_size
            speed = int(self._min_scroll_speed + (self._max_scroll_speed - self._min_scroll_speed) * speed_factor)
            return -1, speed

        # Check if we should scroll down (mouse below or near bottom of viewport)
        if distance_from_bottom > -self._scroll_zone_size:
            distance_out = distance_from_bottom + self._scroll_zone_size
            # Scale speed based on distance outside viewport
            if distance_from_bottom > 0:
                # Mouse is below viewport - increase speed based on distance
                distance_out = min(distance_out, viewport_rect.height() * 2)

            speed_factor = distance_out / self._scroll_zone_size
            speed = int(self._min_scroll_speed + (self._max_scroll_speed - self._min_scroll_speed) * speed_factor)
            return 1, speed

        # Not in scroll zone
        return 0, 0

    def _start_auto_scroll(self, direction: int, speed: int) -> None:
        """
        Start auto-scrolling in the specified direction.

        Args:
            direction: -1 for up, 1 for down
            speed: Pixels to scroll per timer interval
        """
        if direction == 0:
            self._stop_auto_scroll()
            return

        # Only start if not already scrolling in this direction at this speed
        if self._scroll_direction != direction or self._scroll_speed != speed:
            self._scroll_direction = direction
            self._scroll_speed = speed

            if not self._auto_scroll_timer.isActive():
                self._auto_scroll_timer.start()

    def _stop_auto_scroll(self) -> None:
        """Stop auto-scrolling."""
        if self._auto_scroll_timer.isActive():
            self._auto_scroll_timer.stop()

        self._scroll_direction = 0
        self._scroll_speed = 0

    def _handle_auto_scroll_timeout(self) -> None:
        """Handle auto-scroll timer timeout by scrolling the viewport."""
        # Update scroll direction and speed based on current global mouse position
        global_pos = QCursor.pos()
        direction, speed = self._get_scroll_direction_and_speed(global_pos)

        if direction == 0:
            self._stop_auto_scroll()
            return

        # Update current direction and speed
        self._scroll_direction = direction
        self._scroll_speed = speed

        # Get current scroll bar and perform scrolling
        scroll_bar = self.verticalScrollBar()
        if not scroll_bar:
            return

        current_value = scroll_bar.value()

        if self._scroll_direction < 0:  # Scroll up
            new_value = max(scroll_bar.minimum(), current_value - self._scroll_speed)

        else:  # Scroll down
            new_value = min(scroll_bar.maximum(), current_value + self._scroll_speed)

        # Only scroll if we can actually move
        if new_value != current_value:
            scroll_bar.setValue(new_value)

        else:
            # We've hit the boundary, stop scrolling
            self._stop_auto_scroll()

    def mousePressEvent(self, event: QMouseEvent) -> None:
        """Handle mouse press events for drag initiation."""
        if event.button() & Qt.MouseButton.LeftButton:
            self._drag_start_pos = event.pos()

        super().mousePressEvent(event)

    def mouseMoveEvent(self, event: QMouseEvent) -> None:
        """Handle mouse move events."""
        # Get the file path from the source model
        index = self.indexAt(event.pos())
        path = self.get_path_from_index(index)

        # Get the item under the mouse to work out tool tips.
        self.setToolTip(path if path else "")

        if not event.buttons() & Qt.MouseButton.LeftButton:
            return

        if not self._drag_start_pos:
            return

        # Check if we've moved far enough to start a drag
        if (event.pos() - self._drag_start_pos).manhattanLength() < QApplication.startDragDistance():
            return

        # Get the item under the mouse
        drag_index = self.indexAt(self._drag_start_pos)
        if not drag_index.isValid():
            return

        # Get path for drag operation
        drag_path = self.get_path_from_index(drag_index)
        if not drag_path:
            return

        # Create mime data with path
        mime_data = QMimeData()
        mime_data.setData("application/x-humbug-path", drag_path.encode())

        # Create drag object
        drag = QDrag(self)
        drag.setMimeData(mime_data)

        # Create drag pixmap from the tree item
        pixmap = self.viewport().grab(self.visualRect(index))
        drag.setPixmap(pixmap)
        drag.setHotSpot(event.pos() - self._drag_start_pos)

        # Execute drag operation
        drag.exec_(Qt.DropAction.MoveAction | Qt.DropAction.CopyAction)

        # Clear drop target, stop timers, and close auto-opened folders when drag ends
        self.clear_drop_target()
        self._stop_auto_expand_timer()
        self._stop_auto_scroll()  # This will stop scrolling when drag completes
        self._close_auto_opened_folders()  # Close all auto-opened folders
        self._drag_start_pos = None

    def dragEnterEvent(self, event: QDragEnterEvent) -> None:
        """Handle drag enter events."""
        if not event.mimeData().hasFormat("application/x-humbug-path"):
            event.ignore()
            return

        event.acceptProposedAction()

    def dragLeaveEvent(self, event: QDragLeaveEvent) -> None:
        """Handle drag leave events."""
        # Clear drop target and stop auto-expand timer when drag leaves the tree view
        self.clear_drop_target()
        self._stop_auto_expand_timer()
        self._close_auto_opened_folders()
        super().dragLeaveEvent(event)

    def dragMoveEvent(self, event: QDragMoveEvent) -> None:
        """Handle drag move events to provide visual feedback and auto-scroll."""
        if not event.mimeData().hasFormat("application/x-humbug-path"):
            event.ignore()
            self.clear_drop_target()
            self._stop_auto_expand_timer()
            self._stop_auto_scroll()
            return

        # Handle auto-scrolling based on global mouse position
        global_pos = QCursor.pos()
        scroll_direction, scroll_speed = self._get_scroll_direction_and_speed(global_pos)
        if scroll_direction != 0:
            self._start_auto_scroll(scroll_direction, scroll_speed)

        else:
            self._stop_auto_scroll()

        # Get the index at the current position
        index = self.indexAt(event.pos())
        if not index.isValid():
            event.ignore()
            self.clear_drop_target()
            self._stop_auto_expand_timer()
            return

        # Get the target path
        target_path = self.get_path_from_index(index)
        if not target_path:
            event.ignore()
            self.clear_drop_target()
            self._stop_auto_expand_timer()
            return

        # Get the dragged item path
        mime_data = event.mimeData().data("application/x-humbug-path").data()

        # Convert to bytes first if it's not already bytes
        if not isinstance(mime_data, bytes):
            mime_data = bytes(mime_data)

        dragged_path = mime_data.decode()

        # Can't drop a parent folder into one of its children
        if target_path.startswith(dragged_path + os.sep):
            event.ignore()
            self.clear_drop_target()
            self._stop_auto_expand_timer()
            return

        # Can't drop into the parent directory (no-op move)
        if self._is_parent_directory(target_path, dragged_path):
            event.ignore()
            self.clear_drop_target()
            self._stop_auto_expand_timer()
            return

        # If the target is not a directory, ignore the event
        if not os.path.isdir(target_path):
            event.ignore()
            self.clear_drop_target()
            self._stop_auto_expand_timer()
            return

        # If the dragged path is the same as the target, ignore the event
        if dragged_path == target_path:
            event.ignore()
            self.clear_drop_target()
            self._stop_auto_expand_timer()
            return

        # Check if this is a valid drag target
        source_basename = os.path.basename(dragged_path)
        if source_basename in ['.humbug', 'conversations', 'metaphor']:
            event.ignore()
            self.clear_drop_target()
            self._stop_auto_expand_timer()
            return

        # Set this index as the current drop target
        self._set_drop_target(index)

        # Handle auto-expansion for collapsed folders
        if self._should_auto_expand(index):
            # If we're hovering over a different folder than before, start a new timer
            if self._pending_expand_index is None or self._pending_expand_index != index:
                # Close auto-opened folders that are not ancestors of current target
                self._close_auto_opened_folders(target_path)
                self._start_auto_expand_timer(index)

        else:
            # Not a folder that should auto-expand, stop any existing timer
            # and close auto-opened folders that are not ancestors of current target
            self._stop_auto_expand_timer()
            if os.path.isdir(target_path):
                self._close_auto_opened_folders(target_path)

            else:
                self._close_auto_opened_folders()

        event.acceptProposedAction()

    def dropEvent(self, event: QDropEvent) -> None:
        """Handle drop events."""
        # Get the target index before clearing state
        drop_target_index = self.indexAt(event.pos())

        # Clear drop target and stop timers when drop completes
        self.clear_drop_target()
        self._stop_auto_expand_timer()
        self._stop_auto_scroll()

        # Keep the final drop target open but close other auto-opened folders
        if drop_target_index.isValid():
            target_path = self.get_path_from_index(drop_target_index)
            self._close_auto_opened_folders(target_path)

        else:
            self._close_auto_opened_folders()

        # Clear our tracking since the drag operation is complete
        self._clear_auto_opened_folders()

        if not event.mimeData().hasFormat("application/x-humbug-path"):
            event.ignore()
            return

        # Get the target index and path
        index = drop_target_index
        if not index.isValid():
            event.ignore()
            return

        target_path = self.get_path_from_index(index)
        if not target_path or not os.path.isdir(target_path):
            event.ignore()
            return

        # Get the dragged item path
        mime_data = event.mimeData().data("application/x-humbug-path").data()

        # Convert to bytes first if it's not already bytes
        if not isinstance(mime_data, bytes):
            mime_data = bytes(mime_data)

        dragged_path = mime_data.decode()

        # Can't drop a parent folder into one of its children
        if target_path.startswith(dragged_path + os.sep):
            event.ignore()
            return

        # Can't drop into the parent directory (no-op move)
        if self._is_parent_directory(target_path, dragged_path):
            event.ignore()
            return

        # If the dragged path is the same as the target, ignore the event
        if dragged_path == target_path:
            event.ignore()
            return

        # Signal the drop event
        self.file_dropped.emit(dragged_path, target_path)

        event.acceptProposedAction()
