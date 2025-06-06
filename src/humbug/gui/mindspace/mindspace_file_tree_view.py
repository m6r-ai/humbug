"""File tree view implementation for mindspace files with drag and drop support."""

import os
from typing import cast

from PySide6.QtWidgets import QTreeView, QApplication, QWidget, QFileSystemModel
from PySide6.QtCore import Qt, QSortFilterProxyModel, QMimeData, QPoint, Signal, QModelIndex, QPersistentModelIndex, QTimer
from PySide6.QtGui import QDrag, QMouseEvent, QDragEnterEvent, QDragMoveEvent, QDropEvent, QDragLeaveEvent


class MindspaceFileTreeView(QTreeView):
    """Custom tree view with drag and drop support including auto-scroll."""

    file_dropped = Signal(str, str)  # dragged_path, target_path
    drop_target_changed = Signal(QModelIndex)  # current drop target index

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

        self.setHeaderHidden(True)
        self.setAnimated(True)
        self.header().setSortIndicator(0, Qt.SortOrder.AscendingOrder)
        self.setSortingEnabled(True)
        self.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.setMouseTracking(True)
        self.setToolTipDuration(10000)

    def get_current_drop_target(self) -> QModelIndex | None:
        """Get the current drop target index."""
        return self._current_drop_target

    def clear_drop_target(self) -> None:
        """Clear the current drop target and emit signal."""
        if self._current_drop_target is not None:
            self._current_drop_target = None
            self.drop_target_changed.emit(QModelIndex())  # Invalid index signals clear

    def _set_drop_target(self, index: QModelIndex) -> None:
        """Set the current drop target and emit signal if changed."""
        if self._current_drop_target != index:
            self._current_drop_target = index
            self.drop_target_changed.emit(index)

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

    def _close_auto_opened_folders(self, except_index: QModelIndex | None = None) -> None:
        """
        Close all auto-opened folders except the specified one.

        Args:
            except_index: Optional index to keep open (typically the current drop target)
        """
        persistent_except = QPersistentModelIndex(except_index) if except_index and except_index.isValid() else None

        folders_to_close = []
        for persistent_index in self._auto_opened_folders:
            # Skip if this is the folder we want to keep open
            if persistent_except and persistent_index == persistent_except:
                continue

            # Skip if the persistent index is no longer valid
            if not persistent_index.isValid():
                folders_to_close.append(persistent_index)
                continue

            # Close the folder
            self.collapse(QModelIndex(persistent_index))
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
        source_model = cast(QSortFilterProxyModel, self.model())
        if not source_model:
            return

        source_index = source_model.mapToSource(self._pending_expand_index)
        file_model = cast(QFileSystemModel, source_model.sourceModel())
        if not file_model:
            return

        target_path = file_model.filePath(source_index)

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
        source_model = cast(QSortFilterProxyModel, self.model())
        if not source_model:
            return False

        source_index = source_model.mapToSource(index)
        file_model = cast(QFileSystemModel, source_model.sourceModel())
        if not file_model:
            return False

        target_path = file_model.filePath(source_index)
        return os.path.isdir(target_path)

    def _get_scroll_direction_and_speed(self, pos: QPoint) -> tuple[int, int]:
        """
        Calculate scroll direction and speed based on mouse position.

        Args:
            pos: Current mouse position in viewport coordinates

        Returns:
            Tuple of (direction, speed) where direction is -1/0/1 and speed is pixels per scroll
        """
        viewport_rect = self.viewport().rect()

        # Check if we're in the top scroll zone
        if pos.y() < self._scroll_zone_size:
            distance_from_edge = pos.y()
            # Closer to edge = faster scroll (invert for top)
            speed_factor = 1.0 - (distance_from_edge / self._scroll_zone_size)
            speed = int(self._min_scroll_speed + (self._max_scroll_speed - self._min_scroll_speed) * speed_factor)
            return -1, speed

        # Check if we're in the bottom scroll zone
        bottom_threshold = viewport_rect.height() - self._scroll_zone_size
        if pos.y() > bottom_threshold:
            distance_from_edge = viewport_rect.height() - pos.y()
            # Closer to edge = faster scroll
            speed_factor = 1.0 - (distance_from_edge / self._scroll_zone_size)
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
        if self._scroll_direction == 0 or self._scroll_speed == 0:
            self._stop_auto_scroll()
            return

        # Get current scroll bar
        scroll_bar = self.verticalScrollBar()
        if not scroll_bar:
            return

        # Calculate new scroll position
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
        source_model = cast(QSortFilterProxyModel, self.model())
        if not source_model:
            return

        index = self.indexAt(event.pos())
        source_index = source_model.mapToSource(index)
        file_model = cast(QFileSystemModel, source_model.sourceModel())
        if not file_model:
            return

        path = file_model.filePath(source_index)

        # Get the item under the mouse to work out tool tips.
        self.setToolTip(path if index.isValid() else "")

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

        # Create mime data with path
        mime_data = QMimeData()
        mime_data.setData("application/x-humbug-path", path.encode())

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
        self._stop_auto_scroll()
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
        # Clear drop target, stop timers, and close auto-opened folders when drag leaves the tree view
        self.clear_drop_target()
        self._stop_auto_expand_timer()
        self._stop_auto_scroll()
        self._close_auto_opened_folders()  # Close all auto-opened folders
        super().dragLeaveEvent(event)

    def dragMoveEvent(self, event: QDragMoveEvent) -> None:
        """Handle drag move events to provide visual feedback and auto-scroll."""
        if not event.mimeData().hasFormat("application/x-humbug-path"):
            event.ignore()
            self.clear_drop_target()
            self._stop_auto_expand_timer()
            self._stop_auto_scroll()
            return

        # Handle auto-scrolling based on mouse position
        scroll_direction, scroll_speed = self._get_scroll_direction_and_speed(event.pos())
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
        source_model = cast(QSortFilterProxyModel, self.model())
        if not source_model:
            event.ignore()
            self.clear_drop_target()
            self._stop_auto_expand_timer()
            return

        source_index = source_model.mapToSource(index)
        file_model = cast(QFileSystemModel, source_model.sourceModel())
        if not file_model:
            event.ignore()
            self.clear_drop_target()
            self._stop_auto_expand_timer()
            return

        target_path = file_model.filePath(source_index)

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
            if self._pending_expand_index != index:
                # Close previously auto-opened folders except the current target
                self._close_auto_opened_folders(index)
                self._start_auto_expand_timer(index)
        else:
            # Not a folder that should auto-expand, stop any existing timer
            # and close auto-opened folders except current target (if it's a valid directory)
            self._stop_auto_expand_timer()
            if os.path.isdir(target_path):
                self._close_auto_opened_folders(index)
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
            self._close_auto_opened_folders(drop_target_index)
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

        source_model = cast(QSortFilterProxyModel, self.model())
        if not source_model:
            event.ignore()
            return

        source_index = source_model.mapToSource(index)
        file_model = cast(QFileSystemModel, source_model.sourceModel())
        if not file_model:
            event.ignore()
            return

        target_path = file_model.filePath(source_index)
        if not os.path.isdir(target_path):
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

        # If the target is not a directory, ignore the event
        if not os.path.isdir(target_path):
            event.ignore()
            return

        # If the dragged path is the same as the target, ignore the event
        if dragged_path == target_path:
            event.ignore()
            return

        # Signal the drop event
        self.file_dropped.emit(dragged_path, target_path)

        event.acceptProposedAction()
