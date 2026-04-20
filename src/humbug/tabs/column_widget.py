from typing import cast

from PySide6.QtWidgets import QTabWidget, QWidget
from PySide6.QtCore import Signal, QEvent, QObject, QPoint
from PySide6.QtGui import QDragEnterEvent, QDragLeaveEvent, QDragMoveEvent, QDropEvent

from humbug.tabs.tab_bar import TabBar


class ColumnWidget(QTabWidget):
    """Enhanced QTabWidget for use in columns with drag and drop support."""

    column_activated = Signal(QTabWidget)
    tab_dropped = Signal(str, str, QTabWidget, int)  # tab_id, source_manager_id, target_column, target_index
    path_dropped = Signal(str, str, QTabWidget, int)  # source_type, path, target_column, target_index

    def __init__(self, parent: QWidget | None = None) -> None:
        """Initialize the tab widget."""
        super().__init__(parent)
        self.setMovable(False)
        self.setDocumentMode(True)

        # Enable drag and drop
        self.setAcceptDrops(True)

        # Configure tab bar
        self.setTabBar(TabBar(self))
        tab_bar = self.tabBar()
        tab_bar.setAcceptDrops(True)
        tab_bar.setDrawBase(False)
        tab_bar.setUsesScrollButtons(True)
        tab_bar.installEventFilter(self)
        for child in tab_bar.findChildren(QWidget):
            child.installEventFilter(self)

    def _drop_insertion_index(self, pos_in_widget: QPoint) -> int:
        """
        Compute the insertion index for a drop at the given widget-local position.

        Returns a value in the range 0..count() inclusive.
        """
        tab_bar = self.tabBar()
        pos = tab_bar.mapFromParent(pos_in_widget)
        index = tab_bar.tabAt(pos)
        if index == -1:
            return self.count()

        tab_rect = tab_bar.tabRect(index)
        if pos.x() > tab_rect.center().x():
            return index + 1

        return index

    def eventFilter(self, watched: QObject, event: QEvent) -> bool:
        """Handle window activation and mouse events to detect active column."""
        tab_bar = self.tabBar()
        if watched == tab_bar or (isinstance(watched, QWidget) and watched.parent() == tab_bar):
            mapped_source = tab_bar
            if isinstance(watched, QWidget) and watched is not tab_bar:
                mapped_source = watched

            if event.type() == QEvent.Type.DragEnter:
                drag_event = cast(QDragEnterEvent, event)
                self._handle_drag_enter_or_move(
                    drag_event,
                    mapped_source.mapTo(self, drag_event.position().toPoint())
                )
                return True

            if event.type() == QEvent.Type.DragMove:
                drag_event = cast(QDragMoveEvent, event)
                self._handle_drag_enter_or_move(
                    drag_event,
                    mapped_source.mapTo(self, drag_event.position().toPoint())
                )
                return True

            if event.type() == QEvent.Type.DragLeave:
                self.dragLeaveEvent(cast(QDragLeaveEvent, event))
                return True

            if event.type() == QEvent.Type.Drop:
                drop_event = cast(QDropEvent, event)
                self._handle_drop(
                    drop_event,
                    mapped_source.mapTo(self, drop_event.position().toPoint())
                )
                return True

            if watched == tab_bar and event.type() == QEvent.Type.ChildAdded:
                for child in tab_bar.findChildren(QWidget):
                    child.installEventFilter(self)

        if event.type() in (QEvent.Type.MouseButtonPress, QEvent.Type.FocusIn):
            # Emit activation on mouse press or focus
            self.column_activated.emit(self)
            return False  # Don't consume the event

        return super().eventFilter(watched, event)

    def _handle_drag_enter_or_move(self, event: QDragEnterEvent | QDragMoveEvent, pos_in_widget: QPoint) -> None:
        """Accept supported drags and update the insertion indicator."""
        if (event.mimeData().hasFormat("application/x-humbug-tab") or
                event.mimeData().hasFormat("application/x-humbug-path")):
            event.acceptProposedAction()
            tab_bar = self.tabBar()
            assert isinstance(tab_bar, TabBar)
            tab_bar.set_drop_index(self._drop_insertion_index(pos_in_widget))
            return

        event.ignore()

    def dragEnterEvent(self, event: QDragEnterEvent) -> None:
        """
        Handle drag enter events for tab drops.

        Args:
            event: The drag enter event

        Raises:
            None
        """
        self._handle_drag_enter_or_move(event, event.position().toPoint())

    def dragMoveEvent(self, event: QDragMoveEvent) -> None:
        """Handle drag move events, updating the insertion indicator."""
        self._handle_drag_enter_or_move(event, event.position().toPoint())

    def dragLeaveEvent(self, event: QDragLeaveEvent) -> None:
        """Clear the insertion indicator when the drag leaves the widget."""
        tab_bar = self.tabBar()
        assert isinstance(tab_bar, TabBar)
        tab_bar.clear_drop_index()
        super().dragLeaveEvent(event)

    def _handle_drop(self, event: QDropEvent, pos_in_widget: QPoint) -> None:
        """
        Handle drop events for tab movement and file drops.

        Args:
            event: The drop event

        Raises:
            None
        """
        tab_bar = self.tabBar()
        assert isinstance(tab_bar, TabBar)
        tab_bar.clear_drop_index()

        if event.mimeData().hasFormat("application/x-humbug-tab"):
            # Extract tab ID from mime data
            mime_data = event.mimeData().data("application/x-humbug-tab").data()

            # Convert to bytes first if it's not already bytes
            if not isinstance(mime_data, bytes):
                mime_data = bytes(mime_data)

            tab_id = mime_data.decode()
            source_manager_id = ""
            if event.mimeData().hasFormat("application/x-humbug-tab-manager"):
                source_data = event.mimeData().data("application/x-humbug-tab-manager").data()
                if not isinstance(source_data, bytes):
                    source_data = bytes(source_data)
                source_manager_id = source_data.decode()

            # Map the drop position to the tab bar
            pos = self.tabBar().mapFromParent(pos_in_widget)
            target_index = self.tabBar().tabAt(pos)

            # If dropped past the last tab, append
            if target_index == -1:
                target_index = self.count()

            # Emit signal with drop info for tab manager to handle
            self.tab_dropped.emit(tab_id, source_manager_id, self, target_index)
            event.acceptProposedAction()
            return

        if event.mimeData().hasFormat("application/x-humbug-path"):
            # Extract path from mime data
            mime_data = event.mimeData().data("application/x-humbug-path").data()

            # Convert to bytes first if it's not already bytes
            if not isinstance(mime_data, bytes):
                mime_data = bytes(mime_data)

            path = mime_data.decode()

            # Extract source type if available
            source_type = None
            if event.mimeData().hasFormat("application/x-humbug-source"):
                source_data = event.mimeData().data("application/x-humbug-source").data()

                # Convert to bytes first if it's not already bytes
                if not isinstance(source_data, bytes):
                    source_data = bytes(source_data)

                source_type = source_data.decode()

            # Map the drop position to the tab bar
            pos = self.tabBar().mapFromParent(pos_in_widget)
            target_index = self.tabBar().tabAt(pos)

            # If dropped past the last tab, append
            if target_index == -1:
                target_index = self.count()

            # Emit signal with path info for column manager to handle
            self.path_dropped.emit(source_type, path, self, target_index)
            event.acceptProposedAction()
            return

        event.ignore()

    def dropEvent(self, event: QDropEvent) -> None:
        """Handle drop events for tab movement and file drops."""
        self._handle_drop(event, event.position().toPoint())
