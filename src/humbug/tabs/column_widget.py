from PySide6.QtWidgets import QTabWidget, QWidget
from PySide6.QtCore import Signal, QEvent, QObject
from PySide6.QtGui import QDragEnterEvent, QDragMoveEvent, QDropEvent

from humbug.tabs.tab_bar import TabBar


class ColumnWidget(QTabWidget):
    """Enhanced QTabWidget for use in columns with drag and drop support."""

    column_activated = Signal(QTabWidget)
    tab_dropped = Signal(str, QTabWidget, int)  # tab_id, target_column, target_index
    path_dropped = Signal(str, str, QTabWidget, int)  # source_type, path, target_column, target_index

    def __init__(self, parent: QWidget | None = None) -> None:
        """Initialize the tab widget."""
        super().__init__(parent)
        self.setMovable(True)
        self.setDocumentMode(True)

        # Enable drag and drop
        self.setAcceptDrops(True)

        # Configure tab bar
        self.setTabBar(TabBar(self))
        tab_bar = self.tabBar()
        tab_bar.setDrawBase(False)
        tab_bar.setUsesScrollButtons(True)
        tab_bar.installEventFilter(self)

    def eventFilter(self, watched: QObject, event: QEvent) -> bool:
        """Handle window activation and mouse events to detect active column."""
        if event.type() in (QEvent.Type.MouseButtonPress, QEvent.Type.FocusIn):
            # Emit activation on mouse press or focus
            self.column_activated.emit(self)
            return False  # Don't consume the event

        return super().eventFilter(watched, event)

    def dragEnterEvent(self, event: QDragEnterEvent) -> None:
        """
        Handle drag enter events for tab drops.

        Args:
            event: The drag enter event

        Raises:
            None
        """
        if event.mimeData().hasFormat("application/x-humbug-tab"):
            event.acceptProposedAction()
            return

        if event.mimeData().hasFormat("application/x-humbug-path"):
            event.acceptProposedAction()
            return

        event.ignore()

    def dragMoveEvent(self, event: QDragMoveEvent) -> None:
        """
        Handle drag move events to show insertion position.

        Args:
            event: The drag move event

        Raises:
            None
        """
        if (event.mimeData().hasFormat("application/x-humbug-tab") or
                event.mimeData().hasFormat("application/x-humbug-path")):
            event.acceptProposedAction()

            # Map cursor position to the tab bar to find insertion position
            pos = self.tabBar().mapFromParent(event.pos())
            _index = self.tabBar().tabAt(pos)
            return
            # Note: Could add visual indicator of insertion position here if desired

        event.ignore()

    def dropEvent(self, event: QDropEvent) -> None:
        """
        Handle drop events for tab movement and file drops.

        Args:
            event: The drop event

        Raises:
            None
        """
        if event.mimeData().hasFormat("application/x-humbug-tab"):
            # Extract tab ID from mime data
            mime_data = event.mimeData().data("application/x-humbug-tab").data()

            # Convert to bytes first if it's not already bytes
            if not isinstance(mime_data, bytes):
                mime_data = bytes(mime_data)

            tab_id = mime_data.decode()

            # Map the drop position to the tab bar
            pos = self.tabBar().mapFromParent(event.pos())
            target_index = self.tabBar().tabAt(pos)

            # If dropped past the last tab, append
            if target_index == -1:
                target_index = self.count()

            # Emit signal with drop info for tab manager to handle
            self.tab_dropped.emit(tab_id, self, target_index)
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
            pos = self.tabBar().mapFromParent(event.pos())
            target_index = self.tabBar().tabAt(pos)

            # If dropped past the last tab, append
            if target_index == -1:
                target_index = self.count()

            # Emit signal with path info for column manager to handle
            self.path_dropped.emit(source_type, path, self, target_index)
            event.acceptProposedAction()
            return

        event.ignore()
