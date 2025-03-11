from PySide6.QtWidgets import QTabWidget
from PySide6.QtCore import Signal, QEvent
from PySide6.QtGui import QDragEnterEvent, QDropEvent

from humbug.gui.tab.tab_bar import TabBar


class TabColumn(QTabWidget):
    """Enhanced QTabWidget for use in columns with drag and drop support."""

    column_activated = Signal(QTabWidget)
    tab_drop = Signal(str, QTabWidget, int)  # tab_id, target_column, target_index
    file_drop = Signal(str, QTabWidget, int)  # file_path, target_column, target_index

    def __init__(self, parent=None):
        """Initialize the tab widget."""
        super().__init__(parent)
        self.setMovable(True)
        self.setDocumentMode(True)

        # Enable drag and drop
        self.setAcceptDrops(True)

        # Configure tab bar
        self.setTabBar(TabBar())
        tab_bar = self.tabBar()
        tab_bar.setDrawBase(False)
        tab_bar.setUsesScrollButtons(True)

        # Install event filter on all child widgets
        self.installEventFilter(self)
        tab_bar.installEventFilter(self)

    def eventFilter(self, obj, event) -> bool:
        """Handle window activation and mouse events to detect active column."""
        if event.type() in (QEvent.MouseButtonPress, QEvent.FocusIn):
            # Emit activation on mouse press or focus
            self.column_activated.emit(self)
            return False  # Don't consume the event

        return super().eventFilter(obj, event)

    def addTab(self, widget, *args, **kwargs):
        """Override addTab to install event filter on new tabs."""
        result = super().addTab(widget, *args, **kwargs)
        # Install event filter on the widget to catch focus/mouse events
        widget.installEventFilter(self)
        return result

    def removeTab(self, index):
        """Override removeTab to properly clean up event filters."""
        widget = self.widget(index)
        if widget:
            widget.removeEventFilter(self)

        super().removeTab(index)

    def dragEnterEvent(self, event: QDragEnterEvent):
        """Handle drag enter events for tab drops."""
        if event.mimeData().hasFormat("application/x-humbug-tab"):
            event.acceptProposedAction()
        elif event.mimeData().hasFormat("application/x-humbug-file"):
            event.acceptProposedAction()
        else:
            event.ignore()

    def dragMoveEvent(self, event):
        """Handle drag move events to show insertion position."""
        if (event.mimeData().hasFormat("application/x-humbug-tab") or
                event.mimeData().hasFormat("application/x-humbug-file")):
            event.acceptProposedAction()

            # Map cursor position to the tab bar to find insertion position
            pos = self.tabBar().mapFromParent(event.pos())
            _index = self.tabBar().tabAt(pos)
            # Note: Could add visual indicator of insertion position here if desired
        else:
            event.ignore()

    def dropEvent(self, event: QDropEvent):
        """Handle drop events for tab movement and file drops."""
        mime_data = event.mimeData()
        if mime_data.hasFormat("application/x-humbug-tab"):
            # Extract tab ID from mime data
            tab_id = mime_data.data("application/x-humbug-tab").data().decode()

            # Map the drop position to the tab bar
            pos = self.tabBar().mapFromParent(event.pos())
            target_index = self.tabBar().tabAt(pos)

            # If dropped past the last tab, append
            if target_index == -1:
                target_index = self.count()

            # Emit signal with drop info for tab manager to handle
            self.tab_drop.emit(tab_id, self, target_index)
            event.acceptProposedAction()
        elif mime_data.hasFormat("application/x-humbug-file"):
            # Extract file path from mime data
            file_path = mime_data.data("application/x-humbug-file").data().decode()

            # Map the drop position to the tab bar
            pos = self.tabBar().mapFromParent(event.pos())
            target_index = self.tabBar().tabAt(pos)

            # If dropped past the last tab, append
            if target_index == -1:
                target_index = self.count()

            # Emit signal with file info for tab manager to handle
            self.file_drop.emit(file_path, self, target_index)
            event.acceptProposedAction()
        else:
            event.ignore()
