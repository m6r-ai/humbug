from PySide6.QtWidgets import QTabWidget
from PySide6.QtCore import Signal, QEvent


class TabColumn(QTabWidget):
    """Enhanced QTabWidget for use in columns."""

    column_activated = Signal(QTabWidget)

    def __init__(self, parent=None):
        """Initialize the tab widget."""
        super().__init__(parent)
        self.setMovable(True)
        self.setDocumentMode(True)

        # Configure tab bar
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
