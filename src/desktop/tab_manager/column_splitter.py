from typing import Any

from PySide6.QtWidgets import QSplitter, QSplitterHandle
from PySide6.QtCore import Qt, Signal
from PySide6.QtGui import QPainter, QMouseEvent

from desktop.color_role import ColorRole
from desktop.style_manager import StyleManager


class ColumnSplitterHandle(QSplitterHandle):
    """Custom splitter handle that paints itself without using stylesheets."""

    def __init__(self, orientation: Qt.Orientation, parent: QSplitter) -> None:
        super().__init__(orientation, parent)

    def paintEvent(self, _event: Any) -> None:
        """Paint the handle with the current style."""
        style_manager = StyleManager()
        painter = QPainter(self)
        painter.fillRect(self.rect(), style_manager.get_color(ColorRole.SPLITTER))
        painter.end()

    def mousePressEvent(self, event: QMouseEvent) -> None:
        """Notify the parent splitter that a user drag has started."""
        parent = self.splitter()
        if isinstance(parent, ColumnSplitter):
            parent.set_user_resizing(True)

        super().mousePressEvent(event)

    def mouseReleaseEvent(self, event: QMouseEvent) -> None:
        """Notify the parent splitter that a user drag has finished."""
        parent = self.splitter()
        if isinstance(parent, ColumnSplitter):
            parent.set_user_resizing(False)

        super().mouseReleaseEvent(event)


class ColumnSplitter(QSplitter):
    """
    A custom splitter that allows for a minimum width for each column
    when moving the splitter handle.
    """

    user_resize_started = Signal()
    user_resize_finished = Signal()

    def __init__(self, *args: Any, **kwargs: Any) -> None:
        super().__init__(*args, **kwargs)
        self.setChildrenCollapsible(True)
        self.setHandleWidth(1)
        self._user_resizing = False

    def createHandle(self) -> QSplitterHandle:
        """Create a custom handle that paints itself without stylesheets."""
        return ColumnSplitterHandle(self.orientation(), self)

    def user_resizing(self) -> bool:
        """Return True while the user is actively dragging a splitter handle."""
        return self._user_resizing

    def set_user_resizing(self, resizing: bool) -> None:
        """Set whether the user is currently dragging a splitter handle."""
        if resizing == self._user_resizing:
            return

        self._user_resizing = resizing
        if resizing:
            self.user_resize_started.emit()

        else:
            self.user_resize_finished.emit()

    def moveSplitter(self, pos: int, index: int) -> None:
        # Get the widget on either side of the splitter handle
        widget1 = self.widget(index)
        widget2 = self.widget(index + 1)

        if widget1 and widget2:
            # Calculate the minimum width needed for tabs
            min_width = 100

            # If moving would make either widget too small, adjust the position
            if pos < min_width or (self.width() - pos) < min_width:
                # Trigger merge behavior through splitterMoved signal
                self.splitterMoved.emit(pos, index)

        return super().moveSplitter(pos, index)
