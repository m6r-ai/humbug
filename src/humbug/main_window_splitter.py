"""Custom splitter for main window that uses direct painting instead of stylesheets."""

from typing import Any

from PySide6.QtWidgets import QSplitter, QSplitterHandle
from PySide6.QtCore import Qt
from PySide6.QtGui import QPainter

from humbug.color_role import ColorRole
from humbug.style_manager import StyleManager


class MainWindowSplitterHandle(QSplitterHandle):
    """Custom splitter handle that paints itself without using stylesheets."""

    def __init__(self, orientation: Qt.Orientation, parent: QSplitter) -> None:
        super().__init__(orientation, parent)

    def paintEvent(self, event: Any) -> None:
        """Paint the handle with the current style."""
        style_manager = StyleManager()
        painter = QPainter(self)
        painter.fillRect(self.rect(), style_manager.get_color(ColorRole.SPLITTER))
        painter.end()


class MainWindowSplitter(QSplitter):
    """
    Custom splitter for the main window that uses custom painted handles
    to avoid stylesheet cascade through child widgets.
    """

    def __init__(self, orientation: Qt.Orientation) -> None:
        super().__init__(orientation)
        self.setHandleWidth(1)

    def createHandle(self) -> QSplitterHandle:
        """Create a custom handle that paints itself without stylesheets."""
        return MainWindowSplitterHandle(self.orientation(), self)
