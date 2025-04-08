from typing import Callable

from PySide6.QtWidgets import QWidget
from PySide6.QtCore import QSize


class EditorLineNumberArea(QWidget):
    """Widget that displays line numbers for the editor."""

    def __init__(
        self,
        parent: QWidget,
        get_line_number_area_width: Callable,
        line_number_area_paint_event: Callable
    ) -> None:
        """Initialize the line number area."""
        super().__init__(parent)
        self._get_line_number_area_width = get_line_number_area_width
        self._line_number_area_paint_event = line_number_area_paint_event

    def sizeHint(self):
        """Get the needed width for the widget."""
        return QSize(self._get_line_number_area_width(), 0)

    def paintEvent(self, event):
        """Paint the line numbers."""
        self._line_number_area_paint_event(event)
