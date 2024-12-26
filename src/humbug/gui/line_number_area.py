from PySide6.QtWidgets import QWidget
from PySide6.QtCore import QSize


class LineNumberArea(QWidget):
    """Widget that displays line numbers for the editor."""

    def __init__(self, editor):
        """Initialize the line number area."""
        super().__init__(editor)
        self._editor = editor

    def sizeHint(self):
        """Get the needed width for the widget."""
        return QSize(self._editor.line_number_area_width(), 0)

    def paintEvent(self, event):
        """Paint the line numbers."""
        self._editor.line_number_area_paint_event(event)
