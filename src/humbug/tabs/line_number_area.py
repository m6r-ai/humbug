"""Shared line number gutter widget for QPlainTextEdit-based views."""

from typing import Callable

from PySide6.QtWidgets import QWidget
from PySide6.QtCore import QSize
from PySide6.QtGui import QPaintEvent


class LineNumberArea(QWidget):
    """Widget that displays line numbers alongside a QPlainTextEdit.

    Painting and width calculation are fully delegated to the owning editor
    via callables supplied at construction, keeping all logic in one place.
    """

    def __init__(
        self,
        parent: QWidget,
        get_width: Callable[[], int],
        paint: Callable[[QPaintEvent], None],
    ) -> None:
        """
        Initialise the line number area.

        Args:
            parent: The owning QPlainTextEdit (used as the Qt parent widget).
            get_width: Callable returning the current required width in pixels.
            paint: Callable that performs the actual painting for a given event.
        """
        super().__init__(parent)
        self._get_width = get_width
        self._paint = paint

    def sizeHint(self) -> QSize:
        """Return the preferred size, driven by the owner's width calculation."""
        return QSize(self._get_width(), 0)

    def paintEvent(self, event: QPaintEvent) -> None:
        """Delegate painting entirely to the owning editor."""
        self._paint(event)
