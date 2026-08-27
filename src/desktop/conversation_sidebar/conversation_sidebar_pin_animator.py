"""Overlay animation that slides a pinned/unpinned row to its new position."""

from PySide6.QtCore import QEasingCurve, QPropertyAnimation, QRect, Qt
from PySide6.QtGui import QPixmap
from PySide6.QtWidgets import QLabel, QWidget


class PinSlideOverlay(QLabel):
    """
    Transient overlay that shows a snapshot of a tree row sliding from its old
    position to its new position after a pin/unpin reorder.

    Deletes itself once the animation finishes.
    """

    DURATION_MS = 220

    def __init__(self, parent: QWidget, snapshot: QPixmap, old_rect: QRect, new_rect: QRect) -> None:
        """
        Start the slide animation immediately.

        Args:
            parent: Widget to overlay onto (typically the tree view's viewport).
            snapshot: Rendered appearance of the row at its old position.
            old_rect: Row's rect before the reorder.
            new_rect: Row's rect after the reorder.
        """
        super().__init__(parent)
        self.setPixmap(snapshot)
        self.setGeometry(old_rect)
        self.setAttribute(Qt.WidgetAttribute.WA_TransparentForMouseEvents)
        self.show()
        self.raise_()

        self._animation = QPropertyAnimation(self, b"geometry", self)
        self._animation.setDuration(self.DURATION_MS)
        self._animation.setEasingCurve(QEasingCurve.Type.OutCubic)
        self._animation.setStartValue(old_rect)
        self._animation.setEndValue(new_rect)
        self._animation.finished.connect(self.deleteLater)
        self._animation.start()
