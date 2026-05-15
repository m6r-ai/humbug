from PySide6.QtCore import QEvent, QSize, Qt
from PySide6.QtGui import QFontMetrics, QResizeEvent
from PySide6.QtWidgets import QLabel, QSizePolicy, QWidget


class ElidedLabel(QLabel):
    """A QLabel that elides its text with an ellipsis when the available width is too narrow."""

    def __init__(self, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._full_text = ""
        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Preferred)
        self.setMinimumWidth(0)

    def setText(self, text: str) -> None:
        """Store the full text and update the displayed (possibly elided) text."""
        self._full_text = text
        self._update_elided_text()

    def text(self) -> str:
        """Return the full (non-elided) text."""
        return self._full_text

    def minimumSizeHint(self) -> QSize:
        """Return a minimal size so the label can shrink to show just an ellipsis."""
        fm = QFontMetrics(self.font())
        return QSize(fm.horizontalAdvance("…"), super().minimumSizeHint().height())

    def sizeHint(self) -> QSize:
        """Return the full text width as the preferred size."""
        fm = QFontMetrics(self.font())
        return QSize(fm.horizontalAdvance(self._full_text), super().sizeHint().height())

    def resizeEvent(self, event: QResizeEvent) -> None:
        super().resizeEvent(event)
        self._update_elided_text()

    def changeEvent(self, event: QEvent) -> None:
        super().changeEvent(event)
        if event.type() == QEvent.Type.FontChange:
            self._update_elided_text()

    def _update_elided_text(self) -> None:
        """Recompute and apply the elided text based on the current width and font."""
        fm = QFontMetrics(self.font())
        elided = fm.elidedText(self._full_text, Qt.TextElideMode.ElideRight, self.width())
        super().setText(elided)
        if elided != self._full_text:
            self.setToolTip(self._full_text)

        else:
            self.setToolTip("")
