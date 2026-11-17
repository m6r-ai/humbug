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
        self.updateGeometry()

    def text(self) -> str:
        """Return the full (non-elided) text."""
        return self._full_text

    def minimumSizeHint(self) -> QSize:
        """Return a minimal size so the label can shrink to show just an ellipsis."""
        fm = QFontMetrics(self.font())
        return QSize(fm.horizontalAdvance("…"), super().minimumSizeHint().height())

    def sizeHint(self) -> QSize:
        """Return the full text width, plus any padding and border, as the preferred size."""
        fm = QFontMetrics(self.font())
        margins = self.contentsMargins()
        # A pixel either side, as glyphs can be drawn a little beyond their advance.
        width = fm.horizontalAdvance(self._full_text) + margins.left() + margins.right() + 2
        return QSize(width, super().sizeHint().height())

    def resizeEvent(self, event: QResizeEvent) -> None:
        super().resizeEvent(event)
        self._update_elided_text()

    def changeEvent(self, event: QEvent) -> None:
        super().changeEvent(event)
        if event.type() == QEvent.Type.FontChange:
            # The preferred width follows the font, which a style sheet may set only once the label is shown.
            self._update_elided_text()
            self.updateGeometry()

    def _update_elided_text(self) -> None:
        """Recompute and apply the elided text based on the current width and font."""
        fm = QFontMetrics(self.font())
        width = self.contentsRect().width()
        # Text exactly as wide as the label, which is what its size hint asks for, can still be
        # elided because of rounding, so text that fits by its advance is shown whole.
        fits = fm.horizontalAdvance(self._full_text) <= width
        elided = self._full_text if fits else fm.elidedText(self._full_text, Qt.TextElideMode.ElideRight, width)
        super().setText(elided)
        if elided != self._full_text:
            self.setToolTip(self._full_text)

        else:
            self.setToolTip("")
