"""Gradient-aware QLabel widgets."""

from PySide6.QtCore import Qt, QRectF
from PySide6.QtGui import QBrush, QColor, QLinearGradient, QPainter, QPainterPath, QPen
from PySide6.QtWidgets import QLabel, QWidget


class GradientLabel(QLabel):
    """Renders label text with a left-to-right colour gradient."""

    def __init__(
        self,
        text: str,
        start_color: str,
        end_color: str,
        parent: QWidget | None = None,
    ) -> None:
        super().__init__(text, parent)
        self._start = QColor(start_color)
        self._end = QColor(end_color)

    def update_colors(self, start_color: str, end_color: str) -> None:
        self._start = QColor(start_color)
        self._end = QColor(end_color)
        self.update()

    def paintEvent(self, event) -> None:
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        painter.setFont(self.font())

        gradient = QLinearGradient(0.0, 0.0, float(self.width()), 0.0)
        gradient.setColorAt(0.0, self._start)
        gradient.setColorAt(1.0, self._end)

        painter.setPen(QPen(QBrush(gradient), 0))
        painter.drawText(self.rect(), int(self.alignment()), self.text())
        painter.end()


class GradientBorderLabel(QLabel):
    """QLabel that draws its pixmap with a gradient rounded-rect border stroke."""

    def __init__(
        self,
        start_color: str,
        end_color: str,
        radius: float = 16.0,
        border_width: float = 2.0,
        parent: QWidget | None = None,
    ) -> None:
        super().__init__(parent)
        self._start = QColor(start_color)
        self._end = QColor(end_color)
        self._radius = radius
        self._bw = border_width
        self.setAutoFillBackground(False)
        self.setStyleSheet("background: transparent;")

    def update_colors(self, start_color: str, end_color: str) -> None:
        self._start = QColor(start_color)
        self._end = QColor(end_color)
        self.update()

    def paintEvent(self, event) -> None:
        pixmap = self.pixmap()
        if not pixmap or pixmap.isNull():
            return

        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        painter.setRenderHint(QPainter.RenderHint.SmoothPixmapTransform)

        # Scale the pixmap to fit, leaving room for the border stroke
        gap = int(self._bw) + 2
        available = self.rect().adjusted(gap, gap, -gap, -gap)
        scaled = pixmap.scaled(
            available.width(), available.height(),
            Qt.AspectRatioMode.KeepAspectRatio,
            Qt.TransformationMode.SmoothTransformation,
        )

        # Centre the scaled pixmap inside the label
        px = (self.rect().width() - scaled.width()) // 2
        py = (self.rect().height() - scaled.height()) // 2
        painter.drawPixmap(px, py, scaled)

        # Draw the border tightly around the actual pixmap — not the full label
        half = self._bw / 2.0
        border_rect = QRectF(
            px - half - 1,
            py - half - 1,
            scaled.width() + self._bw + 2,
            scaled.height() + self._bw + 2,
        )
        path = QPainterPath()
        path.addRoundedRect(border_rect, self._radius, self._radius)

        gradient = QLinearGradient(border_rect.topLeft(), border_rect.topRight())
        gradient.setColorAt(0.0, self._start)
        gradient.setColorAt(1.0, self._end)

        pen = QPen(QBrush(gradient), self._bw)
        pen.setCapStyle(Qt.PenCapStyle.RoundCap)
        pen.setJoinStyle(Qt.PenJoinStyle.RoundJoin)
        painter.strokePath(path, pen)
        painter.end()
