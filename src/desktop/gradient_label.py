"""Gradient-aware QLabel widgets."""

import math

from PySide6.QtCore import Qt, QPointF, QRectF, QTimer
from PySide6.QtGui import QBrush, QColor, QLinearGradient, QPainter, QPainterPath, QPen
from PySide6.QtGui import QPaintEvent
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
        """Update the gradient start and end colours and trigger a repaint."""
        self._start = QColor(start_color)
        self._end = QColor(end_color)
        self.update()

    def paintEvent(self, _event: QPaintEvent) -> None:
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
        fill_color: str | None = None,
        parent: QWidget | None = None,
    ) -> None:
        super().__init__(parent)
        self._start = QColor(start_color)
        self._end = QColor(end_color)
        self._radius = radius
        self._bw = border_width
        self._fill = QColor(fill_color) if fill_color else None
        self._angle = 0.0
        self.setAutoFillBackground(False)
        self.setStyleSheet("background: transparent;")

        # Rotate the gradient ~60 times per second
        self._timer = QTimer(self)
        self._timer.timeout.connect(self._tick)
        self._timer.start(16)

    def _tick(self) -> None:
        self._angle = (self._angle + 1.5) % 360.0
        self.update()

    def update_colors(self, start_color: str, end_color: str) -> None:
        """Update the gradient start and end colours and trigger a repaint."""
        self._start = QColor(start_color)
        self._end = QColor(end_color)
        self.update()

    def update_fill_color(self, fill_color: str | None) -> None:
        """Update the fill colour and trigger a repaint."""
        self._fill = QColor(fill_color) if fill_color else None
        self.update()

    def paintEvent(self, _event: QPaintEvent) -> None:
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

        # Fill the rounded-rect interior behind the pixmap
        half = self._bw / 2.0
        border_rect = QRectF(
            px - half - 1,
            py - half - 1,
            scaled.width() + self._bw + 2,
            scaled.height() + self._bw + 2,
        )
        if self._fill is not None:
            fill_path = QPainterPath()
            fill_path.addRoundedRect(border_rect, self._radius, self._radius)
            painter.fillPath(fill_path, self._fill)

        painter.drawPixmap(px, py, scaled)

        # Rotating gradient: project start/end points from centre along _angle
        cx = border_rect.x() + border_rect.width() / 2.0
        cy = border_rect.y() + border_rect.height() / 2.0
        diag = math.hypot(border_rect.width(), border_rect.height()) / 2.0
        rad = math.radians(self._angle)
        cos_a, sin_a = math.cos(rad), math.sin(rad)
        g_start = QPointF(cx - diag * cos_a, cy - diag * sin_a)
        g_end   = QPointF(cx + diag * cos_a, cy + diag * sin_a)

        gradient = QLinearGradient(g_start, g_end)
        gradient.setColorAt(0.0, self._start)
        gradient.setColorAt(0.5, self._end)
        gradient.setColorAt(1.0, self._start)   # wrap for smooth loop

        path = QPainterPath()
        path.addRoundedRect(border_rect, self._radius, self._radius)

        pen = QPen(QBrush(gradient), self._bw)
        pen.setCapStyle(Qt.PenCapStyle.RoundCap)
        pen.setJoinStyle(Qt.PenJoinStyle.RoundJoin)
        painter.strokePath(path, pen)
        painter.end()
