"""Right-edge minimap for navigating user questions."""

from PySide6.QtCore import QRectF, Signal, Qt
from PySide6.QtGui import QBrush, QMouseEvent, QPainter
from PySide6.QtWidgets import QWidget

from desktop.color_role import ColorRole
from desktop.style_manager import StyleManager


class QuestionMinimap(QWidget):
    """Draw individual clickable markers for user questions beside a conversation."""

    question_clicked = Signal(int)

    def __init__(self, parent: QWidget) -> None:
        super().__init__(parent)
        self._markers: list[tuple[int, float, str]] = []
        self._active_index = -1
        self._style_manager = StyleManager()
        self.setFixedWidth(10)
        self.setCursor(Qt.CursorShape.PointingHandCursor)
        self.setMouseTracking(True)
        self._style_manager.style_changed.connect(self.update)

    def set_markers(self, markers: list[tuple[int, float, str]], active_index: int) -> None:
        """Set the individual question markers and active question."""
        self._markers = markers
        self._active_index = active_index
        self.update()

    def paintEvent(self, event: object) -> None:
        """Draw one filled rounded pill for each user question."""
        del event
        painter = QPainter(self)
        padding = 8
        usable_height = max(1, self.height() - 2 * padding)
        inactive = self._style_manager.get_color(ColorRole.TEXT_INACTIVE)
        active = self._style_manager.get_color(ColorRole.BRAND_PRIMARY)
        for message_index, position, _question in self._markers:
            y = padding + round(position * usable_height)
            is_active = message_index == self._active_index
            marker_width = 8 if is_active else 6
            marker_height = 8 if is_active else 4
            painter.setPen(Qt.PenStyle.NoPen)
            painter.setBrush(QBrush(active if is_active else inactive))
            painter.drawRoundedRect(
                QRectF(
                    (self.width() - marker_width) / 2,
                    y - marker_height / 2,
                    marker_width,
                    marker_height,
                ),
                marker_height / 2,
                marker_height / 2,
            )

    def mouseReleaseEvent(self, event: QMouseEvent) -> None:
        """Navigate to the question marker nearest the released pointer."""
        if not self._markers:
            return

        target = self._nearest_marker(event.position().y())
        self.question_clicked.emit(target[0])

    def mouseMoveEvent(self, event: QMouseEvent) -> None:
        """Show the question represented by the nearest marker."""
        if not self._markers:
            return

        question = " ".join(self._nearest_marker(event.position().y())[2].split())
        self.setToolTip(question[:180] + ("…" if len(question) > 180 else ""))

    def _nearest_marker(self, mouse_y: float) -> tuple[int, float, str]:
        """Return the individual marker closest to a pointer position."""
        padding = 8
        usable_height = max(1, self.height() - 2 * padding)
        return min(
            self._markers,
            key=lambda marker: abs(padding + round(marker[1] * usable_height) - mouse_y),
        )
