"""Reusable switch widget."""

from PySide6.QtCore import QEvent, QPoint, QSize, Qt
from PySide6.QtGui import QColor, QFont, QMouseEvent, QPainter
from PySide6.QtWidgets import QCheckBox, QWidget

from humbug.color_role import ColorRole
from humbug.style_manager import StyleManager


class Switch(QCheckBox):
    """Compact pill switch control."""

    def __init__(self, parent: QWidget | None = None) -> None:
        """Initialize the switch."""
        super().__init__(parent)
        self._track_on_color: QColor
        self._track_off_color: QColor
        self._track_border_color: QColor
        self._knob_color: QColor
        self._text_on_color: QColor
        self._text_off_color: QColor
        self._on_label = "\u23fd"
        self._off_label = "\u23fc"
        self._knob_inset = 3
        self.setCursor(Qt.CursorShape.PointingHandCursor)
        self.setFocusPolicy(Qt.FocusPolicy.NoFocus)
        self.setMinimumSize(44, 20)
        self.toggled.connect(lambda _checked: self.update())
        self._style_manager = StyleManager()
        self.apply_style(self._style_manager)

    def apply_style(self, style_manager: StyleManager) -> None:
        """Apply design-system colors and metrics."""
        # TODO: Should we make this the default approach everywhere we use apply_style?
        self._track_on_color = QColor(style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_RECOMMENDED))
        self._track_off_color = QColor(style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND))
        self._track_border_color = QColor(style_manager.get_color_str(ColorRole.EDIT_BOX_BORDER))
        self._knob_color = QColor(style_manager.get_color_str(ColorRole.TEXT_RECOMMENDED))
        self._text_on_color = QColor(style_manager.get_color_str(ColorRole.TEXT_RECOMMENDED))
        self._text_off_color = QColor(style_manager.get_color_str(ColorRole.TEXT_PRIMARY))
        self._knob_inset = style_manager.switch_knob_inset()
        self.setFixedSize(style_manager.switch_width(), style_manager.switch_height())
        self.update()

    def set_labels(self, on_label: str, off_label: str) -> None:
        """Set the text shown for on and off states."""
        self._on_label = on_label
        self._off_label = off_label
        self.update()

    def sizeHint(self) -> QSize:
        """Return the preferred switch size."""
        return self.minimumSize()

    def hitButton(self, pos: QPoint) -> bool:
        """Make the whole pill clickable."""
        return self.rect().contains(pos)

    def mouseReleaseEvent(self, event: QMouseEvent) -> None:
        """Toggle from anywhere inside the switch."""
        if event.button() == Qt.MouseButton.LeftButton and self.rect().contains(event.position().toPoint()):
            self.setChecked(not self.isChecked())
            event.accept()
            return

        super().mouseReleaseEvent(event)

    def paintEvent(self, event: QEvent) -> None:
        """Paint the switch as a rounded pill."""
        del event

        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)

        track_rect = self.rect().adjusted(1, 1, -1, -1)
        radius = track_rect.height() / 2
        track_color = self._track_on_color if self.isChecked() else self._track_off_color
        text_color = self._text_on_color if self.isChecked() else self._text_off_color

        painter.setPen(self._track_border_color)
        painter.setBrush(track_color)
        painter.drawRoundedRect(track_rect, radius, radius)

        knob_size = track_rect.height() - (self._knob_inset * 2)
        knob_y = track_rect.y() + self._knob_inset
        if self.isChecked():
            knob_x = track_rect.right() - knob_size - self._knob_inset

        else:
            knob_x = track_rect.x() + self._knob_inset

        painter.setPen(Qt.PenStyle.NoPen)
        painter.setBrush(self._knob_color)
        painter.drawEllipse(knob_x, knob_y, knob_size, knob_size)

        label = self._on_label if self.isChecked() else self._off_label
        text_rect = track_rect.adjusted(6, 0, -6, 0)
        if self.isChecked():
            text_rect.setRight(knob_x - 2)

        else:
            text_rect.setLeft(knob_x + knob_size + 2)

        font = QFont(painter.font())
        font.setBold(True)
        font.setPointSizeF(max(6.0, font.pointSizeF() * 0.68))
        painter.setFont(font)
        painter.setPen(text_color)
        painter.drawText(text_rect, Qt.AlignmentFlag.AlignCenter, label)
