"""Reusable switch widget."""

from PySide6.QtCore import QEasingCurve, QEvent, QPoint, QSize, Qt, QVariantAnimation
from PySide6.QtGui import QColor, QFont, QLinearGradient, QMouseEvent, QPainter
from PySide6.QtWidgets import QCheckBox, QWidget

from humbug.color_role import ColorRole
from humbug.style_manager import StyleManager


class Switch(QCheckBox):
    """Compact pill switch control."""

    def __init__(self, parent: QWidget | None = None) -> None:
        """Initialize the switch."""
        super().__init__(parent)
        self._track_on_color: QColor
        self._track_on_end_color: QColor
        self._track_off_color: QColor
        self._track_border_color: QColor
        self._track_disabled_color: QColor
        self._knob_color: QColor
        self._knob_disabled_color: QColor
        self._text_on_color: QColor
        self._text_off_color: QColor
        self._text_disabled_color: QColor
        self._on_label = "\u23fd"
        self._off_label = "\u23fc"
        self._knob_inset = 3
        self._position = 1.0 if self.isChecked() else 0.0
        self._animation = QVariantAnimation(self)
        self._animation.setDuration(140)
        self._animation.setEasingCurve(QEasingCurve.Type.OutCubic)
        self._animation.valueChanged.connect(self._on_animation_value_changed)
        self.setCursor(Qt.CursorShape.PointingHandCursor)
        self.setFocusPolicy(Qt.FocusPolicy.NoFocus)
        self.setAttribute(Qt.WidgetAttribute.WA_TranslucentBackground, True)
        self.setStyleSheet("background: transparent; border: none;")
        self.setMinimumSize(44, 20)
        self.toggled.connect(self._start_transition)
        self._style_manager = StyleManager()
        self.apply_style(self._style_manager)

    def apply_style(self, style_manager: StyleManager) -> None:
        """Apply design-system colors and metrics."""
        # TODO: Should we make this the default approach everywhere we use apply_style?
        self._track_on_color = QColor("#375f8c")
        self._track_on_end_color = QColor("#5a4f93")
        self._track_off_color = QColor(style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND))
        self._track_border_color = QColor(style_manager.get_color_str(ColorRole.EDIT_BOX_BORDER))
        self._track_disabled_color = QColor(style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_DISABLED))
        self._knob_color = QColor(style_manager.get_color_str(ColorRole.TEXT_RECOMMENDED))
        self._knob_disabled_color = QColor(style_manager.get_color_str(ColorRole.TEXT_DISABLED))
        self._text_on_color = QColor(style_manager.get_color_str(ColorRole.TEXT_RECOMMENDED))
        self._text_off_color = QColor(style_manager.get_color_str(ColorRole.TEXT_PRIMARY))
        self._text_disabled_color = QColor(style_manager.get_color_str(ColorRole.TEXT_DISABLED))
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

    def _start_transition(self, checked: bool) -> None:
        """Animate the knob between checked states."""
        self._animation.stop()
        self._animation.setStartValue(self._position)
        self._animation.setEndValue(1.0 if checked else 0.0)
        self._animation.start()

    def _on_animation_value_changed(self, value: object) -> None:
        """Update the animated knob position."""
        self._position = float(value) if isinstance(value, (int, float)) else self._position
        self.update()

    def paintEvent(self, event: QEvent) -> None:
        """Paint the switch as a rounded pill."""
        del event

        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)

        track_rect = self.rect().adjusted(1, 1, -1, -1)
        radius = track_rect.height() / 2
        track_color: QColor | QLinearGradient
        if not self.isEnabled():
            track_color = self._track_disabled_color

        elif self.isChecked():
            track_color = QLinearGradient(track_rect.topLeft(), track_rect.topRight())
            track_color.setColorAt(0.0, self._track_on_color)
            track_color.setColorAt(1.0, self._track_on_end_color)

        else:
            track_color = self._track_off_color

        if not self.isEnabled():
            text_color = self._text_disabled_color

        else:
            text_color = self._text_on_color if self.isChecked() else self._text_off_color

        painter.setPen(self._track_border_color)
        painter.setBrush(track_color)
        painter.drawRoundedRect(track_rect, radius, radius)

        knob_size = track_rect.height() - (self._knob_inset * 2)
        knob_y = track_rect.y() + self._knob_inset
        knob_start = track_rect.x() + self._knob_inset
        knob_end = track_rect.right() - knob_size - self._knob_inset
        knob_x = round(knob_start + ((knob_end - knob_start) * self._position))

        painter.setPen(Qt.PenStyle.NoPen)
        painter.setBrush(self._knob_disabled_color if not self.isEnabled() else self._knob_color)
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
