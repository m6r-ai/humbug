"""
Overlay widget for the Humbug onboarding product tour.

This module is purely presentational.  It knows nothing about tour steps,
persistence or the wider application: it is told what to display and emits
signals when the user navigates or dismisses the tour.  TourController is
responsible for supplying the step content and acting on the signals.
"""

from collections.abc import Callable

from PySide6.QtCore import (
    QAbstractAnimation, QEasingCurve, QPoint, QPropertyAnimation, QRect, QRectF, QSize, Qt, QVariantAnimation, Signal
)
from PySide6.QtGui import QColor, QKeyEvent, QPainter, QPainterPath, QPaintEvent, QPen, QResizeEvent
from PySide6.QtWidgets import QGraphicsOpacityEffect, QHBoxLayout, QLabel, QPushButton, QScrollArea, QVBoxLayout, QWidget

from desktop.color_role import ColorRole
from desktop.style_manager import StyleManager


def _scroll_target_into_view(target: QWidget) -> None:
    """Ask the nearest enclosing scroll area (if any) to reveal target."""
    ancestor = target.parentWidget()
    while ancestor is not None:
        if isinstance(ancestor, QScrollArea):
            ancestor.ensureWidgetVisible(target)
            return

        ancestor = ancestor.parentWidget()


_OVERLAY_FADE_DURATION_MS = 220
_SPOTLIGHT_GLIDE_DURATION_MS = 450
_PULSE_DURATION_MS = 1400
_PULSE_MIN_ALPHA = 0.35
_PULSE_MAX_ALPHA = 1.0
_DIM_BASE_ALPHA = 210


class TourOverlay(QWidget):
    """
    Full-area overlay that dims the application and spotlights one widget at a time.

    Only the card itself carries a QGraphicsOpacityEffect.  Qt's effect
    compositing breaks (silently, with a painter-reentrancy error) when a
    widget with an effect has an ancestor that also has one, so the dim
    background and the pulsing ring are plain manual painting driven by
    tracked alpha values instead.
    """

    next_requested = Signal()
    back_requested = Signal()
    skip_requested = Signal()

    def __init__(self, parent: QWidget) -> None:
        super().__init__(parent)
        self._style_manager = StyleManager()
        self._spotlight_rect = QRect()
        self._current_target: QWidget | None = None
        self._has_shown_step = False
        self._card_fade_animation: QPropertyAnimation | None = None
        self._dim_fade_animation: QVariantAnimation | None = None
        self._spotlight_animation: QVariantAnimation | None = None
        self._spotlight_alpha_animation: QVariantAnimation | None = None
        self._card_position_animation: QVariantAnimation | None = None
        self._dim_alpha = 0.0
        self._pulse_alpha = _PULSE_MIN_ALPHA
        self._spotlight_alpha = 0.0

        self.setFocusPolicy(Qt.FocusPolicy.StrongFocus)

        self._pulse_animation = QVariantAnimation(self)
        self._pulse_animation.setDuration(_PULSE_DURATION_MS)
        self._pulse_animation.setLoopCount(-1)
        self._pulse_animation.setEasingCurve(QEasingCurve.Type.InOutSine)
        self._pulse_animation.setKeyValueAt(0.0, _PULSE_MIN_ALPHA)
        self._pulse_animation.setKeyValueAt(0.5, _PULSE_MAX_ALPHA)
        self._pulse_animation.setKeyValueAt(1.0, _PULSE_MIN_ALPHA)
        self._pulse_animation.valueChanged.connect(self._on_pulse_value_changed)

        self._card = QWidget(self)
        self._card.setObjectName("_tour_card")

        self._opacity_effect = QGraphicsOpacityEffect(self._card)
        self._opacity_effect.setOpacity(0.0)
        self._card.setGraphicsEffect(self._opacity_effect)

        card_layout = QVBoxLayout(self._card)
        self._card_layout = card_layout

        self._step_label = QLabel(self._card)
        card_layout.addWidget(self._step_label)

        self._title_label = QLabel(self._card)
        self._title_label.setWordWrap(True)
        card_layout.addWidget(self._title_label)

        self._description_label = QLabel(self._card)
        self._description_label.setWordWrap(True)
        card_layout.addWidget(self._description_label)

        button_row = QHBoxLayout()
        self._skip_button = QPushButton(self._card)
        self._skip_button.clicked.connect(self.skip_requested)
        button_row.addWidget(self._skip_button)
        button_row.addStretch()

        self._back_button = QPushButton(self._card)
        self._back_button.clicked.connect(self.back_requested)
        button_row.addWidget(self._back_button)

        self._next_button = QPushButton(self._card)
        self._next_button.clicked.connect(self.next_requested)
        button_row.addWidget(self._next_button)

        card_layout.addLayout(button_row)

        self.apply_style()

    def apply_style(self) -> None:
        """Refresh colours, fonts and spacing from the style manager."""
        sm = self._style_manager
        base_font_size = sm.base_font_size() * sm.zoom_factor()
        radius = sm.radius("panel")
        card_width = sm.scale(320)

        self._card.setFixedWidth(card_width)
        self._card_layout.setContentsMargins(sm.scale(16), sm.scale(16), sm.scale(16), sm.scale(16))
        self._card_layout.setSpacing(sm.scale(8))

        self._card.setStyleSheet(f"""
            QWidget#_tour_card {{
                background-color: {sm.get_color_str(ColorRole.BACKGROUND_DIALOG)};
                border: 1px solid {sm.get_color_str(ColorRole.SPLITTER)};
                border-radius: {radius}px;
            }}
        """)

        self._step_label.setStyleSheet(
            f"color: {sm.get_color_str(ColorRole.TEXT_INACTIVE)}; font-size: {base_font_size * 0.9}pt; border: none;"
        )
        self._title_label.setStyleSheet(
            f"color: {sm.get_color_str(ColorRole.TEXT_HEADING)}; "
            f"font-size: {base_font_size * 1.1}pt; font-weight: bold; border: none;"
        )
        self._description_label.setStyleSheet(
            f"color: {sm.get_color_str(ColorRole.TEXT_PRIMARY)}; font-size: {base_font_size}pt; border: none;"
        )

        button_style = f"""
            QPushButton {{
                background-color: {sm.get_color_str(ColorRole.BUTTON_SECONDARY_BACKGROUND)};
                color: {sm.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: none;
                border-radius: {sm.radius()}px;
                padding: {sm.scale(4)}px {sm.scale(10)}px;
                font-size: {base_font_size}pt;
            }}
            QPushButton:hover {{
                background-color: {sm.get_color_str(ColorRole.BUTTON_SECONDARY_BACKGROUND_HOVER)};
            }}
            QPushButton:pressed {{
                background-color: {sm.get_color_str(ColorRole.BUTTON_SECONDARY_BACKGROUND_PRESSED)};
            }}
        """
        self._skip_button.setStyleSheet(button_style)
        self._back_button.setStyleSheet(button_style)

        self._next_button.setStyleSheet(f"""
            QPushButton {{
                background-color: {sm.get_color_str(ColorRole.BUTTON_BACKGROUND_RECOMMENDED)};
                color: {sm.get_color_str(ColorRole.TEXT_RECOMMENDED)};
                border: none;
                border-radius: {sm.radius()}px;
                padding: {sm.scale(4)}px {sm.scale(10)}px;
                font-size: {base_font_size}pt;
            }}
            QPushButton:hover {{
                background-color: {sm.get_color_str(ColorRole.BUTTON_BACKGROUND_RECOMMENDED_HOVER)};
            }}
            QPushButton:pressed {{
                background-color: {sm.get_color_str(ColorRole.BUTTON_BACKGROUND_RECOMMENDED_PRESSED)};
            }}
        """)

        self._card.adjustSize()
        self._position_card()
        self.update()

    def show_step(
        self,
        title: str,
        description: str,
        step_label: str,
        back_label: str,
        skip_label: str,
        next_label: str,
        can_go_back: bool,
        spotlight_target: QWidget | None,
    ) -> None:
        """Display one tour step: its text, its buttons, and its spotlight target."""
        content = (title, description, step_label, back_label, skip_label, next_label, can_go_back)

        if not self._has_shown_step:
            self._has_shown_step = True
            self._apply_content(*content)
            self._current_target = spotlight_target
            self._spotlight_rect = self._resolve_spotlight_rect(spotlight_target)
            self._spotlight_alpha = 0.0 if self._spotlight_rect.isEmpty() else 1.0
            self._card.adjustSize()
            self._position_card()
            self._set_pulse_active(not self._spotlight_rect.isEmpty())
            self.update()
            self._fade_overlay_in()
            self._next_button.setFocus()
            return

        self._current_target = spotlight_target
        self._apply_content(*content)
        self._card.adjustSize()
        self._position_card()
        self._animate_spotlight_to(self._resolve_spotlight_rect(spotlight_target))
        self.raise_()
        self._next_button.setFocus()

    def fade_out(self, on_finished: Callable[[], None]) -> None:
        """Fade the whole overlay out, then invoke on_finished."""
        self._set_pulse_active(False)
        self._animate_dim(self._dim_alpha, 0.0, on_finished)
        self._animate_card_opacity(self._opacity_effect.opacity(), 0.0, _OVERLAY_FADE_DURATION_MS)

    def refresh_layout(self) -> None:
        """Recompute the spotlight rect for the current target and reposition the card."""
        self._stop_transition_animations()
        self._spotlight_rect = self._resolve_spotlight_rect(self._current_target)
        self._spotlight_alpha = 0.0 if self._spotlight_rect.isEmpty() else 1.0
        self._card.adjustSize()
        self._position_card()
        self._set_pulse_active(not self._spotlight_rect.isEmpty())
        self.update()

    def _stop_transition_animations(self) -> None:
        """Stop any in-flight spotlight/card-position transition animations."""
        for animation in (self._spotlight_animation, self._spotlight_alpha_animation, self._card_position_animation):
            if animation is not None:
                animation.stop()

        self._spotlight_animation = None
        self._spotlight_alpha_animation = None
        self._card_position_animation = None

    def _fade_overlay_in(self) -> None:
        """Fade the whole overlay in from fully transparent."""
        self.raise_()
        self.show()
        self._animate_dim(self._dim_alpha, 1.0)
        self._animate_card_opacity(self._opacity_effect.opacity(), 1.0, _OVERLAY_FADE_DURATION_MS)

    def _animate_dim(self, start: float, end: float, on_finished: Callable[[], None] | None = None) -> None:
        """Animate the dim background's alpha multiplier (tour open/close only)."""
        if self._dim_fade_animation is not None:
            self._dim_fade_animation.stop()

        animation = QVariantAnimation(self)
        animation.setDuration(_OVERLAY_FADE_DURATION_MS)
        animation.setEasingCurve(QEasingCurve.Type.InOutQuad)
        animation.setStartValue(start)
        animation.setEndValue(end)
        animation.valueChanged.connect(self._on_dim_alpha_changed)
        if on_finished is not None:
            animation.finished.connect(on_finished)

        self._dim_fade_animation = animation
        animation.start()

    def _on_dim_alpha_changed(self, value: float) -> None:
        """Advance the dim background's alpha by one animation frame."""
        self._dim_alpha = value
        self.update()

    def _animate_card_opacity(
        self, start: float, end: float, duration: int, on_finished: Callable[[], None] | None = None,
    ) -> None:
        """Animate the card's own opacity effect."""
        if self._card_fade_animation is not None:
            self._card_fade_animation.stop()

        animation = QPropertyAnimation(self._opacity_effect, b"opacity", self)
        animation.setDuration(duration)
        animation.setEasingCurve(QEasingCurve.Type.InOutQuad)
        animation.setStartValue(start)
        animation.setEndValue(end)
        if on_finished is not None:
            animation.finished.connect(on_finished)

        self._card_fade_animation = animation
        animation.start()

    def _apply_content(
        self, title: str, description: str, step_label: str, back_label: str, skip_label: str,
        next_label: str, can_go_back: bool,
    ) -> None:
        """Set the card's text and buttons to one step's content."""
        self._title_label.setText(title)
        self._description_label.setText(description)
        self._step_label.setText(step_label)
        self._skip_button.setText(skip_label)
        self._back_button.setText(back_label)
        self._back_button.setVisible(can_go_back)
        self._next_button.setText(next_label)

    def _animate_spotlight_to(self, new_rect: QRect) -> None:
        """
        Transition the spotlight (and the card following it) to new_rect.

        When both the old and new spotlights are real widgets, the ring glides
        directly between them.  When one side has no spotlight at all (e.g. the
        centred Welcome step), there is no rect to glide from/to, so instead the
        ring cross-fades in or out in place while the card's position is animated
        on its own between its current spot and its new one.
        """
        old_rect = self._spotlight_rect
        self._stop_transition_animations()

        card_size = self._card.size()
        old_card_pos = self._card.pos()
        new_card_pos = self._compute_card_position(new_rect, card_size)

        if not old_rect.isEmpty() and not new_rect.isEmpty():
            self._spotlight_alpha = 1.0
            animation = QVariantAnimation(self)
            animation.setDuration(_SPOTLIGHT_GLIDE_DURATION_MS)
            animation.setEasingCurve(QEasingCurve.Type.OutCubic)
            animation.setStartValue(old_rect)
            animation.setEndValue(new_rect)
            animation.valueChanged.connect(self._on_spotlight_rect_animated)
            self._spotlight_animation = animation
            animation.start()
            return

        if old_rect.isEmpty() and new_rect.isEmpty():
            self._spotlight_rect = new_rect
            self._position_card()
            self._set_pulse_active(False)
            self.update()
            return

        if new_rect.isEmpty():
            # Fade the ring out in place (nothing to glide it towards) while the card leaves.
            self._spotlight_rect = old_rect
            self._animate_spotlight_alpha(1.0, 0.0, on_finished=lambda: self._finish_spotlight_fade(new_rect))

        else:
            # Show the ring at its new spot immediately and fade it in while the card arrives.
            self._spotlight_rect = new_rect
            self._animate_spotlight_alpha(0.0, 1.0)

        self._set_pulse_active(not new_rect.isEmpty())
        self._animate_card_position(old_card_pos, new_card_pos)

    def _finish_spotlight_fade(self, final_rect: QRect) -> None:
        """Settle the spotlight rect once a fade-out transition has finished."""
        self._spotlight_rect = final_rect
        self.update()

    def _on_spotlight_rect_animated(self, value: QRect) -> None:
        """Advance the spotlight rect by one animation frame, keeping the card in step."""
        self._spotlight_rect = value
        self._position_card()
        self.update()

    def _animate_spotlight_alpha(self, start: float, end: float, on_finished: Callable[[], None] | None = None) -> None:
        """Cross-fade the spotlight ring/hole in or out in place."""
        animation = QVariantAnimation(self)
        animation.setDuration(_SPOTLIGHT_GLIDE_DURATION_MS)
        animation.setEasingCurve(QEasingCurve.Type.InOutQuad)
        animation.setStartValue(start)
        animation.setEndValue(end)
        animation.valueChanged.connect(self._on_spotlight_alpha_animated)
        if on_finished is not None:
            animation.finished.connect(on_finished)

        self._spotlight_alpha_animation = animation
        animation.start()

    def _on_spotlight_alpha_animated(self, value: float) -> None:
        """Advance the spotlight ring/hole's cross-fade by one animation frame."""
        self._spotlight_alpha = value
        self.update()

    def _animate_card_position(self, start: QPoint, end: QPoint) -> None:
        """Glide the card between two positions, independently of the spotlight ring."""
        animation = QVariantAnimation(self)
        animation.setDuration(_SPOTLIGHT_GLIDE_DURATION_MS)
        animation.setEasingCurve(QEasingCurve.Type.OutCubic)
        animation.setStartValue(start)
        animation.setEndValue(end)
        animation.valueChanged.connect(self._on_card_position_animated)
        self._card_position_animation = animation
        animation.start()

    def _on_card_position_animated(self, value: QPoint) -> None:
        """Advance the card's position by one animation frame."""
        self._card.move(value)

    def _set_pulse_active(self, active: bool) -> None:
        """Start, resume, or pause the idle pulsing glow on the spotlight ring."""
        if active:
            if self._pulse_animation.state() == QAbstractAnimation.State.Paused:
                self._pulse_animation.resume()

            elif self._pulse_animation.state() != QAbstractAnimation.State.Running:
                self._pulse_animation.start()

        else:
            if self._pulse_animation.state() == QAbstractAnimation.State.Running:
                self._pulse_animation.pause()

    def _on_pulse_value_changed(self, value: float) -> None:
        """Advance the spotlight ring's pulse glow by one animation frame."""
        self._pulse_alpha = value
        if not self._spotlight_rect.isEmpty():
            self.update()

    def _resolve_spotlight_rect(self, target: QWidget | None) -> QRect:
        """Return target's geometry in this overlay's coordinate space, or an empty rect."""
        if target is None:
            return QRect()

        parent = self.parentWidget()
        if parent is None:
            return QRect()

        try:
            if not target.isVisible():
                return QRect()

            _scroll_target_into_view(target)
            top_left = target.mapTo(parent, QPoint(0, 0))
            return QRect(top_left, target.size())

        except RuntimeError:
            # The target's underlying C++ object has already been destroyed.
            return QRect()

    def _position_card(self) -> None:
        """Position the coach-mark card near the spotlight, or centred if there is none."""
        # The card has a fixed width (see apply_style), so its actual, already-clamped
        # size must be used here rather than sizeHint(), which ignores that clamp.
        card_size = self._card.size()
        pos = self._compute_card_position(self._spotlight_rect, card_size)
        self._card.setGeometry(QRect(pos, card_size))

    def _compute_card_position(self, spotlight_rect: QRect, card_size: QSize) -> QPoint:
        """Compute where the card should sit for spotlight_rect, without moving it."""
        margin = self._style_manager.scale(16)
        overlay_rect = self.rect()

        if spotlight_rect.isEmpty():
            x = (overlay_rect.width() - card_size.width()) // 2
            y = (overlay_rect.height() - card_size.height()) // 2
            return QPoint(x, y)

        spot = spotlight_rect
        gap = self._style_manager.scale(12)

        space_below = overlay_rect.bottom() - spot.bottom()
        space_above = spot.top() - overlay_rect.top()
        space_right = overlay_rect.right() - spot.right()
        space_left = spot.left() - overlay_rect.left()

        if space_below >= card_size.height() + gap:
            x = spot.center().x() - card_size.width() // 2
            y = spot.bottom() + gap

        elif space_above >= card_size.height() + gap:
            x = spot.center().x() - card_size.width() // 2
            y = spot.top() - gap - card_size.height()

        elif space_right >= card_size.width() + gap:
            x = spot.right() + gap
            y = spot.center().y() - card_size.height() // 2

        elif space_left >= card_size.width() + gap:
            x = spot.left() - gap - card_size.width()
            y = spot.center().y() - card_size.height() // 2

        else:
            x = spot.center().x() - card_size.width() // 2
            y = spot.center().y() - card_size.height() // 2

        x = max(margin, min(x, overlay_rect.width() - card_size.width() - margin))
        y = max(margin, min(y, overlay_rect.height() - card_size.height() - margin))
        return QPoint(x, y)

    def paintEvent(self, _event: QPaintEvent) -> None:
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)

        dim = QColor(self._style_manager.get_color(ColorRole.BACKGROUND_PRIMARY))
        dim.setAlpha(int(_DIM_BASE_ALPHA * self._dim_alpha))

        full_path = QPainterPath()
        full_path.addRect(QRectF(self.rect()))

        if self._spotlight_rect.isEmpty():
            painter.fillPath(full_path, dim)

        else:
            radius = self._style_manager.radius("surface")
            hole_path = QPainterPath()
            hole_path.addRoundedRect(QRectF(self._spotlight_rect), radius, radius)
            painter.fillPath(full_path.subtracted(hole_path), dim)

            # When cross-fading in/out (rather than gliding between two real spotlights),
            # the hole itself fades between fully dimmed and fully clear.
            if self._spotlight_alpha < 1.0:
                hole_dim = QColor(dim)
                hole_dim.setAlpha(int(dim.alpha() * (1.0 - self._spotlight_alpha)))
                painter.fillPath(hole_path, hole_dim)

            ring_color = QColor(self._style_manager.get_color(ColorRole.BRAND_PRIMARY))
            ring_color.setAlphaF(self._pulse_alpha * self._dim_alpha * self._spotlight_alpha)
            pen = QPen(ring_color)
            pen.setWidthF(2.0)
            painter.setPen(pen)
            painter.setBrush(Qt.BrushStyle.NoBrush)
            painter.drawRoundedRect(QRectF(self._spotlight_rect).adjusted(1, 1, -1, -1), radius, radius)

    def resizeEvent(self, event: QResizeEvent) -> None:
        super().resizeEvent(event)
        self.refresh_layout()

    def keyPressEvent(self, event: QKeyEvent) -> None:
        if event.key() == Qt.Key.Key_Escape:
            self.skip_requested.emit()
            return

        if event.key() in (Qt.Key.Key_Return, Qt.Key.Key_Enter, Qt.Key.Key_Right):
            self.next_requested.emit()
            return

        if event.key() == Qt.Key.Key_Left and self._back_button.isVisible():
            self.back_requested.emit()
            return

        super().keyPressEvent(event)
