"""Small custom-painted widgets used by Mission Control."""

import math

from PySide6.QtCore import QEasingCurve, QPropertyAnimation, QRectF, QSize, Qt, QTimer
from PySide6.QtGui import QColor, QFont, QFontMetrics, QPainter, QPaintEvent, QPen, QShowEvent
from PySide6.QtWidgets import QGraphicsOpacityEffect, QSizePolicy, QWidget

from desktop.color_role import ColorRole
from desktop.mission_tab.mission_display import MissionStepState, MissionTone
from desktop.mission_tab.mission_motion import MissionMotion
from desktop.style_manager import StyleManager


_TONE_ROLES = {
    MissionTone.ACCENT: ColorRole.BUTTON_BACKGROUND_RECOMMENDED,
    MissionTone.INFO: ColorRole.TEXT_LINK,
    MissionTone.SUCCESS: ColorRole.TEXT_SUCCESS,
    MissionTone.WARNING: ColorRole.MESSAGE_WARNING,
    MissionTone.DANGER: ColorRole.TEXT_ERROR,
    MissionTone.NEUTRAL: ColorRole.TEXT_INACTIVE,
}


def tone_color(tone: MissionTone) -> QColor:
    """Return the theme colour for a tone."""
    return StyleManager().get_color(_TONE_ROLES[tone])


def _luminance(color: QColor) -> float:
    """Return the relative luminance of a colour, as defined by WCAG."""
    def channel(value: int) -> float:
        v = value / 255
        return v / 12.92 if v <= 0.03928 else ((v + 0.055) / 1.055) ** 2.4

    return 0.2126 * channel(color.red()) + 0.7152 * channel(color.green()) + 0.0722 * channel(color.blue())


def contrast_ratio(first: QColor, second: QColor) -> float:
    """Return the WCAG contrast ratio between two colours."""
    a, b = _luminance(first), _luminance(second)
    return (max(a, b) + 0.05) / (min(a, b) + 0.05)


def readable(color: QColor, background: QColor, minimum: float = 4.5) -> QColor:
    """
    Return a colour adjusted just enough to meet a contrast ratio on a background.

    Theme accents are chosen for fills; used as text on a light tint some fall
    short of WCAG AA, so they are darkened (or, on dark backgrounds, lightened)
    a step at a time until they are readable.

    Args:
        color: The intended colour
        background: What it is drawn on
        minimum: Required contrast ratio; 4.5 for text, 3.0 for icons and large text
    """
    result = QColor(color)
    darken = _luminance(background) > 0.18
    for _ in range(24):
        if contrast_ratio(result, background) >= minimum:
            break

        result = result.darker(110) if darken else result.lighter(112)

    return result


def blend(top: QColor, alpha: float, bottom: QColor) -> QColor:
    """Return the colour of top drawn at the given opacity over bottom."""
    return QColor(
        round(top.red() * alpha + bottom.red() * (1 - alpha)),
        round(top.green() * alpha + bottom.green() * (1 - alpha)),
        round(top.blue() * alpha + bottom.blue() * (1 - alpha)),
    )


def surface_colors() -> tuple[QColor, QColor]:
    """
    Return the page and card colours for the current theme.

    Cards are always the lighter of the two so they read as raised: white on a
    soft grey page in light themes, a lifted grey on a near-black page in dark ones.
    """
    style_manager = StyleManager()
    primary = style_manager.get_color(ColorRole.BACKGROUND_PRIMARY)
    secondary = style_manager.get_color(ColorRole.BACKGROUND_SECONDARY)
    tertiary = style_manager.get_color(ColorRole.BACKGROUND_TERTIARY)
    page = primary if _luminance(primary) < _luminance(secondary) else secondary
    card = tertiary if _luminance(tertiary) > _luminance(secondary) else secondary
    return page, card


def tone_text(tone: MissionTone, alpha: float = 0.14) -> QColor:
    """Return a tone's colour made readable as text on its own tint over a card."""
    _page, card = surface_colors()
    color = tone_color(tone)
    return readable(color, blend(color, alpha, card))


def tone_rgba(tone: MissionTone, alpha: float) -> str:
    """Return a tone's colour as a CSS rgba() string with the given opacity."""
    color = tone_color(tone)
    return f"rgba({color.red()}, {color.green()}, {color.blue()}, {alpha})"


def fade_in(widget: QWidget, duration_ms: int = 220) -> None:
    """
    Fade a newly shown widget in.

    The opacity effect is removed once the fade finishes, because a widget that
    keeps one is redrawn off screen on every paint.

    Args:
        widget: The widget to fade in
        duration_ms: Length of the fade
    """
    if MissionMotion().reduced():
        return

    effect = QGraphicsOpacityEffect(widget)
    effect.setOpacity(0.0)
    widget.setGraphicsEffect(effect)
    animation = QPropertyAnimation(effect, b"opacity", widget)
    animation.setDuration(duration_ms)
    animation.setStartValue(0.0)
    animation.setEndValue(1.0)
    animation.setEasingCurve(QEasingCurve.Type.OutCubic)
    animation.finished.connect(lambda: widget.setGraphicsEffect(None))  # type: ignore[arg-type]
    animation.start(QPropertyAnimation.DeletionPolicy.DeleteWhenStopped)


def _scaled(value: float) -> int:
    """Scale a pixel size by the current zoom factor."""
    return max(1, round(value * StyleManager().zoom_factor()))


class GlyphBadge(QWidget):
    """A tinted circle with a symbol in the middle."""

    def __init__(self, glyph: str, tone: MissionTone, diameter: int, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._glyph = glyph
        self._tone = tone
        self._diameter = diameter
        self.setSizePolicy(QSizePolicy.Policy.Fixed, QSizePolicy.Policy.Fixed)
        self.setAttribute(Qt.WidgetAttribute.WA_TranslucentBackground)

    def set_badge(self, glyph: str, tone: MissionTone) -> None:
        """Change the symbol and colour."""
        if glyph != self._glyph or tone != self._tone:
            self._glyph = glyph
            self._tone = tone
            self.update()

    def sizeHint(self) -> QSize:
        size = _scaled(self._diameter)
        return QSize(size, size)

    def minimumSizeHint(self) -> QSize:
        return self.sizeHint()

    def paintEvent(self, event: QPaintEvent) -> None:
        """Draw the badge."""
        super().paintEvent(event)
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        color = tone_color(self._tone)
        background = QColor(color)
        background.setAlphaF(0.16)
        _page, card = surface_colors()
        glyph_color = readable(color, blend(color, 0.16, card), 3.0)
        size = min(self.width(), self.height())
        rect = QRectF((self.width() - size) / 2, (self.height() - size) / 2, size, size)
        painter.setPen(Qt.PenStyle.NoPen)
        painter.setBrush(background)
        painter.drawEllipse(rect)

        font = QFont(self.font())
        font.setPixelSize(max(6, round(size * 0.46)))
        font.setBold(True)
        painter.setFont(font)
        painter.setPen(glyph_color)
        painter.drawText(rect, int(Qt.AlignmentFlag.AlignCenter), self._glyph)
        painter.end()


class StepMarker(QWidget):
    """The circle and connecting line drawn beside each plan step."""

    def __init__(self, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._number = ""
        self._state = MissionStepState.PENDING
        self._first = False
        self._last = False
        self.setSizePolicy(QSizePolicy.Policy.Fixed, QSizePolicy.Policy.Expanding)

    def set_step(self, number: str, state: MissionStepState, first: bool, last: bool) -> None:
        """Describe the step this marker belongs to."""
        self._number = number
        self._state = state
        self._first = first
        self._last = last
        self.update()

    def sizeHint(self) -> QSize:
        return QSize(_scaled(30), _scaled(44))

    def minimumSizeHint(self) -> QSize:
        return QSize(_scaled(30), _scaled(30))

    def paintEvent(self, event: QPaintEvent) -> None:
        """Draw the connector and the state circle."""
        super().paintEvent(event)
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        diameter = _scaled(24)
        center_x = self.width() / 2
        center_y = _scaled(22)
        line_color = QColor(StyleManager().get_color(ColorRole.TEXT_INACTIVE))
        line_color.setAlphaF(0.35)
        painter.setPen(QPen(line_color, _scaled(2)))
        if not self._first:
            painter.drawLine(int(center_x), 0, int(center_x), int(center_y - diameter / 2))

        if not self._last:
            painter.drawLine(int(center_x), int(center_y + diameter / 2), int(center_x), self.height())

        rect = QRectF(center_x - diameter / 2, center_y - diameter / 2, diameter, diameter)
        font = QFont(self.font())
        font.setPixelSize(max(6, round(diameter * 0.5)))
        font.setBold(True)
        painter.setFont(font)

        if self._state == MissionStepState.DONE:
            color = tone_color(MissionTone.SUCCESS)
            painter.setPen(Qt.PenStyle.NoPen)
            painter.setBrush(color)
            painter.drawEllipse(rect)
            painter.setPen(surface_colors()[1])
            painter.drawText(rect, int(Qt.AlignmentFlag.AlignCenter), "✓")

        elif self._state == MissionStepState.STOPPED:
            color = tone_color(MissionTone.NEUTRAL)
            painter.setPen(Qt.PenStyle.NoPen)
            painter.setBrush(color)
            painter.drawEllipse(rect)
            painter.setPen(surface_colors()[1])
            painter.drawText(rect, int(Qt.AlignmentFlag.AlignCenter), "■")

        elif self._state == MissionStepState.BLOCKED:
            color = tone_color(MissionTone.DANGER)
            painter.setPen(Qt.PenStyle.NoPen)
            painter.setBrush(color)
            painter.drawEllipse(rect)
            painter.setPen(surface_colors()[1])
            painter.drawText(rect, int(Qt.AlignmentFlag.AlignCenter), "!")

        elif self._state == MissionStepState.ACTIVE:
            color = tone_color(MissionTone.ACCENT)
            halo = QColor(color)
            halo.setAlphaF(0.18)
            painter.setPen(Qt.PenStyle.NoPen)
            painter.setBrush(halo)
            painter.drawEllipse(rect.adjusted(-3, -3, 3, 3))
            painter.setPen(QPen(color, _scaled(2)))
            painter.setBrush(surface_colors()[1])
            painter.drawEllipse(rect)
            painter.setPen(color)
            painter.drawText(rect, int(Qt.AlignmentFlag.AlignCenter), self._number)

        else:
            color = tone_color(MissionTone.NEUTRAL)
            painter.setPen(QPen(color, _scaled(1.5)))
            painter.setBrush(surface_colors()[1])
            painter.drawEllipse(rect)
            painter.drawText(rect, int(Qt.AlignmentFlag.AlignCenter), self._number)

        painter.end()


class BusySpinner(QWidget):
    """A small rotating arc shown while an agent is working."""

    def __init__(self, tone: MissionTone, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._tone = tone
        self._angle = 0
        self.setSizePolicy(QSizePolicy.Policy.Fixed, QSizePolicy.Policy.Fixed)

    def advance(self) -> None:
        """Rotate the arc one step, unless motion is reduced."""
        if MissionMotion().reduced():
            return

        self._angle = (self._angle + 30) % 360
        self.update()

    def sizeHint(self) -> QSize:
        size = _scaled(14)
        return QSize(size, size)

    def paintEvent(self, event: QPaintEvent) -> None:
        """Draw the arc at its current angle."""
        super().paintEvent(event)
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        width = _scaled(2)
        rect = QRectF(width, width, self.width() - 2 * width, self.height() - 2 * width)
        track = QColor(tone_color(self._tone))
        track.setAlphaF(0.2)
        painter.setPen(QPen(track, width))
        painter.drawEllipse(rect)
        pen = QPen(tone_color(self._tone), width)
        pen.setCapStyle(Qt.PenCapStyle.RoundCap)
        painter.setPen(pen)
        painter.drawArc(rect, -self._angle * 16, 100 * 16)
        painter.end()


class StatusPill(QWidget):
    """A rounded, tinted label for a status."""

    def __init__(self, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._text = ""
        self._tone = MissionTone.NEUTRAL
        self._pulsing = False
        self._phase = 0.0
        self.setSizePolicy(QSizePolicy.Policy.Fixed, QSizePolicy.Policy.Fixed)
        self.setAttribute(Qt.WidgetAttribute.WA_TranslucentBackground)
        self._pulse_timer = QTimer(self)
        self._pulse_timer.setInterval(60)
        self._pulse_timer.timeout.connect(self._advance_pulse)

    def set_pill(self, text: str, tone: MissionTone, pulsing: bool = False) -> None:
        """Set the text and colour, optionally with a pulsing dot showing that something is live."""
        if text != self._text or tone != self._tone or pulsing != self._pulsing:
            self._text = text
            self._tone = tone
            self._pulsing = pulsing
            self.updateGeometry()
            self.update()

        if pulsing and not self._pulse_timer.isActive() and not MissionMotion().reduced():
            self._pulse_timer.start()

        elif not pulsing:
            self._pulse_timer.stop()

    def showEvent(self, event: QShowEvent) -> None:
        """Pulse again once shown, if the pill is live."""
        super().showEvent(event)
        if self._pulsing and not self._pulse_timer.isActive() and not MissionMotion().reduced():
            self._pulse_timer.start()

    def _advance_pulse(self) -> None:
        """Advance the pulse, pausing while the pill is not on screen and stopping if motion is reduced."""
        if MissionMotion().reduced():
            self._pulse_timer.stop()
            self._phase = 0.0
            self.update()
            return

        if not self.isVisible():
            # Hidden pills do not keep a timer running; showing the pill again starts it.
            self._pulse_timer.stop()
            return

        self._phase = (self._phase + 0.12) % (2 * math.pi)
        self.update()

    def _dot_space(self) -> int:
        """Return the width taken by the pulsing dot."""
        return _scaled(12) if self._pulsing else 0

    def restyle(self) -> None:
        """Pick up the current theme and zoom."""
        self.updateGeometry()
        self.update()

    def _pill_font(self) -> QFont:
        """Return the font the pill text is drawn in."""
        style_manager = StyleManager()
        font = QFont(self.font())
        font.setFamilies(style_manager.proportional_font_families())
        font.setPointSizeF(style_manager.base_font_size() * style_manager.zoom_factor() * 0.82)
        font.setWeight(QFont.Weight.DemiBold)
        return font

    def sizeHint(self) -> QSize:
        metrics = QFontMetrics(self._pill_font())
        return QSize(
            metrics.horizontalAdvance(self._text) + _scaled(20) + self._dot_space(), metrics.height() + _scaled(6)
        )

    def minimumSizeHint(self) -> QSize:
        return self.sizeHint()

    def paintEvent(self, event: QPaintEvent) -> None:
        """Draw the pill."""
        super().paintEvent(event)
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        color = tone_color(self._tone)
        fill = QColor(color)
        fill.setAlphaF(0.14)
        outline = QColor(color)
        outline.setAlphaF(0.32)
        rect = QRectF(0.5, 0.5, self.width() - 1, self.height() - 1)
        radius = rect.height() / 2
        painter.setPen(QPen(outline, 1))
        painter.setBrush(fill)
        painter.drawRoundedRect(rect, radius, radius)
        text_rect = rect
        if self._pulsing:
            dot = _scaled(6)
            glow = QColor(color)
            glow.setAlphaF(0.25 + 0.75 * (0.5 + 0.5 * math.sin(self._phase)))
            painter.setPen(Qt.PenStyle.NoPen)
            painter.setBrush(glow)
            painter.drawEllipse(QRectF(rect.left() + _scaled(9), rect.center().y() - dot / 2, dot, dot))
            text_rect = rect.adjusted(self._dot_space(), 0, 0, 0)

        painter.setFont(self._pill_font())
        painter.setPen(tone_text(self._tone))
        painter.drawText(text_rect, int(Qt.AlignmentFlag.AlignCenter), self._text)
        painter.end()
