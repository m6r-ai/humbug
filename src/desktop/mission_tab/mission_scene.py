"""Animated 3D-style scenes for Mission Control: the mission orb and the journey bar."""

from dataclasses import dataclass
import math
import random
import time

from PySide6.QtCore import QPointF, QRectF, QSize, Qt, QTimer
from PySide6.QtGui import (
    QBrush, QColor, QFont, QFontMetrics, QHideEvent, QLinearGradient, QPainter, QPaintEvent, QPen,
    QRadialGradient, QShowEvent
)
from PySide6.QtWidgets import QSizePolicy, QWidget

from desktop.color_role import ColorRole
from desktop.mission_tab.mission_display import MissionTone
from desktop.mission_tab.mission_painted import surface_colors, tone_color
from desktop.style_manager import StyleManager


_FRAME_MS = 33

# Orbits around the orb: inclination of the ring's plane, its turn in the screen plane, the
# speed of the satellite on it and where the satellite starts.
_ORBITS = (
    (1.18, -0.42, 0.9, 0.0),
    (1.25, 0.55, -0.7, 2.1),
    (1.32, 1.57, 0.55, 4.2),
)
_CONFETTI_SECONDS = 2.2
_CONFETTI_PIECES = 70

# How far, in unzoomed pixels per second of flight, confetti travels, and the size of a piece.
_CONFETTI_REACH = 220.0
_CONFETTI_PIECE = 9.0


def _scaled(value: float) -> float:
    """Scale a pixel size by the current zoom factor."""
    return value * StyleManager().zoom_factor()


def _shade(color: QColor, factor: int) -> QColor:
    """Return a colour made lighter (factor > 100) or darker (factor < 100)."""
    return color.lighter(factor) if factor >= 100 else color.darker(round(10000 / factor))


def _paint_sphere(painter: QPainter, center: QPointF, radius: float, color: QColor) -> None:
    """Draw a lit sphere: a radial body, a soft rim and a specular highlight."""
    body = QRadialGradient(center - QPointF(radius * 0.35, radius * 0.4), radius * 1.45)
    body.setColorAt(0.0, _shade(color, 175))
    body.setColorAt(0.45, color)
    body.setColorAt(1.0, _shade(color, 45))
    painter.setPen(Qt.PenStyle.NoPen)
    painter.setBrush(QBrush(body))
    painter.drawEllipse(center, radius, radius)

    rim = QRadialGradient(center, radius)
    rim.setColorAt(0.82, QColor(255, 255, 255, 0))
    rim.setColorAt(1.0, QColor(255, 255, 255, 46))
    painter.setBrush(QBrush(rim))
    painter.drawEllipse(center, radius, radius)

    highlight = QRadialGradient(center - QPointF(radius * 0.38, radius * 0.45), radius * 0.45)
    highlight.setColorAt(0.0, QColor(255, 255, 255, 170))
    highlight.setColorAt(1.0, QColor(255, 255, 255, 0))
    painter.setBrush(QBrush(highlight))
    painter.drawEllipse(center - QPointF(radius * 0.38, radius * 0.45), radius * 0.45, radius * 0.45)


@dataclass
class _Confetti:
    """One piece of confetti: where it is, where it is going, and how it tumbles."""
    x: float
    y: float
    dx: float
    dy: float
    spin: float
    turn: float
    color: QColor


class MissionOrb(QWidget):
    """
    A glossy sphere with satellites on tilted orbits, drawn in perspective.

    Satellites pass behind the sphere and in front of it, which gives the
    scene its depth.  It animates only while it is visible.
    """

    def __init__(self, glyph: str, tone: MissionTone, diameter: int, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._glyph = glyph
        self._tone = tone
        self._diameter = diameter
        self._start = time.monotonic()
        self.setSizePolicy(QSizePolicy.Policy.Fixed, QSizePolicy.Policy.Fixed)
        self.setAttribute(Qt.WidgetAttribute.WA_TranslucentBackground)
        self._timer = QTimer(self)
        self._timer.setInterval(_FRAME_MS)
        self._timer.timeout.connect(self.update)

    def sizeHint(self) -> QSize:
        size = round(_scaled(self._diameter))
        return QSize(size, size)

    def minimumSizeHint(self) -> QSize:
        return self.sizeHint()

    def showEvent(self, event: QShowEvent) -> None:
        """Animate while shown."""
        super().showEvent(event)
        self._timer.start()

    def hideEvent(self, event: QHideEvent) -> None:
        """Rest while hidden."""
        super().hideEvent(event)
        self._timer.stop()

    def _orbit_point(self, orbit: tuple[float, float, float, float], angle: float, radius: float) -> tuple[float, float, float]:
        """Return a point on an orbit as screen offsets and depth, depth positive towards the viewer."""
        inclination, turn, _speed, _start = orbit
        x = radius * math.cos(angle)
        y = radius * math.sin(angle) * math.cos(inclination)
        z = radius * math.sin(angle) * math.sin(inclination)
        sx = x * math.cos(turn) - y * math.sin(turn)
        sy = x * math.sin(turn) + y * math.cos(turn)
        perspective = 4.0 / (4.0 - z / radius)
        return sx * perspective, sy * perspective, z / radius

    def paintEvent(self, event: QPaintEvent) -> None:
        """Draw the orbits behind the sphere, the sphere, then the orbits in front of it."""
        super().paintEvent(event)
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        phase = time.monotonic() - self._start
        size = min(self.width(), self.height())
        center = QPointF(self.width() / 2, self.height() / 2 + math.sin(phase * 1.6) * size * 0.015)
        radius = size * 0.24
        orbit_radius = size * 0.4
        color = tone_color(self._tone)

        # A shadow below, so the orb floats.
        shadow = QRadialGradient(QPointF(center.x(), size * 0.92), size * 0.3)
        shadow.setColorAt(0.0, QColor(0, 0, 0, 55))
        shadow.setColorAt(1.0, QColor(0, 0, 0, 0))
        painter.setPen(Qt.PenStyle.NoPen)
        painter.setBrush(QBrush(shadow))
        painter.drawEllipse(QPointF(center.x(), size * 0.92), size * 0.3, size * 0.05)

        glow = QRadialGradient(center, radius * 2.0)
        halo = QColor(color)
        halo.setAlphaF(0.22 + 0.06 * math.sin(phase * 2.0))
        glow.setColorAt(0.0, halo)
        clear = QColor(color)
        clear.setAlphaF(0.0)
        glow.setColorAt(1.0, clear)
        painter.setBrush(QBrush(glow))
        painter.drawEllipse(center, radius * 2.0, radius * 2.0)

        for front in (False, True):
            self._paint_orbits(painter, center, orbit_radius, phase, color, front)
            if not front:
                _paint_sphere(painter, center, radius, color)
                font = QFont(self.font())
                font.setPixelSize(max(6, round(radius * 0.95)))
                font.setBold(True)
                painter.setFont(font)
                painter.setPen(QColor(0, 0, 0, 60))
                glyph_rect = QRectF(center.x() - radius, center.y() - radius, radius * 2, radius * 2)
                painter.drawText(glyph_rect.translated(0, radius * 0.05), int(Qt.AlignmentFlag.AlignCenter), self._glyph)
                painter.setPen(QColor("#ffffff"))
                painter.drawText(glyph_rect, int(Qt.AlignmentFlag.AlignCenter), self._glyph)

        painter.end()

    def _paint_orbits(
        self, painter: QPainter, center: QPointF, radius: float, phase: float, color: QColor, front: bool
    ) -> None:
        """Draw the half of each orbit, and the satellites, on one side of the sphere."""
        line = QColor(color)
        steps = 72
        for index, orbit in enumerate(_ORBITS):
            # The orbits precess slowly, so the whole scene turns.
            turning = (orbit[0], orbit[1] + phase * 0.12, orbit[2], orbit[3])
            for step in range(steps):
                a0 = math.tau * step / steps
                a1 = math.tau * (step + 1) / steps
                x0, y0, z0 = self._orbit_point(turning, a0, radius)
                x1, y1, z1 = self._orbit_point(turning, a1, radius)
                if ((z0 + z1) / 2 >= 0) != front:
                    continue

                line.setAlphaF(0.18 + 0.22 * max(0.0, (z0 + z1) / 2) if front else 0.1)
                painter.setPen(QPen(line, max(1.0, radius * 0.018)))
                painter.drawLine(center + QPointF(x0, y0), center + QPointF(x1, y1))

            angle = orbit[3] + phase * orbit[2]
            x, y, z = self._orbit_point(turning, angle, radius)
            if (z >= 0) != front:
                continue

            satellite = QColor(tone_color((MissionTone.INFO, MissionTone.SUCCESS, MissionTone.WARNING)[index]))
            depth = 0.7 + 0.3 * (z + 1) / 2
            painter.setOpacity(0.55 + 0.45 * (z + 1) / 2)
            _paint_sphere(painter, center + QPointF(x, y), radius * 0.11 * depth * 1.4, satellite)
            painter.setOpacity(1.0)


class ConfettiOverlay(QWidget):
    """
    A see-through layer over a page that throws a burst of confetti.

    It covers its parent and lets every click through, so the confetti can
    fly across the page rather than being clipped to the widget it comes from.
    It animates only while confetti is in the air.
    """

    def __init__(self, parent: QWidget) -> None:
        super().__init__(parent)
        self._confetti: list[_Confetti] = []
        self._origin = QPointF()
        self._started = 0.0
        self.setAttribute(Qt.WidgetAttribute.WA_TransparentForMouseEvents)
        self.setAttribute(Qt.WidgetAttribute.WA_TranslucentBackground)
        self._timer = QTimer(self)
        self._timer.setInterval(_FRAME_MS)
        self._timer.timeout.connect(self._tick)
        self.hide()

    def burst(self, origin: QPointF) -> None:
        """
        Throw confetti from a point.

        Args:
            origin: Where the confetti comes from, in this widget's coordinates
        """
        colors = [tone_color(tone) for tone in (
            MissionTone.ACCENT, MissionTone.SUCCESS, MissionTone.WARNING, MissionTone.INFO, MissionTone.DANGER
        )]
        rng = random.Random()
        self._confetti = []
        for _ in range(_CONFETTI_PIECES):
            angle = rng.uniform(-math.pi * 0.95, -math.pi * 0.05)
            speed = rng.uniform(0.9, 1.9)
            self._confetti.append(_Confetti(
                x=0.0, y=0.0, dx=math.cos(angle) * speed, dy=math.sin(angle) * speed,
                spin=rng.uniform(-9.0, 9.0), turn=rng.uniform(0.0, math.tau), color=rng.choice(colors)
            ))

        self._origin = origin
        self._started = time.monotonic()
        parent = self.parentWidget()
        if parent is not None:
            self.setGeometry(parent.rect())

        self.raise_()
        self.show()
        self._timer.start()

    def is_celebrating(self) -> bool:
        """Return whether confetti is still in the air."""
        return bool(self._confetti) and time.monotonic() - self._started < _CONFETTI_SECONDS

    def _tick(self) -> None:
        """Move the confetti, and put the layer away once it has all fallen."""
        if not self.is_celebrating():
            self._confetti = []
            self._timer.stop()
            self.hide()
            return

        self.update()

    def paintEvent(self, event: QPaintEvent) -> None:
        """Draw confetti falling from the burst."""
        super().paintEvent(event)
        elapsed = time.monotonic() - self._started
        if not self._confetti or elapsed >= _CONFETTI_SECONDS:
            return

        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        reach = _scaled(_CONFETTI_REACH)
        piece_size = _scaled(_CONFETTI_PIECE)
        fade = 1.0 - max(0.0, elapsed - _CONFETTI_SECONDS * 0.6) / (_CONFETTI_SECONDS * 0.4)
        painter.setOpacity(max(0.0, fade))
        painter.setPen(Qt.PenStyle.NoPen)
        for piece in self._confetti:
            x = self._origin.x() + piece.dx * elapsed * reach
            y = self._origin.y() + (piece.dy * elapsed + 0.9 * elapsed * elapsed) * reach
            turn = piece.turn + piece.spin * elapsed
            painter.save()
            painter.translate(x, y)
            painter.rotate(math.degrees(turn))
            # Squashing one side as the piece tumbles makes it read as a flat card turning in space.
            width = piece_size * abs(math.cos(piece.turn + piece.spin * elapsed * 0.7))
            painter.setBrush(piece.color)
            painter.drawRect(QRectF(-width / 2, -piece_size * 0.35, max(1.0, width), piece_size * 0.7))
            painter.restore()

        painter.end()


# The current stage's bead is a little larger than the others, and its glow reaches this many radii.
_CURRENT_SCALE = 1.12
_GLOW_REACH = 2.4


class MissionJourney(QWidget):
    """
    The stages a mission passes through, from the user's request to the AI's report.

    Each stage is a glossy bead on a track.  Stages already passed are filled,
    the current one glows inside a tilted orbit that a small moon travels
    round, passing behind the bead and then in front of it, and the track's
    fill glides forward when the mission moves on.  While the team works, the
    track beyond the current stage fills with the share of the plan done.
    """

    def __init__(self, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._labels: list[str] = []
        self._stage = 0
        self._fraction = 0.0
        self._shown_stage = 0.0
        self._tone = MissionTone.ACCENT
        self._live = False
        self._start = time.monotonic()
        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
        self._timer = QTimer(self)
        self._timer.setInterval(_FRAME_MS)
        self._timer.timeout.connect(self._tick)

    def set_labels(self, labels: list[str]) -> None:
        """Name the stages."""
        if labels != self._labels:
            self._labels = labels
            self.updateGeometry()
            self.update()

    def set_stage(self, stage: int, tone: MissionTone, live: bool, fraction: float = 0.0) -> None:
        """
        Show how far the mission has got.

        Args:
            stage: Index of the current stage
            tone: Colour of the current stage
            live: Whether the AI is working on it now, which animates it
            fraction: How far towards the next stage the mission is, between 0.0 and 1.0
        """
        fraction = max(0.0, min(1.0, fraction))
        if (stage, tone, live, fraction) == (self._stage, self._tone, self._live, self._fraction):
            return

        self._stage = stage
        self._fraction = fraction
        self._tone = tone
        self._live = live
        self._update_timer()
        self.update()

    def stage(self) -> int:
        """Return the index of the current stage."""
        return self._stage

    def _fill_target(self) -> float:
        """Return where the track's fill is heading, in stages."""
        return self._stage + self._fraction

    def _moving(self) -> bool:
        """Return whether the fill is still gliding to where the mission is."""
        return abs(self._shown_stage - self._fill_target()) > 0.01

    def _update_timer(self) -> None:
        """Run the animation only while something moves."""
        if self.isVisible() and (self._live or self._moving()):
            if not self._timer.isActive():
                self._timer.start()

        else:
            self._timer.stop()

    def _tick(self) -> None:
        """Advance the glide and the glow."""
        if self._moving():
            self._shown_stage += (self._fill_target() - self._shown_stage) * 0.12

        else:
            self._shown_stage = self._fill_target()

        self._update_timer()
        self.update()

    def showEvent(self, event: QShowEvent) -> None:
        """Animate while shown."""
        super().showEvent(event)
        self._update_timer()

    def hideEvent(self, event: QHideEvent) -> None:
        """Rest while hidden."""
        super().hideEvent(event)
        self._timer.stop()

    def _label_font(self) -> QFont:
        """Return the font for the stage names."""
        font = QFont(self.font())
        font.setPixelSize(max(8, round(_scaled(12))))
        return font

    @staticmethod
    def _bead() -> float:
        """Return the radius of a stage's bead."""
        return _scaled(11)

    def _track_y(self) -> float:
        """Return the height of the track, leaving room above it for the current stage's glow."""
        return self._bead() * _CURRENT_SCALE * _GLOW_REACH + _scaled(2)

    def sizeHint(self) -> QSize:
        metrics = QFontMetrics(self._label_font())
        height = self._track_y() + self._bead() * 1.5 + metrics.height() + _scaled(8)
        return QSize(round(_scaled(320)), round(height))

    def minimumSizeHint(self) -> QSize:
        return QSize(round(_scaled(160)), self.sizeHint().height())

    def paintEvent(self, event: QPaintEvent) -> None:
        """Draw the track, the beads and the stage names."""
        super().paintEvent(event)
        count = len(self._labels)
        if count < 2:
            return

        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        style = StyleManager()
        phase = time.monotonic() - self._start
        bead = self._bead()
        margin = max(bead * _CURRENT_SCALE * _GLOW_REACH, self.width() / count / 2)
        y = self._track_y()
        span = (self.width() - margin * 2) / (count - 1)
        xs = [margin + span * index for index in range(count)]
        done_color = tone_color(MissionTone.SUCCESS)
        current = tone_color(self._tone)
        inactive = QColor(style.get_color(ColorRole.TEXT_INACTIVE))

        track = QColor(inactive)
        track.setAlphaF(0.25)
        painter.setPen(Qt.PenStyle.NoPen)
        painter.setBrush(track)
        thickness = _scaled(4)
        painter.drawRoundedRect(QRectF(xs[0], y - thickness / 2, xs[-1] - xs[0], thickness), thickness / 2, thickness / 2)

        fill_to = xs[0] + span * max(0.0, min(count - 1.0, self._shown_stage))
        if fill_to > xs[0]:
            gradient = QLinearGradient(xs[0], 0, fill_to, 0)
            gradient.setColorAt(0.0, done_color)
            gradient.setColorAt(1.0, current)
            painter.setBrush(QBrush(gradient))
            painter.drawRoundedRect(QRectF(xs[0], y - thickness / 2, fill_to - xs[0], thickness), thickness / 2, thickness / 2)

            if self._live:
                # A spark travels along the filled track, so the work reads as flowing forward.
                spark_x = xs[0] + (fill_to - xs[0]) * ((phase * 0.5) % 1.0)
                spark = QRadialGradient(QPointF(spark_x, y), thickness * 2.5)
                spark.setColorAt(0.0, QColor(255, 255, 255, 200))
                spark.setColorAt(1.0, QColor(255, 255, 255, 0))
                painter.setBrush(QBrush(spark))
                painter.drawEllipse(QPointF(spark_x, y), thickness * 2.5, thickness * 2.5)

        font = self._label_font()
        painter.setFont(font)
        metrics = QFontMetrics(font)
        text_color = QColor(style.get_color(ColorRole.TEXT_PRIMARY))
        for index, x in enumerate(xs):
            center = QPointF(x, y)
            if index < self._stage:
                color, glyph = done_color, "✓"

            elif index == self._stage:
                color, glyph = current, "✓" if index == count - 1 and self._tone == MissionTone.SUCCESS else str(index + 1)

            else:
                color, glyph = inactive, str(index + 1)

            radius = bead * (_CURRENT_SCALE if index == self._stage else 1.0)
            if index == self._stage:
                self._paint_current_glow(painter, center, radius, color, phase)
                self._paint_orbit(painter, center, radius, color, phase, front=False)

            if index > self._stage:
                faded = QColor(color)
                faded.setAlphaF(0.35)
                painter.setPen(QPen(faded, _scaled(1.5)))
                painter.setBrush(surface_colors()[1])
                painter.drawEllipse(center, radius, radius)

            else:
                _paint_sphere(painter, center, radius, color)

            if index == self._stage:
                self._paint_orbit(painter, center, radius, color, phase, front=True)

            glyph_font = QFont(font)
            glyph_font.setPixelSize(max(6, round(radius * 1.0)))
            glyph_font.setBold(True)
            painter.setFont(glyph_font)
            painter.setPen(QColor("#ffffff") if index <= self._stage else inactive)
            painter.drawText(QRectF(center.x() - radius, center.y() - radius, radius * 2, radius * 2),
                             int(Qt.AlignmentFlag.AlignCenter), glyph)

            painter.setFont(font)
            label = metrics.elidedText(self._labels[index], Qt.TextElideMode.ElideRight, round(span * 0.96))
            label_color = QColor(text_color)
            label_color.setAlphaF(1.0 if index <= self._stage else 0.55)
            painter.setPen(label_color)
            font.setBold(index == self._stage)
            painter.setFont(font)
            painter.drawText(
                QRectF(x - span / 2, y + bead * 1.5, span, metrics.height() + _scaled(4)),
                int(Qt.AlignmentFlag.AlignHCenter | Qt.AlignmentFlag.AlignTop), label
            )
            font.setBold(False)

        painter.end()

    def _paint_current_glow(
        self, painter: QPainter, center: QPointF, radius: float, color: QColor, phase: float
    ) -> None:
        """Draw a soft glow round the current stage and, while the AI works on it, a ripple spreading out."""
        reach = radius * _GLOW_REACH
        glow = QRadialGradient(center, reach)
        halo = QColor(color)
        halo.setAlphaF(0.28 + (0.1 * math.sin(phase * 3.0) if self._live else 0.0))
        glow.setColorAt(0.0, halo)
        clear = QColor(color)
        clear.setAlphaF(0.0)
        glow.setColorAt(1.0, clear)
        painter.setPen(Qt.PenStyle.NoPen)
        painter.setBrush(QBrush(glow))
        painter.drawEllipse(center, reach, reach)
        if not self._live:
            return

        wave = (phase * 0.6) % 1.0
        ripple = QColor(color)
        ripple.setAlphaF(0.45 * (1.0 - wave))
        painter.setPen(QPen(ripple, _scaled(1.2)))
        painter.setBrush(Qt.BrushStyle.NoBrush)
        size = radius * (1.15 + (_GLOW_REACH - 1.25) * wave)
        painter.drawEllipse(center, size, size)

    def _paint_orbit(
        self, painter: QPainter, center: QPointF, radius: float, color: QColor, phase: float, front: bool
    ) -> None:
        """
        Draw half of the current stage's tilted orbit and its moon, while the AI works on it.

        The far half is drawn before the bead and the near half after it, so the
        orbit and its moon pass behind the bead and then in front of it.
        """
        if not self._live:
            return

        width = radius * 1.85
        height = radius * 0.45
        painter.save()
        painter.translate(center)
        painter.rotate(-14)
        ring = QColor(color)
        ring.setAlphaF(0.9 if front else 0.4)
        painter.setPen(QPen(ring, _scaled(1.5)))
        painter.setBrush(Qt.BrushStyle.NoBrush)
        box = QRectF(-width, -height, width * 2, height * 2)
        # Qt measures arcs anticlockwise from three o'clock in sixteenths of a degree; the top half is the far side.
        painter.drawArc(box, 0 if not front else 180 * 16, 180 * 16)

        angle = phase * 2.2
        moon = QPointF(width * math.cos(angle), height * math.sin(angle))
        if (moon.y() > 0) == front:
            moon_radius = _scaled(2.6) * (1.0 if front else 0.8)
            moon_color = QColor(color).lighter(150) if front else QColor(color)
            _paint_sphere(painter, moon, moon_radius, moon_color)

        painter.restore()
