"""Animated scenes for Mission Control: the confetti and the journey bar."""

from dataclasses import dataclass
import math
import random
import time

from PySide6.QtCore import QEvent, QObject, QPointF, QRectF, QSize, Qt, QTimer
from PySide6.QtGui import (
    QBrush, QColor, QFont, QFontMetrics, QHideEvent, QLinearGradient, QPainter, QPaintEvent, QShowEvent
)
from PySide6.QtWidgets import QSizePolicy, QWidget

from desktop.color_role import ColorRole
from desktop.mission_tab.mission_display import MissionTone
from desktop.mission_tab.mission_motion import MissionMotion
from desktop.mission_tab.mission_painted import readable, surface_colors, tone_color
from desktop.style_manager import StyleManager


_FRAME_MS = 33

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
        parent.installEventFilter(self)
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
        if MissionMotion().reduced():
            return

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
        self._cover_parent()
        self.raise_()
        self.show()
        self._timer.start()

    def _cover_parent(self) -> None:
        """Take the whole of the parent, so confetti can fly across the page rather than be clipped."""
        parent = self.parentWidget()
        if parent is not None:
            self.setGeometry(parent.rect())

    def eventFilter(self, watched: QObject, event: QEvent) -> bool:
        """Follow the parent when the page is resized while the confetti is in the air."""
        if watched is self.parentWidget() and event.type() == QEvent.Type.Resize:
            self._cover_parent()

        return super().eventFilter(watched, event)

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


# The bars' thickness and the gap between them, in unzoomed pixels.
_BAR_HEIGHT = 5.0
_BAR_GAP = 6.0
_BAR_TOP = 2.0


class MissionJourney(QWidget):
    """
    The stages a mission passes through, from the user's request to the AI's report.

    Each stage is a slim segment with its name beneath.  Stages already passed are
    filled, the current one fills with the share of its work done, and while
    the AI works on it a sheen travels along it.  The fill glides forward when
    the mission moves on.
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
        """Return where the fill is heading, in stages."""
        return self._stage + self._fraction

    def _moving(self) -> bool:
        """Return whether the fill is still gliding to where the mission is."""
        return abs(self._shown_stage - self._fill_target()) > 0.01

    def apply_motion(self) -> None:
        """Animate or rest, as the user's choice to reduce motion says."""
        self._update_timer()
        self.update()

    def _update_timer(self) -> None:
        """Run the animation only while something moves, and never while motion is reduced."""
        if MissionMotion().reduced():
            # The fill goes straight to where the mission is, and the sheen stands still.
            self._shown_stage = self._fill_target()
            self._timer.stop()
            return

        if self.isVisible() and (self._live or self._moving()):
            if not self._timer.isActive():
                self._timer.start()

        else:
            self._timer.stop()

    def _tick(self) -> None:
        """Advance the glide and the sheen."""
        if self._moving():
            self._shown_stage += (self._fill_target() - self._shown_stage) * 0.12

        else:
            self._shown_stage = self._fill_target()

        self._update_timer()
        # Only the bars move; the names beneath them stay as they are.
        self.update(QRectF(0, 0, self.width(), _scaled(_BAR_TOP + _BAR_HEIGHT) + 2).toAlignedRect())

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

    def sizeHint(self) -> QSize:
        metrics = QFontMetrics(self._label_font())
        height = _scaled(_BAR_TOP + _BAR_HEIGHT + 7) + metrics.height() + _scaled(2)
        return QSize(round(_scaled(320)), round(height))

    def minimumSizeHint(self) -> QSize:
        return QSize(round(_scaled(160)), self.sizeHint().height())

    def paintEvent(self, event: QPaintEvent) -> None:
        """Draw a segment for each stage, filled as far as the mission has got, with the stage names beneath."""
        super().paintEvent(event)
        count = len(self._labels)
        if count < 2:
            return

        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        style = StyleManager()
        phase = 0.0 if MissionMotion().reduced() else time.monotonic() - self._start
        gap = _scaled(_BAR_GAP)
        width = (self.width() - gap * (count - 1)) / count
        top = _scaled(_BAR_TOP)
        height = _scaled(_BAR_HEIGHT)
        done_color = tone_color(MissionTone.SUCCESS)
        current = tone_color(self._tone)
        inactive = QColor(style.get_color(ColorRole.TEXT_INACTIVE))
        track = QColor(inactive)
        track.setAlphaF(0.22)
        _page, card = surface_colors()
        font = self._label_font()
        metrics = QFontMetrics(font)
        finished = self._stage == count - 1 and self._tone == MissionTone.SUCCESS
        # In a right-to-left language the journey starts at the right and fills towards the left.
        rtl = self.isRightToLeft()

        for index, label in enumerate(self._labels):
            x = self.width() - width - index * (width + gap) if rtl else index * (width + gap)
            segment = QRectF(x, top, width, height)
            painter.setPen(Qt.PenStyle.NoPen)
            painter.setBrush(track)
            painter.drawRoundedRect(segment, height / 2, height / 2)

            # A passed stage is full, the current one is filled with the share of its work done.
            filled = 1.0 if index < self._stage or finished else max(0.0, min(1.0, self._shown_stage - index))
            color = done_color if index < self._stage or finished else current
            if index == self._stage and not finished:
                tint = QColor(current)
                tint.setAlphaF(0.25)
                painter.setBrush(tint)
                painter.drawRoundedRect(segment, height / 2, height / 2)

            if filled > 0.0:
                painter.setBrush(color)
                fill = max(height, width * filled)
                painter.drawRoundedRect(QRectF(x + width - fill if rtl else x, top, fill, height), height / 2, height / 2)

            if index == self._stage and self._live and not finished:
                # A sheen travels along the current stage, so the work reads as moving forward.
                travelled = (phase * 0.45) % 1.0
                sweep = x + width * (1.0 - travelled if rtl else travelled)
                sheen = QLinearGradient(sweep - width * 0.25, 0, sweep + width * 0.25, 0)
                clear = QColor(card)
                clear.setAlphaF(0.0)
                bright = QColor(card)
                bright.setAlphaF(0.7)
                sheen.setColorAt(0.0, clear)
                sheen.setColorAt(0.5, bright)
                sheen.setColorAt(1.0, clear)
                painter.save()
                painter.setClipRect(segment)
                painter.setBrush(QBrush(sheen))
                painter.drawRoundedRect(segment, height / 2, height / 2)
                painter.restore()

            label_font = QFont(font)
            label_font.setBold(index <= self._stage)
            painter.setFont(label_font)
            if index < self._stage or finished:
                label_color = readable(done_color, card)

            elif index == self._stage:
                label_color = readable(current, card)

            else:
                label_color = readable(inactive, card)

            painter.setPen(label_color)
            text = QFontMetrics(label_font).elidedText(label, Qt.TextElideMode.ElideRight, round(width))
            painter.drawText(
                QRectF(x, top + height + _scaled(7), width, metrics.height() + _scaled(2)),
                int(Qt.AlignmentFlag.AlignLeft | Qt.AlignmentFlag.AlignTop), text
            )

        painter.end()
