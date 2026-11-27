"""The picture at the top of the new-mission screen: Humbug's logo arriving in a sweep of light."""

import math
import time

from PySide6.QtCore import QPointF, QRectF, QSize, Qt, QTimer
from PySide6.QtGui import (
    QBrush, QColor, QHideEvent, QImage, QLinearGradient, QPainter, QPainterPath, QPaintEvent, QPen,
    QIcon, QPixmap, QRadialGradient, QResizeEvent, QTransform
)
from PySide6.QtWidgets import QSizePolicy, QToolButton, QWidget

from desktop.mission_tab.mission_motion import MissionMotion
from desktop.mission_tab.mission_painted import surface_colors
from desktop.style_manager import StyleManager


_FRAME_MS = 16

# The story, in seconds: two streams of light fly in and meet, the logo turns into view out of the
# flash, rings of light circle it, a sheen crosses it, and everything settles.
_STREAM_SECONDS = 0.9
_ARRIVE_AT = 0.75
_TURN_SECONDS = 1.1
_SHEEN_AT = _ARRIVE_AT + _TURN_SECONDS * 0.8
_SHEEN_SECONDS = 0.8
_DURATION = _SHEEN_AT + _SHEEN_SECONDS + 0.3

# The logo's box as a share of the picture's height, and where in that box the drawn letter ends.
_LOGO_BOX = 0.86
_LETTER_FOOT = 0.34

# The colours of the two streams: the logo's own cyan and violet.
_STREAM_LEFT = QColor("#35d8ff")
_STREAM_RIGHT = QColor("#c95cff")

# Rings of light round the logo: each one's tilt in degrees, how flat it is, how fast its satellite
# goes round in turns a second, and where the satellite starts.
_ORBITS = ((-16.0, 0.3, 0.55, 0.0), (20.0, 0.24, -0.4, 2.2), (3.0, 0.36, 0.3, 4.1))

# Where the sparkles sit, as fractions of the logo's box from its centre, with each one's rhythm.
_SPARKLES = (
    (-0.52, -0.3, 0.0), (0.5, -0.36, 1.3), (0.6, 0.08, 2.1), (-0.62, 0.14, 3.4), (0.26, -0.47, 4.0), (-0.3, 0.4, 5.2)
)


def _ease_out(value: float) -> float:
    """Return a cubic ease-out of a value, held between 0 and 1."""
    value = max(0.0, min(1.0, value))
    return 1 - (1 - value) ** 3


def _ease_back(value: float) -> float:
    """Return an ease-out that overshoots a little before settling, of a value held between 0 and 1."""
    value = max(0.0, min(1.0, value)) - 1
    return 1 + 2.2 * value ** 3 + 1.2 * value ** 2


def _smooth(value: float) -> float:
    """Return a smooth step of a value, held between 0 and 1."""
    value = max(0.0, min(1.0, value))
    return value * value * (3 - 2 * value)


def _white(alpha: float) -> QColor:
    """Return white at an opacity between 0 and 1."""
    return QColor(255, 255, 255, round(255 * max(0.0, min(1.0, alpha))))


def _replay_icon(size: int) -> QIcon:
    """Return a white circular arrow, the usual sign for playing something again."""
    ratio = 2
    pixmap = QPixmap(size * ratio, size * ratio)
    pixmap.fill(Qt.GlobalColor.transparent)
    pixmap.setDevicePixelRatio(ratio)
    painter = QPainter(pixmap)
    painter.setRenderHint(QPainter.RenderHint.Antialiasing)
    unit = size / 24
    pen = QPen(QColor("#ffffff"), 2.2 * unit)
    pen.setCapStyle(Qt.PenCapStyle.RoundCap)
    pen.setJoinStyle(Qt.PenJoinStyle.RoundJoin)
    painter.setPen(pen)
    painter.drawArc(QRectF(5 * unit, 5 * unit, 14 * unit, 14 * unit), 100 * 16, 290 * 16)
    # The arrowhead, at the top of the circle, pointing on round it.
    painter.drawPolyline([QPointF(8.4 * unit, 2.6 * unit), QPointF(11.4 * unit, 5.2 * unit), QPointF(8.4 * unit, 8 * unit)])
    painter.end()
    return QIcon(pixmap)


def _faded(color: QColor, alpha: float) -> QColor:
    """Return a colour at an opacity between 0 and 1."""
    faded = QColor(color)
    faded.setAlphaF(max(0.0, min(1.0, alpha)))
    return faded


class MissionIntro(QWidget):
    """
    Humbug's logo arriving on stage: streams of light meet, and the logo turns into view out of the flash.

    Rings of light circle the logo, passing behind it and in front of it, a sheen crosses it, and
    it comes to rest over a pedestal of light with its reflection on the floor.  The story plays
    the first time the picture is shown, and again each time the new-mission screen is opened on
    purpose; otherwise, and whenever motion is reduced, the picture rests on the logo.  It only
    animates while it is playing, so it costs nothing once it has settled.
    """

    def __init__(self, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._started: float | None = None
        self._played = False
        self._source = QImage(StyleManager().get_app_logo_path())
        self._logo = QImage()
        self._reflection = QImage()
        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
        self.setAttribute(Qt.WidgetAttribute.WA_TranslucentBackground)
        self._timer = QTimer(self)
        self._timer.setInterval(_FRAME_MS)
        self._timer.timeout.connect(self._tick)

        # The story can be watched again whenever it is not already playing.
        self._replay = QToolButton(self)
        self._replay.setObjectName("MissionIntroReplay")
        self._replay.setCursor(Qt.CursorShape.PointingHandCursor)
        self._replay.clicked.connect(self.replay)

    def set_replay_text(self, text: str) -> None:
        """Set what the button that plays the story again says to a tooltip and to assistive technology."""
        self._replay.setToolTip(text)
        self._replay.setAccessibleName(text)

    def play_once(self) -> None:
        """Play the story, unless it has been played already; with motion reduced, show how it ends."""
        if self._played:
            return

        self._played = True
        if MissionMotion().reduced():
            self.update()
            return

        self._play()

    def play(self) -> None:
        """Play the story from the start, as when the new-mission screen is opened; with motion reduced, show its end."""
        self._played = True
        if MissionMotion().reduced():
            self.update()
            return

        self._play()

    def replay(self) -> None:
        """
        Play the story again, from the start.

        The user asked to see it, so it plays even with motion reduced.
        """
        self._played = True
        self._play()

    def _play(self) -> None:
        """Start the story from the beginning."""
        self._started = time.monotonic()
        self._timer.start()
        self._replay.hide()
        self.update()

    def is_playing(self) -> bool:
        """Return True while the story is still being told."""
        return self._timer.isActive()

    def sizeHint(self) -> QSize:
        return QSize(round(480 * StyleManager().zoom_factor()), round(260 * StyleManager().zoom_factor()))

    def minimumSizeHint(self) -> QSize:
        return QSize(round(240 * StyleManager().zoom_factor()), self.sizeHint().height())

    def resizeEvent(self, event: QResizeEvent) -> None:
        """Forget the logo drawn for the old size, and keep the replay button in the corner where lines end."""
        super().resizeEvent(event)
        self._logo = QImage()
        zoom = StyleManager().zoom_factor()
        side = round(30 * zoom)
        inset = round(12 * zoom)
        self._replay.setIcon(_replay_icon(round(16 * zoom)))
        self._replay.setIconSize(QSize(round(16 * zoom), round(16 * zoom)))
        self._replay.setFixedSize(side, side)
        x = inset if self.isRightToLeft() else self.width() - side - inset
        self._replay.move(x, inset)

    def hideEvent(self, event: QHideEvent) -> None:
        """Finish at once if hidden while playing, so it is never found half way."""
        super().hideEvent(event)
        if self._timer.isActive():
            self._timer.stop()
            self._started = None
            self._replay.show()

    def _tick(self) -> None:
        """Advance the animation, stopping once it has settled."""
        if self._elapsed() >= _DURATION:
            self._timer.stop()
            self._started = None
            self._replay.show()

        self.update()

    def _elapsed(self) -> float:
        """Return how far into the story the picture is; a picture not playing is at its end."""
        return _DURATION if self._started is None else time.monotonic() - self._started

    def _prepare_logo(self, box: float) -> None:
        """Scale the logo to its box, and make the faded mirror image that lies on the floor under it."""
        ratio = self.devicePixelRatioF()
        side = max(1, round(box * ratio))
        if self._source.isNull() or (not self._logo.isNull() and self._logo.width() == side):
            return

        self._logo = self._source.scaled(
            side, side, Qt.AspectRatioMode.KeepAspectRatio, Qt.TransformationMode.SmoothTransformation
        ).convertToFormat(QImage.Format.Format_ARGB32_Premultiplied)
        self._logo.setDevicePixelRatio(ratio)

        # The reflection is the logo upside down, strongest where it meets the floor and soon gone.
        mirrored = self._logo.mirrored(False, True)
        mirrored.setDevicePixelRatio(1.0)
        fade = QLinearGradient(0, 0, 0, mirrored.height())
        foot = 0.5 - _LETTER_FOOT
        fade.setColorAt(0.0, _white(0.0))
        fade.setColorAt(max(0.0, foot - 0.01), _white(0.0))
        fade.setColorAt(foot, _white(0.34))
        fade.setColorAt(min(1.0, foot + 0.3), _white(0.0))
        fade.setColorAt(1.0, _white(0.0))
        painter = QPainter(mirrored)
        painter.setCompositionMode(QPainter.CompositionMode.CompositionMode_DestinationIn)
        painter.fillRect(mirrored.rect(), QBrush(fade))
        painter.end()
        mirrored.setDevicePixelRatio(ratio)
        self._reflection = mirrored

    def paintEvent(self, event: QPaintEvent) -> None:
        """Draw the backdrop and whichever parts of the story are on stage."""
        super().paintEvent(event)
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        painter.setRenderHint(QPainter.RenderHint.SmoothPixmapTransform)
        elapsed = self._elapsed()
        rect = QRectF(self.rect())
        radius = 16 * StyleManager().zoom_factor()
        clip = QPainterPath()
        clip.addRoundedRect(rect, radius, radius)
        painter.setClipPath(clip)
        self._paint_backdrop(painter, rect)

        box = rect.height() * _LOGO_BOX
        center = QPointF(rect.center().x(), rect.top() + rect.height() * 0.44)
        self._prepare_logo(box)
        arrive = (elapsed - _ARRIVE_AT) / _TURN_SECONDS
        # The rings and sparkles belong to the logo's arrival: they come with it and are gone by the end.
        energy = _smooth(arrive * 3.0) * (1.0 - _smooth((elapsed - _SHEEN_AT) / (_DURATION - _SHEEN_AT)))

        if elapsed < _STREAM_SECONDS + 0.4:
            self._paint_streams(painter, rect, center, elapsed)

        if arrive > 0.0:
            self._paint_pedestal(painter, center, box, _smooth(arrive * 2.0))
            self._paint_flash(painter, center, box, arrive)
            self._paint_orbits(painter, center, box, elapsed, energy, front=False)
            self._paint_logo(painter, center, box, arrive, elapsed)
            self._paint_orbits(painter, center, box, elapsed, energy, front=True)
            self._paint_sparkles(painter, center, box, elapsed, energy)

        painter.end()

    def _paint_backdrop(self, painter: QPainter, rect: QRectF) -> None:
        """Draw the soft blue-to-violet backdrop with a lit floor, darker in a dark theme."""
        _page, card = surface_colors()
        dark = card.lightnessF() < 0.4
        sky = QLinearGradient(rect.topLeft(), rect.bottomRight())
        sky.setColorAt(0.0, QColor("#16306b") if dark else QColor("#8fb6ff"))
        sky.setColorAt(0.55, QColor("#2c2f86") if dark else QColor("#a9a8ff"))
        sky.setColorAt(1.0, QColor("#42287f") if dark else QColor("#c7a6ff"))
        painter.fillRect(rect, QBrush(sky))

        # A haze of light in the middle, where everything happens.
        haze = QRadialGradient(rect.center(), rect.width() * 0.4)
        haze.setColorAt(0.0, _white(0.22 if dark else 0.45))
        haze.setColorAt(1.0, _white(0.0))
        painter.fillRect(rect, QBrush(haze))

        # The floor: a horizon line with light pooled on it under the middle of the stage.
        horizon = rect.top() + rect.height() * 0.8
        floor = QLinearGradient(0, horizon, 0, rect.bottom())
        floor.setColorAt(0.0, _white(0.24 if dark else 0.41))
        floor.setColorAt(0.12, _white(0.08))
        floor.setColorAt(1.0, QColor(90, 80, 200, 40 if dark else 28))
        painter.fillRect(QRectF(rect.left(), horizon, rect.width(), rect.bottom() - horizon), QBrush(floor))

    def _stream_point(self, rect: QRectF, center: QPointF, side: float, along: float) -> QPointF:
        """Return a point on a stream's curved way in from one side (-1 left, 1 right) to the centre."""
        start = QPointF(center.x() + side * rect.width() * 0.56, center.y() + side * rect.height() * 0.3)
        bend = QPointF(center.x() + side * rect.width() * 0.2, center.y() - side * rect.height() * 0.75)
        inverse = 1 - along
        return start * (inverse * inverse) + bend * (2 * inverse * along) + center * (along * along)

    def _paint_streams(self, painter: QPainter, rect: QRectF, center: QPointF, elapsed: float) -> None:
        """Draw the two streams of light flying in, each a bright head with a tail thinning out behind it."""
        head = _ease_out(elapsed / _STREAM_SECONDS)
        fade = 1.0 - _smooth((elapsed - _STREAM_SECONDS) / 0.4)
        thick = rect.height() * 0.05
        steps = 26
        for side, color in ((-1.0, _STREAM_LEFT), (1.0, _STREAM_RIGHT)):
            previous = self._stream_point(rect, center, side, max(0.0, head - 0.5))
            for step in range(1, steps + 1):
                share = step / steps
                point = self._stream_point(rect, center, side, max(0.0, head - 0.5 * (1 - share)))
                for width, alpha in ((thick * 2.6, 0.22), (thick, 0.9)):
                    pen = QPen(_faded(color, alpha * share * fade), width * (0.25 + 0.75 * share))
                    pen.setCapStyle(Qt.PenCapStyle.RoundCap)
                    painter.setPen(pen)
                    painter.drawLine(previous, point)

                previous = point

            glow = QRadialGradient(previous, thick * 3.2)
            glow.setColorAt(0.0, _white(0.95 * fade))
            glow.setColorAt(0.3, _faded(color, 0.7 * fade))
            glow.setColorAt(1.0, _faded(color, 0.0))
            painter.setPen(Qt.PenStyle.NoPen)
            painter.setBrush(QBrush(glow))
            painter.drawEllipse(previous, thick * 3.2, thick * 3.2)

    def _paint_pedestal(self, painter: QPainter, center: QPointF, box: float, strength: float) -> None:
        """Draw discs of light on the floor under the logo, with a rim."""
        floor = QPointF(center.x(), center.y() + box * (_LETTER_FOOT + 0.035))
        painter.setPen(Qt.PenStyle.NoPen)
        for scale, alpha in ((1.0, 0.2), (0.7, 0.26), (0.42, 0.4)):
            painter.setBrush(_white(alpha * strength))
            painter.drawEllipse(floor, box * 0.46 * scale, box * 0.07 * scale)

        painter.setBrush(Qt.BrushStyle.NoBrush)
        painter.setPen(QPen(_white(0.5 * strength), max(1.0, box * 0.006)))
        painter.drawEllipse(floor, box * 0.46, box * 0.07)

    def _paint_flash(self, painter: QPainter, center: QPointF, box: float, arrive: float) -> None:
        """Draw the burst of light the logo arrives out of, and the glow that stays behind it."""
        burst = 1.0 - _smooth(arrive * 2.2)
        reach = box * (0.35 + 0.9 * _ease_out(arrive * 2.2))
        if burst > 0.0:
            flash = QRadialGradient(center, reach)
            flash.setColorAt(0.0, _white(0.95 * burst))
            flash.setColorAt(0.35, QColor(190, 215, 255, round(170 * burst)))
            flash.setColorAt(1.0, QColor(190, 170, 255, 0))
            painter.setPen(Qt.PenStyle.NoPen)
            painter.setBrush(QBrush(flash))
            painter.drawEllipse(center, reach, reach)

        aura = QRadialGradient(center, box * 0.62)
        aura.setColorAt(0.0, _white(0.55 * _smooth(arrive * 2.0)))
        aura.setColorAt(0.55, QColor(205, 195, 255, round(55 * _smooth(arrive * 2.0))))
        aura.setColorAt(1.0, QColor(205, 195, 255, 0))
        painter.setPen(Qt.PenStyle.NoPen)
        painter.setBrush(QBrush(aura))
        painter.drawEllipse(center, box * 0.62, box * 0.62)

    def _paint_logo(self, painter: QPainter, center: QPointF, box: float, arrive: float, elapsed: float) -> None:
        """Draw the logo turning to face the viewer as it grows into place, with its reflection and its sheen."""
        if self._logo.isNull():
            return

        place = QRectF(-box / 2, -box / 2, box, box)
        settled = _smooth((arrive - 0.55) / 0.45)
        if settled > 0.0:
            # The mirror image hangs from the floor line, just under the letter's foot.
            painter.setOpacity(settled)
            mirror = center.y() + box * (_LETTER_FOOT + 0.02)
            painter.drawImage(QRectF(center.x() - box / 2, 2 * mirror - (center.y() + box / 2), box, box), self._reflection)

        logo = self._logo
        sheen = (elapsed - _SHEEN_AT) / _SHEEN_SECONDS
        if 0.0 < sheen < 1.0:
            # A band of light crosses the logo, drawn only where the logo itself is.
            logo = QImage(self._logo)
            width = logo.width()
            sweep = -0.4 * width + 1.8 * width * sheen
            band = QLinearGradient(sweep - width * 0.22, 0, sweep + width * 0.22, width * 0.25)
            band.setColorAt(0.0, _white(0.0))
            band.setColorAt(0.5, _white(0.75))
            band.setColorAt(1.0, _white(0.0))
            shine = QPainter(logo)
            shine.setCompositionMode(QPainter.CompositionMode.CompositionMode_SourceAtop)
            shine.fillRect(QRectF(0, 0, width, logo.height()), QBrush(band))
            shine.end()

        painter.save()
        painter.setOpacity(min(1.0, arrive * 5.0))
        painter.translate(center)
        turn = QTransform()
        turn.rotate(105.0 * (1.0 - _ease_out(arrive)), Qt.Axis.YAxis)
        painter.setTransform(turn, True)
        grow = 0.5 + 0.5 * _ease_back(arrive)
        painter.scale(grow, grow)
        painter.drawImage(place, logo)
        painter.restore()
        painter.setOpacity(1.0)

    def _paint_orbits(
        self, painter: QPainter, center: QPointF, box: float, elapsed: float, strength: float, front: bool
    ) -> None:
        """
        Draw the half of each ring of light that is behind the logo, or the half in front of it.

        Drawing the far halves before the logo and the near halves after it is what makes the
        rings, and the satellite on each, pass round the logo rather than lie flat on it.
        """
        if strength <= 0.0:
            return

        reach = box * 0.56
        for index, (tilt, squash, speed, offset) in enumerate(_ORBITS):
            painter.save()
            painter.translate(center)
            painter.rotate(tilt)
            size = reach * (1.0 + index * 0.13)
            ring = QRectF(-size, -size * squash, size * 2, size * squash * 2)
            painter.setBrush(Qt.BrushStyle.NoBrush)
            painter.setPen(QPen(_white((0.8 if front else 0.4) * strength), max(1.0, box * 0.007)))
            # Qt measures arcs anticlockwise from three o'clock in sixteenths of a degree; the top half is the far side.
            painter.drawArc(ring, 180 * 16 if front else 0, 180 * 16)

            angle = offset + elapsed * speed * math.tau
            satellite = QPointF(size * math.cos(angle), size * squash * math.sin(angle))
            if (satellite.y() > 0) == front:
                dot = box * (0.02 if front else 0.014)
                glow = QRadialGradient(satellite, dot * 3)
                glow.setColorAt(0.0, _white(strength))
                glow.setColorAt(0.35, _faded(_STREAM_LEFT if index % 2 == 0 else _STREAM_RIGHT, 0.7 * strength))
                glow.setColorAt(1.0, _white(0.0))
                painter.setPen(Qt.PenStyle.NoPen)
                painter.setBrush(QBrush(glow))
                painter.drawEllipse(satellite, dot * 3, dot * 3)

            painter.restore()

    def _paint_sparkles(self, painter: QPainter, center: QPointF, box: float, elapsed: float, strength: float) -> None:
        """Draw small lights that come and go round the logo as it arrives."""
        if strength <= 0.0:
            return

        painter.setPen(Qt.PenStyle.NoPen)
        for across, down, rhythm in _SPARKLES:
            painter.setBrush(_white(0.9 * strength * (0.5 + 0.5 * math.sin(elapsed * 7.0 + rhythm))))
            painter.drawEllipse(center + QPointF(across * box, down * box), box * 0.011, box * 0.011)
