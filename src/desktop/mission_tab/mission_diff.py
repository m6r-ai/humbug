"""What a mission did to a file, drawn the way a code review shows a change."""

from PySide6.QtCore import QRectF, QSize, Qt
from PySide6.QtGui import QFont, QFontMetricsF, QPainter, QPaintEvent
from PySide6.QtWidgets import QSizePolicy, QWidget

from desktop.color_role import ColorRole
from desktop.mission_tab.mission_display import MissionDiffLineKind, MissionDiffLineView, MissionTone
from desktop.mission_tab.mission_painted import blend, readable, surface_colors, tone_color
from desktop.style_manager import StyleManager


# How many blocks the change bar has, as a code review shows how much of a change is added and removed.
_STAT_BLOCKS = 5

# Spaces a tab is drawn as.
_TAB_WIDTH = 4


class DiffStatBar(QWidget):
    """
    A row of small blocks showing how much of a file's change was added and how much removed.

    Green blocks stand for added lines and red for removed, in proportion; a change too small
    to fill the row leaves the rest grey, so a one-line fix and a rewrite look different.
    """

    def __init__(self, added: int, removed: int, parent: QWidget | None = None) -> None:
        """
        Initialise the bar.

        Args:
            added: Lines added
            removed: Lines removed
        """
        super().__init__(parent)
        self._added = added
        self._removed = removed
        self.setSizePolicy(QSizePolicy.Policy.Fixed, QSizePolicy.Policy.Fixed)
        self.setAttribute(Qt.WidgetAttribute.WA_TranslucentBackground)

    def blocks(self) -> tuple[int, int]:
        """Return how many blocks are green and how many red; the rest are grey."""
        total = self._added + self._removed
        if total == 0:
            return 0, 0

        filled = min(_STAT_BLOCKS, total)
        green = round(filled * self._added / total)
        if self._added and green == 0:
            green = 1

        red = filled - green
        if self._removed and red == 0 and green > 1:
            green -= 1
            red = 1

        return green, red

    def sizeHint(self) -> QSize:
        """Ask for room for the blocks."""
        zoom = StyleManager().zoom_factor()
        size = round(8 * zoom)
        gap = max(1, round(2 * zoom))
        return QSize(_STAT_BLOCKS * size + (_STAT_BLOCKS - 1) * gap, size)

    def paintEvent(self, event: QPaintEvent) -> None:
        """Draw the blocks."""
        super().paintEvent(event)
        zoom = StyleManager().zoom_factor()
        size = round(8 * zoom)
        gap = max(1, round(2 * zoom))
        green, red = self.blocks()
        _page, card = surface_colors()
        colors = (
            [tone_color(MissionTone.SUCCESS)] * green + [tone_color(MissionTone.DANGER)] * red
            + [blend(StyleManager().get_color(ColorRole.TEXT_INACTIVE), 0.35, card)] * (_STAT_BLOCKS - green - red)
        )
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        painter.setPen(Qt.PenStyle.NoPen)
        top = (self.height() - size) / 2
        for index, color in enumerate(colors):
            painter.setBrush(color)
            painter.drawRoundedRect(QRectF(index * (size + gap), top, size, size), 2 * zoom, 2 * zoom)


class FileDiffPanel(QWidget):
    """
    The lines of a file's diff: removed lines in red, added in green, with the line numbers before and after.

    Unchanged lines between changes are folded into a single row saying how many there are.
    Lines too long for the width are cut short rather than wrapped, so the columns stay in step;
    the panel never asks for more width than it is given.
    """

    def __init__(self, lines: list[MissionDiffLineView], parent: QWidget | None = None) -> None:
        """
        Initialise the panel.

        Args:
            lines: The lines to draw
        """
        super().__init__(parent)
        self._lines = lines
        self.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Fixed)
        # Code reads left to right whatever the language around it.
        self.setLayoutDirection(Qt.LayoutDirection.LeftToRight)

    def _metrics(self) -> tuple[QFontMetricsF, float, float]:
        """Return the font's metrics, the height of a row, and the width of a line-number column."""
        metrics = QFontMetricsF(self._font())
        digits = max((len(line.old_number) for line in self._lines), default=1)
        digits = max(digits, max((len(line.new_number) for line in self._lines), default=1), 2)
        return metrics, metrics.height() + 2 * StyleManager().zoom_factor(), metrics.horizontalAdvance("0" * digits) + 12

    def _font(self) -> QFont:
        """Return the code font, a little smaller than the text around it."""
        font = StyleManager().make_monospace_font()
        font.setPointSizeF(font.pointSizeF() * 0.85)
        return font

    def sizeHint(self) -> QSize:
        """Ask for a row for every line."""
        _metrics, row, _number = self._metrics()
        return QSize(0, round(row * len(self._lines)))

    def minimumSizeHint(self) -> QSize:
        """Ask for the height of every row and no width."""
        return self.sizeHint()

    def paintEvent(self, event: QPaintEvent) -> None:
        """Draw the rows."""
        super().paintEvent(event)
        style_manager = StyleManager()
        metrics, row, number_width = self._metrics()
        _page, card = surface_colors()
        text = style_manager.get_color(ColorRole.TEXT_PRIMARY)
        muted = readable(style_manager.get_color(ColorRole.TEXT_INACTIVE), card)
        success = tone_color(MissionTone.SUCCESS)
        danger = tone_color(MissionTone.DANGER)
        accent = tone_color(MissionTone.ACCENT)
        backgrounds = {
            MissionDiffLineKind.ADDED: blend(success, 0.13, card),
            MissionDiffLineKind.REMOVED: blend(danger, 0.11, card),
            MissionDiffLineKind.CONTEXT: card,
            MissionDiffLineKind.GAP: blend(accent, 0.07, card),
        }
        gutters = {
            MissionDiffLineKind.ADDED: blend(success, 0.22, card),
            MissionDiffLineKind.REMOVED: blend(danger, 0.19, card),
            MissionDiffLineKind.CONTEXT: blend(text, 0.04, card),
            MissionDiffLineKind.GAP: blend(accent, 0.12, card),
        }
        signs = {MissionDiffLineKind.ADDED: "+", MissionDiffLineKind.REMOVED: "−"}
        sign_colors = {
            MissionDiffLineKind.ADDED: readable(success, backgrounds[MissionDiffLineKind.ADDED]),
            MissionDiffLineKind.REMOVED: readable(danger, backgrounds[MissionDiffLineKind.REMOVED]),
        }
        sign_width = metrics.horizontalAdvance("+") + 10
        text_left = 2 * number_width + sign_width
        painter = QPainter(self)
        painter.setFont(self._font())
        width = self.width()
        exposed = event.rect()
        first = max(0, int(exposed.top() // row))
        last = min(len(self._lines), int(exposed.bottom() // row) + 1)
        for index in range(first, last):
            line = self._lines[index]
            top = index * row
            painter.fillRect(QRectF(0, top, width, row), backgrounds[line.kind])
            painter.fillRect(QRectF(0, top, 2 * number_width, row), gutters[line.kind])
            if line.kind == MissionDiffLineKind.GAP:
                painter.setPen(readable(accent, backgrounds[line.kind]))
                painter.drawText(
                    QRectF(text_left, top, max(0.0, width - text_left - 8), row),
                    Qt.AlignmentFlag.AlignVCenter | Qt.AlignmentFlag.AlignLeft,
                    f"⋯  {line.text}",
                )
                continue

            painter.setPen(muted)
            for column, number in enumerate((line.old_number, line.new_number)):
                painter.drawText(
                    QRectF(column * number_width, top, number_width - 6, row),
                    Qt.AlignmentFlag.AlignVCenter | Qt.AlignmentFlag.AlignRight,
                    number,
                )

            if line.kind in signs:
                painter.setPen(sign_colors[line.kind])
                painter.drawText(
                    QRectF(2 * number_width, top, sign_width, row), Qt.AlignmentFlag.AlignCenter, signs[line.kind]
                )

            painter.setPen(text)
            available = max(0.0, width - text_left - 8)
            shown = metrics.elidedText(line.text.replace("\t", " " * _TAB_WIDTH), Qt.TextElideMode.ElideRight, available)
            painter.drawText(
                QRectF(text_left, top, available, row), Qt.AlignmentFlag.AlignVCenter | Qt.AlignmentFlag.AlignLeft, shown
            )
