"""Mission Control dashboard widget."""

import dataclasses
from functools import partial
import os
import re
import sys
from collections.abc import Callable, Hashable

from PySide6.QtCore import (
    QEasingCurve, QEvent, QMimeData, QObject, QPoint, QPointF, QPropertyAnimation, QRect, QSize, Qt, QTimer, Signal,
    SignalInstance
)
from PySide6.QtGui import (
    QColor, QDrag, QDragEnterEvent, QDragLeaveEvent, QDragMoveEvent, QDropEvent, QHideEvent, QIcon, QKeyEvent,
    QMouseEvent, QPainter, QPainterPath, QPen, QPixmap, QResizeEvent, QLinearGradient, QPaintEvent, QShowEvent,
    QTextCursor, QTextDocument, QWheelEvent
)
from PySide6.QtWidgets import (
    QApplication, QBoxLayout, QCheckBox, QFrame, QHBoxLayout, QLabel, QLayout, QLayoutItem, QLineEdit, QPlainTextEdit,
    QPushButton,
    QScrollArea, QScroller, QSizePolicy, QToolButton, QVBoxLayout, QWidget
)

from desktop.attachments_bar import AttachmentsBar
from desktop.color_role import ColorRole
from desktop.language.language_manager import LanguageManager
from desktop.mission_tab.mission_display import (
    MissionActivityView, MissionAgentView, MissionChatView, MissionCheckView, MissionChipView, MissionDecisionView,
    MissionDiffView,
    MissionFileView,
    MissionGlanceView, MissionPhaseView, MissionResultView, MissionStatTarget, MissionStatView, MissionStepState,
    MissionStepView, MissionTone, MissionView
)
from desktop.mission_tab.mission_diff import DiffStatBar, FileDiffPanel
from desktop.mission_tab.mission_motion import MissionMotion
from desktop.mission_tab.mission_network import AgentNetworkView
from desktop.mission_tab.mission_intro import MissionIntro
from desktop.mission_tab.mission_scene import ConfettiOverlay, MissionJourney
from desktop.mission_tab.mission_painted import (
    BusySpinner, GlyphBadge, ProgressRing, StatusPill, StepMarker, blend, fade_in, readable, surface_colors,
    tone_color, tone_rgba, tone_text
)
from desktop.style_manager import StyleManager
from desktop.widgets.elided_label import ElidedLabel


# Widths, in unzoomed pixels, at which the layout reflows for narrow columns.
_SIDE_BY_SIDE_WIDTH = 1020
_SPLIT_LOWER_WIDTH = 900
_SIDE_COLUMN_WIDTH = 360

_DETAIL_MAX_LINES = 10
_DETAIL_MAX_CHARS = 900
_ACTIVITY_HEIGHT = 520
_ACTIVITY_WAITING_HEIGHT = 200
_CHAT_MAX_HEIGHT = 560
# Each entry's Markdown is measured as it is laid out, so a long conversation is shown from its end, a page at a time.
_CHAT_PAGE = 40

# How many missions there must be before the switcher offers to search them.
_SEARCH_FROM_MISSIONS = 4

# How long the report's copy button says the report was copied.
_COPIED_MS = 1600
_COMPOSER_MAX_LINES = 6

# The symbol beside each line of the mission at a glance.
_GLANCE_GLYPHS = {"now": "▶", "progress": "✓"}


def submit_key_text() -> str:
    """Return the key that sends a message, as Humbug names it elsewhere on this platform."""
    return "⌘ Enter" if sys.platform == "darwin" else "Ctrl+Enter"


def _scope_to_widget(stylesheet: str) -> str:
    """
    Prefix every rule with the widget's own selector.

    The widget first makes every plain container inside it transparent, so no
    band of colour shows behind rows of chips or controls; scoping the remaining
    rules to the widget makes them more specific than that, so cards, buttons
    and labels keep their own colours.
    """
    def scope(match: re.Match) -> str:
        selectors = [part.strip() for part in match.group(1).split(",")]
        scoped = [sel if sel.startswith("#MissionWidget") else f"#MissionWidget {sel}" for sel in selectors]
        return "\n" + ", ".join(scoped) + " {"

    return re.sub(r"([^{}]+?)\s*\{", scope, stylesheet)


# The line under a table's heading row: dashes between bars, with optional colons for alignment.
_TABLE_RULE = re.compile(r"^\s*\|?\s*:?-+:?\s*(\|\s*:?-+:?\s*)+\|?\s*$")


def _with_tables_set_apart(text: str) -> str:
    """
    Return Markdown with a blank line before any table that follows straight on from a line of text.

    Leads often write a table directly under the sentence that introduces it.  Without the blank
    line the table is not recognised, and is shown as a row of bars and dashes.
    """
    lines = text.split("\n")
    result: list[str] = []
    for index, line in enumerate(lines):
        starts_table = (
            "|" in line and index + 1 < len(lines) and _TABLE_RULE.match(lines[index + 1]) is not None
            and not _TABLE_RULE.match(line)
        )
        if starts_table and result and result[-1].strip() and "|" not in result[-1]:
            result.append("")

        result.append(line)

    return "\n".join(result)


def _report_markdown(text: str) -> str:
    """
    Return a report's Markdown made ready to show: its tables recognised, and its headings brought
    down to the size of bold text.

    A report's own headings would otherwise be drawn larger than the mission's name above them.
    """
    return re.sub(r"^#{1,3}(?=\s)", "####", _with_tables_set_apart(text), flags=re.MULTILINE)


def _clear_layout(layout: QLayout) -> None:
    """Remove and delete every widget in a layout."""
    while layout.count():
        item = layout.takeAt(0)
        widget = item.widget() if item is not None else None
        if widget is not None:
            widget.hide()
            widget.deleteLater()


def _sync_rows(
    layout: QBoxLayout, rows: list[tuple[Hashable, QWidget]], keys: list[Hashable], make: Callable[[int], QWidget]
) -> list[tuple[Hashable, QWidget]]:
    """
    Show one row per key, in order, keeping the rows already shown for a key and making only the rest.

    Rebuilding every row on each update makes Qt lay out and measure all their text again, which
    stalls the interface while a mission is busy; usually only a row or two is new.

    Args:
        layout: Layout holding nothing but the rows
        rows: The rows shown now, with their keys
        keys: The keys to show, in order
        make: Makes the row for the key at an index

    Returns:
        The rows now shown, with their keys
    """
    spare: dict[Hashable, list[QWidget]] = {}
    for key, row in rows:
        spare.setdefault(key, []).append(row)

    shown: list[tuple[Hashable, QWidget]] = []
    for index, key in enumerate(keys):
        kept = spare.get(key)
        shown.append((key, kept.pop(0) if kept else make(index)))

    for leftover in spare.values():
        for row in leftover:
            layout.removeWidget(row)
            row.hide()
            row.deleteLater()

    for index, (_key, row) in enumerate(shown):
        if layout.indexOf(row) != index:
            layout.removeWidget(row)
            layout.insertWidget(index, row)

    return shown


def _elide_detail(text: str) -> str:
    """Shorten approval detail to something that fits on a card."""
    lines = text.splitlines()
    shortened = "\n".join(lines[:_DETAIL_MAX_LINES])
    if len(shortened) > _DETAIL_MAX_CHARS:
        shortened = shortened[:_DETAIL_MAX_CHARS]

    if shortened != text:
        shortened = shortened.rstrip() + "\n…"

    return shortened


_REASON_MAX_CHARS = 220


def _split_reason(reason: str) -> tuple[str, str]:
    """
    Split an approval reason into a short summary and the rest.

    The summary is the reason's first paragraph, shortened if need be; the rest
    is shown with the request's detail, so a long request never crowds the card.
    """
    paragraphs = [part.strip() for part in reason.strip().split("\n\n") if part.strip()]
    if not paragraphs:
        return "", ""

    summary = " ".join(paragraphs[0].split())
    rest = "\n\n".join(paragraphs[1:])
    if len(summary) > _REASON_MAX_CHARS:
        rest = paragraphs[0] + (f"\n\n{rest}" if rest else "")
        summary = summary[:_REASON_MAX_CHARS - 1].rstrip() + "…"

    return summary, rest


def _breakable(text: str) -> str:
    """Let long unbroken text, such as a URL, wrap after its separators instead of running off the card."""
    for separator in ("/", "?", "&", "=", "_"):
        text = text.replace(separator, separator + "\u200b")

    return text


def _first_report_line(report: str) -> str:
    """Return the first line of a report with its Markdown marks removed, to show it folded."""
    for line in report.splitlines():
        plain = re.sub(r"[*_`#>]+", "", line).strip(" -")
        if plain:
            return plain

    return ""


def _send_icon(color: QColor, size: int) -> QIcon:
    """Return a paper-plane icon, the usual sign for sending a message."""
    ratio = 2
    pixmap = QPixmap(size * ratio, size * ratio)
    pixmap.fill(Qt.GlobalColor.transparent)
    pixmap.setDevicePixelRatio(ratio)
    painter = QPainter(pixmap)
    painter.setRenderHint(QPainter.RenderHint.Antialiasing)
    unit = size / 24
    plane = QPainterPath()
    plane.moveTo(3 * unit, 3.5 * unit)
    plane.lineTo(21.5 * unit, 12 * unit)
    plane.lineTo(3 * unit, 20.5 * unit)
    plane.lineTo(5.5 * unit, 12 * unit)
    plane.closeSubpath()
    painter.setPen(Qt.PenStyle.NoPen)
    painter.setBrush(color)
    painter.drawPath(plane)
    # The fold along the middle, cut out so the plane reads as paper.
    painter.setCompositionMode(QPainter.CompositionMode.CompositionMode_Clear)
    painter.setPen(QPen(Qt.GlobalColor.transparent, 1.6 * unit))
    painter.drawLine(QPointF(5.5 * unit, 12 * unit), QPointF(12 * unit, 12 * unit))
    painter.end()
    return QIcon(pixmap)


def _file_action_icon(action: str, color: QColor, size: int) -> QIcon:
    """
    Return a line icon for something that can be done to a file the mission changed.

    Args:
        action: "review" for a plus over a minus, the sign of a diff; "open" for an arrow leaving
            a box; "undo" for an arrow turning back on itself
        color: The colour of the lines
        size: The icon's width and height
    """
    ratio = 2
    pixmap = QPixmap(size * ratio, size * ratio)
    pixmap.fill(Qt.GlobalColor.transparent)
    pixmap.setDevicePixelRatio(ratio)
    painter = QPainter(pixmap)
    painter.setRenderHint(QPainter.RenderHint.Antialiasing)
    unit = size / 24
    pen = QPen(color, 2 * unit)
    pen.setCapStyle(Qt.PenCapStyle.RoundCap)
    pen.setJoinStyle(Qt.PenJoinStyle.RoundJoin)
    painter.setPen(pen)
    painter.setBrush(Qt.BrushStyle.NoBrush)
    path = QPainterPath()
    if action == "review":
        path.moveTo(12 * unit, 4 * unit)
        path.lineTo(12 * unit, 13 * unit)
        path.moveTo(7.5 * unit, 8.5 * unit)
        path.lineTo(16.5 * unit, 8.5 * unit)
        path.moveTo(7.5 * unit, 19 * unit)
        path.lineTo(16.5 * unit, 19 * unit)

    elif action == "open":
        path.moveTo(11 * unit, 5 * unit)
        path.lineTo(6 * unit, 5 * unit)
        path.quadTo(4 * unit, 5 * unit, 4 * unit, 7 * unit)
        path.lineTo(4 * unit, 18 * unit)
        path.quadTo(4 * unit, 20 * unit, 6 * unit, 20 * unit)
        path.lineTo(17 * unit, 20 * unit)
        path.quadTo(19 * unit, 20 * unit, 19 * unit, 18 * unit)
        path.lineTo(19 * unit, 13 * unit)
        path.moveTo(14 * unit, 4 * unit)
        path.lineTo(20 * unit, 4 * unit)
        path.lineTo(20 * unit, 10 * unit)
        path.moveTo(20 * unit, 4 * unit)
        path.lineTo(11 * unit, 13 * unit)

    else:
        path.moveTo(4 * unit, 9 * unit)
        path.lineTo(15 * unit, 9 * unit)
        path.cubicTo(18.5 * unit, 9 * unit, 20.5 * unit, 11.5 * unit, 20.5 * unit, 14.5 * unit)
        path.cubicTo(20.5 * unit, 17.5 * unit, 18.5 * unit, 20 * unit, 15 * unit, 20 * unit)
        path.lineTo(10 * unit, 20 * unit)
        path.moveTo(8 * unit, 5 * unit)
        path.lineTo(4 * unit, 9 * unit)
        path.lineTo(8 * unit, 13 * unit)

    painter.drawPath(path)
    painter.end()
    return QIcon(pixmap)


class _WrapLabel(QLabel):
    """
    A word-wrapped label that always keeps the height its text needs at its width.

    Wrapped labels in nested layouts can be laid out for a different width than
    they end up with, which clips their first and last lines.
    """

    def __init__(self, text: str = "", parent: QWidget | None = None) -> None:
        # Laying out Markdown to measure it is slow, and layouts ask a label its height for the same
        # width several times on every update, so each answer is kept until the label changes.
        self._heights: dict[tuple[int, int], int] = {}
        super().__init__(text, parent)
        self.setWordWrap(True)

    def setText(self, text: str) -> None:
        """Show new text, fitting the height to it: shorter text must not keep the height of longer text."""
        # Views are set again on every update, mostly with the text already shown, and measuring it
        # afresh each time is what makes an update slow.
        if text == self.text():
            return

        self._heights.clear()
        super().setText(text)
        self._fit_height()

    def setTextFormat(self, text_format: Qt.TextFormat) -> None:
        """Interpret the text in a new format, which changes the height it needs."""
        if text_format == self.textFormat():
            return

        self._heights.clear()
        super().setTextFormat(text_format)

    def heightForWidth(self, width: int) -> int:
        """Return the height the text needs at a width, measuring it only once for each width."""
        key = (width, self.minimumHeight())
        height = self._heights.get(key)
        if height is None:
            height = super().heightForWidth(width)
            self._heights[key] = height

        return height

    def changeEvent(self, event: QEvent) -> None:
        """Measure afresh when the font or style changes, as both change the height the text needs."""
        if event.type() in (QEvent.Type.FontChange, QEvent.Type.StyleChange):
            self._heights.clear()

        super().changeEvent(event)

    def resizeEvent(self, event: QResizeEvent) -> None:
        """Fit the wrapped text at the new width."""
        super().resizeEvent(event)
        self._fit_height()

    def _fit_height(self) -> None:
        """Keep exactly the height the text needs at the current width."""
        # A label not yet in a layout has Qt's default width, not one it will be shown at, so
        # measuring it there is wasted: it is fitted when its layout first gives it a width.
        if self.width() <= 0 or self.parentWidget() is None:
            return

        # The height a label reports for its width is never less than its minimum, so the
        # minimum is cleared before asking; otherwise the height could only ever grow.
        current = self.minimumHeight()
        self.setMinimumHeight(0)
        needed = self.heightForWidth(self.width())
        self.setMinimumHeight(needed if needed > 0 else current)


def _ignore_width(label: QLabel) -> None:
    """
    Let a wrapped label take whatever width its layout gives it, and the height its text needs there.

    Setting a size policy replaces the height-for-width flag a wrapped label sets for itself, and
    without it layouts guess the label's height, clipping its text or leaving space below it.
    """
    policy = QSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
    policy.setHeightForWidth(True)
    label.setSizePolicy(policy)


class _FlowLayout(QLayout):
    """
    Lays its widgets out in a row, starting a new row when the next one would not fit.

    However narrow it is made, it asks for no more width than its widest widget, so a row of
    buttons never holds the page wider than the window.
    """

    def __init__(self, spacing: int) -> None:
        """
        Initialise the layout.

        Args:
            spacing: The gap between widgets, across and down
        """
        super().__init__()
        self._items: list[QLayoutItem] = []
        self._gap = spacing

    def addItem(self, item: QLayoutItem) -> None:  # pylint: disable=invalid-name
        """Add a widget's item at the end."""
        self._items.append(item)

    def count(self) -> int:
        """Return how many items the layout holds."""
        return len(self._items)

    def itemAt(self, index: int) -> QLayoutItem | None:  # type: ignore[override]  # pylint: disable=invalid-name
        """Return the item at an index, or None past the end."""
        return self._items[index] if 0 <= index < len(self._items) else None

    def takeAt(self, index: int) -> QLayoutItem | None:  # type: ignore[override]  # pylint: disable=invalid-name
        """Remove and return the item at an index, or None past the end."""
        return self._items.pop(index) if 0 <= index < len(self._items) else None

    def expandingDirections(self) -> Qt.Orientation:  # pylint: disable=invalid-name
        """Take no more space than the widgets need."""
        return Qt.Orientation(0)

    def hasHeightForWidth(self) -> bool:  # pylint: disable=invalid-name
        """Say that the height depends on how many rows the width allows."""
        return True

    def heightForWidth(self, width: int) -> int:  # pylint: disable=invalid-name
        """Return the height the widgets take when laid out across a width."""
        return self._arrange(QRect(0, 0, width, 0), False)

    def setGeometry(self, rect: QRect) -> None:  # pylint: disable=invalid-name
        """Place the widgets within a rectangle."""
        super().setGeometry(rect)
        self._arrange(rect, True)

    def sizeHint(self) -> QSize:  # pylint: disable=invalid-name
        """Ask for the room the widgets need on a single row."""
        margins = self.contentsMargins()
        width = sum(item.sizeHint().width() for item in self._items) + self._gap * max(0, len(self._items) - 1)
        height = max((item.sizeHint().height() for item in self._items), default=0)
        return QSize(width + margins.left() + margins.right(), height + margins.top() + margins.bottom())

    def minimumSize(self) -> QSize:  # pylint: disable=invalid-name
        """Ask for no more width than the widest widget."""
        margins = self.contentsMargins()
        size = QSize()
        for item in self._items:
            size = size.expandedTo(item.sizeHint())

        return size.grownBy(margins)

    def _arrange(self, rect: QRect, place: bool) -> int:
        """
        Work out where each widget goes across a rectangle, placing them if asked.

        Returns:
            The height the rows take, with the layout's margins
        """
        margins = self.contentsMargins()
        rect = rect.marginsRemoved(margins)
        x = rect.x()
        y = rect.y()
        row_height = 0
        for item in self._items:
            hint = item.sizeHint()
            if x > rect.x() and x + hint.width() > rect.right() + 1:
                x = rect.x()
                y += row_height + self._gap
                row_height = 0

            if place:
                item.setGeometry(QRect(QPoint(x, y), hint))

            x += hint.width() + self._gap
            row_height = max(row_height, hint.height())

        return y + row_height - rect.y() + margins.top() + margins.bottom()


class _FittedScroll(QScrollArea):
    """
    A scroll area as tall as its content, up to its maximum height, that scrolls beyond that.

    Like a conversation, it keeps the newest content at the bottom in view, unless the
    user has scrolled up to read something earlier.  Content whose newest entry is at the
    top, such as an activity feed, is not followed.
    """

    def __init__(self, content: QWidget, parent: QWidget | None = None, follow_end: bool = True) -> None:
        super().__init__(parent)
        self.setFrameShape(QFrame.Shape.NoFrame)
        self.setWidgetResizable(True)
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.setSizePolicy(QSizePolicy.Policy.Preferred, QSizePolicy.Policy.Fixed)
        self.setWidget(content)
        content.installEventFilter(self)
        self._follow = follow_end
        self._to_top = False
        scrollbar = self.verticalScrollBar()
        scrollbar.valueChanged.connect(self._on_scrolled)
        scrollbar.rangeChanged.connect(self._on_range_changed)

    def sizeHint(self) -> QSize:
        content = self.widget()
        layout = content.layout() if content is not None else None
        if layout is None:
            return super().sizeHint()

        width = self.viewport().width()
        height = layout.totalHeightForWidth(width) if layout.hasHeightForWidth() else layout.sizeHint().height()
        return QSize(super().sizeHint().width(), min(height, self.maximumHeight()))

    def eventFilter(self, watched: QObject, event: QEvent) -> bool:
        """
        Take the content's height whenever it is laid out again.

        A scroll area does not tell the layout it sits in when its content changes
        size, so without this it keeps the height it had when it was empty.
        """
        if watched is self.widget() and event.type() == QEvent.Type.LayoutRequest:
            self.updateGeometry()
            # The scroll area sizes its content only when it is itself asked to lay out, so a
            # change inside the content would otherwise leave it at a height it no longer needs.
            # The request is posted, not sent, so that many changes at once are laid out once.
            QApplication.postEvent(self, QEvent(QEvent.Type.LayoutRequest))

        return super().eventFilter(watched, event)

    def resizeEvent(self, event: QResizeEvent) -> None:
        """Wrapped text needs a different height at a different width."""
        super().resizeEvent(event)
        if event.size().width() != event.oldSize().width():
            self.updateGeometry()

    def _on_scrolled(self, value: int) -> None:
        """Follow new content only while the newest is in view."""
        self._follow = value >= self.verticalScrollBar().maximum() - 2

    def show_top_next(self) -> None:
        """Show the top of the content once it has been laid out again, for content added above what is in view."""
        self._follow = False
        self._to_top = True

    def _on_range_changed(self, _minimum: int, maximum: int) -> None:
        """Keep the newest content in view when more arrives."""
        if self._to_top:
            self._to_top = False
            self.verticalScrollBar().setValue(0)
            return

        if self._follow:
            self.verticalScrollBar().setValue(maximum)


def _dot_icon(tone: MissionTone, ringed: bool) -> QIcon:
    """
    Return a small coloured dot used to show a mission's status on its chip.

    On the selected chip, which is filled with the primary colour, the dot gets a
    light ring so it stays visible whatever its colour.
    """
    size = 12
    pixmap = QPixmap(size, size)
    pixmap.fill(Qt.GlobalColor.transparent)
    painter = QPainter(pixmap)
    painter.setRenderHint(QPainter.RenderHint.Antialiasing)
    if ringed:
        painter.setPen(QPen(QColor("#ffffff"), 2))

    else:
        painter.setPen(Qt.PenStyle.NoPen)

    painter.setBrush(tone_color(tone))
    painter.drawEllipse(2, 2, size - 4, size - 4)
    painter.end()
    return QIcon(pixmap)


class _ClickableFrame(QFrame):
    """
    A frame that reports clicks, used for step and agent rows.

    It takes focus and answers Return and Space as a click, so the rows it is
    used for can be opened without a mouse.
    """

    clicked = Signal()

    def __init__(self, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self.setFocusPolicy(Qt.FocusPolicy.StrongFocus)

    def set_clickable(self, clickable: bool) -> None:
        """Choose whether the frame can be clicked; one that cannot is not a stop when tabbing."""
        self.setFocusPolicy(Qt.FocusPolicy.StrongFocus if clickable else Qt.FocusPolicy.NoFocus)

    def mousePressEvent(self, event: QMouseEvent) -> None:
        """Report a left click."""
        if event.button() == Qt.MouseButton.LeftButton:
            self.clicked.emit()
            event.accept()
            return

        super().mousePressEvent(event)

    def keyPressEvent(self, event: QKeyEvent) -> None:
        """Report Return and Space as a click."""
        if event.key() in (Qt.Key.Key_Return, Qt.Key.Key_Enter, Qt.Key.Key_Space):
            self.clicked.emit()
            event.accept()
            return

        super().keyPressEvent(event)


# The type a step being dragged to a new place in the plan travels as.
_STEP_MIME = "application/x-humbug-mission-step"


class _StepHandle(QLabel):
    """The grip a plan step is dragged by, to move it to another place in the plan."""

    def __init__(self, task_id: str, parent: QWidget | None = None) -> None:
        super().__init__("⠿", parent)
        self._task_id = task_id
        self._pressed_at: QPoint | None = None
        self.setObjectName("MissionStepHandle")
        self.setCursor(Qt.CursorShape.OpenHandCursor)

    def mousePressEvent(self, event: QMouseEvent) -> None:
        """Note where a drag may start; the press does not open the step."""
        if event.button() == Qt.MouseButton.LeftButton:
            self._pressed_at = event.position().toPoint()
            event.accept()
            return

        super().mousePressEvent(event)

    def mouseMoveEvent(self, event: QMouseEvent) -> None:
        """Start dragging the step once the mouse has moved far enough."""
        if self._pressed_at is None or not event.buttons() & Qt.MouseButton.LeftButton:
            return

        if (event.position().toPoint() - self._pressed_at).manhattanLength() < QApplication.startDragDistance():
            return

        self._pressed_at = None
        content = QMimeData()
        content.setData(_STEP_MIME, self._task_id.encode("utf-8"))
        drag = QDrag(self)
        drag.setMimeData(content)
        row = self.parentWidget()
        if row is not None:
            drag.setPixmap(row.grab())
            drag.setHotSpot(self.mapTo(row, event.position().toPoint()))

        drag.exec(Qt.DropAction.MoveAction)

    def mouseReleaseEvent(self, event: QMouseEvent) -> None:
        """Forget the press."""
        self._pressed_at = None
        super().mouseReleaseEvent(event)


class _StepRow(_ClickableFrame):
    """
    A plan step that, while the plan can be rearranged, takes other steps dropped on it.

    A step dropped on the upper half of the row goes before it, and on the lower half after it.
    Alt+Up and Alt+Down move the focused step without a mouse.
    """

    def __init__(
        self, task_id: str, move: Callable[[str, str, bool], None] | None, nudge: Callable[[int], None] | None
    ) -> None:
        """
        Initialise the row.

        Args:
            task_id: The step the row shows
            move: Called with the dragged step, this step, and whether it goes after this one; None when
                the plan cannot be rearranged
            nudge: Called with -1 or 1 to move this step up or down one place; None when it cannot be
        """
        super().__init__()
        self._task_id = task_id
        self._move = move
        self._nudge = nudge
        self.setAcceptDrops(move is not None)

    def _set_drop_edge(self, edge: str) -> None:
        """Show where a dropped step would land: a line above or below the row, or none."""
        if self.property("dropEdge") != edge:
            self.setProperty("dropEdge", edge)
            self.style().unpolish(self)
            self.style().polish(self)

    def dragEnterEvent(self, event: QDragEnterEvent) -> None:
        """Take a step being dragged over the row."""
        if event.mimeData().hasFormat(_STEP_MIME):
            event.acceptProposedAction()
            return

        event.ignore()

    def dragMoveEvent(self, event: QDragMoveEvent) -> None:
        """Show whether the step would land above or below this one."""
        if not event.mimeData().hasFormat(_STEP_MIME):
            event.ignore()
            return

        self._set_drop_edge("bottom" if event.position().y() > self.height() / 2 else "top")
        event.acceptProposedAction()

    def dragLeaveEvent(self, event: QDragLeaveEvent) -> None:
        """Stop showing where a step would land."""
        self._set_drop_edge("")
        super().dragLeaveEvent(event)

    def dropEvent(self, event: QDropEvent) -> None:
        """Move the dropped step before or after this one."""
        self._set_drop_edge("")
        if self._move is None or not event.mimeData().hasFormat(_STEP_MIME):
            event.ignore()
            return

        dragged = bytes(event.mimeData().data(_STEP_MIME).data()).decode("utf-8")
        event.acceptProposedAction()
        self._move(dragged, self._task_id, event.position().y() > self.height() / 2)

    def keyPressEvent(self, event: QKeyEvent) -> None:
        """Move the step up or down with Alt+Up and Alt+Down."""
        if self._nudge is not None and event.modifiers() & Qt.KeyboardModifier.AltModifier and event.key() in (
            Qt.Key.Key_Up, Qt.Key.Key_Down
        ):
            self._nudge(-1 if event.key() == Qt.Key.Key_Up else 1)
            event.accept()
            return

        super().keyPressEvent(event)


class _EdgeFade(QWidget):
    """A fade over one end of the chip strip, hinting that more missions lie that way."""

    def __init__(self, direction: int, parent: QWidget) -> None:
        super().__init__(parent)
        self._direction = direction
        self.setAttribute(Qt.WidgetAttribute.WA_TransparentForMouseEvents)
        self.hide()

    def paintEvent(self, event: QPaintEvent) -> None:
        """Fade from the page colour at the edge to clear."""
        super().paintEvent(event)
        page, _card = surface_colors()
        clear = QColor(page)
        clear.setAlphaF(0.0)
        gradient = QLinearGradient(0, 0, self.width(), 0)
        gradient.setColorAt(0.0, page if self._direction < 0 else clear)
        gradient.setColorAt(1.0, clear if self._direction < 0 else page)
        painter = QPainter(self)
        painter.fillRect(self.rect(), gradient)
        painter.end()


class _ChipStrip(QScrollArea):
    """
    A row of mission chips that scrolls sideways when there are more than fit.

    A sideways swipe on a trackpad or a touch screen moves it, while an
    up-and-down scroll is left to the page.  It reports when its position or
    extent changes, so arrow buttons beside it can follow.
    """

    moved = Signal()

    def __init__(self, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self.setObjectName("MissionChipStrip")
        self.setFrameShape(QFrame.Shape.NoFrame)
        self.setWidgetResizable(True)
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
        self._keep_in_view: QWidget | None = None
        scrollbar = self.horizontalScrollBar()
        scrollbar.valueChanged.connect(lambda _value: self.moved.emit())
        scrollbar.rangeChanged.connect(self._on_range_changed)
        self._animation = QPropertyAnimation(scrollbar, b"value", self)
        self._animation.setDuration(260)
        self._animation.setEasingCurve(QEasingCurve.Type.OutCubic)
        QScroller.grabGesture(self.viewport(), QScroller.ScrollerGestureType.TouchGesture)
        self._fades = {-1: _EdgeFade(-1, self), 1: _EdgeFade(1, self)}
        self.moved.connect(self._update_fades)

    def _update_fades(self) -> None:
        """Fade each end that has more missions beyond it."""
        width = round(36 * StyleManager().zoom_factor())
        for direction, fade in self._fades.items():
            fade.setGeometry(0 if direction < 0 else self.width() - width, 0, width, self.height())
            fade.setVisible(self.can_scroll(direction))
            fade.raise_()

    def resizeEvent(self, event: QResizeEvent) -> None:
        """Keep the fades at the ends."""
        super().resizeEvent(event)
        self._update_fades()

    def sizeHint(self) -> QSize:
        content = self.widget()
        height = content.sizeHint().height() if content is not None else 0
        return QSize(super().sizeHint().width(), height)

    def eventFilter(self, watched: QObject, event: QEvent) -> bool:
        """
        Take the chips' height whenever they are laid out again.

        A scroll area does not tell the layout it sits in when its content changes
        size, so without this the strip keeps the height it had when it was empty.
        """
        if watched is self.widget() and event.type() == QEvent.Type.LayoutRequest:
            self.updateGeometry()

        return super().eventFilter(watched, event)

    def minimumSizeHint(self) -> QSize:
        return QSize(0, self.sizeHint().height())

    def keep_in_view(self, widget: QWidget | None) -> None:
        """Scroll a chip into view once the strip has been laid out."""
        self._keep_in_view = widget
        self.updateGeometry()

    def _on_range_changed(self, _minimum: int, _maximum: int) -> None:
        """Bring the chip asked for into view now its position is known, and report the change."""
        if self._keep_in_view is not None:
            self._glide_into_view(self._keep_in_view)
            self._keep_in_view = None

        self.moved.emit()

    def _glide_into_view(self, chip: QWidget) -> None:
        """Glide sideways just far enough for a chip to be fully in view."""
        content = self.widget()
        if content is None or not content.isAncestorOf(chip):
            return

        margin = round(24 * StyleManager().zoom_factor())
        left = chip.mapTo(content, QPoint(0, 0)).x()
        scrollbar = self.horizontalScrollBar()
        view = self.viewport().width()
        if left - margin < scrollbar.value():
            target = left - margin

        elif left + chip.width() + margin > scrollbar.value() + view:
            target = left + chip.width() + margin - view

        else:
            return

        self._glide(max(scrollbar.minimum(), min(scrollbar.maximum(), target)))

    def _glide(self, target: int) -> None:
        """Move smoothly to a scroll position, or straight there if motion is reduced."""
        self._animation.stop()
        if MissionMotion().reduced():
            self.horizontalScrollBar().setValue(target)
            return

        self._animation.setStartValue(self.horizontalScrollBar().value())
        self._animation.setEndValue(target)
        self._animation.start()

    def can_scroll(self, direction: int) -> bool:
        """Return whether there are chips further left (direction -1) or right (direction 1)."""
        scrollbar = self.horizontalScrollBar()
        return scrollbar.value() > scrollbar.minimum() if direction < 0 else scrollbar.value() < scrollbar.maximum()

    def overflowing(self) -> bool:
        """Return whether the chips are wider than the strip."""
        return self.horizontalScrollBar().maximum() > 0

    def scroll_page(self, direction: int) -> None:
        """Glide most of a strip's width left (direction -1) or right (direction 1)."""
        scrollbar = self.horizontalScrollBar()
        start = self._animation.endValue() if self._animation.state() == QPropertyAnimation.State.Running else scrollbar.value()
        target = max(scrollbar.minimum(), min(scrollbar.maximum(), int(start) + direction * round(self.viewport().width() * 0.7)))
        self._glide(target)

    def wheelEvent(self, event: QWheelEvent) -> None:
        """Move sideways on a sideways swipe; leave an up-and-down scroll to the page."""
        delta = event.pixelDelta() if not event.pixelDelta().isNull() else event.angleDelta()
        if delta.x() == 0 or abs(delta.x()) < abs(delta.y()):
            event.ignore()
            return

        scrollbar = self.horizontalScrollBar()
        scrollbar.setValue(scrollbar.value() - delta.x())
        event.accept()


class _ComposerEdit(QPlainTextEdit):
    """
    The composer's text box, with the same keys as a conversation's input.

    Enter starts a new line and Ctrl+Enter (Cmd+Enter on macOS) sends.  The box
    grows with its text up to a few lines and then scrolls.
    """

    submitted = Signal()

    def __init__(self, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self.setFrameShape(QFrame.Shape.NoFrame)
        self.setTabChangesFocus(True)
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAsNeeded)
        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
        self.document().documentLayout().documentSizeChanged.connect(self._fit_to_text)
        self._fit_to_text()

    def keyPressEvent(self, event: QKeyEvent) -> None:
        """Send on Ctrl+Enter (Cmd+Enter on macOS); every other key edits as usual."""
        submits = event.modifiers() & (
            Qt.KeyboardModifier.ControlModifier | Qt.KeyboardModifier.MetaModifier
        )
        if event.key() in (Qt.Key.Key_Return, Qt.Key.Key_Enter) and submits:
            self.submitted.emit()
            event.accept()
            return

        super().keyPressEvent(event)

    def set_margin(self, margin: int) -> None:
        """Set the space around the text."""
        self.document().setDocumentMargin(margin)
        self._fit_to_text()

    def _fit_to_text(self) -> None:
        """Grow with the text, up to the maximum number of lines."""
        lines = max(1, min(_COMPOSER_MAX_LINES, round(self.document().size().height())))
        margin = self.document().documentMargin()
        self.setFixedHeight(round(lines * self.fontMetrics().lineSpacing() + 2 * margin + 4))


class MissionWidget(QWidget):
    """
    The Mission Control dashboard.

    Presentational only: it is given display records and reports what the
    user does through signals.  It holds no mission state.
    """

    mission_selected = Signal(str)
    new_mission_requested = Signal()
    submit_requested = Signal(str)
    attach_requested = Signal()
    settings_requested = Signal()
    switch_to_classic_requested = Signal()
    switch_to_autonomous_requested = Signal()
    task_stop_requested = Signal(str)
    resume_requested = Signal()
    retry_requested = Signal()
    pause_requested = Signal()
    unpause_requested = Signal()
    carry_on_requested = Signal()
    auto_approve_selected = Signal(str)
    new_mission_auto_approve_selected = Signal(str)
    template_save_requested = Signal(str)
    new_mission_review_plan_toggled = Signal(bool)
    new_mission_level_remembered = Signal(bool)
    start_work_requested = Signal()
    plan_rearranged = Signal(list, list)
    file_open_requested = Signal(str)
    file_undo_requested = Signal(str)
    file_review_requested = Signal(str, int)
    file_diff_requested = Signal(str)
    report_export_requested = Signal(str)
    template_delete_requested = Signal(str)
    auto_resume_toggled = Signal(bool)
    reduce_motion_toggled = Signal(bool)
    stop_requested = Signal()
    remove_requested = Signal()
    mission_rename_requested = Signal(str)
    mission_delete_requested = Signal(str)
    mission_archive_requested = Signal(str, bool)
    mission_rerun_requested = Signal(str)
    guide_dismissed = Signal(bool)
    conversation_requested = Signal(str)
    decision_approved = Signal(str)
    decision_unsure = Signal(str)
    decision_rejected = Signal(str)
    wait_finish_requested = Signal(str)

    def __init__(self, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self.setObjectName("MissionWidget")
        # Clicking anywhere on the page gives it the keyboard, for the Decision Inbox's shortcuts.
        self.setFocusPolicy(Qt.FocusPolicy.ClickFocus)
        self._style_manager = StyleManager()
        self._language_manager = LanguageManager()

        self._chips: list[MissionChipView] | None = None
        self._steps: list[MissionStepView] | None = None
        self._history: list[MissionPhaseView] | None = None
        self._agents: list[MissionAgentView] | None = None
        self._decisions: list[MissionDecisionView] | None = None
        self._activity: list[MissionActivityView] | None = None
        self._stats: list[MissionStatView] | None = None
        self._pills: list[StatusPill] = []
        self._spinners: list[BusySpinner] = []
        self._has_mission = False
        # A finished mission is its report and the conversation that led to it: the cards that showed
        # it working are put away, and its plan and activity are folded behind links on the report.
        self._completed = False
        self._report_steps_shown = False
        self._report_activity_shown = False
        self._report_source = ""
        self._files: list[MissionFileView] | None = None
        self._checks: list[MissionCheckView] | None = None
        self._focus_step_after_render = ""
        # Each file's chevron and the frame its diff unfolds into, the files unfolded, and the diffs shown.
        self._file_rows: dict[str, tuple[QLabel, QFrame]] = {}
        self._expanded_files: set[str] = set()
        self._file_diffs: dict[str, MissionDiffView] = {}
        self._templates: list[tuple[str, str]] | None = None
        self._shown_chip_views: list[MissionChipView] = []
        # The request the keyboard answers, and whether the user has started answering by keyboard,
        # which is when the selected request is marked.
        self._selected_decision = ""
        self._keyboard_answering = False
        # The details below the mission at a glance are shown until the user folds them away.
        self._details_shown = True
        self._glance_shown = False
        self._view: MissionView | None = None
        self._new_mission_selected = True
        self._new_chip: QPushButton | None = None
        self._awaiting_reply = False
        self._plan_ready = False
        self._lead_text = ""
        self._step_filter: MissionStatTarget = MissionStatTarget.ALL_STEPS
        self._expanded_steps: set[str] = set()
        self._just_expanded = ""
        self._history_open = False
        self._expanded_phases: set[str] = set()
        self._shown_decisions: set[str] = set()
        self._shown_activity: set[tuple[str, str]] = set()
        self._activity_rows: list[tuple[Hashable, QWidget]] = []
        self._chat_rows: list[tuple[Hashable, QWidget]] = []
        self._chat_shown = _CHAT_PAGE
        self._decision_cards: dict[str, QFrame] = {}
        self._highlighted_card: QFrame | None = None
        self._open_activity: set[tuple[str, str]] = set()
        self._results: list[MissionResultView] | None = None
        self._conversation: list[MissionChatView] | None = None
        self._lead_context_id = ""
        self._open_results: set[str] = set()
        self._new_mission_auto_approve = "off"
        self._just_opened_result = ""
        self._just_opened_activity: tuple[str, str] | None = None

        # Takes the highlight off a decision card the user was taken to.
        self._highlight_timer = QTimer(self)
        self._highlight_timer.setSingleShot(True)
        self._highlight_timer.setInterval(1600)
        self._highlight_timer.timeout.connect(self._clear_highlight)

        # Moving the page to show something glides there.  The move waits for the next turn of the
        # event loop, so a card that has just been shown or rebuilt is measured once laid out.
        self._glide_target: QWidget | None = None
        self._glide_timer = QTimer(self)
        self._glide_timer.setSingleShot(True)
        self._glide_timer.setInterval(0)
        self._glide_timer.timeout.connect(self._glide_now)

        # While an answer given in an agent's card takes effect, the cards above it change size; the
        # agents are held where they were on screen so the user is not carried somewhere else.
        self._hold_offset: int | None = None
        self._hold_timer = QTimer(self)
        self._hold_timer.setSingleShot(True)
        self._hold_timer.setInterval(2500)
        self._hold_timer.timeout.connect(self._release_hold)

        self._spinner_timer = QTimer(self)
        self._spinner_timer.setInterval(90)
        self._spinner_timer.timeout.connect(self._advance_spinners)

        root = QVBoxLayout(self)
        root.setContentsMargins(0, 0, 0, 0)
        root.setSpacing(0)

        root.addWidget(self._build_top_bar())
        root.addWidget(self._build_everything_banner())

        self._scroll = QScrollArea()
        self._scroll.setObjectName("MissionScroll")
        self._scroll.setFrameShape(QFrame.Shape.NoFrame)
        self._scroll.setWidgetResizable(True)
        self._scroll.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        root.addWidget(self._scroll, 1)
        self._scroll_animations: dict[QScrollArea, QPropertyAnimation] = {}

        content = QWidget()
        content.setObjectName("MissionContent")
        self._content_layout = QVBoxLayout(content)
        self._content_layout.setSpacing(16)
        self._scroll.setWidget(content)

        # New mission stays in place on the left; the missions themselves scroll beside it, with
        # arrows that appear when there are more than fit.
        self._chip_row = QWidget()
        row_layout = QHBoxLayout(self._chip_row)
        row_layout.setContentsMargins(0, 0, 0, 0)
        row_layout.setSpacing(8)
        self._new_chip_layout = QHBoxLayout()
        self._new_chip_layout.setContentsMargins(0, 0, 0, 0)
        row_layout.addLayout(self._new_chip_layout)
        self._chip_left = self._chip_arrow("‹", -1)
        row_layout.addWidget(self._chip_left)
        self._chip_strip = _ChipStrip()
        self._chip_strip.moved.connect(self._update_chip_arrows)
        strip_content = QWidget()
        self._chip_layout = QHBoxLayout(strip_content)
        self._chip_layout.setContentsMargins(0, 0, 0, 0)
        self._chip_layout.setSpacing(8)
        self._chip_strip.setWidget(strip_content)
        row_layout.addWidget(self._chip_strip, 1)
        self._chip_right = self._chip_arrow("›", 1)
        row_layout.addWidget(self._chip_right)
        # Finding a mission among many, and the ones put away.
        self._mission_search = QLineEdit()
        self._mission_search.setObjectName("MissionSearch")
        self._mission_search.setClearButtonEnabled(True)
        self._mission_search.setMaximumWidth(220)
        self._mission_search.textChanged.connect(self._refilter_missions)
        row_layout.addWidget(self._mission_search)
        self._archived_toggle = QPushButton()
        self._archived_toggle.setObjectName("MissionPlanFilter")
        self._archived_toggle.setCheckable(True)
        self._archived_toggle.setCursor(Qt.CursorShape.PointingHandCursor)
        self._archived_toggle.toggled.connect(self._refilter_missions)
        row_layout.addWidget(self._archived_toggle)
        self._content_layout.addWidget(self._chip_row)

        # Another mission waiting for an answer, so it is noticed from wherever the user is.
        self._elsewhere = QFrame()
        self._elsewhere.setObjectName("MissionAttention")
        elsewhere_layout = QHBoxLayout(self._elsewhere)
        elsewhere_layout.setContentsMargins(16, 8, 10, 8)
        elsewhere_layout.setSpacing(12)
        self._elsewhere_bell = GlyphBadge("!", MissionTone.WARNING, 24)
        elsewhere_layout.addWidget(self._elsewhere_bell)
        self._elsewhere_text = QLabel()
        self._elsewhere_text.setObjectName("MissionAttentionText")
        self._elsewhere_text.setWordWrap(True)
        self._elsewhere_text.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
        elsewhere_layout.addWidget(self._elsewhere_text, 1)
        self._elsewhere_button = QPushButton()
        self._elsewhere_button.setObjectName("MissionReplyButton")
        self._elsewhere_button.setCursor(Qt.CursorShape.PointingHandCursor)
        self._elsewhere_button.clicked.connect(self._open_waiting_mission)
        elsewhere_layout.addWidget(self._elsewhere_button)
        self._elsewhere_mission_id = ""
        self._elsewhere.hide()
        self._content_layout.addWidget(self._elsewhere)

        self._body = QBoxLayout(QBoxLayout.Direction.LeftToRight)
        self._body.setSpacing(16)
        self._content_layout.addLayout(self._body, 1)

        self._main_column = QWidget()
        main_layout = QVBoxLayout(self._main_column)
        main_layout.setContentsMargins(0, 0, 0, 0)
        main_layout.setSpacing(16)
        self._hero = self._build_hero()
        main_layout.addWidget(self._hero)
        self._lead_card = self._build_lead_card()
        self._classic_card = self._build_classic_card()
        self._report_card = self._build_report_card()
        self._results_card = self._build_results_card()
        self._chat_card = self._build_chat_card()
        self._glance = self._build_glance()
        self._dashboard = self._build_dashboard()
        main_layout.addWidget(self._dashboard)
        main_layout.addStretch(1)
        self._body.addWidget(self._main_column, 1)

        self._side_column = QWidget()
        side_layout = QVBoxLayout(self._side_column)
        side_layout.setContentsMargins(0, 0, 0, 0)
        side_layout.setSpacing(16)
        self._inbox_card = self._build_inbox()
        side_layout.addWidget(self._inbox_card)
        self._activity_card = self._build_activity()
        side_layout.addWidget(self._activity_card)
        side_layout.addWidget(self._chat_card)
        side_layout.addStretch(1)
        self._body.addWidget(self._side_column)
        # Takes the spare height once the columns are stacked, so the side cards follow the main ones directly.
        self._body.addStretch(0)

        # The call to action sits above the page rather than in it, so it stays in view however far
        # the user has scrolled, and at a narrow width it is not left below the whole dashboard.
        attention_bar = QWidget()
        attention_bar.setObjectName("MissionAttentionBar")
        attention_layout = QVBoxLayout(attention_bar)
        attention_layout.setContentsMargins(24, 12, 24, 0)
        attention_layout.addWidget(self._attention)
        self._attention_bar = attention_bar
        attention_bar.hide()
        root.insertWidget(root.indexOf(self._scroll), attention_bar)
        self._report_pin = self._build_report_pin()
        self._scroll.verticalScrollBar().valueChanged.connect(lambda _value: self._update_report_pin())
        self._scroll.verticalScrollBar().rangeChanged.connect(lambda _low, _high: self._update_report_pin())
        root.addWidget(self._build_composer())
        self._confetti = ConfettiOverlay(self)

        self.retranslate()
        self.set_mission(None)
        self.set_decisions([])

    def _chip_arrow(self, glyph: str, direction: int) -> QPushButton:
        """Create an arrow that scrolls the mission chips one way, labelled in retranslate."""
        button = QPushButton(glyph)
        button.setObjectName("MissionChipArrow")
        button.setCursor(Qt.CursorShape.PointingHandCursor)
        button.clicked.connect(lambda _checked=False: self._chip_strip.scroll_page(direction))
        button.hide()
        return button

    def _update_chip_arrows(self) -> None:
        """Show the arrows only when the chips overflow, and enable each while there is more that way."""
        overflowing = self._chip_strip.overflowing()
        for button, direction in ((self._chip_left, -1), (self._chip_right, 1)):
            button.setVisible(overflowing)
            button.setEnabled(self._chip_strip.can_scroll(direction))

    def _build_top_bar(self) -> QWidget:
        """Create the title bar."""
        top_bar = QFrame()
        top_bar.setObjectName("MissionTopBar")
        layout = QHBoxLayout(top_bar)
        layout.setContentsMargins(20, 12, 20, 12)
        layout.setSpacing(12)

        self._top_badge = GlyphBadge("◎", MissionTone.ACCENT, 36)
        layout.addWidget(self._top_badge)

        titles = QVBoxLayout()
        titles.setSpacing(0)
        self._top_title = QLabel()
        self._top_title.setObjectName("MissionTopTitle")
        titles.addWidget(self._top_title)
        self._top_subtitle = QLabel()
        self._top_subtitle.setObjectName("MissionTopSubtitle")
        titles.addWidget(self._top_subtitle)
        layout.addLayout(titles)
        layout.addStretch(1)
        # Side by side when there is room, one above the other when the window is narrow.
        self._top_options = _FlowLayout(16)
        layout.addLayout(self._top_options)
        self._auto_resume = QCheckBox()
        self._auto_resume.setObjectName("MissionAutoResume")
        self._auto_resume.setCursor(Qt.CursorShape.PointingHandCursor)
        self._auto_resume.toggled.connect(self.auto_resume_toggled)
        self._top_options.addWidget(self._auto_resume)
        self._reduce_motion = QCheckBox()
        self._reduce_motion.setObjectName("MissionReduceMotion")
        self._reduce_motion.setCursor(Qt.CursorShape.PointingHandCursor)
        self._reduce_motion.toggled.connect(self.reduce_motion_toggled)
        self._top_options.addWidget(self._reduce_motion)
        return top_bar

    def _build_everything_banner(self) -> QWidget:
        """
        Create the warning shown while the selected mission approves everything.

        It sits outside the scrolling page, so it stays in view wherever the user is on it.
        """
        self._everything_banner = QFrame()
        self._everything_banner.setObjectName("MissionEverythingBanner")
        layout = QHBoxLayout(self._everything_banner)
        layout.setContentsMargins(20, 8, 20, 8)
        layout.setSpacing(12)
        layout.addWidget(GlyphBadge("!", MissionTone.DANGER, 24))
        self._everything_text = QLabel()
        self._everything_text.setObjectName("MissionEverythingText")
        self._everything_text.setWordWrap(True)
        self._everything_text.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
        layout.addWidget(self._everything_text, 1)
        self._everything_off_button = QPushButton()
        self._everything_off_button.setObjectName("MissionStopButton")
        self._everything_off_button.setCursor(Qt.CursorShape.PointingHandCursor)
        self._everything_off_button.clicked.connect(lambda: self._on_auto_approve_clicked("off"))
        layout.addWidget(self._everything_off_button)
        self._everything_title = ""
        self._everything_banner.hide()
        return self._everything_banner

    def _auto_approve_controls(
        self, on_clicked: Callable[[str], None], centred: bool = False
    ) -> tuple[QWidget, QLabel, dict[str, QPushButton], QLabel]:
        """
        Create a row choosing an auto-approve level, and the note saying what the level means.

        Args:
            on_clicked: Called with the level a button stands for when it is clicked
            centred: Whether the row and note are centred rather than set to the left

        Returns:
            The row, its label, its buttons by level, and the note
        """
        row = QWidget()
        row_layout = QHBoxLayout(row)
        row_layout.setContentsMargins(0, 8, 0, 0)
        row_layout.setSpacing(10)
        if centred:
            row_layout.addStretch(1)

        label = QLabel()
        label.setObjectName("MissionFieldLabel")
        row_layout.addWidget(label)
        segment = QFrame()
        segment.setObjectName("MissionSegment")
        segment_layout = QHBoxLayout(segment)
        segment_layout.setContentsMargins(3, 3, 3, 3)
        segment_layout.setSpacing(2)
        buttons: dict[str, QPushButton] = {}
        for level in ("off", "safe", "all"):
            button = QPushButton()
            button.setObjectName("MissionSegmentButton")
            button.setCheckable(True)
            button.setCursor(Qt.CursorShape.PointingHandCursor)
            button.clicked.connect(lambda _checked=False, v=level: on_clicked(v))
            segment_layout.addWidget(button)
            buttons[level] = button

        row_layout.addWidget(segment)
        row_layout.addStretch(1)
        # What the chosen level means, in plain words, so the user need not hover for a tooltip.
        note = QLabel()
        note.setObjectName("MissionHintText")
        note.setWordWrap(True)
        note.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
        if centred:
            note.setAlignment(Qt.AlignmentFlag.AlignCenter)

        return row, label, buttons, note

    def _build_hero(self) -> QWidget:
        """Create the welcome card shown when starting a new mission."""
        hero = QFrame()
        hero.setObjectName("MissionHero")
        layout = QVBoxLayout(hero)
        layout.setContentsMargins(28, 28, 28, 40)
        layout.setSpacing(12)

        # Humbug's logo arrives on stage, once, to open the page.
        self._intro = MissionIntro()
        layout.addWidget(self._intro)
        layout.addSpacing(12)

        self._hero_title = QLabel()
        self._hero_title.setObjectName("MissionHeroTitle")
        self._hero_title.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self._hero_title.setWordWrap(True)
        layout.addWidget(self._hero_title)

        self._hero_subtitle = QLabel()
        self._hero_subtitle.setObjectName("MissionHeroSubtitle")
        self._hero_subtitle.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self._hero_subtitle.setWordWrap(True)
        layout.addWidget(self._hero_subtitle)
        (
            self._guide, self._guide_title, self._guide_steps, self._guide_steps_layout, self._guide_dismiss
        ) = self._build_guide()
        layout.addWidget(self._guide)
        self._guide_link = QPushButton()
        self._guide_link.setObjectName("MissionLinkButton")
        self._guide_link.setCursor(Qt.CursorShape.PointingHandCursor)
        self._guide_link.clicked.connect(lambda: self.guide_dismissed.emit(False))
        layout.addWidget(self._guide_link, 0, Qt.AlignmentFlag.AlignHCenter)

        # The level is chosen before the mission starts, so even the lead's first request is covered.
        (
            self._hero_auto_approve_row, self._hero_auto_approve_label, self._hero_auto_approve_buttons,
            self._hero_auto_approve_note
        ) = self._auto_approve_controls(self.new_mission_auto_approve_selected.emit, centred=True)
        layout.addWidget(self._hero_auto_approve_row)
        layout.addWidget(self._hero_auto_approve_note)
        self._remember_level_check = QCheckBox()
        self._remember_level_check.setObjectName("MissionReviewPlan")
        self._remember_level_check.setCursor(Qt.CursorShape.PointingHandCursor)
        self._remember_level_check.clicked.connect(self.new_mission_level_remembered)
        layout.addWidget(self._remember_level_check, 0, Qt.AlignmentFlag.AlignHCenter)
        self._review_plan_check = QCheckBox()
        self._review_plan_check.setObjectName("MissionReviewPlan")
        self._review_plan_check.setCursor(Qt.CursorShape.PointingHandCursor)
        self._review_plan_check.toggled.connect(self.new_mission_review_plan_toggled)
        layout.addWidget(self._review_plan_check, 0, Qt.AlignmentFlag.AlignHCenter)
        # Done checks are optional, so their box only appears once asked for.
        self._checks_option = QCheckBox()
        self._checks_option.setObjectName("MissionReviewPlan")
        self._checks_option.setCursor(Qt.CursorShape.PointingHandCursor)
        self._checks_option.toggled.connect(self._show_checks_editor)
        layout.addWidget(self._checks_option, 0, Qt.AlignmentFlag.AlignHCenter)
        self._checks_editor = QWidget()
        checks_layout = QVBoxLayout(self._checks_editor)
        checks_layout.setContentsMargins(0, 0, 0, 0)
        checks_layout.setSpacing(4)
        self._checks_input = QPlainTextEdit()
        self._checks_input.setObjectName("MissionChecksInput")
        self._checks_input.setTabChangesFocus(True)
        checks_layout.addWidget(self._checks_input)
        self._checks_explain = QLabel()
        self._checks_explain.setObjectName("MissionHintText")
        self._checks_explain.setWordWrap(True)
        checks_layout.addWidget(self._checks_explain)
        self._checks_editor.hide()
        layout.addWidget(self._checks_editor)

        layout.addSpacing(8)
        self._examples_label = QLabel()
        self._examples_label.setObjectName("MissionExamplesLabel")
        self._examples_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(self._examples_label)

        self._example_buttons: list[QPushButton] = []
        for _ in range(3):
            button = QPushButton()
            button.setObjectName("MissionExampleChip")
            button.setCursor(Qt.CursorShape.PointingHandCursor)
            button.clicked.connect(lambda _checked=False, b=button: self._use_example(b.text()))
            layout.addWidget(button, 0, Qt.AlignmentFlag.AlignHCenter)
            self._example_buttons.append(button)

        # The user's own saved requests, to start the missions they run again and again.
        layout.addSpacing(8)
        self._templates_label = QLabel()
        self._templates_label.setObjectName("MissionExamplesLabel")
        self._templates_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(self._templates_label)
        self._templates_layout = QVBoxLayout()
        self._templates_layout.setSpacing(8)
        layout.addLayout(self._templates_layout)
        self._save_template_button = QPushButton()
        self._save_template_button.setObjectName("MissionLinkButton")
        self._save_template_button.setCursor(Qt.CursorShape.PointingHandCursor)
        self._save_template_button.clicked.connect(
            lambda: self.template_save_requested.emit(self._input.toPlainText().strip())
        )
        layout.addWidget(self._save_template_button, 0, Qt.AlignmentFlag.AlignHCenter)
        self._templates_label.hide()
        return hero

    def _build_guide(self) -> tuple[QFrame, QLabel, list[tuple[QLabel, QLabel]], QBoxLayout, QPushButton]:
        """
        Create the short guide to how Mission Control works, for someone using it for the first time.

        Returns:
            The guide, its title, each step's heading and words, the layout of its steps, and its dismiss button
        """
        guide = QFrame()
        guide.setObjectName("MissionGuide")
        layout = QVBoxLayout(guide)
        layout.setContentsMargins(20, 16, 20, 16)
        layout.setSpacing(10)
        title = QLabel()
        title.setObjectName("MissionCardTitle")
        layout.addWidget(title)
        steps = QBoxLayout(QBoxLayout.Direction.LeftToRight)
        steps.setSpacing(18)
        step_texts: list[tuple[QLabel, QLabel]] = []
        for number in range(1, 4):
            step = QHBoxLayout()
            step.setSpacing(10)
            step.addWidget(GlyphBadge(str(number), MissionTone.ACCENT, 28), 0, Qt.AlignmentFlag.AlignTop)
            texts = QVBoxLayout()
            texts.setSpacing(2)
            heading = QLabel()
            heading.setObjectName("MissionStepsLabel")
            heading.setWordWrap(True)
            texts.addWidget(heading)
            body = QLabel()
            body.setObjectName("MissionHintText")
            body.setWordWrap(True)
            body.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
            texts.addWidget(body)
            step.addLayout(texts, 1)
            steps.addLayout(step, 1)
            step_texts.append((heading, body))

        layout.addLayout(steps)
        dismiss_row = QHBoxLayout()
        dismiss_row.addStretch(1)
        dismiss = QPushButton()
        dismiss.setObjectName("MissionReplyButton")
        dismiss.setCursor(Qt.CursorShape.PointingHandCursor)
        dismiss.clicked.connect(lambda: self.guide_dismissed.emit(True))
        dismiss_row.addWidget(dismiss)
        layout.addLayout(dismiss_row)
        return guide, title, step_texts, steps, dismiss

    def _show_checks_editor(self, shown: bool) -> None:
        """Show the box for done checks when the user asks to add some, and hide it again when they do not."""
        self._checks_editor.setVisible(shown)
        if shown:
            self._checks_input.setFocus()

    def done_checks(self) -> list[str]:
        """Return the done checks the user wrote for the next mission, one per line, or none if they chose none."""
        if not self._checks_option.isChecked():
            return []

        return [line.strip() for line in self._checks_input.toPlainText().splitlines() if line.strip()]

    def clear_done_checks(self) -> None:
        """Forget the done checks once a mission has started with them, so the next one starts without."""
        self._checks_input.clear()
        self._checks_option.setChecked(False)

    def set_guide_visible(self, visible: bool) -> None:
        """Show the guide to how Mission Control works, or the link that brings it back."""
        self._guide.setVisible(visible)
        self._guide_link.setVisible(not visible)

    def set_templates(self, templates: list[tuple[str, str]]) -> None:
        """
        Show the requests the user saved, each starting a mission's request when chosen.

        Args:
            templates: (name, request) pairs, in the order they were saved
        """
        if templates == self._templates:
            return

        self._templates = templates
        _clear_layout(self._templates_layout)
        self._templates_label.setVisible(bool(templates))
        strings = self._language_manager.strings()
        for name, request in templates:
            row = QHBoxLayout()
            row.setSpacing(4)
            row.addStretch(1)
            chip = QPushButton("☆  " + name)
            chip.setObjectName("MissionExampleChip")
            chip.setCursor(Qt.CursorShape.PointingHandCursor)
            chip.setToolTip(request)
            chip.clicked.connect(lambda _checked=False, r=request: self._use_example(r))
            row.addWidget(chip)
            remove = QToolButton()
            remove.setObjectName("MissionTemplateRemove")
            remove.setText("✕")
            remove.setCursor(Qt.CursorShape.PointingHandCursor)
            remove.setToolTip(strings.mission_template_delete.format(name))
            remove.setAccessibleName(strings.mission_template_delete.format(name))
            remove.clicked.connect(lambda _checked=False, n=name: self.template_delete_requested.emit(n))
            row.addWidget(remove)
            row.addStretch(1)
            self._templates_layout.addLayout(row)

    def _card(self, name: str = "MissionCard") -> tuple[QFrame, QVBoxLayout]:
        """Create an empty card."""
        card = QFrame()
        card.setObjectName(name)
        layout = QVBoxLayout(card)
        layout.setContentsMargins(18, 16, 18, 16)
        layout.setSpacing(10)
        return card, layout

    def _card_header(self, glyph: str, tone: MissionTone) -> tuple[QHBoxLayout, QLabel, StatusPill]:
        """Create a card heading with a badge, a title and a count pill."""
        header = QHBoxLayout()
        header.setSpacing(10)
        header.addWidget(GlyphBadge(glyph, tone, 26))
        title = QLabel()
        title.setObjectName("MissionCardTitle")
        header.addWidget(title)
        header.addStretch(1)
        pill = StatusPill()
        self._pills.append(pill)
        header.addWidget(pill)
        return header, title, pill

    def _build_glance(self) -> QWidget:
        """Create the facts shown under the mission's title: what it is doing now and how far it has got."""
        # A frame, so the line setting the facts apart from the title is drawn.
        glance = QFrame()
        glance.setObjectName("MissionGlance")
        row = QBoxLayout(QBoxLayout.Direction.LeftToRight, glance)
        row.setContentsMargins(14, 10, 14, 10)
        row.setSpacing(18)
        self._glance_row = row
        self._glance_cells: dict[str, tuple[QWidget, GlyphBadge, QLabel, _WrapLabel]] = {}
        for key, glyph in _GLANCE_GLYPHS.items():
            cell = QWidget()
            cell_layout = QHBoxLayout(cell)
            cell_layout.setContentsMargins(0, 0, 0, 0)
            cell_layout.setSpacing(10)
            badge = GlyphBadge(glyph, MissionTone.NEUTRAL, 24)
            cell_layout.addWidget(badge, 0, Qt.AlignmentFlag.AlignTop)
            texts = QVBoxLayout()
            texts.setSpacing(2)
            caption = QLabel()
            caption.setObjectName("MissionGlanceCaption")
            texts.addWidget(caption)
            value = _WrapLabel()
            value.setObjectName("MissionGlanceValue")
            _ignore_width(value)
            texts.addWidget(value)
            cell_layout.addLayout(texts, 1)
            row.addWidget(cell, 1)
            self._glance_cells[key] = (cell, badge, caption, value)

        glance.hide()
        return glance

    def _set_glance(self, glance: MissionGlanceView | None) -> None:
        """Show the mission at a glance, leaving out a fact that has nothing to say."""
        cells = (
            ("now", glance.now_text, glance.now_tone),
            ("progress", glance.progress_text, MissionTone.SUCCESS),
        ) if glance is not None else ()
        self._glance.setVisible(any(text for _key, text, _tone in cells))
        for key, text, tone in cells:
            cell, badge, _caption, value = self._glance_cells[key]
            cell.setVisible(bool(text))
            badge.set_badge(_GLANCE_GLYPHS[key], tone)
            value.setText(text)

    def _details(self, visible: bool) -> bool:
        """Return whether a detail card that would otherwise be visible is, given the details switch."""
        return visible and (self._completed or self._details_shown or not self._glance_shown)

    def _toggle_details(self) -> None:
        """Fold the details below the mission at a glance away, or bring them back."""
        self._details_shown = not self._details_shown
        self._update_details_button()
        if self._view is not None:
            self.set_mission(self._view)

    def _update_details_button(self) -> None:
        """Label the details switch for what it will do."""
        strings = self._language_manager.strings()
        self._details_button.setText(
            ("▲  " + strings.mission_details_hide) if self._details_shown else ("▼  " + strings.mission_details_show)
        )

    def _build_dashboard(self) -> QWidget:
        """Create the view of the selected mission."""
        dashboard = QWidget()
        layout = QVBoxLayout(dashboard)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(16)

        self._header_card = QFrame()
        self._header_card.setObjectName("MissionHeaderCard")
        header_layout = QHBoxLayout(self._header_card)
        header_layout.setContentsMargins(26, 24, 26, 24)
        header_layout.setSpacing(18)

        text_column = QVBoxLayout()
        text_column.setSpacing(8)

        # Status and title on the left, a ring showing how much of the plan is done on the
        # right, so the mission's state is read together with its name at any width.
        title_row = QHBoxLayout()
        title_row.setSpacing(18)
        titles = QVBoxLayout()
        titles.setSpacing(6)
        status_row = QHBoxLayout()
        status_row.setSpacing(10)
        self._status_pill = StatusPill()
        self._pills.append(self._status_pill)
        status_row.addWidget(self._status_pill)
        self._timing_label = QLabel()
        self._timing_label.setObjectName("MissionTimingText")
        status_row.addWidget(self._timing_label)
        self._tokens_label = QLabel()
        self._tokens_label.setObjectName("MissionTimingText")
        status_row.addWidget(self._tokens_label)
        status_row.addStretch(1)
        titles.addLayout(status_row)
        self._title = QLabel()
        self._title.setObjectName("MissionTitle")
        self._title.setWordWrap(True)
        self._title.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
        self._title.setTextInteractionFlags(Qt.TextInteractionFlag.TextSelectableByMouse)
        titles.addWidget(self._title)
        self._summary = QLabel()
        self._summary.setObjectName("MissionSummary")
        self._summary.setWordWrap(True)
        self._summary.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
        self._summary.setTextInteractionFlags(Qt.TextInteractionFlag.TextSelectableByMouse)
        titles.addWidget(self._summary)
        title_row.addLayout(titles, 1)
        self._progress_box = QWidget()
        progress_layout = QHBoxLayout(self._progress_box)
        progress_layout.setContentsMargins(0, 0, 0, 0)
        progress_layout.setSpacing(14)
        self._progress_ring = ProgressRing(72)
        progress_layout.addWidget(self._progress_ring)
        progress_texts = QVBoxLayout()
        progress_texts.setSpacing(2)
        progress_texts.addStretch(1)
        self._steps_label = QLabel()
        self._steps_label.setObjectName("MissionStepsLabel")
        progress_texts.addWidget(self._steps_label)
        self._progress_label = QLabel()
        self._progress_label.setObjectName("MissionProgressLabel")
        self._progress_label.setWordWrap(True)
        self._progress_label.setMaximumWidth(260)
        progress_texts.addWidget(self._progress_label)
        progress_texts.addStretch(1)
        progress_layout.addLayout(progress_texts)
        title_row.addWidget(self._progress_box, 0, Qt.AlignmentFlag.AlignVCenter)
        text_column.addLayout(title_row)

        # Where the mission is on its way from the user's request to the AI's report.
        self._journey = MissionJourney()
        text_column.addWidget(self._journey)

        # What is happening now and how far the mission has got, read together with its name.
        text_column.addWidget(self._glance)

        text_column.addSpacing(4)
        self._interrupted_notice = QLabel()
        self._interrupted_notice.setObjectName("MissionNotice")
        self._interrupted_notice.setWordWrap(True)
        self._interrupted_notice.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
        text_column.addWidget(self._interrupted_notice)
        self._failed_notice = QLabel()
        self._failed_notice.setObjectName("MissionFailedNotice")
        self._failed_notice.setWordWrap(True)
        self._failed_notice.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
        text_column.addWidget(self._failed_notice)
        self._paused_notice = QLabel()
        self._paused_notice.setObjectName("MissionNotice")
        self._paused_notice.setWordWrap(True)
        self._paused_notice.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
        text_column.addWidget(self._paused_notice)
        self._recovery_notice = QLabel()
        self._recovery_notice.setObjectName("MissionNotice")
        self._recovery_notice.setWordWrap(True)
        self._recovery_notice.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
        text_column.addWidget(self._recovery_notice)
        self._stuck_notice = QLabel()
        self._stuck_notice.setObjectName("MissionNotice")
        self._stuck_notice.setWordWrap(True)
        self._stuck_notice.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
        text_column.addWidget(self._stuck_notice)
        self._plan_ready_notice = QLabel()
        self._plan_ready_notice.setObjectName("MissionNotice")
        self._plan_ready_notice.setWordWrap(True)
        self._plan_ready_notice.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
        text_column.addWidget(self._plan_ready_notice)

        actions = QHBoxLayout()
        actions.setSpacing(8)
        actions.setContentsMargins(0, 6, 0, 0)
        self._resume_button = QPushButton()
        self._resume_button.setObjectName("MissionReplyButton")
        self._resume_button.setCursor(Qt.CursorShape.PointingHandCursor)
        self._resume_button.clicked.connect(self.resume_requested)
        actions.addWidget(self._resume_button)
        self._retry_button = QPushButton()
        self._retry_button.setObjectName("MissionReplyButton")
        self._retry_button.setCursor(Qt.CursorShape.PointingHandCursor)
        self._retry_button.clicked.connect(self.retry_requested)
        actions.addWidget(self._retry_button)
        self._lead_conversation_button = QPushButton()
        self._lead_conversation_button.setObjectName("MissionReplyButton")
        self._lead_conversation_button.setCursor(Qt.CursorShape.PointingHandCursor)
        self._lead_conversation_button.clicked.connect(self._open_lead_conversation)
        actions.addWidget(self._lead_conversation_button)
        self._unpause_button = QPushButton()
        self._unpause_button.setObjectName("MissionReplyButton")
        self._unpause_button.setCursor(Qt.CursorShape.PointingHandCursor)
        self._unpause_button.clicked.connect(self.unpause_requested)
        actions.addWidget(self._unpause_button)
        self._carry_on_button = QPushButton()
        self._carry_on_button.setObjectName("MissionReplyButton")
        self._carry_on_button.setCursor(Qt.CursorShape.PointingHandCursor)
        self._carry_on_button.clicked.connect(self.carry_on_requested)
        actions.addWidget(self._carry_on_button)
        self._start_work_button = QPushButton()
        self._start_work_button.setObjectName("MissionReplyButton")
        self._start_work_button.setCursor(Qt.CursorShape.PointingHandCursor)
        self._start_work_button.clicked.connect(self.start_work_requested)
        actions.addWidget(self._start_work_button)
        self._pause_button = QPushButton()
        self._pause_button.setObjectName("MissionPauseButton")
        self._pause_button.setCursor(Qt.CursorShape.PointingHandCursor)
        self._pause_button.clicked.connect(self.pause_requested)
        actions.addWidget(self._pause_button)
        self._stop_button = QPushButton()
        self._stop_button.setObjectName("MissionStopButton")
        self._stop_button.setCursor(Qt.CursorShape.PointingHandCursor)
        self._stop_button.clicked.connect(self.stop_requested)
        actions.addWidget(self._stop_button)
        # Switching mode and deleting are rarer, and deleting is not to be hit by accident beside
        # Stop, so they wait behind a menu.
        self._more_button = QPushButton("⋯")
        self._more_button.setObjectName("MissionPauseButton")
        self._more_button.setCursor(Qt.CursorShape.PointingHandCursor)
        self._more_button.clicked.connect(lambda: self._show_more_actions(self._more_button))
        actions.addWidget(self._more_button)
        self._can_switch_to_classic = True
        actions.addStretch(1)
        # The details below the header fold away, leaving the mission at a glance.
        self._details_button = QPushButton()
        self._details_button.setObjectName("MissionLinkButton")
        self._details_button.setCursor(Qt.CursorShape.PointingHandCursor)
        self._details_button.clicked.connect(self._toggle_details)
        actions.addWidget(self._details_button)
        text_column.addLayout(actions)

        (
            self._auto_approve_row, self._auto_approve_label, self._auto_approve_buttons, self._auto_approve_note
        ) = self._auto_approve_controls(self._on_auto_approve_clicked)
        text_column.addWidget(self._auto_approve_row)
        text_column.addWidget(self._auto_approve_note)
        header_layout.addLayout(text_column, 1)

        # A call to action when something waits for the user, so it is never missed below the fold.
        self._attention = QFrame()
        self._attention.setObjectName("MissionAttention")
        attention_layout = QHBoxLayout(self._attention)
        attention_layout.setContentsMargins(16, 10, 10, 10)
        attention_layout.setSpacing(12)
        attention_layout.addWidget(GlyphBadge("!", MissionTone.WARNING, 28))
        self._attention_text = QLabel()
        self._attention_text.setObjectName("MissionAttentionText")
        self._attention_text.setWordWrap(True)
        self._attention_text.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
        attention_layout.addWidget(self._attention_text, 1)
        self._attention_button = QPushButton()
        self._attention_button.setObjectName("MissionReplyButton")
        self._attention_button.setCursor(Qt.CursorShape.PointingHandCursor)
        self._attention_button.clicked.connect(self._review_first_decision)
        attention_layout.addWidget(self._attention_button)
        self._attention.hide()

        # Says the lead is working on what the user just wrote, such as a follow-up to a finished mission.
        self._working_on = QFrame()
        self._working_on.setObjectName("MissionWorkingOn")
        working_layout = QHBoxLayout(self._working_on)
        working_layout.setContentsMargins(16, 10, 12, 10)
        working_layout.setSpacing(12)
        working_layout.addWidget(GlyphBadge("✎", MissionTone.ACCENT, 28))
        self._working_on_text = _WrapLabel()
        self._working_on_text.setObjectName("MissionWorkingOnText")
        working_layout.addWidget(self._working_on_text, 1)
        self._working_on_pill = StatusPill()
        working_layout.addWidget(self._working_on_pill)
        self._working_on.hide()
        layout.addWidget(self._working_on)

        layout.addWidget(self._report_card)
        layout.addWidget(self._header_card)
        layout.addWidget(self._results_card)
        layout.addWidget(self._lead_card)
        layout.addWidget(self._classic_card)

        self._lower = QBoxLayout(QBoxLayout.Direction.LeftToRight)
        self._lower.setSpacing(16)

        self._plan_card, plan_layout = self._card()
        header, self._plan_title, plan_pill = self._card_header("≡", MissionTone.ACCENT)
        # The filters say how many steps there are and how many are done, so no count is needed.
        plan_pill.hide()
        self._plan_filters = QWidget()
        self._plan_filters_layout = QHBoxLayout(self._plan_filters)
        self._plan_filters_layout.setContentsMargins(0, 0, 0, 0)
        self._plan_filters_layout.setSpacing(6)
        header.insertWidget(header.count() - 1, self._plan_filters)
        self._history_button = QPushButton()
        self._history_button.setObjectName("MissionHistoryButton")
        self._history_button.setCheckable(True)
        self._history_button.setCursor(Qt.CursorShape.PointingHandCursor)
        self._history_button.clicked.connect(self._toggle_history)
        self._history_button.hide()
        header.insertWidget(header.count() - 1, self._history_button)
        plan_layout.addLayout(header)
        self._plan_hint = QLabel()
        self._plan_hint.setObjectName("MissionHintText")
        plan_layout.addWidget(self._plan_hint)
        self._plan_empty = QLabel()
        self._plan_empty.setObjectName("MissionEmptyText")
        self._plan_empty.setWordWrap(True)
        plan_layout.addWidget(self._plan_empty)
        self._steps_layout = QVBoxLayout()
        self._steps_layout.setSpacing(0)
        plan_layout.addLayout(self._steps_layout)
        self._history_panel = QFrame()
        self._history_panel.setObjectName("MissionHistoryPanel")
        history_panel_layout = QVBoxLayout(self._history_panel)
        history_panel_layout.setContentsMargins(12, 10, 12, 10)
        history_panel_layout.setSpacing(6)
        self._history_title = QLabel()
        self._history_title.setObjectName("MissionStepSection")
        history_panel_layout.addWidget(self._history_title)
        self._history_layout = QVBoxLayout()
        self._history_layout.setSpacing(6)
        history_panel_layout.addLayout(self._history_layout)
        self._history_panel.hide()
        plan_layout.addWidget(self._history_panel)
        plan_layout.addStretch(1)
        self._lower.addWidget(self._plan_card, 3)

        self._agents_card, agents_layout = self._card()
        self._agents_card.installEventFilter(self)
        self._scroll.verticalScrollBar().rangeChanged.connect(lambda _low, _high: self._keep_hold())
        header, self._agents_title, self._agents_pill = self._card_header("◆", MissionTone.INFO)
        agents_layout.addLayout(header)
        self._agents_empty = QLabel()
        self._agents_empty.setObjectName("MissionEmptyText")
        self._agents_empty.setWordWrap(True)
        agents_layout.addWidget(self._agents_empty)
        self._agent_network = AgentNetworkView()
        self._agent_network.agent_clicked.connect(self.conversation_requested)
        self._agent_network.decision_approved.connect(lambda d: self._answer_in_place(self.decision_approved, d))
        self._agent_network.decision_unsure.connect(lambda d: self._answer_in_place(self.decision_unsure, d))
        self._agent_network.decision_rejected.connect(lambda d: self._answer_in_place(self.decision_rejected, d))
        self._agent_network.decision_details_requested.connect(self.show_decision)
        self._agent_network.wait_finish_requested.connect(
            lambda c: self._answer_in_place(self.wait_finish_requested, c)
        )
        self._agent_network.plan_accepted.connect(self.start_work_requested)
        self._agent_network.plan_declined.connect(self.focus_composer)
        agents_layout.addWidget(self._agent_network)
        # The same agents as a list, for reading their states at a glance; folded away by default
        # because the picture above already shows them.
        self._agents_list_button = QPushButton()
        self._agents_list_button.setObjectName("MissionLinkButton")
        self._agents_list_button.setCursor(Qt.CursorShape.PointingHandCursor)
        self._agents_list_button.clicked.connect(self._toggle_agents_list)
        agents_layout.addWidget(self._agents_list_button, 0, Qt.AlignmentFlag.AlignLeft)
        self._agents_list = QWidget()
        self._agents_layout = QVBoxLayout(self._agents_list)
        self._agents_layout.setContentsMargins(0, 0, 0, 0)
        self._agents_layout.setSpacing(8)
        self._agents_list.hide()
        agents_layout.addWidget(self._agents_list)
        agents_layout.addStretch(1)

        self._lower_container = QWidget()
        self._lower_container.setLayout(self._lower)
        self._lower.setContentsMargins(0, 0, 0, 0)
        # The team at work comes first, across the full width, so the user sees it working for them;
        # the plan it is carrying out follows.
        layout.addWidget(self._agents_card)
        layout.addWidget(self._lower_container)

        # The user's done checks, and what the lead found when it last ran them.
        self._checks_card, checks_layout = self._card()
        header, self._checks_title, self._checks_pill = self._card_header("✓", MissionTone.SUCCESS)
        checks_layout.addLayout(header)
        self._checks_hint = QLabel()
        self._checks_hint.setObjectName("MissionHintText")
        self._checks_hint.setWordWrap(True)
        checks_layout.addWidget(self._checks_hint)
        self._checks_layout = QVBoxLayout()
        self._checks_layout.setSpacing(4)
        checks_layout.addLayout(self._checks_layout)
        self._checks_card.hide()
        # They decide whether the mission is done, so they sit straight under the mission itself.
        layout.insertWidget(layout.indexOf(self._header_card) + 1, self._checks_card)

        # What the mission did to the user's files, each change a click from being undone.
        self._files_card, files_layout = self._card()
        header, self._files_title, self._files_pill = self._card_header("✎", MissionTone.ACCENT)
        self._files_undo_all = QPushButton()
        self._files_undo_all.setObjectName("MissionLinkButton")
        self._files_undo_all.setCursor(Qt.CursorShape.PointingHandCursor)
        self._files_undo_all.clicked.connect(lambda: self.file_undo_requested.emit(""))
        header.insertWidget(header.count() - 1, self._files_undo_all)
        files_layout.addLayout(header)
        self._files_summary = QLabel()
        self._files_summary.setObjectName("MissionStepsLabel")
        self._files_summary.setWordWrap(True)
        files_layout.addWidget(self._files_summary)
        self._files_hint = QLabel()
        self._files_hint.setObjectName("MissionHintText")
        self._files_hint.setWordWrap(True)
        files_layout.addWidget(self._files_hint)
        self._files_layout = QVBoxLayout()
        self._files_layout.setSpacing(4)
        files_layout.addLayout(self._files_layout)
        self._files_card.hide()
        layout.addWidget(self._files_card)
        return dashboard

    def _build_chat_card(self) -> QWidget:
        """Create the card showing the conversation with the lead, as a conversation tab would, without its input."""
        card, layout = self._card()
        header, self._chat_title, self._chat_pill = self._card_header("✉", MissionTone.ACCENT)
        layout.addLayout(header)
        self._chat_hint = QLabel()
        self._chat_hint.setObjectName("MissionHintText")
        self._chat_hint.setWordWrap(True)
        layout.addWidget(self._chat_hint)
        self._chat_earlier_button = QPushButton()
        self._chat_earlier_button.setObjectName("MissionLinkButton")
        self._chat_earlier_button.setCursor(Qt.CursorShape.PointingHandCursor)
        self._chat_earlier_button.clicked.connect(self._show_earlier_chat)
        self._chat_earlier_button.hide()
        layout.addWidget(self._chat_earlier_button, 0, Qt.AlignmentFlag.AlignLeft)
        feed = QWidget()
        feed.setObjectName("MissionChatFeed")
        self._chat_layout = QVBoxLayout(feed)
        self._chat_layout.setContentsMargins(0, 0, 8, 0)
        self._chat_layout.setSpacing(10)
        self._chat_layout.setAlignment(Qt.AlignmentFlag.AlignTop)
        self._chat_scroll = _FittedScroll(feed)
        self._chat_scroll.setObjectName("MissionChatScroll")
        layout.addWidget(self._chat_scroll)
        card.hide()
        return card

    def _set_conversation(self, conversation: list[MissionChatView]) -> None:
        """Show the user's messages and the lead's replies, with the tools the lead used between them."""
        self._chat_card.setVisible(self._details(bool(conversation)))
        self._chat_hint.setVisible(bool(conversation))
        self._chat_pill.set_pill(str(sum(1 for entry in conversation if entry.kind == "user")), MissionTone.ACCENT)
        if conversation == self._conversation:
            return

        if self._conversation is None:
            # Everything is drawn afresh, for example after a style or language change.
            _clear_layout(self._chat_layout)
            self._chat_rows = []

        if self._conversation is None or conversation[:1] != self._conversation[:1]:
            # A different conversation, such as another mission's, starts again from its latest page.
            self._chat_shown = _CHAT_PAGE

        self._conversation = conversation
        self._show_chat_rows()

    def _show_chat_rows(self) -> None:
        """Show the latest entries of the conversation, as many as the user has asked to see."""
        conversation = self._conversation or []
        shown = conversation[-self._chat_shown:]
        self._chat_rows = _sync_rows(
            self._chat_layout, self._chat_rows, list(shown), lambda index: self._chat_row(shown[index])
        )
        hidden = len(conversation) - len(shown)
        self._chat_earlier_button.setText(self._language_manager.strings().mission_chat_show_earlier.format(hidden))
        self._chat_earlier_button.setVisible(hidden > 0)

    def _show_earlier_chat(self) -> None:
        """Show another page of earlier entries, and bring them into view."""
        self._chat_shown += _CHAT_PAGE
        self._chat_scroll.show_top_next()
        self._show_chat_rows()

    def _chat_row(self, entry: MissionChatView) -> QWidget:
        """Create the row showing one entry in the conversation with the lead."""
        if entry.kind == "tool":
            row = QWidget()
            row_layout = QHBoxLayout(row)
            row_layout.setContentsMargins(14, 0, 4, 0)
            row_layout.setSpacing(8)
            used = _WrapLabel(f"⚙  {entry.text}")
            used.setObjectName("MissionHintText")
            _ignore_width(used)
            row_layout.addWidget(used, 1)
            when = QLabel(entry.time_text)
            when.setObjectName("MissionTimeText")
            row_layout.addWidget(when, 0, Qt.AlignmentFlag.AlignTop)
            return row

        bubble = QFrame()
        bubble.setObjectName("MissionChatUser" if entry.kind == "user" else "MissionChatLead")
        bubble_layout = QVBoxLayout(bubble)
        bubble_layout.setContentsMargins(14, 10, 14, 12)
        bubble_layout.setSpacing(6)
        heading = QHBoxLayout()
        name = QLabel(entry.speaker)
        name.setObjectName("MissionResultHeading")
        heading.addWidget(name)
        if entry.model_text:
            # Elided, so a long model name ends in an ellipsis rather than being cut mid-word.
            model = ElidedLabel()
            model.setObjectName("MissionChatModel")
            model.setText(entry.model_text)
            model.setToolTip(entry.model_text)
            heading.addWidget(model)

        heading.addStretch(1)
        when = QLabel(entry.time_text)
        when.setObjectName("MissionTimeText")
        heading.addWidget(when)
        bubble_layout.addLayout(heading)
        # The label keeps the height its text needs, so a crowded layout can never squeeze a reply
        # into a single clipped line. The text format is set first, as the height depends on it.
        body = _WrapLabel()
        body.setObjectName("MissionLeadText")
        # The lead writes Markdown; what the user typed is shown exactly as they typed it.
        body.setTextFormat(Qt.TextFormat.MarkdownText if entry.kind == "lead" else Qt.TextFormat.PlainText)
        body.setText(entry.text)
        body.setTextInteractionFlags(Qt.TextInteractionFlag.TextSelectableByMouse)
        _ignore_width(body)
        bubble_layout.addWidget(body)
        if entry.kind != "user":
            return bubble

        # The user's messages sit to one side, as in a conversation tab.
        row = QWidget()
        row_layout = QHBoxLayout(row)
        row_layout.setContentsMargins(0, 0, 0, 0)
        row_layout.addSpacing(32)
        row_layout.addWidget(bubble, 1)
        return row

    def _build_lead_card(self) -> QWidget:
        """Create the card showing the lead's latest message and where to reply."""
        lead_card = QFrame()
        lead_card.setObjectName("MissionLeadCard")
        layout = QVBoxLayout(lead_card)
        layout.setContentsMargins(18, 16, 18, 16)
        layout.setSpacing(10)

        header = QHBoxLayout()
        header.setSpacing(10)
        header.addWidget(GlyphBadge("★", MissionTone.ACCENT, 26))
        self._lead_title = QLabel()
        self._lead_title.setObjectName("MissionCardTitle")
        header.addWidget(self._lead_title)
        self._lead_pill = StatusPill()
        self._pills.append(self._lead_pill)
        header.addWidget(self._lead_pill)
        header.addStretch(1)
        self._lead_time = QLabel()
        self._lead_time.setObjectName("MissionTimeText")
        header.addWidget(self._lead_time)
        layout.addLayout(header)

        self._lead_body = _WrapLabel()
        self._lead_body.setObjectName("MissionLeadText")
        self._lead_body.setTextFormat(Qt.TextFormat.MarkdownText)
        self._lead_body.setTextInteractionFlags(Qt.TextInteractionFlag.TextSelectableByMouse)
        self._lead_body.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
        layout.addWidget(self._lead_body)

        actions = QHBoxLayout()
        actions.setSpacing(8)
        self._reply_button = QPushButton()
        self._reply_button.setObjectName("MissionReplyButton")
        self._reply_button.setCursor(Qt.CursorShape.PointingHandCursor)
        self._reply_button.clicked.connect(self.focus_composer)
        actions.addWidget(self._reply_button)
        actions.addStretch(1)
        layout.addLayout(actions)
        return lead_card

    def _build_report_card(self) -> QWidget:
        """Create the card that presents the AI's final report once the mission is done."""
        card = QFrame()
        card.setObjectName("MissionReportCard")
        layout = QVBoxLayout(card)
        layout.setContentsMargins(28, 24, 28, 26)
        layout.setSpacing(14)

        # The headline sits in a row of its own, with what to do next under it, so the report
        # below has the card's whole width.
        self._report_header = QWidget()
        header = QHBoxLayout(self._report_header)
        header.setContentsMargins(0, 0, 0, 0)
        header.setSpacing(16)
        self._report_badge = GlyphBadge("✓", MissionTone.SUCCESS, 48)
        header.addWidget(self._report_badge, 0, Qt.AlignmentFlag.AlignTop)
        column = QVBoxLayout()
        column.setSpacing(4)
        self._report_caption = QLabel()
        self._report_caption.setObjectName("MissionReportCaption")
        column.addWidget(self._report_caption)
        self._report_title = QLabel()
        self._report_title.setObjectName("MissionReportTitle")
        self._report_title.setWordWrap(True)
        self._report_title.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
        column.addWidget(self._report_title)
        self._report_stats = QLabel()
        self._report_stats.setObjectName("MissionReportStats")
        self._report_stats.setWordWrap(True)
        column.addWidget(self._report_stats)
        header.addLayout(column, 1)

        # The buttons wrap onto more rows rather than hold the page wider than the window.
        actions = _FlowLayout(8)
        actions.setContentsMargins(0, 10, 0, 0)
        # The report is what the user came for, so taking it elsewhere is one click.
        self._report_copy_button = QPushButton()
        self._report_copy_button.setObjectName("MissionPauseButton")
        self._report_copy_button.setCursor(Qt.CursorShape.PointingHandCursor)
        self._report_copy_button.clicked.connect(self._copy_report)
        actions.addWidget(self._report_copy_button)
        self._report_copied_timer = QTimer(self)
        self._report_copied_timer.setSingleShot(True)
        self._report_copied_timer.setInterval(_COPIED_MS)
        self._report_copied_timer.timeout.connect(self._update_report_copy_button)
        self._report_new_button = QPushButton()
        self._report_new_button.setObjectName("MissionPauseButton")
        self._report_new_button.setCursor(Qt.CursorShape.PointingHandCursor)
        self._report_new_button.clicked.connect(self.new_mission_requested)
        actions.addWidget(self._report_new_button)
        self._follow_up_button = QPushButton()
        self._follow_up_button.setObjectName("MissionReplyButton")
        self._follow_up_button.setCursor(Qt.CursorShape.PointingHandCursor)
        self._follow_up_button.clicked.connect(self.focus_composer)
        actions.addWidget(self._follow_up_button)
        # The mission card is put away once the mission is done, so its rarer actions are offered here.
        self._report_more_button = QPushButton("⋯")
        self._report_more_button.setObjectName("MissionPauseButton")
        self._report_more_button.setCursor(Qt.CursorShape.PointingHandCursor)
        self._report_more_button.clicked.connect(self._show_report_actions)
        actions.addWidget(self._report_more_button)
        column.addLayout(actions)
        layout.addWidget(self._report_header)

        divider = QFrame()
        divider.setObjectName("MissionReportDivider")
        divider.setFixedHeight(1)
        layout.addWidget(divider)

        self._report_section = QLabel()
        self._report_section.setObjectName("MissionReportSection")
        layout.addWidget(self._report_section)
        self._report_body = _WrapLabel()
        self._report_body.setObjectName("MissionReportBody")
        self._report_body.setTextFormat(Qt.TextFormat.MarkdownText)
        self._report_body.setTextInteractionFlags(Qt.TextInteractionFlag.TextSelectableByMouse)
        self._report_body.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
        layout.addWidget(self._report_body)

        # The files the report names, a click from being opened.
        self._report_files_row = QWidget()
        report_files = QVBoxLayout(self._report_files_row)
        report_files.setContentsMargins(0, 2, 0, 0)
        report_files.setSpacing(6)
        self._report_files_label = QLabel()
        self._report_files_label.setObjectName("MissionHintText")
        report_files.addWidget(self._report_files_label)
        # The buttons wrap onto more rows rather than hold the page wider than the window.
        self._report_files_layout = _FlowLayout(6)
        report_files.addLayout(self._report_files_layout)
        self._report_files_row.hide()
        layout.addWidget(self._report_files_row)

        # What lies behind the report: the steps that were done, and what happened along the way.
        links = QHBoxLayout()
        links.setSpacing(18)
        links.setContentsMargins(0, 4, 0, 0)
        self._report_steps_button = QPushButton()
        self._report_steps_button.setObjectName("MissionLinkButton")
        self._report_steps_button.setCursor(Qt.CursorShape.PointingHandCursor)
        self._report_steps_button.clicked.connect(self._toggle_report_steps)
        links.addWidget(self._report_steps_button)
        self._report_activity_button = QPushButton()
        self._report_activity_button.setObjectName("MissionLinkButton")
        self._report_activity_button.setCursor(Qt.CursorShape.PointingHandCursor)
        self._report_activity_button.clicked.connect(self._toggle_report_activity)
        links.addWidget(self._report_activity_button)
        links.addStretch(1)
        layout.addLayout(links)
        card.hide()
        return card

    def _show_report_actions(self) -> None:
        """Offer what can be done with the report, and the mission's less common actions."""
        strings = self._language_manager.strings()
        menu = self._style_manager.create_menu(self)
        export = menu.addAction(strings.mission_report_save_word)
        image = menu.addAction(strings.mission_report_copy_image)
        rerun = menu.addAction(strings.mission_run_again)
        menu.addSeparator()
        switch = menu.addAction(strings.mission_switch_to_classic) if self._can_switch_to_classic else None
        menu.addSeparator()
        delete = menu.addAction(strings.mission_delete)
        button = self._report_more_button
        chosen = menu.exec(button.mapToGlobal(button.rect().bottomLeft()))
        if chosen is None:
            return

        if chosen == export:
            self.report_export_requested.emit(self._report_source)

        elif chosen == image:
            self._copy_report_image()

        elif chosen == rerun:
            self.mission_rerun_requested.emit("")

        elif chosen == switch:
            self.switch_to_classic_requested.emit()

        elif chosen == delete:
            self.remove_requested.emit()

    def _copy_report_image(self) -> None:
        """Put a picture of the report card, as it looks on screen, on the clipboard."""
        QApplication.clipboard().setPixmap(self._report_card.grab())
        self._report_copied_timer.start()
        self._update_report_copy_button()

    def _set_report_files(self, files: list[str]) -> None:
        """Offer to open each file the report names."""
        self._report_files_row.setVisible(bool(files))
        _clear_layout(self._report_files_layout)
        for path in files:
            button = QPushButton(path)
            button.setObjectName("MissionExampleChip")
            button.setCursor(Qt.CursorShape.PointingHandCursor)
            button.setToolTip(self._language_manager.strings().mission_report_open_file.format(path))
            button.clicked.connect(lambda _checked=False, p=path: self.file_open_requested.emit(p))
            self._report_files_layout.addWidget(button)

    def _copy_report(self) -> None:
        """
        Put the report on the clipboard, and say so for a moment.

        It is copied twice over: formatted, with its headings, lists, bold text and tables, for a
        document, a mail or a chat that takes formatted text, and as the lead wrote it, in
        Markdown, for anywhere that takes only plain text.
        """
        document = QTextDocument()
        document.setDefaultFont(self._report_body.font())
        document.setMarkdown(_with_tables_set_apart(self._report_source))
        content = QMimeData()
        content.setText(self._report_source)
        content.setHtml(document.toHtml())
        QApplication.clipboard().setMimeData(content)
        self._report_copied_timer.start()
        self._update_report_copy_button()

    def _update_report_copy_button(self) -> None:
        """Label the copy button, saying the report was copied just after it has been."""
        strings = self._language_manager.strings()
        self._report_copy_button.setText(
            ("✓  " + strings.mission_report_copied) if self._report_copied_timer.isActive()
            else strings.mission_report_copy
        )

    def _toggle_report_steps(self) -> None:
        """Show a finished mission's steps under its report, or fold them away again."""
        self._report_steps_shown = not self._report_steps_shown
        self._update_report_links()
        if self._view is not None:
            self.set_mission(self._view)

    def _toggle_report_activity(self) -> None:
        """Show what happened during a finished mission beside its report, or fold it away again."""
        self._report_activity_shown = not self._report_activity_shown
        self._update_report_links()
        if self._view is not None:
            self.set_mission(self._view)

    def _update_report_links(self) -> None:
        """Label the report's links for what each will do."""
        strings = self._language_manager.strings()
        self._report_steps_button.setText(
            ("▼  " + strings.mission_report_hide_steps) if self._report_steps_shown
            else ("▶  " + strings.mission_report_show_steps)
        )
        self._report_activity_button.setText(
            ("▼  " + strings.mission_report_hide_activity) if self._report_activity_shown
            else ("▶  " + strings.mission_report_show_activity)
        )

    def _build_report_pin(self) -> QFrame:
        """
        Create the final report's sticky headline.

        It lies over the scrolling page rather than in it, as a child of the page's viewport, and is
        placed over the report card itself, so it reads as the top of that card wherever the card is.
        Held against the top of the view, it has square top corners and no top edge of its own, and
        a soft shadow, kept within the card's borders, shows the report passing beneath it.
        """
        pin = QFrame(self._scroll.viewport())
        pin.setObjectName("MissionReportPin")
        pin_layout = QHBoxLayout(pin)
        pin_layout.setContentsMargins(20, 9, 16, 9)
        pin_layout.setSpacing(12)
        pin_layout.addWidget(GlyphBadge("✓", MissionTone.SUCCESS, 26))
        self._report_pin_title = QLabel()
        self._report_pin_title.setObjectName("MissionReportPinTitle")
        pin_layout.addWidget(self._report_pin_title)
        self._report_pin_stats = QLabel()
        self._report_pin_stats.setObjectName("MissionReportPinStats")
        pin_layout.addWidget(self._report_pin_stats)
        pin_layout.addStretch(1)
        self._report_pin_top = QPushButton()
        self._report_pin_top.setObjectName("MissionLinkButton")
        self._report_pin_top.setCursor(Qt.CursorShape.PointingHandCursor)
        self._report_pin_top.clicked.connect(lambda: self.glide_to(self._report_header))
        pin_layout.addWidget(self._report_pin_top)
        self._report_pin_shadow = QWidget(self._scroll.viewport())
        self._report_pin_shadow.setObjectName("MissionReportPinShadow")
        self._report_pin_shadow.setAttribute(Qt.WidgetAttribute.WA_StyledBackground)
        self._report_pin_shadow.setAttribute(Qt.WidgetAttribute.WA_TransparentForMouseEvents)
        self._report_pin_shadow.hide()
        pin.hide()
        return pin

    def _update_report_pin(self) -> None:
        """
        Keep the report's headline at the top of the view while the rest of the report is read.

        As a section's sticky heading does, it appears once the card's own headline has scrolled
        away, lies exactly over the card, and is pushed up by the card's bottom edge as the report
        ends, so it never covers what follows the report.
        """
        pin = self._report_pin
        shown = self._has_mission and self._report_card.isVisibleTo(self)
        if shown:
            viewport = self._scroll.viewport()
            card = self._report_card
            header_bottom = self._report_header.mapTo(viewport, QPoint(0, self._report_header.height())).y()
            card_top_left = card.mapTo(viewport, QPoint(0, 0))
            card_bottom = card_top_left.y() + card.height()
            shown = header_bottom <= 0 < card_bottom
            if shown:
                height = pin.sizeHint().height()
                top = min(0, card_bottom - height)
                pin.setGeometry(card_top_left.x(), top, card.width(), height)
                # Inside the card's side borders, and no lower than the card, so it never falls on the page.
                shadow_height = max(0, min(round(8 * self._style_manager.zoom_factor()), card_bottom - top - height))
                self._report_pin_shadow.setGeometry(
                    card_top_left.x() + 1, top + height, card.width() - 2, shadow_height
                )
                self._report_pin_shadow.raise_()
                pin.raise_()

        if shown != pin.isVisibleTo(self):
            pin.setVisible(shown)
            self._report_pin_shadow.setVisible(shown)

    def _set_report(self, view: MissionView) -> None:
        """Show the final report when the mission is done, celebrating the moment it finishes."""
        completed = view.completed
        self._report_card.setVisible(completed)
        if completed:
            text = view.report_message
            self._report_source = text
            self._report_copy_button.setVisible(bool(text))
            self._set_report_files(view.report_files)
            self._report_title.setText(view.header.title)
            self._report_body.setText(_report_markdown(text))
            self._report_body.setVisible(bool(text))
            self._report_section.setVisible(bool(text))
            self._report_stats.setText(view.report_text)
            self._report_pin_stats.setText(view.report_text)
            # The owner says when to celebrate: once per mission, and only when the user watched it finish.
            if view.celebrate:
                fade_in(self._report_card, 420)
                badge = self._report_badge
                self._confetti.burst(QPointF(badge.mapTo(self, badge.rect().center())))

    def _build_results_card(self) -> QWidget:
        """Create the card that keeps the results of a mission's earlier rounds, for follow-ups to build on."""
        card, layout = self._card()
        header, self._results_title, self._results_pill = self._card_header("✓", MissionTone.SUCCESS)
        layout.addLayout(header)
        self._results_hint = QLabel()
        self._results_hint.setObjectName("MissionHintText")
        self._results_hint.setWordWrap(True)
        layout.addWidget(self._results_hint)
        self._results_layout = QVBoxLayout()
        self._results_layout.setSpacing(6)
        layout.addLayout(self._results_layout)
        card.hide()
        return card

    def _set_results(self, results: list[MissionResultView]) -> None:
        """Show the results of the mission's earlier rounds, newest first, each opening to its full report."""
        self._results_card.setVisible(self._details(bool(results)))
        self._results_pill.set_pill(str(len(results)), MissionTone.SUCCESS)
        if results == self._results:
            return

        self._results = results
        _clear_layout(self._results_layout)
        strings = self._language_manager.strings()
        for result in results:
            opened = result.key in self._open_results
            row = _ClickableFrame()
            row.setObjectName("MissionActivityRowOpen" if opened else "MissionActivityRow")
            row.setCursor(Qt.CursorShape.PointingHandCursor)
            row.setToolTip(strings.mission_activity_open_tip)
            row.setAccessibleName(result.heading)
            row.clicked.connect(lambda k=result.key: self._toggle_result(k))
            column = QVBoxLayout(row)
            column.setContentsMargins(8, 6, 8, 6)
            column.setSpacing(4)
            top = QHBoxLayout()
            top.setSpacing(8)
            heading = QLabel(result.heading)
            heading.setObjectName("MissionResultHeading")
            heading.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
            top.addWidget(heading, 1)
            chevron = QLabel("▼" if opened else "▶")
            chevron.setObjectName("MissionTimeText")
            top.addWidget(chevron, 0, Qt.AlignmentFlag.AlignTop)
            column.addLayout(top)
            if result.request_text:
                asked = QLabel(result.request_text)
                asked.setObjectName("MissionHintText")
                asked.setToolTip(result.request_text)
                asked.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
                column.addWidget(asked)

            report = result.report or strings.mission_result_no_report
            if opened:
                body = QLabel(report)
                body.setObjectName("MissionLeadText")
                body.setWordWrap(True)
                body.setTextFormat(Qt.TextFormat.MarkdownText if result.report else Qt.TextFormat.PlainText)
                body.setTextInteractionFlags(Qt.TextInteractionFlag.TextSelectableByMouse)
                body.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
                column.addWidget(body)

            else:
                excerpt = QLabel(_first_report_line(report))
                excerpt.setObjectName("MissionActivityText")
                excerpt.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
                column.addWidget(excerpt)

            self._results_layout.addWidget(row)
            if opened and result.key == self._just_opened_result:
                self._just_opened_result = ""
                self.glide_to(row)

    def _toggle_result(self, key: str) -> None:
        """Open or close an earlier result's full report."""
        if key in self._open_results:
            self._open_results.discard(key)

        else:
            self._open_results.add(key)
            self._just_opened_result = key

        results = self._results or []
        self._results = None
        self._set_results(results)

    def _build_classic_card(self) -> QWidget:
        """Create the panel shown instead of the dashboard while a mission is in classic mode."""
        card = QFrame()
        card.setObjectName("MissionClassicCard")
        layout = QVBoxLayout(card)
        layout.setContentsMargins(20, 18, 20, 18)
        layout.setSpacing(8)
        header = QHBoxLayout()
        header.setSpacing(10)
        header.addWidget(GlyphBadge("☰", MissionTone.INFO, 30))
        self._classic_title = QLabel()
        self._classic_title.setObjectName("MissionClassicTitle")
        header.addWidget(self._classic_title, 1)
        layout.addLayout(header)
        self._classic_body = QLabel()
        self._classic_body.setObjectName("MissionClassicBody")
        self._classic_body.setWordWrap(True)
        layout.addWidget(self._classic_body)
        actions = QHBoxLayout()
        actions.setSpacing(8)
        actions.setContentsMargins(0, 6, 0, 0)
        self._to_autonomous_button = QPushButton()
        self._to_autonomous_button.setObjectName("MissionReplyButton")
        self._to_autonomous_button.setCursor(Qt.CursorShape.PointingHandCursor)
        self._to_autonomous_button.clicked.connect(self.switch_to_autonomous_requested)
        actions.addWidget(self._to_autonomous_button)
        self._classic_open_button = QPushButton()
        self._classic_open_button.setObjectName("MissionGhostButton")
        self._classic_open_button.setCursor(Qt.CursorShape.PointingHandCursor)
        self._classic_open_button.clicked.connect(self.switch_to_classic_requested)
        actions.addWidget(self._classic_open_button)
        actions.addStretch(1)
        layout.addLayout(actions)
        card.hide()
        return card

    def _set_lead_message(self, text: str, time_text: str, awaiting_reply: bool) -> None:
        """Show the lead's latest message, highlighted when it is waiting for the user."""
        self._awaiting_reply = awaiting_reply
        self._lead_card.setVisible(bool(text))
        if text != self._lead_text:
            self._lead_text = text
            self._lead_body.setText(text)

        self._lead_time.setText(time_text)
        self._lead_pill.setVisible(awaiting_reply)
        self._reply_button.setVisible(awaiting_reply)
        name = "MissionLeadCardWaiting" if awaiting_reply else "MissionLeadCard"
        if self._lead_card.objectName() != name:
            self._lead_card.setObjectName(name)
            self._lead_card.style().unpolish(self._lead_card)
            self._lead_card.style().polish(self._lead_card)

    def _build_inbox(self) -> QFrame:
        """Create the decision inbox card."""
        card, layout = self._card("MissionInboxCard")
        header, self._inbox_title, self._inbox_pill = self._card_header("!", MissionTone.WARNING)
        layout.addLayout(header)
        self._decisions_layout = QVBoxLayout()
        self._decisions_layout.setSpacing(10)
        layout.addLayout(self._decisions_layout)
        self._inbox_keys = QLabel()
        self._inbox_keys.setObjectName("MissionHintText")
        self._inbox_keys.setWordWrap(True)
        self._inbox_keys.hide()
        layout.addWidget(self._inbox_keys)
        return card

    def _build_activity(self) -> QWidget:
        """Create the recent activity card."""
        card, layout = self._card()
        header, self._activity_title, self._activity_pill = self._card_header("↻", MissionTone.SUCCESS)
        self._activity_pill.hide()
        layout.addLayout(header)
        self._activity_empty = QLabel()
        self._activity_empty.setObjectName("MissionEmptyText")
        self._activity_empty.setWordWrap(True)
        layout.addWidget(self._activity_empty)
        feed = QWidget()
        feed.setObjectName("MissionActivityFeed")
        self._activity_layout = QVBoxLayout(feed)
        self._activity_layout.setContentsMargins(0, 0, 8, 0)
        self._activity_layout.setSpacing(4)
        self._activity_layout.setAlignment(Qt.AlignmentFlag.AlignTop)
        # As tall as its entries, up to a limit, so a quiet mission's feed does not leave a tall empty card.
        self._activity_scroll = _FittedScroll(feed, follow_end=False)
        self._activity_scroll.setObjectName("MissionActivityScroll")
        layout.addWidget(self._activity_scroll)
        return card

    def _build_composer(self) -> QWidget:
        """Create the objective and guidance input at the bottom."""
        composer_bar = QFrame()
        composer_bar.setObjectName("MissionComposerBar")
        outer = QHBoxLayout(composer_bar)
        outer.setContentsMargins(20, 12, 20, 16)

        composer = QFrame()
        composer.setObjectName("MissionComposer")
        composer_layout = QVBoxLayout(composer)
        composer_layout.setContentsMargins(0, 0, 0, 0)
        composer_layout.setSpacing(0)

        # Files attached to the next message, as a conversation's input shows them.
        self._attachments_bar = AttachmentsBar()
        self._attachments_bar.setContentsMargins(14, 8, 14, 0)
        composer_layout.addWidget(self._attachments_bar)

        row = QWidget()
        layout = QHBoxLayout(row)
        layout.setContentsMargins(10, 6, 6, 6)
        layout.setSpacing(10)
        composer_layout.addWidget(row)

        self._attach_button = QToolButton()
        self._attach_button.setObjectName("MissionComposerTool")
        self._attach_button.setCursor(Qt.CursorShape.PointingHandCursor)
        self._attach_button.clicked.connect(self.attach_requested)
        layout.addWidget(self._attach_button, 0, Qt.AlignmentFlag.AlignVCenter)

        self._target_pill = StatusPill()
        self._pills.append(self._target_pill)
        layout.addWidget(self._target_pill, 0, Qt.AlignmentFlag.AlignVCenter)

        self._input = _ComposerEdit()
        self._input.setObjectName("MissionComposerInput")
        self._input.submitted.connect(self._submit)
        self._input.textChanged.connect(self._update_send_enabled)
        layout.addWidget(self._input, 1)

        self._settings_button = QToolButton()
        self._settings_button.setObjectName("MissionComposerTool")
        self._settings_button.setCursor(Qt.CursorShape.PointingHandCursor)
        self._settings_button.clicked.connect(self.settings_requested)
        layout.addWidget(self._settings_button, 0, Qt.AlignmentFlag.AlignVCenter)

        self._send_button = QPushButton()
        self._send_button.setObjectName("MissionSendButton")
        self._send_button.setCursor(Qt.CursorShape.PointingHandCursor)
        self._send_button.clicked.connect(self._submit)
        layout.addWidget(self._send_button, 0, Qt.AlignmentFlag.AlignVCenter)

        outer.addWidget(composer)
        self._update_send_enabled()
        return composer_bar

    def retranslate(self) -> None:
        """Apply the current language to fixed text."""
        strings = self._language_manager.strings()
        self._top_title.setText(strings.mission_control)
        self._top_subtitle.setText(strings.mission_subtitle)
        self._hero_title.setText(strings.mission_hero_title)
        self._hero_subtitle.setText(strings.mission_hero_subtitle)
        self._examples_label.setText(strings.mission_examples_label)
        self._guide_title.setText(strings.mission_guide_title)
        self._guide_dismiss.setText(strings.mission_guide_dismiss)
        self._guide_link.setText(strings.mission_guide_link)
        guide = (
            (strings.mission_guide_1_title, strings.mission_guide_1),
            (strings.mission_guide_2_title, strings.mission_guide_2),
            (strings.mission_guide_3_title, strings.mission_guide_3),
        )
        for (heading, body), (heading_text, body_text) in zip(self._guide_steps, guide):
            heading.setText(heading_text)
            body.setText(body_text)

        self._templates_label.setText(strings.mission_templates_label)
        self._save_template_button.setText("☆  " + strings.mission_template_save)
        self._save_template_button.setToolTip(strings.mission_template_save_tip)
        templates = self._templates
        self._templates = None
        if templates is not None:
            self.set_templates(templates)

        examples = (strings.mission_example_1, strings.mission_example_2, strings.mission_example_3)
        for button, text in zip(self._example_buttons, examples):
            button.setText(text)

        self._attach_button.setToolTip(strings.tooltip_attach_file)
        self._settings_button.setToolTip(strings.tooltip_settings_message)
        self._journey.set_labels([
            strings.mission_stage_asked, strings.mission_stage_planning, strings.mission_stage_working,
            strings.mission_stage_done
        ])
        self._report_caption.setText(strings.mission_report_title.upper())
        self._report_section.setText(strings.mission_report_caption.upper())
        self._report_pin_title.setText(strings.mission_report_title)
        self._report_pin_top.setText("↑  " + strings.mission_report_back_to_top)
        self._follow_up_button.setText(strings.mission_follow_up)
        self._update_report_copy_button()
        self._report_files_label.setText(strings.mission_report_files)
        self._report_more_button.setToolTip(strings.mission_more_actions)
        self._report_more_button.setAccessibleName(strings.mission_more_actions)
        self._update_report_links()
        self._report_new_button.setText("+  " + strings.mission_new)
        self._classic_title.setText(strings.mission_classic_title)
        self._classic_body.setText(strings.mission_classic_body)
        self._to_autonomous_button.setText("✦  " + strings.mission_switch_to_autonomous)
        self._classic_open_button.setText(strings.mission_task_open_agent)
        self._lead_title.setText(strings.mission_lead_message_title)
        self._lead_pill.set_pill(strings.mission_lead_waiting, MissionTone.WARNING)
        self._reply_button.setText(strings.mission_reply)
        self._elsewhere_button.setText(strings.mission_go_to_waiting)
        for button, text in ((self._chip_left, strings.mission_scroll_left), (self._chip_right, strings.mission_scroll_right)):
            button.setToolTip(text)
            button.setAccessibleName(text)

        for arrow, arrow_text in (
            (self._chip_left, strings.mission_chips_back), (self._chip_right, strings.mission_chips_more)
        ):
            arrow.setToolTip(arrow_text)
            arrow.setAccessibleName(arrow_text)

        self._more_button.setToolTip(strings.mission_more_actions)
        self._more_button.setAccessibleName(strings.mission_more_actions)
        self._update_agents_list_button()
        self._stop_button.setText(strings.mission_stop)
        for label, row, buttons in (
            (self._auto_approve_label, self._auto_approve_row, self._auto_approve_buttons),
            (self._hero_auto_approve_label, self._hero_auto_approve_row, self._hero_auto_approve_buttons),
        ):
            label.setText(strings.mission_auto_approve)
            row.setToolTip(strings.mission_auto_approve_tip)
            buttons["off"].setText(strings.mission_auto_approve_off)
            buttons["safe"].setText(strings.mission_auto_approve_safe)
            buttons["all"].setText(strings.mission_auto_approve_all)

        self._mark_auto_approve(
            self._hero_auto_approve_buttons, self._hero_auto_approve_note, self._new_mission_auto_approve
        )
        self._everything_text.setText(strings.mission_everything_banner.format(self._everything_title))
        self._everything_off_button.setText(strings.mission_everything_turn_off)
        self._plan_title.setText(strings.mission_plan_title)
        self._plan_hint.setText(strings.mission_plan_hint)
        self._history_button.setToolTip(strings.mission_plan_history_tip)
        self._history_title.setText(strings.mission_plan_history_title.upper())
        self._results_title.setText(strings.mission_results_title)
        self._results_hint.setText(strings.mission_results_hint)
        self._chat_title.setText(strings.mission_chat_title)
        self._chat_hint.setText(strings.mission_chat_hint)
        self._agents_title.setText(strings.mission_agents_title)
        for key, caption_text in (("now", strings.mission_glance_now), ("progress", strings.mission_glance_progress)):
            self._glance_cells[key][2].setText(caption_text.upper())

        self._working_on_pill.set_pill(strings.mission_status_running, MissionTone.ACCENT, pulsing=True)
        self._update_details_button()
        self._agent_network.setAccessibleName(strings.mission_agents_title)
        self._agents_empty.setText(strings.mission_agents_empty)
        self._inbox_title.setText(strings.mission_inbox_title)
        self._mission_search.setPlaceholderText(strings.mission_search)
        self._mission_search.setAccessibleName(strings.mission_search)
        self._files_title.setText(strings.mission_files_title)
        self._files_hint.setText(strings.mission_files_hint)
        self._files_undo_all.setText("↺  " + strings.mission_files_undo_all)
        files = self._files
        self._files = None
        if files is not None:
            self._set_files(files, self._files_summary.text())

        self._inbox_keys.setText(strings.mission_inbox_keys)
        self._activity_title.setText(strings.mission_activity_title)
        self._activity_empty.setText(strings.mission_activity_empty)
        self._send_button.setToolTip(f"{strings.mission_send} ({submit_key_text()})")
        self._send_button.setAccessibleName(strings.mission_send)
        self._resume_button.setText("↻  " + strings.mission_resume)
        self._retry_button.setText("↻  " + strings.mission_retry)
        self._pause_button.setText("‖  " + strings.mission_pause)
        self._pause_button.setToolTip(strings.mission_pause_tip)
        self._unpause_button.setText("▶  " + strings.mission_unpause)
        self._carry_on_button.setText("▶  " + strings.mission_carry_on)
        self._start_work_button.setText("▶  " + strings.mission_start_work)
        self._plan_ready_notice.setText(strings.mission_plan_ready_notice)
        self._review_plan_check.setText(strings.mission_review_plan_option)
        self._remember_level_check.setText(strings.mission_remember_level)
        self._remember_level_check.setToolTip(strings.mission_remember_level_tip)
        self._review_plan_check.setToolTip(strings.mission_review_plan_tip)
        self._checks_option.setText(strings.mission_checks_option)
        self._checks_input.setPlaceholderText(strings.mission_checks_placeholder)
        self._checks_explain.setText(strings.mission_checks_explain)
        self._checks_title.setText(strings.mission_checks_title)
        self._checks_hint.setText(strings.mission_checks_hint)
        self._paused_notice.setText(strings.mission_paused_notice)
        self._interrupted_notice.setText(strings.mission_interrupted_notice)
        self._failed_notice.setText(strings.mission_failed_notice)
        self._lead_conversation_button.setText(strings.mission_show_lead_conversation)
        self._auto_resume.setText(strings.mission_auto_resume)
        self._reduce_motion.setText(strings.mission_reduce_motion)
        self._reduce_motion.setToolTip(strings.mission_reduce_motion_tip)
        self._update_composer_target(None)
        self._forget_rendered_views()

    def _forget_rendered_views(self) -> None:
        """Make the next update rebuild every list, for example after a style or language change."""
        self._chips = None
        self._steps = None
        self._history = None
        self._agents = None
        self._decisions = None
        self._activity = None
        self._stats = None
        self._conversation = None
        self._files = None
        self._checks = None

    def set_missions(self, chips: list[MissionChipView], new_mission_selected: bool) -> None:
        """
        Show the mission switcher.

        Args:
            chips: Missions to offer, newest first
            new_mission_selected: True when the user is starting a new mission
        """
        self._new_mission_selected = new_mission_selected
        archived = sum(1 for chip in chips if chip.archived)
        self._archived_toggle.setVisible(bool(archived))
        self._archived_toggle.setText(self._language_manager.strings().mission_archived_count.format(archived))
        if not archived and self._archived_toggle.isChecked():
            self._archived_toggle.setChecked(False)

        self._mission_search.setVisible(len(chips) > _SEARCH_FROM_MISSIONS)
        shown = self._shown_chips(chips)
        if chips == self._chips and shown == self._shown_chip_views and self._chip_layout.count():
            self._update_new_chip()
            return

        self._chips = chips
        self._shown_chip_views = shown
        _clear_layout(self._chip_layout)
        _clear_layout(self._new_chip_layout)
        self._new_chip = QPushButton("+  " + self._language_manager.strings().mission_new)
        self._new_chip.setObjectName("MissionNewChip")
        self._new_chip.setCursor(Qt.CursorShape.PointingHandCursor)
        self._new_chip.clicked.connect(self.new_mission_requested)
        self._new_chip_layout.addWidget(self._new_chip)
        selected_group: QWidget | None = None

        strings = self._language_manager.strings()
        for chip in shown:
            group = QFrame()
            group.setObjectName("MissionChipGroupSelected" if chip.selected else "MissionChipGroup")
            group_layout = QHBoxLayout(group)
            group_layout.setContentsMargins(0, 0, 0, 0)
            group_layout.setSpacing(0)

            button = QPushButton(chip.title)
            button.setObjectName("MissionChipLabel")
            button.setIcon(_dot_icon(chip.tone, chip.selected))
            button.setIconSize(QSize(12, 12))
            button.setToolTip(f"{chip.title} — {chip.status_text}" if chip.status_text else chip.title)
            button.setAccessibleName(f"{chip.title} — {chip.status_text}" if chip.status_text else chip.title)
            button.setCursor(Qt.CursorShape.PointingHandCursor)
            button.setMaximumWidth(400)
            button.clicked.connect(lambda _checked=False, m=chip.mission_id: self.mission_selected.emit(m))
            button.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
            button.customContextMenuRequested.connect(
                lambda pos, b=button, m=chip.mission_id: self._show_chip_menu(m, b.mapToGlobal(pos))
            )
            group_layout.addWidget(button)
            if chip.waiting_text:
                # A mission waiting for an answer says so on its chip, so it is noticed from any mission.
                waiting = StatusPill()
                waiting.setObjectName("MissionChipWaiting")
                waiting.set_pill(chip.waiting_text, MissionTone.WARNING, pulsing=True)
                waiting.setToolTip(chip.status_text)
                group_layout.addWidget(waiting, 0, Qt.AlignmentFlag.AlignVCenter)

            menu_button = QPushButton("⋯")
            menu_button.setObjectName("MissionChipMenu")
            menu_button.setToolTip(strings.mission_options)
            menu_button.setAccessibleName(strings.mission_options)
            menu_button.setCursor(Qt.CursorShape.PointingHandCursor)
            menu_button.clicked.connect(
                lambda _checked=False, b=menu_button, m=chip.mission_id: self._show_chip_menu(
                    m, b.mapToGlobal(b.rect().bottomLeft())
                )
            )
            group_layout.addWidget(menu_button)
            self._chip_layout.addWidget(group)
            if chip.selected:
                selected_group = group

        if not shown:
            empty = QLabel(strings.mission_search_none)
            empty.setObjectName("MissionHintText")
            self._chip_layout.addWidget(empty)

        self._chip_layout.addStretch(1)
        self._chip_row.setVisible(bool(chips))
        self._chip_strip.keep_in_view(selected_group)
        self._update_chip_arrows()
        self._update_new_chip()

    def _shown_chips(self, chips: list[MissionChipView]) -> list[MissionChipView]:
        """
        Return the missions the switcher shows: those matching the search, without archived ones
        unless the user asked to see them.  The mission on screen is always shown.
        """
        words = self._mission_search.text().casefold().split()
        show_archived = self._archived_toggle.isChecked()
        return [
            chip for chip in chips
            if chip.selected or (
                (show_archived or not chip.archived)
                and all(word in chip.search_text.casefold() for word in words)
            )
        ]

    def _refilter_missions(self) -> None:
        """Show the missions that match the search and the archived switch."""
        if self._chips is not None:
            chips = self._chips
            self._chips = None
            self.set_missions(chips, self._new_mission_selected)

    def _show_chip_menu(self, mission_id: str, position: QPoint) -> None:
        """Offer to rename, archive or delete a mission."""
        strings = self._language_manager.strings()
        archived = any(chip.archived for chip in self._chips or [] if chip.mission_id == mission_id)
        menu = self._style_manager.create_menu(self)
        rename = menu.addAction(strings.mission_rename)
        rerun = menu.addAction(strings.mission_run_again)
        archive = menu.addAction(strings.mission_unarchive if archived else strings.mission_archive)
        menu.addSeparator()
        delete = menu.addAction(strings.mission_delete)
        chosen = menu.exec(position)
        if chosen == rename:
            self.mission_rename_requested.emit(mission_id)

        elif chosen == rerun:
            self.mission_rerun_requested.emit(mission_id)

        elif chosen == archive:
            self.mission_archive_requested.emit(mission_id, not archived)

        elif chosen == delete:
            self.mission_delete_requested.emit(mission_id)

    def _update_new_chip(self) -> None:
        """Highlight the new-mission chip when it is selected."""
        if self._new_chip is None:
            return

        name = "MissionNewChipSelected" if self._new_mission_selected else "MissionNewChip"
        if self._new_chip.objectName() != name:
            self._new_chip.setObjectName(name)
            self._new_chip.style().unpolish(self._new_chip)
            self._new_chip.style().polish(self._new_chip)

    def set_mission(self, view: MissionView | None) -> None:
        """
        Show a mission, or the new-mission welcome when view is None.

        Args:
            view: The mission to show
        """
        self._has_mission = view is not None
        self._completed = view is not None and view.completed
        # The view is shown again whenever a fold is opened or closed, and a celebration happens only once.
        self._view = dataclasses.replace(view, celebrate=False) if view is not None and view.celebrate else view
        self._hero.setVisible(view is None)
        self._dashboard.setVisible(view is not None)
        self._activity_card.setVisible(view is not None)
        if view is None:
            self._intro.play_once()
            self._glance_shown = False
            self._working_on.hide()
            self._everything_banner.hide()
            self._chat_card.hide()
            self._update_side_column()
            self._set_spinners([])
            self._set_lead_message("", "", False)
            self._update_composer_target(None)
            self._update_report_pin()
            return

        header = view.header
        classic = header.is_classic
        self._glance_shown = view.glance is not None
        # The ring beside the title already says how far the plan has got and what is next.
        self._set_glance(
            dataclasses.replace(view.glance, progress_text="")
            if view.glance is not None and view.header.steps_text else view.glance
        )
        self._working_on.setVisible(bool(view.working_on_text))
        self._working_on_text.setText(view.working_on_text)
        self._classic_card.setVisible(classic)
        # The filters describe the plan, so they only appear once there is one.
        self._plan_filters.setVisible(not classic and bool(view.steps))
        completed = self._completed
        self._header_card.setVisible(not completed)
        self._lower_container.setVisible(self._report_steps_shown if completed else self._details(not classic))
        self._agents_card.setVisible(not classic and not completed)
        self._activity_card.setVisible(self._report_activity_shown if completed else self._details(not classic))
        self._can_switch_to_classic = not classic
        self._auto_approve_row.setVisible(not classic)
        self._show_auto_approve(header.auto_approve)
        self._everything_banner.setVisible(not classic and header.auto_approve == "all")
        self._everything_title = header.title
        self._everything_text.setText(
            self._language_manager.strings().mission_everything_banner.format(header.title)
        )
        self._set_lead_message("" if classic else view.lead_message, view.lead_message_time, view.awaiting_reply)
        self._set_report(view)
        self._set_results(view.results)
        self._set_conversation([] if classic else view.conversation)
        self._update_side_column()
        self._journey.setVisible(not classic)
        self._journey.set_stage(
            header.stage, header.stage_tone, header.live, header.progress if header.stage == 2 else 0.0
        )
        self._update_attention()
        self._update_report_pin()

        self._title.setText(header.title)
        self._summary.setText(header.summary)
        self._summary.setVisible(bool(header.summary) and header.summary.strip() != header.title.strip())
        self._status_pill.set_pill(header.status_text, header.tone, pulsing=header.live)
        self._timing_label.setText(header.timing_text)
        self._timing_label.setVisible(bool(header.timing_text))
        self._tokens_label.setText(f"·  {header.tokens_text}" if header.timing_text else header.tokens_text)
        self._tokens_label.setToolTip(header.tokens_detail)
        self._tokens_label.setVisible(bool(header.tokens_text))
        # The ring is for a plan; until there is one, there is nothing to count.
        self._progress_box.setVisible(bool(header.steps_text))
        self._progress_ring.set_progress(header.progress, header.stage_tone)
        self._progress_ring.setAccessibleName(header.progress_text)
        self._steps_label.setText(header.steps_text)
        self._progress_label.setText(header.next_text)
        self._progress_label.setVisible(bool(header.next_text))
        self._stop_button.setVisible(header.can_stop)
        self._resume_button.setVisible(header.can_resume)
        self._interrupted_notice.setVisible(header.can_resume)
        # A failed mission is not resumed: its last request is retried, or its lead's conversation is
        # opened to see what went wrong.
        # A mission that lost the AI service is retried by itself, and can be retried at once.
        self._retry_button.setVisible(header.can_retry)
        strings = self._language_manager.strings()
        self._retry_button.setText("↻  " + (strings.mission_retry_now if header.recovery_text else strings.mission_retry))
        self._pause_button.setVisible(header.can_pause)
        self._unpause_button.setVisible(header.paused)
        self._paused_notice.setVisible(header.paused)
        failed = header.can_retry and not header.recovery_text
        self._failed_notice.setVisible(failed)
        # What went wrong and what to do about it, when the kind of error is known, with what the AI
        # service said kept for those who want it.
        strings_now = self._language_manager.strings()
        self._failed_notice.setText(header.failure_text or strings_now.mission_failed_notice)
        self._failed_notice.setToolTip(header.failure_detail)
        self._recovery_notice.setText(header.recovery_text)
        # What the AI service said is technical, so it is kept for those who want it.
        self._recovery_notice.setToolTip(header.recovery_error)
        self._recovery_notice.setVisible(bool(header.recovery_text))
        self._stuck_notice.setText(header.stuck_text)
        self._stuck_notice.setVisible(bool(header.stuck_text))
        self._carry_on_button.setVisible(bool(header.stuck_text))
        self._plan_ready = header.plan_ready
        self._plan_ready_notice.setVisible(header.plan_ready)
        self._start_work_button.setVisible(header.plan_ready)
        self._lead_context_id = header.lead_context_id
        self._lead_conversation_button.setVisible(failed and bool(header.lead_context_id))
        self._update_composer_target(header.title)

        self._set_checks(view.checks, view.checks_summary)
        self._set_files(view.files, view.files_summary)
        self._set_stats(view.stats)
        self._set_history(view.history)
        self._set_steps(view.steps)
        self._set_agents(view.agents, view.agents_badge)
        self._set_activity(view.activity)

    def _set_checks(self, checks: list[MissionCheckView], summary: str) -> None:
        """Show the user's done checks, each with its latest result and what the lead saw."""
        self._checks_card.setVisible(bool(checks))
        self._checks_pill.set_pill(summary, MissionTone.ACCENT)
        if checks == self._checks:
            return

        self._checks = checks
        _clear_layout(self._checks_layout)
        for check in checks:
            row = QFrame()
            row.setObjectName("MissionFileRow")
            layout = QHBoxLayout(row)
            layout.setContentsMargins(6, 6, 6, 6)
            layout.setSpacing(10)
            layout.addWidget(GlyphBadge(check.glyph, check.tone, 24), 0, Qt.AlignmentFlag.AlignTop)
            texts = QVBoxLayout()
            texts.setSpacing(2)
            text = QLabel(check.text)
            text.setObjectName("MissionStepsLabel")
            text.setWordWrap(True)
            text.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
            texts.addWidget(text)
            detail = QLabel(f"{check.state_text}  ·  {check.evidence}" if check.evidence else check.state_text)
            detail.setObjectName("MissionAgentRole")
            detail.setWordWrap(True)
            detail.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
            detail.setTextInteractionFlags(Qt.TextInteractionFlag.TextSelectableByMouse)
            texts.addWidget(detail)
            layout.addLayout(texts, 1)
            self._checks_layout.addWidget(row)

    def _set_files(self, files: list[MissionFileView], summary: str) -> None:
        """
        Show the files the mission changed, each with what was done to it, by how many lines, and
        ways to see the change, open the file or undo it.

        A text file's row unfolds to show the change line by line.  Rows unfolded before stay
        unfolded when the list changes, with their diffs asked for again, since the change may
        have grown.
        """
        self._files_card.setVisible(bool(files))
        self._files_summary.setText(summary)
        self._files_summary.setVisible(bool(summary))
        self._files_pill.set_pill(str(len(files)), MissionTone.ACCENT)
        self._files_undo_all.setVisible(sum(1 for f in files if f.can_undo) > 1)
        if files == self._files:
            return

        self._files = files
        _clear_layout(self._files_layout)
        self._file_rows = {}
        self._file_diffs = {}
        strings = self._language_manager.strings()
        for file in files:
            row = _ClickableFrame()
            row.setObjectName("MissionFileRow")
            outer = QVBoxLayout(row)
            outer.setContentsMargins(6, 4, 6, 4)
            outer.setSpacing(6)
            layout = QHBoxLayout()
            layout.setSpacing(10)
            outer.addLayout(layout)
            unfoldable = file.lines_added is not None
            row.set_clickable(unfoldable)
            chevron = QLabel("▶" if unfoldable else "")
            chevron.setObjectName("MissionFileChevron")
            chevron.setFixedWidth(round(12 * self._style_manager.zoom_factor()))
            layout.addWidget(chevron)
            if unfoldable:
                row.setCursor(Qt.CursorShape.PointingHandCursor)
                row.setToolTip(strings.mission_file_diff_show)
                row.clicked.connect(lambda p=file.path: self._toggle_file_diff(p))

            layout.addWidget(GlyphBadge(file.glyph, file.tone, 24))
            texts = QVBoxLayout()
            texts.setSpacing(0)
            name_row = QHBoxLayout()
            name_row.setSpacing(10)
            folder, name = os.path.split(file.path)
            path = QLabel(name)
            path.setObjectName("MissionFilePath")
            path.setToolTip(file.path)
            name_row.addWidget(path)
            counts = []
            if file.lines_added:
                counts.append(f"<span style='color:{tone_color(MissionTone.SUCCESS).name()}'>+{file.lines_added}</span>")

            if file.lines_removed:
                counts.append(f"<span style='color:{tone_color(MissionTone.DANGER).name()}'>−{file.lines_removed}</span>")

            if counts:
                lines = QLabel("&nbsp;&nbsp;".join(counts))
                lines.setObjectName("MissionFileLines")
                lines.setTextFormat(Qt.TextFormat.RichText)
                lines.setToolTip(file.lines_text)
                name_row.addWidget(lines)
                stat = DiffStatBar(file.lines_added or 0, file.lines_removed or 0)
                stat.setToolTip(file.lines_text)
                name_row.addWidget(stat, 0, Qt.AlignmentFlag.AlignVCenter)

            name_row.addStretch(1)
            texts.addLayout(name_row)
            details = [folder + "/"] if folder else []
            details.append(file.kind_text)
            if file.state_text:
                details.append(file.state_text)

            kind = QLabel("  ·  ".join(details))
            kind.setObjectName("MissionAgentRole")
            kind.setToolTip(file.path)
            kind.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
            texts.addWidget(kind)
            layout.addLayout(texts, 1)
            if file.review_sequence is not None:
                review = self._file_action_button(
                    "review", strings.mission_file_review, strings.mission_file_review_tip.format(file.path)
                )
                review.clicked.connect(
                    lambda _checked=False, p=file.path, s=file.review_sequence: self.file_review_requested.emit(p, s)
                )
                layout.addWidget(review, 0, Qt.AlignmentFlag.AlignVCenter)

            if file.can_open:
                open_button = self._file_action_button(
                    "open", strings.mission_file_open, strings.mission_file_open_tip.format(file.path)
                )
                open_button.clicked.connect(lambda _checked=False, p=file.path: self.file_open_requested.emit(p))
                layout.addWidget(open_button, 0, Qt.AlignmentFlag.AlignVCenter)

            if file.can_undo:
                undo = self._file_action_button(
                    "undo", strings.mission_file_undo, strings.mission_file_undo_tip.format(file.path)
                )
                undo.clicked.connect(lambda _checked=False, p=file.path: self.file_undo_requested.emit(p))
                layout.addWidget(undo, 0, Qt.AlignmentFlag.AlignVCenter)

            diff = QFrame()
            diff.setObjectName("MissionFileDiff")
            QVBoxLayout(diff).setContentsMargins(0, 0, 0, 0)
            diff.hide()
            outer.addWidget(diff)
            self._file_rows[file.path] = (chevron, diff)
            self._files_layout.addWidget(row)

        self._expanded_files &= set(self._file_rows)
        for unfolded in sorted(self._expanded_files):
            self._file_rows[unfolded][0].setText("▼")
            self.file_diff_requested.emit(unfolded)

    def _file_action_button(self, action: str, name: str, tip: str) -> QToolButton:
        """
        Create one of the small icon buttons on a file's row.

        Args:
            action: Which icon to draw, as `_file_action_icon` takes it
            name: What the button does, for screen readers
            tip: What the button does to this file, shown on hover
        """
        zoom = self._style_manager.zoom_factor()
        button = QToolButton()
        button.setObjectName("MissionFileAction")
        button.setCursor(Qt.CursorShape.PointingHandCursor)
        button.setAccessibleName(name)
        button.setToolTip(tip)
        _page, card = surface_colors()
        color = readable(self._style_manager.get_color(ColorRole.TEXT_PRIMARY), card)
        icon_size = round(16 * zoom)
        button.setIcon(_file_action_icon(action, color, icon_size))
        button.setIconSize(QSize(icon_size, icon_size))
        button.setFixedSize(round(30 * zoom), round(30 * zoom))
        return button

    def _toggle_file_diff(self, path: str) -> None:
        """Unfold a file's row to show what changed in it line by line, or fold it away again."""
        if path not in self._file_rows:
            return

        chevron, diff = self._file_rows[path]
        if path in self._expanded_files:
            self._expanded_files.discard(path)
            chevron.setText("▶")
            diff.hide()
            return

        self._expanded_files.add(path)
        chevron.setText("▼")
        if path in self._file_diffs:
            diff.show()
            return

        self.file_diff_requested.emit(path)

    def set_file_diff(self, path: str, view: MissionDiffView) -> None:
        """
        Show what changed in a file, under its row, if the row is unfolded.

        Args:
            path: The file, relative to the mindspace
            view: Its lines, and anything to say under them
        """
        self._file_diffs[path] = view
        if path not in self._file_rows or path not in self._expanded_files:
            return

        diff = self._file_rows[path][1]
        layout = diff.layout()
        if layout is None:
            return

        _clear_layout(layout)
        if view.lines:
            layout.addWidget(FileDiffPanel(view.lines))

        if view.note:
            note = QLabel(view.note)
            note.setObjectName("MissionFileDiffNote")
            note.setWordWrap(True)
            layout.addWidget(note)

        diff.show()

    def _set_stats(self, stats: list[MissionStatView]) -> None:
        """Show the plan's filters, with how many steps each shows."""
        if stats == self._stats:
            return

        self._stats = stats
        _clear_layout(self._plan_filters_layout)
        for stat in stats:
            button = QPushButton(f"{stat.label}  {stat.value}")
            button.setObjectName(
                "MissionPlanFilterActive" if stat.target == self._step_filter else "MissionPlanFilter"
            )
            button.setCursor(Qt.CursorShape.PointingHandCursor)
            button.clicked.connect(lambda _checked=False, t=stat.target: self._choose_step_filter(t))
            self._plan_filters_layout.addWidget(button)

    def glide_to(self, widget: QWidget) -> None:
        """Scroll the page smoothly until a widget is in view, once the layout has caught up."""
        self._glide_target = widget
        self._glide_timer.start()

    def _glide_now(self) -> None:
        """Glide just far enough for the requested widget to be in view."""
        widget = self._glide_target
        self._glide_target = None
        self._release_hold()
        if widget is None or not widget.isVisible():
            return

        # The activity feed scrolls on its own; everything else is on the page.
        area = self._activity_scroll if self._activity_scroll.widget().isAncestorOf(widget) else self._scroll
        content = area.widget()
        if content is None or not content.isAncestorOf(widget):
            return

        margin = round((8 if area is self._activity_scroll else 24) * self._style_manager.zoom_factor())
        top = widget.mapTo(content, QPoint(0, 0)).y()
        scrollbar = area.verticalScrollBar()
        view = area.viewport().height()
        if top - margin < scrollbar.value():
            target = top - margin

        elif top + widget.height() + margin > scrollbar.value() + view:
            # Show the bottom, but never push the top out of view.
            target = min(top - margin, top + widget.height() + margin - view)

        else:
            return

        target = max(scrollbar.minimum(), min(scrollbar.maximum(), target))
        if MissionMotion().reduced():
            scrollbar.setValue(target)
            return

        animation = self._scroll_animations.get(area)
        if animation is None:
            animation = QPropertyAnimation(scrollbar, b"value", self)
            animation.setDuration(420)
            animation.setEasingCurve(QEasingCurve.Type.OutCubic)
            self._scroll_animations[area] = animation

        animation.stop()
        animation.setStartValue(scrollbar.value())
        animation.setEndValue(target)
        animation.start()

    def _answer_in_place(self, answered: SignalInstance, key: str) -> None:
        """Pass on an answer given in an agent's card, keeping the agents where they are on screen."""
        content = self._scroll.widget()
        if self._agent_network.isVisible() and content is not None:
            top = self._agent_network.mapTo(content, QPoint(0, 0)).y()
            self._hold_offset = top - self._scroll.verticalScrollBar().value()
            self._hold_timer.start()

        answered.emit(key)

    def _keep_hold(self) -> None:
        """Scroll so the agents stay where they were on screen when the page above them changes."""
        content = self._scroll.widget()
        if self._hold_offset is None or content is None or not self._agent_network.isVisible():
            return

        top = self._agent_network.mapTo(content, QPoint(0, 0)).y()
        self._scroll.verticalScrollBar().setValue(top - self._hold_offset)

    def _release_hold(self) -> None:
        """Let the page move freely again."""
        self._hold_offset = None
        self._hold_timer.stop()

    def eventFilter(self, watched: QObject, event: QEvent) -> bool:
        """Keep the agents in place while an answer given in their cards takes effect."""
        if watched is self._agents_card and event.type() == QEvent.Type.Move:
            self._keep_hold()

        return super().eventFilter(watched, event)

    def set_waiting_elsewhere(self, mission_id: str, text: str) -> None:
        """
        Say that another mission is waiting for the user, or hide the notice.

        Args:
            mission_id: The mission to go to, or "" when no other mission is waiting
            text: What to tell the user
        """
        self._elsewhere_mission_id = mission_id
        self._elsewhere_text.setText(text)
        self._elsewhere.setVisible(bool(mission_id))

    def _open_waiting_mission(self) -> None:
        """Go to the mission that is waiting for the user."""
        if self._elsewhere_mission_id:
            self.mission_selected.emit(self._elsewhere_mission_id)

    def _choose_step_filter(self, target: MissionStatTarget) -> None:
        """Narrow the plan to a filter's steps, or show every step if that filter is already chosen."""
        self._set_step_filter(MissionStatTarget.ALL_STEPS if target == self._step_filter else target)

    def _set_step_filter(self, target: MissionStatTarget) -> None:
        """Show only the steps a statistic stands for."""
        self._step_filter = target
        stats = self._stats
        self._stats = None
        if stats is not None:
            self._set_stats(stats)

        self._render_steps()

    def _set_steps(self, steps: list[MissionStepView]) -> None:
        """Show the plan timeline."""
        if steps == self._steps:
            return

        self._steps = steps
        self._render_steps()

    def _toggle_step(self, task_id: str) -> None:
        """Open or close a step's progress."""
        if task_id in self._expanded_steps:
            self._expanded_steps.discard(task_id)

        else:
            self._just_expanded = task_id
            self._expanded_steps.add(task_id)

        self._render_steps()

    def _render_steps(self) -> None:
        """Draw the plan, filtered and with opened steps showing their progress."""
        strings = self._language_manager.strings()
        steps = self._steps or []
        wanted = {
            MissionStatTarget.DONE_STEPS: MissionStepState.DONE,
            MissionStatTarget.ACTIVE_STEPS: MissionStepState.ACTIVE,
        }.get(self._step_filter)
        shown = [step for step in steps if wanted is None or step.state == wanted]
        self._plan_hint.setVisible(bool(shown))
        self._plan_empty.setVisible(not shown)
        self._plan_empty.setText(
            strings.mission_plan_filter_empty if steps
            else strings.mission_plan_empty_next if self._history
            else strings.mission_plan_empty
        )

        arranging = any(step.arrangeable for step in steps)
        self._plan_hint.setText(strings.mission_plan_arrange_hint if arranging else strings.mission_plan_hint)
        _clear_layout(self._steps_layout)
        for index, step in enumerate(shown):
            expanded = step.task_id in self._expanded_steps
            row = _StepRow(
                step.task_id,
                self._move_step if step.arrangeable else None,
                partial(self._nudge_step, step.task_id) if step.arrangeable else None,
            )
            row.setObjectName("MissionStepRowOpen" if expanded else "MissionStepRow")
            row.setCursor(Qt.CursorShape.PointingHandCursor)
            row.setAccessibleName(f"{step.number}. {step.title} — {step.status_text}")
            row.clicked.connect(lambda t=step.task_id: self._toggle_step(t))
            row_layout = QHBoxLayout(row)
            row_layout.setContentsMargins(4, 0, 8, 0)
            row_layout.setSpacing(10)
            if step.arrangeable:
                handle = _StepHandle(step.task_id)
                handle.setToolTip(strings.mission_step_move_tip)
                row_layout.addWidget(handle)

            marker = StepMarker()
            marker.set_step(step.number, step.state, index == 0, index == len(shown) - 1)
            row_layout.addWidget(marker)

            text = QVBoxLayout()
            text.setContentsMargins(0, 10, 0, 12)
            text.setSpacing(2)
            title = QLabel(step.title)
            title.setObjectName("MissionStepTitleSkipped" if step.skipped else "MissionStepTitle")
            title.setWordWrap(True)
            text.addWidget(title)
            if step.detail:
                detail = QLabel(step.detail)
                detail.setObjectName("MissionStepDetail")
                detail.setWordWrap(True)
                text.addWidget(detail)

            if expanded:
                text.addWidget(self._step_progress(step))

            row_layout.addLayout(text, 1)
            # The status and the open/close chevron sit on one line, level with the title.
            side = QHBoxLayout()
            side.setContentsMargins(0, 10, 0, 0)
            side.setSpacing(8)
            side.setAlignment(Qt.AlignmentFlag.AlignTop)
            pill = StatusPill()
            pill.set_pill(step.status_text, step.tone)
            side.addWidget(pill, 0, Qt.AlignmentFlag.AlignTop)
            if step.arrangeable:
                skip = QPushButton(strings.mission_step_unskip if step.skipped else strings.mission_step_skip)
                skip.setObjectName("MissionPlanFilter")
                skip.setCursor(Qt.CursorShape.PointingHandCursor)
                skip.clicked.connect(lambda _checked=False, t=step.task_id: self._toggle_skip(t))
                side.addWidget(skip, 0, Qt.AlignmentFlag.AlignTop)

            chevron = QLabel("▼" if expanded else "▶")
            chevron.setObjectName("MissionStepChevron")
            side.addWidget(chevron, 0, Qt.AlignmentFlag.AlignTop)
            row_layout.addLayout(side)
            self._steps_layout.addWidget(row)
            if step.task_id == self._focus_step_after_render:
                # A step moved from the keyboard keeps the focus, so it can be moved again.
                self._focus_step_after_render = ""
                row.setFocus()

    def _plan_order(self) -> tuple[list[str], list[str]]:
        """Return the plan's steps in their order, and the ones the user chose to skip."""
        steps = self._steps or []
        return [step.task_id for step in steps], [step.task_id for step in steps if step.skipped]

    def _move_step(self, dragged: str, target: str, after: bool) -> None:
        """Move a dragged step to just before or just after another, and ask for the plan to be kept so."""
        order, skipped = self._plan_order()
        if dragged not in order or target not in order or dragged == target:
            return

        moved = [task_id for task_id in order if task_id != dragged]
        moved.insert(moved.index(target) + (1 if after else 0), dragged)
        if moved != order:
            self.plan_rearranged.emit(moved, skipped)

    def _nudge_step(self, task_id: str, offset: int) -> None:
        """Move a step one place up or down, and keep the focus on it so it can be moved again."""
        order, skipped = self._plan_order()
        index = order.index(task_id) if task_id in order else -1
        if index < 0 or not 0 <= index + offset < len(order):
            return

        order[index], order[index + offset] = order[index + offset], order[index]
        self._focus_step_after_render = task_id
        self.plan_rearranged.emit(order, skipped)

    def _toggle_skip(self, task_id: str) -> None:
        """Skip a step the user does not want done, or bring it back."""
        order, skipped = self._plan_order()
        skipped = [t for t in skipped if t != task_id] if task_id in skipped else [*skipped, task_id]
        self.plan_rearranged.emit(order, [t for t in order if t in skipped])

    def _set_history(self, history: list[MissionPhaseView]) -> None:
        """Show the plans this mission finished earlier."""
        strings = self._language_manager.strings()
        self._history_button.setVisible(bool(history))
        self._history_button.setText(f"↺  {strings.mission_plan_history.format(len(history))}")
        if not history:
            self._history_open = False

        self._history_button.setChecked(self._history_open)
        self._history_panel.setVisible(self._history_open)
        if history == self._history:
            return

        self._history = history
        self._render_history()

    def _toggle_history(self) -> None:
        """Open or close the list of earlier plans."""
        self._history_open = not self._history_open
        self._history_button.setChecked(self._history_open)
        self._history_panel.setVisible(self._history_open)
        if self._history_open:
            fade_in(self._history_panel)
            self.glide_to(self._history_panel)

    @staticmethod
    def _phase_key(phase: MissionPhaseView) -> str:
        """Return a key that identifies a finished plan while the history grows."""
        return f"{phase.time_text}|{phase.title}"

    def _toggle_phase(self, key: str) -> None:
        """Open or close one earlier plan."""
        if key in self._expanded_phases:
            self._expanded_phases.discard(key)

        else:
            self._expanded_phases.add(key)

        self._render_history()

    def _render_history(self) -> None:
        """Draw the earlier plans, newest first, with opened ones listing their steps."""
        _clear_layout(self._history_layout)
        for phase in self._history or []:
            key = self._phase_key(phase)
            expanded = key in self._expanded_phases
            row = _ClickableFrame()
            row.setObjectName("MissionPhaseRowOpen" if expanded else "MissionPhaseRow")
            row.setCursor(Qt.CursorShape.PointingHandCursor)
            row.setAccessibleName(phase.title)
            row.clicked.connect(lambda k=key: self._toggle_phase(k))
            row_layout = QVBoxLayout(row)
            row_layout.setContentsMargins(12, 10, 12, 10)
            row_layout.setSpacing(4)

            heading = QHBoxLayout()
            heading.setSpacing(10)
            heading.addWidget(GlyphBadge("✓", MissionTone.SUCCESS, 22))
            title = QLabel(phase.title)
            title.setObjectName("MissionStepTitle")
            title.setWordWrap(True)
            heading.addWidget(title, 1)
            chevron = QLabel("▼" if expanded else "▶")
            chevron.setObjectName("MissionStepChevron")
            heading.addWidget(chevron)
            row_layout.addLayout(heading)

            meta = QLabel("  ·  ".join(text for text in (phase.count_text, phase.time_text) if text))
            meta.setObjectName("MissionStepDetail")
            row_layout.addWidget(meta)

            if expanded:
                if phase.summary and phase.summary.strip() != phase.title.strip():
                    summary = QLabel(phase.summary)
                    summary.setObjectName("MissionStepProgressText")
                    summary.setWordWrap(True)
                    row_layout.addWidget(summary)

                for index, step in enumerate(phase.steps):
                    row_layout.addWidget(self._history_step(step, index == 0, index == len(phase.steps) - 1))

            self._history_layout.addWidget(row)

    def _history_step(self, step: MissionStepView, first: bool, last: bool) -> QWidget:
        """Create a read-only row for a step of an earlier plan."""
        row = QWidget()
        layout = QHBoxLayout(row)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(10)
        marker = StepMarker()
        marker.set_step(step.number, step.state, first, last)
        layout.addWidget(marker)
        text = QVBoxLayout()
        text.setContentsMargins(0, 8, 0, 8)
        text.setSpacing(2)
        title = QLabel(step.title)
        title.setObjectName("MissionStepProgressText")
        title.setWordWrap(True)
        text.addWidget(title)
        note = step.evidence or step.detail
        if note:
            detail = QLabel(note)
            detail.setObjectName("MissionStepDetail")
            detail.setWordWrap(True)
            detail.setTextInteractionFlags(Qt.TextInteractionFlag.TextSelectableByMouse)
            text.addWidget(detail)

        layout.addLayout(text, 1)
        pill = StatusPill()
        pill.set_pill(step.status_text, step.tone)
        layout.addWidget(pill, 0, Qt.AlignmentFlag.AlignTop)
        return row

    def _step_progress(self, step: MissionStepView) -> QWidget:
        """Create the panel showing an opened step's progress, evidence and agent."""
        strings = self._language_manager.strings()
        box = QFrame()
        box.setObjectName("MissionStepProgress")
        layout = QVBoxLayout(box)
        layout.setContentsMargins(12, 10, 12, 10)
        layout.setSpacing(6)

        sections = [(strings.mission_task_progress, step.progress), (strings.mission_task_evidence, step.evidence)]
        if not step.progress and not step.evidence:
            sections = [(strings.mission_task_progress, strings.mission_task_no_update)]

        for heading, body in sections:
            if not body:
                continue

            label = QLabel(heading.upper())
            label.setObjectName("MissionStepSection")
            layout.addWidget(label)
            text = QLabel(body)
            text.setObjectName("MissionStepProgressText")
            text.setWordWrap(True)
            text.setTextInteractionFlags(Qt.TextInteractionFlag.TextSelectableByMouse)
            layout.addWidget(text)

        if step.agent_title:
            agent_row = QHBoxLayout()
            agent_row.setSpacing(8)
            agent_row.addWidget(GlyphBadge("◆", MissionTone.INFO, 20))
            agent = QLabel(strings.mission_task_worked_by.format(step.agent_title))
            agent.setObjectName("MissionStepProgressText")
            agent.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
            agent_row.addWidget(agent, 1)
            if step.agent_context_id:
                open_button = QPushButton(strings.mission_task_open_agent)
                open_button.setObjectName("MissionGhostButton")
                open_button.setCursor(Qt.CursorShape.PointingHandCursor)
                open_button.clicked.connect(
                    lambda _checked=False, c=step.agent_context_id: self.conversation_requested.emit(c)
                )
                agent_row.addWidget(open_button)

            layout.addLayout(agent_row)

        if step.can_stop:
            stop_row = QHBoxLayout()
            stop_row.addStretch(1)
            stop_button = QPushButton("■  " + strings.mission_task_stop)
            stop_button.setObjectName("MissionStopTaskButton")
            stop_button.setCursor(Qt.CursorShape.PointingHandCursor)
            stop_button.clicked.connect(lambda _checked=False, t=step.task_id: self.task_stop_requested.emit(t))
            stop_row.addWidget(stop_button)
            layout.addLayout(stop_row)

        if step.task_id == self._just_expanded:
            self._just_expanded = ""
            fade_in(box)
            self.glide_to(box)

        return box

    def _set_agents(self, agents: list[MissionAgentView], badge: str) -> None:
        """Show the conversations taking part."""
        busy = sum(1 for agent in agents if agent.busy)
        self._agents_pill.set_pill(badge, MissionTone.SUCCESS if busy else MissionTone.NEUTRAL)
        if agents == self._agents:
            return

        self._agents = agents
        _clear_layout(self._agents_layout)
        self._agents_empty.setVisible(not agents)
        self._agent_network.setVisible(bool(agents))
        strings = self._language_manager.strings()
        self._agent_network.set_agents(
            agents, strings.mission_approve, strings.mission_reject,
            strings.mission_finish_waiting, strings.mission_wait_left, strings.mission_read_more,
            strings.mission_unsure, strings.mission_review_request, strings.mission_plan_yes, strings.mission_plan_no
        )
        spinners: list[BusySpinner] = []
        for agent in agents:
            row = _ClickableFrame()
            row.setObjectName("MissionAgentRow")
            row.setAccessibleName(f"{agent.title} — {agent.role_text}")
            row.set_clickable(agent.clickable)
            if agent.clickable:
                row.setCursor(Qt.CursorShape.PointingHandCursor)
                row.clicked.connect(lambda c=agent.context_id: self.conversation_requested.emit(c))

            layout = QHBoxLayout(row)
            layout.setContentsMargins(10, 8, 12, 8)
            layout.setSpacing(10)
            glyph = "★" if agent.is_lead else "◆"
            layout.addWidget(GlyphBadge(glyph, MissionTone.ACCENT if agent.is_lead else MissionTone.INFO, 32))

            text = QVBoxLayout()
            text.setSpacing(0)
            title = QLabel(agent.name or agent.title)
            title.setObjectName("MissionAgentTitle")
            title.setToolTip(agent.title)
            title.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
            text.addWidget(title)
            role = QLabel(f"{agent.role_text}  ·  {agent.model_text}" if agent.model_text else agent.role_text)
            role.setObjectName("MissionAgentRole")
            text.addWidget(role)
            layout.addLayout(text, 1)

            if agent.busy:
                spinner = BusySpinner(agent.tone)
                spinners.append(spinner)
                layout.addWidget(spinner)

            pill = StatusPill()
            pill.set_pill(agent.state_text, agent.tone)
            layout.addWidget(pill)
            self._agents_layout.addWidget(row)

        self._set_spinners(spinners)

    def set_decisions(self, decisions: list[MissionDecisionView]) -> None:
        """
        Show the decision inbox.

        Args:
            decisions: Decisions waiting for the user, oldest first
        """
        # With nothing waiting, the card is a single line saying so.
        self._inbox_pill.set_pill(
            str(len(decisions)) if decisions else self._language_manager.strings().mission_inbox_clear,
            MissionTone.WARNING if decisions else MissionTone.SUCCESS
        )
        if decisions == self._decisions:
            return

        self._decisions = decisions
        self._update_side_column()
        self._update_attention()
        _clear_layout(self._decisions_layout)
        self._decision_cards = {}
        self._highlighted_card = None
        strings = self._language_manager.strings()
        for decision in decisions:
            card = QFrame()
            card.setObjectName("MissionDecisionCardDestructive" if decision.destructive else "MissionDecisionCard")
            layout = QVBoxLayout(card)
            layout.setContentsMargins(14, 12, 14, 12)
            layout.setSpacing(6)

            top = QHBoxLayout()
            top.setSpacing(8)
            top.addWidget(GlyphBadge(
                "⚠" if decision.destructive else "›",
                MissionTone.DANGER if decision.destructive else MissionTone.ACCENT, 26
            ))
            tool = QLabel(decision.tool_name)
            tool.setObjectName("MissionDecisionTool")
            tool.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
            top.addWidget(tool, 1)
            if decision.destructive:
                pill = StatusPill()
                pill.set_pill(strings.mission_destructive, MissionTone.DANGER)
                top.addWidget(pill)

            if decision.reviewing:
                reviewing = StatusPill()
                reviewing.set_pill(strings.mission_decision_reviewing, MissionTone.INFO, pulsing=True)
                top.addWidget(reviewing)

            when = QLabel(decision.time_text)
            when.setObjectName("MissionTimeText")
            top.addWidget(when)
            layout.addLayout(top)

            note_text = (
                strings.mission_decision_review_note.format(decision.review_note) if decision.review_note
                else decision.auto_note
            )
            if note_text:
                note = _WrapLabel(note_text)
                note.setObjectName("MissionReviewNote")
                layout.addWidget(note)

            summary, more = _split_reason(decision.reason)
            if summary:
                reason = _WrapLabel(_breakable(summary))
                reason.setObjectName("MissionDecisionReason")
                reason.setToolTip(decision.reason)
                layout.addWidget(reason)

            # What the request acts on, and whether a destructive one can be undone, before Approve.
            for fact_text, fact_name in (
                (decision.target_text, "MissionDecisionTarget"), (decision.undo_text, "MissionDecisionUndo")
            ):
                if fact_text:
                    fact = _WrapLabel(_breakable(_elide_detail(fact_text)))
                    fact.setObjectName(fact_name)
                    fact.setTextInteractionFlags(Qt.TextInteractionFlag.TextSelectableByMouse)
                    layout.addWidget(fact)

            source = QPushButton(decision.source_text)
            source.setObjectName("MissionLinkButton")
            source.setToolTip(decision.source_text)
            source.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Fixed)
            source.setCursor(Qt.CursorShape.PointingHandCursor)
            source.clicked.connect(lambda _checked=False, c=decision.context_id: self.conversation_requested.emit(c))
            layout.addWidget(source)

            detail_text = "\n\n".join(part for part in (more, decision.detail) if part)
            if detail_text:
                detail = _WrapLabel(_elide_detail(detail_text))
                detail.setObjectName("MissionDecisionDetail")
                detail.setTextInteractionFlags(Qt.TextInteractionFlag.TextSelectableByMouse)
                layout.addWidget(detail)

            if not decision.answerable:
                self._add_decision_card(card, decision.decision_id)
                continue

            buttons = QHBoxLayout()
            buttons.setSpacing(8)
            approve = QPushButton(strings.mission_approve)
            approve.setObjectName("MissionApproveDestructive" if decision.destructive else "MissionApprove")
            approve.setCursor(Qt.CursorShape.PointingHandCursor)
            approve.clicked.connect(lambda _checked=False, d=decision.decision_id: self.decision_approved.emit(d))
            buttons.addWidget(approve, 1)
            unsure = QPushButton(strings.mission_unsure)
            unsure.setObjectName("MissionSecondaryButton")
            unsure.setCursor(Qt.CursorShape.PointingHandCursor)
            unsure.clicked.connect(lambda _checked=False, d=decision.decision_id: self.decision_unsure.emit(d))
            buttons.addWidget(unsure, 1)
            reject = QPushButton(strings.mission_reject)
            reject.setObjectName("MissionSecondaryButton")
            reject.setCursor(Qt.CursorShape.PointingHandCursor)
            reject.clicked.connect(lambda _checked=False, d=decision.decision_id: self.decision_rejected.emit(d))
            buttons.addWidget(reject, 1)
            layout.addLayout(buttons)
            self._add_decision_card(card, decision.decision_id)

        self._shown_decisions = {decision.decision_id for decision in decisions}
        self._inbox_keys.setVisible(any(decision.answerable for decision in decisions))
        self._mark_selected_decision()

    def keyPressEvent(self, event: QKeyEvent) -> None:
        """Answer the Decision Inbox's requests from the keyboard; every other key is handled as usual."""
        if self._answer_by_key(event):
            event.accept()
            return

        super().keyPressEvent(event)

    def _answer_by_key(self, event: QKeyEvent) -> bool:
        """
        Act on a Decision Inbox shortcut, returning False if the key is not one.

        A approves the selected request, R rejects it, U says the user is unsure, and N and P select
        the next and the previous request.  A destructive request is approved only with Shift+A: A
        alone takes the user to it, where they can see what it acts on before they approve it.
        """
        answerable = [decision for decision in self._decisions or [] if decision.answerable]
        held = Qt.KeyboardModifier.ControlModifier | Qt.KeyboardModifier.MetaModifier | Qt.KeyboardModifier.AltModifier
        if not answerable or event.modifiers() & held:
            return False

        ids = [decision.decision_id for decision in answerable]
        if self._selected_decision not in ids:
            self._selected_decision = ids[0]

        current = answerable[ids.index(self._selected_decision)]
        key = event.key()
        if key in (Qt.Key.Key_N, Qt.Key.Key_P):
            step = 1 if key == Qt.Key.Key_N else -1
            self._selected_decision = ids[(ids.index(self._selected_decision) + step) % len(ids)]
            card = self._decision_cards.get(self._selected_decision)
            if card is not None:
                self.glide_to(card)

        elif key == Qt.Key.Key_A:
            if current.destructive and not event.modifiers() & Qt.KeyboardModifier.ShiftModifier:
                self.show_decision(current.decision_id)

            else:
                self.decision_approved.emit(current.decision_id)

        elif key == Qt.Key.Key_R:
            self.decision_rejected.emit(current.decision_id)

        elif key == Qt.Key.Key_U:
            self.decision_unsure.emit(current.decision_id)

        else:
            return False

        self._keyboard_answering = True
        self._mark_selected_decision()
        return True

    def _mark_selected_decision(self) -> None:
        """Ring the request the keyboard answers, once the user has started answering by keyboard."""
        for decision_id, card in self._decision_cards.items():
            selected = self._keyboard_answering and decision_id == self._selected_decision
            if bool(card.property("selected")) != selected:
                card.setProperty("selected", selected)
                card.style().unpolish(card)
                card.style().polish(card)

    def _add_decision_card(self, card: QFrame, decision_id: str) -> None:
        """Put a decision's card in the inbox, fading it in the first time it is shown."""
        self._decisions_layout.addWidget(card)
        self._decision_cards[decision_id] = card
        if decision_id not in self._shown_decisions:
            fade_in(card, 320)

    def show_decision(self, decision_id: str) -> None:
        """Bring a decision's card in the inbox into view and highlight it, so its whole request can be read."""
        card = self._decision_cards.get(decision_id)
        if card is None:
            return

        self._clear_highlight()
        self.glide_to(card)
        self._set_card_name(card, "MissionDecisionCardHighlight")
        self._highlighted_card = card
        self._highlight_timer.start()

    def _clear_highlight(self) -> None:
        """Take the highlight off the decision card the user was taken to."""
        card = self._highlighted_card
        self._highlighted_card = None
        if card is None or card not in self._decision_cards.values():
            return

        destructive = any(
            d.destructive for d in self._decisions or [] if self._decision_cards.get(d.decision_id) is card
        )
        self._set_card_name(card, "MissionDecisionCardDestructive" if destructive else "MissionDecisionCard")

    @staticmethod
    def _set_card_name(card: QFrame, name: str) -> None:
        """Restyle a card by changing its object name."""
        card.setObjectName(name)
        card.style().unpolish(card)
        card.style().polish(card)

    def _toggle_activity(self, key: tuple[str, str]) -> None:
        """Open or close an activity entry's details."""
        if key in self._open_activity:
            self._open_activity.discard(key)

        else:
            self._open_activity.add(key)
            self._just_opened_activity = key

        if self._activity is not None:
            self._show_activity(self._activity)

    def _set_activity(self, activity: list[MissionActivityView]) -> None:
        """Show the activity feed."""
        self._activity_scroll.setVisible(bool(activity))
        if activity == self._activity:
            return

        if self._activity is None:
            # Everything is drawn afresh, for example after a style or language change.
            _clear_layout(self._activity_layout)
            self._activity_rows = []

        self._activity = activity
        self._show_activity(activity)

    def _show_activity(self, activity: list[MissionActivityView]) -> None:
        """Show the activity rows, making only those that are new or have been opened or closed."""
        self._activity_empty.setVisible(not activity)
        fresh = bool(self._shown_activity)
        keys: list[Hashable] = [
            (entry, bool(entry.more) and (entry.text, entry.time_text) in self._open_activity) for entry in activity
        ]
        self._activity_rows = _sync_rows(
            self._activity_layout, self._activity_rows, keys, lambda index: self._activity_row(activity[index], fresh)
        )
        self._shown_activity = {(entry.text, entry.time_text) for entry in activity}

    def _activity_row(self, entry: MissionActivityView, fade_new: bool) -> QWidget:
        """Create the row for one activity entry, fading it in if it has just happened."""
        strings = self._language_manager.strings()
        key = (entry.text, entry.time_text)
        opened = bool(entry.more) and key in self._open_activity
        row: QWidget
        if entry.more:
            clickable = _ClickableFrame()
            clickable.setObjectName("MissionActivityRowOpen" if opened else "MissionActivityRow")
            clickable.setCursor(Qt.CursorShape.PointingHandCursor)
            clickable.setToolTip(strings.mission_activity_open_tip)
            clickable.setAccessibleName(entry.text)
            clickable.clicked.connect(lambda k=key: self._toggle_activity(k))
            row = clickable

        else:
            row = QWidget()

        column = QVBoxLayout(row)
        column.setContentsMargins(4, 3, 4, 3)
        column.setSpacing(4)
        layout = QHBoxLayout()
        layout.setSpacing(10)
        layout.addWidget(GlyphBadge(entry.glyph, entry.tone, 22), 0, Qt.AlignmentFlag.AlignTop)
        text = QLabel(entry.text)
        text.setObjectName("MissionActivityText")
        text.setWordWrap(True)
        text.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
        layout.addWidget(text, 1)
        when = QLabel(entry.time_text)
        when.setObjectName("MissionTimeText")
        layout.addWidget(when, 0, Qt.AlignmentFlag.AlignTop)
        if entry.more:
            chevron = QLabel("▼" if opened else "▶")
            chevron.setObjectName("MissionTimeText")
            layout.addWidget(chevron, 0, Qt.AlignmentFlag.AlignTop)

        column.addLayout(layout)
        if opened:
            more = _WrapLabel(_breakable(entry.more))
            more.setObjectName("MissionActivityMore")
            more.setTextInteractionFlags(Qt.TextInteractionFlag.TextSelectableByMouse)
            column.addWidget(more)
            if key == self._just_opened_activity:
                self._just_opened_activity = None
                self.glide_to(row)

        if fade_new and key not in self._shown_activity:
            fade_in(row, 360)

        return row

    def _set_spinners(self, spinners: list[BusySpinner]) -> None:
        """Track the spinners on screen and animate them only while there are any."""
        self._spinners = spinners
        if spinners and self.isVisible():
            self._spinner_timer.start()

        else:
            self._spinner_timer.stop()

    def _advance_spinners(self) -> None:
        """Advance every visible spinner."""
        for spinner in self._spinners:
            spinner.advance()

    def _update_composer_target(self, mission_title: str | None) -> None:
        """Show whether the composer starts a new mission or guides the selected one."""
        strings = self._language_manager.strings()
        if mission_title is None or not self._has_mission:
            self._target_pill.set_pill("✦ " + strings.mission_new, MissionTone.ACCENT)
            self._target_pill.show()
            self._input.setPlaceholderText(strings.mission_composer_new.format(key=submit_key_text()))
            return

        # The mission being guided is the one on show, so the composer does not repeat its name.
        self._target_pill.hide()
        # While the plan waits for approval, a message is how the user asks for it to be changed.
        prompt = (
            strings.mission_composer_plan_change if self._plan_ready
            else strings.mission_composer_reply if self._awaiting_reply
            else strings.mission_composer_guidance
        )
        self._input.setPlaceholderText(prompt.format(key=submit_key_text()))

    def _use_example(self, text: str) -> None:
        """Put an example objective in the composer."""
        self._input.setPlainText(text)
        self._input.moveCursor(QTextCursor.MoveOperation.End)
        self._input.setFocus()

    def _update_send_enabled(self) -> None:
        """Enable sending, and saving what was typed as a template, only when there is something typed."""
        typed = bool(self._input.toPlainText().strip())
        self._send_button.setEnabled(typed)
        self._save_template_button.setEnabled(typed)

    def _submit(self) -> None:
        """Send the composer text."""
        text = self._input.toPlainText().strip()
        if not text:
            return

        self._input.clear()
        self.submit_requested.emit(text)

    def add_attachment(self, filename: str, content: str) -> None:
        """Attach a file to the next message, and show it above the composer."""
        self._attachments_bar.add_attachment(filename, content)

    def restore_message(self, text: str, attachments: list[tuple[str, str]]) -> None:
        """
        Put back a message that could not be sent, so the user does not have to write or attach it again.

        Text the user has typed since is kept rather than replaced.
        """
        if not self._input.toPlainText().strip():
            self._input.setPlainText(text)
            self._input.moveCursor(QTextCursor.MoveOperation.End)

        for filename, content in attachments:
            self._attachments_bar.add_attachment(filename, content)

    def take_attachments(self) -> list[tuple[str, str]]:
        """Return the files attached to the message being sent, as (filename, content) pairs, and clear them."""
        attachments = self._attachments_bar.attachments()
        self._attachments_bar.clear()
        return attachments

    def _show_auto_approve(self, level: str) -> None:
        """Mark the current auto-approve level, and say what it means."""
        self._mark_auto_approve(self._auto_approve_buttons, self._auto_approve_note, level)

    def set_new_mission_auto_approve(self, level: str, remembered: bool = False) -> None:
        """
        Show the auto-approve level the next mission will start with, without reporting a change.

        Args:
            level: The level
            remembered: Whether every new mission starts at this level; approving everything never does
        """
        self._new_mission_auto_approve = level
        self._mark_auto_approve(self._hero_auto_approve_buttons, self._hero_auto_approve_note, level)
        self._remember_level_check.setEnabled(level != "all")
        self._remember_level_check.setChecked(remembered and level != "all")

    def _mark_auto_approve(self, buttons: dict[str, QPushButton], note: QLabel, level: str) -> None:
        """Check the button for a level, and say in the note what the level means."""
        for value, button in buttons.items():
            button.setChecked(value == level)

        strings = self._language_manager.strings()
        note.setText({
            "off": strings.mission_auto_approve_off_note,
            "safe": strings.mission_auto_approve_safe_note,
            "all": strings.mission_auto_approve_all_note,
        }.get(level, ""))

    def _show_more_actions(self, button: QPushButton) -> None:
        """Offer the mission's less common actions, under the button that was pressed."""
        strings = self._language_manager.strings()
        menu = self._style_manager.create_menu(self)
        switch = menu.addAction(strings.mission_switch_to_classic) if self._can_switch_to_classic else None
        menu.addSeparator()
        delete = menu.addAction(strings.mission_delete)
        chosen = menu.exec(button.mapToGlobal(button.rect().bottomLeft()))
        if chosen is not None and chosen == switch:
            self.switch_to_classic_requested.emit()

        elif chosen is not None and chosen == delete:
            self.remove_requested.emit()

    def _toggle_agents_list(self) -> None:
        """Show or hide the agents as a list."""
        self._agents_list.setVisible(self._agents_list.isHidden())
        self._update_agents_list_button()

    def _update_agents_list_button(self) -> None:
        """Label the agents list toggle for what it will do."""
        strings = self._language_manager.strings()
        self._agents_list_button.setText(
            ("▼  " + strings.mission_agents_hide_list) if not self._agents_list.isHidden()
            else ("▶  " + strings.mission_agents_show_list)
        )

    def _review_first_decision(self) -> None:
        """Take the user to the oldest request waiting for them."""
        if self._decisions:
            self.show_decision(self._decisions[0].decision_id)

    def _open_lead_conversation(self) -> None:
        """Open the lead conversation of a failed mission, where the error is explained."""
        if self._lead_context_id:
            self.conversation_requested.emit(self._lead_context_id)

    def _update_side_column(self) -> None:
        """
        Show the side column only when it has something to show.

        Starting a new mission has no activity or conversation yet, and a finished mission has put
        its own away, so the inbox is shown on those pages only while a request is waiting, and
        otherwise the welcome or the report takes the page's whole width.
        """
        inbox = bool(self._decisions) or (self._has_mission and not self._completed)
        self._inbox_card.setVisible(inbox)
        self._side_column.setVisible(inbox or not self._activity_card.isHidden() or not self._chat_card.isHidden())

    def _update_attention(self) -> None:
        """
        Make what waits for the user the most prominent thing on the page.

        While requests wait, the call to action is shown, the Decision Inbox is drawn as needing
        the user, and the activity feed, which is only context, gives up its height.
        """
        count = len(self._decisions or [])
        self._attention_bar.setVisible(self._has_mission and count > 0)
        self._attention.setVisible(count > 0)
        strings = self._language_manager.strings()
        self._attention_text.setText(strings.mission_attention.format(count))
        self._attention_button.setText(strings.mission_review_now)
        inbox_name = "MissionInboxCardActive" if count else "MissionInboxCard"
        if self._inbox_card.objectName() != inbox_name:
            self._set_card_name(self._inbox_card, inbox_name)
            self._fit_activity()

    def _fit_activity(self) -> None:
        """Let the activity feed grow to its limit, or a shorter one while requests wait for the user."""
        height = _ACTIVITY_WAITING_HEIGHT if self._decisions else _ACTIVITY_HEIGHT
        self._activity_scroll.setMaximumHeight(round(height * self._style_manager.zoom_factor()))

    def _on_auto_approve_clicked(self, level: str) -> None:
        """Report a chosen auto-approve level; the owner confirms it and then redraws the actual level."""
        self.auto_approve_selected.emit(level)

    def set_reduce_motion(self, reduced: bool) -> None:
        """
        Show whether motion is reduced, without reporting a change, and draw everything to match.

        Args:
            reduced: True to draw everything still
        """
        self._reduce_motion.blockSignals(True)
        self._reduce_motion.setChecked(reduced)
        self._reduce_motion.blockSignals(False)
        if reduced == MissionMotion().reduced():
            return

        MissionMotion().set_reduced(reduced)
        for animation in self._scroll_animations.values():
            animation.stop()

        self._agent_network.apply_motion()
        self._journey.apply_motion()
        self.update()

    def set_auto_resume(self, enabled: bool) -> None:
        """Show whether interrupted missions are resumed automatically, without reporting a change."""
        self._auto_resume.blockSignals(True)
        self._auto_resume.setChecked(enabled)
        self._auto_resume.blockSignals(False)

    def has_composer_text(self) -> bool:
        """Return True if there is something in the composer to send."""
        return bool(self._input.toPlainText().strip())

    def submit_composer(self) -> None:
        """Send what is in the composer, as Ctrl+Enter or the Send button does."""
        self._submit()

    def focus_composer(self) -> None:
        """Move keyboard focus to the composer."""
        self._input.setFocus()
        self._input.selectAll()

    def showEvent(self, event: QShowEvent) -> None:
        """Resume animation when shown."""
        super().showEvent(event)
        self._set_spinners(self._spinners)

    def hideEvent(self, event: QHideEvent) -> None:
        """Pause animation while hidden."""
        super().hideEvent(event)
        self._spinner_timer.stop()

    def resizeEvent(self, event: QResizeEvent) -> None:
        """Reflow the layout for the available width."""
        super().resizeEvent(event)
        zoom = self._style_manager.zoom_factor()
        width = self.width()
        if width >= _SIDE_BY_SIDE_WIDTH * zoom:
            self._body.setDirection(QBoxLayout.Direction.LeftToRight)
            self._body.setStretchFactor(self._main_column, 1)
            self._side_column.setFixedWidth(round(_SIDE_COLUMN_WIDTH * zoom))

        else:
            self._body.setDirection(QBoxLayout.Direction.TopToBottom)
            self._body.setStretchFactor(self._main_column, 0)
            self._side_column.setMinimumWidth(0)
            self._side_column.setMaximumWidth(16777215)

        main_width = width - (round(_SIDE_COLUMN_WIDTH * zoom) if width >= _SIDE_BY_SIDE_WIDTH * zoom else 0)
        split = (
            QBoxLayout.Direction.LeftToRight if main_width >= _SPLIT_LOWER_WIDTH * zoom
            else QBoxLayout.Direction.TopToBottom
        )
        self._lower.setDirection(split)
        self._glance_row.setDirection(split)
        self._guide_steps_layout.setDirection(split)
        self._update_report_pin()

    def apply_style(self) -> None:
        """Apply the current theme and zoom."""
        for pill in self._pills:
            pill.restyle()

        self._content_layout.setContentsMargins(24, 20, 24, 24)
        self._fit_activity()
        self._chat_scroll.setMaximumHeight(round(_CHAT_MAX_HEIGHT * self._style_manager.zoom_factor()))
        self._input.set_margin(round(6 * self._style_manager.zoom_factor()))
        icon_size = round(18 * self._style_manager.zoom_factor())
        self._send_button.setIcon(_send_icon(self._style_manager.get_color(ColorRole.TEXT_RECOMMENDED), icon_size))
        self._send_button.setIconSize(QSize(icon_size, icon_size))
        tool_size = round(16 * self._style_manager.zoom_factor())
        for button, icon in ((self._attach_button, "paperclip"), (self._settings_button, "cog")):
            button.setIcon(QIcon(self._style_manager.scale_icon(icon, 16)))
            button.setIconSize(QSize(tool_size, tool_size))

        self._attachments_bar.apply_style()
        new_stylesheet = self._build_stylesheet()
        if new_stylesheet != self.styleSheet():
            self.setStyleSheet(new_stylesheet)

        # Cached views were drawn with the old style, so the next update must rebuild them.
        self._forget_rendered_views()
        self.update()

    def _build_stylesheet(self) -> str:
        """
        Build the widget stylesheet from the current theme.

        One level of cards sits on the page; details inside a card (a decision,
        an opened step) are inset in the page colour rather than boxed again.
        Secondary text, links and status colours are adjusted to meet WCAG AA
        contrast on the surface they appear on, and every button shows a focus
        ring for keyboard users.
        """
        sm = self._style_manager
        zoom = sm.zoom_factor()
        fs = sm.base_font_size() * zoom
        radius = round(18 * zoom)
        small_radius = round(10 * zoom)
        control_height = round(30 * zoom)

        page_color, card_color = surface_colors()
        page = page_color.name()
        card = card_color.name()
        line = sm.get_color_str(ColorRole.MESSAGE_BORDER)
        text = sm.get_color_str(ColorRole.TEXT_PRIMARY)
        heading = readable(sm.get_color(ColorRole.TEXT_HEADING), card_color).name()
        muted = readable(sm.get_color(ColorRole.TEXT_INACTIVE), card_color).name()
        muted_on_page = readable(sm.get_color(ColorRole.TEXT_INACTIVE), page_color).name()
        link = readable(sm.get_color(ColorRole.TEXT_LINK), card_color).name()
        link_on_page = readable(sm.get_color(ColorRole.TEXT_LINK), page_color).name()
        input_bg = sm.get_color_str(ColorRole.MESSAGE_INPUT_BACKGROUND)
        input_border = sm.get_color_str(ColorRole.MESSAGE_INPUT_BORDER)
        primary = sm.get_color_str(ColorRole.BUTTON_BACKGROUND_RECOMMENDED)
        primary_hover = sm.get_color_str(ColorRole.BUTTON_BACKGROUND_RECOMMENDED_HOVER)
        primary_pressed = sm.get_color_str(ColorRole.BUTTON_BACKGROUND_RECOMMENDED_PRESSED)
        danger_fill = sm.get_color_str(ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE)
        danger_hover = sm.get_color_str(ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE_HOVER)
        danger_pressed = sm.get_color_str(ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE_PRESSED)
        secondary = sm.get_color_str(ColorRole.BUTTON_SECONDARY_BACKGROUND)
        secondary_hover = sm.get_color_str(ColorRole.BUTTON_SECONDARY_BACKGROUND_HOVER)
        secondary_pressed = sm.get_color_str(ColorRole.BUTTON_SECONDARY_BACKGROUND_PRESSED)
        on_primary = sm.get_color_str(ColorRole.TEXT_RECOMMENDED)
        disabled_fill = sm.get_color_str(ColorRole.BUTTON_BACKGROUND_DISABLED)
        disabled_text = sm.get_color_str(ColorRole.TEXT_DISABLED)
        mono = sm.make_monospace_font().family()

        focus = readable(tone_color(MissionTone.ACCENT), card_color, 3.0).name()
        accent_text = tone_text(MissionTone.ACCENT, 0.0)
        accent_soft = tone_rgba(MissionTone.ACCENT, 0.08)
        accent_line = tone_rgba(MissionTone.ACCENT, 0.45)
        warning_soft = blend(tone_color(MissionTone.WARNING), 0.10, card_color).name()
        danger_soft = blend(tone_color(MissionTone.DANGER), 0.07, page_color).name()
        hover_color = QColor(sm.get_color(ColorRole.TEXT_PRIMARY))
        hover = f"rgba({hover_color.red()}, {hover_color.green()}, {hover_color.blue()}, 0.05)"

        stylesheet = f"""
            #MissionWidget QWidget {{
                background: transparent;
            }}
            #MissionWidget, #MissionContent, #MissionAttentionBar {{
                background-color: {page};
            }}
            #MissionWidget QLabel {{
                background: transparent;
                color: {text};
                font-size: {fs:.1f}pt;
            }}

            #MissionTopBar {{
                background-color: {card};
                border-bottom: 1px solid {line};
            }}
            #MissionEverythingBanner {{
                background-color: {danger_soft};
                border-bottom: 1px solid {tone_rgba(MissionTone.DANGER, 0.6)};
            }}
            #MissionWidget QLabel#MissionEverythingText {{
                color: {heading};
                font-weight: bold;
            }}
            #MissionComposerBar {{
                background-color: {card};
                border-top: 1px solid {line};
            }}
            #MissionWidget QLabel#MissionTopTitle {{
                color: {heading};
                font-size: {fs * 1.25:.1f}pt;
                font-weight: bold;
            }}
            #MissionWidget QLabel#MissionTopSubtitle {{
                color: {muted};
                font-size: {fs * 0.88:.1f}pt;
            }}
            QCheckBox#MissionAutoResume, QCheckBox#MissionReduceMotion, QCheckBox#MissionReviewPlan {{
                color: {muted};
                font-size: {fs * 0.88:.1f}pt;
                spacing: {round(8 * zoom)}px;
                background: transparent;
            }}
            QCheckBox#MissionAutoResume:hover, QCheckBox#MissionReduceMotion:hover {{
                color: {text};
            }}
            QCheckBox#MissionAutoResume::indicator, QCheckBox#MissionReduceMotion::indicator,
            QCheckBox#MissionReviewPlan::indicator {{
                width: {round(16 * zoom)}px;
                height: {round(16 * zoom)}px;
                border: 1px solid {input_border};
                border-radius: {round(4 * zoom)}px;
                background-color: {input_bg};
            }}
            QCheckBox#MissionAutoResume::indicator:hover, QCheckBox#MissionReduceMotion::indicator:hover,
            QCheckBox#MissionAutoResume::indicator:focus, QCheckBox#MissionReduceMotion::indicator:focus,
            QCheckBox#MissionReviewPlan::indicator:hover, QCheckBox#MissionReviewPlan::indicator:focus {{
                border-color: {focus};
            }}
            QCheckBox#MissionAutoResume::indicator:checked, QCheckBox#MissionReduceMotion::indicator:checked,
            QCheckBox#MissionReviewPlan::indicator:checked {{
                background-color: {primary};
                border-color: {primary};
                image: url({sm.get_icon_path("check")});
            }}

            #MissionHero, #MissionHeaderCard, #MissionCard, #MissionInboxCard, #MissionInboxCardActive,
            #MissionLeadCard, #MissionLeadCardWaiting, #MissionClassicCard {{
                background-color: {card};
                border: 1px solid {line};
                border-radius: {radius}px;
            }}
            #MissionClassicCard {{
                background-color: {blend(tone_color(MissionTone.ACCENT), 0.04, card_color).name()};
                border: 1px solid {tone_rgba(MissionTone.ACCENT, 0.30)};
            }}
            #MissionInboxCardActive {{
                border: 2px solid {tone_rgba(MissionTone.WARNING, 0.85)};
            }}
            #MissionGlance {{
                background-color: {blend(tone_color(MissionTone.ACCENT), 0.045, card_color).name()};
                border: 1px solid {tone_rgba(MissionTone.ACCENT, 0.16)};
                border-radius: {round(12 * zoom)}px;
            }}
            #MissionAttention {{
                background-color: {warning_soft};
                border: 1px solid {tone_rgba(MissionTone.WARNING, 0.6)};
                border-radius: {radius}px;
            }}
            #MissionWorkingOn {{
                background-color: {accent_soft};
                border: 1px solid {accent_line};
                border-radius: {radius}px;
            }}
            #MissionWidget QLabel#MissionWorkingOnText {{
                color: {heading};
                font-weight: bold;
            }}
            #MissionWidget QLabel#MissionGlanceCaption {{
                color: {muted};
                font-size: {fs * 0.75:.1f}pt;
                font-weight: bold;
                letter-spacing: 1px;
            }}
            #MissionWidget QLabel#MissionGlanceValue {{
                color: {heading};
                font-size: {fs * 0.95:.1f}pt;
            }}
            #MissionWidget QLabel#MissionAttentionText {{
                color: {heading};
                font-weight: bold;
            }}
            QPushButton#MissionStopButton {{
                background-color: {card};
                color: {readable(tone_color(MissionTone.DANGER), card_color).name()};
                border: 1px solid {tone_rgba(MissionTone.DANGER, 0.45)};
            }}
            QPushButton#MissionStopButton:hover {{
                background-color: {blend(tone_color(MissionTone.DANGER), 0.08, card_color).name()};
            }}
            QPushButton#MissionStopButton:pressed {{
                background-color: {blend(tone_color(MissionTone.DANGER), 0.16, card_color).name()};
            }}
            QPushButton#MissionStopButton:focus {{
                border: 2px solid {focus};
            }}
            #MissionReportCard {{
                background-color: {card};
                border: 1px solid {line};
                border-top: {round(4 * zoom)}px solid {tone_color(MissionTone.SUCCESS).name()};
                border-radius: {radius}px;
            }}
            #MissionReportDivider {{
                background-color: {line};
            }}
            #MissionWidget QLabel#MissionReportCaption {{
                color: {readable(tone_color(MissionTone.SUCCESS), card_color).name()};
                font-size: {fs * 0.78:.1f}pt;
                font-weight: bold;
                letter-spacing: 1.5px;
            }}
            #MissionWidget QLabel#MissionReportSection {{
                color: {muted};
                font-size: {fs * 0.76:.1f}pt;
                font-weight: bold;
                letter-spacing: 1.5px;
            }}
            #MissionWidget QLabel#MissionReportBody {{
                color: {text};
                font-size: {fs * 1.0:.1f}pt;
            }}
            #MissionReportPin {{
                background-color: qlineargradient(x1:0, y1:0, x2:1, y2:0,
                    stop:0 {blend(tone_color(MissionTone.SUCCESS), 0.14, card_color).name()},
                    stop:1 {blend(tone_color(MissionTone.SUCCESS), 0.08, card_color).name()});
                border: none;
                border-left: 1px solid {tone_rgba(MissionTone.SUCCESS, 0.55)};
                border-right: 1px solid {tone_rgba(MissionTone.SUCCESS, 0.55)};
                border-bottom: 1px solid {tone_rgba(MissionTone.SUCCESS, 0.40)};
            }}
            #MissionReportPinShadow {{
                background-color: qlineargradient(x1:0, y1:0, x2:0, y2:1,
                    stop:0 rgba(0, 0, 0, 30), stop:1 rgba(0, 0, 0, 0));
            }}
            #MissionWidget QLabel#MissionReportPinTitle {{
                color: {heading};
                font-size: {fs * 1.08:.1f}pt;
                font-weight: bold;
            }}
            #MissionWidget QLabel#MissionReportPinStats {{
                color: {readable(tone_color(MissionTone.SUCCESS), card_color).name()};
                background-color: {tone_rgba(MissionTone.SUCCESS, 0.12)};
                border: 1px solid {tone_rgba(MissionTone.SUCCESS, 0.30)};
                border-radius: {round(11 * zoom)}px;
                padding: {round(3 * zoom)}px {round(10 * zoom)}px;
                font-size: {fs * 0.88:.1f}pt;
                font-weight: 600;
            }}
            #MissionWidget QLabel#MissionReportTitle {{
                color: {text};
                font-size: {fs * 1.5:.1f}pt;
                font-weight: bold;
            }}
            #MissionWidget QLabel#MissionReportStats {{
                color: {muted};
                font-size: {fs * 0.9:.1f}pt;
            }}
            #MissionLeadCardWaiting {{
                background-color: {warning_soft};
                border: 1px solid {tone_rgba(MissionTone.WARNING, 0.55)};
            }}

            QPushButton#MissionNewChip, QPushButton#MissionNewChipSelected {{
                min-height: {round(22 * zoom)}px;
                border-radius: {round(13 * zoom)}px;
                padding: {round(5 * zoom)}px {round(14 * zoom)}px;
                font-size: {fs * 0.92:.1f}pt;
                text-align: left;
                background-color: {card};
                color: {text};
                border: 1px solid {line};
            }}
            QPushButton#MissionNewChip:hover {{
                border-color: {accent_line};
            }}
            QPushButton#MissionNewChipSelected {{
                background-color: {primary};
                color: {on_primary};
                border: 1px solid {primary};
                font-weight: 600;
            }}
            #MissionChipGroup, #MissionChipGroupSelected {{
                background-color: {card};
                border: 1px solid {line};
                border-radius: {round(14 * zoom)}px;
            }}
            #MissionChipGroup:hover {{
                border-color: {accent_line};
            }}
            #MissionChipGroupSelected {{
                background-color: {primary};
                border: 1px solid {primary};
            }}
            QPushButton#MissionChipLabel, QPushButton#MissionChipMenu {{
                background: transparent;
                border: 1px solid transparent;
                color: {text};
                min-height: {round(22 * zoom)}px;
                font-size: {fs * 0.92:.1f}pt;
            }}
            QPushButton#MissionChipLabel {{
                border-top-left-radius: {round(13 * zoom)}px;
                border-bottom-left-radius: {round(13 * zoom)}px;
                padding: {round(5 * zoom)}px {round(6 * zoom)}px {round(5 * zoom)}px {round(14 * zoom)}px;
                text-align: left;
            }}
            #MissionChipStrip {{
                background: transparent;
            }}
            QPushButton#MissionChipArrow {{
                min-width: {round(28 * zoom)}px;
                max-width: {round(28 * zoom)}px;
                min-height: {round(28 * zoom)}px;
                max-height: {round(28 * zoom)}px;
                padding: 0px;
                border-radius: {round(14 * zoom)}px;
                background-color: {secondary};
                border: 1px solid transparent;
                color: {text};
                font-size: {fs * 1.25:.1f}pt;
                font-weight: bold;
            }}
            QPushButton#MissionChipArrow:hover {{
                background-color: {secondary_hover};
            }}
            QPushButton#MissionChipArrow:pressed {{
                background-color: {secondary_pressed};
            }}
            QPushButton#MissionChipArrow:focus {{
                border: 2px solid {focus};
            }}
            QPushButton#MissionChipArrow:disabled {{
                color: {disabled_text};
                background-color: {disabled_fill};
            }}
            QPushButton#MissionChipMenu {{
                border-top-right-radius: {round(13 * zoom)}px;
                border-bottom-right-radius: {round(13 * zoom)}px;
                padding: {round(5 * zoom)}px {round(11 * zoom)}px;
                font-weight: bold;
                color: {muted};
            }}
            QPushButton#MissionChipMenu:hover {{
                background-color: {hover};
                color: {text};
            }}
            #MissionChipGroupSelected QPushButton#MissionChipLabel, #MissionChipGroupSelected QPushButton#MissionChipMenu {{
                color: {on_primary};
                font-weight: 600;
            }}
            #MissionChipGroupSelected QPushButton#MissionChipMenu:hover {{
                background-color: {tone_rgba(MissionTone.NEUTRAL, 0.25)};
            }}

            #MissionWidget QLabel#MissionHeroTitle {{
                color: {heading};
                font-size: {fs * 2.0:.1f}pt;
                font-weight: bold;
            }}
            #MissionWidget QLabel#MissionHeroSubtitle {{
                color: {muted};
                font-size: {fs * 1.02:.1f}pt;
            }}
            #MissionWidget QLabel#MissionExamplesLabel, #MissionWidget QLabel#MissionFieldLabel {{
                color: {muted};
                font-size: {fs * 0.8:.1f}pt;
                font-weight: 600;
                letter-spacing: 1px;
            }}
            QPushButton#MissionExampleChip {{
                background-color: {page};
                color: {text};
                border: 1px solid {line};
                border-radius: {round(14 * zoom)}px;
                padding: {round(7 * zoom)}px {round(18 * zoom)}px;
                font-size: {fs * 0.95:.1f}pt;
            }}
            QPushButton#MissionExampleChip:hover {{
                border-color: {accent_line};
                color: {heading};
            }}
            QToolButton#MissionTemplateRemove {{
                background: transparent;
                color: {muted};
                border: none;
                border-radius: {round(10 * zoom)}px;
                padding: {round(3 * zoom)}px {round(6 * zoom)}px;
            }}
            QToolButton#MissionTemplateRemove:hover {{
                color: {readable(tone_color(MissionTone.DANGER), card_color).name()};
                background-color: {blend(tone_color(MissionTone.DANGER), 0.08, card_color).name()};
            }}

            #MissionWidget QLabel#MissionCaption {{
                color: {accent_text.name()};
                font-size: {fs * 0.76:.1f}pt;
                font-weight: bold;
                letter-spacing: 2px;
            }}
            #MissionWidget QLabel#MissionTitle {{
                color: {text};
                font-size: {fs * 1.75:.1f}pt;
                font-weight: bold;
            }}
            #MissionWidget QLabel#MissionSummary {{
                color: {muted};
            }}
            #MissionWidget QLabel#MissionStepsLabel {{
                color: {text};
                font-size: {fs * 1.05:.1f}pt;
                font-weight: bold;
            }}
            #MissionGuide {{
                background-color: {blend(tone_color(MissionTone.ACCENT), 0.05, card_color).name()};
                border: 1px solid {tone_rgba(MissionTone.ACCENT, 0.22)};
                border-radius: {radius}px;
            }}
            #MissionFileRow {{
                background: transparent;
                border-bottom: 1px solid {line};
            }}
            #MissionWidget QLabel#MissionFilePath {{
                color: {text};
                font-family: "{mono}";
                font-size: {fs * 0.9:.1f}pt;
                font-weight: 600;
            }}
            QToolButton#MissionFileAction {{
                background-color: {card};
                border: 1px solid {input_border};
                border-radius: 7px;
                padding: 0;
            }}
            QToolButton#MissionFileAction:hover {{
                background-color: {secondary_hover};
            }}
            QToolButton#MissionFileAction:pressed {{
                background-color: {secondary_pressed};
            }}
            QToolButton#MissionFileAction:focus {{
                border: 2px solid {focus};
            }}
            QPlainTextEdit#MissionChecksInput {{
                background-color: {card};
                color: {text};
                border: 1px solid {input_border};
                border-radius: 8px;
                padding: 6px 8px;
                font-size: {fs * 0.9:.1f}pt;
                max-height: {round(96 * zoom)}px;
            }}
            #MissionFileDiff {{
                border: 1px solid {line};
                border-radius: 6px;
                background: {card};
            }}
            #MissionWidget QLabel#MissionFileDiffNote {{
                color: {muted};
                font-size: {fs * 0.85:.1f}pt;
                padding: 6px 10px;
            }}
            #MissionWidget QLabel#MissionFileChevron {{
                color: {muted};
                font-size: {fs * 0.7:.1f}pt;
            }}
            #MissionWidget QLabel#MissionFileLines {{
                font-family: "{mono}";
                font-size: {fs * 0.85:.1f}pt;
                font-weight: 600;
            }}
            #MissionWidget QLabel#MissionTimingText {{
                color: {muted};
                font-size: {fs * 0.85:.1f}pt;
            }}
            #MissionWidget QLabel#MissionProgressLabel {{
                color: {muted};
                font-size: {fs * 0.85:.1f}pt;
            }}
            #MissionWidget QLabel#MissionNotice {{
                color: {text};
                background-color: {warning_soft};
                border: 1px solid {tone_rgba(MissionTone.WARNING, 0.55)};
                border-radius: {small_radius}px;
                padding: {round(8 * zoom)}px {round(12 * zoom)}px;
                font-size: {fs * 0.92:.1f}pt;
            }}
            #MissionWidget QLabel#MissionFailedNotice {{
                color: {text};
                background-color: {danger_soft};
                border: 1px solid {tone_rgba(MissionTone.DANGER, 0.55)};
                border-radius: {small_radius}px;
                padding: {round(8 * zoom)}px {round(12 * zoom)}px;
                font-size: {fs * 0.92:.1f}pt;
            }}

            #MissionSegment {{
                background-color: {page};
                border: 1px solid {line};
                border-radius: {small_radius + 2}px;
            }}
            QPushButton#MissionSegmentButton {{
                background: transparent;
                color: {muted};
                border: 1px solid transparent;
                border-radius: {small_radius}px;
                padding: {round(4 * zoom)}px {round(14 * zoom)}px;
                font-size: {fs * 0.88:.1f}pt;
                font-weight: 600;
            }}
            QPushButton#MissionSegmentButton:hover {{
                color: {text};
            }}
            QPushButton#MissionSegmentButton:checked {{
                background-color: {card};
                color: {heading};
                border: 1px solid {line};
            }}

            QPushButton#MissionGhostButton, QPushButton#MissionSecondaryButton,
            QPushButton#MissionPauseButton, QPushButton#MissionApprove,
            QPushButton#MissionApproveDestructive, QPushButton#MissionReplyButton, QPushButton#MissionStopTaskButton,
            QPushButton#MissionStopButton, QPushButton#MissionButtonProbe {{
                min-height: {control_height - round(14 * zoom)}px;
                border-radius: {small_radius}px;
                padding: {round(6 * zoom)}px {round(14 * zoom)}px;
                font-size: {fs * 0.9:.1f}pt;
                font-weight: 600;
            }}
            QPushButton#MissionGhostButton, QPushButton#MissionSecondaryButton {{
                background-color: {secondary};
                color: {text};
                border: 1px solid transparent;
            }}
            QPushButton#MissionPauseButton {{
                background-color: {card};
                color: {text};
                border: 1px solid {input_border};
            }}
            QPushButton#MissionGhostButton:hover, QPushButton#MissionSecondaryButton:hover,
            QPushButton#MissionPauseButton:hover {{
                background-color: {secondary_hover};
            }}
            QPushButton#MissionGhostButton:pressed, QPushButton#MissionSecondaryButton:pressed,
            QPushButton#MissionPauseButton:pressed {{
                background-color: {secondary_pressed};
            }}
            QPushButton#MissionApprove, QPushButton#MissionReplyButton {{
                background-color: {primary};
                color: {on_primary};
                border: 1px solid {primary};
            }}
            QPushButton#MissionApprove:hover, QPushButton#MissionReplyButton:hover {{
                background-color: {primary_hover};
            }}
            QPushButton#MissionApprove:pressed, QPushButton#MissionReplyButton:pressed {{
                background-color: {primary_pressed};
            }}
            QPushButton#MissionApproveDestructive {{
                background-color: {danger_fill};
                color: {on_primary};
                border: 1px solid {danger_fill};
            }}
            QPushButton#MissionApproveDestructive:hover {{
                background-color: {danger_hover};
            }}
            QPushButton#MissionApproveDestructive:pressed {{
                background-color: {danger_pressed};
            }}
            QPushButton#MissionStopTaskButton {{
                background-color: {danger_fill};
                color: {on_primary};
                border: 1px solid {danger_fill};
            }}
            QPushButton#MissionStopTaskButton:hover {{
                background-color: {danger_hover};
            }}
            QPushButton#MissionStopTaskButton:pressed {{
                background-color: {danger_pressed};
            }}
            QPushButton#MissionGhostButton:disabled, QPushButton#MissionSecondaryButton:disabled,
            QPushButton#MissionPauseButton:disabled,
            QPushButton#MissionApprove:disabled, QPushButton#MissionApproveDestructive:disabled,
            QPushButton#MissionReplyButton:disabled, QPushButton#MissionStopTaskButton:disabled,
            QPushButton#MissionStopButton:disabled {{
                background-color: {disabled_fill};
                color: {disabled_text};
                border: 1px solid {disabled_fill};
            }}
            QPushButton#MissionGhostButton:focus, QPushButton#MissionSecondaryButton:focus,
            QPushButton#MissionPauseButton:focus,
            QPushButton#MissionApprove:focus, QPushButton#MissionApproveDestructive:focus,
            QPushButton#MissionReplyButton:focus, QPushButton#MissionStopTaskButton:focus,
            QPushButton#MissionSegmentButton:focus, QPushButton#MissionChipLabel:focus,
            QPushButton#MissionChipMenu:focus,
            QPushButton#MissionNewChip:focus, QPushButton#MissionExampleChip:focus {{
                border: 2px solid {focus};
            }}

            #MissionWidget QLabel#MissionCardTitle {{
                color: {text};
                font-size: {fs * 1.1:.1f}pt;
                font-weight: bold;
            }}
            #MissionWidget QLabel#MissionHintText, #MissionWidget QLabel#MissionEmptyText {{
                color: {muted};
                font-size: {fs * 0.86:.1f}pt;
            }}
            QLineEdit#MissionSearch {{
                background-color: {input_bg};
                color: {text};
                border: 1px solid {input_border};
                border-radius: {round(12 * zoom)}px;
                padding: {round(4 * zoom)}px {round(10 * zoom)}px;
                font-size: {fs * 0.88:.1f}pt;
            }}
            QLineEdit#MissionSearch:focus {{
                border-color: {focus};
            }}
            QPushButton#MissionPlanFilter:checked {{
                background-color: {accent_soft};
                color: {heading};
                border-color: {accent_line};
            }}
            QPushButton#MissionPlanFilter, QPushButton#MissionPlanFilterActive {{
                background-color: transparent;
                color: {muted};
                border: 1px solid {line};
                border-radius: {round(11 * zoom)}px;
                padding: {round(3 * zoom)}px {round(10 * zoom)}px;
                font-size: {fs * 0.82:.1f}pt;
                font-weight: 600;
            }}
            QPushButton#MissionPlanFilter:hover {{
                color: {heading};
                border-color: {accent_line};
            }}
            QPushButton#MissionPlanFilterActive {{
                background-color: {accent_soft};
                color: {heading};
                border-color: {accent_line};
            }}
            QPushButton#MissionPlanFilter:focus, QPushButton#MissionPlanFilterActive:focus {{
                border: 2px solid {focus};
            }}
            #MissionWidget QLabel#MissionClassicTitle {{
                color: {heading};
                font-size: {fs * 1.15:.1f}pt;
                font-weight: bold;
            }}

            QPushButton#MissionHistoryButton {{
                background-color: {secondary};
                color: {text};
                border: 1px solid transparent;
                border-radius: {round(8 * zoom)}px;
                padding: {round(3 * zoom)}px {round(10 * zoom)}px;
                font-size: {fs * 0.82:.1f}pt;
                font-weight: 600;
            }}
            QPushButton#MissionHistoryButton:hover {{
                background-color: {secondary_hover};
            }}
            QPushButton#MissionHistoryButton:checked {{
                background-color: {primary};
                color: {on_primary};
            }}
            #MissionChatUser {{
                background-color: {sm.get_color_str(ColorRole.MESSAGE_USER_BACKGROUND)};
                border: 1px solid {sm.get_color_str(ColorRole.MESSAGE_USER_BORDER)};
                border-radius: {small_radius}px;
            }}
            #MissionWidget QLabel#MissionChatModel {{
                background-color: {accent_soft};
                border: 1px solid {tone_rgba(MissionTone.ACCENT, 0.25)};
                border-radius: {round(8 * zoom)}px;
                padding: 0px {round(6 * zoom)}px;
                color: {heading};
                font-size: {fs * 0.8:.1f}pt;
            }}
            #MissionChatLead {{
                background-color: {sm.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
                border: 1px solid {line};
                border-radius: {small_radius}px;
            }}
            #MissionHistoryPanel {{
                background-color: {page};
                border: 1px solid {line};
                border-radius: {round(8 * zoom)}px;
            }}
            #MissionPhaseRow, #MissionPhaseRowOpen {{
                background-color: {card};
                border: 1px solid {line};
                border-radius: {small_radius}px;
            }}
            #MissionPhaseRow:hover {{
                background-color: {hover};
            }}
            #MissionPhaseRowOpen {{
                border-color: {tone_rgba(MissionTone.ACCENT, 0.25)};
            }}

            #MissionStepRow, #MissionStepRowOpen {{
                background: transparent;
                border: 1px solid transparent;
                border-radius: {small_radius}px;
            }}
            #MissionStepRow:hover {{
                background-color: {hover};
            }}
            #MissionStepRowOpen {{
                background-color: {blend(tone_color(MissionTone.ACCENT), 0.05, card_color).name()};
                border-color: {tone_rgba(MissionTone.ACCENT, 0.25)};
            }}
            #MissionWidget QLabel#MissionStepTitle {{
                font-weight: 600;
            }}
            #MissionWidget QLabel#MissionStepTitleSkipped {{
                font-weight: 600;
                color: {muted};
                text-decoration: line-through;
            }}
            #MissionWidget QLabel#MissionStepHandle {{
                color: {muted};
                font-size: {fs * 1.15:.1f}pt;
                padding: 0 2px;
            }}
            #MissionStepRow[dropEdge="top"], #MissionStepRowOpen[dropEdge="top"] {{
                border-top: 2px solid {focus};
            }}
            #MissionStepRow[dropEdge="bottom"], #MissionStepRowOpen[dropEdge="bottom"] {{
                border-bottom: 2px solid {focus};
            }}
            #MissionWidget QLabel#MissionStepDetail {{
                color: {muted};
                font-size: {fs * 0.88:.1f}pt;
            }}
            #MissionStepProgress {{
                background-color: {card};
                border: 1px solid {line};
                border-radius: {round(8 * zoom)}px;
            }}
            #MissionWidget QLabel#MissionStepSection {{
                color: {link};
                font-size: {fs * 0.72:.1f}pt;
                font-weight: bold;
                letter-spacing: 1px;
            }}
            #MissionWidget QLabel#MissionStepProgressText {{
                font-size: {fs * 0.92:.1f}pt;
            }}
            #MissionWidget QLabel#MissionStepChevron {{
                color: {muted};
                font-size: {fs * 0.7:.1f}pt;
            }}

            #MissionAgentRow {{
                background: transparent;
                border: 1px solid transparent;
                border-bottom: 1px solid {line};
                border-radius: 0px;
            }}
            #MissionAgentRow:hover {{
                background-color: {hover};
            }}
            #MissionWidget QLabel#MissionAgentTitle {{
                font-weight: 600;
            }}
            #MissionWidget QLabel#MissionAgentRole {{
                color: {muted};
                font-size: {fs * 0.84:.1f}pt;
            }}

            #MissionDecisionCard, #MissionDecisionCardDestructive {{
                background-color: {page};
                border: 1px solid {line};
                border-radius: {small_radius}px;
            }}
            #MissionDecisionCardHighlight {{
                background-color: {blend(tone_color(MissionTone.WARNING), 0.16, page_color).name()};
                border: 2px solid {tone_rgba(MissionTone.WARNING, 0.9)};
                border-radius: {small_radius}px;
            }}
            #MissionDecisionCardDestructive {{
                background-color: {danger_soft};
                border: 1px solid {tone_rgba(MissionTone.DANGER, 0.45)};
            }}
            #MissionDecisionCard[selected="true"], #MissionDecisionCardDestructive[selected="true"] {{
                border: 2px solid {focus};
            }}
            #MissionWidget QLabel#MissionDecisionTool {{
                color: {heading};
                font-weight: bold;
            }}
            #MissionWidget QLabel#MissionDecisionReason {{
                font-size: {fs * 0.92:.1f}pt;
            }}
            #MissionWidget QLabel#MissionDecisionTarget {{
                color: {heading};
                font-family: "{mono}";
                font-size: {fs * 0.86:.1f}pt;
            }}
            #MissionWidget QLabel#MissionDecisionUndo {{
                color: {heading};
                font-size: {fs * 0.86:.1f}pt;
                font-weight: bold;
            }}
            #MissionWidget QLabel#MissionReviewNote {{
                color: {muted_on_page};
                font-size: {fs * 0.86:.1f}pt;
                font-style: italic;
            }}
            #MissionWidget QLabel#MissionDecisionDetail {{
                background-color: {input_bg};
                border: 1px solid {line};
                border-radius: {round(6 * zoom)}px;
                padding: {round(8 * zoom)}px;
                font-family: "{mono}";
                font-size: {fs * 0.85:.1f}pt;
            }}
            #MissionWidget QLabel#MissionTimeText {{
                color: {muted};
                font-size: {fs * 0.8:.1f}pt;
            }}
            QPushButton#MissionLinkButton {{
                background: transparent;
                border: none;
                color: {link_on_page};
                padding: 0px;
                font-size: {fs * 0.86:.1f}pt;
                text-align: left;
            }}
            QPushButton#MissionLinkButton:hover {{
                color: {heading};
            }}

            #MissionActivityScroll, #MissionActivityFeed, #MissionChatScroll, #MissionChatFeed {{
                background: transparent;
            }}
            #MissionWidget QLabel#MissionActivityText {{
                font-size: {fs * 0.9:.1f}pt;
            }}
            #MissionWidget QLabel#MissionResultHeading {{
                color: {heading};
                font-weight: bold;
            }}
            #MissionActivityRow, #MissionActivityRowOpen {{
                border: 1px solid transparent;
                border-radius: {round(6 * zoom)}px;
            }}
            #MissionActivityRow:hover {{
                background-color: {hover};
                border-color: {line};
            }}
            #MissionActivityRowOpen {{
                background-color: {accent_soft};
                border-color: {accent_line};
            }}
            #MissionWidget QLabel#MissionActivityMore {{
                background-color: {input_bg};
                border: 1px solid {line};
                border-radius: {round(6 * zoom)}px;
                padding: {round(8 * zoom)}px;
                font-size: {fs * 0.86:.1f}pt;
            }}

            #MissionComposer {{
                background-color: {input_bg};
                border: 1px solid {input_border};
                border-radius: {radius + 4}px;
            }}
            QPlainTextEdit#MissionComposerInput {{
                background: transparent;
                border: none;
                color: {text};
                font-size: {fs * 1.05:.1f}pt;
                padding: 0px;
            }}
            QToolButton#MissionComposerTool, #MissionComposer QToolButton#_attachment_remove {{
                background-color: transparent;
                border: none;
                border-radius: {round(6 * zoom)}px;
                padding: {round(4 * zoom)}px;
            }}
            QToolButton#MissionComposerTool:hover, #MissionComposer QToolButton#_attachment_remove:hover {{
                background-color: {sm.get_color_str(ColorRole.MESSAGE_BACKGROUND_HOVER)};
            }}
            QToolButton#MissionComposerTool:pressed, #MissionComposer QToolButton#_attachment_remove:pressed {{
                background-color: {sm.get_color_str(ColorRole.MESSAGE_BACKGROUND_PRESSED)};
            }}
            QToolButton#MissionComposerTool:focus, #MissionComposer QToolButton#_attachment_remove:focus {{
                border: 1px solid {focus};
            }}
            #MissionComposer #_attachment_widget {{
                background-color: {sm.get_color_str(ColorRole.MESSAGE_ATTACHMENT_BACKGROUND)};
                border: 1px solid {input_border};
                border-radius: {round(4 * zoom)}px;
            }}
            #MissionComposer #_attachment_label {{
                background: transparent;
                color: {text};
            }}
            QPushButton#MissionSendButton {{
                background-color: {primary};
                color: {on_primary};
                border: 2px solid transparent;
                border-radius: {round(19 * zoom)}px;
                min-width: {round(34 * zoom)}px;
                min-height: {round(34 * zoom)}px;
                padding: 0px;
            }}
            QPushButton#MissionSendButton:hover {{
                background-color: {primary_hover};
            }}
            QPushButton#MissionSendButton:pressed {{
                background-color: {primary_pressed};
            }}
            QPushButton#MissionSendButton:focus {{
                border-color: {focus};
            }}
            QPushButton#MissionSendButton:disabled {{
                background-color: {disabled_fill};
                color: {disabled_text};
            }}

            #MissionStepRow:focus, #MissionStepRowOpen:focus, #MissionPhaseRow:focus,
            #MissionPhaseRowOpen:focus, #MissionAgentRow:focus, #MissionActivityRow:focus,
            #MissionActivityRowOpen:focus {{
                border: 2px solid {focus};
            }}

            {sm.get_scrollbar_stylesheet("#MissionScroll QScrollBar")}
            {sm.get_scrollbar_stylesheet("#MissionActivityScroll QScrollBar")}
            {sm.get_scrollbar_stylesheet("#MissionChatScroll QScrollBar")}
            {sm.get_scrollbar_stylesheet("#MissionComposerInput QScrollBar")}
        """
        return _scope_to_widget(stylesheet)
