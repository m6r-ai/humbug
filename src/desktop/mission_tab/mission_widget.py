"""Mission Control dashboard widget."""

import re
import sys
from collections.abc import Callable, Hashable

from PySide6.QtCore import (
    QEasingCurve, QEvent, QObject, QPoint, QPointF, QPropertyAnimation, QSize, Qt, QTimer, QVariantAnimation, Signal,
    SignalInstance
)
from PySide6.QtGui import (
    QColor, QHideEvent, QIcon, QKeyEvent, QMouseEvent, QPainter, QPainterPath, QPen, QPixmap, QResizeEvent,
    QLinearGradient, QPaintEvent, QShowEvent, QTextCursor, QWheelEvent
)
from PySide6.QtWidgets import (
    QApplication, QBoxLayout, QCheckBox, QFrame, QGridLayout, QHBoxLayout, QLabel, QLayout, QPlainTextEdit, QPushButton,
    QScrollArea, QScroller, QSizePolicy, QToolButton, QVBoxLayout, QWidget
)

from desktop.color_role import ColorRole
from desktop.language.language_manager import LanguageManager
from desktop.mission_tab.mission_display import (
    MissionActivityView, MissionAgentView, MissionChatView, MissionChipView, MissionDecisionView, MissionGlanceView,
    MissionPhaseView, MissionResultView, MissionStatTarget, MissionStatView, MissionStepState, MissionStepView,
    MissionTone, MissionView
)
from desktop.mission_tab.mission_motion import MissionMotion
from desktop.mission_tab.mission_network import AgentNetworkView
from desktop.mission_tab.mission_scene import ConfettiOverlay, MissionJourney, MissionOrb
from desktop.mission_tab.mission_painted import (
    BusySpinner, GlyphBadge, StatusPill, StepMarker, blend, fade_in, readable, surface_colors,
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
_CHAT_MAX_HEIGHT = 560
_COMPOSER_MAX_LINES = 6

# The symbol beside each line of the mission at a glance.
_GLANCE_GLYPHS = {"now": "▶", "blocking": "!", "approvals": "✋", "done": "✓"}


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
        self._heights.clear()
        super().setText(text)
        self._fit_height()

    def setTextFormat(self, text_format: Qt.TextFormat) -> None:
        """Interpret the text in a new format, which changes the height it needs."""
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
        if self.width() <= 0:
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


class _FittedScroll(QScrollArea):
    """
    A scroll area as tall as its content, up to its maximum height, that scrolls beyond that.

    Like a conversation, it keeps the newest content at the bottom in view, unless the
    user has scrolled up to read something earlier.
    """

    def __init__(self, content: QWidget, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self.setFrameShape(QFrame.Shape.NoFrame)
        self.setWidgetResizable(True)
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.setSizePolicy(QSizePolicy.Policy.Preferred, QSizePolicy.Policy.Fixed)
        self.setWidget(content)
        content.installEventFilter(self)
        self._follow = True
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

    def _on_range_changed(self, _minimum: int, maximum: int) -> None:
        """Keep the newest content in view when more arrives."""
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
    auto_approve_selected = Signal(str)
    new_mission_auto_approve_selected = Signal(str)
    auto_resume_toggled = Signal(bool)
    reduce_motion_toggled = Signal(bool)
    stop_requested = Signal()
    remove_requested = Signal()
    mission_rename_requested = Signal(str)
    mission_delete_requested = Signal(str)
    conversation_requested = Signal(str)
    decision_approved = Signal(str)
    decision_unsure = Signal(str)
    decision_rejected = Signal(str)
    wait_finish_requested = Signal(str)

    def __init__(self, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self.setObjectName("MissionWidget")
        self._style_manager = StyleManager()
        self._language_manager = LanguageManager()

        self._chips: list[MissionChipView] | None = None
        self._steps: list[MissionStepView] | None = None
        self._history: list[MissionPhaseView] | None = None
        self._attachments: list[tuple[str, str]] = []
        self._agents: list[MissionAgentView] | None = None
        self._decisions: list[MissionDecisionView] | None = None
        self._activity: list[MissionActivityView] | None = None
        self._stats: list[MissionStatView] | None = None
        self._pills: list[StatusPill] = []
        self._spinners: list[BusySpinner] = []
        self._stat_cards: list[QFrame] = []
        self._stat_columns = 0
        self._has_mission = False
        # The details below the mission at a glance are shown until the user folds them away.
        self._details_shown = True
        self._glance_shown = False
        self._view: MissionView | None = None
        self._new_mission_selected = True
        self._new_chip: QPushButton | None = None
        self._awaiting_reply = False
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
        self._stat_values: dict[MissionStatTarget, str] = {}
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
        self._glance_card = self._build_glance_card()
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

        root.addWidget(self._build_composer())
        self._confetti = ConfettiOverlay(self)

        self.retranslate()
        self.set_mission(None)
        self.set_decisions([])

    def _chip_arrow(self, glyph: str, direction: int) -> QPushButton:
        """Create an arrow that scrolls the mission chips one way."""
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
        self._auto_resume = QCheckBox()
        self._auto_resume.setObjectName("MissionAutoResume")
        self._auto_resume.setCursor(Qt.CursorShape.PointingHandCursor)
        self._auto_resume.toggled.connect(self.auto_resume_toggled)
        layout.addWidget(self._auto_resume)
        self._reduce_motion = QCheckBox()
        self._reduce_motion.setObjectName("MissionReduceMotion")
        self._reduce_motion.setCursor(Qt.CursorShape.PointingHandCursor)
        self._reduce_motion.toggled.connect(self.reduce_motion_toggled)
        layout.addWidget(self._reduce_motion)
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
        layout.setContentsMargins(48, 40, 48, 40)
        layout.setSpacing(12)

        self._hero_orb = MissionOrb("◎", MissionTone.ACCENT, 150)
        layout.addWidget(self._hero_orb, 0, Qt.AlignmentFlag.AlignHCenter)

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

        # The level is chosen before the mission starts, so even the lead's first request is covered.
        (
            self._hero_auto_approve_row, self._hero_auto_approve_label, self._hero_auto_approve_buttons,
            self._hero_auto_approve_note
        ) = self._auto_approve_controls(self.new_mission_auto_approve_selected.emit, centred=True)
        layout.addWidget(self._hero_auto_approve_row)
        layout.addWidget(self._hero_auto_approve_note)

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

        return hero

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

    def _build_glance_card(self) -> QWidget:
        """Create the card summing the mission up, with the switch that folds the details below it away."""
        card, layout = self._card()
        header = QHBoxLayout()
        header.setSpacing(10)
        self._glance_title = QLabel()
        self._glance_title.setObjectName("MissionCardTitle")
        header.addWidget(self._glance_title)
        header.addStretch(1)
        self._details_button = QPushButton()
        self._details_button.setObjectName("MissionLinkButton")
        self._details_button.setCursor(Qt.CursorShape.PointingHandCursor)
        self._details_button.clicked.connect(self._toggle_details)
        header.addWidget(self._details_button)
        layout.addLayout(header)

        grid = QGridLayout()
        grid.setHorizontalSpacing(18)
        grid.setVerticalSpacing(10)
        self._glance_cells: dict[str, tuple[GlyphBadge, QLabel, _WrapLabel]] = {}
        for index, (key, glyph) in enumerate(_GLANCE_GLYPHS.items()):
            cell = QHBoxLayout()
            cell.setSpacing(10)
            badge = GlyphBadge(glyph, MissionTone.NEUTRAL, 24)
            cell.addWidget(badge, 0, Qt.AlignmentFlag.AlignTop)
            texts = QVBoxLayout()
            texts.setSpacing(2)
            caption = QLabel()
            caption.setObjectName("MissionGlanceCaption")
            texts.addWidget(caption)
            value = _WrapLabel()
            value.setObjectName("MissionGlanceValue")
            texts.addWidget(value)
            cell.addLayout(texts, 1)
            grid.addLayout(cell, index // 2, index % 2)
            self._glance_cells[key] = (badge, caption, value)

        grid.setColumnStretch(0, 1)
        grid.setColumnStretch(1, 1)
        layout.addLayout(grid)
        card.hide()
        return card

    def _set_glance(self, glance: MissionGlanceView | None) -> None:
        """Show the mission at a glance, or hide it where it does not apply."""
        self._glance_card.setVisible(glance is not None)
        if glance is None:
            return

        for key, text, tone in (
            ("now", glance.now_text, MissionTone.ACCENT),
            ("blocking", glance.blocking_text, glance.blocking_tone),
            ("approvals", glance.approvals_text, glance.approvals_tone),
            ("done", glance.last_done_text, MissionTone.SUCCESS),
        ):
            badge, _caption, value = self._glance_cells[key]
            badge.set_badge(_GLANCE_GLYPHS[key], tone)
            value.setText(text)

    def _details(self, visible: bool) -> bool:
        """Return whether a detail card that would otherwise be visible is, given the details switch."""
        return visible and (self._details_shown or not self._glance_shown)

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

        header_card = QFrame()
        header_card.setObjectName("MissionHeaderCard")
        header_layout = QHBoxLayout(header_card)
        header_layout.setContentsMargins(24, 22, 24, 22)
        header_layout.setSpacing(18)
        header_layout.addWidget(GlyphBadge("◎", MissionTone.ACCENT, 52), 0, Qt.AlignmentFlag.AlignTop)

        text_column = QVBoxLayout()
        text_column.setSpacing(6)

        # Title on the left, status and progress figure on the right, so the mission's
        # state is read together with its name at any width.
        title_row = QHBoxLayout()
        title_row.setSpacing(12)
        titles = QVBoxLayout()
        titles.setSpacing(2)
        self._caption = QLabel()
        self._caption.setObjectName("MissionCaption")
        titles.addWidget(self._caption)
        self._title = QLabel()
        self._title.setObjectName("MissionTitle")
        self._title.setWordWrap(True)
        self._title.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
        self._title.setTextInteractionFlags(Qt.TextInteractionFlag.TextSelectableByMouse)
        titles.addWidget(self._title)
        title_row.addLayout(titles, 1)
        status_column = QVBoxLayout()
        status_column.setSpacing(4)
        status_column.setAlignment(Qt.AlignmentFlag.AlignTop | Qt.AlignmentFlag.AlignRight)
        self._status_pill = StatusPill()
        self._pills.append(self._status_pill)
        status_column.addWidget(self._status_pill, 0, Qt.AlignmentFlag.AlignRight)
        self._progress_label = QLabel()
        self._progress_label.setObjectName("MissionProgressLabel")
        self._progress_label.setAlignment(Qt.AlignmentFlag.AlignRight)
        status_column.addWidget(self._progress_label, 0, Qt.AlignmentFlag.AlignRight)
        title_row.addLayout(status_column)
        text_column.addLayout(title_row)

        # Where the mission is on its way from the user's request to the AI's report.
        self._journey = MissionJourney()
        text_column.addWidget(self._journey)

        # What is happening right now, in the lead's own words.
        self._now_row = QFrame()
        self._now_row.setObjectName("MissionNowRow")
        now_layout = QHBoxLayout(self._now_row)
        now_layout.setContentsMargins(12, 8, 12, 8)
        now_layout.setSpacing(10)
        self._now_badge = GlyphBadge("★", MissionTone.ACCENT, 24)
        now_layout.addWidget(self._now_badge, 0, Qt.AlignmentFlag.AlignTop)
        self._now_text = QLabel()
        self._now_text.setObjectName("MissionNowText")
        self._now_text.setWordWrap(True)
        self._now_text.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
        now_layout.addWidget(self._now_text, 1)
        text_column.addWidget(self._now_row)

        # How far a mission that is still going has got: a step done, never the mission done.
        self._milestone_row = QFrame()
        self._milestone_row.setObjectName("MissionMilestoneRow")
        milestone_layout = QHBoxLayout(self._milestone_row)
        milestone_layout.setContentsMargins(12, 8, 12, 8)
        milestone_layout.setSpacing(10)
        milestone_layout.addWidget(GlyphBadge("✓", MissionTone.SUCCESS, 24), 0, Qt.AlignmentFlag.AlignTop)
        self._milestone_text = _WrapLabel()
        self._milestone_text.setObjectName("MissionMilestoneText")
        milestone_layout.addWidget(self._milestone_text, 1)
        self._milestone_row.hide()
        text_column.addWidget(self._milestone_row)

        self._summary = QLabel()
        self._summary.setObjectName("MissionSummary")
        self._summary.setWordWrap(True)
        self._summary.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
        self._summary.setTextInteractionFlags(Qt.TextInteractionFlag.TextSelectableByMouse)
        text_column.addWidget(self._summary)
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
        self._more_button.setObjectName("MissionGhostButton")
        self._more_button.setCursor(Qt.CursorShape.PointingHandCursor)
        self._more_button.clicked.connect(self._show_more_actions)
        actions.addWidget(self._more_button)
        self._can_switch_to_classic = True
        actions.addStretch(1)
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
        layout.addWidget(self._attention)

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
        layout.addWidget(header_card)
        layout.addWidget(self._glance_card)
        layout.addWidget(self._results_card)
        layout.addWidget(self._lead_card)
        layout.addWidget(self._classic_card)

        self._stats_container = QWidget()
        self._stats_grid = QGridLayout(self._stats_container)
        self._stats_grid.setContentsMargins(0, 0, 0, 0)
        self._stats_grid.setSpacing(12)
        layout.addWidget(self._stats_container)

        self._lower = QBoxLayout(QBoxLayout.Direction.LeftToRight)
        self._lower.setSpacing(16)

        self._plan_card, plan_layout = self._card()
        header, self._plan_title, self._plan_pill = self._card_header("≡", MissionTone.ACCENT)
        self._filter_chip = QPushButton()
        self._filter_chip.setObjectName("MissionFilterChip")
        self._filter_chip.setCursor(Qt.CursorShape.PointingHandCursor)
        self._filter_chip.clicked.connect(lambda: self._set_step_filter(MissionStatTarget.ALL_STEPS))
        self._filter_chip.hide()
        header.insertWidget(header.count() - 1, self._filter_chip)
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

        # The team, and what each agent is doing and waiting on, comes first; the plan follows it.
        self._lower_container = QWidget()
        self._lower_container.setLayout(self._lower)
        self._lower.setContentsMargins(0, 0, 0, 0)
        layout.addWidget(self._agents_card)
        layout.addWidget(self._lower_container)
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

        self._conversation = conversation
        self._chat_rows = _sync_rows(
            self._chat_layout, self._chat_rows, list(conversation), lambda index: self._chat_row(conversation[index])
        )

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
        layout = QHBoxLayout(card)
        layout.setContentsMargins(24, 22, 24, 22)
        layout.setSpacing(20)
        self._report_orb = MissionOrb("✓", MissionTone.SUCCESS, 120)
        layout.addWidget(self._report_orb, 0, Qt.AlignmentFlag.AlignTop)

        column = QVBoxLayout()
        column.setSpacing(6)
        self._report_caption = QLabel()
        self._report_caption.setObjectName("MissionCaption")
        column.addWidget(self._report_caption)
        self._report_title = QLabel()
        self._report_title.setObjectName("MissionReportTitle")
        self._report_title.setWordWrap(True)
        column.addWidget(self._report_title)
        self._report_stats = QLabel()
        self._report_stats.setObjectName("MissionReportStats")
        self._report_stats.setWordWrap(True)
        column.addWidget(self._report_stats)
        column.addSpacing(4)
        self._report_body = _WrapLabel()
        self._report_body.setObjectName("MissionLeadText")
        self._report_body.setTextFormat(Qt.TextFormat.MarkdownText)
        self._report_body.setTextInteractionFlags(Qt.TextInteractionFlag.TextSelectableByMouse)
        self._report_body.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
        column.addWidget(self._report_body)

        actions = QHBoxLayout()
        actions.setSpacing(8)
        actions.setContentsMargins(0, 8, 0, 0)
        self._follow_up_button = QPushButton()
        self._follow_up_button.setObjectName("MissionReplyButton")
        self._follow_up_button.setCursor(Qt.CursorShape.PointingHandCursor)
        self._follow_up_button.clicked.connect(self.focus_composer)
        actions.addWidget(self._follow_up_button)
        self._report_new_button = QPushButton()
        self._report_new_button.setObjectName("MissionGhostButton")
        self._report_new_button.setCursor(Qt.CursorShape.PointingHandCursor)
        self._report_new_button.clicked.connect(self.new_mission_requested)
        actions.addWidget(self._report_new_button)
        actions.addStretch(1)
        column.addLayout(actions)
        layout.addLayout(column, 1)
        card.hide()
        return card

    def _set_report(self, view: MissionView) -> None:
        """Show the final report when the mission is done, celebrating the moment it finishes."""
        completed = view.completed
        self._report_card.setVisible(completed)
        if completed:
            text = view.report_message
            self._report_body.setText(text)
            self._report_body.setVisible(bool(text))
            self._report_stats.setText(view.report_text)
            # The owner says when to celebrate: once per mission, and only when the user watched it finish.
            if view.celebrate:
                fade_in(self._report_card, 420)
                orb = self._report_orb
                self._confetti.burst(QPointF(orb.mapTo(self, orb.rect().center())))

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

    def _build_inbox(self) -> QWidget:
        """Create the decision inbox card."""
        card, layout = self._card("MissionInboxCard")
        header, self._inbox_title, self._inbox_pill = self._card_header("!", MissionTone.WARNING)
        layout.addLayout(header)
        self._inbox_empty = QLabel()
        self._inbox_empty.setObjectName("MissionEmptyText")
        self._inbox_empty.setWordWrap(True)
        layout.addWidget(self._inbox_empty)
        self._decisions_layout = QVBoxLayout()
        self._decisions_layout.setSpacing(10)
        layout.addLayout(self._decisions_layout)
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
        self._activity_scroll = QScrollArea()
        self._activity_scroll.setObjectName("MissionActivityScroll")
        self._activity_scroll.setFrameShape(QFrame.Shape.NoFrame)
        self._activity_scroll.setWidgetResizable(True)
        self._activity_scroll.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        feed = QWidget()
        feed.setObjectName("MissionActivityFeed")
        self._activity_layout = QVBoxLayout(feed)
        self._activity_layout.setContentsMargins(0, 0, 8, 0)
        self._activity_layout.setSpacing(4)
        self._activity_layout.setAlignment(Qt.AlignmentFlag.AlignTop)
        self._activity_scroll.setWidget(feed)
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
        self._attachments_bar = QWidget()
        self._attachments_bar.setObjectName("MissionAttachmentsBar")
        self._attachments_layout = QHBoxLayout(self._attachments_bar)
        self._attachments_layout.setContentsMargins(14, 8, 14, 0)
        self._attachments_layout.setSpacing(4)
        self._attachments_layout.addStretch()
        self._attachments_bar.hide()
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
        examples = (strings.mission_example_1, strings.mission_example_2, strings.mission_example_3)
        for button, text in zip(self._example_buttons, examples):
            button.setText(text)

        self._caption.setText(strings.mission_caption)
        self._attach_button.setToolTip(strings.tooltip_attach_file)
        self._settings_button.setToolTip(strings.tooltip_settings_message)
        self._journey.set_labels([
            strings.mission_stage_asked, strings.mission_stage_planning, strings.mission_stage_working,
            strings.mission_stage_done
        ])
        self._report_caption.setText(strings.mission_report_caption.upper())
        self._report_title.setText("🎉  " + strings.mission_report_title)
        self._follow_up_button.setText("✎  " + strings.mission_follow_up)
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
        self._glance_title.setText(strings.mission_glance_title)
        for key, caption_text in (
            ("now", strings.mission_glance_now), ("blocking", strings.mission_glance_blocking),
            ("approvals", strings.mission_glance_approvals), ("done", strings.mission_glance_last_done),
        ):
            self._glance_cells[key][1].setText(caption_text.upper())

        self._working_on_pill.set_pill(strings.mission_status_running, MissionTone.ACCENT, pulsing=True)
        self._update_details_button()
        self._agent_network.setAccessibleName(strings.mission_agents_title)
        self._agents_empty.setText(strings.mission_agents_empty)
        self._inbox_title.setText(strings.mission_inbox_title)
        self._inbox_empty.setText(strings.mission_inbox_empty)
        self._activity_title.setText(strings.mission_activity_title)
        self._activity_empty.setText(strings.mission_activity_empty)
        self._send_button.setToolTip(f"{strings.mission_send} ({submit_key_text()})")
        self._send_button.setAccessibleName(strings.mission_send)
        self._resume_button.setText("↻  " + strings.mission_resume)
        self._retry_button.setText("↻  " + strings.mission_retry)
        self._pause_button.setText("‖  " + strings.mission_pause)
        self._pause_button.setToolTip(strings.mission_pause_tip)
        self._unpause_button.setText("▶  " + strings.mission_unpause)
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

    def set_missions(self, chips: list[MissionChipView], new_mission_selected: bool) -> None:
        """
        Show the mission switcher.

        Args:
            chips: Missions to offer, newest first
            new_mission_selected: True when the user is starting a new mission
        """
        self._new_mission_selected = new_mission_selected
        if chips == self._chips and self._chip_layout.count():
            self._update_new_chip()
            return

        self._chips = chips
        _clear_layout(self._chip_layout)
        _clear_layout(self._new_chip_layout)
        self._new_chip = QPushButton("+  " + self._language_manager.strings().mission_new)
        self._new_chip.setObjectName("MissionNewChip")
        self._new_chip.setCursor(Qt.CursorShape.PointingHandCursor)
        self._new_chip.clicked.connect(self.new_mission_requested)
        self._new_chip_layout.addWidget(self._new_chip)
        selected_group: QWidget | None = None

        strings = self._language_manager.strings()
        for chip in chips:
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

        self._chip_layout.addStretch(1)
        self._chip_row.setVisible(bool(chips))
        self._chip_strip.keep_in_view(selected_group)
        self._update_chip_arrows()
        self._update_new_chip()

    def _show_chip_menu(self, mission_id: str, position: QPoint) -> None:
        """Offer to rename or delete a mission."""
        strings = self._language_manager.strings()
        menu = self._style_manager.create_menu(self)
        rename = menu.addAction(strings.mission_rename)
        menu.addSeparator()
        delete = menu.addAction(strings.mission_delete)
        chosen = menu.exec(position)
        if chosen == rename:
            self.mission_rename_requested.emit(mission_id)

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
        self._view = view
        self._hero.setVisible(view is None)
        self._dashboard.setVisible(view is not None)
        self._activity_card.setVisible(view is not None)
        if view is None:
            self._glance_shown = False
            self._working_on.hide()
            self._everything_banner.hide()
            self._chat_card.hide()
            self._set_spinners([])
            self._set_lead_message("", "", False)
            self._update_composer_target(None)
            return

        header = view.header
        classic = header.is_classic
        self._glance_shown = view.glance is not None
        self._set_glance(view.glance)
        self._working_on.setVisible(bool(view.working_on_text))
        self._working_on_text.setText(view.working_on_text)
        self._classic_card.setVisible(classic)
        # The figures describe the plan, so they only appear once there is one.
        self._stats_container.setVisible(self._details(not classic and bool(view.steps)))
        self._lower_container.setVisible(self._details(not classic))
        self._agents_card.setVisible(not classic)
        self._activity_card.setVisible(self._details(not classic))
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
        self._journey.setVisible(not classic)
        self._journey.set_stage(
            header.stage, header.stage_tone, header.live, header.progress if header.stage == 2 else 0.0
        )
        self._update_attention()

        self._title.setText(header.title)
        self._summary.setText(header.summary)
        self._summary.setVisible(bool(header.summary) and header.summary.strip() != header.title.strip())
        self._status_pill.set_pill(header.status_text, header.tone, pulsing=header.live)
        self._progress_label.setText(header.progress_text)
        self._now_row.setVisible(not classic and bool(header.now_text))
        self._milestone_row.setVisible(bool(header.milestone_text))
        self._milestone_text.setText(header.milestone_text)
        self._now_badge.set_badge("★", header.now_tone)
        self._now_text.setText(header.now_text)
        self._stop_button.setVisible(header.can_stop)
        self._resume_button.setVisible(header.can_resume)
        self._interrupted_notice.setVisible(header.can_resume)
        # A failed mission is not resumed: its last request is retried, or its lead's conversation is
        # opened to see what went wrong.
        self._retry_button.setVisible(header.can_retry)
        self._pause_button.setVisible(header.can_pause)
        self._unpause_button.setVisible(header.paused)
        self._paused_notice.setVisible(header.paused)
        self._failed_notice.setVisible(header.can_retry)
        self._lead_context_id = header.lead_context_id
        self._lead_conversation_button.setVisible(header.can_retry and bool(header.lead_context_id))
        self._update_composer_target(header.title)

        self._set_stats(view.stats)
        self._set_history(view.history)
        self._set_steps(view.steps)
        self._set_agents(view.agents, view.agents_badge)
        self._set_activity(view.activity)

    def _set_stats(self, stats: list[MissionStatView]) -> None:
        """Show the statistics row."""
        if stats == self._stats:
            return

        self._stats = stats
        for card in self._stat_cards:
            card.hide()
            card.deleteLater()

        self._stat_cards = []
        for stat in stats:
            card = _ClickableFrame()
            card.setObjectName("MissionStatCardActive" if stat.target == self._step_filter else "MissionStatCard")
            card.setCursor(Qt.CursorShape.PointingHandCursor)
            card.clicked.connect(lambda t=stat.target: self._on_stat_clicked(t))
            layout = QHBoxLayout(card)
            layout.setContentsMargins(14, 12, 14, 12)
            layout.setSpacing(12)
            layout.addWidget(GlyphBadge(stat.glyph, stat.tone, 38))
            text = QVBoxLayout()
            text.setSpacing(0)
            value = QLabel(stat.value)
            value.setObjectName("MissionStatValue")
            self._animate_count(value, self._stat_values.get(stat.target, ""), stat.value)
            self._stat_values[stat.target] = stat.value
            text.addWidget(value)
            label = QLabel(stat.label)
            label.setObjectName("MissionStatLabel")
            label.setWordWrap(True)
            text.addWidget(label)
            layout.addLayout(text, 1)
            self._stat_cards.append(card)

        self._stat_columns = 0
        self._layout_stats()

    def _animate_count(self, label: QLabel, old: str, new: str) -> None:
        """Count a statistic up or down to its new value."""
        if not old.isdigit() or not new.isdigit() or old == new:
            return

        animation = QVariantAnimation(label)
        animation.setDuration(450)
        animation.setEasingCurve(QEasingCurve.Type.OutCubic)
        animation.setStartValue(int(old))
        animation.setEndValue(int(new))
        animation.valueChanged.connect(lambda v: label.setText(str(v)))
        label.setText(old)
        animation.start(QVariantAnimation.DeletionPolicy.DeleteWhenStopped)

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

    def _on_stat_clicked(self, target: MissionStatTarget) -> None:
        """Filter the plan, or bring the related card into view."""
        if target == MissionStatTarget.INBOX:
            self.glide_to(self._inbox_card)
            return

        if target == MissionStatTarget.AGENTS:
            self.glide_to(self._agents_card)
            return

        self._set_step_filter(MissionStatTarget.ALL_STEPS if target == self._step_filter else target)
        self.glide_to(self._plan_card)

    def _set_step_filter(self, target: MissionStatTarget) -> None:
        """Show only the steps a statistic stands for."""
        self._step_filter = target
        stats = self._stats
        self._stats = None
        if stats is not None:
            self._set_stats(stats)

        self._render_steps()

    def _layout_stats(self) -> None:
        """Arrange statistics cards in as many columns as fit."""
        zoom = self._style_manager.zoom_factor()
        width = self._main_column.width()
        columns = max(1, len(self._stat_cards)) if width >= 640 * zoom else 2

        if columns == self._stat_columns:
            return

        self._stat_columns = columns
        while self._stats_grid.count():
            self._stats_grid.takeAt(0)

        for index, card in enumerate(self._stat_cards):
            self._stats_grid.addWidget(card, index // columns, index % columns)

        for column in range(max(columns, self._stats_grid.columnCount())):
            self._stats_grid.setColumnStretch(column, 1 if column < columns else 0)

    def _set_steps(self, steps: list[MissionStepView]) -> None:
        """Show the plan timeline."""
        self._plan_pill.setVisible(bool(steps))
        if steps:
            done = sum(1 for step in steps if step.state == MissionStepState.DONE)
            self._plan_pill.set_pill(f"{done}/{len(steps)}", MissionTone.ACCENT)

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

        filter_label = next(
            (stat.label for stat in self._stats or [] if stat.target == self._step_filter), ""
        )
        self._filter_chip.setVisible(wanted is not None)
        self._filter_chip.setText(f"{filter_label}  ✕   {strings.mission_plan_show_all}")
        self._plan_hint.setVisible(bool(shown))
        self._plan_empty.setVisible(not shown)
        self._plan_empty.setText(
            strings.mission_plan_filter_empty if steps
            else strings.mission_plan_empty_next if self._history
            else strings.mission_plan_empty
        )

        _clear_layout(self._steps_layout)
        for index, step in enumerate(shown):
            expanded = step.task_id in self._expanded_steps
            row = _ClickableFrame()
            row.setObjectName("MissionStepRowOpen" if expanded else "MissionStepRow")
            row.setCursor(Qt.CursorShape.PointingHandCursor)
            row.setAccessibleName(f"{step.number}. {step.title} — {step.status_text}")
            row.clicked.connect(lambda t=step.task_id: self._toggle_step(t))
            row_layout = QHBoxLayout(row)
            row_layout.setContentsMargins(4, 0, 8, 0)
            row_layout.setSpacing(10)
            marker = StepMarker()
            marker.set_step(step.number, step.state, index == 0, index == len(shown) - 1)
            row_layout.addWidget(marker)

            text = QVBoxLayout()
            text.setContentsMargins(0, 10, 0, 12)
            text.setSpacing(2)
            title = QLabel(step.title)
            title.setObjectName("MissionStepTitle")
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
            chevron = QLabel("▼" if expanded else "▶")
            chevron.setObjectName("MissionStepChevron")
            side.addWidget(chevron, 0, Qt.AlignmentFlag.AlignTop)
            row_layout.addLayout(side)
            self._steps_layout.addWidget(row)

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
            agents, strings.mission_you, strings.mission_approve, strings.mission_reject,
            strings.mission_finish_waiting, strings.mission_wait_left, strings.mission_read_more,
            strings.mission_unsure, strings.mission_review_request
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
        self._inbox_pill.setVisible(bool(decisions))
        self._inbox_pill.set_pill(str(len(decisions)), MissionTone.WARNING)
        if decisions == self._decisions:
            return

        self._decisions = decisions
        self._update_attention()
        _clear_layout(self._decisions_layout)
        self._decision_cards = {}
        self._highlighted_card = None
        self._inbox_empty.setVisible(not decisions)
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
            self._decisions_layout.addWidget(card)
            self._decision_cards[decision.decision_id] = card
            if decision.decision_id not in self._shown_decisions:
                fade_in(card, 320)

        self._shown_decisions = {decision.decision_id for decision in decisions}

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
        prompt = strings.mission_composer_reply if self._awaiting_reply else strings.mission_composer_guidance
        self._input.setPlaceholderText(prompt.format(key=submit_key_text()))

    def _use_example(self, text: str) -> None:
        """Put an example objective in the composer."""
        self._input.setPlainText(text)
        self._input.moveCursor(QTextCursor.MoveOperation.End)
        self._input.setFocus()

    def _update_send_enabled(self) -> None:
        """Enable sending only when there is something to send."""
        self._send_button.setEnabled(bool(self._input.toPlainText().strip()))

    def _submit(self) -> None:
        """Send the composer text."""
        text = self._input.toPlainText().strip()
        if not text:
            return

        self._input.clear()
        self.submit_requested.emit(text)

    def add_attachment(self, filename: str, content: str) -> None:
        """Attach a file to the next message, and show it above the composer."""
        self._attachments.append((filename, content))
        self._show_attachments()

    def take_attachments(self) -> list[tuple[str, str]]:
        """Return the files attached to the message being sent, as (filename, content) pairs, and clear them."""
        attachments = self._attachments
        self._attachments = []
        self._show_attachments()
        return attachments

    def _remove_attachment(self, index: int) -> None:
        """Take a file off the next message."""
        if 0 <= index < len(self._attachments):
            self._attachments.pop(index)
            self._show_attachments()

    def _show_attachments(self) -> None:
        """Show a chip for each attached file, as a conversation's input does."""
        while self._attachments_layout.count() > 1:
            item = self._attachments_layout.takeAt(0)
            widget = item.widget() if item else None
            if widget:
                widget.deleteLater()

        icon_size = round(14 * self._style_manager.zoom_factor())
        for index, (filename, _) in enumerate(self._attachments):
            chip = QFrame()
            chip.setObjectName("MissionAttachment")
            chip_layout = QHBoxLayout(chip)
            chip_layout.setContentsMargins(8, 4, 4, 4)
            chip_layout.setSpacing(4)
            name = QLabel(filename)
            name.setObjectName("MissionAttachmentName")
            chip_layout.addWidget(name)
            remove = QToolButton()
            remove.setObjectName("MissionComposerTool")
            remove.setCursor(Qt.CursorShape.PointingHandCursor)
            remove.setIcon(QIcon(self._style_manager.scale_icon("close", 14)))
            remove.setIconSize(QSize(icon_size, icon_size))
            remove.clicked.connect(lambda _checked=False, i=index: self._remove_attachment(i))
            chip_layout.addWidget(remove)
            self._attachments_layout.insertWidget(index, chip)

        self._attachments_bar.setVisible(bool(self._attachments))

    def _show_auto_approve(self, level: str) -> None:
        """Mark the current auto-approve level, and say what it means."""
        self._mark_auto_approve(self._auto_approve_buttons, self._auto_approve_note, level)

    def set_new_mission_auto_approve(self, level: str) -> None:
        """Show the auto-approve level the next mission will start with, without reporting a change."""
        self._new_mission_auto_approve = level
        self._mark_auto_approve(self._hero_auto_approve_buttons, self._hero_auto_approve_note, level)

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

    def _show_more_actions(self) -> None:
        """Offer the mission's less common actions."""
        strings = self._language_manager.strings()
        menu = self._style_manager.create_menu(self)
        switch = menu.addAction(strings.mission_switch_to_classic) if self._can_switch_to_classic else None
        menu.addSeparator()
        delete = menu.addAction(strings.mission_delete)
        chosen = menu.exec(self._more_button.mapToGlobal(self._more_button.rect().bottomLeft()))
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

    def _update_attention(self) -> None:
        """Show the call to action while requests wait for the user."""
        count = len(self._decisions or [])
        self._attention.setVisible(self._has_mission and count > 0)
        strings = self._language_manager.strings()
        self._attention_text.setText(strings.mission_attention.format(count))
        self._attention_button.setText(strings.mission_review_now)

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
        self._hero_orb.apply_motion()
        self._report_orb.apply_motion()
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
        self._layout_stats()

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
        self._lower.setDirection(
            QBoxLayout.Direction.LeftToRight if main_width >= _SPLIT_LOWER_WIDTH * zoom
            else QBoxLayout.Direction.TopToBottom
        )

        self._layout_stats()

    def apply_style(self) -> None:
        """Apply the current theme and zoom."""
        for pill in self._pills:
            pill.restyle()

        self._content_layout.setContentsMargins(24, 20, 24, 24)
        self._activity_scroll.setFixedHeight(round(_ACTIVITY_HEIGHT * self._style_manager.zoom_factor()))
        self._chat_scroll.setMaximumHeight(round(_CHAT_MAX_HEIGHT * self._style_manager.zoom_factor()))
        self._input.set_margin(round(6 * self._style_manager.zoom_factor()))
        icon_size = round(18 * self._style_manager.zoom_factor())
        self._send_button.setIcon(_send_icon(self._style_manager.get_color(ColorRole.TEXT_RECOMMENDED), icon_size))
        self._send_button.setIconSize(QSize(icon_size, icon_size))
        tool_size = round(16 * self._style_manager.zoom_factor())
        for button, icon in ((self._attach_button, "paperclip"), (self._settings_button, "cog")):
            button.setIcon(QIcon(self._style_manager.scale_icon(icon, 16)))
            button.setIconSize(QSize(tool_size, tool_size))

        self._show_attachments()
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
        radius = round(14 * zoom)
        small_radius = round(9 * zoom)
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
            #MissionWidget, #MissionContent {{
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
            QCheckBox#MissionAutoResume, QCheckBox#MissionReduceMotion {{
                color: {muted};
                font-size: {fs * 0.88:.1f}pt;
                spacing: {round(8 * zoom)}px;
                background: transparent;
            }}
            QCheckBox#MissionAutoResume:hover, QCheckBox#MissionReduceMotion:hover {{
                color: {text};
            }}
            QCheckBox#MissionAutoResume::indicator, QCheckBox#MissionReduceMotion::indicator {{
                width: {round(16 * zoom)}px;
                height: {round(16 * zoom)}px;
                border: 1px solid {input_border};
                border-radius: {round(4 * zoom)}px;
                background-color: {input_bg};
            }}
            QCheckBox#MissionAutoResume::indicator:hover, QCheckBox#MissionReduceMotion::indicator:hover,
            QCheckBox#MissionAutoResume::indicator:focus, QCheckBox#MissionReduceMotion::indicator:focus {{
                border-color: {focus};
            }}
            QCheckBox#MissionAutoResume::indicator:checked, QCheckBox#MissionReduceMotion::indicator:checked {{
                background-color: {primary};
                border-color: {primary};
                image: url({sm.get_icon_path("check")});
            }}

            #MissionHero, #MissionHeaderCard, #MissionCard, #MissionInboxCard, #MissionStatCard,
            #MissionStatCardActive, #MissionLeadCard, #MissionLeadCardWaiting, #MissionClassicCard {{
                background-color: {card};
                border: 1px solid {line};
                border-radius: {radius}px;
            }}
            #MissionStatCard:hover {{
                border-color: {accent_line};
            }}
            #MissionStatCardActive {{
                border: 1px solid {accent_line};
                background-color: {blend(tone_color(MissionTone.ACCENT), 0.06, card_color).name()};
            }}
            #MissionLeadCard, #MissionClassicCard {{
                background-color: {blend(tone_color(MissionTone.ACCENT), 0.04, card_color).name()};
                border: 1px solid {tone_rgba(MissionTone.ACCENT, 0.30)};
            }}
            #MissionNowRow {{
                background-color: {accent_soft};
                border: 1px solid {tone_rgba(MissionTone.ACCENT, 0.25)};
                border-radius: {round(10 * zoom)}px;
            }}
            #MissionWidget QLabel#MissionNowText {{
                color: {heading};
                font-size: {fs * 1.02:.1f}pt;
            }}
            #MissionAttention {{
                background-color: {warning_soft};
                border: 1px solid {tone_rgba(MissionTone.WARNING, 0.6)};
                border-radius: {radius}px;
            }}
            #MissionMilestoneRow {{
                background-color: {tone_rgba(MissionTone.SUCCESS, 0.08)};
                border: 1px solid {tone_rgba(MissionTone.SUCCESS, 0.30)};
                border-radius: {round(10 * zoom)}px;
            }}
            #MissionWidget QLabel#MissionMilestoneText {{
                color: {heading};
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
                background-color: {danger_fill};
                color: {on_primary};
                border: 1px solid {danger_fill};
            }}
            QPushButton#MissionStopButton:hover {{
                background-color: {danger_hover};
            }}
            QPushButton#MissionStopButton:pressed {{
                background-color: {danger_pressed};
            }}
            QPushButton#MissionStopButton:focus {{
                border: 2px solid {focus};
            }}
            #MissionReportCard {{
                background-color: qlineargradient(x1:0, y1:0, x2:1, y2:1,
                    stop:0 {blend(tone_color(MissionTone.SUCCESS), 0.14, card_color).name()},
                    stop:1 {blend(tone_color(MissionTone.ACCENT), 0.06, card_color).name()});
                border: 1px solid {tone_rgba(MissionTone.SUCCESS, 0.55)};
                border-radius: {radius}px;
            }}
            #MissionWidget QLabel#MissionReportTitle {{
                color: {heading};
                font-size: {fs * 1.6:.1f}pt;
                font-weight: bold;
            }}
            #MissionWidget QLabel#MissionReportStats {{
                color: {readable(tone_color(MissionTone.SUCCESS), card_color).name()};
                font-weight: 600;
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

            #MissionWidget QLabel#MissionCaption {{
                color: {accent_text.name()};
                font-size: {fs * 0.76:.1f}pt;
                font-weight: bold;
                letter-spacing: 2px;
            }}
            #MissionWidget QLabel#MissionTitle {{
                color: {heading};
                font-size: {fs * 1.6:.1f}pt;
                font-weight: bold;
            }}
            #MissionWidget QLabel#MissionSummary {{
                color: {muted};
            }}
            #MissionWidget QLabel#MissionProgressLabel {{
                color: {heading};
                font-size: {fs * 1.05:.1f}pt;
                font-weight: bold;
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
            QPushButton#MissionGhostButton, QPushButton#MissionSecondaryButton,
            QPushButton#MissionPauseButton {{
                background-color: {secondary};
                color: {text};
                border: 1px solid transparent;
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

            #MissionWidget QLabel#MissionStatValue {{
                color: {heading};
                font-size: {fs * 1.75:.1f}pt;
                font-weight: bold;
            }}
            #MissionWidget QLabel#MissionStatLabel {{
                color: {muted};
                font-size: {fs * 0.84:.1f}pt;
            }}

            #MissionWidget QLabel#MissionCardTitle {{
                color: {heading};
                font-size: {fs * 1.1:.1f}pt;
                font-weight: bold;
            }}
            #MissionWidget QLabel#MissionHintText, #MissionWidget QLabel#MissionEmptyText {{
                color: {muted};
                font-size: {fs * 0.86:.1f}pt;
            }}
            QPushButton#MissionFilterChip {{
                background-color: {accent_soft};
                color: {heading};
                border: 1px solid {accent_line};
                border-radius: {round(11 * zoom)}px;
                padding: {round(3 * zoom)}px {round(10 * zoom)}px;
                font-size: {fs * 0.82:.1f}pt;
                font-weight: 600;
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
            QToolButton#MissionComposerTool {{
                background-color: transparent;
                border: none;
                border-radius: {round(6 * zoom)}px;
                padding: {round(4 * zoom)}px;
            }}
            QToolButton#MissionComposerTool:hover {{
                background-color: {sm.get_color_str(ColorRole.MESSAGE_BACKGROUND_HOVER)};
            }}
            QToolButton#MissionComposerTool:pressed {{
                background-color: {sm.get_color_str(ColorRole.MESSAGE_BACKGROUND_PRESSED)};
            }}
            QToolButton#MissionComposerTool:focus {{
                border: 1px solid {focus};
            }}
            #MissionAttachment {{
                background-color: {sm.get_color_str(ColorRole.MESSAGE_ATTACHMENT_BACKGROUND)};
                border: 1px solid {input_border};
                border-radius: {round(4 * zoom)}px;
            }}
            QLabel#MissionAttachmentName {{
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
            #MissionPhaseRowOpen:focus, #MissionAgentRow:focus, #MissionStatCard:focus,
            #MissionStatCardActive:focus, #MissionActivityRow:focus, #MissionActivityRowOpen:focus {{
                border: 2px solid {focus};
            }}

            {sm.get_scrollbar_stylesheet("#MissionScroll QScrollBar")}
            {sm.get_scrollbar_stylesheet("#MissionActivityScroll QScrollBar")}
            {sm.get_scrollbar_stylesheet("#MissionChatScroll QScrollBar")}
            {sm.get_scrollbar_stylesheet("#MissionComposerInput QScrollBar")}
        """
        return _scope_to_widget(stylesheet)
