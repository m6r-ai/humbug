"""Animated network of a mission's agents, drawn as working robots joined by flowing arrows."""

from collections.abc import Callable
from dataclasses import dataclass, field
from functools import partial
import math
import time

from PySide6.QtCore import QEvent, QPointF, QRectF, QSize, Qt, QTimer, Signal, SignalInstance
from PySide6.QtGui import (
    QBrush, QColor, QFont, QFontMetrics, QHideEvent, QLinearGradient, QMouseEvent, QPainter, QPainterPath,
    QPaintEvent, QPen, QRadialGradient, QResizeEvent, QShowEvent
)
from PySide6.QtWidgets import QPushButton, QSizePolicy, QToolTip, QWidget

from desktop.color_role import ColorRole
from desktop.mission_tab.mission_display import MissionAgentView, MissionTone
from desktop.mission_tab.mission_painted import readable, surface_colors, tone_color
from desktop.style_manager import StyleManager


# Frame interval while agents are working, and while everything is at rest.
_BUSY_FRAME_MS = 33
_IDLE_FRAME_MS = 90

_USER_ID = "__user__"

# How fast an agent's words appear, in characters per second, and how long a new remark makes it wave.
_TYPING_RATE = 45.0
_WAVE_SECONDS = 1.4
_POP_SECONDS = 0.25
_CARD_LINES = 3

# How far below a robot's centre its name sits, in robot sizes, clearing the pedestal it stands on.
_LABEL_DROP = 0.94


@dataclass
class _Node:
    """A node of the network: the user, or an agent."""
    node_id: str
    label: str
    caption: str
    agent: MissionAgentView | None
    level: int = 0
    center: QPointF = field(default_factory=QPointF)


def _shade(color: QColor, factor: int) -> QColor:
    """Return a lighter (factor > 100) or darker (factor < 100) version of a colour."""
    return color.lighter(factor) if factor >= 100 else color.darker(round(10000 / factor))


class AgentNetworkView(QWidget):
    """
    A live picture of who is working on a mission.

    The user sits at the top, the mission lead below, and each delegated agent
    below the agent that delegated to it.  Every agent is drawn as a small
    robot whose animation shows what it is doing; arrows carry moving particles
    while work flows along them.  Presentational only: it is given display
    records and reports clicks.

    Each agent says what it is doing in a speech bubble, typed out as it
    speaks, and an agent waiting for approval offers Approve, Unsure, Reject
    and Read more buttons in its bubble.
    """

    agent_clicked = Signal(str)
    decision_approved = Signal(str)
    decision_unsure = Signal(str)
    decision_rejected = Signal(str)
    decision_details_requested = Signal(str)
    wait_finish_requested = Signal(str)

    def __init__(self, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self.setObjectName("MissionAgentNetwork")
        self.setMouseTracking(True)
        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
        self.setAttribute(Qt.WidgetAttribute.WA_TranslucentBackground)
        self._style_manager = StyleManager()
        self._nodes: list[_Node] = []
        self._edges: list[tuple[_Node, _Node]] = []
        self._user_label = ""
        self._phase = 0.0
        self._hover_id = ""
        self._speech: dict[str, tuple[str, float]] = {}
        self._fitted: dict[tuple[str, int, int], str] = {}
        self._buttons: dict[str, tuple[str, list[QPushButton]]] = {}
        self._time_shown: dict[str, bool] = {}
        self._labels = {"approve": "", "reject": "", "finish": "", "left": "{0}"}
        # Sizes a button the way the stylesheet will, so space for buttons is reserved before any exist.
        self._probe = QPushButton("Approve", self)
        self._probe.setObjectName("MissionButtonProbe")
        self._probe.hide()
        self._timer = QTimer(self)
        self._timer.timeout.connect(self._tick)

    def set_agents(
        self,
        agents: list[MissionAgentView],
        user_label: str,
        approve_label: str = "",
        reject_label: str = "",
        finish_label: str = "",
        left_label: str = "{0}",
        more_label: str = "",
        unsure_label: str = ""
    ) -> None:
        """
        Show a mission's agents.

        Args:
            agents: The agents, lead first
            user_label: Label for the user's own node
            approve_label: Label of the Approve button shown in a waiting agent's card
            reject_label: Label of the Reject button shown in a waiting agent's card
            finish_label: Label of the button that cuts a wait short
            left_label: Format for the time a wait has left, such as "{0} left"
            more_label: Label of the button that shows a waiting agent's whole request
            unsure_label: Label of the Unsure button shown in a waiting agent's card
        """
        self._user_label = user_label
        self._labels = {
            "approve": approve_label, "reject": reject_label, "finish": finish_label, "left": left_label,
            "more": more_label, "unsure": unsure_label
        }
        speech: dict[str, tuple[str, float]] = {}
        for agent in agents:
            if not agent.speech:
                continue

            previous = self._speech.get(agent.context_id)
            # A new remark is typed out afresh; the same remark carries on where it was.
            speech[agent.context_id] = previous if previous and previous[0] == agent.speech else (
                agent.speech, self._phase
            )

        self._speech = speech
        self._sync_buttons(agents)
        user = _Node(_USER_ID, user_label, "", None)
        nodes = {_USER_ID: user}
        lead_id = next((agent.context_id for agent in agents if agent.is_lead), "")
        for agent in agents:
            nodes[agent.context_id] = _Node(agent.context_id, agent.name, agent.role_text, agent)

        edges: list[tuple[_Node, _Node]] = []
        for agent in agents:
            if agent.is_lead:
                parent_id = _USER_ID

            elif agent.parent_id in nodes:
                parent_id = agent.parent_id

            else:
                parent_id = lead_id or _USER_ID

            edges.append((nodes[parent_id], nodes[agent.context_id]))

        children: dict[str, list[_Node]] = {}
        for parent, child in edges:
            children.setdefault(parent.node_id, []).append(child)

        pending = [user]
        while pending:
            node = pending.pop(0)
            for child in children.get(node.node_id, []):
                child.level = node.level + 1
                pending.append(child)

        self._nodes = list(nodes.values())
        self._edges = edges
        self._layout_nodes()
        self.updateGeometry()
        self._update_timer()
        self._place_buttons()
        self.update()

    def _sync_buttons(self, agents: list[MissionAgentView]) -> None:
        """Create the buttons each agent's card offers, and drop those no longer wanted."""
        wanted: dict[str, tuple[str, MissionAgentView]] = {}
        for agent in agents:
            if not agent.speech:
                continue

            if agent.decision_id:
                wanted[agent.context_id] = (agent.decision_id, agent)

            elif agent.wait_started:
                wanted[agent.context_id] = (f"wait:{agent.wait_started}", agent)

        for context_id in list(self._buttons):
            key, buttons = self._buttons[context_id]
            if wanted.get(context_id, ("", None))[0] != key:
                self._release_focus(buttons)
                for button in buttons:
                    button.deleteLater()

                del self._buttons[context_id]

        for context_id, (key, agent) in wanted.items():
            if context_id in self._buttons:
                buttons = self._buttons[context_id][1]
                texts = [
                    self._labels["approve"], self._labels["unsure"], self._labels["reject"], self._labels["more"]
                ] if agent.decision_id else [self._labels["finish"]]
                for button, text in zip(buttons, texts):
                    button.setText(text)

                continue

            if agent.decision_id:
                decision_id = agent.decision_id
                approve = self._make_button(
                    self._labels["approve"],
                    "MissionApproveDestructive" if agent.decision_destructive else "MissionApprove",
                    partial(self._answer, decision_id, self.decision_approved)
                )
                unsure = self._make_button(
                    self._labels["unsure"], "MissionSecondaryButton",
                    partial(self._answer, decision_id, self.decision_unsure)
                )
                reject = self._make_button(
                    self._labels["reject"], "MissionSecondaryButton",
                    partial(self._answer, decision_id, self.decision_rejected)
                )
                more = self._make_button(
                    self._labels["more"], "MissionLinkButton", partial(self.decision_details_requested.emit, decision_id)
                )
                self._buttons[context_id] = (key, [approve, unsure, reject, more])
                continue

            finish = self._make_button(
                self._labels["finish"], "MissionSecondaryButton", partial(self._finish_wait, context_id)
            )
            self._buttons[context_id] = (key, [finish])

    def _make_button(self, text: str, name: str, action: Callable[[], None]) -> QPushButton:
        """Create a hidden button for an agent's card."""
        button = QPushButton(text, self)
        button.setObjectName(name)
        button.setCursor(Qt.CursorShape.PointingHandCursor)
        button.hide()
        button.clicked.connect(lambda _checked=False: action())
        return button

    def _answer(self, decision_id: str, answered: SignalInstance) -> None:
        """Report an answer given from a card, and stop it being given twice."""
        for key, buttons in self._buttons.values():
            if key == decision_id:
                self._release_focus(buttons)
                for button in buttons:
                    button.setEnabled(False)

        answered.emit(decision_id)

    def _release_focus(self, buttons: list[QPushButton]) -> None:
        """
        Keep focus here when a card's buttons are about to go.

        A focused button that is disabled or hidden hands focus to the next widget, and the
        page scrolls to wherever that is, so the user would be carried away from the agents.
        """
        if any(button.hasFocus() for button in buttons):
            self.setFocus(Qt.FocusReason.OtherFocusReason)

    def _finish_wait(self, context_id: str) -> None:
        """Ask for an agent's wait to be cut short, once."""
        entry = self._buttons.get(context_id)
        if entry is not None:
            self._release_focus(entry[1])
            for button in entry[1]:
                button.setEnabled(False)

        self.wait_finish_requested.emit(context_id)

    def _typing(self, node_id: str) -> bool:
        """Return True while an agent's latest remark is still being typed out."""
        remark = self._speech.get(node_id)
        return remark is not None and (self._phase - remark[1]) * _TYPING_RATE < len(remark[0])

    def _speech_font(self) -> QFont:
        """Return the font agents speak in."""
        font = QFont(self.font())
        font.setPointSizeF(self._style_manager.base_font_size() * self._style_manager.zoom_factor() * 0.86)
        return font

    def _button_height(self) -> int:
        """Return the height of a button in a card."""
        self._probe.ensurePolished()
        return self._probe.sizeHint().height()

    def _card_reserve(self) -> float:
        """
        Return the height kept under every agent for its card.

        Every agent gets the same space whatever it says, so the network never
        changes height, and nothing below it jumps, as agents start and stop talking.
        """
        pad = 10 * self._style_manager.zoom_factor()
        line = QFontMetrics(self._speech_font()).lineSpacing()
        return pad * 2 + line * _CARD_LINES + pad * 0.8 + self._button_height()

    def _name_height(self) -> float:
        """Return the height of a node's name and caption."""
        base = self._style_manager.base_font_size() * self._style_manager.zoom_factor()
        name_font = QFont(self.font())
        name_font.setPointSizeF(base * 0.95)
        caption_font = QFont(self.font())
        caption_font.setPointSizeF(base * 0.8)
        return QFontMetrics(name_font).height() + QFontMetrics(caption_font).height() + 4

    def _fit_text(self, text: str, width: int, lines: int = _CARD_LINES) -> str:
        """Return text shortened, if need be, to fit a number of the card's lines at a width."""
        key = (text, width, lines)
        fitted = self._fitted.get(key)
        if fitted is not None:
            return fitted

        metrics = QFontMetrics(self._speech_font())
        limit = metrics.lineSpacing() * lines + 1
        flags = int(Qt.TextFlag.TextWordWrap)
        fitted = text
        if metrics.boundingRect(0, 0, width, 10000, flags, text).height() > limit:
            low, high = 0, len(text)
            while low < high:
                middle = (low + high + 1) // 2
                candidate = text[:middle].rstrip() + "…"
                if metrics.boundingRect(0, 0, width, 10000, flags, candidate).height() <= limit:
                    low = middle

                else:
                    high = middle - 1

            fitted = text[:low].rstrip() + "…"

        if len(self._fitted) > 200:
            self._fitted.clear()

        self._fitted[key] = fitted
        return fitted

    def _bubble_rect(self, node: _Node) -> QRectF:
        """
        Return where an agent's card goes: under its name, centred, within its share of the width.

        Agents on one level each have their own share of the width, so cards
        never overlap; the card's size fits the whole remark, so it does not
        grow while the words are typed out.
        """
        size, _gap, _top = self._metrics()
        zoom = self._style_manager.zoom_factor()
        pad = 10 * zoom
        share = self.width() / max(1, sum(1 for n in self._nodes if n.level == node.level))
        width = max(110 * zoom, min(320 * zoom, share - 14 * zoom))
        wrapped = self._link_on_own_row(node.node_id, width)
        text = self._card_text(node.node_id, width)
        metrics = QFontMetrics(self._speech_font())
        text_rect = metrics.boundingRect(0, 0, round(width - pad * 2), 10000, int(Qt.TextFlag.TextWordWrap), text)
        height = text_rect.height() + pad * 2
        if node.node_id in self._buttons:
            height += self._button_height() + pad * 0.8
            if wrapped:
                height += self._buttons[node.node_id][1][-1].sizeHint().height() + pad * 0.4

        else:
            width = min(width, text_rect.width() + pad * 2 + 2)

        top = node.center.y() + size * _LABEL_DROP + self._name_height() + 6 * zoom
        left = max(6 * zoom, min(node.center.x() - width / 2, self.width() - 6 * zoom - width))
        return QRectF(left, top, width, height)

    def _link_on_own_row(self, context_id: str, width: float) -> bool:
        """
        Return True when a card's Read more link moves to a row of its own.

        That happens when the answer buttons and the link do not all fit on one row at
        their natural widths, so no button is squeezed until its label is cut off.
        """
        entry = self._buttons.get(context_id)
        if entry is None or len(entry[1]) < 2 or entry[1][-1].objectName() != "MissionLinkButton":
            return False

        pad = 10 * self._style_manager.zoom_factor()
        buttons = entry[1]
        needed = sum(button.sizeHint().width() for button in buttons) + pad * 0.8 * (len(buttons) - 1)
        return needed > width - pad * 2

    def _card_text(self, context_id: str, width: float) -> str:
        """Return what a card shows, giving up a line when its link needs a row of its own."""
        pad = 10 * self._style_manager.zoom_factor()
        lines = _CARD_LINES - 1 if self._link_on_own_row(context_id, width) else _CARD_LINES
        return self._fit_text(self._speech[context_id][0], round(width - pad * 2), lines)

    def _place_buttons(self) -> None:
        """Put each agent's buttons along the bottom of its card once its words are typed out."""
        zoom = self._style_manager.zoom_factor()
        pad = 10 * zoom
        nodes = {node.node_id: node for node in self._nodes}
        for context_id, (_key, buttons) in self._buttons.items():
            node = nodes.get(context_id)
            if node is None or context_id not in self._speech or self._typing(context_id):
                for button in buttons:
                    button.hide()

                continue

            card = self._bubble_rect(node)
            row = self._button_height()
            y = round(card.bottom() - pad - row)
            if self._link_on_own_row(context_id, card.width()):
                # Read more sits on its own row above the answer buttons.
                link = buttons[-1]
                link_height = link.sizeHint().height()
                link.setGeometry(
                    round(card.left() + pad), round(y - pad * 0.4 - link_height),
                    min(link.sizeHint().width(), round(card.width() - pad * 2)), link_height
                )
                if link.isHidden():
                    link.show()

                buttons = buttons[:-1]

            available = card.width() - pad * 2 - pad * 0.8 * (len(buttons) - 1)
            natural = [button.sizeHint().width() for button in buttons]
            limit = available
            if len(buttons) == 1:
                # A wait's button shares its row with the time left, if there is room for both.
                font = self._speech_font()
                font.setBold(True)
                time_width = QFontMetrics(font).horizontalAdvance(self._labels["left"].format("0:00:00")) + pad
                fits = natural[0] <= available - time_width
                self._time_shown[context_id] = fits
                if fits:
                    limit -= time_width

            scale = min(1.0, limit / max(1, sum(natural)))
            x = card.left() + pad
            for button, width in zip(buttons, natural):
                button.setGeometry(round(x), y, round(width * scale), row)
                x += width * scale + pad * 0.8
                if button.isHidden():
                    button.show()

    def _metrics(self) -> tuple[float, float, float]:
        """Return the robot size, the gap between agent levels and the top margin, scaled by zoom."""
        zoom = self._style_manager.zoom_factor()
        size = 56 * zoom
        gap = size * _LABEL_DROP + self._name_height() + 6 * zoom + self._card_reserve() + 40 * zoom + size * 0.8
        return size, gap, 40 * zoom

    def _level_y(self, level: int) -> float:
        """Return the vertical centre of a level; the user's level is shorter, having no card."""
        size, gap, top = self._metrics()
        first = top + size / 2
        if level == 0:
            return first

        user_gap = size * 0.46 + self._name_height() + 40 * self._style_manager.zoom_factor() + size * 0.8
        return first + user_gap + (level - 1) * gap

    def _layout_nodes(self) -> None:
        """Spread each level's nodes evenly across the width."""
        levels: dict[int, list[_Node]] = {}
        for node in self._nodes:
            levels.setdefault(node.level, []).append(node)

        width = max(1, self.width())
        for level, members in levels.items():
            share = width / len(members)
            y = self._level_y(level)
            for index, node in enumerate(members):
                node.center = QPointF(share * (index + 0.5), y)

    def sizeHint(self) -> QSize:
        size, _gap, _top = self._metrics()
        deepest = max((node.level for node in self._nodes), default=0)
        bottom = self._level_y(deepest) + size * _LABEL_DROP + self._name_height()
        if deepest > 0:
            bottom += 6 * self._style_manager.zoom_factor() + self._card_reserve()

        return QSize(400, round(bottom + 12 * self._style_manager.zoom_factor()))

    def minimumSizeHint(self) -> QSize:
        return QSize(200, self.sizeHint().height())

    def resizeEvent(self, event: QResizeEvent) -> None:
        """Reposition nodes for the new width."""
        super().resizeEvent(event)
        self._layout_nodes()
        self._place_buttons()

    def showEvent(self, event: QShowEvent) -> None:
        """Resume the animation."""
        super().showEvent(event)
        self._update_timer()

    def hideEvent(self, event: QHideEvent) -> None:
        """Pause the animation while hidden."""
        super().hideEvent(event)
        self._timer.stop()

    def _any_active(self) -> bool:
        """Return True if any agent is working, thinking, waiting or still speaking."""
        return any(
            node.agent is not None and (node.agent.activity != "idle" or self._typing(node.node_id))
            for node in self._nodes
        )

    def _update_timer(self) -> None:
        """Animate quickly while agents work and slowly at rest; not at all when there is nothing to show."""
        if not self._nodes or len(self._nodes) == 1 or not self.isVisible():
            self._timer.stop()
            return

        self._timer.setInterval(_BUSY_FRAME_MS if self._any_active() else _IDLE_FRAME_MS)
        if not self._timer.isActive():
            self._timer.start()

    def _tick(self) -> None:
        """Advance the animation."""
        was_typing = {node_id for node_id in self._speech if self._typing(node_id)}
        self._phase += self._timer.interval() / 1000.0
        if any(not self._typing(node_id) for node_id in was_typing):
            # Someone finished speaking: show their buttons, and slow down if nothing else moves.
            self._place_buttons()
            self._update_timer()

        self.update()

    def _node_at(self, position: QPointF) -> _Node | None:
        """Return the node under a point, or whose speech bubble is under it, if any."""
        size, _gap, _top = self._metrics()
        for node in self._nodes:
            if (node.center - position).manhattanLength() < size * 0.9:
                return node

        for node in self._nodes:
            if node.node_id in self._speech and self._bubble_rect(node).contains(position):
                return node

        return None

    def mouseMoveEvent(self, event: QMouseEvent) -> None:
        """Highlight the agent under the pointer and describe it."""
        node = self._node_at(event.position())
        hover_id = node.node_id if node is not None else ""
        if hover_id != self._hover_id:
            self._hover_id = hover_id
            clickable = node is not None and node.agent is not None and node.agent.clickable
            self.setCursor(Qt.CursorShape.PointingHandCursor if clickable else Qt.CursorShape.ArrowCursor)
            self.update()

        if node is not None and node.agent is not None:
            QToolTip.showText(
                event.globalPosition().toPoint(),
                f"{node.label} · {node.caption}\n{node.agent.speech or node.agent.state_text}",
                self
            )

        super().mouseMoveEvent(event)

    def leaveEvent(self, event: QEvent) -> None:
        """Clear the highlight."""
        self._hover_id = ""
        self.update()
        super().leaveEvent(event)

    def mousePressEvent(self, event: QMouseEvent) -> None:
        """Open the conversation of a clicked agent."""
        node = self._node_at(event.position())
        if node is not None and node.agent is not None and node.agent.clickable:
            self.agent_clicked.emit(node.agent.context_id)
            event.accept()
            return

        super().mousePressEvent(event)

    def paintEvent(self, event: QPaintEvent) -> None:
        """Draw the grid, the arrows and the robots."""
        super().paintEvent(event)
        if len(self._nodes) < 2:
            return

        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        self._paint_grid(painter)
        for parent, child in self._edges:
            self._paint_edge(painter, parent, child)

        for node in self._nodes:
            if node.agent is None:
                self._paint_user(painter, node)

            else:
                self._paint_robot(painter, node)

        # Speech goes on top, so arrows and neighbours never cover what an agent says.
        for node in self._nodes:
            if node.agent is not None and node.node_id in self._speech:
                self._paint_speech(painter, node)

        painter.end()

    def _paint_grid(self, painter: QPainter) -> None:
        """Draw a floor grid receding in perspective, so the robots stand in a room rather than on a page."""
        line = QColor(self._style_manager.get_color(ColorRole.TEXT_PRIMARY))
        width, height = float(self.width()), float(self.height())
        horizon = -height * 0.35
        vanish = QPointF(width / 2, horizon)
        spacing = 46 * self._style_manager.zoom_factor()

        # Lines running into the distance, all meeting at the vanishing point above the view.
        count = int(width / spacing) + 6
        for index in range(-count, count + 1):
            foot = QPointF(width / 2 + index * spacing * 1.6, height)
            line.setAlphaF(0.06)
            painter.setPen(QPen(line, 1.0))
            painter.drawLine(vanish + (foot - vanish) * 0.42, foot)

        # Cross lines, closer together towards the horizon.
        rows = 9
        for row in range(1, rows + 1):
            depth = row / rows
            y = horizon + (height - horizon) * (0.42 + 0.58 * depth * depth)
            line.setAlphaF(0.025 + 0.05 * depth)
            painter.setPen(QPen(line, 1.0))
            painter.drawLine(QPointF(0, y), QPointF(width, y))

        # Fade the far end out, so the floor dissolves into the card rather than stopping.
        _page, card = surface_colors()
        mist = QLinearGradient(0, 0, 0, height * 0.45)
        top = QColor(card)
        top.setAlphaF(0.9)
        mist.setColorAt(0.0, top)
        clear = QColor(card)
        clear.setAlphaF(0.0)
        mist.setColorAt(1.0, clear)
        painter.fillRect(QRectF(0, 0, width, height * 0.45), QBrush(mist))

    def _paint_edge(self, painter: QPainter, parent: _Node, child: _Node) -> None:
        """Draw a curved arrow from a delegator to its agent, with particles while work flows."""
        size, _gap, _top = self._metrics()
        zoom = self._style_manager.zoom_factor()
        # Arrows leave from the bottom of what the delegator is saying, or below its name when it
        # is quiet, so the link visibly joins the two agents without crossing either label.
        if parent.agent is not None and parent.node_id in self._speech:
            start = QPointF(parent.center.x(), self._bubble_rect(parent).bottom())

        else:
            start = QPointF(parent.center.x(), parent.center.y() + self._name_bottom(parent))

        end = QPointF(child.center.x(), child.center.y() - size * 0.78)
        bend = (end.y() - start.y()) * 0.55
        path = QPainterPath(start)
        path.cubicTo(QPointF(start.x(), start.y() + bend), QPointF(end.x(), end.y() - bend), end)

        activity = child.agent.activity if child.agent is not None else "idle"
        tone = {
            "waiting": MissionTone.WARNING, "error": MissionTone.DANGER, "idle": MissionTone.NEUTRAL,
        }.get(activity, MissionTone.ACCENT)
        # An idle link keeps the team's colour, faded, so it never reads as a line of the floor grid.
        color = QColor(tone_color(MissionTone.ACCENT if tone == MissionTone.NEUTRAL else tone))
        line = QColor(color)
        line.setAlphaF(0.85 if activity != "idle" else 0.5)

        halo = QColor(color)
        halo.setAlphaF(0.16 if activity != "idle" else 0.08)
        halo_pen = QPen(halo, 8 * zoom)
        halo_pen.setCapStyle(Qt.PenCapStyle.RoundCap)
        painter.setPen(halo_pen)
        painter.setBrush(Qt.BrushStyle.NoBrush)
        painter.drawPath(path)

        pen = QPen(line, 2.5 * zoom)
        pen.setCapStyle(Qt.PenCapStyle.RoundCap)
        if activity == "waiting":
            pen.setStyle(Qt.PenStyle.DashLine)
            pen.setDashOffset(-self._phase * 8)

        painter.setPen(pen)
        painter.setBrush(Qt.BrushStyle.NoBrush)
        painter.drawPath(path)

        angle = math.radians(-path.angleAtPercent(1.0))
        head = 9 * self._style_manager.zoom_factor()
        arrow = QPainterPath(end)
        arrow.lineTo(end - QPointF(math.cos(angle - 0.45) * head, math.sin(angle - 0.45) * head))
        arrow.lineTo(end - QPointF(math.cos(angle + 0.45) * head, math.sin(angle + 0.45) * head))
        arrow.closeSubpath()
        painter.setPen(Qt.PenStyle.NoPen)
        painter.setBrush(line)
        painter.drawPath(arrow)

        # A socket where the link leaves, so it reads as plugged into the delegator.
        _page, card = surface_colors()
        painter.setPen(QPen(line, 2 * zoom))
        painter.setBrush(card)
        painter.drawEllipse(start, 4 * zoom, 4 * zoom)
        painter.setPen(Qt.PenStyle.NoPen)

        if activity in ("working", "thinking"):
            for index in range(3):
                t = (self._phase * 0.55 + index / 3) % 1.0
                point = path.pointAtPercent(t)
                glow = QRadialGradient(point, 7 * self._style_manager.zoom_factor())
                bright = QColor(color.lighter(130))
                glow.setColorAt(0.0, bright)
                fade = QColor(bright)
                fade.setAlphaF(0.0)
                glow.setColorAt(1.0, fade)
                painter.setBrush(QBrush(glow))
                painter.drawEllipse(point, 7 * self._style_manager.zoom_factor(), 7 * self._style_manager.zoom_factor())

    def _name_bottom(self, node: _Node) -> float:
        """Return how far below a node's centre its name and caption end."""
        size, _gap, _top = self._metrics()
        if node.agent is None:
            return size * 0.5 + self._name_height()

        return size * _LABEL_DROP + self._name_height() + 4 * self._style_manager.zoom_factor()

    def _paint_label(self, painter: QPainter, node: _Node, below: float) -> None:
        """Draw a node's name and caption under it."""
        _page, card = surface_colors()
        zoom = self._style_manager.zoom_factor()
        base = self._style_manager.base_font_size() * zoom
        width = max(80.0, self.width() / max(1, sum(1 for n in self._nodes if n.level == node.level)) - 12)

        name_font = QFont(self.font())
        name_font.setPointSizeF(base * 0.95)
        name_font.setBold(True)
        painter.setFont(name_font)
        painter.setPen(readable(self._style_manager.get_color(ColorRole.TEXT_HEADING), card))
        metrics = QFontMetrics(name_font)
        name_rect = QRectF(node.center.x() - width / 2, below, width, metrics.height())
        painter.drawText(name_rect, int(Qt.AlignmentFlag.AlignCenter), metrics.elidedText(
            node.label, Qt.TextElideMode.ElideRight, round(width)
        ))

        if not node.caption:
            return

        caption_font = QFont(self.font())
        caption_font.setPointSizeF(base * 0.8)
        painter.setFont(caption_font)
        painter.setPen(readable(self._style_manager.get_color(ColorRole.TEXT_INACTIVE), card))
        caption_metrics = QFontMetrics(caption_font)
        caption_rect = QRectF(node.center.x() - width / 2, below + metrics.height(), width, caption_metrics.height())
        painter.drawText(caption_rect, int(Qt.AlignmentFlag.AlignCenter), caption_metrics.elidedText(
            node.caption, Qt.TextElideMode.ElideRight, round(width)
        ))

    def _paint_user(self, painter: QPainter, node: _Node) -> None:
        """Draw the user's node: a simple person in a circle."""
        size, _gap, _top = self._metrics()
        radius = size * 0.42
        color = tone_color(MissionTone.INFO)
        gradient = QRadialGradient(node.center - QPointF(radius * 0.4, radius * 0.4), radius * 1.6)
        gradient.setColorAt(0.0, _shade(color, 160))
        gradient.setColorAt(1.0, color)
        painter.setPen(Qt.PenStyle.NoPen)
        painter.setBrush(QBrush(gradient))
        painter.drawEllipse(node.center, radius, radius)
        white = QColor("#ffffff")
        painter.setBrush(white)
        painter.drawEllipse(node.center - QPointF(0, radius * 0.25), radius * 0.28, radius * 0.28)
        body = QPainterPath()
        body.addEllipse(QRectF(node.center.x() - radius * 0.5, node.center.y() + radius * 0.12, radius, radius * 0.75))
        clip = QPainterPath()
        clip.addEllipse(node.center, radius, radius)
        painter.drawPath(body.intersected(clip))
        self._paint_label(painter, node, node.center.y() + radius + 4)

    def _paint_robot(self, painter: QPainter, node: _Node) -> None:
        """Draw an agent as a shaded robot whose motion shows what it is doing."""
        agent = node.agent
        assert agent is not None
        size, _gap, _top = self._metrics()
        activity = agent.activity
        phase = self._phase + (hash(node.node_id) % 100) / 37.0
        busy = activity in ("working", "thinking")
        bob = math.sin(phase * (6.0 if busy else 1.6)) * size * (0.05 if busy else 0.02)
        cx, cy = node.center.x(), node.center.y() + bob

        body_color = QColor(tone_color(MissionTone.ACCENT if agent.is_lead else MissionTone.INFO))
        if activity == "error":
            body_color = QColor(tone_color(MissionTone.DANGER))

        if node.node_id == self._hover_id and agent.clickable:
            ring = QColor(body_color)
            ring.setAlphaF(0.18)
            painter.setPen(Qt.PenStyle.NoPen)
            painter.setBrush(ring)
            painter.drawEllipse(QPointF(cx, cy), size * 0.95, size * 0.95)

        if busy:
            halo = QRadialGradient(QPointF(cx, cy), size * 1.1)
            glow = QColor(body_color)
            glow.setAlphaF(0.16 + 0.08 * math.sin(phase * 4))
            halo.setColorAt(0.0, glow)
            clear = QColor(body_color)
            clear.setAlphaF(0.0)
            halo.setColorAt(1.0, clear)
            painter.setPen(Qt.PenStyle.NoPen)
            painter.setBrush(QBrush(halo))
            painter.drawEllipse(QPointF(cx, cy), size * 1.1, size * 1.1)

        if agent.wait_started:
            self._paint_wait_ring(painter, QPointF(cx, cy), agent, size)

        self._paint_pedestal(painter, node, body_color, busy, phase, size)

        # Shadow on the pedestal, which shrinks as the robot bobs up, for depth.
        shadow = QRadialGradient(QPointF(node.center.x(), node.center.y() + size * 0.6), size * 0.5)
        shadow.setColorAt(0.0, QColor(0, 0, 0, 80))
        shadow.setColorAt(1.0, QColor(0, 0, 0, 0))
        painter.setPen(Qt.PenStyle.NoPen)
        painter.setBrush(QBrush(shadow))
        spread = 1.0 - bob / size
        painter.drawEllipse(QPointF(node.center.x(), node.center.y() + size * 0.6), size * 0.4 * spread, size * 0.08)

        # Torso.
        torso = QRectF(cx - size * 0.28, cy + size * 0.1, size * 0.56, size * 0.42)
        torso_gradient = QLinearGradient(torso.topLeft(), torso.bottomRight())
        torso_gradient.setColorAt(0.0, _shade(body_color, 130))
        torso_gradient.setColorAt(1.0, _shade(body_color, 70))
        painter.setBrush(QBrush(torso_gradient))
        painter.drawRoundedRect(torso, size * 0.1, size * 0.1)

        # Arms that type while working.
        arm_pen = QPen(_shade(body_color, 80), size * 0.08)
        arm_pen.setCapStyle(Qt.PenCapStyle.RoundCap)
        painter.setPen(arm_pen)
        swing = math.sin(phase * 14) * size * 0.08 if activity == "working" else 0.0
        painter.drawLine(QPointF(torso.left(), torso.top() + size * 0.1),
                         QPointF(torso.left() - size * 0.12, torso.top() + size * 0.3 + swing))
        remark = self._speech.get(node.node_id)
        if remark is not None and self._phase - remark[1] < _WAVE_SECONDS:
            # A new remark: wave at the user.
            wave = math.sin((self._phase - remark[1]) * 16) * size * 0.09
            painter.drawLine(QPointF(torso.right(), torso.top() + size * 0.1),
                             QPointF(torso.right() + size * 0.2 + wave, torso.top() - size * 0.22))

        else:
            painter.drawLine(QPointF(torso.right(), torso.top() + size * 0.1),
                             QPointF(torso.right() + size * 0.12, torso.top() + size * 0.3 - swing))

        # Head, lit from the top left.  It turns slowly from side to side, and the side
        # face that shows as it turns gives it depth.
        head = QRectF(cx - size * 0.4, cy - size * 0.5, size * 0.8, size * 0.6)
        turn = math.sin(phase * (0.9 if busy else 0.5)) * size * 0.07
        side = QRectF(head.translated(turn, size * 0.04))
        painter.setPen(Qt.PenStyle.NoPen)
        painter.setBrush(_shade(body_color, 50))
        painter.drawRoundedRect(side, size * 0.2, size * 0.2)
        head_gradient = QLinearGradient(head.topLeft(), head.bottomRight())
        head_gradient.setColorAt(0.0, _shade(body_color, 165))
        head_gradient.setColorAt(0.55, body_color)
        head_gradient.setColorAt(1.0, _shade(body_color, 65))
        painter.setPen(Qt.PenStyle.NoPen)
        painter.setBrush(QBrush(head_gradient))
        painter.drawRoundedRect(head, size * 0.2, size * 0.2)
        shine = QColor(255, 255, 255, 70)
        painter.setBrush(shine)
        painter.drawRoundedRect(
            QRectF(head.left() + size * 0.08, head.top() + size * 0.05, head.width() * 0.45, size * 0.08),
            size * 0.04, size * 0.04
        )

        # Antenna with a light that pulses faster while busy.
        top = QPointF(cx, head.top())
        tip = QPointF(cx, head.top() - size * 0.16)
        painter.setPen(QPen(_shade(body_color, 75), size * 0.04))
        painter.drawLine(top, tip)
        pulse = 0.5 + 0.5 * math.sin(phase * (8.0 if busy else 2.0))
        light = QColor(tone_color(
            MissionTone.WARNING if activity == "waiting" else MissionTone.DANGER if activity == "error"
            else MissionTone.SUCCESS if busy else MissionTone.NEUTRAL
        ))
        bulb = QRadialGradient(tip, size * 0.14)
        bright = QColor(light.lighter(140))
        bright.setAlphaF(0.55 + 0.45 * pulse)
        bulb.setColorAt(0.0, bright)
        faded = QColor(light)
        faded.setAlphaF(0.0)
        bulb.setColorAt(1.0, faded)
        painter.setPen(Qt.PenStyle.NoPen)
        painter.setBrush(QBrush(bulb))
        painter.drawEllipse(tip, size * 0.14, size * 0.14)
        painter.setBrush(light)
        painter.drawEllipse(tip, size * 0.05, size * 0.05)

        # Visor and eyes, which blink, and glance about while thinking.
        visor = QRectF(head.left() + size * 0.1, head.top() + size * 0.17, head.width() - size * 0.2, size * 0.28)
        painter.setBrush(QColor(12, 18, 32))
        painter.drawRoundedRect(visor, size * 0.12, size * 0.12)
        # A robot taking a pause keeps its eyes half closed.
        blink = (phase % 3.4) < 0.12 or bool(agent.wait_started)
        glance = math.sin(phase * 2.2) * size * 0.06 if activity == "thinking" else 0.0
        eye_color = QColor("#7df9ff") if activity != "error" else QColor("#ff8a80")
        eye_height = size * (0.02 if blink else 0.1)
        painter.setBrush(eye_color)
        for offset in (-0.14, 0.14):
            eye = QPointF(cx + size * offset + glance, visor.center().y())
            painter.drawRoundedRect(
                QRectF(eye.x() - size * 0.05, eye.y() - eye_height / 2, size * 0.1, eye_height),
                size * 0.03, size * 0.03
            )

        if self._typing(node.node_id):
            # A mouth that moves while the agent speaks.
            mouth = size * (0.015 + 0.03 * abs(math.sin(self._phase * 22)))
            painter.drawRoundedRect(
                QRectF(cx - size * 0.07, visor.bottom() - size * 0.06 - mouth / 2, size * 0.14, mouth),
                size * 0.01, size * 0.01
            )

        if agent.is_lead:
            self._paint_badge(painter, QPointF(head.left() + size * 0.02, head.top() + size * 0.02), "★",
                              tone_color(MissionTone.WARNING), size)

        if activity == "working":
            self._paint_gear(painter, QPointF(torso.right() + size * 0.18, torso.bottom() - size * 0.02), size)

        if node.node_id not in self._speech:
            self._paint_bubble(painter, activity, QPointF(head.right() + size * 0.18, head.top() - size * 0.12), size)

        self._paint_label(painter, node, node.center.y() + size * _LABEL_DROP)

    def _paint_pedestal(
        self, painter: QPainter, node: _Node, color: QColor, busy: bool, phase: float, size: float
    ) -> None:
        """Draw the round platform a robot stands on, lit from inside while the robot works."""
        center = QPointF(node.center.x(), node.center.y() + size * 0.62)
        radius_x, radius_y, depth = size * 0.58, size * 0.16, size * 0.1

        # The platform's side: a band from the top ellipse down to the bottom one.
        side = QPainterPath()
        side.moveTo(center.x() - radius_x, center.y())
        side.lineTo(center.x() - radius_x, center.y() + depth)
        side.arcTo(QRectF(center.x() - radius_x, center.y() + depth - radius_y, radius_x * 2, radius_y * 2), 180, 180)
        side.lineTo(center.x() + radius_x, center.y())
        side.arcTo(QRectF(center.x() - radius_x, center.y() - radius_y, radius_x * 2, radius_y * 2), 0, -180)
        side_gradient = QLinearGradient(center.x() - radius_x, 0, center.x() + radius_x, 0)
        side_gradient.setColorAt(0.0, _shade(color, 40))
        side_gradient.setColorAt(0.4, _shade(color, 75))
        side_gradient.setColorAt(1.0, _shade(color, 35))
        painter.setPen(Qt.PenStyle.NoPen)
        painter.setBrush(QBrush(side_gradient))
        painter.drawPath(side)

        top_gradient = QRadialGradient(center - QPointF(radius_x * 0.3, radius_y * 0.4), radius_x * 1.2)
        top_gradient.setColorAt(0.0, _shade(color, 150))
        top_gradient.setColorAt(1.0, _shade(color, 85))
        painter.setBrush(QBrush(top_gradient))
        painter.drawEllipse(center, radius_x, radius_y)

        # A ring of light that runs round the rim while the robot is busy.
        glow = QColor(tone_color(MissionTone.SUCCESS) if busy else _shade(color, 170))
        glow.setAlphaF(0.85 if busy else 0.35)
        painter.setPen(QPen(glow, max(1.0, size * 0.025)))
        painter.setBrush(Qt.BrushStyle.NoBrush)
        rim = QRectF(center.x() - radius_x * 0.86, center.y() - radius_y * 0.86, radius_x * 1.72, radius_y * 1.72)
        if busy:
            start = int((phase * 220) % 360 * 16)
            painter.drawArc(rim, start, 120 * 16)
            painter.drawArc(rim, start + 180 * 16, 120 * 16)

        else:
            painter.drawEllipse(rim)

    def _paint_badge(self, painter: QPainter, center: QPointF, glyph: str, color: QColor, size: float) -> None:
        """Draw a small round badge with a symbol."""
        radius = size * 0.15
        painter.setPen(QPen(QColor("#ffffff"), 1.5))
        painter.setBrush(color)
        painter.drawEllipse(center, radius, radius)
        font = QFont(self.font())
        font.setPixelSize(max(6, round(radius * 1.3)))
        font.setBold(True)
        painter.setFont(font)
        painter.setPen(QColor("#ffffff"))
        painter.drawText(QRectF(center.x() - radius, center.y() - radius, radius * 2, radius * 2),
                         int(Qt.AlignmentFlag.AlignCenter), glyph)

    def _paint_gear(self, painter: QPainter, center: QPointF, size: float) -> None:
        """Draw a spinning gear beside a working robot."""
        painter.save()
        painter.translate(center)
        painter.rotate((self._phase * 180) % 360)
        color = tone_color(MissionTone.SUCCESS)
        painter.setPen(Qt.PenStyle.NoPen)
        painter.setBrush(color)
        teeth = 8
        outer, inner = size * 0.13, size * 0.09
        gear = QPainterPath()
        for index in range(teeth * 2):
            radius = outer if index % 2 == 0 else inner
            angle = math.pi * index / teeth
            point = QPointF(math.cos(angle) * radius, math.sin(angle) * radius)
            if index == 0:
                gear.moveTo(point)

            else:
                gear.lineTo(point)

        gear.closeSubpath()
        hole = QPainterPath()
        hole.addEllipse(QPointF(0, 0), size * 0.04, size * 0.04)
        painter.drawPath(gear.subtracted(hole))
        painter.restore()

    def _paint_bubble(self, painter: QPainter, activity: str, anchor: QPointF, size: float) -> None:
        """Draw a speech bubble showing what the robot is doing, if anything worth showing."""
        if activity not in ("thinking", "waiting", "error", "idle"):
            return

        _page, card = surface_colors()
        radius = size * 0.2
        if activity == "idle":
            drift = (self._phase * 0.6) % 1.0
            font = QFont(self.font())
            font.setPixelSize(max(6, round(size * 0.22)))
            font.setBold(True)
            painter.setFont(font)
            z_color = QColor(readable(self._style_manager.get_color(ColorRole.TEXT_INACTIVE), card, 3.0))
            z_color.setAlphaF(1.0 - drift)
            painter.setPen(z_color)
            painter.drawText(QPointF(anchor.x(), anchor.y() - drift * size * 0.3), "z")
            return

        tone = {"thinking": MissionTone.INFO, "waiting": MissionTone.WARNING, "error": MissionTone.DANGER}[activity]
        color = tone_color(tone)
        painter.setPen(QPen(color, 1.5))
        painter.setBrush(card)
        painter.drawEllipse(anchor, radius * 1.15, radius)
        painter.drawEllipse(anchor + QPointF(-radius * 0.9, radius * 0.95), radius * 0.18, radius * 0.18)
        painter.setPen(Qt.PenStyle.NoPen)
        painter.setBrush(readable(color, card, 3.0))
        if activity == "thinking":
            for index in range(3):
                lift = math.sin(self._phase * 6 + index * 0.9) * radius * 0.18
                painter.drawEllipse(anchor + QPointF((index - 1) * radius * 0.45, lift), radius * 0.13, radius * 0.13)

            return

        font = QFont(self.font())
        font.setPixelSize(max(6, round(radius * 1.3)))
        font.setBold(True)
        painter.setFont(font)
        painter.setPen(readable(color, card, 3.0))
        painter.drawText(QRectF(anchor.x() - radius, anchor.y() - radius, radius * 2, radius * 2),
                         int(Qt.AlignmentFlag.AlignCenter), "!" if activity == "waiting" else "×")

    def _wait_fraction(self, agent: MissionAgentView) -> float:
        """Return how much of an agent's wait has passed, from 0.0 to 1.0."""
        if not agent.wait_started or agent.wait_seconds <= 0:
            return 0.0

        return max(0.0, min(1.0, (time.time() - agent.wait_started) / agent.wait_seconds))

    def _paint_wait_ring(self, painter: QPainter, center: QPointF, agent: MissionAgentView, size: float) -> None:
        """Draw a ring around a waiting robot that fills as its wait runs down."""
        zoom = self._style_manager.zoom_factor()
        radius = size * 0.86
        color = QColor(tone_color(MissionTone.INFO))
        track = QColor(color)
        track.setAlphaF(0.18)
        painter.setBrush(Qt.BrushStyle.NoBrush)
        painter.setPen(QPen(track, 3 * zoom))
        painter.drawEllipse(center, radius, radius)
        pen = QPen(color, 3 * zoom)
        pen.setCapStyle(Qt.PenCapStyle.RoundCap)
        painter.setPen(pen)
        box = QRectF(center.x() - radius, center.y() - radius, radius * 2, radius * 2)
        painter.drawArc(box, 90 * 16, -round(360 * 16 * self._wait_fraction(agent)))

    def _paint_speech(self, painter: QPainter, node: _Node) -> None:
        """Draw what an agent is saying, typed out, in a card under its name."""
        agent = node.agent
        assert agent is not None
        zoom = self._style_manager.zoom_factor()
        pad = 10 * zoom
        _full_text, started = self._speech[node.node_id]
        age = self._phase - started
        card = self._bubble_rect(node)
        text = self._card_text(node.node_id, card.width())
        _page, surface = surface_colors()
        tone = {
            "waiting": MissionTone.WARNING, "error": MissionTone.DANGER, "idle": MissionTone.NEUTRAL,
        }.get(agent.activity, MissionTone.ACCENT)
        if agent.wait_started:
            tone = MissionTone.INFO

        color = QColor(tone_color(tone))

        painter.save()
        # A new card fades and drops gently into place.
        grow = min(1.0, age / _POP_SECONDS)
        painter.setOpacity(grow)
        painter.translate(0, (1 - grow) * -6 * zoom)

        shape = QPainterPath()
        radius = 10 * zoom
        shape.addRoundedRect(card, radius, radius)
        tail = QPainterPath()
        tip_x = max(card.left() + radius * 1.5, min(node.center.x(), card.right() - radius * 1.5))
        tail.moveTo(QPointF(tip_x - 7 * zoom, card.top() + 1))
        tail.lineTo(QPointF(tip_x, card.top() - 6 * zoom))
        tail.lineTo(QPointF(tip_x + 7 * zoom, card.top() + 1))
        tail.closeSubpath()
        shape = shape.united(tail)

        painter.setPen(Qt.PenStyle.NoPen)
        painter.setBrush(QColor(0, 0, 0, 24))
        painter.drawPath(shape.translated(0, 2 * zoom))
        painter.setBrush(surface)
        painter.drawPath(shape)
        tint = QColor(color)
        tint.setAlphaF(0.08)
        painter.setBrush(tint)
        border = QColor(color)
        border.setAlphaF(0.8 if agent.activity == "waiting" else 0.45)
        painter.setPen(QPen(border, 1.2 * zoom))
        painter.drawPath(shape)

        shown = text[:max(0, int(age * _TYPING_RATE))]
        typing = len(shown) < len(text)
        text_rect = QRectF(card.left() + pad, card.top() + pad, card.width() - pad * 2, card.height())
        painter.setFont(self._speech_font())
        painter.setPen(readable(self._style_manager.get_color(ColorRole.TEXT_PRIMARY), surface))
        flags = int(Qt.TextFlag.TextWordWrap) | int(Qt.AlignmentFlag.AlignLeft) | int(Qt.AlignmentFlag.AlignTop)
        painter.drawText(text_rect, flags, shown + ("▍" if typing and int(self._phase * 4) % 2 == 0 else ""))

        if agent.wait_started and not typing:
            self._paint_wait_progress(painter, card, agent, color, surface)

        painter.restore()

    def _paint_wait_progress(
        self, painter: QPainter, card: QRectF, agent: MissionAgentView, color: QColor, surface: QColor
    ) -> None:
        """Draw the time a wait has left beside its button, and a bar along the card's foot."""
        zoom = self._style_manager.zoom_factor()
        pad = 10 * zoom
        fraction = self._wait_fraction(agent)
        left = max(0, math.ceil(agent.wait_seconds * (1 - fraction)))
        minutes, seconds = divmod(left, 60)
        hours, minutes = divmod(minutes, 60)
        clock = f"{hours}:{minutes:02d}:{seconds:02d}" if hours else f"{minutes}:{seconds:02d}"
        row = self._button_height()
        buttons = self._buttons.get(agent.context_id, ("", []))[1]
        show_time = self._time_shown.get(agent.context_id, True)
        start = buttons[-1].geometry().right() + pad if buttons and buttons[-1].isVisible() else card.left() + pad
        painter.setPen(readable(self._style_manager.get_color(ColorRole.TEXT_INACTIVE), surface))
        font = self._speech_font()
        font.setBold(True)
        painter.setFont(font)
        if show_time:
            painter.drawText(
                QRectF(start, card.bottom() - pad - row, card.right() - pad - start, row),
                int(Qt.AlignmentFlag.AlignRight) | int(Qt.AlignmentFlag.AlignVCenter),
                self._labels["left"].format(clock)
            )

        strip =QRectF(card.left() + pad, card.bottom() - 4 * zoom, card.width() - pad * 2, 2.5 * zoom)
        track = QColor(color)
        track.setAlphaF(0.18)
        painter.setPen(Qt.PenStyle.NoPen)
        painter.setBrush(track)
        painter.drawRoundedRect(strip, strip.height() / 2, strip.height() / 2)
        painter.setBrush(color)
        painter.drawRoundedRect(
            QRectF(strip.left(), strip.top(), strip.width() * fraction, strip.height()),
            strip.height() / 2, strip.height() / 2
        )
