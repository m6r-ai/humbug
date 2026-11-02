"""
Drive a conversation view offscreen and record what it costs.

Each measured phase reports its wall-clock cost together with the share of that
cost spent delivering each kind of Qt event, which shows directly whether the
time goes into layout, painting, or Python callbacks.
"""

from collections.abc import Callable
from dataclasses import dataclass, field
import time

from PySide6.QtCore import QEvent, QObject
from PySide6.QtWidgets import QApplication, QWidget

# pylint: disable=unused-import
import syntax.parser_imports
# pylint: enable=unused-import

from ai import AIConversationHistory
from ai_transcript_conversation import AITranscriptConversation
from desktop.conversation_tab.conversation_message import ConversationMessage
from desktop.conversation_tab.conversation_widget import ConversationWidget


def _event_name(event: QEvent) -> str:
    """Return the name of an event's type."""
    return event.type().name


@dataclass
class Phase:
    """The cost of one interaction with the conversation view."""

    name: str
    seconds: float
    detail: str = ""
    event_counts: dict[str, int] = field(default_factory=dict)
    event_seconds: dict[str, float] = field(default_factory=dict)

    def top_events(self, limit: int) -> list[tuple[str, int, float]]:
        """Return the most expensive event types in this phase, worst first."""
        ranked = sorted(self.event_seconds.items(), key=lambda item: item[1], reverse=True)
        return [(name, self.event_counts[name], seconds) for name, seconds in ranked[:limit]]


class ProfilingApplication(QApplication):
    """A QApplication that attributes event delivery time to each event type."""

    def __init__(self, argv: list[str]) -> None:
        super().__init__(argv)
        self._event_counts: dict[str, int] = {}
        self._event_seconds: dict[str, float] = {}
        self._recording = False
        self._depth = 0

    def start_recording(self) -> None:
        """Discard any previous measurements and start attributing event time."""
        self._event_counts = {}
        self._event_seconds = {}
        self._recording = True

    def stop_recording(self) -> None:
        """Stop attributing event time."""
        self._recording = False

    def event_counts(self) -> dict[str, int]:
        """Return the number of events delivered of each type since recording started."""
        return self._event_counts

    def event_seconds(self) -> dict[str, float]:
        """Return the seconds spent delivering each event type since recording started."""
        return self._event_seconds

    def notify(self, receiver: QObject, event: QEvent) -> bool:
        """
        Deliver an event, recording its type and duration while recording.

        An event handler often delivers further events to other widgets, which
        arrive here re-entrantly.  Only the outermost delivery is timed, so that
        the time a nested event spends is not counted against its parent as well.
        """
        if not self._recording or self._depth > 0:
            self._depth += 1
            try:
                return super().notify(receiver, event)

            finally:
                self._depth -= 1

        self._depth = 1
        name = _event_name(event)
        start = time.perf_counter()
        try:
            return super().notify(receiver, event)

        finally:
            self._depth = 0
            elapsed = time.perf_counter() - start
            self._event_counts[name] = self._event_counts.get(name, 0) + 1
            self._event_seconds[name] = self._event_seconds.get(name, 0.0) + elapsed


def _loading_state(view: ConversationWidget) -> tuple[int, int]:
    """
    Return the numbers that change while a conversation view is still loading.

    A view is not settled until every message it is going to show has been shown, and
    widgets are revealed over several turns, so the count of visible messages has to
    be part of the settled test as well as the count of widgets.

    Args:
        view: The conversation view being watched.

    Returns:
        The number of message widgets, and how many of them are visible.
    """
    messages = view.findChildren(ConversationMessage)
    return len(messages), sum(1 for message in messages if message.isVisible())


def _pump_until_settled(app: QApplication, view: ConversationWidget, quiet_turns: int) -> int:
    """
    Process events until the view stops creating and showing message widgets.

    Args:
        app: The application delivering events to the view.
        view: The conversation view to settle.
        quiet_turns: How many turns without a change to accept as settled.

    Returns:
        The number of event-loop turns processed.
    """
    turns = 0
    stable = 0
    state = _loading_state(view)
    while stable < quiet_turns:
        app.processEvents()
        turns += 1
        current = _loading_state(view)
        stable = stable + 1 if current == state else 0
        state = current

    return turns


def write_trimmed_transcript(source_path: str, destination_path: str, limit: int) -> None:
    """Write a copy of a transcript holding only its most recent ``limit`` messages."""
    history = AITranscriptConversation(source_path).read()
    messages = history.get_messages()[-limit:]
    AITranscriptConversation(destination_path).set_conversation_history(
        AIConversationHistory(messages=messages, attachments=history.attachments())
    )


def run(
    app: ProfilingApplication,
    transcript_path: str,
    resize_steps: int,
    scroll_steps: int,
) -> list[Phase]:
    """
    Measure constructing, loading, resizing and scrolling a conversation view.

    Args:
        app: The application that delivers the events being measured.
        transcript_path: Transcript holding the conversation to display.
        resize_steps: How many width changes to measure.
        scroll_steps: How many scrollbar positions to measure.

    Returns:
        One Phase per measured interaction, in the order they were measured.
    """
    phases: list[Phase] = []

    def measure(name: str, action: Callable[[], None]) -> Phase:
        app.start_recording()
        start = time.perf_counter()
        action()
        phase = Phase(name, time.perf_counter() - start)
        app.stop_recording()
        phase.event_counts = dict(app.event_counts())
        phase.event_seconds = dict(app.event_seconds())
        phases.append(phase)
        return phase

    built: list[ConversationWidget] = []

    def construct() -> None:
        transcript = AITranscriptConversation(transcript_path)
        view = ConversationWidget(transcript_path, ai_transcript_conversation=transcript)
        view.resize(1000, 800)
        view.show()
        app.processEvents()
        built.append(view)

    phase = measure("construct", construct)
    view = built[0]
    phase.detail = f"{len(view.findChildren(ConversationMessage))} messages on show"

    load_turns: list[int] = []

    def load() -> None:
        load_turns.append(_pump_until_settled(app, view, 20))

    phase = measure("load", load)
    phase.detail = (
        f"{load_turns[0]} event-loop turns, "
        f"{len(view.findChildren(QWidget))} widgets under the view"
    )

    def resize() -> None:
        for _ in range(resize_steps):
            width = 960 if view.width() >= 1000 else 1000
            view.resize(width, 800)
            app.processEvents()

    phase = measure("resize", resize)
    phase.detail = f"{resize_steps} width changes"

    def scroll() -> None:
        scrollbar = view.scroll_area().verticalScrollBar()
        span = scrollbar.maximum() - scrollbar.minimum()
        for step in range(scroll_steps):
            scrollbar.setValue(scrollbar.minimum() + span * step // max(scroll_steps - 1, 1))
            app.processEvents()

    phase = measure("scroll", scroll)
    phase.detail = f"{scroll_steps} scrollbar positions"

    view.close_widget()
    app.processEvents()

    return phases
