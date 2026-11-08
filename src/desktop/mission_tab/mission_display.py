"""Display-only records consumed by the Mission Control widget."""

from dataclasses import dataclass, field
from enum import Enum, auto


class MissionTone(Enum):
    """Colour family used to present a state."""
    ACCENT = auto()
    INFO = auto()
    SUCCESS = auto()
    WARNING = auto()
    DANGER = auto()
    NEUTRAL = auto()


class MissionStatTarget(Enum):
    """Where clicking a statistic takes the user."""
    ALL_STEPS = auto()
    DONE_STEPS = auto()
    ACTIVE_STEPS = auto()
    INBOX = auto()
    AGENTS = auto()


class MissionStepState(Enum):
    """How a plan step is drawn on the timeline."""
    PENDING = auto()
    ACTIVE = auto()
    DONE = auto()
    BLOCKED = auto()
    STOPPED = auto()


@dataclass(frozen=True)
class MissionChipView:
    """A mission in the mission switcher."""
    mission_id: str
    title: str
    tone: MissionTone
    selected: bool
    status_text: str = ""
    waiting_text: str = ""


@dataclass(frozen=True)
class MissionHeaderView:
    """The mission summary card."""
    title: str
    summary: str
    status_text: str
    tone: MissionTone
    progress: float
    progress_text: str
    can_stop: bool
    can_resume: bool
    is_classic: bool
    live: bool
    auto_approve: str
    stage: int = 0
    stage_tone: MissionTone = MissionTone.ACCENT
    now_text: str = ""
    now_tone: MissionTone = MissionTone.ACCENT


@dataclass(frozen=True)
class MissionStatView:
    """One figure in the statistics row."""
    glyph: str
    value: str
    label: str
    tone: MissionTone
    target: MissionStatTarget


@dataclass(frozen=True)
class MissionStepView:
    """
    One step of the plan.

    Attributes:
        task_id: Identifier used to remember whether the step is opened
        number: Position shown on the timeline
        title: What the step is
        detail: What done looks like
        state: How the step is drawn
        status_text: Label for the step's status
        tone: Colour of the status
        progress: The lead's latest note on the step
        evidence: What shows the step is done
        agent_title: Conversation working on the step, if any
        agent_context_id: Context of that conversation while it is open, otherwise ""
        can_stop: Whether the user can stop the step
    """
    task_id: str
    number: str
    title: str
    detail: str
    state: MissionStepState
    status_text: str
    tone: MissionTone
    progress: str
    evidence: str
    agent_title: str
    agent_context_id: str
    can_stop: bool


@dataclass(frozen=True)
class MissionAgentView:
    """
    A conversation taking part in the mission.

    Attributes:
        context_id: Context of the conversation
        title: The conversation's own title
        role_text: What the agent is doing for the mission
        is_lead: Whether it leads the mission
        state_text: Label for what it is doing now
        tone: Colour of that state
        busy: Whether it is working
        clickable: Whether its conversation can be shown
        name: The agent's friendly name
        parent_id: Context of the agent that delegated to it, or ""
        activity: "thinking", "working", "waiting", "idle" or "error", for animation
        speech: What the agent tells the user it is doing, in its own words, or "" to say nothing
        decision_id: Approval request the agent is waiting on, answered from its speech bubble, or ""
        decision_destructive: Whether that request is destructive
        wait_seconds: Length of a wait the agent is in that the user can cut short, or 0
        wait_started: When that wait started, in seconds since the epoch, or 0
        model_text: The AI model the agent uses, or "" when not known
    """
    context_id: str
    title: str
    role_text: str
    is_lead: bool
    state_text: str
    tone: MissionTone
    busy: bool
    clickable: bool
    name: str = ""
    parent_id: str = ""
    activity: str = "idle"
    speech: str = ""
    decision_id: str = ""
    decision_destructive: bool = False
    wait_seconds: float = 0.0
    wait_started: float = 0.0
    model_text: str = ""


@dataclass(frozen=True)
class MissionDecisionView:
    """A decision waiting in the inbox."""
    decision_id: str
    context_id: str
    tool_name: str
    source_text: str
    time_text: str
    reason: str
    detail: str
    destructive: bool
    reviewing: bool
    review_note: str
    auto_note: str


@dataclass(frozen=True)
class MissionActivityView:
    """
    One line of the activity feed.

    Attributes:
        glyph: Symbol shown beside the line
        text: The line itself
        time_text: When it happened
        tone: Colour of the symbol
        more: The whole story, shown when the user opens the line, or "" if the line says it all
    """
    glyph: str
    text: str
    time_text: str
    tone: MissionTone
    more: str = ""


@dataclass(frozen=True)
class MissionPhaseView:
    """
    A finished plan kept in a mission's history.

    Attributes:
        title: The mission's title when the plan finished
        summary: The mission's summary when the plan finished
        time_text: When the plan finished
        count_text: How many of its steps were done, e.g. "4 of 4 steps done"
        steps: The plan's steps
    """
    title: str
    summary: str
    time_text: str
    count_text: str
    steps: list[MissionStepView]


@dataclass(frozen=True)
class MissionResultView:
    """
    The outcome of an earlier round of a mission, kept when the user asked a follow-up.

    Attributes:
        key: Identifies the round while the mission is shown
        heading: Which round it was, when it finished and how much of its plan was done
        request_text: What the user asked for in that round, or ""
        report: The lead's final report on that round, or ""
    """
    key: str
    heading: str
    request_text: str
    report: str


@dataclass(frozen=True)
class MissionChatView:
    """
    One entry in the conversation between the user and the mission's lead.

    Attributes:
        kind: "user", "lead" or "tool", for how the entry is drawn
        speaker: Who it comes from, or "" for a tool the lead used
        text: The message in full (Markdown for the lead's replies), or what a tool was used for
        time_text: When it was written
    """
    kind: str
    speaker: str
    text: str
    time_text: str


@dataclass(frozen=True)
class MissionView:
    """
    Everything shown for the selected mission.

    `lead_message` is the lead's latest reply, shown where the user replies,
    and `report_message` the final report shown once the mission is done.
    """
    header: MissionHeaderView
    stats: list[MissionStatView]
    steps: list[MissionStepView]
    agents: list[MissionAgentView]
    agents_badge: str
    activity: list[MissionActivityView]
    lead_message: str
    lead_message_time: str
    awaiting_reply: bool
    history: list[MissionPhaseView] = field(default_factory=list)
    completed: bool = False
    report_text: str = ""
    results: list[MissionResultView] = field(default_factory=list)
    report_message: str = ""
    conversation: list[MissionChatView] = field(default_factory=list)
