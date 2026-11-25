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
    """Which steps of the plan a filter shows."""
    ALL_STEPS = auto()
    DONE_STEPS = auto()
    ACTIVE_STEPS = auto()


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
    archived: bool = False
    search_text: str = ""


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
    lead_context_id: str = ""
    can_retry: bool = False
    can_pause: bool = False
    paused: bool = False
    recovery_text: str = ""
    steps_text: str = ""
    timing_text: str = ""
    tokens_text: str = ""
    tokens_detail: str = ""
    next_text: str = ""
    recovery_error: str = ""
    plan_ready: bool = False
    failure_text: str = ""
    failure_detail: str = ""
    stuck_text: str = ""


@dataclass(frozen=True)
class MissionFileView:
    """
    A file the mission changed.

    Attributes:
        path: The file, relative to the mindspace
        glyph: Symbol for what was done to it
        tone: Colour of that symbol
        kind_text: What was done to it, in words
        state_text: Where it stands now, or "" when the mission's changes are still there
        can_undo: Whether its changes can be undone from here
        can_open: Whether it exists to be opened
        lines_added: Lines the mission added, or None when they could not be counted
        lines_removed: Lines the mission removed, or None when they could not be counted
        lines_text: The line counts in words, for a tooltip, or "" when they could not be counted
        review_sequence: The journal entry to compare the file as it is now against, or None
            when there is no earlier version to compare with
    """
    path: str
    glyph: str
    tone: MissionTone
    kind_text: str
    state_text: str
    can_undo: bool
    can_open: bool
    lines_added: int | None = None
    lines_removed: int | None = None
    lines_text: str = ""
    review_sequence: int | None = None


class MissionDiffLineKind(Enum):
    """What a line of a file's diff is: added, removed, unchanged, or a run of unchanged lines left out."""
    ADDED = auto()
    REMOVED = auto()
    CONTEXT = auto()
    GAP = auto()


@dataclass(frozen=True)
class MissionDiffLineView:
    """
    One line of a file's diff.

    Attributes:
        kind: Whether the line was added, removed, left as it was, or stands for unchanged lines left out
        text: The line, or for a gap, how many unchanged lines it stands for, in words
        old_number: The line's number before the mission, or ""
        new_number: The line's number as the mission left the file, or ""
    """
    kind: MissionDiffLineKind
    text: str
    old_number: str = ""
    new_number: str = ""


@dataclass(frozen=True)
class MissionDiffView:
    """
    What a mission did to a file, line by line.

    Attributes:
        lines: The lines to show
        note: Something to say under the lines, such as how many more there are, or why there are none
    """
    lines: list[MissionDiffLineView]
    note: str = ""


@dataclass(frozen=True)
class MissionStatView:
    """One of the plan's filters, with how many steps it shows."""
    value: str
    label: str
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
    """
    A decision waiting in the inbox.

    `target_text` says what the request acts on, and `undo_text` whether a
    destructive request can be undone once approved; either is "" when not shown.
    `answerable` is False for a request the user is not asked to answer, because its
    mission approves everything, so the card shows no buttons to answer it with.
    """
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
    target_text: str = ""
    undo_text: str = ""
    answerable: bool = True


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
        model_text: The AI model that wrote a reply from the lead, or ""
    """
    kind: str
    speaker: str
    text: str
    time_text: str
    model_text: str = ""


@dataclass(frozen=True)
class MissionGlanceView:
    """
    The mission at a glance, shown with its title: what it is doing now and how far it has got.

    Each fact has one place on the dashboard, so neither repeats what is said elsewhere: a notice
    that the mission was interrupted, paused or failed says what is happening instead of the lead,
    and requests waiting for the user are in the call to action and the Decision Inbox.

    Attributes:
        now_text: What is happening now, in the lead's words, or "" when something else says it
        now_tone: Colour of that
        progress_text: How far the mission has got and what comes next, or "" before any step is done
    """
    now_text: str
    now_tone: MissionTone
    progress_text: str


@dataclass(frozen=True)
class MissionView:
    """
    Everything shown for the selected mission.

    `lead_message` is the lead's latest reply, shown where the user replies,
    and `report_message` the final report shown once the mission is done.
    `glance` is the mission at a glance, or None where it is not shown, and
    `working_on_text` says the lead is working on the user's latest message,
    or is "" when it is not.  `celebrate` is set the one time the mission's
    completion is to be celebrated.
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
    glance: MissionGlanceView | None = None
    working_on_text: str = ""
    celebrate: bool = False
    files: list[MissionFileView] = field(default_factory=list)
    files_summary: str = ""
    report_files: list[str] = field(default_factory=list)
