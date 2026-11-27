"""Point-in-time views of mission state for front ends."""

from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum, auto

from mission.mission import Mission


class MissionStatus(Enum):
    """Overall state of a mission, derived from its plan and conversations."""
    PLANNING = auto()
    RUNNING = auto()
    NEEDS_YOU = auto()
    IDLE = auto()
    COMPLETED = auto()
    STOPPED = auto()
    INTERRUPTED = auto()
    PAUSED = auto()
    REPLIED = auto()
    FAILED = auto()
    RECOVERING = auto()
    STUCK = auto()
    PLAN_READY = auto()
    OUT_OF_TOKENS = auto()


class MissionAgentState(Enum):
    """What a conversation taking part in a mission is doing."""
    THINKING = auto()
    WORKING = auto()
    WAITING_FOR_APPROVAL = auto()
    IDLE = auto()
    ERROR = auto()


class MissionActivityKind(Enum):
    """Kinds of activity recorded for the activity feed."""
    MISSION_STARTED = auto()
    PLAN_UPDATED = auto()
    AGENT_JOINED = auto()
    TURN_STARTED = auto()
    TURN_FINISHED = auto()
    TOOL_USED = auto()
    APPROVAL_REQUESTED = auto()
    APPROVED = auto()
    REJECTED = auto()
    GUIDANCE_SENT = auto()
    MISSION_STOPPED = auto()
    AUTO_CONTINUED = auto()
    AUTO_PAUSED = auto()
    RESUMED = auto()
    RETRIED = auto()
    PAUSED = auto()
    UNPAUSED = auto()
    TASK_STOPPED = auto()
    MODE_CHANGED = auto()
    AUTO_APPROVED = auto()
    REVERSIBLE_APPROVED = auto()
    EVERYTHING_APPROVED = auto()
    USER_MESSAGE = auto()
    REVIEW_DEFERRED = auto()
    PLAN_ARCHIVED = auto()
    ERROR = auto()
    RECOVERY_SCHEDULED = auto()
    AUTO_RETRIED = auto()
    RECOVERY_GAVE_UP = auto()
    LOOP_DETECTED = auto()
    PLAN_APPROVED = auto()
    FILES_UNDONE = auto()
    CHECKS_PASSED = auto()
    CHECKS_FAILED = auto()
    TOKEN_LIMIT_REACHED = auto()
    TOKEN_LIMIT_CHANGED = auto()


@dataclass(frozen=True)
class MissionRecovery:
    """
    An automatic retry of a lead turn that failed for a reason that may pass, such as a lost connection.

    Attributes:
        attempt: Which automatic retry this is, counting from 1
        attempts: How many automatic retries are made before the mission is left failed
        retry_at: When the retry happens
        error: What went wrong, as the AI service reported it
    """
    attempt: int
    attempts: int
    retry_at: datetime
    error: str


class MissionStuckReason(Enum):
    """Why Mission Control thinks a mission is stuck."""
    NO_PROGRESS = auto()
    REPEATING = auto()
    CHECKS_FAILING = auto()


@dataclass(frozen=True)
class MissionStuck:
    """
    A mission Mission Control stopped driving because it was getting nowhere.

    Attributes:
        reason: What gave it away
        count: How many continues finished no step, how many times the action was repeated, or how
            many times the done checks failed
        agent_title: The conversation that repeated itself, for REPEATING
        tool_name: The tool call it repeated, for REPEATING
        tool_target: What that tool call worked on, such as a file or a command, when known
    """
    reason: MissionStuckReason
    count: int
    agent_title: str = ""
    tool_name: str = ""
    tool_target: str = ""


class MissionFileChangeKind(Enum):
    """What a mission did to a file, taking all its changes to the file together."""
    CREATED = auto()
    MODIFIED = auto()
    DELETED = auto()


class MissionFileDiffLineKind(Enum):
    """What a line of a file's diff is: added, removed, unchanged, or a run of unchanged lines left out."""
    ADDED = auto()
    REMOVED = auto()
    CONTEXT = auto()
    GAP = auto()


@dataclass(frozen=True)
class MissionFileDiffLine:
    """
    One line of what a mission did to a file.

    Attributes:
        kind: Whether the line was added, removed, left as it was, or stands for unchanged lines left out
        text: The line, without its line ending, or "" for a gap
        old_number: The line's number before the mission, or None for an added line or a gap
        new_number: The line's number as the mission left the file, or None for a removed line or a gap
        skipped: For a gap, how many unchanged lines it stands for
    """
    kind: MissionFileDiffLineKind
    text: str
    old_number: int | None = None
    new_number: int | None = None
    skipped: int = 0


@dataclass(frozen=True)
class MissionFileDiff:
    """
    What a mission did to a file, line by line, as the file was before the mission against how it left it.

    Attributes:
        lines: The changed lines with a few unchanged ones around each change, and gaps between
        more: How many further lines there were, left out to keep the diff short
    """
    lines: list[MissionFileDiffLine]
    more: int


class MissionFileChangeState(Enum):
    """Where a file the mission changed stands now."""
    CURRENT = auto()
    UNDONE = auto()
    CHANGED_SINCE = auto()


@dataclass(frozen=True)
class MissionFileChange:
    """
    A file a mission changed, as the change journal recorded it.

    Attributes:
        path: The file, relative to the mindspace
        kind: Whether the mission created, changed or deleted it
        changes: How many times the mission changed it
        state: Whether its changes are still there, were undone, or something has changed it since
        changed_at: When the mission last changed it, as an ISO-8601 time
        lines_added: Lines the mission added, or None when the file is not text or its versions are not held
        lines_removed: Lines the mission removed, or None when the file is not text or its versions are not held
        before_sequence: The journal entry holding the file as it was before the mission, or None
            when there was no such version (the mission created it) or it is no longer held
    """
    path: str
    kind: MissionFileChangeKind
    changes: int
    state: MissionFileChangeState
    changed_at: str
    lines_added: int | None = None
    lines_removed: int | None = None
    before_sequence: int | None = None


@dataclass(frozen=True)
class MissionAgent:
    """
    A conversation taking part in a mission.

    Attributes:
        context_id: Context of the conversation, used to focus it
        title: Conversation title
        is_lead: True for the mission lead, False for a delegated conversation
        state: What the conversation is doing
        tool_name: Tool being used, when state is WORKING
        parent_id: Context of the conversation that delegated to this one, or "" for the lead
            and for a conversation whose delegator is not open
        tool_target: What the tool is working on, such as a file, a URL or a command, when known
        wait_seconds: Length of a wait the user can cut short, such as a clock sleep, or 0 when not waiting
        wait_started_at: When that wait started
        model: Display name of the AI model the conversation uses
        actions: How many tool calls the conversation has made so far
        last_tool_name: The most recent tool the conversation used, kept after the tool finishes
        last_tool_target: What that tool worked on, when known
    """
    context_id: str
    title: str
    is_lead: bool
    state: MissionAgentState
    tool_name: str
    parent_id: str = ""
    tool_target: str = ""
    wait_seconds: float = 0.0
    wait_started_at: datetime | None = None
    model: str = ""
    actions: int = 0
    last_tool_name: str = ""
    last_tool_target: str = ""


@dataclass(frozen=True)
class MissionDecision:
    """
    A tool approval waiting for a human, from any open conversation.

    Attributes:
        decision_id: Identifier used to answer the decision
        context_id: Context of the conversation that asked
        conversation_title: Title of that conversation
        mission_id: Mission the conversation belongs to, if any
        tool_name: Tool asking for approval
        reason: Reason the tool gave
        detail: Optional extra detail, such as a diff
        destructive: Whether the operation is destructive
        requested_at: When the request was seen
        reviewing: Whether an AI reviewer is deciding on it now
        review_note: Why the AI reviewer left it for the user, if it did
        reversible: Whether the change is one the file change journal records, so can be undone
        target: What the request acts on, such as a file, a command or a URL, or "" if it names none
    """
    decision_id: str
    context_id: str
    conversation_title: str
    mission_id: str | None
    tool_name: str
    reason: str
    detail: str | None
    destructive: bool
    requested_at: datetime
    reviewing: bool = False
    review_note: str = ""
    reversible: bool = False
    target: str = ""


@dataclass(frozen=True)
class MissionActivity:
    """
    One entry in the activity feed.

    Attributes:
        kind: What happened
        subject: Who it happened to, usually a conversation title
        detail: Extra detail, such as a tool name
        mission_id: Mission the activity belongs to, if any
        occurred_at: When it happened
        more: The whole story, such as the full message or request, shown when the entry is opened
    """
    kind: MissionActivityKind
    subject: str
    detail: str
    mission_id: str | None
    occurred_at: datetime
    more: str = ""


class MissionSpeaker(Enum):
    """Who an entry in a mission's conversation comes from."""
    USER = auto()
    LEAD = auto()
    TOOL = auto()


@dataclass(frozen=True)
class MissionExchange:
    """
    One entry in the conversation between the user and a mission's lead.

    Attributes:
        speaker: Who it comes from
        text: What the user asked or the lead replied, in full, or for a tool, what it was used for
        at: When it was written
        model: Display name of the AI model that wrote a reply from the lead, or ""
    """
    speaker: MissionSpeaker
    text: str
    at: datetime | None = None
    model: str = ""


@dataclass(frozen=True)
class MissionSnapshot:
    """
    Everything a front end needs to show one mission.

    Attributes:
        mission: The mission record, or a copy showing a finished plan as history while the next is planned
        status: Derived overall status
        agents: Conversations taking part, lead first
        decisions: Decisions waiting on this mission's conversations
        activity: Recent activity for this mission, newest first
        lead_message: The lead's latest reply without its plan block, or "" if there is none
        lead_message_at: When that reply was written, if there is one
        conversation: The user's messages, the lead's replies and the tools it used, oldest first
        recovery: The automatic retry waiting to happen, if the lead's last turn failed and will be retried
        stuck: Why Mission Control stopped driving the mission, if it looked stuck
        working_since: When the mission last set to work, while it is working, or None
        failure_code: What kind of error ended the lead's latest turn, as the AI service named it, when it failed
        failure_message: What the AI service said about that error
    """
    mission: Mission
    status: MissionStatus
    agents: list[MissionAgent]
    decisions: list[MissionDecision]
    activity: list[MissionActivity]
    lead_message: str
    lead_message_at: datetime | None
    conversation: list[MissionExchange] = field(default_factory=list)
    recovery: MissionRecovery | None = None
    stuck: MissionStuck | None = None
    working_since: datetime | None = None
    failure_code: str = ""
    failure_message: str = ""
