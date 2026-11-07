"""Point-in-time views of mission state for front ends."""

from dataclasses import dataclass
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
    TASK_STOPPED = auto()
    MODE_CHANGED = auto()
    AUTO_APPROVED = auto()
    REVIEW_DEFERRED = auto()
    PLAN_ARCHIVED = auto()
    ERROR = auto()


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
    """
    kind: MissionActivityKind
    subject: str
    detail: str
    mission_id: str | None
    occurred_at: datetime


@dataclass(frozen=True)
class MissionSnapshot:
    """
    Everything a front end needs to show one mission.

    Attributes:
        mission: The mission record
        status: Derived overall status
        agents: Conversations taking part, lead first
        decisions: Decisions waiting on this mission's conversations
        activity: Recent activity for this mission, newest first
        lead_message: The lead's latest reply without its plan block, or "" if there is none
        lead_message_at: When that reply was written, if there is one
    """
    mission: Mission
    status: MissionStatus
    agents: list[MissionAgent]
    decisions: list[MissionDecision]
    activity: list[MissionActivity]
    lead_message: str
    lead_message_at: datetime | None
