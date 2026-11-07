"""Mission Control service: missions, their conversations, and pending decisions."""

import asyncio
from collections import deque
from collections.abc import Callable
from dataclasses import dataclass, replace
from datetime import datetime, timezone
import logging
import os
from typing import Any
import uuid

from ai import (
    AIConversation, AIConversationEvent, AIConversationSettings, AIMessage, AIMessageSource
)
from ai_tool import AIToolCall
from ai_transcript_conversation import AITranscriptConversation
from context.context_info import ContextInfo
from context.context_registry import ContextEvent
from conversation_context.conversation_context import ConversationContext
from mindspace.mindspace import Mindspace
from mindspace.mindspace_log_level import MindspaceLogLevel
from mission.mission import AutoApproveLevel, Mission, MissionMode, MissionPhase, MissionTaskStatus
from mission.mission_audit import MissionConversationAuditor
from mission.mission_error import MissionError
from mission.mission_plan_parser import parse_mission_plan, strip_mission_plan
from mission.mission_prompt import build_lead_prompt
from mission.mission_reviewer import MissionApprovalReviewer, ReviewRequest, ReviewVerdict
from mission.mission_snapshot import (
    MissionActivity, MissionActivityKind, MissionAgent, MissionAgentState,
    MissionDecision, MissionSnapshot, MissionStatus
)
from mission.mission_store import MissionStore


MISSIONS_DIR = "missions"

# Shown as the sender of the opening message in the lead conversation.
MISSION_REQUESTER = "Mission Control"

_REJECT_REASON = "Tool call was rejected by the user"
_UNSURE_REASON = "User is unsure about this tool request and wants to discuss"
_UNSURE_MESSAGE = "I'm not sure about this tool request. Let's discuss."

# A short pause before asking the lead to carry on lets its reply settle and gives the
# user a moment to step in.
AUTO_CONTINUE_DELAY_SECONDS = 1.5

# How many times the lead is asked to carry on without completing a step before
# Mission Control stops asking and leaves the mission waiting for the user.
MAX_CONTINUES_WITHOUT_PROGRESS = 3

_CONTINUE_MESSAGE = (
    "Continue the mission: work on the next unfinished task. Make reasonable assumptions for details "
    "you can decide yourself, note them in the task's progress, and report the updated plan. Only stop "
    "and mark a task blocked if you truly need a decision or information that only the user can give."
)

_RESUME_MESSAGE = (
    "Humbug was closed while this mission was in progress, so the work stopped part way. Review the "
    "conversation so far and check what was actually completed: an action may not have finished, and a "
    "delegated task may not have reported back. Then continue with the next unfinished task and report "
    "the updated plan."
)

# Lead conversations of autonomous missions have no tab, so they are tracked under a key of their own.
_HEADLESS_PREFIX = "mission-lead:"

# How much of the user's later messages, and of the lead's latest update, the approval reviewer sees.
_REVIEW_INSTRUCTION_COUNT = 6
_REVIEW_TEXT_LIMIT = 800

_NEW_PHASE_NOTE = (
    "(Mission Control: the previous plan is finished and has been moved to this mission's history. Treat the "
    "message above as new work: report a new mission-plan block for it.)"
)

_MAX_ACTIVITY = 300
_SNAPSHOT_ACTIVITY = 40
_MAX_TITLE_LENGTH = 72


def _tool_label(tool_call: AIToolCall) -> str:
    """Return a readable label for a tool call, naming its operation if it has one."""
    operation = tool_call.arguments.get("operation", "")
    return f"{tool_call.name}.{operation}" if operation else tool_call.name


_TARGET_ARGUMENTS = ("path", "file_path", "url", "command", "destination", "task_prompt", "message")
_MAX_TARGET_LENGTH = 80


_MAX_EXCERPT_LENGTH = 280


def _excerpt(text: str) -> str:
    """Return text on one line, shortened to fit an activity entry."""
    flat = " ".join(text.split())
    return flat if len(flat) <= _MAX_EXCERPT_LENGTH else f"{flat[:_MAX_EXCERPT_LENGTH - 1]}…"


def _tool_target(tool_call: AIToolCall) -> str:
    """Return what a tool call works on, such as a file, a URL or a command, on one short line."""
    for name in _TARGET_ARGUMENTS:
        value = tool_call.arguments.get(name)
        if isinstance(value, str) and value.strip():
            flat = " ".join(value.split())
            return flat if len(flat) <= _MAX_TARGET_LENGTH else f"{flat[:_MAX_TARGET_LENGTH - 1]}…"

    return ""


def _initial_title(objective: str) -> str:
    """Return a provisional mission title taken from the objective's first line."""
    first_line = objective.strip().splitlines()[0].strip() if objective.strip() else ""
    if len(first_line) <= _MAX_TITLE_LENGTH:
        return first_line

    return first_line[:_MAX_TITLE_LENGTH - 1].rstrip() + "…"


@dataclass
class _PendingDecision:
    """A decision together with what identifies the request it answers."""
    decision: MissionDecision
    tool_call_id: str
    message_id: str | None
    reviewing: bool = False
    review_note: str = ""


class _TrackedConversation:
    """
    Observes one open conversation on behalf of the service.

    Holds the conversation's current activity and forwards the events the
    service cares about.  Callbacks only update state and return, so they
    never hold up the conversation that raised them.
    """

    def __init__(
        self,
        service: "MissionService",
        context_id: str,
        transcript: AITranscriptConversation
    ) -> None:
        self.context_id = context_id
        self.transcript = transcript
        self.conversation: AIConversation = transcript.inner_conversation()
        self.state = MissionAgentState.IDLE
        self.tool_name = ""
        self.tool_target = ""
        self.wait_seconds = 0.0
        self.wait_started_at: datetime | None = None
        self._service = service
        self._callbacks: list[tuple[AIConversationEvent, Callable]] = [
            (AIConversationEvent.AI_CONNECTED, self._on_ai_connected),
            (AIConversationEvent.COMPLETED, self._on_completed),
            (AIConversationEvent.ERROR, self._on_error),
            (AIConversationEvent.MESSAGE_ADDED, self._on_message_added),
            (AIConversationEvent.MESSAGE_COMPLETED, self._on_message_completed),
            (AIConversationEvent.MESSAGE_ADDED_AND_COMPLETED, self._on_message_completed),
            (AIConversationEvent.TOOL_APPROVAL_REQUIRED, self._on_approval_required),
            (AIConversationEvent.TOOL_INTERRUPTIBLE_WAIT_STARTED, self._on_wait_started),
            (AIConversationEvent.TOOL_INTERRUPTIBLE_WAIT_ENDED, self._on_wait_ended),
        ]
        for event, callback in self._callbacks:
            self.conversation.register_callback(event, callback)

        if self.conversation.is_streaming():
            self.state = MissionAgentState.THINKING

    def detach(self) -> None:
        """Stop observing the conversation."""
        for event, callback in self._callbacks:
            self.conversation.unregister_callback(event, callback)

    async def _on_ai_connected(self, *_args: Any) -> None:
        """Note that the conversation started a turn."""
        self.state = MissionAgentState.THINKING
        self.tool_name = ""
        self.tool_target = ""
        self._service.handle_turn_started(self)

    async def _on_completed(self, *_args: Any) -> None:
        """Note that the conversation finished its turn."""
        self.state = MissionAgentState.IDLE
        self.tool_name = ""
        self.tool_target = ""
        self._service.handle_turn_finished(self, failed=False)

    async def _on_error(self, retries_exhausted: bool, *_args: Any) -> None:
        """Note that the conversation's turn failed for good."""
        if not retries_exhausted:
            return

        self.state = MissionAgentState.ERROR
        self.tool_name = ""
        self.tool_target = ""
        self._service.handle_turn_finished(self, failed=True)

    async def _on_message_added(self, message: AIMessage) -> None:
        """Note which tool the conversation is using."""
        if message.source != AIMessageSource.TOOL_CALL or not message.tool_calls:
            return

        self.state = MissionAgentState.WORKING
        self.tool_name = _tool_label(message.tool_calls[0])
        self.tool_target = _tool_target(message.tool_calls[0])
        self._service.handle_tool_used(self, self.tool_name)

    async def _on_wait_started(self, _message: AIMessage | None, duration: float, *_args: Any) -> None:
        """Note that a tool, such as the clock, started a wait the user can cut short."""
        self.wait_seconds = float(duration)
        self.wait_started_at = datetime.now(timezone.utc)
        self._service.handle_wait_changed(self)

    async def _on_wait_ended(self, *_args: Any) -> None:
        """Note that the wait finished or was cut short."""
        self.wait_seconds = 0.0
        self.wait_started_at = None
        self._service.handle_wait_changed(self)

    async def _on_message_completed(self, message: AIMessage) -> None:
        """Pass completed messages to the service."""
        self._service.handle_message_completed(self, message)

    async def _on_approval_required(
        self,
        message: AIMessage | None,
        tool_call: AIToolCall,
        reason: str,
        context: str | None,
        destructive: bool
    ) -> None:
        """Pass an approval request to the service."""
        self.state = MissionAgentState.WAITING_FOR_APPROVAL
        self._service.handle_approval_required(self, message, tool_call, reason, context, destructive)


class MissionService:
    """
    Backend for Mission Control.

    Missions are recorded per mindspace.  Each mission is led by an ordinary
    conversation that plans the work and delegates it with the existing
    delegate tool; this service never plans or dispatches work itself.  It
    tracks the conversations taking part, reads the plan the lead reports,
    and collects every pending tool approval in the mindspace so the user can
    answer them in one place.  Approvals are answered through the same
    methods a conversation tab uses, so there is only one approval path.

    Front ends subscribe with add_listener and read snapshots; they hold no
    mission state of their own.

    See ADR-0021.
    """

    def __init__(self, mindspace: Mindspace, reviewer: MissionApprovalReviewer | None = None) -> None:
        """
        Initialise the service for the open mindspace.

        Args:
            mindspace: The open mindspace
            reviewer: Reviews approval requests for missions that allow auto-approval
        """
        self._mindspace = mindspace
        self._reviewer = reviewer if reviewer is not None else MissionApprovalReviewer(mindspace)
        self._mindspace_path = mindspace.mindspace_path()
        self._registry = mindspace.contexts()
        self._store = MissionStore(
            os.path.join(self._mindspace_path, Mindspace.MINDSPACE_DIR, MISSIONS_DIR)
        )
        self._logger = logging.getLogger("MissionService")
        self._missions: dict[str, Mission] = {}
        self._tracked: dict[str, _TrackedConversation] = {}
        self._members: dict[str, str] = {}
        self._decisions: dict[str, _PendingDecision] = {}
        self._activity: deque[MissionActivity] = deque(maxlen=_MAX_ACTIVITY)
        self._listeners: list[Callable[[], None]] = []
        self._sync_scheduled = False
        self._started = False
        self._continues_without_progress: dict[str, int] = {}
        self._completed_at_last_continue: dict[str, int] = {}
        self._scheduled_continues: dict[str, asyncio.TimerHandle] = {}
        self._continue_tasks: set[asyncio.Task] = set()
        self._interrupted: set[str] = set()
        self._resume_automatically = False
        self._auditors: dict[str, MissionConversationAuditor] = {}

    def mindspace_path(self) -> str:
        """Return the path of the mindspace this service belongs to."""
        return self._mindspace_path

    def start(self) -> None:
        """Load missions and begin observing the mindspace's conversations."""
        if self._started:
            return

        self._started = True
        for mission in self._store.load_all():
            self._missions[mission.mission_id] = mission
            if mission.active and not mission.stopped and not mission.is_complete():
                self._interrupted.add(mission.mission_id)

        self._resume_automatically = self._store.load_resume_automatically()

        self._registry.register_callback(ContextEvent.OPENED, self._on_context_opened)
        self._registry.register_callback(ContextEvent.CLOSED, self._on_context_closed)
        self._registry.register_callback(ContextEvent.UPDATED, self._on_context_updated)
        self._sync()

        if self._resume_automatically:
            for mission_id in sorted(self._interrupted):
                self._start_resume(mission_id)

    def stop(self) -> None:
        """Stop observing the mindspace.  Missions on disk are unaffected."""
        if not self._started:
            return

        self._started = False
        self._registry.unregister_callback(ContextEvent.OPENED, self._on_context_opened)
        self._registry.unregister_callback(ContextEvent.CLOSED, self._on_context_closed)
        self._registry.unregister_callback(ContextEvent.UPDATED, self._on_context_updated)
        for mission_id in list(self._auditors):
            self._detach_headless(mission_id, close_transcript=True)

        for tracked in self._tracked.values():
            tracked.detach()

        self._tracked.clear()
        self._decisions.clear()
        self._members.clear()
        for handle in self._scheduled_continues.values():
            handle.cancel()

        self._scheduled_continues.clear()

    def add_listener(self, listener: Callable[[], None]) -> None:
        """
        Register a function called whenever mission state changes.

        Args:
            listener: Function taking no arguments
        """
        if listener not in self._listeners:
            self._listeners.append(listener)

    def remove_listener(self, listener: Callable[[], None]) -> None:
        """
        Unregister a listener added with add_listener.

        Args:
            listener: The listener to remove
        """
        if listener in self._listeners:
            self._listeners.remove(listener)

    def _notify(self) -> None:
        """Tell listeners that state changed."""
        for listener in list(self._listeners):
            try:
                listener()

            except Exception:
                self._logger.exception("Mission listener failed")

    def missions(self) -> list[Mission]:
        """Return all missions, newest first."""
        return sorted(self._missions.values(), key=lambda m: m.created_at, reverse=True)

    def snapshot(self, mission_id: str) -> MissionSnapshot | None:
        """
        Return the current view of a mission.

        Args:
            mission_id: Mission to describe

        Returns:
            The snapshot, or None if there is no such mission
        """
        mission = self._missions.get(mission_id)
        if mission is None:
            return None

        delegators = self._delegator_index(mission_id)
        agents = [
            MissionAgent(
                context_id=tracked.context_id,
                title=self._title_for(tracked.context_id),
                is_lead=self._is_lead(mission, tracked),
                state=tracked.state,
                tool_name=tracked.tool_name,
                tool_target=tracked.tool_target,
                wait_seconds=tracked.wait_seconds,
                wait_started_at=tracked.wait_started_at,
                parent_id=self._delegator_of(tracked, delegators),
            )
            for tracked in self._tracked.values()
            if self._members.get(tracked.context_id) == mission_id
        ]
        agents.sort(key=lambda agent: not agent.is_lead)

        activity = [entry for entry in reversed(self._activity) if entry.mission_id == mission_id]
        lead_message, lead_message_at = self._latest_lead_message(mission)
        decisions = self.decisions()
        return MissionSnapshot(
            mission=mission,
            status=self._status_for(mission, decisions),
            agents=agents,
            decisions=[d for d in decisions if d.mission_id == mission_id],
            activity=activity[:_SNAPSHOT_ACTIVITY],
            lead_message=lead_message,
            lead_message_at=lead_message_at,
        )

    def _latest_lead_message(self, mission: Mission) -> tuple[str, datetime | None]:
        """
        Return the lead's latest completed reply, without its plan block.

        This is usually where the lead reports progress or asks the user a
        question, so a front end shows it next to the place the user replies.
        A reply that held nothing but a plan is skipped.
        """
        tracked = self._lead_for(mission)
        if tracked is None:
            return "", None

        for message in reversed(tracked.transcript.get_conversation_history().get_messages()):
            if message.source != AIMessageSource.AI or not message.completed:
                continue

            text = strip_mission_plan(message.content)
            if text:
                return text, message.timestamp

        return "", None

    def status(self, mission_id: str) -> MissionStatus | None:
        """
        Return the derived status of a mission.

        Args:
            mission_id: Mission to check

        Returns:
            The status, or None if there is no such mission
        """
        mission = self._missions.get(mission_id)
        return self._status_for(mission) if mission is not None else None

    def decisions(self) -> list[MissionDecision]:
        """
        Return every decision waiting for the user in this mindspace, oldest first.

        A decision is only listed while its conversation is still waiting on the
        same request, so one answered elsewhere disappears even if no event said so.
        """
        live: list[MissionDecision] = []
        for context_id, pending in self._decisions.items():
            tracked = self._tracked.get(context_id)
            if tracked is None:
                continue

            request = tracked.conversation.pending_tool_authorization()
            if request is not None and request.tool_call.id == pending.tool_call_id:
                live.append(replace(pending.decision, reviewing=pending.reviewing, review_note=pending.review_note))

        return sorted(live, key=lambda d: d.requested_at)

    async def create_mission(self, objective: str) -> Mission:
        """
        Start an autonomous mission: record it, start its lead conversation and brief the lead.

        The lead conversation has no tab; the mission is followed in Mission
        Control until the user switches it to classic mode.

        Args:
            objective: What the user wants to achieve

        Returns:
            The new mission

        Raises:
            MissionError: If the objective is empty or the mission cannot be started
        """
        objective = objective.strip()
        if not objective:
            raise MissionError("Describe what you want to achieve")

        settings = self._mindspace.settings()
        if settings is None:
            raise MissionError("No mindspace is open")

        conversation_settings = AIConversationSettings(
            model=settings.model,
            provider=settings.provider,
            temperature=settings.temperature,
            reasoning=settings.reasoning,
            reasoning_effort=settings.reasoning_effort,
        )

        try:
            conversations_rel = self._mindspace.conversations_rel_path()
            self._mindspace.ensure_mindspace_dir(conversations_rel)
            timestamp = datetime.now(timezone.utc)
            title = "Mission-" + timestamp.strftime("%Y-%m-%d-%H-%M-%S-%f")[:23]
            lead_rel_path = os.path.join(conversations_rel, f"{title}.conv")
            lead_path = self._mindspace.get_absolute_path(lead_rel_path)

            conversation = AIConversation()
            conversation.update_conversation_settings(conversation_settings)
            transcript = AITranscriptConversation(lead_path, conversation)

        except Exception as e:
            raise MissionError(f"Could not create the lead conversation: {e}") from e

        mission = Mission(
            mission_id=uuid.uuid4().hex,
            objective=objective,
            title=_initial_title(objective),
            lead_path=lead_rel_path,
            created_at=timestamp.isoformat(),
            active=True,
        )
        self._store.save(mission)
        self._missions[mission.mission_id] = mission
        self._attach_headless(mission, transcript)

        self._record(MissionActivityKind.MISSION_STARTED, mission.title, _excerpt(objective), mission.mission_id)
        self._mindspace.add_interaction(
            MindspaceLogLevel.INFO,
            f"Human started mission '{mission.title}'\nobjective: '{objective[:200]}'",
            context=self._mindspace.conversation_log_context(conversation)
        )

        await transcript.submit_message(MISSION_REQUESTER, build_lead_prompt(objective))
        self._notify()
        return mission

    async def send_guidance(self, mission_id: str, text: str, requester_id: str = "") -> None:
        """
        Send the user's message to a mission's lead conversation.

        The lead conversation is opened if it is not already.  A stopped
        mission is resumed by sending it guidance.

        Args:
            mission_id: Mission to send to
            text: The user's message
            requester_id: Context sending the guidance, used to place the lead
                conversation if it has to be opened

        Raises:
            MissionError: If the mission or its lead conversation is unavailable
        """
        text = text.strip()
        user_text = text
        mission = self._missions.get(mission_id)
        if mission is None or not text:
            raise MissionError("There is no such mission")

        tracked = self._lead_for(mission)
        if tracked is None and mission.mode == MissionMode.CLASSIC:
            self.open_lead(mission_id, requester_id)
            self._sync()
            tracked = self._lead_for(mission)

        if tracked is None:
            raise MissionError("The mission's lead conversation could not be opened")

        mission.stopped = False
        self._set_active(mission, True)
        if mission.is_complete():
            self._archive_plan(mission)
            text = f"{text}\n\n{_NEW_PHASE_NOTE}"

        # The user has stepped in, so automatic continuing starts afresh.
        self._continues_without_progress.pop(mission_id, None)
        self._cancel_continue(mission_id)
        self._record(MissionActivityKind.GUIDANCE_SENT, mission.title, _excerpt(user_text), mission_id)
        self._mindspace.add_interaction(
            MindspaceLogLevel.INFO,
            f"Human sent guidance to mission '{mission.title}'",
            context=self._mindspace.conversation_log_context(tracked.conversation)
        )
        await tracked.transcript.submit_message(None, text)
        self._notify()

    def stop_mission(self, mission_id: str) -> None:
        """
        Stop every conversation taking part in a mission.

        Pending approvals for those conversations are rejected, as they are
        when a conversation is cancelled from its own tab.

        Args:
            mission_id: Mission to stop
        """
        mission = self._missions.get(mission_id)
        if mission is None:
            return

        mission.stopped = True
        self._set_active(mission, False)
        self._interrupted.discard(mission_id)
        self._cancel_continue(mission_id)
        for tracked in list(self._tracked.values()):
            if self._members.get(tracked.context_id) == mission_id:
                tracked.transcript.cancel_current_tasks()
                tracked.state = MissionAgentState.IDLE
                tracked.tool_name = ""
                tracked.tool_target = ""

        self._record(MissionActivityKind.MISSION_STOPPED, mission.title, "", mission_id)
        self._mindspace.add_interaction(MindspaceLogLevel.INFO, f"Human stopped mission '{mission.title}'")
        self._notify()

    def remove_mission(self, mission_id: str) -> None:
        """
        Remove a mission from Mission Control.  Its conversations are kept.

        Args:
            mission_id: Mission to remove

        Raises:
            MissionError: If the record cannot be removed
        """
        mission = self._missions.get(mission_id)
        if mission is None:
            return

        self._store.delete(mission_id)
        self._cancel_continue(mission_id)
        self._interrupted.discard(mission_id)
        self._detach_headless(mission_id, close_transcript=True)
        del self._missions[mission_id]
        self._members = {ctx: mid for ctx, mid in self._members.items() if mid != mission_id}
        self._mindspace.add_interaction(MindspaceLogLevel.INFO, f"Human removed mission '{mission.title}'")
        self._notify()

    def rename_mission(self, mission_id: str, title: str) -> None:
        """
        Give a mission a name of the user's choosing.

        The name is kept from then on, even when the lead reports a plan with a
        title of its own.

        Args:
            mission_id: Mission to rename
            title: The new name

        Raises:
            MissionError: If there is no such mission or the name is empty
        """
        mission = self._missions.get(mission_id)
        title = " ".join(title.split())
        if mission is None:
            raise MissionError("There is no such mission")

        if not title:
            raise MissionError("A mission needs a name")

        if title == mission.title and mission.title_locked:
            return

        old_title = mission.title
        mission.title = title[:_MAX_TITLE_LENGTH * 2]
        mission.title_locked = True
        self._save(mission)
        self._mindspace.add_interaction(MindspaceLogLevel.INFO, f"Human renamed mission '{old_title}' to '{mission.title}'")
        self._notify()

    def open_lead(self, mission_id: str, requester_id: str = "") -> str | None:
        """
        Show a mission's lead conversation as a tab, opening it if needed.

        For an autonomous mission this switches the mission to classic mode.

        Args:
            mission_id: Mission whose lead to show
            requester_id: Context asking, used to place the conversation beside it

        Returns:
            The context ID of the lead conversation, or None if it could not be opened
        """
        mission = self._missions.get(mission_id)
        if mission is None:
            return None

        if mission.mode == MissionMode.AUTONOMOUS:
            # A mission is followed in one place at a time, so showing its conversation
            # as a tab moves the mission to classic mode.
            return self.switch_to_classic(mission_id, requester_id)

        tracked = self._lead_for(mission)
        if tracked is not None:
            self._registry.focus(tracked.context_id)
            return tracked.context_id

        lead_path = self._mindspace.get_absolute_path(mission.lead_path)
        if not os.path.exists(lead_path):
            return None

        context_id = self._registry.open(
            context_type="conversation",
            path=lead_path,
            title=os.path.splitext(os.path.basename(lead_path))[0],
            requester_id=requester_id,
        )
        self._members[context_id] = mission_id
        return context_id

    def handle_wait_changed(self, _tracked: _TrackedConversation) -> None:
        """Refresh listeners when a conversation starts or finishes a wait."""
        self._notify()

    async def finish_wait(self, context_id: str) -> bool:
        """
        Cut short a wait a mission conversation is in, such as a clock sleep, so it carries on now.

        Args:
            context_id: Context of the waiting conversation

        Returns:
            True if a wait was cut short, False if the conversation was not waiting
        """
        tracked = self._tracked.get(context_id)
        if tracked is None or tracked.wait_started_at is None:
            return False

        self._mindspace.add_interaction(
            MindspaceLogLevel.INFO,
            "Human finished a tool wait early from Mission Control",
            context=self._mindspace.conversation_log_context(tracked.conversation)
        )
        await tracked.conversation.interrupt_pending_wait()
        return True

    def runs_in_background(self, info: ContextInfo, requester_id: str) -> bool:
        """
        Return True if a conversation being opened works for an autonomous mission.

        Such a conversation is delegated by the mission's lead or one of its
        helpers, and should open without taking the user's focus, because the
        user follows the mission in Mission Control.

        Args:
            info: The context being opened
            requester_id: Context that asked for it to be opened, or ""
        """
        if info.context_type != "conversation":
            return False

        mission_id = self._members.get(requester_id) if requester_id else None
        if mission_id is None:
            transcript = self._transcript_for(info.context_id)
            parent = transcript.get_conversation_history().parent() if transcript is not None else None
            if parent is None:
                return False

            for context_id, tracked in self._tracked.items():
                owner = self._members.get(context_id)
                if owner is None:
                    continue

                messages = tracked.transcript.get_conversation_history().get_messages()
                if any(message.id == parent.message_id for message in messages):
                    mission_id = owner
                    break

        mission = self._missions.get(mission_id) if mission_id is not None else None
        return mission is not None and mission.mode == MissionMode.AUTONOMOUS

    def show_beside(self, context_id: str, beside_id: str) -> None:
        """
        Bring a mission conversation forward in a column beside another context, so that one stays in view.

        Args:
            context_id: Context of the conversation to show
            beside_id: Context to keep in view, such as Mission Control
        """
        info = self._registry.get(context_id)
        beside = self._registry.get(beside_id)
        if info is None:
            return

        if beside is not None and info.column == beside.column:
            last = self._registry.MAX_COLUMNS - 1
            self._registry.move(context_id, beside.column + 1 if beside.column < last else beside.column - 1)

        self._registry.focus(context_id)

    def focus_conversation(self, context_id: str) -> None:
        """
        Bring a conversation taking part in a mission to the front.

        Args:
            context_id: Context of the conversation
        """
        if self._registry.get(context_id) is not None:
            self._registry.focus(context_id)

    async def approve(self, decision_id: str) -> bool:
        """
        Approve a pending decision.

        Args:
            decision_id: Decision to approve

        Returns:
            True if the request was approved, False if it was no longer pending
        """
        found = self._take_decision(decision_id)
        if found is None:
            return False

        tracked, pending = found
        self._log_decision(tracked, f"Human approved tool call from Mission Control: {pending.decision.tool_name}")
        self._record(
            MissionActivityKind.APPROVED, self._title_for(tracked.context_id),
            pending.decision.tool_name, pending.decision.mission_id
        )
        await tracked.transcript.approve_pending_tool_calls()
        self._notify()
        return True

    async def reject(self, decision_id: str) -> bool:
        """
        Reject a pending decision.

        Args:
            decision_id: Decision to reject

        Returns:
            True if the request was rejected, False if it was no longer pending
        """
        found = self._take_decision(decision_id)
        if found is None:
            return False

        tracked, pending = found
        self._log_decision(tracked, f"Human denied tool call from Mission Control: {pending.decision.tool_name}")
        self._record(
            MissionActivityKind.REJECTED, self._title_for(tracked.context_id),
            pending.decision.tool_name, pending.decision.mission_id
        )
        await tracked.transcript.reject_pending_tool_calls(_REJECT_REASON)
        self._notify()
        return True

    async def unsure(self, decision_id: str) -> bool:
        """
        Decline a pending decision and ask the conversation to discuss it.

        Args:
            decision_id: Decision the user is unsure about

        Returns:
            True if the request was declined, False if it was no longer pending
        """
        found = self._take_decision(decision_id)
        if found is None:
            return False

        tracked, pending = found
        self._log_decision(
            tracked,
            f"Human indicated uncertainty about tool call from Mission Control: {pending.decision.tool_name}"
        )
        self._record(
            MissionActivityKind.REJECTED, self._title_for(tracked.context_id),
            pending.decision.tool_name, pending.decision.mission_id
        )
        await tracked.transcript.reject_pending_tool_calls(_UNSURE_REASON)
        await tracked.transcript.submit_message(None, _UNSURE_MESSAGE)
        self._notify()
        return True

    def _take_decision(self, decision_id: str) -> tuple[_TrackedConversation, _PendingDecision] | None:
        """
        Remove a decision if it still matches the request its conversation is waiting on.

        A decision that no longer matches, because the request was answered or
        replaced, is discarded so it can never answer a different request.

        Args:
            decision_id: Decision to take

        Returns:
            The conversation and decision, or None if the decision is stale
        """
        context_id = next(
            (ctx for ctx, p in self._decisions.items() if p.decision.decision_id == decision_id), None
        )
        if context_id is None:
            return None

        pending = self._decisions.pop(context_id)
        tracked = self._tracked.get(context_id)
        if tracked is None:
            self._notify()
            return None

        request = tracked.conversation.pending_tool_authorization()
        if request is None or request.tool_call.id != pending.tool_call_id:
            self._notify()
            return None

        return tracked, pending

    def _log_decision(self, tracked: _TrackedConversation, content: str) -> None:
        """Record a decision in the interaction log, attributed to its conversation."""
        self._mindspace.add_interaction(
            MindspaceLogLevel.INFO,
            content,
            context=self._mindspace.conversation_log_context(tracked.conversation)
        )

    def resume_automatically(self) -> bool:
        """Return whether interrupted missions are resumed when the mindspace opens."""
        return self._resume_automatically

    def set_resume_automatically(self, enabled: bool) -> None:
        """
        Choose whether interrupted missions are resumed when the mindspace opens.

        Args:
            enabled: True to resume them automatically

        Raises:
            MissionError: If the preference cannot be saved
        """
        self._store.save_resume_automatically(enabled)
        self._resume_automatically = enabled
        self._notify()

    def is_interrupted(self, mission_id: str) -> bool:
        """
        Return True if a mission was working when Humbug last closed and has not been resumed.

        Args:
            mission_id: Mission to check
        """
        return mission_id in self._interrupted

    async def resume_mission(self, mission_id: str, requester_id: str = "") -> None:
        """
        Carry on with a mission that was interrupted when Humbug closed.

        The lead conversation is opened if needed and told what happened, so it
        checks what was actually finished before continuing.

        Args:
            mission_id: Mission to resume
            requester_id: Context asking, used to place the lead conversation if it has to be opened

        Raises:
            MissionError: If the mission or its lead conversation is unavailable
        """
        mission = self._missions.get(mission_id)
        if mission is None or mission_id not in self._interrupted:
            raise MissionError("This mission is not waiting to be resumed")

        # The lead's tab may have been restored since the last scan; finding it avoids opening a second one.
        self._sync()
        tracked = self._lead_for(mission)
        if tracked is None and mission.mode == MissionMode.CLASSIC:
            self.open_lead(mission_id, requester_id)
            self._sync()
            tracked = self._lead_for(mission)

        if tracked is None:
            raise MissionError("The mission's lead conversation could not be opened")

        self._interrupted.discard(mission_id)
        self._continues_without_progress.pop(mission_id, None)
        self._set_active(mission, True)
        self._record(MissionActivityKind.RESUMED, mission.title, "", mission_id)
        self._mindspace.add_interaction(
            MindspaceLogLevel.INFO,
            f"Mission '{mission.title}' resumed after Humbug was closed",
            context=self._mindspace.conversation_log_context(tracked.conversation)
        )
        await tracked.transcript.submit_message(MISSION_REQUESTER, _RESUME_MESSAGE)
        self._notify()

    def _start_resume(self, mission_id: str) -> None:
        """Resume an interrupted mission in the background, logging rather than raising on failure."""
        async def resume() -> None:
            try:
                await self.resume_mission(mission_id)

            except MissionError as e:
                self._logger.warning("Could not resume mission '%s': %s", mission_id, e)

        self._run_task(resume(), mission_id)

    def _set_active(self, mission: Mission, active: bool) -> None:
        """Record whether a mission is working, saving only when that changes."""
        if active:
            self._interrupted.discard(mission.mission_id)

        if mission.active != active:
            mission.active = active
            self._save(mission)

    def handle_turn_started(self, tracked: _TrackedConversation) -> None:
        """Record that a conversation began a turn."""
        mission_id = self._members.get(tracked.context_id)
        if mission_id is not None:
            self._record(MissionActivityKind.TURN_STARTED, self._title_for(tracked.context_id), "", mission_id)
            mission = self._missions.get(mission_id)
            if mission is not None:
                self._set_active(mission, True)

        self._notify()

    def handle_turn_finished(self, tracked: _TrackedConversation, failed: bool) -> None:
        """Record that a conversation finished a turn, successfully or not."""
        self._decisions.pop(tracked.context_id, None)
        mission_id = self._members.get(tracked.context_id)
        if mission_id is not None:
            kind = MissionActivityKind.ERROR if failed else MissionActivityKind.TURN_FINISHED
            self._record(kind, self._title_for(tracked.context_id), "", mission_id)
            mission = self._missions.get(mission_id)
            if not failed and mission is not None and self._is_lead(mission, tracked):
                self._consider_continuing(mission)

                # Unless it is about to carry on, the mission is now waiting for the user.  A failed
                # turn leaves the mission marked active: that is also what happens when Humbug closes
                # mid-turn, so the mission can be resumed later.
                if mission_id not in self._scheduled_continues:
                    self._set_active(mission, False)

        self._notify()

    def _can_continue(self, mission: Mission, decisions: list[MissionDecision] | None = None) -> bool:
        """
        Return True if the lead should be asked to carry on with a mission by itself.

        It is not asked when the mission is stopped or finished, when a step is
        blocked on the user, when an approval is waiting, or while any of the
        mission's conversations is still working.

        Args:
            mission: Mission to check
            decisions: Decisions already computed for this operation, or None to compute them
        """
        if not self._started or mission.stopped or mission.is_complete():
            return False

        if mission.mission_id in self._interrupted or mission.mode == MissionMode.CLASSIC:
            return False

        if any(task.status == MissionTaskStatus.BLOCKED for task in mission.tasks):
            return False

        live = decisions if decisions is not None else self.decisions()
        if any(d.mission_id == mission.mission_id for d in live):
            return False

        lead = self._lead_for(mission)
        if lead is None:
            return False

        return not any(
            tracked.state in (
                MissionAgentState.THINKING, MissionAgentState.WORKING, MissionAgentState.WAITING_FOR_APPROVAL
            )
            for tracked in self._tracked.values()
            if self._members.get(tracked.context_id) == mission.mission_id
        )

    def _consider_continuing(self, mission: Mission) -> None:
        """Schedule an automatic continue after the lead's turn, unless the mission has stalled."""
        mission_id = mission.mission_id
        if mission_id in self._scheduled_continues or not self._can_continue(mission):
            return

        completed = mission.completed_count()
        if completed > self._completed_at_last_continue.get(mission_id, -1):
            self._continues_without_progress[mission_id] = 0

        attempts = self._continues_without_progress.get(mission_id, 0)
        if attempts >= MAX_CONTINUES_WITHOUT_PROGRESS:
            if attempts == MAX_CONTINUES_WITHOUT_PROGRESS:
                self._continues_without_progress[mission_id] = attempts + 1
                self._record(
                    MissionActivityKind.AUTO_PAUSED, mission.title, str(MAX_CONTINUES_WITHOUT_PROGRESS), mission_id
                )

            return

        self._scheduled_continues[mission_id] = asyncio.get_running_loop().call_later(
            AUTO_CONTINUE_DELAY_SECONDS, self._start_continue, mission_id
        )

    def _cancel_continue(self, mission_id: str) -> None:
        """Cancel an automatic continue that has not happened yet."""
        handle = self._scheduled_continues.pop(mission_id, None)
        if handle is not None:
            handle.cancel()

    def _start_continue(self, mission_id: str) -> None:
        """Run a scheduled automatic continue."""
        self._scheduled_continues.pop(mission_id, None)
        self._run_task(self._continue_mission(mission_id), mission_id)

    async def _continue_mission(self, mission_id: str) -> None:
        """Ask a mission's lead to carry on, if that is still appropriate."""
        mission = self._missions.get(mission_id)
        if mission is None or not self._can_continue(mission):
            return

        lead = self._lead_for(mission)
        if lead is None:
            return

        self._continues_without_progress[mission_id] = self._continues_without_progress.get(mission_id, 0) + 1
        self._completed_at_last_continue[mission_id] = mission.completed_count()
        self._record(MissionActivityKind.AUTO_CONTINUED, mission.title, "", mission_id)
        self._mindspace.add_interaction(
            MindspaceLogLevel.INFO,
            f"Mission Control asked the lead of mission '{mission.title}' to continue",
            context=self._mindspace.conversation_log_context(lead.conversation)
        )
        await lead.transcript.submit_message(MISSION_REQUESTER, _CONTINUE_MESSAGE)
        self._notify()

    def handle_tool_used(self, tracked: _TrackedConversation, tool_name: str) -> None:
        """Record that a conversation called a tool."""
        mission_id = self._members.get(tracked.context_id)
        if mission_id is not None:
            self._record(MissionActivityKind.TOOL_USED, self._title_for(tracked.context_id), tool_name, mission_id)

        self._notify()

    def handle_message_completed(self, tracked: _TrackedConversation, message: AIMessage) -> None:
        """Clear answered decisions and pick up plan updates from the lead."""
        pending = self._decisions.get(tracked.context_id)
        if pending is not None and pending.message_id == message.id:
            del self._decisions[tracked.context_id]
            if tracked.state == MissionAgentState.WAITING_FOR_APPROVAL:
                tracked.state = MissionAgentState.WORKING

        mission_id = self._members.get(tracked.context_id)
        if mission_id is not None and message.source == AIMessageSource.AI:
            mission = self._missions.get(mission_id)
            if mission is not None and self._is_lead(mission, tracked):
                self._apply_plan(mission, message.content, announce=True)

        self._notify()

    def handle_approval_required(
        self,
        tracked: _TrackedConversation,
        message: AIMessage | None,
        tool_call: AIToolCall,
        reason: str,
        context: str | None,
        destructive: bool
    ) -> None:
        """Add a conversation's approval request to the decision inbox, and have it reviewed if allowed."""
        self._add_decision(tracked, message, tool_call, reason, context, destructive)
        mission_id = self._members.get(tracked.context_id)
        if mission_id is not None:
            self._record(
                MissionActivityKind.APPROVAL_REQUESTED, self._title_for(tracked.context_id),
                _tool_label(tool_call), mission_id
            )
            self._consider_review(tracked.context_id)

        self._notify()

    def set_auto_approve(self, mission_id: str, level: AutoApproveLevel) -> None:
        """
        Choose which of a mission's approval requests an AI reviewer may approve without the user.

        Requests already waiting are reviewed straight away under the new level.

        Args:
            mission_id: Mission to change
            level: The new level

        Raises:
            MissionError: If there is no such mission
        """
        mission = self._missions.get(mission_id)
        if mission is None:
            raise MissionError("There is no such mission")

        if mission.auto_approve == level:
            return

        mission.auto_approve = level
        self._save(mission)
        self._mindspace.add_interaction(
            MindspaceLogLevel.INFO, f"Human set auto-approve for mission '{mission.title}' to {level.value}"
        )
        for context_id, pending in list(self._decisions.items()):
            if pending.decision.mission_id == mission_id and not pending.reviewing:
                # A new level is a new instruction, so requests already left for the user are reconsidered.
                pending.review_note = ""
                self._consider_review(context_id)

        self._notify()

    def _user_instructions(self, mission: Mission) -> list[str]:
        """Return the most recent messages the user typed to a mission's lead, oldest first."""
        tracked = self._lead_for(mission)
        if tracked is None:
            return []

        messages = [
            message.content.strip()[:_REVIEW_TEXT_LIMIT]
            for message in tracked.transcript.get_conversation_history().get_messages()
            if message.source == AIMessageSource.USER and not message.tool_results and not message.user_name
            and message.content.strip()
        ]
        return messages[-_REVIEW_INSTRUCTION_COUNT:]

    def _consider_review(self, context_id: str) -> None:
        """Send a pending decision to the AI reviewer if its mission's auto-approve level covers it."""
        pending = self._decisions.get(context_id)
        if pending is None or pending.reviewing or pending.review_note:
            return

        mission = self._missions.get(pending.decision.mission_id or "")
        if mission is None or mission.mode != MissionMode.AUTONOMOUS:
            return

        if mission.auto_approve == AutoApproveLevel.OFF:
            return

        if mission.auto_approve == AutoApproveLevel.SAFE and pending.decision.destructive:
            return

        pending.reviewing = True
        self._run_task(self._review_decision(mission, context_id, pending), mission.mission_id)

    async def _review_decision(self, mission: Mission, context_id: str, pending: _PendingDecision) -> None:
        """Ask the reviewer about a decision and act on the verdict if the request is still waiting."""
        tracked = self._tracked.get(context_id)
        if tracked is None:
            return

        decision = pending.decision
        lead_update, _when = self._latest_lead_message(mission)
        request = ReviewRequest(
            objective=mission.objective,
            title=mission.title,
            summary=mission.summary,
            instructions=self._user_instructions(mission),
            lead_update=lead_update[:_REVIEW_TEXT_LIMIT],
            plan=[f"{task.task_id}. {task.title} [{task.status.value}]" for task in mission.tasks],
            tool_name=decision.tool_name,
            reason=decision.reason,
            detail=decision.detail or "",
            destructive=decision.destructive,
            settings=tracked.conversation.conversation_settings(),
        )
        try:
            verdict = await self._reviewer.review(request)

        except Exception as e:
            self._logger.warning("Approval review failed: %s", e)
            verdict = ReviewVerdict(False, "The review could not be completed")

        if self._decisions.get(context_id) is not pending:
            return

        still_allowed = mission.auto_approve == AutoApproveLevel.ALL or (
            mission.auto_approve == AutoApproveLevel.SAFE and not decision.destructive
        )
        if verdict.approved and still_allowed and mission.mode == MissionMode.AUTONOMOUS:
            found = self._take_decision(decision.decision_id)
            if found is not None:
                found_tracked, _ = found
                self._log_decision(
                    found_tracked,
                    f"AI reviewer approved tool call for mission '{mission.title}': {decision.tool_name}\n"
                    f"reason: {verdict.reason}"
                )
                self._record(
                    MissionActivityKind.AUTO_APPROVED, self._title_for(context_id), decision.tool_name,
                    mission.mission_id
                )
                await found_tracked.transcript.approve_pending_tool_calls()
                self._notify()
                return

        pending.reviewing = False
        pending.review_note = verdict.reason
        self._log_decision(
            tracked,
            f"AI reviewer left tool call for the human: {decision.tool_name}\nreason: {verdict.reason}"
        )
        self._record(
            MissionActivityKind.REVIEW_DEFERRED, self._title_for(context_id), decision.tool_name, mission.mission_id
        )
        self._notify()

    def _add_decision(
        self,
        tracked: _TrackedConversation,
        message: AIMessage | None,
        tool_call: AIToolCall,
        reason: str,
        context: str | None,
        destructive: bool
    ) -> None:
        """Record a pending approval for a conversation, replacing any earlier one."""
        decision = MissionDecision(
            decision_id=uuid.uuid4().hex,
            context_id=tracked.context_id,
            conversation_title=self._title_for(tracked.context_id),
            mission_id=self._members.get(tracked.context_id),
            tool_name=_tool_label(tool_call),
            reason=reason,
            detail=context,
            destructive=destructive,
            requested_at=datetime.now(timezone.utc),
        )
        self._decisions[tracked.context_id] = _PendingDecision(
            decision=decision,
            tool_call_id=tool_call.id,
            message_id=message.id if message is not None else None,
        )

    def _on_context_opened(self, info: ContextInfo, _is_ephemeral: bool, requester_id: str) -> None:
        """Note conversations delegated from a mission conversation, then resynchronise."""
        if info.context_type != "conversation":
            return

        mission_id = self._members.get(requester_id) if requester_id else None
        if mission_id is not None and info.context_id not in self._members:
            self._members[info.context_id] = mission_id
            self._record(MissionActivityKind.AGENT_JOINED, info.title, "", mission_id)

        self._schedule_sync()

    def _on_context_closed(self, _context_id: str) -> None:
        """Resynchronise after a context closes."""
        self._schedule_sync()

    def _on_context_updated(self, info: ContextInfo) -> None:
        """Refresh listeners when a conversation's title changes."""
        if info.context_id in self._tracked:
            self._notify()

    def _schedule_sync(self) -> None:
        """
        Resynchronise once the current registry change has finished.

        A conversation tab registers its model after the registry announces the
        context, so synchronising immediately could miss it.
        """
        if self._sync_scheduled:
            return

        self._sync_scheduled = True
        asyncio.get_running_loop().call_soon(self._run_scheduled_sync)

    def _run_scheduled_sync(self) -> None:
        """Run a scheduled resynchronisation."""
        self._sync_scheduled = False
        if self._started:
            self._sync()

    def _transcript_for(self, context_id: str) -> AITranscriptConversation | None:
        """Return the transcript behind a conversation context, if it has one yet."""
        context = self._registry.get_model(context_id, ConversationContext)
        if context is not None:
            return context.ai_transcript_conversation()

        return self._registry.get_model(context_id, AITranscriptConversation)

    def _sync(self) -> None:
        """Match tracked conversations to the conversations currently open."""
        open_transcripts: dict[str, AITranscriptConversation] = {}
        for info in self._registry.list_all():
            if info.context_type != "conversation":
                continue

            transcript = self._transcript_for(info.context_id)
            if transcript is not None:
                open_transcripts[info.context_id] = transcript

        for context_id in list(self._tracked):
            if context_id.startswith(_HEADLESS_PREFIX):
                continue

            transcript = open_transcripts.get(context_id)
            tracked = self._tracked[context_id]
            if transcript is None or transcript.inner_conversation() is not tracked.conversation:
                tracked.detach()
                del self._tracked[context_id]
                self._decisions.pop(context_id, None)
                if transcript is None:
                    self._members.pop(context_id, None)

        lead_paths = {
            os.path.normpath(self._mindspace.get_absolute_path(mission.lead_path)): mission.mission_id
            for mission in self._missions.values()
        }
        for context_id, transcript in open_transcripts.items():
            lead_mission_id = lead_paths.get(os.path.normpath(transcript.path()))
            if lead_mission_id is not None:
                lead_mission = self._missions[lead_mission_id]
                if lead_mission.mode == MissionMode.AUTONOMOUS:
                    # The user opened the conversation of an autonomous mission as a tab.
                    self._take_over_by_tab(lead_mission)

                if context_id not in self._members:
                    self._members[context_id] = lead_mission_id

                continue

        delegators: dict[str, str] = {}
        for context_id, owner_id in self._members.items():
            owner = self._tracked.get(context_id)
            if owner is None:
                continue

            for message in owner.transcript.get_conversation_history().get_messages():
                delegators[message.id] = owner_id

        for context_id, transcript in open_transcripts.items():
            if context_id in self._members:
                continue

            child_mission_id = self._mission_for_child(transcript, delegators)
            if child_mission_id is not None:
                self._members[context_id] = child_mission_id
                self._record(
                    MissionActivityKind.AGENT_JOINED, self._title_for(context_id), "", child_mission_id
                )

        for context_id, transcript in open_transcripts.items():
            if context_id in self._tracked:
                continue

            tracked = _TrackedConversation(self, context_id, transcript)
            self._tracked[context_id] = tracked
            request = tracked.conversation.pending_tool_authorization()
            if request is not None:
                tracked.state = MissionAgentState.WAITING_FOR_APPROVAL
                self._add_decision(
                    tracked, request.message, request.tool_call, request.reason,
                    request.context, request.destructive
                )

        for tracked in self._tracked.values():
            mission_id = lead_paths.get(os.path.normpath(tracked.transcript.path()))
            mission = self._missions.get(mission_id) if mission_id is not None else None
            if mission is not None:
                self._refresh_plan_from_history(mission, tracked)

        self._notify()

    def _refresh_plan_from_history(self, mission: Mission, tracked: _TrackedConversation) -> None:
        """Pick up the latest plan the lead reported, including while it was not observed."""
        for message in reversed(tracked.transcript.get_conversation_history().get_messages()):
            if message.source != AIMessageSource.AI:
                continue

            if parse_mission_plan(message.content) is not None:
                self._apply_plan(mission, message.content, announce=False)
                return

    def _apply_plan(self, mission: Mission, text: str, announce: bool) -> None:
        """Update a mission from a plan block in the lead's message, if there is one."""
        plan = parse_mission_plan(text)
        if plan is None:
            return

        if mission.history and not mission.tasks and (
            [t.to_dict() for t in plan.tasks] == [t.to_dict() for t in mission.history[-1].tasks]
        ):
            # The lead repeated the plan that was just moved to history; it is not new work.
            return

        starts_new_work = any(
            task.status not in (MissionTaskStatus.COMPLETED, MissionTaskStatus.STOPPED)
            and task.task_id not in mission.stopped_tasks
            for task in plan.tasks
        )
        if mission.is_complete() and starts_new_work:
            # New unfinished work after the plan was done: the finished plan becomes history.
            self._archive_plan(mission)

        for task in plan.tasks:
            if task.task_id in mission.stopped_tasks:
                task.status = MissionTaskStatus.STOPPED

        new_tasks = [task.to_dict() for task in plan.tasks]
        old_tasks = [task.to_dict() for task in mission.tasks]
        # A name the user chose is kept; the lead's title only names missions the user has not renamed.
        title = mission.title if mission.title_locked else (plan.title or mission.title)
        summary = plan.summary or mission.summary
        if new_tasks == old_tasks and title == mission.title and summary == mission.summary:
            return

        mission.tasks = plan.tasks
        mission.title = title[:_MAX_TITLE_LENGTH * 2]
        mission.summary = summary
        self._save(mission)
        if announce:
            self._record(MissionActivityKind.PLAN_UPDATED, mission.title, "", mission.mission_id)

    def _archive_plan(self, mission: Mission) -> None:
        """Move a mission's finished plan into its history, so new work starts from an empty plan."""
        if not mission.tasks:
            return

        mission.history.append(MissionPhase(
            title=mission.title,
            summary=mission.summary,
            tasks=list(mission.tasks),
            finished_at=datetime.now(timezone.utc).isoformat(),
        ))
        mission.tasks = []
        mission.stopped_tasks = []
        self._continues_without_progress.pop(mission.mission_id, None)
        self._completed_at_last_continue.pop(mission.mission_id, None)
        self._save(mission)
        self._record(MissionActivityKind.PLAN_ARCHIVED, mission.title, "", mission.mission_id)

    def _save(self, mission: Mission) -> None:
        """Persist a mission, logging rather than raising if the write fails."""
        try:
            self._store.save(mission)

        except MissionError as e:
            self._logger.warning("Could not save mission '%s': %s", mission.mission_id, e)

    def _lead_for(self, mission: Mission) -> _TrackedConversation | None:
        """
        Return the tracked lead conversation of a mission.

        An autonomous mission's lead is loaded from its transcript the first time
        it is needed; a classic mission's lead is only available while its tab is open.
        """
        if mission.mode == MissionMode.AUTONOMOUS:
            tracked = self._tracked.get(_HEADLESS_PREFIX + mission.mission_id)
            if tracked is None and self._started:
                tracked = self._load_headless(mission)

            return tracked

        for context_id, tracked in self._tracked.items():
            if not context_id.startswith(_HEADLESS_PREFIX) and self._is_lead(mission, tracked):
                return tracked

        return None

    def _attach_headless(self, mission: Mission, transcript: AITranscriptConversation) -> _TrackedConversation:
        """Follow an autonomous mission's lead conversation, which has no tab."""
        key = _HEADLESS_PREFIX + mission.mission_id
        tracked = _TrackedConversation(self, key, transcript)
        self._tracked[key] = tracked
        self._members[key] = mission.mission_id
        self._auditors[mission.mission_id] = MissionConversationAuditor(self._mindspace, transcript)
        return tracked

    def _load_headless(self, mission: Mission) -> _TrackedConversation | None:
        """Load an autonomous mission's lead conversation from its transcript."""
        lead_path = self._mindspace.get_absolute_path(mission.lead_path)
        settings = self._mindspace.settings()
        if not os.path.exists(lead_path) or settings is None:
            return None

        try:
            transcript = AITranscriptConversation(lead_path)
            history = transcript.read()

        except Exception as e:
            self._logger.warning("Could not load the lead conversation of mission '%s': %s", mission.mission_id, e)
            return None

        transcript.update_conversation_settings(AIConversationSettings(
            model=settings.model,
            provider=settings.provider,
            temperature=settings.temperature,
            reasoning=settings.reasoning,
            reasoning_effort=settings.reasoning_effort,
        ))
        transcript.load_history(history)
        tracked = self._attach_headless(mission, transcript)
        self._refresh_plan_from_history(mission, tracked)
        return tracked

    def _detach_headless(self, mission_id: str, close_transcript: bool) -> _TrackedConversation | None:
        """Stop following an autonomous mission's lead conversation, returning it if it was followed."""
        key = _HEADLESS_PREFIX + mission_id
        auditor = self._auditors.pop(mission_id, None)
        if auditor is not None:
            auditor.detach()

        tracked = self._tracked.pop(key, None)
        self._members.pop(key, None)
        self._decisions.pop(key, None)
        if tracked is None:
            return None

        tracked.detach()
        if close_transcript:
            tracked.transcript.close()

        return tracked

    def _take_over_by_tab(self, mission: Mission) -> None:
        """
        Move an autonomous mission to classic mode because its conversation was opened as a tab.

        The tab has loaded the transcript itself, so the conversation Mission
        Control was driving is stopped and let go: two copies must never write
        the same transcript.
        """
        self._cancel_continue(mission.mission_id)
        tracked = self._detach_headless(mission.mission_id, close_transcript=False)
        if tracked is not None:
            tracked.transcript.cancel_current_tasks(notify=False)
            tracked.transcript.close()

        mission.mode = MissionMode.CLASSIC
        self._save(mission)
        self._record(MissionActivityKind.MODE_CHANGED, mission.title, MissionMode.CLASSIC.value, mission.mission_id)

    def _delegator_index(self, mission_id: str) -> dict[str, str]:
        """
        Map every message id in a mission's conversations to the context that holds it.

        A delegated conversation records the message that delegated to it, so this
        index turns "which conversation sent this message" into one lookup instead
        of a scan of every conversation's history per agent.

        Args:
            mission_id: Mission whose conversations to index

        Returns:
            Message id to context id
        """
        index: dict[str, str] = {}
        for context_id, tracked in self._tracked.items():
            if self._members.get(context_id) != mission_id:
                continue

            for message in tracked.transcript.get_conversation_history().get_messages():
                index.setdefault(message.id, context_id)

        return index

    def _delegator_of(self, tracked: _TrackedConversation, delegators: dict[str, str]) -> str:
        """Return the context of the mission conversation that delegated to this one, if it is followed."""
        parent = tracked.transcript.get_conversation_history().parent()
        if parent is None:
            return ""

        context_id = delegators.get(parent.message_id, "")
        return context_id if context_id != tracked.context_id else ""

    def _mission_for_child(self, transcript: AITranscriptConversation, delegators: dict[str, str]) -> str | None:
        """
        Return the mission whose conversation delegated to this one, if any.

        Args:
            transcript: The delegated conversation
            delegators: Index of every tracked conversation's message ids to their mission ids
        """
        parent = transcript.get_conversation_history().parent()
        if parent is None:
            return None

        return delegators.get(parent.message_id)

    def mission_for_conversation(self, path: str) -> Mission | None:
        """
        Return the mission led by the conversation at a path, if any.

        Args:
            path: Absolute path of a conversation transcript
        """
        target = os.path.normpath(path)
        for mission in self._missions.values():
            if os.path.normpath(self._mindspace.get_absolute_path(mission.lead_path)) == target:
                return mission

        return None

    def switch_to_classic(self, mission_id: str, requester_id: str = "") -> str | None:
        """
        Follow a mission through its conversation tab instead of Mission Control.

        The running conversation is handed to the tab as it is, so nothing in
        progress is interrupted.  Mission Control stops carrying the mission on.

        Args:
            mission_id: Mission to switch
            requester_id: Context asking, used to place the tab beside it

        Returns:
            The context of the conversation tab, or None if it could not be opened
        """
        mission = self._missions.get(mission_id)
        if mission is None:
            return None

        lead_path = self._mindspace.get_absolute_path(mission.lead_path)
        if not os.path.exists(lead_path):
            return None

        self._cancel_continue(mission_id)
        tracked = self._detach_headless(mission_id, close_transcript=False)
        was_autonomous = mission.mode == MissionMode.AUTONOMOUS
        mission.mode = MissionMode.CLASSIC
        self._save(mission)

        context_id = self._registry.open(
            context_type="conversation",
            path=lead_path,
            title=os.path.splitext(os.path.basename(lead_path))[0],
            initial_model=tracked.transcript if tracked is not None else None,
            requester_id=requester_id,
        )
        self._members[context_id] = mission_id
        if was_autonomous:
            self._record(MissionActivityKind.MODE_CHANGED, mission.title, MissionMode.CLASSIC.value, mission_id)
            self._mindspace.add_interaction(
                MindspaceLogLevel.INFO, f"Human switched mission '{mission.title}' to classic mode"
            )

        self._sync()
        return context_id

    def switch_to_autonomous(self, mission_id: str) -> None:
        """
        Follow a mission in Mission Control instead of its conversation tab.

        The tab is closed and Mission Control carries the mission on.  A
        conversation in the middle of a reply is not switched, because closing
        its tab would cut the reply off.

        Args:
            mission_id: Mission to switch

        Raises:
            MissionError: If the mission does not exist or its conversation is busy
        """
        mission = self._missions.get(mission_id)
        if mission is None:
            raise MissionError("There is no such mission")

        if mission.mode == MissionMode.AUTONOMOUS:
            return

        tracked = self._lead_for(mission)
        if tracked is not None:
            busy = tracked.conversation.is_streaming() or tracked.state in (
                MissionAgentState.THINKING, MissionAgentState.WORKING, MissionAgentState.WAITING_FOR_APPROVAL
            ) or tracked.conversation.pending_tool_authorization() is not None
            if busy:
                raise MissionError("The conversation is busy. Wait for it to finish its reply, or stop it, then switch")

        mission.mode = MissionMode.AUTONOMOUS
        self._save(mission)
        if tracked is not None:
            self._registry.close(tracked.context_id)

        self._sync()
        self._load_headless(mission)
        self._record(MissionActivityKind.MODE_CHANGED, mission.title, MissionMode.AUTONOMOUS.value, mission_id)
        self._mindspace.add_interaction(
            MindspaceLogLevel.INFO, f"Human switched mission '{mission.title}' to autonomous mode"
        )
        self._consider_continuing(mission)
        self._notify()

    def stop_task(self, mission_id: str, task_id: str) -> None:
        """
        Stop one task of a mission.

        The task is marked stopped and stays stopped whatever the lead reports
        later.  The delegated conversation working on it, if it is open, is
        stopped, and the lead is told not to continue the task.  When the
        delegate cannot be found the activity feed says it may still be running.

        Args:
            mission_id: Mission the task belongs to
            task_id: Task to stop

        Raises:
            MissionError: If there is no such task
        """
        mission = self._missions.get(mission_id)
        task = next((t for t in mission.tasks if t.task_id == task_id), None) if mission is not None else None
        if mission is None or task is None:
            raise MissionError("There is no such task")

        if task.status == MissionTaskStatus.STOPPED:
            return

        task.status = MissionTaskStatus.STOPPED
        if task_id not in mission.stopped_tasks:
            mission.stopped_tasks.append(task_id)

        self._save(mission)

        agent = os.path.splitext(os.path.basename(task.agent.strip()))[0] if task.agent else ""
        stopped_delegate = not agent
        if agent:
            for context_id, tracked in self._tracked.items():
                if self._members.get(context_id) != mission_id or self._is_lead(mission, tracked):
                    continue

                if os.path.splitext(os.path.basename(tracked.transcript.path()))[0] == agent:
                    tracked.transcript.cancel_current_tasks()
                    stopped_delegate = True

        detail = task_id if stopped_delegate else f"{task_id} — its delegate could not be found, so it may still be running"
        self._record(MissionActivityKind.TASK_STOPPED, task.title, detail, mission_id)
        self._mindspace.add_interaction(
            MindspaceLogLevel.INFO, f"Human stopped task {task_id} '{task.title}' of mission '{mission.title}'"
        )

        lead = self._lead_for(mission)
        if lead is not None:
            message = (
                f"The user stopped task {task_id} (\"{task.title}\"). Do not do any more work on it and keep it "
                f"marked `stopped` in the plan. Carry on with the remaining tasks."
            )
            self._run_task(lead.transcript.submit_message(MISSION_REQUESTER, message), mission_id)

        self._notify()

    def _run_task(self, coroutine: Any, mission_id: str | None = None) -> None:
        """
        Run a coroutine on the event loop, keeping a reference until it finishes.

        A failure is logged and, when the work belongs to a mission, recorded in
        the activity feed: a background task that raises would otherwise leave the
        mission in an unexplained state with nothing to show the user.

        Args:
            coroutine: The coroutine to run
            mission_id: Mission the work belongs to, if any
        """
        task = asyncio.get_running_loop().create_task(coroutine)
        self._continue_tasks.add(task)
        task.add_done_callback(lambda finished: self._task_finished(finished, mission_id))

    def _task_finished(self, task: asyncio.Task, mission_id: str | None) -> None:
        """Release a finished task and report a failure it did not handle itself."""
        self._continue_tasks.discard(task)
        if task.cancelled():
            return

        error = task.exception()
        if error is None:
            return

        self._logger.error("Mission background task failed: %s", error, exc_info=error)
        mission = self._missions.get(mission_id or "")
        if mission is not None:
            self._record(MissionActivityKind.ERROR, mission.title, str(error), mission_id)
            self._notify()

    def _is_lead(self, mission: Mission, tracked: _TrackedConversation) -> bool:
        """Return True if a tracked conversation is the mission's lead."""
        lead_path = os.path.normpath(self._mindspace.get_absolute_path(mission.lead_path))
        return os.path.normpath(tracked.transcript.path()) == lead_path

    def _title_for(self, context_id: str) -> str:
        """Return the current title of a context, or of an autonomous mission's lead conversation."""
        if context_id.startswith(_HEADLESS_PREFIX):
            tracked = self._tracked.get(context_id)
            return os.path.splitext(os.path.basename(tracked.transcript.path()))[0] if tracked is not None else ""

        info = self._registry.get(context_id)
        return info.title if info is not None else ""

    def _status_for(self, mission: Mission, decisions: list[MissionDecision] | None = None) -> MissionStatus:
        """
        Derive a mission's overall status from its plan and conversations.

        Args:
            mission: Mission to derive the status of
            decisions: Decisions already computed for this operation, or None to compute them
        """
        if mission.stopped:
            return MissionStatus.STOPPED

        if mission.mission_id in self._interrupted:
            return MissionStatus.INTERRUPTED

        live = decisions if decisions is not None else self.decisions()
        if any(d.mission_id == mission.mission_id and not d.reviewing for d in live):
            return MissionStatus.NEEDS_YOU

        busy = any(
            tracked.state in (MissionAgentState.THINKING, MissionAgentState.WORKING)
            for tracked in self._tracked.values()
            if self._members.get(tracked.context_id) == mission.mission_id
        )
        if busy:
            return MissionStatus.RUNNING if mission.tasks else MissionStatus.PLANNING

        if mission.is_complete():
            return MissionStatus.COMPLETED

        return MissionStatus.IDLE

    def _record(self, kind: MissionActivityKind, subject: str, detail: str, mission_id: str | None) -> None:
        """Append an entry to the activity feed."""
        self._activity.append(MissionActivity(
            kind=kind,
            subject=subject,
            detail=detail,
            mission_id=mission_id,
            occurred_at=datetime.now(timezone.utc),
        ))
