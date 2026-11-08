"""Mission Control tab."""

import asyncio
import dataclasses
from collections.abc import Callable, Coroutine
from datetime import datetime
import logging
import os
import zlib
from typing import Any

from PySide6.QtCore import QTimer
from PySide6.QtWidgets import QInputDialog, QVBoxLayout, QWidget

from desktop.language.language_manager import LanguageManager
from desktop.language.language_strings import LanguageStrings
from desktop.message_box import MessageBox, MessageBoxButton, MessageBoxType
from desktop.mindspace.mindspace_manager import MindspaceManager
from desktop.mission_tab.mission_display import (
    MissionActivityView, MissionAgentView, MissionChatView, MissionChipView, MissionDecisionView, MissionHeaderView,
    MissionPhaseView, MissionResultView, MissionStatTarget, MissionStatView, MissionStepState, MissionStepView,
    MissionTone, MissionView
)
from desktop.mission_tab.mission_service_holder import MissionServiceHolder
from desktop.mission_tab.mission_widget import MissionWidget
from desktop.status_message import StatusMessage
from desktop.style_manager import StyleManager
from desktop.tab import TabBase
from mission import (
    AutoApproveLevel, MissionActivity, MissionActivityKind, MissionAgent, MissionAgentState, MissionDecision,
    MissionError, MissionExchange, MissionMode, MissionPhase, MissionService, MissionSnapshot, MissionSpeaker,
    MissionStatus, MissionTask, MissionTaskStatus
)


_STATUS_TONES = {
    MissionStatus.PLANNING: MissionTone.INFO,
    MissionStatus.RUNNING: MissionTone.ACCENT,
    MissionStatus.NEEDS_YOU: MissionTone.WARNING,
    MissionStatus.IDLE: MissionTone.NEUTRAL,
    MissionStatus.COMPLETED: MissionTone.SUCCESS,
    MissionStatus.STOPPED: MissionTone.DANGER,
    MissionStatus.INTERRUPTED: MissionTone.WARNING,
    MissionStatus.REPLIED: MissionTone.SUCCESS,
}

_TASK_STATES = {
    MissionTaskStatus.PENDING: (MissionStepState.PENDING, MissionTone.NEUTRAL),
    MissionTaskStatus.IN_PROGRESS: (MissionStepState.ACTIVE, MissionTone.ACCENT),
    MissionTaskStatus.COMPLETED: (MissionStepState.DONE, MissionTone.SUCCESS),
    MissionTaskStatus.BLOCKED: (MissionStepState.BLOCKED, MissionTone.DANGER),
    MissionTaskStatus.STOPPED: (MissionStepState.STOPPED, MissionTone.NEUTRAL),
}

_ACTIVITY_STYLES = {
    MissionActivityKind.MISSION_STARTED: ("★", MissionTone.ACCENT),
    MissionActivityKind.PLAN_UPDATED: ("≡", MissionTone.ACCENT),
    MissionActivityKind.AGENT_JOINED: ("◆", MissionTone.INFO),
    MissionActivityKind.TURN_STARTED: ("▶", MissionTone.INFO),
    MissionActivityKind.TURN_FINISHED: ("✓", MissionTone.SUCCESS),
    MissionActivityKind.TOOL_USED: ("›", MissionTone.NEUTRAL),
    MissionActivityKind.APPROVAL_REQUESTED: ("!", MissionTone.WARNING),
    MissionActivityKind.APPROVED: ("✓", MissionTone.SUCCESS),
    MissionActivityKind.REJECTED: ("✕", MissionTone.DANGER),
    MissionActivityKind.GUIDANCE_SENT: ("✎", MissionTone.ACCENT),
    MissionActivityKind.USER_MESSAGE: ("✎", MissionTone.ACCENT),
    MissionActivityKind.MISSION_STOPPED: ("■", MissionTone.DANGER),
    MissionActivityKind.AUTO_CONTINUED: ("↻", MissionTone.ACCENT),
    MissionActivityKind.AUTO_PAUSED: ("‖", MissionTone.WARNING),
    MissionActivityKind.RESUMED: ("↻", MissionTone.SUCCESS),
    MissionActivityKind.TASK_STOPPED: ("■", MissionTone.DANGER),
    MissionActivityKind.MODE_CHANGED: ("⇄", MissionTone.INFO),
    MissionActivityKind.AUTO_APPROVED: ("✓", MissionTone.ACCENT),
    MissionActivityKind.REVERSIBLE_APPROVED: ("↺", MissionTone.SUCCESS),
    MissionActivityKind.REVIEW_DEFERRED: ("?", MissionTone.WARNING),
    MissionActivityKind.PLAN_ARCHIVED: ("▣", MissionTone.INFO),
    MissionActivityKind.ERROR: ("!", MissionTone.DANGER),
}

# Colour of the current stage of the journey, where it differs from the status colour.
_STAGE_TONES = {
    MissionStatus.PLANNING: MissionTone.ACCENT,
    MissionStatus.RUNNING: MissionTone.ACCENT,
    MissionStatus.IDLE: MissionTone.WARNING,
    MissionStatus.COMPLETED: MissionTone.SUCCESS,
    MissionStatus.REPLIED: MissionTone.SUCCESS,
}


def _stage(status: MissionStatus, has_plan: bool) -> int:
    """
    Return how far along its journey a mission is.

    The stages are: the user asked, the AI planned, the AI team worked, and the job is done.
    A question the lead answered needed no plan, so it is done as soon as it is answered.
    """
    if status in (MissionStatus.COMPLETED, MissionStatus.REPLIED):
        return 3

    if status == MissionStatus.PLANNING or not has_plan:
        return 1

    return 2


# Friendly names given to a mission's agents, so they read as a team rather than as file names.
_AGENT_NAMES = (
    "Atlas", "Nova", "Orion", "Lyra", "Vega", "Juno", "Sol", "Echo", "Iris", "Kai",
    "Rhea", "Zeno", "Mira", "Theo", "Cleo", "Ravi", "Luna", "Felix", "Ada", "Milo",
)

_AGENT_ACTIVITY = {
    MissionAgentState.THINKING: "thinking",
    MissionAgentState.WORKING: "working",
    MissionAgentState.WAITING_FOR_APPROVAL: "waiting",
    MissionAgentState.IDLE: "idle",
    MissionAgentState.ERROR: "error",
}


def agent_names(mission_id: str, agents: list[MissionAgent]) -> dict[str, str]:
    """
    Give each of a mission's agents a short, stable name.

    The lead's name comes from the mission and each delegate's from its
    conversation, so an agent keeps its name between refreshes and restarts.
    No two agents of a mission share a name.

    Args:
        mission_id: The mission
        agents: Its agents, lead first

    Returns:
        Names keyed by agent context
    """
    names: dict[str, str] = {}
    used: set[str] = set()
    for agent in agents:
        seed = mission_id if agent.is_lead else agent.title
        start = zlib.crc32(seed.encode("utf-8")) % len(_AGENT_NAMES)
        name = next(
            (candidate for step in range(len(_AGENT_NAMES))
             if (candidate := _AGENT_NAMES[(start + step) % len(_AGENT_NAMES)]) not in used),
            f"Agent {len(names) + 1}"
        )
        used.add(name)
        names[agent.context_id] = name

    return names


# Service changes arrive in bursts while conversations stream, so redraws are coalesced.
_REFRESH_DELAY_MS = 120


_SPEECH_REQUEST_LENGTH = 90


def _first_line(text: str) -> str:
    """Return the first non-empty line of text, shortened to fit a speech bubble."""
    line = next((part.strip() for part in text.splitlines() if part.strip()), "")
    return line if len(line) <= _SPEECH_REQUEST_LENGTH else f"{line[:_SPEECH_REQUEST_LENGTH - 1].rstrip()}…"


def _duration_text(seconds: float) -> str:
    """Format a length of time compactly, such as "45s", "2m 30s" or "1h 5m"."""
    total = max(0, round(seconds))
    hours, rest = divmod(total, 3600)
    minutes, secs = divmod(rest, 60)
    if hours:
        return f"{hours}h {minutes}m" if minutes else f"{hours}h"

    if minutes:
        return f"{minutes}m {secs}s" if secs else f"{minutes}m"

    return f"{secs}s"


def _local_time(moment: datetime, with_seconds: bool) -> str:
    """Format a timestamp in local time."""
    return moment.astimezone().strftime("%H:%M:%S" if with_seconds else "%H:%M")


class MissionTab(TabBase):
    """
    Tab hosting Mission Control: missions, their agents and the decision inbox.

    This tab mediates between the presentational MissionWidget and the backend
    MissionService, turning service snapshots into display records and the
    user's actions into service calls.  The service belongs to the mindspace,
    not the tab, so it keeps following missions while the tab is closed.
    """

    def __init__(self, tab_id: str, parent: QWidget | None = None, selected_mission_id: str = "") -> None:
        super().__init__(tab_id, parent)
        self._logger = logging.getLogger("MissionTab")
        self._style_manager = StyleManager()
        self._language_manager = LanguageManager()
        self._mindspace_manager = MindspaceManager()
        self._service: MissionService | None = None
        self._selected_mission_id: str | None = selected_mission_id or None
        self._starting_new = False
        self._tasks: set[asyncio.Task] = set()
        self._agent_display_names: dict[str, str] = {}

        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)
        self._widget = MissionWidget(self)
        layout.addWidget(self._widget)

        self._refresh_timer = QTimer(self)
        self._refresh_timer.setSingleShot(True)
        self._refresh_timer.setInterval(_REFRESH_DELAY_MS)
        self._refresh_timer.timeout.connect(self._refresh)

        self._widget.mission_selected.connect(self._on_mission_selected)
        self._widget.new_mission_requested.connect(self._on_new_mission_requested)
        self._widget.submit_requested.connect(self._on_submit_requested)
        self._widget.switch_to_classic_requested.connect(self._on_switch_to_classic_requested)
        self._widget.switch_to_autonomous_requested.connect(self._on_switch_to_autonomous_requested)
        self._widget.task_stop_requested.connect(self._on_task_stop_requested)
        self._widget.auto_approve_selected.connect(self._on_auto_approve_selected)
        self._widget.resume_requested.connect(self._on_resume_requested)
        self._widget.auto_resume_toggled.connect(self._on_auto_resume_toggled)
        self._widget.stop_requested.connect(self._on_stop_requested)
        self._widget.remove_requested.connect(self._on_remove_requested)
        self._widget.mission_rename_requested.connect(self._rename_mission)
        self._widget.mission_delete_requested.connect(self._delete_mission)
        self._widget.conversation_requested.connect(self._on_conversation_requested)
        self._widget.decision_approved.connect(lambda d: self._answer(lambda s: s.approve(d)))
        self._widget.wait_finish_requested.connect(self._on_wait_finish_requested)
        self._widget.decision_unsure.connect(lambda d: self._answer(lambda s: s.unsure(d)))
        self._widget.decision_rejected.connect(lambda d: self._answer(lambda s: s.reject(d)))

        self._style_manager.style_changed.connect(self.apply_style)
        self._language_manager.language_changed.connect(self._on_language_changed)

        # The tab is created while the context registry is announcing it, and starting the
        # service subscribes to that registry, so the service is attached once that is over.
        self._attach_timer = QTimer(self)
        self._attach_timer.setSingleShot(True)
        self._attach_timer.timeout.connect(self.attach_service)
        self._attach_timer.start(0)

        self.apply_style()

    def attach_service(self) -> None:
        """Start following the mindspace's missions, if this tab is not already."""
        if self._service is not None or not self._mindspace_manager.has_mindspace():
            return

        self._service = MissionServiceHolder().service_for(self._mindspace_manager.mindspace())
        self._service.add_listener(self._on_service_changed)
        self._refresh()

    def _on_service_changed(self) -> None:
        """Schedule a redraw after the service reports a change."""
        try:
            self._refresh_timer.start()

        except RuntimeError:
            # This tab's widgets have been destroyed (for example, it was rebuilt in
            # another column), so it no longer needs to hear about changes.
            if self._service is not None:
                self._service.remove_listener(self._on_service_changed)

    def _on_language_changed(self) -> None:
        """Apply a new language."""
        self._widget.retranslate()
        self._refresh()

    def _strings(self) -> LanguageStrings:
        """Return the strings for the current language."""
        return self._language_manager.strings()

    def _status_text(self, status: MissionStatus) -> str:
        """Return the label for a mission status."""
        strings = self._strings()
        return {
            MissionStatus.PLANNING: strings.mission_status_planning,
            MissionStatus.RUNNING: strings.mission_status_running,
            MissionStatus.NEEDS_YOU: strings.mission_status_needs_you,
            MissionStatus.IDLE: strings.mission_status_idle,
            MissionStatus.COMPLETED: strings.mission_status_completed,
            MissionStatus.STOPPED: strings.mission_status_stopped,
            MissionStatus.INTERRUPTED: strings.mission_status_interrupted,
            MissionStatus.REPLIED: strings.mission_status_replied,
        }[status]

    def _task_text(self, status: MissionTaskStatus) -> str:
        """Return the label for a task status."""
        strings = self._strings()
        return {
            MissionTaskStatus.PENDING: strings.mission_task_pending,
            MissionTaskStatus.IN_PROGRESS: strings.mission_task_in_progress,
            MissionTaskStatus.COMPLETED: strings.mission_task_completed,
            MissionTaskStatus.BLOCKED: strings.mission_task_blocked,
            MissionTaskStatus.STOPPED: strings.mission_task_stopped,
        }[status]

    def _agent_view(
        self,
        agent: MissionAgent,
        autonomous: bool,
        name: str,
        task_title: str,
        speech: str = "",
        decision: MissionDecision | None = None
    ) -> MissionAgentView:
        """Turn an agent into its display record, named and described by the task it is doing."""
        strings = self._strings()
        states = {
            MissionAgentState.THINKING: (strings.mission_agent_thinking, MissionTone.ACCENT, True),
            MissionAgentState.WORKING: (strings.mission_agent_working.format(agent.tool_name), MissionTone.INFO, True),
            MissionAgentState.WAITING_FOR_APPROVAL: (strings.mission_agent_waiting, MissionTone.WARNING, False),
            MissionAgentState.IDLE: (strings.mission_agent_idle, MissionTone.NEUTRAL, False),
            MissionAgentState.ERROR: (strings.mission_agent_error, MissionTone.DANGER, False),
        }
        text, tone, busy = states[agent.state]
        return MissionAgentView(
            context_id=agent.context_id,
            title=agent.title,
            role_text=(
                strings.mission_agent_lead if agent.is_lead else task_title or strings.mission_agent_delegate
            ),
            is_lead=agent.is_lead,
            state_text=text,
            tone=tone,
            busy=busy,
            # An autonomous mission's lead has no tab to bring forward.
            clickable=not (agent.is_lead and autonomous),
            name=name,
            parent_id=agent.parent_id,
            activity=_AGENT_ACTIVITY[agent.state],
            speech=speech,
            decision_id=decision.decision_id if decision is not None else "",
            decision_destructive=decision.destructive if decision is not None else False,
            wait_seconds=agent.wait_seconds,
            wait_started=agent.wait_started_at.timestamp() if agent.wait_started_at is not None else 0.0,
            model_text=agent.model,
        )

    def _tool_speech(self, tool_name: str, target: str, done: bool = False) -> str:
        """
        Say, in the agent's words, what it is doing with a tool, or what it has just done with one.

        done selects the words for a tool the agent has finished with and is now thinking about.
        """
        strings = self._strings()
        say = {
            "reading": strings.mission_say_after_reading if done else strings.mission_say_reading,
            "writing": strings.mission_say_after_writing if done else strings.mission_say_writing,
            "deleting": strings.mission_say_after_deleting if done else strings.mission_say_deleting,
            "files": strings.mission_say_after_files if done else strings.mission_say_files,
            "editing": strings.mission_say_after_editing if done else strings.mission_say_editing,
            "delegating": strings.mission_say_after_delegating if done else strings.mission_say_delegating,
            "terminal": strings.mission_say_after_terminal if done else strings.mission_say_terminal,
            "web": strings.mission_say_after_web if done else strings.mission_say_web,
            "tool": strings.mission_say_after_tool if done else strings.mission_say_tool,
        }
        tool, _dot, operation = tool_name.partition(".")
        # A file path reads better as just the file's name.
        short = (os.path.basename(target.rstrip("/\\")) or target) if tool in ("filesystem", "editor") else target
        if tool == "filesystem":
            if operation.startswith(("read", "list", "search", "get", "find")):
                return say["reading"].format(short) if short else say["files"]

            if operation.startswith(("write", "append", "apply", "create")):
                return say["writing"].format(short) if short else say["files"]

            if operation.startswith(("delete", "remove")):
                return say["deleting"].format(short) if short else say["files"]

            return say["files"]

        if tool == "editor":
            return say["editing"].format(short) if short else say["tool"].format(tool_name)

        if tool == "delegate_ai":
            return say["delegating"]

        if tool == "terminal":
            return say["terminal"].format(target) if target else say["tool"].format(tool_name)

        if tool in ("http", "preview"):
            return say["web"]

        return say["tool"].format(tool_name)

    def _progress_speech(self, agent: MissionAgent) -> str:
        """Say what the agent has just done and how far it has got, while it works out its next step."""
        strings = self._strings()
        latest = self._tool_speech(agent.last_tool_name, agent.last_tool_target, done=True)
        if agent.actions == 1:
            return strings.mission_say_progress_one.format(latest)

        return strings.mission_say_progress.format(latest, agent.actions)

    def _agent_speech(
        self,
        agent: MissionAgent,
        name: str,
        task_title: str,
        snapshot: MissionSnapshot,
        decision: MissionDecision | None,
        helpers: list[str]
    ) -> str:
        """
        Say, in the agent's own words, what it is doing for the user.

        helpers names the agents this one delegated to that are still at work.
        """
        strings = self._strings()
        mission = snapshot.mission
        if agent.wait_started_at is not None:
            return strings.mission_say_sleeping.format(_duration_text(agent.wait_seconds))

        if agent.state == MissionAgentState.WAITING_FOR_APPROVAL:
            # The bubble says what is wanted in a line; the Decision Inbox shows the full request.
            request = _first_line(decision.reason or decision.tool_name) if decision is not None else ""
            if decision is not None and decision.reviewing:
                return strings.mission_say_reviewing.format(request)

            return strings.mission_say_approval.format(request) if request else strings.mission_agent_waiting

        if agent.state == MissionAgentState.WORKING:
            if agent.tool_name.startswith("delegate_ai") and helpers:
                # A delegation lasts until the helper reports back, so say who it is waiting for.
                return strings.mission_say_waiting_helpers.format(", ".join(helpers))

            return self._tool_speech(agent.tool_name, agent.tool_target)

        if agent.state == MissionAgentState.ERROR:
            return strings.mission_say_error

        if agent.state == MissionAgentState.THINKING:
            # Once the agent has done something, it says what that was and how far it has got,
            # so its introduction is only heard before its first step.
            if agent.actions and agent.last_tool_name:
                return self._progress_speech(agent)

            if not agent.is_lead:
                return strings.mission_say_task.format(task_title) if task_title else strings.mission_say_hello.format(name)

            current = next((t.title for t in mission.tasks if t.status == MissionTaskStatus.IN_PROGRESS), "")
            if current:
                return strings.mission_say_task.format(current)

            return strings.mission_say_thinking if mission.tasks else strings.mission_say_planning.format(name)

        # An idle helper has finished its part and has nothing to say.
        if not agent.is_lead:
            return ""

        idle_speech = {
            MissionStatus.COMPLETED: strings.mission_say_done,
            MissionStatus.STOPPED: strings.mission_say_stopped,
            MissionStatus.INTERRUPTED: strings.mission_say_interrupted,
            MissionStatus.NEEDS_YOU: strings.mission_say_helper_waiting,
            MissionStatus.REPLIED: strings.mission_say_replied,
        }
        if snapshot.status in idle_speech:
            return idle_speech[snapshot.status]

        return strings.mission_say_reply if snapshot.lead_message else strings.mission_say_ready

    def _activity_view(self, entry: MissionActivity) -> MissionActivityView:
        """Turn an activity entry into its display record, calling agents by their names."""
        strings = self._strings()
        subject = self._agent_display_names.get(entry.subject, entry.subject)
        texts = {
            MissionActivityKind.MISSION_STARTED: (
                strings.mission_activity_started_prompt.format(entry.detail) if entry.detail
                else strings.mission_activity_started
            ),
            MissionActivityKind.PLAN_UPDATED: strings.mission_activity_plan_updated,
            MissionActivityKind.AGENT_JOINED: strings.mission_activity_agent_joined.format(subject),
            MissionActivityKind.TURN_STARTED: strings.mission_activity_turn_started.format(subject),
            MissionActivityKind.TURN_FINISHED: strings.mission_activity_turn_finished.format(subject),
            MissionActivityKind.TOOL_USED: strings.mission_activity_tool_used.format(subject, entry.detail),
            MissionActivityKind.APPROVAL_REQUESTED: strings.mission_activity_approval_requested.format(
                subject, entry.detail
            ),
            MissionActivityKind.APPROVED: strings.mission_activity_approved.format(entry.detail),
            MissionActivityKind.REJECTED: strings.mission_activity_rejected.format(entry.detail),
            MissionActivityKind.GUIDANCE_SENT: (
                strings.mission_activity_guidance_prompt.format(entry.detail) if entry.detail
                else strings.mission_activity_guidance
            ),
            MissionActivityKind.USER_MESSAGE: strings.mission_activity_user_message.format(subject, entry.detail),
            MissionActivityKind.MISSION_STOPPED: strings.mission_activity_stopped,
            MissionActivityKind.AUTO_CONTINUED: strings.mission_activity_auto_continued,
            MissionActivityKind.AUTO_PAUSED: strings.mission_activity_auto_paused.format(entry.detail),
            MissionActivityKind.RESUMED: strings.mission_activity_resumed,
            MissionActivityKind.TASK_STOPPED: strings.mission_activity_task_stopped.format(subject),
            MissionActivityKind.AUTO_APPROVED: strings.mission_activity_auto_approved.format(entry.detail),
            MissionActivityKind.REVERSIBLE_APPROVED: strings.mission_activity_reversible_approved.format(entry.detail),
            MissionActivityKind.REVIEW_DEFERRED: strings.mission_activity_review_deferred.format(entry.detail),
            MissionActivityKind.PLAN_ARCHIVED: strings.mission_activity_plan_archived,
            MissionActivityKind.MODE_CHANGED: (
                strings.mission_activity_mode_classic if entry.detail == MissionMode.CLASSIC.value
                else strings.mission_activity_mode_autonomous
            ),
            MissionActivityKind.ERROR: strings.mission_activity_error.format(subject),
        }
        glyph, tone = _ACTIVITY_STYLES[entry.kind]
        return MissionActivityView(
            glyph=glyph,
            text=texts[entry.kind],
            time_text=_local_time(entry.occurred_at, with_seconds=True),
            tone=tone,
            # Nothing to open when the line already shows everything.
            more=entry.more if entry.more and entry.more != entry.detail else "",
        )

    def _decision_view(self, decision: MissionDecision) -> MissionDecisionView:
        """Turn a decision into its display record."""
        return MissionDecisionView(
            decision_id=decision.decision_id,
            context_id=decision.context_id,
            tool_name=decision.tool_name,
            source_text=self._strings().mission_decision_from.format(
                self._agent_display_names.get(decision.conversation_title, decision.conversation_title)
            ),
            time_text=_local_time(decision.requested_at, with_seconds=False),
            reason=decision.reason,
            detail=decision.detail or "",
            destructive=decision.destructive,
            reviewing=decision.reviewing,
            review_note=decision.review_note,
            auto_note=self._auto_approve_note(decision),
        )

    def _auto_approve_note(self, decision: MissionDecision) -> str:
        """Explain why a request of a mission with auto-approve turned on still needs the user."""
        mission = next(
            (m for m in self._service.missions() if m.mission_id == decision.mission_id), None
        ) if self._service is not None else None
        if mission is None or mission.auto_approve == AutoApproveLevel.OFF:
            return ""

        strings = self._strings()
        if mission.mode == MissionMode.CLASSIC:
            return strings.mission_auto_approve_note_classic

        if mission.auto_approve == AutoApproveLevel.SAFE and decision.destructive:
            return strings.mission_auto_approve_note_destructive

        return ""

    def _mission_view(self, snapshot: MissionSnapshot) -> MissionView:
        """Turn a mission snapshot into its display record."""
        strings = self._strings()
        mission = snapshot.mission
        classic = mission.mode == MissionMode.CLASSIC
        tone = MissionTone.INFO if classic else _STATUS_TONES[snapshot.status]
        percent = round(mission.progress() * 100)
        in_progress = sum(1 for t in mission.tasks if t.status == MissionTaskStatus.IN_PROGRESS)
        busy_agents = sum(1 for a in snapshot.agents if a.state in (MissionAgentState.THINKING, MissionAgentState.WORKING))

        header = MissionHeaderView(
            title=mission.title,
            summary=mission.summary or mission.objective,
            status_text=strings.mission_mode_classic if classic else self._status_text(snapshot.status),
            tone=tone,
            progress=mission.progress(),
            progress_text=strings.mission_progress.format(percent),
            can_stop=snapshot.status in (
                MissionStatus.PLANNING, MissionStatus.RUNNING, MissionStatus.NEEDS_YOU, MissionStatus.INTERRUPTED
            ),
            can_resume=snapshot.status == MissionStatus.INTERRUPTED and not classic,
            is_classic=classic,
            live=not classic and snapshot.status in (MissionStatus.PLANNING, MissionStatus.RUNNING),
            auto_approve=mission.auto_approve.value,
            stage=_stage(snapshot.status, bool(mission.tasks)),
            stage_tone=_STAGE_TONES.get(snapshot.status, tone),
        )
        stats = [
            MissionStatView("✓", str(mission.completed_count()), strings.mission_stat_completed, MissionTone.SUCCESS,
                            MissionStatTarget.DONE_STEPS),
            MissionStatView("▶", str(in_progress), strings.mission_stat_in_progress, MissionTone.INFO,
                            MissionStatTarget.ACTIVE_STEPS),
            MissionStatView("!", str(len(snapshot.decisions)), strings.mission_stat_approvals, MissionTone.WARNING,
                            MissionStatTarget.INBOX),
            MissionStatView("◆", str(busy_agents), strings.mission_stat_agents, MissionTone.ACCENT,
                            MissionStatTarget.AGENTS),
        ]
        open_agents = {agent.title: agent.context_id for agent in snapshot.agents}
        names = agent_names(mission.mission_id, snapshot.agents)
        names_by_title = {agent.title: names[agent.context_id] for agent in snapshot.agents}
        task_titles = {
            os.path.splitext(os.path.basename(task.agent.strip()))[0]: task.title
            for task in mission.tasks if task.agent.strip()
        }

        steps = self._step_views(mission.tasks, names_by_title, open_agents, can_stop=not classic)
        decisions_by_context = {decision.context_id: decision for decision in snapshot.decisions}
        history = [
            self._phase_view(phase, names_by_title, open_agents) for phase in reversed(mission.history)
        ]
        results = [
            self._result_view(number, phase)
            for number, phase in reversed(list(enumerate(mission.history, start=1)))
            if phase.request or phase.report
        ]

        agents = [
            self._agent_view(
                agent, not classic, names[agent.context_id], task_titles.get(agent.title, ""),
                self._agent_speech(
                    agent, names[agent.context_id], task_titles.get(agent.title, ""), snapshot,
                    decisions_by_context.get(agent.context_id),
                    [
                        names[helper.context_id] for helper in snapshot.agents
                        if helper.parent_id == agent.context_id and helper.state != MissionAgentState.IDLE
                    ]
                ),
                decisions_by_context.get(agent.context_id),
            )
            for agent in snapshot.agents
        ]

        # The header says what is happening now, in the lead's own words.
        lead_view = next((agent for agent in agents if agent.is_lead), None)
        if lead_view is not None and lead_view.speech:
            header = dataclasses.replace(
                header, now_text=f"{lead_view.name}: {lead_view.speech}",
                now_tone=(
                    MissionTone.SUCCESS if snapshot.status in (MissionStatus.COMPLETED, MissionStatus.REPLIED)
                    else MissionTone.ACCENT
                )
            )

        lead_name = lead_view.name if lead_view is not None else strings.mission_chat_lead
        conversation = [self._chat_view(entry, lead_name) for entry in snapshot.conversation]

        # Once the mission is done, the lead's report on it is shown as the final report.  A later reply
        # answers something the user asked after that, so it is shown on its own where they reply.
        completed = not classic and snapshot.status == MissionStatus.COMPLETED
        report_message = (mission.report or snapshot.lead_message) if completed else ""
        lead_message = "" if completed and snapshot.lead_message == report_message else snapshot.lead_message
        return MissionView(
            header=header,
            stats=stats,
            steps=steps,
            agents=agents,
            agents_badge=strings.mission_agents_active.format(busy_agents),
            activity=[self._activity_view(entry) for entry in snapshot.activity],
            lead_message=lead_message,
            lead_message_time=(
                _local_time(snapshot.lead_message_at, with_seconds=False) if snapshot.lead_message_at else ""
            ),
            awaiting_reply=bool(snapshot.lead_message) and snapshot.status == MissionStatus.IDLE,
            history=history,
            completed=completed,
            report_text=strings.mission_report_stats.format(
                mission.completed_count(), len(mission.tasks), len(snapshot.agents)
            ),
            results=results,
            report_message=report_message,
            conversation=conversation,
        )

    def _chat_view(self, entry: MissionExchange, lead_name: str) -> MissionChatView:
        """Turn an entry in the conversation with the lead into its display record."""
        strings = self._strings()
        kind, speaker = {
            MissionSpeaker.USER: ("user", strings.mission_chat_you),
            MissionSpeaker.LEAD: ("lead", lead_name),
            MissionSpeaker.TOOL: ("tool", ""),
        }[entry.speaker]
        time_text = _local_time(entry.at, with_seconds=False) if entry.at is not None else ""
        return MissionChatView(kind=kind, speaker=speaker, text=entry.text, time_text=time_text)

    def _result_view(self, number: int, phase: MissionPhase) -> MissionResultView:
        """Turn a finished round kept in a mission's history into its display record."""
        strings = self._strings()
        done = sum(1 for task in phase.tasks if task.status == MissionTaskStatus.COMPLETED)
        try:
            time_text = datetime.fromisoformat(phase.finished_at).astimezone().strftime("%Y-%m-%d %H:%M")

        except ValueError:
            time_text = ""

        heading = "  ·  ".join(part for part in (
            strings.mission_result_number.format(number), time_text,
            strings.mission_phase_count.format(done, len(phase.tasks)),
        ) if part)
        return MissionResultView(
            key=f"{number}:{phase.finished_at}",
            heading=heading,
            request_text=strings.mission_result_asked.format(_first_line(phase.request)) if phase.request else "",
            report=phase.report,
        )

    def _step_views(
        self,
        tasks: list[MissionTask],
        names_by_title: dict[str, str],
        open_agents: dict[str, str],
        can_stop: bool
    ) -> list[MissionStepView]:
        """Turn a plan's tasks into step display records."""
        steps: list[MissionStepView] = []
        for index, task in enumerate(tasks, start=1):
            state, step_tone = _TASK_STATES[task.status]
            # The lead names a delegate by its session ID, a transcript path; tabs are titled by file name.
            agent_title = os.path.splitext(os.path.basename(task.agent.strip()))[0]
            steps.append(MissionStepView(
                task_id=task.task_id,
                number=str(index),
                title=task.title,
                detail=task.detail,
                state=state,
                status_text=self._task_text(task.status),
                tone=step_tone,
                progress=task.progress,
                evidence=task.evidence,
                agent_title=names_by_title.get(agent_title, agent_title),
                agent_context_id=open_agents.get(agent_title, ""),
                can_stop=can_stop and task.status not in (MissionTaskStatus.COMPLETED, MissionTaskStatus.STOPPED),
            ))

        return steps

    def _phase_view(
        self,
        phase: MissionPhase,
        names_by_title: dict[str, str],
        open_agents: dict[str, str]
    ) -> MissionPhaseView:
        """Turn a finished plan from a mission's history into its display record."""
        done = sum(1 for task in phase.tasks if task.status == MissionTaskStatus.COMPLETED)
        try:
            time_text = datetime.fromisoformat(phase.finished_at).astimezone().strftime("%Y-%m-%d %H:%M")

        except ValueError:
            time_text = ""

        return MissionPhaseView(
            title=phase.title,
            summary=phase.summary,
            time_text=time_text,
            count_text=self._strings().mission_phase_count.format(done, len(phase.tasks)),
            steps=self._step_views(phase.tasks, names_by_title, open_agents, can_stop=False),
        )

    def _refresh(self) -> None:
        """Redraw everything from the service's current state."""
        service = self._service
        if service is None:
            self._widget.set_missions([], True)
            self._widget.set_mission(None)
            self._widget.set_decisions([])
            self._widget.set_waiting_elsewhere("", "")
            return

        missions = service.missions()
        self._agent_display_names = {}
        for listed in missions:
            listed_snapshot = service.snapshot(listed.mission_id)
            if listed_snapshot is not None:
                listed_names = agent_names(listed.mission_id, listed_snapshot.agents)
                for agent in listed_snapshot.agents:
                    self._agent_display_names[agent.title] = listed_names[agent.context_id]

        known = {mission.mission_id for mission in missions}
        if self._selected_mission_id not in known:
            self._selected_mission_id = None

        if self._selected_mission_id is None and not self._starting_new and missions:
            self._selected_mission_id = missions[0].mission_id

        decisions = service.decisions()
        waiting: dict[str, int] = {}
        for decision in decisions:
            if decision.mission_id is not None:
                waiting[decision.mission_id] = waiting.get(decision.mission_id, 0) + 1

        chips = []
        for mission in missions:
            status = service.status(mission.mission_id) or MissionStatus.IDLE
            chips.append(MissionChipView(
                mission_id=mission.mission_id,
                title=(
                    f"{mission.title}  ·  {self._strings().mission_mode_classic}"
                    if mission.mode == MissionMode.CLASSIC else mission.title
                ),
                status_text=self._status_text(status),
                tone=_STATUS_TONES[status],
                selected=mission.mission_id == self._selected_mission_id,
                waiting_text=str(waiting[mission.mission_id]) if mission.mission_id in waiting else "",
            ))

        self._widget.set_missions(chips, self._selected_mission_id is None)
        self._widget.set_auto_resume(service.resume_automatically())

        snapshot = service.snapshot(self._selected_mission_id) if self._selected_mission_id else None
        self._widget.set_mission(self._mission_view(snapshot) if snapshot is not None else None)
        # Each mission's inbox holds its own requests, and those from conversations outside any
        # mission; requests from other missions are pointed out instead.
        self._widget.set_decisions([
            self._decision_view(d) for d in decisions if d.mission_id in (self._selected_mission_id, None)
        ])
        elsewhere = [m for m in missions if m.mission_id in waiting and m.mission_id != self._selected_mission_id]
        if elsewhere:
            text = (
                self._strings().mission_waiting_elsewhere.format(elsewhere[0].title) if len(elsewhere) == 1
                else self._strings().mission_waiting_elsewhere_many.format(len(elsewhere))
            )
            self._widget.set_waiting_elsewhere(elsewhere[0].mission_id, text)

        else:
            self._widget.set_waiting_elsewhere("", "")

    def _run(self, coroutine: Coroutine[Any, Any, None]) -> None:
        """Run a coroutine on the event loop, keeping a reference until it finishes."""
        task = asyncio.get_event_loop().create_task(coroutine)
        self._tasks.add(task)
        task.add_done_callback(self._tasks.discard)

    def _show_error(self, message: str) -> None:
        """Tell the user an action failed."""
        try:
            MessageBox.show_message(self, MessageBoxType.CRITICAL, self._strings().mission_control, message)

        except RuntimeError:
            self._logger.warning("Mission Control error after its tab closed: %s", message)

    def _on_conversation_requested(self, context_id: str) -> None:
        """Show a conversation taking part in a mission beside Mission Control, which stays in view."""
        if self._service is not None:
            self._service.show_beside(context_id, self._tab_id)

    def _on_wait_finish_requested(self, context_id: str) -> None:
        """Cut short a wait an agent is in, so it carries on now."""
        service = self._service
        if service is None:
            return

        async def finish() -> None:
            await service.finish_wait(context_id)

        self._run(finish())

    def _on_mission_selected(self, mission_id: str) -> None:
        """Show a mission."""
        self._selected_mission_id = mission_id
        self._starting_new = False
        self._refresh()

    def _on_new_mission_requested(self) -> None:
        """Show the new-mission welcome."""
        self._selected_mission_id = None
        self._starting_new = True
        self._refresh()
        self._widget.focus_composer()

    def _on_submit_requested(self, text: str) -> None:
        """Start a mission, or guide the selected one."""
        if self._service is None:
            return

        if self._selected_mission_id is None:
            self._run(self._create_mission(self._service, text))
            return

        self._run(self._send_guidance(self._service, self._selected_mission_id, text))

    async def _create_mission(self, service: MissionService, objective: str) -> None:
        """Start a mission and show it."""
        try:
            mission = await service.create_mission(objective)

        except MissionError as e:
            self._show_error(self._strings().mission_error_start.format(str(e)))
            return

        self._selected_mission_id = mission.mission_id
        self._starting_new = False
        self._on_service_changed()

    async def _send_guidance(self, service: MissionService, mission_id: str, text: str) -> None:
        """Send the user's message to a mission's lead."""
        try:
            await service.send_guidance(mission_id, text, requester_id=self._tab_id)

        except MissionError as e:
            self._show_error(self._strings().mission_error_send.format(str(e)))

    def _answer(self, decide: Callable[[MissionService], Coroutine[Any, Any, bool]]) -> None:
        """Answer a decision and say so if it had already been answered."""
        service = self._service
        if service is None:
            return

        async def answer() -> None:
            if not await decide(service):
                try:
                    self.status_message.emit(StatusMessage(self._strings().mission_decision_stale, 4000))

                except RuntimeError:
                    pass

        self._run(answer())

    def _on_switch_to_classic_requested(self) -> None:
        """Show the selected mission's conversation as a tab, moving the mission to classic mode."""
        if self._service is not None and self._selected_mission_id is not None:
            self._service.open_lead(self._selected_mission_id, requester_id=self._tab_id)

    def _on_switch_to_autonomous_requested(self) -> None:
        """Hand the selected mission back to Mission Control."""
        if self._service is None or self._selected_mission_id is None:
            return

        try:
            self._service.switch_to_autonomous(self._selected_mission_id)

        except MissionError as e:
            self._show_error(self._strings().mission_error_switch.format(str(e)))

    def _on_auto_approve_selected(self, value: str) -> None:
        """Change the selected mission's auto-approve level, confirming before approving everything."""
        level = next((lvl for lvl in AutoApproveLevel if lvl.value == value), None)
        if self._service is None or self._selected_mission_id is None or level is None:
            self._refresh()
            return

        if level == AutoApproveLevel.ALL:
            strings = self._strings()
            answer = MessageBox.show_message(
                self,
                MessageBoxType.WARNING,
                strings.mission_auto_approve_all_title,
                strings.mission_auto_approve_all_confirm,
                [MessageBoxButton.YES, MessageBoxButton.NO],
                destructive=True
            )
            if answer != MessageBoxButton.YES:
                self._refresh()
                return

        try:
            self._service.set_auto_approve(self._selected_mission_id, level)

        except MissionError as e:
            self._show_error(str(e))

        self._refresh()

    def _on_task_stop_requested(self, task_id: str) -> None:
        """Stop one task of the selected mission."""
        if self._service is None or self._selected_mission_id is None:
            return

        try:
            self._service.stop_task(self._selected_mission_id, task_id)

        except MissionError as e:
            self._show_error(self._strings().mission_error_stop_task.format(str(e)))

    def show_mission(self, mission_id: str) -> None:
        """
        Show a particular mission.

        Args:
            mission_id: Mission to show
        """
        self._selected_mission_id = mission_id
        self._starting_new = False
        self._refresh()

    def _on_resume_requested(self) -> None:
        """Carry on with the selected mission after Humbug was closed during it."""
        if self._service is not None and self._selected_mission_id is not None:
            self._run(self._resume_mission(self._service, self._selected_mission_id))

    async def _resume_mission(self, service: MissionService, mission_id: str) -> None:
        """Resume a mission, telling the user if that fails."""
        try:
            await service.resume_mission(mission_id, requester_id=self._tab_id)

        except MissionError as e:
            self._show_error(self._strings().mission_error_resume.format(str(e)))

    def _on_auto_resume_toggled(self, enabled: bool) -> None:
        """Save whether interrupted missions are resumed automatically."""
        if self._service is None:
            return

        try:
            self._service.set_resume_automatically(enabled)

        except MissionError as e:
            self._show_error(str(e))
            self._widget.set_auto_resume(self._service.resume_automatically())

    def _on_stop_requested(self) -> None:
        """Stop the selected mission."""
        if self._service is not None and self._selected_mission_id is not None:
            self._service.stop_mission(self._selected_mission_id)

    def _on_remove_requested(self) -> None:
        """Delete the selected mission after confirmation."""
        if self._selected_mission_id is not None:
            self._delete_mission(self._selected_mission_id)

    def _delete_mission(self, mission_id: str) -> None:
        """Delete a mission from Mission Control after confirmation; its conversations are kept."""
        if self._service is None:
            return

        mission = next((m for m in self._service.missions() if m.mission_id == mission_id), None)
        if mission is None:
            return

        strings = self._strings()
        result = MessageBox.show_message(
            self,
            MessageBoxType.QUESTION,
            strings.mission_delete_title,
            strings.mission_delete_confirm.format(mission.title),
            [MessageBoxButton.YES, MessageBoxButton.NO],
            destructive=True
        )
        if result != MessageBoxButton.YES:
            return

        try:
            self._service.remove_mission(mission_id)

        except MissionError as e:
            self._show_error(str(e))

    def _rename_mission(self, mission_id: str) -> None:
        """Ask for a new name for a mission and apply it."""
        if self._service is None:
            return

        mission = next((m for m in self._service.missions() if m.mission_id == mission_id), None)
        if mission is None:
            return

        strings = self._strings()
        title, accepted = QInputDialog.getText(
            self, strings.mission_rename_title, strings.mission_rename_label, text=mission.title
        )
        if not accepted or not title.strip():
            return

        try:
            self._service.rename_mission(mission_id, title)

        except MissionError as e:
            self._show_error(self._strings().mission_error_rename.format(str(e)))

    def tool_name(self) -> str:
        return "mission"

    def tab_title_from_path(self) -> str:
        return self._strings().mission_control

    def on_path_renamed(self, new_path: str) -> None:
        pass

    def set_active(self, widget: QWidget, active: bool) -> None:
        if active:
            self.activated.emit()

    def activate(self) -> None:
        self._refresh()

    def set_path(self, path: str) -> None:
        pass

    def can_close_tab(self) -> bool:
        return True

    def close_tab(self) -> None:
        """Stop listening.  The service keeps following missions until the mindspace closes."""
        self._refresh_timer.stop()
        self._attach_timer.stop()
        if self._service is not None:
            self._service.remove_listener(self._on_service_changed)

    def can_save(self) -> bool:
        return False

    def save(self) -> bool:
        return True

    def can_save_as(self) -> bool:
        return False

    def save_as(self) -> bool:
        return True

    def can_undo(self) -> bool:
        return False

    def undo(self) -> None:
        pass

    def can_redo(self) -> bool:
        return False

    def redo(self) -> None:
        pass

    def can_cut(self) -> bool:
        return False

    def cut(self) -> None:
        pass

    def can_copy(self) -> bool:
        return False

    def copy(self) -> None:
        pass

    def can_paste(self) -> bool:
        return False

    def paste(self) -> None:
        pass

    def can_submit(self) -> bool:
        return self._widget.has_composer_text()

    def submit(self) -> None:
        self._widget.submit_composer()

    def show_find(self) -> None:
        pass

    def update_status(self) -> None:
        self.status_message.emit(StatusMessage(self._strings().mission_control))

    def apply_style(self) -> None:
        """Apply the current theme and zoom."""
        self._widget.apply_style()
        self._refresh()

    def capture_view_state(self) -> dict:
        """Capture which mission is shown, for session persistence."""
        return {"mission_id": self._selected_mission_id or ""}

    def restore_view_state(self, state: dict) -> None:
        """Show the mission that was shown when the session was saved."""
        mission_id = state.get("mission_id")
        if isinstance(mission_id, str) and mission_id:
            self._selected_mission_id = mission_id
            self._starting_new = False
            self._refresh()

    def capture_migration_state(self) -> dict:
        """Capture the state needed to rebuild this tab in another column."""
        state = self.capture_view_state()
        state["tab_id"] = self._tab_id
        return state

    @classmethod
    def rebuild_from_migration_state(cls, state: dict, parent: QWidget) -> "MissionTab":
        """Create a replacement tab carrying state from a column move."""
        return cls(state["tab_id"], parent, selected_mission_id=state.get("mission_id", ""))
