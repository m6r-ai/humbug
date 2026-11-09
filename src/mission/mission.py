"""Persisted mission records."""

from dataclasses import dataclass, field
from enum import Enum
from typing import Any


class MissionTaskStatus(Enum):
    """Progress of one task in a mission plan."""
    PENDING = "pending"
    IN_PROGRESS = "in_progress"
    COMPLETED = "completed"
    BLOCKED = "blocked"
    STOPPED = "stopped"

    @classmethod
    def from_text(cls, text: Any) -> "MissionTaskStatus":
        """
        Interpret a status written by the mission lead.

        Unknown or missing values are treated as pending, so a loosely written
        plan still displays rather than being rejected.

        Args:
            text: Status value from the plan

        Returns:
            The matching status
        """
        if not isinstance(text, str):
            return cls.PENDING

        normalized = text.strip().lower().replace("-", "_").replace(" ", "_")
        aliases = {
            "done": cls.COMPLETED,
            "complete": cls.COMPLETED,
            "completed": cls.COMPLETED,
            "in_progress": cls.IN_PROGRESS,
            "running": cls.IN_PROGRESS,
            "active": cls.IN_PROGRESS,
            "blocked": cls.BLOCKED,
            "failed": cls.BLOCKED,
            "stopped": cls.STOPPED,
            "cancelled": cls.STOPPED,
            "canceled": cls.STOPPED,
            "skipped": cls.STOPPED,
        }
        return aliases.get(normalized, cls.PENDING)


class MissionMode(Enum):
    """
    Where a mission is followed.

    An autonomous mission is driven from Mission Control and its lead
    conversation has no tab; a classic mission is an ordinary conversation tab
    the user drives, and Mission Control only lists it.  A mission is always in
    exactly one of the two.
    """
    AUTONOMOUS = "autonomous"
    CLASSIC = "classic"


class AutoApproveLevel(Enum):
    """
    Which approval requests an AI reviewer may approve for a mission without the user.

    Whatever the level, a request the reviewer is unsure about is left for the user.
    """
    OFF = "off"
    SAFE = "safe"
    ALL = "all"


@dataclass
class MissionTask:
    """
    One step of a mission plan, as recorded by the mission lead.

    Attributes:
        task_id: Identifier the lead gave the step
        title: What the step is
        detail: What done looks like
        status: How far the step has got
        progress: The lead's latest note on the step
        evidence: What shows the step is done
        agent: Session of the delegated conversation working on the step, if any
    """
    task_id: str
    title: str
    detail: str = ""
    status: MissionTaskStatus = MissionTaskStatus.PENDING
    progress: str = ""
    evidence: str = ""
    agent: str = ""

    def to_dict(self) -> dict[str, Any]:
        """Return the task as a JSON-safe dictionary."""
        return {
            "id": self.task_id,
            "title": self.title,
            "detail": self.detail,
            "status": self.status.value,
            "progress": self.progress,
            "evidence": self.evidence,
            "agent": self.agent,
        }

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> "MissionTask":
        """
        Build a task from a dictionary.

        Args:
            data: Dictionary produced by to_dict, or a task from a plan block

        Returns:
            The task
        """
        return cls(
            task_id=str(data.get("id", "")),
            title=str(data.get("title", "")),
            detail=str(data.get("detail", "")),
            status=MissionTaskStatus.from_text(data.get("status")),
            progress=str(data.get("progress", "") or ""),
            evidence=str(data.get("evidence", "") or ""),
            agent=str(data.get("agent", "") or ""),
        )


@dataclass
class MissionPhase:
    """
    A finished plan, kept in a mission's history when new work starts in the same mission.

    Attributes:
        title: The mission's title while the plan ran
        summary: Its summary while the plan ran
        tasks: The plan's tasks as they finished
        finished_at: When the plan was moved to history, as an ISO timestamp
        request: What the user asked for that started the plan, or "" if it is not known
        report: The lead's final report on the plan, or "" if there was none
    """
    title: str
    summary: str
    tasks: list[MissionTask]
    finished_at: str
    request: str = ""
    report: str = ""

    def to_dict(self) -> dict[str, Any]:
        """Return the phase as a JSON-safe dictionary."""
        return {
            "title": self.title,
            "summary": self.summary,
            "tasks": [task.to_dict() for task in self.tasks],
            "finishedAt": self.finished_at,
            "request": self.request,
            "report": self.report,
        }

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> "MissionPhase":
        """
        Build a phase from a dictionary.

        Args:
            data: Dictionary produced by to_dict

        Returns:
            The phase
        """
        return cls(
            title=str(data.get("title", "")),
            summary=str(data.get("summary", "")),
            tasks=[MissionTask.from_dict(t) for t in data.get("tasks", []) if isinstance(t, dict)],
            finished_at=str(data.get("finishedAt", "")),
            request=str(data.get("request", "")),
            report=str(data.get("report", "")),
        )


@dataclass
class Mission:
    """
    A mission: an objective pursued by a lead conversation and its delegates.

    The record holds what the user asked for and the plan the lead last
    reported.  What actually happened lives in the conversation transcripts.
    `active` is set while the mission is working and cleared once it is
    waiting for the user, so a record still marked active when Humbug starts
    belongs to a mission that was interrupted.  `request` is what the user asked
    for that started the current plan: the objective, then each follow-up.
    Once the plan is finished, `follow_up` is the user's latest message and
    `report` the lead's report on the finished plan: the lead decides whether
    the message asks for new work, and only a new plan moves the finished one
    to `history`, taking them with it.
    """
    mission_id: str
    objective: str
    title: str
    lead_path: str
    created_at: str
    summary: str = ""
    tasks: list[MissionTask] = field(default_factory=list)
    stopped: bool = False
    active: bool = False
    mode: MissionMode = MissionMode.AUTONOMOUS
    stopped_tasks: list[str] = field(default_factory=list)
    auto_approve: AutoApproveLevel = AutoApproveLevel.OFF
    title_locked: bool = False
    history: list[MissionPhase] = field(default_factory=list)
    request: str = ""
    follow_up: str = ""
    report: str = ""

    def to_dict(self) -> dict[str, Any]:
        """Return the mission as a JSON-safe dictionary."""
        return {
            "version": 1,
            "id": self.mission_id,
            "objective": self.objective,
            "title": self.title,
            "leadPath": self.lead_path,
            "createdAt": self.created_at,
            "summary": self.summary,
            "tasks": [task.to_dict() for task in self.tasks],
            "stopped": self.stopped,
            "active": self.active,
            "mode": self.mode.value,
            "stoppedTasks": list(self.stopped_tasks),
            "autoApprove": self.auto_approve.value,
            "titleLocked": self.title_locked,
            "history": [phase.to_dict() for phase in self.history],
            "request": self.request,
            "followUp": self.follow_up,
            "report": self.report,
        }

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> "Mission":
        """
        Build a mission from a dictionary.

        Args:
            data: Dictionary produced by to_dict

        Returns:
            The mission

        Raises:
            ValueError: If required fields are missing
        """
        mission_id = data.get("id")
        lead_path = data.get("leadPath")
        if not isinstance(mission_id, str) or not mission_id or not isinstance(lead_path, str) or not lead_path:
            raise ValueError("Mission record is missing its id or lead conversation")

        tasks = data.get("tasks", [])
        return cls(
            mission_id=mission_id,
            objective=str(data.get("objective", "")),
            title=str(data.get("title", "")),
            lead_path=lead_path,
            created_at=str(data.get("createdAt", "")),
            summary=str(data.get("summary", "")),
            tasks=[MissionTask.from_dict(t) for t in tasks if isinstance(t, dict)],
            stopped=bool(data.get("stopped", False)),
            active=bool(data.get("active", False)),
            # Records written before missions had a mode were always followed through a tab.
            mode=MissionMode.AUTONOMOUS if data.get("mode") == "autonomous" else MissionMode.CLASSIC,
            stopped_tasks=[str(t) for t in data.get("stoppedTasks", []) if isinstance(t, (str, int))],
            auto_approve=next(
                (level for level in AutoApproveLevel if level.value == data.get("autoApprove")), AutoApproveLevel.OFF
            ),
            title_locked=bool(data.get("titleLocked", False)),
            history=[MissionPhase.from_dict(p) for p in data.get("history", []) if isinstance(p, dict)],
            request=str(data.get("request", "")),
            follow_up=str(data.get("followUp", "")),
            report=str(data.get("report", "")),
        )

    def completed_count(self) -> int:
        """Return how many tasks are completed."""
        return sum(1 for task in self.tasks if task.status == MissionTaskStatus.COMPLETED)

    def is_complete(self) -> bool:
        """Return True when there is a plan and every task in it is completed or stopped."""
        return bool(self.tasks) and all(
            task.status in (MissionTaskStatus.COMPLETED, MissionTaskStatus.STOPPED) for task in self.tasks
        )

    def progress(self) -> float:
        """
        Return progress through the plan between 0.0 and 1.0.

        A task in progress counts as half done so that progress moves while
        work is under way, not only when tasks finish.
        """
        if not self.tasks:
            return 0.0

        score = 0.0
        for task in self.tasks:
            if task.status in (MissionTaskStatus.COMPLETED, MissionTaskStatus.STOPPED):
                score += 1.0

            elif task.status == MissionTaskStatus.IN_PROGRESS:
                score += 0.5

        return score / len(self.tasks)
