"""What has happened in Mission Control that the user should be told about, wherever they are."""

from dataclasses import dataclass
from enum import Enum, auto

from mission.mission_service import MissionService
from mission.mission_snapshot import MissionStatus


class MissionAlertKind(Enum):
    """Kinds of moment worth interrupting the user for."""
    DECISION = auto()
    COMPLETED = auto()
    REPLIED = auto()
    FAILED = auto()
    STUCK = auto()
    PLAN_READY = auto()
    OUT_OF_TOKENS = auto()


@dataclass(frozen=True)
class MissionAlert:
    """
    One moment worth telling the user about.

    Attributes:
        kind: What happened
        mission_id: The mission it happened in
        mission_title: The mission's name
        detail: What a decision asks for, or ""
    """
    kind: MissionAlertKind
    mission_id: str
    mission_title: str
    detail: str = ""


_STATUS_ALERTS = {
    MissionStatus.COMPLETED: MissionAlertKind.COMPLETED,
    MissionStatus.REPLIED: MissionAlertKind.REPLIED,
    MissionStatus.FAILED: MissionAlertKind.FAILED,
    MissionStatus.STUCK: MissionAlertKind.STUCK,
    MissionStatus.PLAN_READY: MissionAlertKind.PLAN_READY,
    MissionStatus.OUT_OF_TOKENS: MissionAlertKind.OUT_OF_TOKENS,
}


def _first_line(text: str) -> str:
    """Return the first non-blank line of a text."""
    return next((line.strip() for line in text.splitlines() if line.strip()), "")


class MissionAlertWatcher:
    """
    Finds what has newly happened in a service's missions that needs the user or that they waited for.

    That is a decision only the user can make, and a mission that finished, answered, failed for
    good or got stuck.  What was already so when watching began is not news, so the first look
    only takes note.  Requests from conversations outside any mission are left to their own tabs.
    """

    def __init__(self) -> None:
        """Start without having looked."""
        self._statuses: dict[str, MissionStatus] | None = None
        self._decisions: set[str] = set()

    def changes(self, service: MissionService) -> list[MissionAlert]:
        """
        Return what has happened since the last look.

        Args:
            service: The service whose missions are watched
        """
        missions = {mission.mission_id: mission for mission in service.missions()}
        statuses = {
            mission_id: status for mission_id in missions
            if (status := service.status(mission_id)) is not None
        }
        # A request an AI reviewer is still deciding on may never reach the user.
        decisions = [d for d in service.decisions() if not d.reviewing and d.mission_id in missions]
        previous = self._statuses
        seen = self._decisions
        self._statuses = statuses
        self._decisions = {d.decision_id for d in decisions}
        if previous is None:
            return []

        alerts = []
        for decision in decisions:
            if decision.decision_id not in seen and decision.mission_id is not None:
                alerts.append(MissionAlert(
                    MissionAlertKind.DECISION, decision.mission_id, missions[decision.mission_id].title,
                    _first_line(decision.reason or decision.tool_name)
                ))

        for mission_id, status in statuses.items():
            kind = _STATUS_ALERTS.get(status)
            if kind is not None and previous.get(mission_id) != status:
                alerts.append(MissionAlert(kind, mission_id, missions[mission_id].title))

        return alerts
