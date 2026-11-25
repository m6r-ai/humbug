"""Tests for finding what in Mission Control the user should be told about."""

from datetime import datetime, timezone

from mission.mission import Mission
from mission.mission_alerts import MissionAlert, MissionAlertKind, MissionAlertWatcher
from mission.mission_snapshot import MissionDecision, MissionStatus


class _Service:
    """Stands in for the service, with missions, statuses and decisions set by each test."""

    def __init__(self) -> None:
        self.mission_list: list[Mission] = []
        self.statuses: dict[str, MissionStatus] = {}
        self.decision_list: list[MissionDecision] = []

    def missions(self) -> list[Mission]:
        return self.mission_list

    def status(self, mission_id: str) -> MissionStatus | None:
        return self.statuses.get(mission_id)

    def decisions(self) -> list[MissionDecision]:
        return self.decision_list

    def add(self, mission_id: str, status: MissionStatus) -> None:
        self.mission_list.append(Mission(
            mission_id=mission_id, objective="Objective", title=f"Mission {mission_id}", lead_path="lead.conv",
            created_at="2026-11-25T00:00:00+00:00",
        ))
        self.statuses[mission_id] = status


def _decision(decision_id: str, mission_id: str | None, reason: str = "Run make\nmore", reviewing: bool = False) -> MissionDecision:
    return MissionDecision(
        decision_id=decision_id, context_id="ctx", conversation_title="Lead", mission_id=mission_id,
        tool_name="terminal", reason=reason, detail=None, destructive=False,
        requested_at=datetime.now(timezone.utc), reviewing=reviewing,
    )


def test_first_look_only_takes_note():
    """What was already so when watching began is not announced."""
    service = _Service()
    service.add("a", MissionStatus.COMPLETED)
    service.decision_list = [_decision("d1", "a")]

    assert MissionAlertWatcher().changes(service) == []  # type: ignore[arg-type]


def test_new_decision_is_announced_once():
    """A decision only the user can make is announced when it arrives, with the first line of what it asks."""
    service = _Service()
    service.add("a", MissionStatus.RUNNING)
    watcher = MissionAlertWatcher()
    watcher.changes(service)  # type: ignore[arg-type]

    service.decision_list = [_decision("d1", "a")]
    assert watcher.changes(service) == [  # type: ignore[arg-type]
        MissionAlert(MissionAlertKind.DECISION, "a", "Mission a", "Run make")
    ]
    assert watcher.changes(service) == []  # type: ignore[arg-type]


def test_decision_under_review_waits_until_it_reaches_the_user():
    """A request an AI reviewer is deciding on is announced only if the reviewer leaves it to the user."""
    service = _Service()
    service.add("a", MissionStatus.RUNNING)
    watcher = MissionAlertWatcher()
    watcher.changes(service)  # type: ignore[arg-type]

    service.decision_list = [_decision("d1", "a", reviewing=True)]
    assert watcher.changes(service) == []  # type: ignore[arg-type]

    service.decision_list = [_decision("d1", "a")]
    assert [alert.kind for alert in watcher.changes(service)] == [MissionAlertKind.DECISION]  # type: ignore[arg-type]


def test_request_outside_any_mission_is_not_announced():
    """A conversation that is not part of a mission asks in its own tab."""
    service = _Service()
    watcher = MissionAlertWatcher()
    watcher.changes(service)  # type: ignore[arg-type]

    service.decision_list = [_decision("d1", None)]
    assert watcher.changes(service) == []  # type: ignore[arg-type]


def test_mission_reaching_an_end_is_announced():
    """Finishing, answering, failing for good and getting stuck are each announced when they happen."""
    service = _Service()
    for mission_id in "abcd":
        service.add(mission_id, MissionStatus.RUNNING)

    watcher = MissionAlertWatcher()
    watcher.changes(service)  # type: ignore[arg-type]
    service.statuses.update({
        "a": MissionStatus.COMPLETED, "b": MissionStatus.REPLIED, "c": MissionStatus.FAILED, "d": MissionStatus.STUCK
    })

    assert {(alert.mission_id, alert.kind) for alert in watcher.changes(service)} == {  # type: ignore[arg-type]
        ("a", MissionAlertKind.COMPLETED), ("b", MissionAlertKind.REPLIED),
        ("c", MissionAlertKind.FAILED), ("d", MissionAlertKind.STUCK),
    }
    assert watcher.changes(service) == []  # type: ignore[arg-type]


def test_reconnecting_is_not_announced():
    """A mission that will retry by itself does not need the user yet."""
    service = _Service()
    service.add("a", MissionStatus.RUNNING)
    watcher = MissionAlertWatcher()
    watcher.changes(service)  # type: ignore[arg-type]

    service.statuses["a"] = MissionStatus.RECOVERING
    assert watcher.changes(service) == []  # type: ignore[arg-type]


def test_new_mission_finishing_is_announced():
    """A mission created after watching began is announced when it finishes."""
    service = _Service()
    watcher = MissionAlertWatcher()
    watcher.changes(service)  # type: ignore[arg-type]

    service.add("a", MissionStatus.PLANNING)
    assert watcher.changes(service) == []  # type: ignore[arg-type]
    service.statuses["a"] = MissionStatus.COMPLETED
    assert [alert.kind for alert in watcher.changes(service)] == [MissionAlertKind.COMPLETED]  # type: ignore[arg-type]


def test_plan_waiting_for_review_is_announced():
    """A plan the user asked to review is announced when it is ready."""
    service = _Service()
    service.add("a", MissionStatus.PLANNING)
    watcher = MissionAlertWatcher()
    watcher.changes(service)  # type: ignore[arg-type]

    service.statuses["a"] = MissionStatus.PLAN_READY
    assert [alert.kind for alert in watcher.changes(service)] == [MissionAlertKind.PLAN_READY]  # type: ignore[arg-type]
