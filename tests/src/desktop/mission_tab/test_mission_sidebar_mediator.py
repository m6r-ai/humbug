"""Tests for the mediator between the Missions panel and the mission service."""

import asyncio
from dataclasses import dataclass

import pytest
from PySide6.QtWidgets import QWidget

from desktop.message_box import MessageBoxButton
from desktop.mission_sidebar.mission_sidebar import MissionSidebar, MissionSidebarEntry
from desktop.mission_tab import mission_actions as mission_actions_module
from desktop.mission_tab.mission_sidebar_mediator import MissionSidebarMediator
from desktop.mission_tab.mission_tab import mission_status_text
from mission import MissionError, MissionMode, MissionStatus


@dataclass
class _FakeMission:
    """The parts of a mission the mediator reads."""
    mission_id: str
    title: str
    archived: bool = False
    mode: MissionMode = MissionMode.AUTONOMOUS
    objective: str = ""


@dataclass
class _FakeDecision:
    """A request waiting for the user."""
    mission_id: str | None


class _FakeService:
    """A mission service holding a fixed list of missions, recording what is done to them."""

    def __init__(
        self,
        missions: list[_FakeMission],
        statuses: dict[str, MissionStatus] | None = None,
        decisions: list[_FakeDecision] | None = None,
    ) -> None:
        self._missions = missions
        self._statuses = statuses or {}
        self._decisions = decisions or []
        self.listeners: list = []
        self.done: list[tuple] = []

    def missions(self) -> list[_FakeMission]:
        """Return the missions, newest first."""
        return self._missions

    def status(self, mission_id: str) -> MissionStatus | None:
        """Return a mission's status."""
        return self._statuses.get(mission_id)

    def decisions(self) -> list[_FakeDecision]:
        """Return the requests waiting for the user."""
        return self._decisions

    def lead_conversation_path(self, mission_id: str) -> str:
        """Return where a mission's lead conversation is."""
        return f"/conversations/{mission_id}.conv"

    def add_listener(self, listener) -> None:
        """Start telling a listener about changes."""
        self.listeners.append(listener)

    def remove_listener(self, listener) -> None:
        """Stop telling a listener about changes."""
        self.listeners.remove(listener)

    def rename_mission(self, mission_id: str, title: str) -> None:
        """Record a rename."""
        self.done.append(("rename", mission_id, title))

    def set_archived(self, mission_id: str, archived: bool) -> None:
        """Record an archive, refusing an unknown mission."""
        if mission_id not in {m.mission_id for m in self._missions}:
            raise MissionError("No such mission")

        self.done.append(("archive", mission_id, archived))

    def remove_mission(self, mission_id: str) -> None:
        """Record a deletion."""
        self.done.append(("delete", mission_id))

    async def run_again(self, mission_id: str) -> _FakeMission:
        """Start a new mission from an earlier one, refusing an unknown mission."""
        if mission_id not in {m.mission_id for m in self._missions}:
            raise MissionError("No such mission")

        self.done.append(("rerun", mission_id))
        return _FakeMission("again", "Again")


@pytest.fixture
def mediated(qapp):  # pylint: disable=unused-argument
    """A mediator for a Missions panel, recording what it lists and the badge it shows."""
    window = QWidget()
    panel = MissionSidebar()
    shown: list[list[MissionSidebarEntry]] = []
    badges: list[str] = []
    mediator = MissionSidebarMediator(window, panel, shown.append, badges.append, lambda _mission_id: None)
    yield mediator, panel, shown, badges
    panel.deleteLater()
    window.deleteLater()


class TestListing:
    """The mediator lists a service's missions in the panel."""

    def test_missions_are_listed_with_their_status_and_lead(self, mediated):
        """Each mission is listed with its title, status and lead conversation, archived ones last."""
        mediator, _panel, shown, _badges = mediated
        service = _FakeService(
            [_FakeMission("old", "Old", archived=True), _FakeMission("new", "New", objective="Write docs")],
            {"new": MissionStatus.RUNNING},
        )
        mediator.follow(service)  # type: ignore[arg-type]

        rows = shown[-1]
        assert [row.mission_id for row in rows] == ["new", "old"]
        assert rows[0].title == "New"
        assert rows[0].status_text == mission_status_text(MissionStatus.RUNNING)
        assert rows[0].lead_path == "/conversations/new.conv"
        assert "Write docs" in rows[0].search_text
        assert rows[1].status_text == mission_status_text(MissionStatus.IDLE)
        assert rows[1].archived

    def test_a_classic_mission_says_so(self, mediated):
        """A mission the user drives in its own tab is marked as Classic."""
        mediator, panel, shown, _badges = mediated
        mediator.follow(_FakeService([_FakeMission("m", "Guide", mode=MissionMode.CLASSIC)]))  # type: ignore[arg-type]
        classic = panel._language_manager.strings().mission_mode_classic  # pylint: disable=protected-access
        assert shown[-1][0].title == f"Guide  ·  {classic}"

    def test_waiting_requests_are_counted_per_mission_and_in_the_badge(self, mediated):
        """Each mission counts its own waiting requests, and the badge counts them all."""
        mediator, _panel, shown, badges = mediated
        service = _FakeService(
            [_FakeMission("a", "A"), _FakeMission("b", "B")],
            decisions=[_FakeDecision("a"), _FakeDecision("a"), _FakeDecision("b"), _FakeDecision(None)],
        )
        mediator.follow(service)  # type: ignore[arg-type]
        assert [row.waiting for row in shown[-1]] == [2, 1]
        assert badges[-1] == "3"

    def test_no_waiting_requests_means_no_badge(self, mediated):
        """With nothing waiting, the badge is put away."""
        mediator, _panel, _shown, badges = mediated
        mediator.follow(_FakeService([_FakeMission("a", "A")]))  # type: ignore[arg-type]
        assert badges[-1] == ""

    def test_no_service_lists_nothing(self, mediated):
        """Once there is no service to follow, nothing is listed and the service is let go."""
        mediator, _panel, shown, badges = mediated
        service = _FakeService([_FakeMission("m", "M")], decisions=[_FakeDecision("m")])
        mediator.follow(service)  # type: ignore[arg-type]
        assert service.listeners

        mediator.follow(None)
        assert shown[-1] == [] and badges[-1] == ""
        assert not service.listeners


class TestActions:
    """What the panel asks to be done to a mission is done through the service."""

    def test_rename_asks_for_a_name(self, mediated, monkeypatch):
        """Renaming asks for a new name and keeps it; cancelling changes nothing."""
        mediator, panel, _shown, _badges = mediated
        service = _FakeService([_FakeMission("m", "Old name")])
        mediator.follow(service)  # type: ignore[arg-type]

        monkeypatch.setattr(mission_actions_module.QInputDialog, "getText", lambda *_a, **_k: ("", False))
        panel.mission_rename_requested.emit("m")
        assert not service.done

        monkeypatch.setattr(mission_actions_module.QInputDialog, "getText", lambda *_a, **_k: ("New name", True))
        panel.mission_rename_requested.emit("m")
        assert service.done == [("rename", "m", "New name")]

    def test_archive_and_bring_back(self, mediated):
        """Archiving and bringing back are passed on as asked."""
        mediator, panel, _shown, _badges = mediated
        service = _FakeService([_FakeMission("m", "M")])
        mediator.follow(service)  # type: ignore[arg-type]
        panel.mission_archive_requested.emit("m", True)
        panel.mission_archive_requested.emit("m", False)
        assert service.done == [("archive", "m", True), ("archive", "m", False)]

    def test_a_failed_archive_is_reported(self, mediated, monkeypatch):
        """When the service refuses, the user is told."""
        mediator, panel, _shown, _badges = mediated
        mediator.follow(_FakeService([_FakeMission("m", "M")]))  # type: ignore[arg-type]
        shown_errors: list[str] = []
        monkeypatch.setattr(
            mission_actions_module.MessageBox, "show_message",
            lambda _parent, _kind, _title, message, *_a, **_k: shown_errors.append(message),
        )
        panel.mission_archive_requested.emit("missing", True)
        assert shown_errors == ["No such mission"]

    def test_delete_asks_first(self, mediated, monkeypatch):
        """Deleting asks for confirmation, and deletes only when the user agrees."""
        mediator, panel, _shown, _badges = mediated
        service = _FakeService([_FakeMission("m", "M")])
        mediator.follow(service)  # type: ignore[arg-type]

        monkeypatch.setattr(mission_actions_module.MessageBox, "show_message", lambda *_a, **_k: MessageBoxButton.NO)
        panel.mission_delete_requested.emit("m")
        assert not service.done

        monkeypatch.setattr(mission_actions_module.MessageBox, "show_message", lambda *_a, **_k: MessageBoxButton.YES)
        panel.mission_delete_requested.emit("m")
        assert service.done == [("delete", "m")]


class TestRunAgain:
    """Running a mission again from the panel opens the new mission in its own tab."""

    def test_the_new_mission_is_opened(self, qapp):  # pylint: disable=unused-argument
        """The new mission started from an earlier one is opened."""
        async def run() -> None:
            window = QWidget()
            panel = MissionSidebar()
            opened: list[str] = []
            mediator = MissionSidebarMediator(window, panel, lambda _rows: None, lambda _text: None, opened.append)
            service = _FakeService([_FakeMission("m", "M")])
            mediator.follow(service)  # type: ignore[arg-type]

            panel.mission_rerun_requested.emit("m")
            for _ in range(3):
                await asyncio.sleep(0)

            assert service.done == [("rerun", "m")]
            assert opened == ["again"]
            panel.deleteLater()
            window.deleteLater()

        asyncio.run(run())

    def test_a_mission_that_cannot_run_again_opens_nothing(self, qapp, monkeypatch):  # pylint: disable=unused-argument
        """When the service refuses, the user is told and no tab is opened."""
        async def run() -> None:
            window = QWidget()
            panel = MissionSidebar()
            opened: list[str] = []
            errors: list[str] = []
            monkeypatch.setattr(
                mission_actions_module.MessageBox, "show_message",
                lambda _parent, _kind, _title, message, *_a, **_k: errors.append(message),
            )
            mediator = MissionSidebarMediator(window, panel, lambda _rows: None, lambda _text: None, opened.append)
            mediator.follow(_FakeService([_FakeMission("m", "M")]))  # type: ignore[arg-type]

            panel.mission_rerun_requested.emit("missing")
            for _ in range(3):
                await asyncio.sleep(0)

            assert not opened and len(errors) == 1
            panel.deleteLater()
            window.deleteLater()

        asyncio.run(run())
