"""Connects the Missions panel to the mission service."""

import asyncio
from collections.abc import Callable

from PySide6.QtCore import QObject, QTimer
from PySide6.QtWidgets import QWidget

from desktop.language.language_manager import LanguageManager
from desktop.mission_sidebar.mission_sidebar import MissionSidebar, MissionSidebarEntry
from desktop.mission_tab.mission_actions import archive_mission, delete_mission, rename_mission, run_mission_again
from desktop.mission_tab.mission_painted import tone_role
from desktop.mission_tab.mission_tab import mission_status_text, mission_status_tone
from mission import MissionMode, MissionService, MissionStatus


# Changes come in bursts while a mission works, so they are looked at together.
_REFRESH_DELAY_MS = 250


class MissionSidebarMediator(QObject):
    """
    Lists a service's missions in the Missions panel, and does what the panel asks.

    Missions are listed newest first, with those the user has archived after the rest.
    How many requests are waiting for the user is counted for each mission, and in total
    for the badge on the panel's rail button.
    """

    def __init__(
        self,
        window: QWidget,
        panel: MissionSidebar,
        show: Callable[[list[MissionSidebarEntry]], None],
        set_badge: Callable[[str], None],
        open_mission: Callable[[str], None],
    ) -> None:
        """
        Initialise the mediator.

        Args:
            window: The main window, which questions about a mission are asked over
            panel: The Missions panel
            show: Lists missions in the sidebar
            set_badge: Shows a count of waiting requests on the panel's rail button, or nothing for ""
            open_mission: Opens a mission in its own tab
        """
        super().__init__(window)
        self._window = window
        self._show = show
        self._set_badge = set_badge
        self._open_mission = open_mission
        self._tasks: set[asyncio.Task] = set()
        self._service: MissionService | None = None
        self._refresh_timer = QTimer(self)
        self._refresh_timer.setSingleShot(True)
        self._refresh_timer.timeout.connect(self._refresh)
        LanguageManager().language_changed.connect(self._refresh)
        panel.mission_rename_requested.connect(self._on_rename_requested)
        panel.mission_archive_requested.connect(self._on_archive_requested)
        panel.mission_delete_requested.connect(self._on_delete_requested)
        panel.mission_rerun_requested.connect(self._on_rerun_requested)

    def follow(self, service: MissionService | None) -> None:
        """List a service's missions, or none when given None."""
        if service is self._service:
            return

        if self._service is not None:
            self._service.remove_listener(self._on_changed)

        self._service = service
        self._refresh_timer.stop()
        if service is not None:
            service.add_listener(self._on_changed)

        self._refresh()

    def _on_changed(self) -> None:
        """Refresh shortly, once the current burst of changes is over."""
        if not self._refresh_timer.isActive():
            self._refresh_timer.start(_REFRESH_DELAY_MS)

    def _refresh(self) -> None:
        """List the missions as they are now."""
        service = self._service
        if service is None:
            self._show([])
            self._set_badge("")
            return

        waiting: dict[str, int] = {}
        for decision in service.decisions():
            if decision.mission_id is not None:
                waiting[decision.mission_id] = waiting.get(decision.mission_id, 0) + 1

        classic = LanguageManager().strings().mission_mode_classic
        rows = []
        for mission in service.missions():
            status = service.status(mission.mission_id) or MissionStatus.IDLE
            rows.append(MissionSidebarEntry(
                mission_id=mission.mission_id,
                title=f"{mission.title}  ·  {classic}" if mission.mode == MissionMode.CLASSIC else mission.title,
                status_text=mission_status_text(status),
                status_role=tone_role(mission_status_tone(status)),
                lead_path=service.lead_conversation_path(mission.mission_id),
                archived=mission.archived,
                waiting=waiting.get(mission.mission_id, 0),
                search_text=f"{mission.title}\n{mission.objective}",
            ))

        rows.sort(key=lambda row: row.archived)
        self._show(rows)
        total = sum(waiting.values())
        self._set_badge(str(total) if total else "")

    def _on_rename_requested(self, mission_id: str) -> None:
        """Ask for a new name for a mission."""
        if self._service is not None:
            rename_mission(self._window, self._service, mission_id)

    def _on_archive_requested(self, mission_id: str, archived: bool) -> None:
        """Archive a mission, or bring it back."""
        if self._service is not None:
            archive_mission(self._window, self._service, mission_id, archived)

    def _on_delete_requested(self, mission_id: str) -> None:
        """Delete a mission once the user confirms."""
        if self._service is not None:
            delete_mission(self._window, self._service, mission_id)

    def _on_rerun_requested(self, mission_id: str) -> None:
        """Start a new mission from an earlier one, and open it."""
        service = self._service
        if service is None:
            return

        async def rerun() -> None:
            new_mission_id = await run_mission_again(self._window, service, mission_id)
            if new_mission_id:
                self._open_mission(new_mission_id)

        task = asyncio.get_event_loop().create_task(rerun())
        self._tasks.add(task)
        task.add_done_callback(self._tasks.discard)
