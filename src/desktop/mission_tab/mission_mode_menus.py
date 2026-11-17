"""Menu entries and tab placement for mission conversations in classic and autonomous mode."""

from collections.abc import Callable

from PySide6.QtWidgets import QWidget

from context.context_info import ContextInfo
from desktop.language.language_manager import LanguageManager
from desktop.message_box import MessageBox, MessageBoxType
from desktop.mission_tab.mission_service_holder import MissionServiceHolder
from desktop.tab import TabBase
from mission import Mission, MissionError, MissionMode


MenuEntries = list[tuple[str, Callable[[], None]]]


class MissionModeMenus:
    """
    Supplies the "Switch to … mode" entries for mission conversations.

    A mission's conversation tab offers to switch the mission to autonomous
    mode, and the mission's conversation in the conversations sidebar offers to
    switch it to whichever mode it is not in.
    """

    def __init__(self, parent: QWidget, show_mission: Callable[[str], None]) -> None:
        """
        Initialise the menus.

        Args:
            parent: Widget that error messages are shown over
            show_mission: Shows a mission in Mission Control
        """
        self._parent = parent
        self._show_mission = show_mission
        self._language_manager = LanguageManager()

    def _mission_for(self, path: str) -> Mission | None:
        """Return the mission led by the conversation at path, if missions are being followed."""
        service = MissionServiceHolder().current_service()
        return service.mission_for_conversation(path) if service is not None and path else None

    def tab_entries(self, tab: TabBase) -> MenuEntries:
        """
        Return the entries for a tab's context menu.

        Args:
            tab: The tab that was right-clicked
        """
        if tab.tool_name() != "conversation":
            return []

        mission = self._mission_for(tab.path())
        if mission is None or mission.mode != MissionMode.CLASSIC:
            return []

        strings = self._language_manager.strings()
        return [(strings.mission_switch_to_autonomous, lambda: self._to_autonomous(mission.mission_id))]

    def file_entries(self, path: str) -> MenuEntries:
        """
        Return the entries for a conversation file's context menu.

        Args:
            path: Path of the file that was right-clicked
        """
        mission = self._mission_for(path)
        if mission is None:
            return []

        strings = self._language_manager.strings()
        if mission.mode == MissionMode.CLASSIC:
            return [(strings.mission_switch_to_autonomous, lambda: self._to_autonomous(mission.mission_id))]

        return [(strings.mission_switch_to_classic, lambda: self._to_classic(mission.mission_id))]

    def opens_in_background(self, info: ContextInfo, requester_id: str) -> bool:
        """
        Return True if a context's tab should open without taking focus.

        Conversations an autonomous mission delegates to work on their own while
        the user follows the mission in Mission Control.

        Args:
            info: The context being opened
            requester_id: Context that asked for it to be opened, or ""
        """
        service = MissionServiceHolder().current_service()
        return service is not None and service.runs_in_background(info, requester_id)

    def _to_autonomous(self, mission_id: str) -> None:
        """Hand a mission to Mission Control and show it there."""
        service = MissionServiceHolder().current_service()
        if service is None:
            return

        try:
            service.switch_to_autonomous(mission_id)

        except MissionError as e:
            strings = self._language_manager.strings()
            MessageBox.show_message(
                self._parent, MessageBoxType.WARNING, strings.mission_control, strings.mission_error_switch.format(e)
            )
            return

        self._show_mission(mission_id)

    def _to_classic(self, mission_id: str) -> None:
        """Move a mission to its conversation tab."""
        service = MissionServiceHolder().current_service()
        if service is not None:
            service.switch_to_classic(mission_id)
