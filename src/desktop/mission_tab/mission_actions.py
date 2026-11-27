"""Actions on a mission that ask the user first, shared by Mission Control and the Missions panel."""

from PySide6.QtWidgets import QInputDialog, QWidget

from desktop.language.language_manager import LanguageManager
from desktop.message_box import MessageBox, MessageBoxButton, MessageBoxType
from mission import MissionError, MissionService


def _show_error(parent: QWidget, message: str) -> None:
    """Tell the user an action on a mission failed."""
    MessageBox.show_message(parent, MessageBoxType.CRITICAL, LanguageManager().strings().mission_control, message)


def rename_mission(parent: QWidget, service: MissionService, mission_id: str) -> None:
    """
    Ask for a new name for a mission and apply it.

    Args:
        parent: Widget the question is asked over
        service: The service following the mission
        mission_id: The mission to rename
    """
    mission = next((m for m in service.missions() if m.mission_id == mission_id), None)
    if mission is None:
        return

    strings = LanguageManager().strings()
    title, accepted = QInputDialog.getText(
        parent, strings.mission_rename_title, strings.mission_rename_label, text=mission.title
    )
    if not accepted or not title.strip():
        return

    try:
        service.rename_mission(mission_id, title)

    except MissionError as e:
        _show_error(parent, strings.mission_error_rename.format(str(e)))


def archive_mission(parent: QWidget, service: MissionService, mission_id: str, archived: bool) -> None:
    """
    Archive a mission, or bring it back.

    Args:
        parent: Widget a failure is reported over
        service: The service following the mission
        mission_id: The mission
        archived: True to archive it, False to bring it back
    """
    try:
        service.set_archived(mission_id, archived)

    except MissionError as e:
        _show_error(parent, str(e))


def delete_mission(parent: QWidget, service: MissionService, mission_id: str) -> None:
    """
    Delete a mission after confirmation; its conversations are kept.

    Args:
        parent: Widget the confirmation is asked over
        service: The service following the mission
        mission_id: The mission to delete
    """
    mission = next((m for m in service.missions() if m.mission_id == mission_id), None)
    if mission is None:
        return

    strings = LanguageManager().strings()
    result = MessageBox.show_message(
        parent,
        MessageBoxType.QUESTION,
        strings.mission_delete_title,
        strings.mission_delete_confirm.format(mission.title),
        [MessageBoxButton.YES, MessageBoxButton.NO],
        destructive=True
    )
    if result != MessageBoxButton.YES:
        return

    try:
        service.remove_mission(mission_id)

    except MissionError as e:
        _show_error(parent, str(e))


async def run_mission_again(parent: QWidget, service: MissionService, mission_id: str) -> str:
    """
    Start a new mission from an earlier one.

    Args:
        parent: Widget a failure is reported over
        service: The service following the mission
        mission_id: The earlier mission

    Returns:
        The new mission's ID, or "" if it could not be started
    """
    try:
        mission = await service.run_again(mission_id)

    except MissionError as e:
        _show_error(parent, LanguageManager().strings().mission_error_start.format(str(e)))
        return ""

    return mission.mission_id
