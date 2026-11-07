"""Deleting a file from a sidebar panel."""

from collections.abc import Callable
import os

from PySide6.QtWidgets import QWidget

from mindspace.journal_entry import JournalActor
from mindspace.mindspace_error import MindspaceError
from mindspace.mindspace_log_level import MindspaceLogLevel

from desktop.language.language_manager import LanguageManager
from desktop.message_box import MessageBox, MessageBoxButton, MessageBoxType
from desktop.mindspace.mindspace_manager import MindspaceManager


def confirm_and_delete_path(
    parent: QWidget,
    path: str,
    display_name: str,
    on_deleted: Callable[[str], None],
) -> None:
    """
    Ask the user, then remove a file or empty folder and close its tabs.

    The removal is recorded in the change journal before the path is removed, so
    a delete the user did not mean can be reverted from the Changes panel.  The
    journal is the only place a removal of mindspace content is captured, whether
    the user or the AI asked for it, so the panel does not choose where to put the
    content.

    The tabs holding the path are closed before it is removed, because a tab
    watching a missing file would otherwise report a change the user did not make.

    This is shared by every panel that lists mindspace files: they differ only in
    how they name a file, and a second implementation of the confirmation or of
    the removal rules would eventually disagree with this one.

    Args:
        parent: Widget the confirmation and any error message is shown against.
        path: Absolute path to the file or empty folder to delete.
        display_name: How the panel names the path in its messages.
        on_deleted: Called with the path before it is removed, so the panel can
            close the tabs and drop it from its listing.
    """
    strings = LanguageManager().strings()
    mindspace_manager = MindspaceManager()
    mindspace = mindspace_manager.mindspace()

    result = MessageBox.show_message(
        parent,
        MessageBoxType.WARNING,
        strings.confirm_delete_title,
        strings.confirm_delete_item_message.format(display_name) + "\n\n" + strings.delete_recorded_warning_detail,
        [MessageBoxButton.YES, MessageBoxButton.NO],
        True,
    )
    if result != MessageBoxButton.YES:
        return

    on_deleted(path)
    if not os.path.exists(path):
        return

    try:
        mindspace.remove_path(path, JournalActor.HUMAN)

    except MindspaceError as e:
        MessageBox.show_message(
            parent,
            MessageBoxType.CRITICAL,
            strings.file_error_title,
            strings.error_deleting_file.format(str(e)),
            [MessageBoxButton.OK],
        )
        return

    mindspace_manager.add_interaction(
        MindspaceLogLevel.INFO,
        f"User deleted '{path}'",
    )
