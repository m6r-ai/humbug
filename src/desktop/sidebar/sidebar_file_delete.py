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


def confirm_and_trash_file(
    parent: QWidget,
    path: str,
    display_name: str,
    on_deleted: Callable[[str], None],
) -> None:
    """
    Ask the user, then move a file into the trash and close its tabs.

    The file goes to the trash rather than being unlinked, so a delete the user
    did not mean can be undone from the trash panel or from the changes panel.
    The tabs holding the file are closed before it moves, because a tab watching
    a missing file would otherwise report a change the user did not make.

    This is shared by every panel that lists mindspace files: they differ only in
    how they name a file, and a second implementation of the confirmation or of
    the trashing rules would eventually disagree with this one.

    Args:
        parent: Widget the confirmation and any error message is shown against.
        path: Absolute path to the file to delete.
        display_name: How the panel names the file in its messages.
        on_deleted: Called with the path before it is moved, so the panel can close
            the tabs and drop the file from its listing.
    """
    strings = LanguageManager().strings()
    mindspace_manager = MindspaceManager()
    mindspace = mindspace_manager.mindspace()

    result = MessageBox.show_message(
        parent,
        MessageBoxType.WARNING,
        strings.confirm_delete_title,
        strings.confirm_delete_item_message.format(display_name) + "\n\n" + strings.delete_warning_detail,
        [MessageBoxButton.YES, MessageBoxButton.NO],
        True,
    )
    if result != MessageBoxButton.YES:
        return

    on_deleted(path)
    if not os.path.exists(path):
        return

    try:
        mindspace.trash_file(path, JournalActor.HUMAN)

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
        f"User deleted file '{path}'",
    )
