"""Specialized delegate for conversations tree view with conversation file extension handling."""

import os

from PySide6.QtCore import QModelIndex, QPersistentModelIndex, QSize
from PySide6.QtWidgets import QStyleOptionViewItem

from desktop.conversation_sidebar.conversation_sidebar_dag_model import ConversationSidebarDAGModel
from desktop.file_utils import is_conversation_file
from desktop.mindspace.mindspace_manager import MindspaceManager
from desktop.sidebar.sidebar_tree_delegate import SidebarTreeDelegate
from desktop.sidebar.sidebar_tree_view import SidebarTreeView
from desktop.style_manager import StyleManager


class ConversationSidebarTreeDelegate(SidebarTreeDelegate):
    """Specialized tree delegate for conversations that handles conversation file extension preservation."""

    # Extra row height (at base zoom) reserved on the row right after the
    # Pinned section, so ConversationSidebarTreeView has real, empty space to
    # draw its divider into instead of overlapping icon/text — see
    # _is_first_row_after_pinned_section() and DIVIDER_PADDING usage below.
    DIVIDER_PADDING = 10

    def __init__(self, tree_view: SidebarTreeView, style_manager: StyleManager) -> None:
        """Initialise the delegate with the shared pin-state manager."""
        super().__init__(tree_view, style_manager)
        self._mindspace_manager = MindspaceManager()

    def initStyleOption(
        self,
        option: QStyleOptionViewItem,
        index: QModelIndex | QPersistentModelIndex
    ) -> None:
        """
        Show the open-folder icon for an expanded pinned folder-group label,
        and keep the pin marker visible on a pinned folder's natural-location
        copy even while it's expanded.

        The base class already swaps in the open-folder icon for a real,
        expanded directory via get_path_from_index() — but a group label's
        path is deliberately hidden from that lookup (to keep it
        non-interactive), so it needs its own check here using the model's
        group_folder_path() escape hatch.  Separately, the base class's
        open-folder swap would otherwise clobber the natural-copy pin marker
        set by the model's DecorationRole whenever that folder is expanded,
        so it's re-applied here to take priority.
        """
        super().initStyleOption(option, index)

        model_index = self._to_model_index(index)
        if not model_index.isValid():
            return

        model = model_index.model()
        if not isinstance(model, ConversationSidebarDAGModel):
            return

        folder_path = model.group_folder_path(model_index)
        if folder_path and self._tree_view.isExpanded(model_index):
            option.icon = self._icon_provider.open_folder_icon()
            return

        path = model.path_for_index(model_index)
        if (
            path
            and model.data(model_index, model.IsDirRole)
            and not model.is_pinned_section_copy(model_index)
            and self._mindspace_manager.folder_has_pinned_content(path)
        ):
            option.icon = self._icon_provider.pinned_section_icon()

    def sizeHint(
        self,
        option: QStyleOptionViewItem,
        index: QModelIndex | QPersistentModelIndex,
    ) -> QSize:
        """
        Reserve real, empty space on the boundary row for the tree view's divider.

        Only the extra height is reserved here — keeping every part of that
        row's own painting (icon, text, and Qt's own branch/selection
        background) out of that space is handled by
        ConversationSidebarTreeView.drawRow(), which is the single place
        that controls a tree row's full paint area; the delegate alone only
        controls the item's content area, not the branch/indentation
        background Qt paints alongside it.
        """
        size = super().sizeHint(option, index)
        model = index.model()
        if not isinstance(model, ConversationSidebarDAGModel) or not model.is_first_row_after_pinned_section(index):
            return size

        zoom = self._style_manager.zoom_factor()
        return QSize(size.width(), size.height() + round(self.DIVIDER_PADDING * zoom))

    def _is_conversation_file(self, file_path: str) -> bool:
        """Check if a file is a conversation file (.conv or .json)."""
        if not file_path or not os.path.isfile(file_path):
            return False

        return is_conversation_file(file_path)

    def _get_original_extension(self, file_path: str) -> str:
        """
        Get the original extension from a conversation file.

        Args:
            file_path: Path to the conversation file

        Returns:
            The extension (including the dot) or empty string if not a conversation file
        """
        if not self._is_conversation_file(file_path):
            return ""

        _, ext = os.path.splitext(file_path)
        return ext

    def validate_new_name(self, index: QModelIndex, new_name: str) -> tuple[bool, str]:
        """
        Validate a new name for uniqueness, taking into account conversation file extension preservation.

        Args:
            index: Model index being edited
            new_name: Proposed new name

        Returns:
            Tuple of (is_valid, error_message)
        """
        try:
            # Get the file path from the tree view
            file_path = self._tree_view.get_path_from_index(index)
            if not file_path:
                return False, self._language_manager.strings().error_validation_failed

            # Get the directory
            directory = os.path.dirname(file_path)

            # Check if this is a conversation file that will have its extension preserved
            if self._is_conversation_file(file_path):
                original_extension = self._get_original_extension(file_path)
                # The final filename will have the original extension appended
                final_name = new_name + original_extension

            else:
                # Non-conversation file - use the name as-is
                final_name = new_name

            # Check if a file with the final name already exists
            new_path = os.path.join(directory, final_name)
            if os.path.exists(new_path) and not os.path.samefile(new_path, file_path):
                return False, self._language_manager.strings().rename_error_exists

            return True, ""

        except Exception:
            return False, self._language_manager.strings().error_validation_failed
