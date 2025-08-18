"""Conversations view widget for mindspace."""

import logging
import os
import shutil
from typing import cast

from PySide6.QtCore import Signal, QModelIndex, Qt, QSize, QPoint, QDir
from PySide6.QtWidgets import (
    QFileSystemModel, QWidget, QVBoxLayout, QMenu
)

from humbug.message_box import MessageBox, MessageBoxButton, MessageBoxType
from humbug.mindspace.mindspace_collapsible_header import MindspaceCollapsibleHeader
from humbug.mindspace.mindspace_conversations_model import MindspaceConversationsModel
from humbug.mindspace.mindspace_conversations_tree_view import MindspaceConversationsTreeView
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.mindspace.mindspace_tree_delegate import MindspaceTreeDelegate
from humbug.mindspace.mindspace_tree_icon_provider import MindspaceTreeIconProvider
from humbug.mindspace.mindspace_tree_style import MindspaceTreeStyle
from humbug.style_manager import StyleManager
from humbug.language.language_manager import LanguageManager


class MindspaceConversationsView(QWidget):
    """Conversations view widget for displaying mindspace conversations."""

    file_clicked = Signal(str, bool)  # Emits path and ephemeral flag when any file is clicked
    file_deleted = Signal(str)  # Emits path when file is deleted
    file_renamed = Signal(str, str)  # Emits (old_path, new_path)
    file_moved = Signal(str, str)  # Emits (old_path, new_path)
    file_edited = Signal(str)  # Emits path when file is edited
    file_opened_in_wiki = Signal(str)  # Emits path when file is opened in wiki

    def __init__(self, parent: QWidget | None = None) -> None:
        """Initialize the conversations view widget."""
        super().__init__(parent)

        self._style_manager = StyleManager()
        self._logger = logging.getLogger("MindspaceConversationsView")
        self._mindspace_manager = MindspaceManager()

        # Create layout
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        # Create collapsible header
        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._on_language_changed)

        self._header = MindspaceCollapsibleHeader(
            self._language_manager.strings().mindspace_conversations,
            self
        )
        self._header.toggled.connect(self._on_header_toggled)
        self._header.set_context_menu_provider(self._create_header_context_menu)
        layout.addWidget(self._header)

        # Create tree view
        self._tree_view = MindspaceConversationsTreeView()
        self._tree_view.customContextMenuRequested.connect(self._show_context_menu)
        self._tree_style = MindspaceTreeStyle()
        self._tree_view.setStyle(self._tree_style)
        self._tree_view.file_dropped.connect(self._on_file_dropped)
        self._tree_view.drop_target_changed.connect(self._on_drop_target_changed)
        self._tree_view.delete_requested.connect(self._on_delete_requested)

        # Create file system model
        self._icon_provider = MindspaceTreeIconProvider()
        self._fs_model = QFileSystemModel()
        self._fs_model.setReadOnly(True)
        self._fs_model.setFilter(QDir.Filter.AllEntries | QDir.Filter.NoDotAndDotDot | QDir.Filter.Hidden)

        # Create filter model
        self._filter_model = MindspaceConversationsModel()
        self._filter_model.setSourceModel(self._fs_model)

        # Create and set the editable delegate
        self._delegate = MindspaceTreeDelegate(self._tree_view, self._style_manager)
        self._delegate.edit_finished.connect(self._on_delegate_edit_finished)
        self._delegate.edit_cancelled.connect(self._on_delegate_edit_cancelled)
        self._tree_view.setItemDelegate(self._delegate)

        # Set model on tree view
        self._tree_view.setModel(self._filter_model)

        # Connect signals
        self._tree_view.clicked.connect(self._on_tree_clicked)
        self._tree_view.doubleClicked.connect(self._on_tree_double_clicked)

        # Add to layout
        layout.addWidget(self._tree_view)

        # Hide horizontal scrollbar
        self._tree_view.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)

        # Track current mindspace and conversations directory
        self._mindspace_path: str | None = None
        self._conversations_path: str | None = None

        # Track pending new items for creation flow
        # Format: (parent_path, is_folder, temp_path)
        self._pending_new_item: tuple[str, bool, str] | None = None

    def get_header_height(self) -> int:
        """
        Get the height of the header.

        Returns:
            Header height in pixels
        """
        return self._header.sizeHint().height()

    def _on_header_toggled(self, expanded: bool) -> None:
        """
        Handle header expand/collapse toggle.

        Args:
            expanded: Whether the section is now expanded
        """
        if expanded:
            self._tree_view.show()

        else:
            self._tree_view.hide()

    def is_expanded(self) -> bool:
        """
        Check if the conversations section is expanded.

        Returns:
            True if expanded, False if collapsed
        """
        return self._header.is_expanded()

    def set_expanded(self, expanded: bool) -> None:
        """
        Set the expanded state of the conversations section.

        Args:
            expanded: Whether the section should be expanded
        """
        self._header.set_expanded(expanded, emit_signal=False)
        if expanded:
            self._tree_view.show()

        else:
            self._tree_view.hide()

    def _on_drop_target_changed(self) -> None:
        """
        Handle changes to the drop target in the tree view.
        """
        # Force a repaint of the entire viewport to ensure proper visual updates
        # This ensures both the old drop target and new drop target are repainted
        self._tree_view.viewport().update()

    def _create_move_confirmation_message(self, item_name: str, source_path: str, dest_path: str) -> str:
        """Create the confirmation message for file/folder move operations."""
        strings = self._language_manager.strings()

        # Get display paths (relative to mindspace if possible)
        display_source = self._get_display_path(source_path)
        display_dest = self._get_display_path(dest_path)

        # Determine if it's a file or folder
        is_folder = os.path.isdir(source_path)
        confirmation_text = (strings.move_folder_confirmation.format(item_name)
                            if is_folder else strings.move_file_confirmation.format(item_name))

        return f"{confirmation_text}\n\n{strings.move_from_label} {display_source}\n\n{strings.move_to_label} {display_dest}"

    def _get_display_path(self, path: str) -> str:
        """Get the display path for the dialog (relative to mindspace if possible)."""
        try:
            if self._mindspace_manager.has_mindspace():
                return self._mindspace_manager.get_relative_path(path)

        except Exception:
            pass

        return path

    def _on_file_dropped(self, source_path: str, target_path: str) -> None:
        """
        Handle a file/folder drop operation.

        Args:
            source_path: Path of the item being moved
            target_path: Path of the drop target directory
        """
        try:
            # Determine the destination path
            item_name = os.path.basename(source_path)
            destination_path = os.path.join(target_path, item_name)

            # Check if destination already exists
            if os.path.exists(destination_path):
                strings = self._language_manager.strings()
                MessageBox.show_message(
                    self,
                    MessageBoxType.WARNING,
                    strings.move_error_title,
                    strings.move_error_exists.format(item_name)
                )
                return

            # Show confirmation dialog using MessageBox instead of custom dialog
            strings = self._language_manager.strings()
            is_folder = os.path.isdir(source_path)
            title = strings.move_folder_title if is_folder else strings.move_file_title
            message = self._create_move_confirmation_message(item_name, source_path, destination_path)

            result = MessageBox.show_message(
                self,
                MessageBoxType.QUESTION,
                title,
                message,
                [MessageBoxButton.YES, MessageBoxButton.NO]
            )

            if result != MessageBoxButton.YES:
                return

            # Perform the move operation
            self._perform_move_operation(source_path, destination_path)

        except Exception as e:
            self._logger.error("Error handling file drop from '%s' to '%s': %s", source_path, target_path, str(e))
            strings = self._language_manager.strings()
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.move_error_title,
                strings.move_error_failed.format(str(e))
            )

    def _perform_move_operation(self, source_path: str, destination_path: str) -> None:
        """
        Perform the actual file/folder move operation.

        Args:
            source_path: Source path of the item to move
            destination_path: Destination path where the item will be moved

        Raises:
            OSError: If the move operation fails
        """
        try:
            # Emit signal first so tabs can be updated
            self.file_moved.emit(source_path, destination_path)

            # Perform the actual move
            shutil.move(source_path, destination_path)

            self._logger.info("Successfully moved '%s' to '%s'", source_path, destination_path)

        except OSError as e:
            self._logger.error("Failed to move '%s' to '%s': %s", source_path, destination_path, str(e))
            raise

    def _on_delegate_edit_finished(self, index: QModelIndex, new_name: str) -> None:
        """
        Handle when inline editing is finished.

        Args:
            index: Model index that was edited
            new_name: The new name entered by the user
        """
        try:
            # Check if this is a pending new item creation
            if self._pending_new_item:
                self._complete_new_item_creation(new_name)
                return

            # This is a rename operation
            self._complete_rename_operation(index, new_name)

        except Exception as e:
            self._logger.error("Error completing inline edit: %s", str(e))
            strings = self._language_manager.strings()
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.error_title,
                str(e)
            )

    def _on_delegate_edit_cancelled(self) -> None:
        """
        Handle when inline editing is cancelled.
        """
        # If this was a pending new item, clean it up
        if self._pending_new_item:
            self._cleanup_pending_new_item()

    def _complete_new_item_creation(self, new_name: str) -> None:
        """
        Complete the creation of a new file or folder by renaming the temporary item.

        Args:
            new_name: Name for the new item
        """
        if not self._pending_new_item:
            return

        parent_path, is_folder, temp_path = self._pending_new_item
        self._pending_new_item = None

        # Create the full path for the final item
        new_path = os.path.join(parent_path, new_name)

        try:
            # Rename the temporary item to the final name
            os.rename(temp_path, new_path)
            self._logger.info("Successfully renamed temporary %s from '%s' to '%s'",
                            "folder" if is_folder else "file", temp_path, new_path)

            # If it's a file, signal that it should be opened for editing
            if not is_folder:
                self.file_edited.emit(new_path)

        except OSError as e:
            self._logger.error("Failed to rename temporary %s from '%s' to '%s': %s",
                             "folder" if is_folder else "file", temp_path, new_path, str(e))

            # Clean up the temporary item on failure
            try:
                if is_folder:
                    os.rmdir(temp_path)

                else:
                    os.remove(temp_path)

            except OSError:
                pass  # Best effort cleanup

            strings = self._language_manager.strings()
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.file_creation_error_title,
                strings.error_creating_folder.format(str(e)) if is_folder else strings.file_creation_error.format(str(e))
            )

    def _cleanup_pending_new_item(self) -> None:
        """
        Clean up a pending new item that was cancelled.
        """
        if not self._pending_new_item:
            return

        _parent_path, is_folder, temp_path = self._pending_new_item
        self._pending_new_item = None

        # Remove the temporary file/folder
        try:
            if is_folder:
                os.rmdir(temp_path)

            else:
                os.remove(temp_path)

            self._logger.info("Cleaned up cancelled temporary %s: '%s'",
                            "folder" if is_folder else "file", temp_path)
        except OSError as e:
            self._logger.warning("Failed to clean up temporary %s '%s': %s",
                               "folder" if is_folder else "file", temp_path, str(e))

    def _complete_rename_operation(self, index: QModelIndex, new_name: str) -> None:
        """
        Complete a rename operation.

        Args:
            index: Model index being renamed
            new_name: New name for the item
        """
        # Get the current file path
        current_path = self._tree_view.get_path_from_index(index)
        if not current_path:
            raise ValueError(self._language_manager.strings().error_invalid_path)

        # Calculate the new path
        directory = os.path.dirname(current_path)
        new_path = os.path.join(directory, new_name)

        try:
            # Perform the rename
            os.rename(current_path, new_path)
            self.file_renamed.emit(current_path, new_path)

            self._logger.info("Successfully renamed '%s' to '%s'", current_path, new_path)

        except OSError as e:
            self._logger.error("Failed to rename '%s' to '%s': %s", current_path, new_path, str(e))
            strings = self._language_manager.strings()
            raise OSError(strings.rename_error_generic.format(str(e))) from e

    def _start_duplicate_file(self, source_path: str) -> None:
        """
        Start the duplication of a file using inline editing.

        Args:
            source_path: Path to the file to duplicate
        """
        # Get the parent directory
        parent_path = os.path.dirname(source_path)

        # Generate the default duplicate name
        duplicate_name = self._get_duplicate_file_name(source_path)
        duplicate_path = os.path.join(parent_path, duplicate_name)

        try:
            # Copy the source file to the duplicate location
            shutil.copy2(source_path, duplicate_path)
            self._logger.info("Created duplicate file: '%s'", duplicate_path)

            # Set up pending creation state with the duplicate path
            self._pending_new_item = (parent_path, False, duplicate_path)

            # Ensure the duplicate file is visible and start editing
            self._ensure_item_visible_and_edit(duplicate_path, select_extension=False)

        except (OSError, shutil.Error) as e:
            self._logger.error("Failed to duplicate file '%s': %s", source_path, str(e))
            strings = self._language_manager.strings()
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.file_creation_error_title,
                strings.error_duplicating_file.format(str(e))
            )

    def _get_duplicate_file_name(self, source_path: str) -> str:
        """
        Generate a unique name for a duplicate file.

        Args:
            source_path: Path to the original file

        Returns:
            New filename with " - copy" suffix that doesn't conflict
        """
        parent_path = os.path.dirname(source_path)
        original_filename = os.path.basename(source_path)

        # Split filename and extension
        name, ext = os.path.splitext(original_filename)

        # Check if the name already ends with " - copy" or " - copy (n)"
        copy_suffix = " - copy"
        if name.endswith(copy_suffix):
            # Remove the existing " - copy" suffix to get the base name
            base_name = name[:-len(copy_suffix)]

        elif " - copy (" in name and name.endswith(")"):
            # Handle case like "filename - copy (2)" - extract base name
            copy_index = name.rfind(" - copy (")
            if copy_index != -1:
                base_name = name[:copy_index]

            else:
                base_name = name

        else:
            # No existing copy suffix
            base_name = name

        # Generate unique copy name
        counter = 1
        while True:
            if counter == 1:
                candidate_name = f"{base_name}{copy_suffix}{ext}"

            else:
                candidate_name = f"{base_name}{copy_suffix} ({counter}){ext}"

            full_path = os.path.join(parent_path, candidate_name)
            if not os.path.exists(full_path):
                return candidate_name

            counter += 1

    def reveal_and_select_file(self, file_path: str) -> None:
        """
        Expand the tree to show the given file and select it.

        Args:
            file_path: Absolute path to the file to reveal and select
        """
        # Validate that we have a conversations directory loaded
        if not self._conversations_path:
            return

        # Normalize the file path
        normalized_path = os.path.normpath(file_path)

        # Ensure the file path is within the conversations directory
        if not normalized_path.startswith(self._conversations_path):
            return

        # Check if the file exists
        if not os.path.exists(normalized_path):
            return

        # Ensure the section is expanded before revealing the file
        if not self.is_expanded():
            self.set_expanded(True)

        # Expand to the file and select it
        target_index = self._expand_to_path(normalized_path)
        if target_index is None:
            return

        if not target_index.isValid():
            return

        self._tree_view.clearSelection()
        self._tree_view.setCurrentIndex(target_index)
        self._tree_view.scrollTo(target_index, self._tree_view.ScrollHint.EnsureVisible)

    def _expand_to_path(self, file_path: str) -> QModelIndex | None:
        """
        Expand tree nodes to reveal the given file path.

        Args:
            file_path: Absolute path to expand to

        Returns:
            QModelIndex of the target file if found, None otherwise
        """
        if not self._conversations_path:
            return None

        # Build list of paths from conversations root to target file
        paths_to_expand = []
        current_path = file_path

        while current_path and current_path != self._conversations_path:
            paths_to_expand.append(current_path)
            parent_path = os.path.dirname(current_path)
            # Prevent infinite loop if we can't go up further
            if parent_path == current_path:
                break

            current_path = parent_path

        # Reverse the list so we expand from root to leaf
        paths_to_expand.reverse()

        # Start from the conversations root
        current_index = QModelIndex()

        # Expand each directory in the path
        for path in paths_to_expand:
            # Get the source index for this path
            source_index = self._fs_model.index(path)
            if not source_index.isValid():
                return None

            # Map to filter model
            filter_index = self._filter_model.mapFromSource(source_index)
            if not filter_index.isValid():
                return None

            # If this is a directory, expand it
            if os.path.isdir(path):
                self._tree_view.expand(filter_index)

            current_index = filter_index

        return current_index

    def _ensure_item_visible_and_edit(self, item_path: str, select_extension: bool = True) -> None:
        """
        Ensure an item is visible in the tree view and start editing it.

        Args:
            item_path: Path to the item to make visible and edit
            select_extension: Whether to select the file extension in addition to the name
        """
        # Make the item visible and start editing
        self._tree_view.ensure_path_visible_for_editing(item_path, lambda: self._start_edit_for_path(item_path, select_extension))

    def _start_edit_for_path(self, item_path: str, select_extension: bool = True) -> None:
        """
        Start editing for a specific file path.

        Args:
            item_path: Path to the item to start editing
            select_extension: Whether to select the file extension in addition to the name
        """
        # Find the item in the model
        source_index = self._fs_model.index(item_path)
        if not source_index.isValid():
            self._logger.warning("Source index not valid for path: '%s'", item_path)
            return

        filter_index = self._filter_model.mapFromSource(source_index)
        if not filter_index.isValid():
            self._logger.warning("Filter index not valid for path: '%s'", item_path)
            return

        # Get the delegate and start editing
        delegate = self._tree_view.itemDelegate(filter_index)
        if not isinstance(delegate, MindspaceTreeDelegate):
            self._logger.error("Delegate is not an instance of MindspaceTreeDelegate")
            return

        delegate.start_editing(filter_index, select_extension)

    def _create_header_context_menu(self) -> QMenu | None:
        """
        Create context menu for the header (root-level actions).

        Returns:
            QMenu with root-level actions, or None if no conversations directory is active
        """
        if not self._conversations_path:
            return None

        return self._create_root_context_menu()

    def _create_root_context_menu(self) -> QMenu:
        """
        Create context menu for root-level actions (conversations directory).

        Returns:
            QMenu with actions appropriate for the conversations directory
        """
        menu = QMenu(self)
        strings = self._language_manager.strings()

        # Conversations root actions
        new_folder_action = menu.addAction(strings.new_folder)
        new_folder_action.triggered.connect(
            lambda: self._start_new_folder_creation(cast(str, self._conversations_path))
        )

        # Add sorting options
        menu.addSeparator()
        sort_menu = menu.addMenu(strings.sort_by)

        current_mode = self._filter_model.get_conversation_sort_mode()

        sort_by_name = sort_menu.addAction(strings.sort_by_name)
        sort_by_name.setCheckable(True)
        sort_by_name.setChecked(current_mode == MindspaceConversationsModel.SortMode.NAME)
        sort_by_name.triggered.connect(
            lambda: self._filter_model.set_conversation_sort_mode(MindspaceConversationsModel.SortMode.NAME)
        )

        sort_by_creation = sort_menu.addAction(strings.sort_by_creation_time)
        sort_by_creation.setCheckable(True)
        sort_by_creation.setChecked(current_mode == MindspaceConversationsModel.SortMode.CREATION_TIME)
        sort_by_creation.triggered.connect(
            lambda: self._filter_model.set_conversation_sort_mode(MindspaceConversationsModel.SortMode.CREATION_TIME)
        )

        return menu

    def _show_context_menu(self, position: QPoint) -> None:
        """Show context menu for conversations tree items."""
        # Get the index at the clicked position
        index = self._tree_view.indexAt(position)

        # Create context menu
        menu = QMenu(self)
        strings = self._language_manager.strings()

        # Determine the path and whether it's a file or directory
        if not index.isValid():
            # Clicked on empty space - show root context menu
            menu = self._create_root_context_menu()

        else:
            # Map to source model to get actual file path
            source_index = self._filter_model.mapToSource(index)
            path = QDir.toNativeSeparators(self._fs_model.filePath(source_index))
            is_dir = os.path.isdir(path)

            # Create actions based on item type
            if is_dir:
                # For directories: show all options (no "New File" option)
                edit_action = None
                wiki_view_action = menu.addAction(strings.wiki_view)
                wiki_view_action.triggered.connect(lambda: self._handle_wiki_view_file(path))
                duplicate_action = None
                new_folder_action = menu.addAction(strings.new_folder)
                new_folder_action.triggered.connect(lambda: self._start_new_folder_creation(path))
                rename_action = menu.addAction(strings.rename)
                rename_action.triggered.connect(lambda: self._start_rename(index))
                delete_action = menu.addAction(strings.delete)
                delete_action.triggered.connect(lambda: self._handle_delete_folder(path))

            else:
                # File context menu
                edit_action = menu.addAction(strings.edit)
                edit_action.triggered.connect(lambda: self._handle_edit_file(path))
                wiki_view_action = menu.addAction(strings.wiki_view)
                wiki_view_action.triggered.connect(lambda: self._handle_wiki_view_file(path))
                duplicate_action = menu.addAction(strings.duplicate)
                duplicate_action.triggered.connect(lambda: self._start_duplicate_file(path))
                new_folder_action = None
                rename_action = menu.addAction(strings.rename)
                rename_action.triggered.connect(lambda: self._start_rename(index))
                delete_action = menu.addAction(strings.delete)
                delete_action.triggered.connect(lambda: self._handle_delete_file(path))

            # Add sorting options
            menu.addSeparator()
            sort_menu = menu.addMenu(strings.sort_by)

            current_mode = self._filter_model.get_conversation_sort_mode()

            sort_by_name = sort_menu.addAction(strings.sort_by_name)
            sort_by_name.setCheckable(True)
            sort_by_name.setChecked(current_mode == MindspaceConversationsModel.SortMode.NAME)
            sort_by_name.triggered.connect(
                lambda: self._filter_model.set_conversation_sort_mode(MindspaceConversationsModel.SortMode.NAME)
            )

            sort_by_creation = sort_menu.addAction(strings.sort_by_creation_time)
            sort_by_creation.setCheckable(True)
            sort_by_creation.setChecked(current_mode == MindspaceConversationsModel.SortMode.CREATION_TIME)
            sort_by_creation.triggered.connect(
                lambda: self._filter_model.set_conversation_sort_mode(MindspaceConversationsModel.SortMode.CREATION_TIME)
            )

        menu.exec_(self._tree_view.viewport().mapToGlobal(position))

    def _start_new_folder_creation(self, parent_path: str) -> None:
        """
        Start the creation of a new folder using inline editing.

        Args:
            parent_path: Path to the parent directory where folder will be created
        """
        # Create a temporary folder with default name
        default_name = self._get_default_folder_name(parent_path)
        temp_folder_path = os.path.join(parent_path, default_name)

        try:
            os.makedirs(temp_folder_path)
            self._logger.info("Created temporary folder: '%s'", temp_folder_path)

            # Set up pending creation state with the temporary path
            self._pending_new_item = (parent_path, True, temp_folder_path)

            # Ensure the new folder is visible and start editing
            self._ensure_item_visible_and_edit(temp_folder_path, select_extension=True)

        except OSError as e:
            self._logger.error("Failed to create temporary folder '%s': %s", temp_folder_path, str(e))
            strings = self._language_manager.strings()
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.file_creation_error_title,
                strings.error_creating_folder.format(str(e))
            )

    def _get_default_folder_name(self, parent_path: str) -> str:
        """
        Get a default name for a new folder.

        Args:
            parent_path: Parent directory path

        Returns:
            Default folder name that doesn't conflict with existing items
        """
        base_name = "New Folder"
        counter = 1

        while True:
            name = base_name if counter == 1 else f"{base_name} {counter}"
            full_path = os.path.join(parent_path, name)
            if not os.path.exists(full_path):
                return name
            counter += 1

    def _start_rename(self, index: QModelIndex) -> None:
        """
        Start inline editing to rename an item.

        Args:
            index: Model index of the item to rename
        """
        if not index.isValid():
            return

        # Get the delegate and start Qt-based editing (excludes extension from selection)
        delegate = self._tree_view.itemDelegate(index)
        if not isinstance(delegate, MindspaceTreeDelegate):
            self._logger.error("Delegate is not an instance of MindspaceTreeDelegate")
            return

        delegate.start_editing(index, select_extension=False)

    def _handle_edit_file(self, path: str) -> None:
        """Edit a file."""
        self.file_edited.emit(path)

    def _handle_wiki_view_file(self, path: str) -> None:
        """View a file in the wiki."""
        self.file_opened_in_wiki.emit(path)

    def _handle_delete_file(self, path: str) -> None:
        """Handle request to delete a file.

        Args:
            path: Path to the file to delete
        """
        # Show confirmation dialog using MessageBox
        strings = self._language_manager.strings()
        result = MessageBox.show_message(
            self,
            MessageBoxType.WARNING,
            strings.confirm_delete_title,
            strings.confirm_delete_item_message.format(os.path.basename(path)) + "\n\n" + strings.delete_warning_detail,
            [MessageBoxButton.YES, MessageBoxButton.NO],
            True
        )

        if result == MessageBoxButton.YES:
            try:
                # First emit signal so tabs can be closed
                self.file_deleted.emit(path)

                # Then delete the file
                os.remove(path)

            except OSError as e:
                self._logger.error("Failed to delete file '%s': %s", path, str(e))
                MessageBox.show_message(
                    self,
                    MessageBoxType.CRITICAL,
                    strings.file_error_title,
                    strings.error_deleting_file.format(str(e)),
                    [MessageBoxButton.OK]
                )

    def _handle_delete_folder(self, path: str) -> None:
        """Handle request to delete a folder.

        Args:
            path: Path to the folder to delete
        """
        strings = self._language_manager.strings()

        # Check if folder is empty
        try:
            if os.listdir(path):
                MessageBox.show_message(
                    self,
                    MessageBoxType.WARNING,
                    strings.confirm_delete_title,
                    strings.error_folder_not_empty,
                    [MessageBoxButton.OK]
                )
                return
        except OSError as e:
            self._logger.error("Failed to check if folder '%s' is empty: %s", path, str(e))
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.file_error_title,
                strings.error_deleting_file.format(str(e)),
                [MessageBoxButton.OK]
            )
            return

        # Show confirmation dialog
        result = MessageBox.show_message(
            self,
            MessageBoxType.WARNING,
            strings.confirm_delete_title,
            strings.confirm_delete_item_message.format(os.path.basename(path)),
            [MessageBoxButton.YES, MessageBoxButton.NO],
            True
        )

        if result == MessageBoxButton.YES:
            try:
                # Delete the empty folder
                os.rmdir(path)

            except OSError as e:
                self._logger.error("Failed to delete folder '%s': %s", path, str(e))
                MessageBox.show_message(
                    self,
                    MessageBoxType.CRITICAL,
                    strings.file_error_title,
                    strings.error_deleting_file.format(str(e)),
                    [MessageBoxButton.OK]
                )

    def _on_delete_requested(self) -> None:
        """Handle delete request from the tree view."""
        # Get the currently selected index
        index = self._tree_view.currentIndex()
        if not index.isValid():
            return

        # Map to source model to get actual file path
        source_index = self._filter_model.mapToSource(index)
        path = QDir.toNativeSeparators(self._fs_model.filePath(source_index))
        if not path:
            return

        # Check if it's a directory or file and call appropriate handler
        if os.path.isdir(path):
            self._handle_delete_folder(path)

        else:
            self._handle_delete_file(path)

    def set_mindspace(self, path: str) -> None:
        """Set the mindspace root directory and configure for conversations."""
        self._mindspace_path = path

        if not path:
            # Clear the model when no mindspace is active
            self._conversations_path = None
            self._filter_model.set_conversations_root("")
            # Configure tree view for empty path
            self._tree_view.configure_for_path("")
            return

        # Set conversations directory path
        self._conversations_path = os.path.join(path, "conversations")

        # Ensure conversations directory exists
        if not os.path.exists(self._conversations_path):
            try:
                os.makedirs(self._conversations_path, exist_ok=True)

            except OSError as e:
                self._logger.error("Failed to create conversations directory '%s': %s", self._conversations_path, str(e))
                self._conversations_path = None
                self._filter_model.set_conversations_root("")
                self._tree_view.configure_for_path("")
                return

        # Set the file system model root directly to the conversations directory
        self._fs_model.setRootPath(self._conversations_path)
        self._filter_model.set_conversations_root(self._conversations_path)

        # Configure tree view with the conversations path
        self._tree_view.configure_for_path(self._conversations_path)

        # Set the root index directly to the conversations directory
        conversations_source_index = self._fs_model.index(self._conversations_path)
        root_index = self._filter_model.mapFromSource(conversations_source_index)
        self._tree_view.setRootIndex(root_index)

        # Hide size, type, and date columns
        self._tree_view.header().hideSection(1)  # Size
        self._tree_view.header().hideSection(2)  # Type
        self._tree_view.header().hideSection(3)  # Date

    def _on_tree_clicked(self, index: QModelIndex) -> None:
        """Handle click events."""
        # Get the file path from the source model
        source_index = self._filter_model.mapToSource(index)
        path = QDir.toNativeSeparators(self._fs_model.filePath(source_index))
        if not path:
            return

        self.file_clicked.emit(path, True)

    def _on_tree_double_clicked(self, index: QModelIndex) -> None:
        """Handle double click events."""
        # Get the file path from the source model
        source_index = self._filter_model.mapToSource(index)
        path = QDir.toNativeSeparators(self._fs_model.filePath(source_index))
        if not path:
            return

        self.file_clicked.emit(path, False)

    def _on_language_changed(self) -> None:
        """Update when the language changes."""
        self._header.set_title(self._language_manager.strings().mindspace_conversations)
        self.apply_style()

    def apply_style(self) -> None:
        """Update styling when application style changes."""
        zoom_factor = self._style_manager.zoom_factor()
        base_font_size = self._style_manager.base_font_size()

        # Apply style to header
        self._header.apply_style()

        self._icon_provider.update_icons()
        self._fs_model.setIconProvider(self._icon_provider)
        file_icon_size = round(16 * zoom_factor)
        self._tree_view.setIconSize(QSize(file_icon_size, file_icon_size))

        # Update font size for tree
        font = self.font()
        font.setPointSizeF(base_font_size * zoom_factor)
        self.setFont(font)
        self._tree_view.setFont(font)

        # Adjust tree indentation
        self._tree_view.setIndentation(file_icon_size)
