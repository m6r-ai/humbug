"""Conversations view widget for mindspace."""

import json
import logging
import os
import shutil
from typing import cast

from PySide6.QtCore import Signal, QModelIndex, Qt, QSize, QPoint
from PySide6.QtWidgets import QWidget, QVBoxLayout, QMenu

from humbug.message_box import MessageBox, MessageBoxButton, MessageBoxType
from humbug.mindspace.conversations.mindspace_conversations_hierarchy_model import (
    MindspaceConversationsHierarchyModel,
    MindspaceConversationsSortProxy,
)
from humbug.mindspace.conversations.mindspace_conversations_tree_delegate import MindspaceConversationsTreeDelegate
from humbug.mindspace.conversations.mindspace_conversations_tree_view import MindspaceConversationsTreeView
from humbug.mindspace.mindspace_collapsible_header import MindspaceCollapsibleHeader
from humbug.mindspace.mindspace_log_level import MindspaceLogLevel
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.mindspace.mindspace_tree_icon_provider import MindspaceTreeIconProvider
from humbug.mindspace.mindspace_tree_style import MindspaceTreeStyle
from humbug.mindspace.mindspace_view_type import MindspaceViewType
from humbug.style_manager import StyleManager
from humbug.language.language_manager import LanguageManager


class MindspaceConversationsView(QWidget):
    """Conversations view widget for displaying mindspace conversations."""

    file_clicked = Signal(MindspaceViewType, str, bool)  # Emits view type, path, and ephemeral flag when any file is clicked
    file_deleted = Signal(str)  # Emits path when file is deleted
    file_renamed = Signal(str, str)  # Emits (old_path, new_path)
    file_moved = Signal(str, str)  # Emits (old_path, new_path)
    file_edited = Signal(str, bool)  # Emits path and ephemeral flag when file is edited
    file_opened_in_preview = Signal(str, bool)  # Emits path and ephemeral flag when file is opened in preview
    new_conversation_requested = Signal(str)  # Emits target folder path when user requests new conversation in folder
    toggled = Signal(bool)  # Emitted when expand/collapse state changes (expanded state)

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
        layout.addWidget(self._header)

        # Create tree view
        self._tree_view = MindspaceConversationsTreeView()
        self._tree_view.customContextMenuRequested.connect(self._show_context_menu)
        self._tree_style = MindspaceTreeStyle()
        self._tree_view.setStyle(self._tree_style)
        self._tree_view.file_dropped.connect(self._on_file_dropped)
        self._tree_view.drop_target_changed.connect(self._on_drop_target_changed)
        self._tree_view.delete_requested.connect(self._on_delete_requested)

        # Create hierarchy model (replaces QFileSystemModel + proxy)
        self._icon_provider = MindspaceTreeIconProvider()
        self._hierarchy_model = MindspaceConversationsHierarchyModel(self._icon_provider)
        self._hierarchy_model.conversations_changed.connect(self._on_conversations_changed)

        self._sort_proxy = MindspaceConversationsSortProxy()
        self._sort_proxy.setSourceModel(self._hierarchy_model)

        # Create and set the specialized conversations delegate
        self._delegate = MindspaceConversationsTreeDelegate(self._tree_view, self._style_manager)
        self._delegate.edit_finished.connect(self._on_delegate_edit_finished)
        self._delegate.edit_cancelled.connect(self._on_delegate_edit_cancelled)
        self._tree_view.setItemDelegate(self._delegate)

        # Set model on tree view
        self._tree_view.setModel(self._sort_proxy)

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

        # Auto-scroll state for drag operations
        self._auto_scroll_active = False

    def _is_conversation_file(self, file_path: str) -> bool:
        """
        Check if a file is a conversation file based on its extension.

        Args:
            file_path: Path to check

        Returns:
            True if this is a conversation file (.conv or .json)
        """
        if not file_path:
            return False

        if not os.path.isfile(file_path):
            return False

        _, ext = os.path.splitext(file_path.lower())
        return ext in ['.conv', '.json']

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

    def _get_display_name_from_filename(self, filename: str, file_path: str) -> str:
        """
        Get the display name from a filename, removing conversation file extensions.

        Args:
            filename: The filename to process
            file_path: Full path to the file (used to determine if it's a conversation file)

        Returns:
            Display name with conversation file extension removed if applicable
        """
        if not filename:
            return filename

        # Check if this is a conversation file and remove its extension for display
        if self._is_conversation_file(file_path):
            if filename.lower().endswith('.conv'):
                return filename[:-5]  # Remove '.conv'

            if filename.lower().endswith('.json'):
                return filename[:-5]  # Remove '.json'

        return filename

    def _handle_auto_scroll(self, start_scrolling: bool) -> None:
        """
        Handle auto-scroll requests from the tree view during drag operations.

        Args:
            start_scrolling: Whether to start or stop auto-scrolling
        """
        self._auto_scroll_active = start_scrolling

        if start_scrolling:
            # Start auto-scrolling - the tree view will handle the actual scrolling
            # We just need to ensure the tree view's scroll bars are properly configured
            pass
        else:
            # Stop auto-scrolling
            pass

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

        self.toggled.emit(expanded)

    def is_expanded(self) -> bool:
        """
        Check if the conversations section is expanded.

        Returns:
            True if expanded, False if collapsed
        """
        return self._header.is_expanded()

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
            # Perform the actual move first
            shutil.move(source_path, destination_path)
        except OSError as e:
            self._logger.error("Failed to move '%s' to '%s': %s", source_path, destination_path, str(e))
            raise

        # Emit signal so tabs can be updated (after move so new path exists)
        self.file_moved.emit(source_path, destination_path)

        # Update any .conv children whose 'parent' field pointed at the old path
        self._update_parent_references_after_move(source_path, destination_path)

        self._logger.info("Successfully moved '%s' to '%s'", source_path, destination_path)

        if source_path.lower().endswith(".conv"):
            dest_dir = os.path.dirname(destination_path)
            for child_path in self._collect_conv_descendants(destination_path):
                child_name = os.path.basename(child_path)
                child_dest = os.path.join(dest_dir, child_name)
                if child_path == child_dest or os.path.exists(child_dest):
                    continue
                try:
                    shutil.move(child_path, child_dest)
                    self.file_moved.emit(child_path, child_dest)
                    self._update_parent_references_after_move(child_path, child_dest)
                    self._logger.info("Moved child conv '%s' to '%s'", child_path, child_dest)
                except OSError as e:
                    self._logger.error("Failed to move child conv '%s': %s", child_path, str(e))

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

        # For conversation files, add the original extension back
        if not is_folder and self._is_conversation_file(temp_path):
            original_extension = self._get_original_extension(temp_path)
            final_name = new_name + original_extension

        else:
            # Non-conversation file or folder - use the name as-is
            final_name = new_name

        # Create the full path for the final item
        new_path = os.path.join(parent_path, final_name)

        try:
            # Rename the temporary item to the final name
            os.rename(temp_path, new_path)
            self._logger.info("Successfully renamed temporary %s from '%s' to '%s'",
                            "folder" if is_folder else "file", temp_path, new_path)

            # If it's a file, signal that it should be opened for editing
            if not is_folder:
                self.file_edited.emit(new_path, False)

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
        Complete a rename operation with conversation file extension preservation.

        Args:
            index: Model index being renamed
            new_name: New name for the item
        """
        # Get the current file path
        current_path = self._tree_view.get_path_from_index(index)
        if not current_path:
            raise ValueError(self._language_manager.strings().error_invalid_path)

        # Calculate the new path with extension preservation for conversation files
        directory = os.path.dirname(current_path)

        # Check if this is a conversation file and preserve its extension
        if self._is_conversation_file(current_path):
            original_extension = self._get_original_extension(current_path)
            final_name = new_name + original_extension

        else:
            # Non-conversation file - use the name as-is
            final_name = new_name

        new_path = os.path.join(directory, final_name)

        try:
            # Perform the rename
            os.rename(current_path, new_path)
            self.file_renamed.emit(current_path, new_path)

            # Update any .conv children whose 'parent' field pointed at the old path
            self._update_parent_references_after_move(current_path, new_path)

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

            # Refresh model immediately so the duplicate is visible in the tree
            self._force_model_refresh()

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
        Generate a unique name for a duplicate file, conversation-aware.

        Args:
            source_path: Path to the original file

        Returns:
            New filename with " - copy" suffix that doesn't conflict
        """
        parent_path = os.path.dirname(source_path)
        original_filename = os.path.basename(source_path)

        # For conversation files, work with the display name (without extension)
        if self._is_conversation_file(source_path):
            original_extension = self._get_original_extension(source_path)
            display_name = self._get_display_name_from_filename(original_filename, source_path)

            # Generate duplicate display name
            duplicate_display_name = self._generate_duplicate_name(display_name, parent_path, original_extension)

            # Return the full filename with extension
            return duplicate_display_name + original_extension

        # For non-conversation files, use the full filename
        return self._generate_duplicate_name(original_filename, parent_path, "")

    def _generate_duplicate_name(self, base_name: str, parent_path: str, extension: str) -> str:
        """
        Generate a unique duplicate name with " - copy" suffix.

        Args:
            base_name: Base name to duplicate (without extension for conversation files)
            parent_path: Parent directory path
            extension: File extension (empty for non-conversation files or folders)

        Returns:
            Unique duplicate name (without extension for conversation files)
        """
        # Check if the name already ends with " - copy" or " - copy (n)"
        copy_suffix = " - copy"
        if base_name.endswith(copy_suffix):
            # Remove the existing " - copy" suffix to get the base name
            core_name = base_name[:-len(copy_suffix)]

        elif " - copy (" in base_name and base_name.endswith(")"):
            # Handle case like "filename - copy (2)" - extract base name
            copy_index = base_name.rfind(" - copy (")
            if copy_index != -1:
                core_name = base_name[:copy_index]

            else:
                core_name = base_name

        else:
            # No existing copy suffix
            core_name = base_name

        # Generate unique copy name
        counter = 1
        while True:
            if counter == 1:
                candidate_name = f"{core_name}{copy_suffix}"

            else:
                candidate_name = f"{core_name}{copy_suffix} ({counter})"

            # Check if this name conflicts (add extension for the filesystem check)
            test_filename = candidate_name + extension
            full_path = os.path.join(parent_path, test_filename)
            if not os.path.exists(full_path):
                return candidate_name

            counter += 1

    def reveal_and_select_file(self, file_path: str) -> None:
        """
        Expand the tree to show the given file and select it.

        Args:
            file_path: Absolute path to the file to reveal and select
        """
        if not self._conversations_path:
            return

        normalized_path = os.path.normpath(file_path)
        if not os.path.exists(normalized_path):
            return

        source_index = self._hierarchy_model.index_for_path(normalized_path)
        if not source_index.isValid():
            return

        proxy_index = self._sort_proxy.mapFromSource(source_index)
        if not proxy_index.isValid():
            return

        # Expand all ancestor nodes
        parent = proxy_index.parent()
        ancestors = []
        while parent.isValid():
            ancestors.append(parent)
            parent = parent.parent()
        for ancestor in reversed(ancestors):
            if not self._tree_view.isExpanded(ancestor):
                self._tree_view.expand(ancestor)

        self._tree_view.clearSelection()
        self._tree_view.setCurrentIndex(proxy_index)
        self._tree_view.scrollTo(proxy_index, self._tree_view.ScrollHint.EnsureVisible)

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
        source_index = self._hierarchy_model.index_for_path(item_path)
        if not source_index.isValid():
            self._logger.warning("Source index not valid for path: '%s'", item_path)
            return

        proxy_index = self._sort_proxy.mapFromSource(source_index)
        if not proxy_index.isValid():
            self._logger.warning("Proxy index not valid for path: '%s'", item_path)
            return

        delegate = self._tree_view.itemDelegate(proxy_index)
        if not isinstance(delegate, MindspaceConversationsTreeDelegate):
            self._logger.error("Delegate is not an instance of MindspaceConversationsTreeDelegate")
            return

        delegate.start_editing(proxy_index, select_extension)

    def _create_root_context_menu(self) -> QMenu:
        """
        Create context menu for root-level actions (conversations directory).

        Returns:
            QMenu with actions appropriate for the conversations directory
        """
        menu = QMenu(self)
        strings = self._language_manager.strings()

        new_conversation_action = menu.addAction(strings.new_conversation)
        new_conversation_action.triggered.connect(
            lambda: self.new_conversation_requested.emit(cast(str, self._conversations_path))
        )
        new_folder_action = menu.addAction(strings.new_folder)
        new_folder_action.triggered.connect(
            lambda: self._start_new_folder_creation(cast(str, self._conversations_path))
        )

        menu.addSeparator()
        sort_menu = menu.addMenu(strings.sort_by)

        current_mode = self._sort_proxy.get_conversation_sort_mode()

        sort_by_name = sort_menu.addAction(strings.sort_by_name)
        sort_by_name.setCheckable(True)
        sort_by_name.setChecked(current_mode == MindspaceConversationsHierarchyModel.SortMode.NAME)
        sort_by_name.triggered.connect(
            lambda: self._sort_proxy.set_conversation_sort_mode(MindspaceConversationsHierarchyModel.SortMode.NAME)
        )

        sort_by_creation = sort_menu.addAction(strings.sort_by_creation_time)
        sort_by_creation.setCheckable(True)
        sort_by_creation.setChecked(current_mode == MindspaceConversationsHierarchyModel.SortMode.CREATION_TIME)
        sort_by_creation.triggered.connect(
            lambda: self._sort_proxy.set_conversation_sort_mode(MindspaceConversationsHierarchyModel.SortMode.CREATION_TIME)
        )

        return menu

    def _show_context_menu(self, position: QPoint) -> None:
        """Show context menu for conversations tree items."""
        index = self._tree_view.indexAt(position)
        menu = QMenu(self)
        strings = self._language_manager.strings()

        if not index.isValid():
            menu = self._create_root_context_menu()
        else:
            path = self._tree_view.get_path_from_index(index)
            if not path:
                menu = self._create_root_context_menu()
            else:
                is_dir = os.path.isdir(path)

                if is_dir:
                    preview_view_action = menu.addAction(strings.preview)
                    preview_view_action.triggered.connect(lambda: self._handle_preview_view_file(path))
                    new_conversation_action = menu.addAction(strings.new_conversation)
                    new_conversation_action.triggered.connect(lambda: self.new_conversation_requested.emit(path))
                    new_folder_action = menu.addAction(strings.new_folder)
                    new_folder_action.triggered.connect(lambda: self._start_new_folder_creation(path))
                    rename_action = menu.addAction(strings.rename)
                    rename_action.triggered.connect(lambda: self._start_rename(index))
                    delete_action = menu.addAction(strings.delete)
                    delete_action.triggered.connect(lambda: self._handle_delete_folder(path))
                else:
                    edit_action = menu.addAction(strings.edit)
                    edit_action.triggered.connect(lambda: self._handle_edit_file(path))
                    preview_view_action = menu.addAction(strings.preview)
                    preview_view_action.triggered.connect(lambda: self._handle_preview_view_file(path))
                    duplicate_action = menu.addAction(strings.duplicate)
                    duplicate_action.triggered.connect(lambda: self._start_duplicate_file(path))
                    rename_action = menu.addAction(strings.rename)
                    rename_action.triggered.connect(lambda: self._start_rename(index))
                    delete_action = menu.addAction(strings.delete)
                    delete_action.triggered.connect(lambda: self._handle_delete_file(path))

                menu.addSeparator()
                sort_menu = menu.addMenu(strings.sort_by)

                current_mode = self._sort_proxy.get_conversation_sort_mode()

                sort_by_name = sort_menu.addAction(strings.sort_by_name)
                sort_by_name.setCheckable(True)
                sort_by_name.setChecked(current_mode == MindspaceConversationsHierarchyModel.SortMode.NAME)
                sort_by_name.triggered.connect(
                    lambda: self._sort_proxy.set_conversation_sort_mode(
                        MindspaceConversationsHierarchyModel.SortMode.NAME
                    )
                )

                sort_by_creation = sort_menu.addAction(strings.sort_by_creation_time)
                sort_by_creation.setCheckable(True)
                sort_by_creation.setChecked(
                    current_mode == MindspaceConversationsHierarchyModel.SortMode.CREATION_TIME
                )
                sort_by_creation.triggered.connect(
                    lambda: self._sort_proxy.set_conversation_sort_mode(
                        MindspaceConversationsHierarchyModel.SortMode.CREATION_TIME
                    )
                )

        menu.exec_(self._tree_view.viewport().mapToGlobal(position))

    def _force_model_refresh(self) -> None:
        """Force an immediate model rebuild so newly-created items are visible."""
        if self._conversations_path and self._mindspace_path:
            self._hierarchy_model.refresh(self._conversations_path, self._mindspace_path)

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

            # Refresh model immediately so the new folder is visible in the tree
            self._force_model_refresh()

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

    def _is_current_directory_item(self, index: QModelIndex) -> bool:
        """Hierarchy model has no '.' root items — always False."""
        return False

    def _update_parent_references_after_move(self, old_abs: str, new_abs: str) -> None:
        """
        After moving or renaming a .conv file (or directory containing .conv files),
        update every .conv file in the conversations directory whose 'parent' metadata
        field references the old path, replacing it with the new path.
        """
        if not self._conversations_path or not self._mindspace_path:
            return

        old_rel = self._mindspace_manager.get_mindspace_relative_path(old_abs)
        new_rel = self._mindspace_manager.get_mindspace_relative_path(new_abs)
        if old_rel is None or new_rel is None:
            return

        is_dir_move = os.path.isdir(new_abs)
        old_rel_norm = old_rel.replace(os.sep, "/")
        new_rel_norm = new_rel.replace(os.sep, "/")
        old_prefix = old_rel_norm.rstrip("/") + "/"

        for root_dir, _, files in os.walk(self._conversations_path):
            for fname in files:
                if not fname.lower().endswith(".conv"):
                    continue
                fpath = os.path.join(root_dir, fname)
                try:
                    with open(fpath, "r", encoding="utf-8") as f:
                        data = json.load(f)
                    parent = data.get("metadata", {}).get("parent")
                    if not isinstance(parent, str) or not parent:
                        continue
                    parent_norm = parent.replace(os.sep, "/")
                    if parent_norm == old_rel_norm:
                        data["metadata"]["parent"] = new_rel_norm
                    elif is_dir_move and parent_norm.startswith(old_prefix):
                        data["metadata"]["parent"] = new_rel_norm.rstrip("/") + "/" + parent_norm[len(old_prefix):]
                    else:
                        continue
                    with open(fpath, "w", encoding="utf-8") as f:
                        json.dump(data, f, indent=2)
                except (OSError, json.JSONDecodeError, AttributeError):
                    pass

    def _collect_conv_descendants(self, conv_abs: str) -> list[str]:
        """Return all .conv files that are direct or transitive children of conv_abs."""
        if not self._conversations_path or not self._mindspace_path:
            return []

        # Build a map of rel_path -> abs_path for every .conv file
        rel_to_abs: dict[str, str] = {}
        parent_map: dict[str, str | None] = {}  # rel -> parent rel
        for root_dir, _, files in os.walk(self._conversations_path):
            for fname in files:
                if not fname.lower().endswith(".conv"):
                    continue
                fpath = os.path.join(root_dir, fname)
                rel = self._mindspace_manager.get_mindspace_relative_path(fpath)
                if rel is None:
                    continue
                rel_norm = rel.replace(os.sep, "/")
                rel_to_abs[rel_norm] = fpath
                try:
                    with open(fpath, "r", encoding="utf-8") as f:
                        data = json.load(f)
                    p = data.get("metadata", {}).get("parent")
                    parent_map[rel_norm] = p.replace(os.sep, "/") if isinstance(p, str) and p else None
                except (OSError, json.JSONDecodeError, KeyError, AttributeError):
                    parent_map[rel_norm] = None

        root_rel = self._mindspace_manager.get_mindspace_relative_path(conv_abs)
        if root_rel is None:
            return []
        root_rel_norm = root_rel.replace(os.sep, "/")

        # BFS to collect all descendants
        result: list[str] = []
        queue = [root_rel_norm]
        visited: set[str] = {root_rel_norm}
        while queue:
            current = queue.pop(0)
            for rel, par in parent_map.items():
                if par == current and rel not in visited:
                    visited.add(rel)
                    queue.append(rel)
                    abs_path = rel_to_abs.get(rel)
                    if abs_path:
                        result.append(abs_path)
        return result

    def _clear_parent_references_for_deleted_file(self, deleted_abs: str) -> None:
        """Null-out the 'parent' field in any .conv file that referenced deleted_abs as its parent."""
        if not self._conversations_path or not self._mindspace_path:
            return
        deleted_rel = self._mindspace_manager.get_mindspace_relative_path(deleted_abs)
        if deleted_rel is None:
            return
        deleted_rel_norm = deleted_rel.replace(os.sep, "/")
        for root_dir, _, files in os.walk(self._conversations_path):
            for fname in files:
                if not fname.lower().endswith(".conv"):
                    continue
                fpath = os.path.join(root_dir, fname)
                try:
                    with open(fpath, "r", encoding="utf-8") as f:
                        data = json.load(f)
                    parent = data.get("metadata", {}).get("parent")
                    if parent and parent.replace(os.sep, "/") == deleted_rel_norm:
                        data["metadata"]["parent"] = None
                        with open(fpath, "w", encoding="utf-8") as f:
                            json.dump(data, f, indent=2)
                except (OSError, json.JSONDecodeError, KeyError):
                    pass

    def _start_rename(self, index: QModelIndex) -> None:
        """
        Start inline editing to rename an item.

        Args:
            index: Model index of the item to rename
        """
        if not index.isValid():
            return

        # Don't allow renaming the current directory item
        if self._is_current_directory_item(index):
            return

        # Get the delegate and start Qt-based editing (excludes extension from selection)
        delegate = self._tree_view.itemDelegate(index)
        if not isinstance(delegate, MindspaceConversationsTreeDelegate):
            self._logger.error("Delegate is not an instance of MindspaceConversationsTreeDelegate")
            return

        delegate.start_editing(index, select_extension=False)

    def _handle_edit_file(self, path: str) -> None:
        """Edit a file."""
        self.file_edited.emit(path, False)

    def _handle_preview_view_file(self, path: str) -> None:
        """View a file in the preview."""
        self.file_opened_in_preview.emit(path, False)

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
                # Collect and delete all descendant conversations first
                if path.lower().endswith(".conv"):
                    descendants = self._collect_conv_descendants(path)
                    for child_path in descendants:
                        try:
                            self.file_deleted.emit(child_path)
                            os.remove(child_path)
                            self._mindspace_manager.add_interaction(
                                MindspaceLogLevel.INFO,
                                f"Deleted child conversation '{child_path}'"
                            )
                        except FileNotFoundError:
                            pass

                # Emit signal so tabs can be closed, then delete the file
                self.file_deleted.emit(path)
                os.remove(path)
                self._mindspace_manager.add_interaction(
                    MindspaceLogLevel.INFO,
                    f"User deleted file '{path}'"
                )

            except FileNotFoundError:
                # This can happen if the file gets auto-deleted before we get to it - ignore!
                pass

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
                self._mindspace_manager.add_interaction(
                    MindspaceLogLevel.INFO,
                    f"User deleted empty folder '{path}'"
                )

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
        index = self._tree_view.currentIndex()
        if not index.isValid():
            return

        path = self._tree_view.get_path_from_index(index)
        if not path:
            return

        if os.path.isdir(path):
            self._handle_delete_folder(path)
        else:
            self._handle_delete_file(path)

    def _on_conversations_changed(self) -> None:
        """Handle file-system changes detected by the hierarchy model."""
        # Nothing extra needed — the model already rebuilt itself.
        pass

    def set_mindspace(self, path: str) -> None:
        """Set the mindspace root directory and configure for conversations."""
        self._mindspace_path = path

        if not path:
            self._conversations_path = None
            self._hierarchy_model.refresh("", "")
            self._tree_view.configure_for_path("")
            return

        self._conversations_path = os.path.join(path, "conversations")

        if not os.path.exists(self._conversations_path):
            try:
                os.makedirs(self._conversations_path, exist_ok=True)

            except OSError as e:
                self._logger.error(
                    "Failed to create conversations directory '%s': %s",
                    self._conversations_path, str(e)
                )
                self._conversations_path = None
                self._hierarchy_model.refresh("", "")
                self._tree_view.configure_for_path("")
                return

        self._hierarchy_model.refresh(self._conversations_path, path)
        self._tree_view.configure_for_path(self._conversations_path)

        # Hide auxiliary columns (size / type / date headers are hidden anyway)
        self._tree_view.header().hideSection(1)
        self._tree_view.header().hideSection(2)
        self._tree_view.header().hideSection(3)

    def _on_tree_clicked(self, index: QModelIndex) -> None:
        """Handle click events."""
        path = self._tree_view.get_path_from_index(index)
        if not path:
            return

        self.file_clicked.emit(MindspaceViewType.CONVERSATIONS, path, True)

    def _on_tree_double_clicked(self, index: QModelIndex) -> None:
        """Handle double click events."""
        path = self._tree_view.get_path_from_index(index)
        if not path:
            return

        self.file_clicked.emit(MindspaceViewType.CONVERSATIONS, path, False)

    def _on_language_changed(self) -> None:
        """Update when the language changes."""
        self._header.set_title(self._language_manager.strings().mindspace_conversations)
        self.apply_style()

    def apply_style(self) -> None:
        """Update styling when application style changes."""
        zoom_factor = self._style_manager.zoom_factor()
        base_font_size = self._style_manager.base_font_size()

        self._header.apply_style()

        self._icon_provider.update_icons()
        self._hierarchy_model.update_icons(self._icon_provider)
        file_icon_size = round(16 * zoom_factor)
        self._tree_view.setIconSize(QSize(file_icon_size, file_icon_size))

        font = self.font()
        font.setPointSizeF(base_font_size * zoom_factor)
        self.setFont(font)
        self._tree_view.setFont(font)

        self._tree_view.setIndentation(file_icon_size)
