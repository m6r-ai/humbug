"""Conversations view widget for mindspace."""

import logging
import os
import shutil

from PySide6.QtCore import Signal, QModelIndex, Qt, QSize, QPoint, QTimer
from PySide6.QtWidgets import (
    QWidget, QVBoxLayout, QMenu
)

from humbug.message_box import MessageBox, MessageBoxButton, MessageBoxType
from humbug.mindspace.conversations.mindspace_conversations_tree_delegate import MindspaceConversationsTreeDelegate
from humbug.mindspace.conversations.mindspace_conversations_tree_view import MindspaceConversationsTreeView
from humbug.mindspace.conversations.mindspace_conversations_dag_model import MindspaceConversationsDAGModel
from humbug.mindspace.conversations.mindspace_conversations_index import MindspaceConversationsIndex
from humbug.mindspace.mindspace_section_header import MindspaceSectionHeader
from humbug.mindspace.mindspace_log_level import MindspaceLogLevel
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.mindspace.mindspace_pane_style import build_tree_pane_stylesheet
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

        self._header = MindspaceSectionHeader(
            self._language_manager.strings().mindspace_conversations,
            self
        )
        layout.addWidget(self._header)

        # Create tree view
        self._tree_view = MindspaceConversationsTreeView()
        self._tree_view.customContextMenuRequested.connect(self._show_context_menu)
        self._tree_style = MindspaceTreeStyle()
        self._tree_view.setStyle(self._tree_style)
        self._tree_view.file_dropped.connect(self._on_file_dropped)
        self._tree_view.drop_target_changed.connect(self._on_drop_target_changed)
        self._tree_view.delete_requested.connect(self._on_delete_requested)

        # Create icon provider for styling
        self._icon_provider = MindspaceTreeIconProvider()

        # Conversation DAG index and model (model created after index in set_mindspace)
        self._conversations_index = MindspaceConversationsIndex(self)
        self._dag_model = MindspaceConversationsDAGModel(self._conversations_index, self._icon_provider, self)
        self._dag_model.about_to_rebuild.connect(self._save_expanded_state)
        self._dag_model.rebuilt.connect(self._restore_expanded_state)
        self._expanded_paths: set[str] = set()
        self._suppress_save_expanded: bool = False

        # Create and set the specialized conversations delegate
        self._delegate = MindspaceConversationsTreeDelegate(self._tree_view, self._style_manager)
        self._delegate.edit_finished.connect(self._on_delegate_edit_finished)
        self._delegate.edit_cancelled.connect(self._on_delegate_edit_cancelled)
        self._tree_view.setItemDelegate(self._delegate)

        # Set model on tree view
        self._tree_view.setModel(self._dag_model)

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
        self._selected_path: str | None = None

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

            # For conversation files, compute the DAG scope
            included: set = {source_path}
            excluded: set = set()
            if not is_folder and source_path.lower().endswith('.conv'):
                included, excluded = self._conversations_index.compute_operation_scope({source_path})

            scope_detail = self._build_scope_detail(included, excluded, source_path)
            message = self._create_move_confirmation_message(item_name, source_path, destination_path)
            if scope_detail:
                message += scope_detail

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
            self._perform_move_operation(source_path, destination_path, included)

        except Exception as e:
            self._logger.error("Error handling file drop from '%s' to '%s': %s", source_path, target_path, str(e))
            strings = self._language_manager.strings()
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.move_error_title,
                strings.move_error_failed.format(str(e))
            )

    def _perform_move_operation(
        self,
        source_path: str,
        destination_path: str,
        included: set | None = None
    ) -> None:
        """
        Perform the actual file/folder move operation.

        Args:
            source_path: Source path of the item to move
            destination_path: Destination path where the item will be moved
            included: Set of additional paths (DAG children) to move to the
                same target directory.  If None, only source_path is moved.

        Raises:
            OSError: If the move operation fails
        """
        try:
            # Emit signal first so tabs can be updated
            self.file_moved.emit(source_path, destination_path)

            # Perform the actual move
            shutil.move(source_path, destination_path)

            self._logger.info("Successfully moved '%s' to '%s'", source_path, destination_path)

            # Move any exclusively-owned children to the same target directory
            if included:
                target_dir = os.path.dirname(destination_path)
                for child_path in included:
                    if os.path.normpath(child_path) == os.path.normpath(source_path):
                        continue

                    child_dest = os.path.join(target_dir, os.path.basename(child_path))
                    self.file_moved.emit(child_path, child_dest)
                    shutil.move(child_path, child_dest)
                    self._logger.info("Successfully moved '%s' to '%s'", child_path, child_dest)

        except OSError as e:
            self._logger.error("Failed to move '%s' to '%s': %s", source_path, destination_path, str(e))
            raise

    def _display_name(self, path: str) -> str:
        """
        Get a human-readable display name for a conversation file.

        Args:
            path: Absolute path to the conversation file.

        Returns:
            Display name with .conv extension removed.
        """
        name = os.path.basename(path)
        if name.lower().endswith('.conv'):
            name = name[:-5]

        return name

    def _build_scope_detail(self, included: set, excluded: set, original: str) -> str:
        """
        Build a detail string describing the operation scope.

        Args:
            included: Paths that will be included in the operation.
            excluded: Paths that will be left behind.
            original: The directly selected path.

        Returns:
            Detail string for the confirmation dialog, or empty string if no children.
        """
        children = sorted(included - {os.path.normpath(original)})
        parts = []
        if children:
            bullets = "\n".join(f"  \u2022 {self._display_name(p)}" for p in children)
            parts.append(f"\n\nRelated conversations that will also be affected:\n\n{bullets}")

        if excluded:
            bullets = "\n".join(f"  \u2022 {self._display_name(p)}" for p in sorted(excluded))
            parts.append(f"\n\nThe following will be left in place (referenced by other conversations):\n{bullets}")

        return "".join(parts)

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
            # Save expansion state now with current paths, before the rename triggers
            # a file watcher event.  The flag prevents _save_expanded_state from
            # overwriting this with stale paths from the tree view.
            self._expanded_paths.clear()
            self._collect_expanded_paths(QModelIndex())
            current = self._tree_view.currentIndex()
            self._selected_path = self._dag_model.path_for_index(current) if current.isValid() else None

            # Perform the rename
            os.rename(current_path, new_path)
            self.file_renamed.emit(current_path, new_path)

            # Update saved expansion state so the renamed node stays expanded
            # after the model rebuilds in response to the file watcher firing.
            if current_path in self._expanded_paths:
                self._expanded_paths.discard(current_path)
                self._expanded_paths.add(new_path)

            # Update saved selection if the renamed item was selected
            if self._selected_path == current_path:
                self._selected_path = new_path

            # For directory renames, update any child paths that were expanded
            if os.path.isdir(new_path):
                old_prefix = current_path + os.sep
                new_prefix = new_path + os.sep
                updated = {
                    new_prefix + p[len(old_prefix):]
                    if p.startswith(old_prefix) else p
                    for p in self._expanded_paths
                }
                self._expanded_paths = updated

                # Also update selected path if it was under the renamed directory
                if self._selected_path and self._selected_path.startswith(current_path + os.sep):
                    self._selected_path = new_path + os.sep + self._selected_path[len(current_path + os.sep):]

            self._suppress_save_expanded = True

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
        # Validate that we have a conversations directory loaded
        if not self._conversations_path:
            return

        if not os.path.exists(file_path):
            return

        # Expand to the file and select it
        target_index = self._dag_model.index_for_path(file_path)
        if not target_index.isValid():
            return

        self._tree_view.clearSelection()
        self._tree_view.setCurrentIndex(target_index)
        self._tree_view.scrollTo(target_index, self._tree_view.ScrollHint.EnsureVisible)

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
        filter_index = self._dag_model.index_for_path(item_path)
        if not filter_index.isValid():
            self._logger.warning("Index not valid for path: '%s'", item_path)
            return

        # Get the delegate and start editing
        delegate = self._tree_view.itemDelegate(filter_index)
        if not isinstance(delegate, MindspaceConversationsTreeDelegate):
            self._logger.error("Delegate is not an instance of MindspaceConversationsTreeDelegate")
            return

        delegate.start_editing(filter_index, select_extension)

    def _create_root_context_menu(self) -> QMenu:
        """
        Create context menu for root-level actions (conversations directory).

        Returns:
            QMenu with actions appropriate for the conversations directory
        """
        menu = QMenu(self)
        strings = self._language_manager.strings()

        # Conversations root actions
        new_conversation_action = menu.addAction(strings.new_conversation)
        new_conversation_action.triggered.connect(
            lambda: self.new_conversation_requested.emit(self._conversations_path or "")
        )
        new_folder_action = menu.addAction(strings.new_folder)
        new_folder_action.triggered.connect(
            lambda: self._start_new_folder_creation(self._conversations_path or "")
        )

        return menu

    def _collect_expanded_paths(self, parent: QModelIndex) -> None:
        """
        Recursively collect paths of all expanded nodes under parent.

        Args:
            parent: Parent model index to recurse from.
        """
        for row in range(self._dag_model.rowCount(parent)):
            index = self._dag_model.index(row, 0, parent)
            if not index.isValid():
                continue

            if self._tree_view.isExpanded(index):
                path = self._dag_model.path_for_index(index)
                if path:
                    self._expanded_paths.add(path)

                self._collect_expanded_paths(index)

    def _save_expanded_state(self) -> None:
        """Save the set of expanded node paths and current selection before a model rebuild."""
        if self._suppress_save_expanded:
            # Paths already updated for a rename — don't overwrite with stale tree state
            self._suppress_save_expanded = False
            return

        self._expanded_paths.clear()
        self._collect_expanded_paths(QModelIndex())

        current = self._tree_view.currentIndex()
        if current.isValid():
            self._selected_path = self._dag_model.path_for_index(current)
        else:
            self._selected_path = None

    def _restore_expanded_state(self) -> None:
        """Restore expanded nodes and selection after a model rebuild."""
        for path in self._expanded_paths:
            index = self._dag_model.index_for_path(path)
            if index.isValid():
                self._tree_view.expand(index)

        if self._selected_path:
            index = self._dag_model.index_for_path(self._selected_path)
            if index.isValid():
                self._tree_view.setCurrentIndex(index)

    def _is_current_directory_item(self, index: QModelIndex) -> bool:
        """
        Check if the given index represents the root sentinel (".") item.

        Args:
            index: Model index to check

        Returns:
            True if this is the root sentinel item
        """
        if not index.isValid():
            return False

        path = self._tree_view.get_path_from_index(index)
        return path is not None and os.path.basename(path) == "."

    def _show_context_menu(self, position: QPoint) -> None:
        """Show context menu for conversations tree items."""
        # Get the index at the clicked position
        index = self._tree_view.indexAt(position)

        # Create context menu
        menu = QMenu(self)

        # Determine the path and whether it's a file or directory
        if not index.isValid():
            # Clicked on empty space - show root context menu
            menu = self._create_root_context_menu()

        elif self._is_current_directory_item(index):
            menu = self._create_root_context_menu()

        else:
            path = self._tree_view.get_path_from_index(index)
            if not path:
                return

            is_dir = os.path.isdir(path)
            strings = self._language_manager.strings()

            # Create actions based on item type
            if is_dir:
                # For directories: show all options (no "New File" option)
                edit_action = None
                preview_view_action = menu.addAction(strings.preview)
                preview_view_action.triggered.connect(lambda: self._handle_preview_view_file(path))
                duplicate_action = None
                new_conversation_action = menu.addAction(strings.new_conversation)
                new_conversation_action.triggered.connect(lambda: self.new_conversation_requested.emit(path))
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
                preview_view_action = menu.addAction(strings.preview)
                preview_view_action.triggered.connect(lambda: self._handle_preview_view_file(path))
                duplicate_action = menu.addAction(strings.duplicate)
                duplicate_action.triggered.connect(lambda: self._start_duplicate_file(path))
                new_folder_action = None
                rename_action = menu.addAction(strings.rename)
                rename_action.triggered.connect(lambda: self._start_rename(index))
                delete_action = menu.addAction(strings.delete)
                delete_action.triggered.connect(lambda: self._handle_delete_file(path))

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
        # Compute which related conversations will also be deleted
        included, excluded = self._conversations_index.compute_operation_scope({path})

        # Show confirmation dialog using MessageBox
        strings = self._language_manager.strings()
        detail = self._build_scope_detail(included, excluded, path)
        message = (
            strings.confirm_delete_item_message.format(self._display_name(path))
            + "\n\n" + strings.delete_warning_detail
            + detail
        )
        result = MessageBox.show_message(
            self,
            MessageBoxType.WARNING,
            strings.confirm_delete_title,
            message,
            [MessageBoxButton.YES, MessageBoxButton.NO],
            True
        )

        if result == MessageBoxButton.YES:
            # Delete all included files — deepest paths first to avoid
            # trying to delete a parent before its children are gone
            for file_path in sorted(included, key=lambda p: len(p), reverse=True):
                try:
                    self.file_deleted.emit(file_path)
                    os.remove(file_path)
                    self._mindspace_manager.add_interaction(
                        MindspaceLogLevel.INFO,
                        f"User deleted file '{file_path}'"
                    )

                except FileNotFoundError:
                    # This can happen if the file gets auto-deleted before we get to it - ignore!
                    pass

                except OSError as e:
                    self._logger.error("Failed to delete file '%s': %s", file_path, str(e))
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
        # Get the currently selected index
        index = self._tree_view.currentIndex()
        if not index.isValid():
            return

        if self._is_current_directory_item(index):
            return

        path = self._tree_view.get_path_from_index(index)
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
            self._conversations_index.set_conversations_dir("")
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
                self._tree_view.configure_for_path("")
                self._conversations_index.set_conversations_dir("")
                return

        # Configure tree view with the conversations path
        self._tree_view.configure_for_path(self._conversations_path)

        # Start the DAG index for this conversations directory
        self._conversations_index.set_conversations_dir(self._conversations_path)

        # Schedule a repaint after the event loop processes the index scan and model reset
        QTimer.singleShot(0, self._tree_view.viewport().update)

    def conversations_index(self) -> MindspaceConversationsIndex:
        """
        Get the live DAG index for the current conversations directory.

        Returns:
            MindspaceConversationsIndex instance.
        """
        return self._conversations_index

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

        # Apply style to header
        self._header.apply_style()

        self._icon_provider.update_icons()
        # Invalidate icon cache in model after icon provider refresh
        self._dag_model.beginResetModel()
        self._dag_model.endResetModel()
        file_icon_size = round(16 * zoom_factor)
        self._tree_view.setIconSize(QSize(file_icon_size, file_icon_size))

        # Update font size for tree
        font = self.font()
        font.setPointSizeF(base_font_size * zoom_factor)
        self.setFont(font)
        self._tree_view.setFont(font)

        # Adjust tree indentation
        self._tree_view.setIndentation(file_icon_size)
        self.setStyleSheet(build_tree_pane_stylesheet(
            self._style_manager,
            "MindspaceConversationsView",
            "MindspaceConversationsTreeView",
            self.layoutDirection(),
            zoom_factor,
        ))
