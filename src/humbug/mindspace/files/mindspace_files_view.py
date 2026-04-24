"""Files view widget for mindspace."""

import json
import logging
import os
import shutil
from typing import cast

from PySide6.QtCore import Signal, QModelIndex, Qt, QSize, QPoint, QDir
from PySide6.QtWidgets import QFileSystemModel, QWidget, QVBoxLayout, QMenu

from humbug.message_box import MessageBox, MessageBoxButton, MessageBoxType
from humbug.mindspace.files.mindspace_files_model import MindspaceFilesModel
from humbug.mindspace.files.mindspace_files_tree_view import MindspaceFilesTreeView
from humbug.mindspace.mindspace_collapsible_header import MindspaceCollapsibleHeader
from humbug.mindspace.mindspace_log_level import MindspaceLogLevel
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.mindspace.mindspace_tree_delegate import MindspaceTreeDelegate
from humbug.mindspace.mindspace_tree_icon_provider import MindspaceTreeIconProvider
from humbug.mindspace.mindspace_tree_style import MindspaceTreeStyle
from humbug.mindspace.mindspace_view_type import MindspaceViewType
from humbug.style_manager import StyleManager
from humbug.language.language_manager import LanguageManager
from humbug.mindspace.vcs.mindspace_vcs_poller import MindspaceVCSPoller


class MindspaceFilesView(QWidget):
    """Files view widget for displaying mindspace files."""

    file_clicked = Signal(MindspaceViewType, str, bool)  # Emits view type, path, and ephemeral flag when any file is clicked
    file_deleted = Signal(str)  # Emits path when file is deleted
    file_renamed = Signal(str, str)  # Emits (old_path, new_path)
    file_moved = Signal(str, str)  # Emits (old_path, new_path)
    file_edited = Signal(str, bool)  # Emits path and ephemeral flag when file is edited
    file_opened_in_preview = Signal(str, bool)  # Emits path and ephemeral flag when file is opened in preview
    file_opened_in_diff = Signal(str, bool)  # Emits path and ephemeral flag when file is opened in diff
    toggled = Signal(bool)  # Emitted when expand/collapse state changes (expanded state)

    def __init__(self, parent: QWidget | None = None) -> None:
        """Initialize the files view widget."""
        super().__init__(parent)

        self._style_manager = StyleManager()
        self._logger = logging.getLogger("MindspaceFilesView")
        self._mindspace_manager = MindspaceManager()
        self._vcs_poller = MindspaceVCSPoller()

        # Create layout
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        # Create collapsible header
        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._on_language_changed)

        self._header = MindspaceCollapsibleHeader(
            self._language_manager.strings().mindspace_files,
            self
        )
        self._header.setProperty("splitter", True)
        self._header.toggled.connect(self._on_header_toggled)
        layout.addWidget(self._header)

        # Create tree view
        self._tree_view = MindspaceFilesTreeView()
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
        self._fs_model.setFilter(QDir.Filter.AllEntries | QDir.Filter.Hidden | QDir.Filter.NoDotDot)

        self._filter_model = MindspaceFilesModel()
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

        # Track current mindspace
        self._mindspace_path: str | None = None

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

        self.toggled.emit(expanded)

    def is_expanded(self) -> bool:
        """
        Check if the files section is expanded.

        Returns:
            True if expanded, False if collapsed
        """
        return self._header.is_expanded()

    def _on_drop_target_changed(self) -> None:
        """Handle changes to the drop target in the tree view."""
        self._tree_view.viewport().update()

    def _create_move_confirmation_message(self, item_name: str, source_path: str, dest_path: str) -> str:
        """Create the confirmation message for file/folder move operations."""
        strings = self._language_manager.strings()
        display_source = self._get_display_path(source_path)
        display_dest = self._get_display_path(dest_path)
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
            item_name = os.path.basename(source_path)
            destination_path = os.path.join(target_path, item_name)

            if os.path.exists(destination_path):
                strings = self._language_manager.strings()
                MessageBox.show_message(
                    self,
                    MessageBoxType.WARNING,
                    strings.move_error_title,
                    strings.move_error_exists.format(item_name)
                )
                return

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
            shutil.move(source_path, destination_path)
        except OSError as e:
            self._logger.error("Failed to move '%s' to '%s': %s", source_path, destination_path, str(e))
            raise

        self.file_moved.emit(source_path, destination_path)
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
            if self._pending_new_item:
                self._complete_new_item_creation(new_name)
                return

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
        """Handle when inline editing is cancelled."""
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

        new_path = os.path.join(parent_path, new_name)

        try:
            os.rename(temp_path, new_path)
            self._logger.info("Successfully renamed temporary %s from '%s' to '%s'",
                            "folder" if is_folder else "file", temp_path, new_path)

            if not is_folder:
                self.file_edited.emit(new_path, False)

        except OSError as e:
            self._logger.error("Failed to rename temporary %s from '%s' to '%s': %s",
                             "folder" if is_folder else "file", temp_path, new_path, str(e))

            try:
                if is_folder:
                    os.rmdir(temp_path)

                else:
                    os.remove(temp_path)

            except OSError:
                pass

            strings = self._language_manager.strings()
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.file_creation_error_title,
                strings.error_creating_folder.format(str(e)) if is_folder else strings.file_creation_error.format(str(e))
            )

    def _cleanup_pending_new_item(self) -> None:
        """Clean up a pending new item that was cancelled."""
        if not self._pending_new_item:
            return

        _parent_path, is_folder, temp_path = self._pending_new_item
        self._pending_new_item = None

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
        current_path = self._tree_view.get_path_from_index(index)
        if not current_path:
            raise ValueError(self._language_manager.strings().error_invalid_path)

        directory = os.path.dirname(current_path)
        new_path = os.path.join(directory, new_name)

        try:
            os.rename(current_path, new_path)
            self.file_renamed.emit(current_path, new_path)
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
        parent_path = os.path.dirname(source_path)
        duplicate_name = self._get_duplicate_file_name(source_path)
        duplicate_path = os.path.join(parent_path, duplicate_name)

        try:
            shutil.copy2(source_path, duplicate_path)
            self._logger.info("Created duplicate file: '%s'", duplicate_path)

            self._pending_new_item = (parent_path, False, duplicate_path)

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
        name, ext = os.path.splitext(original_filename)

        copy_suffix = " - copy"
        if name.endswith(copy_suffix):
            base_name = name[:-len(copy_suffix)]

        elif " - copy (" in name and name.endswith(")"):
            copy_index = name.rfind(" - copy (")
            if copy_index != -1:
                base_name = name[:copy_index]

            else:
                base_name = name

        else:
            base_name = name

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
        if not self._mindspace_path:
            return

        normalized_path = os.path.normpath(file_path)
        if not normalized_path.startswith(self._mindspace_path):
            return

        if not os.path.exists(normalized_path):
            return

        target_index = self._expand_to_path(normalized_path)
        if target_index is None or not target_index.isValid():
            return

        self._tree_view.clearSelection()
        self._tree_view.setCurrentIndex(target_index)
        self._tree_view.scrollTo(target_index, self._tree_view.ScrollHint.EnsureVisible)

    def _expand_to_path(self, file_path: str) -> QModelIndex | None:
        """Expand tree nodes to reveal the given file path."""
        paths_to_expand = []
        current_path = file_path
        while current_path and current_path != self._mindspace_path:
            paths_to_expand.append(current_path)
            parent_path = os.path.dirname(current_path)
            if parent_path == current_path:
                break
            current_path = parent_path

        paths_to_expand.reverse()

        for path in paths_to_expand:
            source_index = self._fs_model.index(path)
            if not source_index.isValid():
                return None
            filter_index = self._filter_model.mapFromSource(source_index)
            if not filter_index.isValid():
                return None
            if os.path.isdir(path):
                self._tree_view.expand(filter_index)
            current_index = filter_index

        return current_index if paths_to_expand else None

    def _ensure_item_visible_and_edit(self, item_path: str, select_extension: bool = True) -> None:
        """
        Ensure an item is visible in the tree view and start editing it.

        Args:
            item_path: Path to the item to make visible and edit
            select_extension: Whether to select the file extension in addition to the name
        """
        self._tree_view.ensure_path_visible_for_editing(item_path, lambda: self._start_edit_for_path(item_path, select_extension))

    def _start_edit_for_path(self, item_path: str, select_extension: bool = True) -> None:
        """
        Start editing for a specific file path.

        Args:
            item_path: Path to the item to start editing
            select_extension: Whether to select the file extension in addition to the name
        """
        source_index = self._fs_model.index(item_path)
        if not source_index.isValid():
            self._logger.warning("Source index not valid for path: '%s'", item_path)
            return

        filter_index = self._filter_model.mapFromSource(source_index)
        if not filter_index.isValid():
            self._logger.warning("Filter index not valid for path: '%s'", item_path)
            return

        delegate = self._tree_view.itemDelegate(filter_index)
        if not isinstance(delegate, MindspaceTreeDelegate):
            self._logger.error("Delegate is not an instance of MindspaceTreeDelegate")
            return

        delegate.start_editing(filter_index, select_extension)

    def _create_root_context_menu(self) -> QMenu:
        """
        Create context menu for root-level actions.

        Returns:
            QMenu with actions appropriate for the root directory
        """
        menu = QMenu(self)
        strings = self._language_manager.strings()

        new_folder_action = menu.addAction(strings.new_folder)
        new_folder_action.triggered.connect(
            lambda: self._start_new_folder_creation(cast(str, self._mindspace_path))
        )
        new_file_action = menu.addAction(strings.new_file)
        new_file_action.triggered.connect(
            lambda: self._start_new_file_creation(cast(str, self._mindspace_path))
        )

        return menu

    def _is_current_directory_item(self, index: QModelIndex) -> bool:
        """Check if the given index represents the current directory (".") item."""
        if not index.isValid():
            return False
        source_index = self._filter_model.mapToSource(index)
        if not source_index.isValid():
            return False
        return self._fs_model.fileName(source_index) == "."

    def _show_context_menu(self, position: QPoint) -> None:
        """Show context menu for file tree items."""
        index = self._tree_view.indexAt(position)
        menu = QMenu(self)
        strings = self._language_manager.strings()

        if not index.isValid():
            menu = self._create_root_context_menu()

        elif self._is_current_directory_item(index):
            menu = self._create_root_context_menu()

        else:
            source_index = self._filter_model.mapToSource(index)
            path = QDir.toNativeSeparators(self._fs_model.filePath(source_index))
            is_dir = os.path.isdir(path)

            if is_dir:
                preview_view_action = menu.addAction(strings.preview)
                preview_view_action.triggered.connect(lambda: self._handle_preview_view_file(path))
                new_folder_action = menu.addAction(strings.new_folder)
                new_folder_action.triggered.connect(lambda: self._start_new_folder_creation(path))
                new_file_action = menu.addAction(strings.new_file)
                new_file_action.triggered.connect(lambda: self._start_new_file_creation(path))
                rename_action = menu.addAction(strings.rename)
                rename_action.triggered.connect(lambda: self._start_rename(index))
                delete_action = menu.addAction(strings.delete)
                delete_action.triggered.connect(lambda: self._handle_delete_folder(path))

            else:
                edit_action = menu.addAction(strings.edit)
                edit_action.triggered.connect(lambda: self._handle_edit_file(path))
                preview_view_action = menu.addAction(strings.preview)
                preview_view_action.triggered.connect(lambda: self._handle_preview_view_file(path))
                if self._vcs_poller.has_vcs_changes(path):
                    diff_action = menu.addAction(strings.diff)
                    diff_action.triggered.connect(lambda: self._handle_diff_file(path))

                duplicate_action = menu.addAction(strings.duplicate)
                duplicate_action.triggered.connect(lambda: self._start_duplicate_file(path))
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
        default_name = self._get_default_folder_name(parent_path)
        temp_folder_path = os.path.join(parent_path, default_name)

        try:
            os.makedirs(temp_folder_path)
            self._logger.info("Created temporary folder: '%s'", temp_folder_path)

            self._pending_new_item = (parent_path, True, temp_folder_path)

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

    def _start_new_file_creation(self, parent_path: str) -> None:
        """
        Start the creation of a new file using inline editing.

        Args:
            parent_path: Path to the parent directory where file will be created
        """
        default_name = self._get_default_file_name(parent_path)
        temp_file_path = os.path.join(parent_path, default_name)

        try:
            with open(temp_file_path, 'w', encoding='utf-8') as f:
                f.write("")

            self._logger.info("Created temporary file: '%s'", temp_file_path)

            self._pending_new_item = (parent_path, False, temp_file_path)

            self._ensure_item_visible_and_edit(temp_file_path, select_extension=True)

        except OSError as e:
            self._logger.error("Failed to create temporary file '%s': %s", temp_file_path, str(e))
            strings = self._language_manager.strings()
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.file_creation_error_title,
                strings.file_creation_error.format(str(e))
            )

    def _get_default_folder_name(self, parent_path: str) -> str:
        """Get a default name for a new folder that doesn't conflict with existing items."""
        base_name = "New Folder"
        counter = 1

        while True:
            name = base_name if counter == 1 else f"{base_name} {counter}"
            full_path = os.path.join(parent_path, name)
            if not os.path.exists(full_path):
                return name
            counter += 1

    def _get_default_file_name(self, parent_path: str) -> str:
        """Get a default name for a new file that doesn't conflict with existing items."""
        base_name = "New File.txt"
        counter = 1

        while True:
            if counter == 1:
                name = base_name

            else:
                name_part, ext = os.path.splitext(base_name)
                name = f"{name_part} {counter}{ext}"

            full_path = os.path.join(parent_path, name)
            if not os.path.exists(full_path):
                return name

            counter += 1

    def _get_conversations_dir(self) -> str | None:
        """Return the absolute conversations directory path, or None if unavailable."""
        if not self._mindspace_manager.has_mindspace():
            return None
        try:
            path = self._mindspace_manager.get_absolute_path("conversations")
            return path if os.path.isdir(path) else None
        except Exception:
            return None

    def _update_parent_references_after_move(self, old_abs: str, new_abs: str) -> None:
        """Update .conv children whose 'parent' field referenced old_abs to use new_abs."""
        conv_dir = self._get_conversations_dir()
        if not conv_dir:
            return
        old_rel = self._mindspace_manager.get_mindspace_relative_path(old_abs)
        new_rel = self._mindspace_manager.get_mindspace_relative_path(new_abs)
        if old_rel is None or new_rel is None:
            return
        old_rel_norm = old_rel.replace(os.sep, "/")
        new_rel_norm = new_rel.replace(os.sep, "/")
        is_dir_move = os.path.isdir(new_abs)
        old_prefix = old_rel_norm.rstrip("/") + "/"
        for root_dir, _, files in os.walk(conv_dir):
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
        conv_dir = self._get_conversations_dir()
        if not conv_dir:
            return []

        rel_to_abs: dict[str, str] = {}
        parent_map: dict[str, str | None] = {}
        for root_dir, _, files in os.walk(conv_dir):
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
        """Null-out the 'parent' field in .conv files that referenced deleted_abs."""
        conv_dir = self._get_conversations_dir()
        if not conv_dir:
            return
        deleted_rel = self._mindspace_manager.get_mindspace_relative_path(deleted_abs)
        if deleted_rel is None:
            return
        deleted_rel_norm = deleted_rel.replace(os.sep, "/")
        for root_dir, _, files in os.walk(conv_dir):
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

        delegate = self._tree_view.itemDelegate(index)
        if not isinstance(delegate, MindspaceTreeDelegate):
            self._logger.error("Delegate is not an instance of MindspaceTreeDelegate")
            return

        delegate.start_editing(index, select_extension=False)

    def _handle_edit_file(self, path: str) -> None:
        """Edit a file."""
        self.file_edited.emit(path, False)

    def _handle_preview_view_file(self, path: str) -> None:
        """View a file in the preview."""
        self.file_opened_in_preview.emit(path, False)

    def _handle_diff_file(self, path: str) -> None:
        """Open a file diff view."""
        self.file_opened_in_diff.emit(path, False)

    def _handle_delete_file(self, path: str) -> None:
        """Handle request to delete a file.

        Args:
            path: Path to the file to delete
        """
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

                self.file_deleted.emit(path)
                os.remove(path)
                self._mindspace_manager.add_interaction(
                    MindspaceLogLevel.INFO,
                    f"User deleted file '{path}'"
                )

            except FileNotFoundError:
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

        if self._is_current_directory_item(index):
            return

        source_index = self._filter_model.mapToSource(index)
        path = QDir.toNativeSeparators(self._fs_model.filePath(source_index))
        if not path:
            return

        if os.path.isdir(path):
            self._handle_delete_folder(path)
        else:
            self._handle_delete_file(path)

    def set_mindspace(self, path: str) -> None:
        """Set the mindspace root directory."""
        self._mindspace_path = path

        if not path:
            self._filter_model.set_mindspace_root("")
            self._tree_view.configure_for_path("")
            return

        parent_path = os.path.dirname(path)
        self._fs_model.setRootPath(parent_path)
        self._filter_model.set_mindspace_root(path)
        self._tree_view.configure_for_path(path)

        mindspace_source_index = self._fs_model.index(path)
        root_index = self._filter_model.mapFromSource(mindspace_source_index)
        self._tree_view.setRootIndex(root_index)

        self._tree_view.header().hideSection(1)
        self._tree_view.header().hideSection(2)
        self._tree_view.header().hideSection(3)

    def _on_tree_clicked(self, index: QModelIndex) -> None:
        """Handle click events."""
        source_index = self._filter_model.mapToSource(index)
        path = QDir.toNativeSeparators(self._fs_model.filePath(source_index))
        if not path:
            return
        self.file_clicked.emit(MindspaceViewType.FILES, path, True)

    def _on_tree_double_clicked(self, index: QModelIndex) -> None:
        """Handle double click events."""
        source_index = self._filter_model.mapToSource(index)
        path = QDir.toNativeSeparators(self._fs_model.filePath(source_index))
        if not path:
            return
        self.file_clicked.emit(MindspaceViewType.FILES, path, False)

    def _on_language_changed(self) -> None:
        """Update when the language changes."""
        self._header.set_title(self._language_manager.strings().mindspace_files)
        self.apply_style()

    def apply_style(self) -> None:
        """Update styling when application style changes."""
        zoom_factor = self._style_manager.zoom_factor()
        base_font_size = self._style_manager.base_font_size()

        self._header.apply_style()

        self._icon_provider.update_icons()
        self._fs_model.setIconProvider(self._icon_provider)
        file_icon_size = round(16 * zoom_factor)
        self._tree_view.setIconSize(QSize(file_icon_size, file_icon_size))

        font = self.font()
        font.setPointSizeF(base_font_size * zoom_factor)
        self.setFont(font)
        self._tree_view.setFont(font)

        self._tree_view.setIndentation(file_icon_size)
