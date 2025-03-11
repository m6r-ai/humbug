import asyncio
from datetime import datetime
import logging
import os
from typing import Optional, Dict, List, cast
import uuid

from PySide6.QtWidgets import QTabBar, QWidget, QVBoxLayout, QStackedWidget
from PySide6.QtCore import Signal

from humbug.ai.ai_conversation_settings import AIConversationSettings
from humbug.gui.color_role import ColorRole
from humbug.gui.conversation.conversation_error import ConversationError
from humbug.gui.conversation.conversation_tab import ConversationTab
from humbug.gui.editor.editor_tab import EditorTab
from humbug.gui.terminal.terminal_tab import TerminalTab
from humbug.gui.message_box import MessageBox, MessageBoxType
from humbug.gui.status_message import StatusMessage
from humbug.gui.style_manager import StyleManager
from humbug.gui.tab_base import TabBase
from humbug.gui.tab_column import TabColumn
from humbug.gui.tab_column_splitter import TabColumnSplitter
from humbug.gui.tab_label import TabLabel
from humbug.gui.tab_state import TabState
from humbug.gui.tab_type import TabType
from humbug.gui.welcome_widget import WelcomeWidget
from humbug.language.language_manager import LanguageManager
from humbug.mindspace.mindspace_manager import MindspaceManager


class TabData:
    """Encapsulates data related to a tab."""
    def __init__(self, tab: TabBase, title: str):
        """
        Initialize tab data.

        Args:
            tab: The tab widget
            title: Initial title for the tab
        """
        self.tab = tab
        self.tab_id = tab.tab_id
        self.label = TabLabel(title, self.tab_id)


class TabManager(QWidget):
    """Manages multiple tabs across one or two columns."""

    status_message = Signal(StatusMessage)

    def __init__(self, parent=None):
        """Initialize the tab manager."""
        super().__init__(parent)

        self._untitled_count = 0
        self._mindspace_manager = MindspaceManager()
        self._logger = logging.getLogger("TabManager")

        self._language_manager = LanguageManager()

        # Create main layout
        main_layout = QVBoxLayout(self)
        main_layout.setContentsMargins(0, 0, 0, 0)
        main_layout.setSpacing(0)

        # Create stack widget for tab container and welcome message
        self._stack = QStackedWidget()
        main_layout.addWidget(self._stack)

        # Create welcome widget
        self._welcome_widget = WelcomeWidget()
        self._welcome_widget.file_dropped.connect(self._handle_welcome_file_drop)
        self._stack.addWidget(self._welcome_widget)

        # Create widget to hold columns
        self._columns_widget = QWidget()
        self._columns_layout = QVBoxLayout(self._columns_widget)
        self._columns_layout.setContentsMargins(0, 0, 0, 0)
        self._columns_layout.setSpacing(0)
        self._stack.addWidget(self._columns_widget)

        # Create splitter for columns
        self._column_splitter = TabColumnSplitter()
        self._column_splitter.setHandleWidth(1)
        self._columns_layout.addWidget(self._column_splitter)

        # Connect to the splitter's moved signal
        self._column_splitter.splitterMoved.connect(self._handle_splitter_moved)

        # Create initial column
        self._tab_columns: List[TabColumn] = []
        self._create_column(0)

        # Track active column
        self._active_column = self._tab_columns[0]

        # Set initial state
        self._stack.setCurrentWidget(self._welcome_widget)

        # Track tabs
        self._tabs: Dict[str, TabBase] = {}
        self._tab_labels: Dict[str, TabLabel] = {}

        self._style_manager = StyleManager()
        self._style_manager.style_changed.connect(self._handle_style_changed)

        self._handle_style_changed()

    def _create_tab_data(self, tab: TabBase, title: str) -> TabData:
        """
        Create TabData instance and connect signals.

        Args:
            tab: The tab widget to add
            title: Initial title for the tab

        Returns:
            TabData instance for the tab
        """
        data = TabData(tab, title)
        data.label.close_clicked.connect(lambda: self._close_tab_by_id(data.tab_id))
        tab.activated.connect(lambda: self._handle_tab_activated(tab))
        return data

    def _remove_tab_from_column(self, tab: TabBase, column: TabColumn) -> None:
        """
        Remove a tab from a column and clean up associated data.

        Args:
            tab: Tab to remove
            column: Column containing the tab
        """
        tab_id = tab.tab_id
        tab_label = self._tab_labels.pop(tab_id)
        del self._tabs[tab_id]
        index = column.indexOf(tab)
        column.removeTab(index)
        tab_label.deleteLater()
        tab.deleteLater()

    def _add_tab_to_column(self, tab_data: TabData, column: TabColumn) -> None:
        """
        Add a tab to a column and set up associated data.

        Args:
            tab_data: TabData instance
            column: Target column
        """
        self._tabs[tab_data.tab_id] = tab_data.tab
        self._tab_labels[tab_data.tab_id] = tab_data.label

        index = column.addTab(tab_data.tab, "")
        column.tabBar().setTabButton(index, QTabBar.LeftSide, tab_data.label)
        column.setCurrentWidget(tab_data.tab)

    def _move_tab_between_columns(
        self,
        tab: TabBase,
        source_column: TabColumn,
        target_column: TabColumn
    ) -> None:
        """
        Move a tab from one column to another.

        Args:
            tab: Tab to move
            source_column: Source column
            target_column: Target column
        """
        # Save tab state before removal
        tab_state = tab.get_state(True)
        tab_id = tab.tab_id
        tab_title = self._tab_labels[tab_id].text()

        # Remove from source
        self._remove_tab_from_column(tab, source_column)

        # Create and add new tab
        new_tab = self._restore_tab_from_state(tab_state)
        if not new_tab:
            return

        tab_data = self._create_tab_data(new_tab, tab_title)
        self._add_tab_to_column(tab_data, target_column)

    def _handle_welcome_file_drop(self, file_path: str) -> None:
        """Handle file drops when only welcome widget is visible."""
        # Create first column if it doesn't exist
        if not self._tab_columns:
            self._create_column(0)

        # Set as active column
        self._active_column = self._tab_columns[0]

        # Handle the file drop
        ext = os.path.splitext(file_path)[1].lower()
        if ext == '.conv':
            conversation_tab = self.open_conversation(file_path)
            if conversation_tab:
                self._stack.setCurrentWidget(self._columns_widget)
        else:
            editor_tab = self.open_file(file_path)
            if editor_tab:
                self._stack.setCurrentWidget(self._columns_widget)

    def _handle_splitter_moved(self, _pos: int, _index: int):
        """Handle splitter movement and potential column merging."""
        sizes = self._column_splitter.sizes()
        min_width = 200  # Minimum width before merging

        # Find any columns that are too small
        for i, size in enumerate(sizes):
            if size < min_width:
                # Determine which column to merge with
                merge_target = i - 1 if i > 0 else i + 1
                if 0 <= merge_target < len(self._tab_columns):
                    source_column = self._tab_columns[i]
                    target_column = self._tab_columns[merge_target]

                    # If the source column was active, make the target column active
                    if self._active_column == source_column:
                        self._active_column = target_column

                    # Move all tabs from source to target
                    while source_column.count() > 0:
                        tab = source_column.widget(0)
                        self._move_tab_between_columns(tab, source_column, target_column)

                    # Remove the empty column
                    self._remove_column_and_resize(i, source_column)

                    # Update tab highlighting
                    self._update_tabs()
                    break

    def _handle_tab_drop(self, tab_id: str, target_column: TabColumn, target_index: int) -> None:
        """
        Handle a tab being dropped into a new position.

        Args:
            tab_id: ID of the tab being moved
            target_column: Column where the tab was dropped
            target_index: Target position in the column
        """
        tab = self._tabs.get(tab_id)
        if not tab:
            return

        source_column = self._find_column_for_tab(tab)
        if not source_column:
            return

        source_index = source_column.indexOf(tab)

        # Don't process if dropped on itself
        if (source_column == target_column and
            (source_index == target_index or source_index == target_index - 1)):
            return

        self._move_tab_between_columns(tab, source_column, target_column)

        # Set our new active column before we possibly delete the previous one
        self._active_column = target_column
        if source_column.count() == 0:
            column_number = self._tab_columns.index(source_column)
            self._remove_column_and_resize(column_number, source_column)

        # Update active states
        self._update_tabs()

    def _handle_file_drop(self, file_path: str, target_column: TabColumn, target_index: int) -> None:
        """
        Handle a file being dropped into a column.

        Args:
            file_path: Path to the dropped file
            target_column: Column where the file was dropped
            target_index: Target position in the column
        """
        # Check file extension
        ext = os.path.splitext(file_path)[1].lower()

        # Set the target column as active
        self._active_column = target_column

        try:
            if ext == '.conv':
                # Open conversation file
                conversation_tab = self.open_conversation(file_path)
                if conversation_tab:
                    # Move the tab to the target position if not already there
                    current_index = target_column.indexOf(conversation_tab)
                    if current_index != target_index:
                        target_column.tabBar().moveTab(current_index, target_index)
            else:
                # Open regular file
                editor_tab = self.open_file(file_path)
                if editor_tab:
                    # Move the tab to the target position if not already there
                    current_index = target_column.indexOf(editor_tab)
                    if current_index != target_index:
                        target_column.tabBar().moveTab(current_index, target_index)

        except (ConversationError, OSError) as e:
            self._logger.exception("Failed to open dropped file '%s': %s", file_path, str(e))

    def handle_file_rename(self, old_path: str, new_path: str):
        """Handle renaming of files by updating any open tabs.

        Args:
            old_path: Original path of renamed file
            new_path: New path after renaming
        """
        # Find any editor tab for this file
        editor = self.find_editor_tab_by_filename(old_path)
        if editor:
            editor.set_filename(new_path)

        # For conversations, find by ID and update path
        if old_path.endswith('.conv'):
            old_id = os.path.splitext(os.path.basename(old_path))[0]
            conversation = self.find_conversation_tab_by_id(old_id)
            if conversation:
                # Get new ID
                new_id = os.path.splitext(os.path.basename(new_path))[0]

                # Update manager's data structures
                self._tabs[new_id] = self._tabs.pop(old_id)
                self._tab_labels[new_id] = self._tab_labels.pop(old_id)

                # Update tab label text
                label = self._tab_labels[new_id]
                label.update_id(new_id, f"Conv: {new_id}")
                label.close_clicked.connect(lambda: self._close_tab_by_id(new_id))

                # Update conversation internals without signaling
                conversation.update_path(new_id, new_path)

    def _create_column(self, index: int) -> TabColumn:
        """Create a new tab column."""
        tab_widget = TabColumn()
        tab_widget.setMinimumWidth(200)  # Set minimum width
        tab_widget.currentChanged.connect(self._handle_tab_changed)
        tab_widget.column_activated.connect(self._handle_column_activated)
        tab_widget.tab_drop.connect(self._handle_tab_drop)
        tab_widget.file_drop.connect(self._handle_file_drop)

        self._column_splitter.insertWidget(index, tab_widget)
        self._tab_columns.insert(index, tab_widget)

        return tab_widget

    def _handle_tab_merge(self, dragged_tab_id: str, target_tab_id: str) -> None:
        """Handle merging tabs when one is dropped directly onto another."""
        dragged_tab = self._tabs.get(dragged_tab_id)
        target_tab = self._tabs.get(target_tab_id)

        if not dragged_tab or not target_tab:
            return

        source_column = self._find_column_for_tab(dragged_tab)
        target_column = self._find_column_for_tab(target_tab)

        if not source_column or not target_column:
            return

        # Get the target index
        target_index = target_column.indexOf(target_tab)
        if target_index == -1:
            return

        # Move the tab
        self._move_tab_between_columns(dragged_tab, source_column, target_column)

        # If the source column is now empty, remove it
        if source_column.count() == 0:
            column_number = self._tab_columns.index(source_column)
            self._remove_column_and_resize(column_number, source_column)

        # Update states
        self._active_column = target_column
        self._update_tabs()

    def _remove_column_and_resize(self, column_number: int, column: TabColumn) -> None:
        """
        Remove a column and resize the remaining columns.

        Args:
            column_number: Index of the column to remove
            column: Column widget to remove
        """
        del self._tab_columns[column_number]
        column.deleteLater()

        # Resize splitter to evenly distribute space
        if self._tab_columns:
            width = self.width()
            column_width = width // len(self._tab_columns)

            # Note: We add 1 to column count because deletion hasn't processed yet
            self._column_splitter.setSizes([column_width] * (len(self._tab_columns) + 1))

    def _update_tabs(self) -> None:
        # Update current states for all tabs
        for tab_id, label in self._tab_labels.items():
            tab = self._tabs[tab_id]
            column = self._find_column_for_tab(tab)
            column_index = column.currentIndex()
            is_current = column_index != -1 and tab == column.widget(column_index)
            label.set_current(is_current, is_current and column == self._active_column)

        # Force style refresh to show active state
        self._handle_style_changed()

        # Emit our new signal with current tab
        current_tab = self._get_current_tab()
        if not current_tab:
            return

        # Disconnect any existing connections to avoid duplicates
        try:
            current_tab.status_message.disconnect()
        except RuntimeError:
            pass  # No existing connections

        current_tab.status_message.connect(self.status_message)
        current_tab.update_status()
        current_tab.setFocus()

    def _handle_tab_changed(self, _index: int) -> None:
        """
        Handle tab selection changes.

        Args:
            index: Index of the newly selected tab
        """
        # Find which column triggered the change
        sender = self.sender()
        self._active_column = sender
        self._update_tabs()

    def _handle_tab_activated(self, tab: TabBase) -> None:
        """
        Handle tab activation from widget focus.

        Args:
            tab: The tab that was activated
        """
        # Find which column contains the tab
        column = self._find_column_for_tab(tab)
        if not column or column == self._active_column:
            return

        # Update active column
        self._active_column = column
        self._update_tabs()

    def _handle_column_activated(self, column: TabColumn) -> None:
        """Handle column activation."""
        if column not in self._tab_columns:
            return

        if column == self._active_column:
            return

        self._active_column = column
        self._update_tabs()

    def add_tab(self, tab: TabBase, title: str) -> None:
        """
        Add a new tab to the manager.

        Args:
            tab: The tab widget to add
            title: Initial title for the tab
        """
        tab_data = self._create_tab_data(tab, title)
        self._add_tab_to_column(tab_data, self._active_column)

        # Set initial state
        if len(self._tabs) == 1:  # If this is the first tab
            tab_data.label.set_current(True, True)
            self._stack.setCurrentWidget(self._columns_widget)

    def _close_tab_by_id(self, tab_id: str, force_close: bool=False) -> None:
        """
        Close a tab by its ID.

        Args:
            tab_id: ID of the tab to close
            force_close: Whether to force close without checks
        """
        tab = self._tabs.get(tab_id)
        if not tab:
            return

        # Check if tab can be closed
        if not force_close and not tab.can_close():
            return

        tab.close()

        # Find which column contains the tab
        column = self._find_column_for_tab(tab)
        if not column:
            return

        self._remove_tab_from_column(tab, column)
        # If we closed the last tab in the column, close the column unless it's the last column
        if column.count() == 0:
            if len(self._tab_columns) > 1:
                column_number = self._tab_columns.index(column)
                if self._active_column == column:
                    new_active_column = 1 if column_number == 0 else column_number - 1
                    self._active_column = self._tab_columns[new_active_column]

                self._remove_column_and_resize(column_number, column)

                self._update_tabs()

        # If no tabs remain clean up the display
        if not self._tabs:
            self.status_message.emit(StatusMessage(""))
            self._stack.setCurrentWidget(self._welcome_widget)

    def _find_column_for_tab(self, tab: TabBase) -> Optional[TabColumn]:
        """Find which column contains the given tab."""
        for column in self._tab_columns:
            if column.indexOf(tab) != -1:
                return column

        return None

    def _get_current_tab(self) -> Optional[TabBase]:
        """
        Get the currently active tab.

        Returns:
            The current tab or None if no tabs exist
        """
        widget = self._active_column.currentWidget()
        return cast(TabBase, widget) if widget else None

    def _set_current_tab(self, tab_id: str) -> None:
        """
        Set the current tab by ID.

        Args:
            tab_id: ID of the tab to make current
        """
        tab = self._tabs.get(tab_id)
        if not tab:
            return

        # Find which column contains the tab
        column = self._find_column_for_tab(tab)
        if column:
            column.setCurrentWidget(tab)
            self._active_column = column

    def _handle_tab_title_changed(self, tab_id: str, title: str) -> None:
        """
        Update a tab's title.

        Args:
            tab_id: ID of the tab to update
            title: New title for the tab
        """
        label = self._tab_labels.get(tab_id)
        if label:
            label.update_text(title)
            self.adjustSize()

    def _handle_tab_modified(self, tab_id: str, modified: bool) -> None:
        """
        Update a tab's modified state.

        Args:
            tab_id: ID of the tab to update
            modified: Whether the tab is modified
        """
        tab = self._tabs.get(tab_id)
        if not tab:
            return

        label = self._tab_labels.get(tab_id)
        if label:
            current_text = label.text()
            if modified and not current_text.endswith('*'):
                label.update_text(f"{current_text}*")
            elif not modified and current_text.endswith('*'):
                label.update_text(current_text[:-1])

    def can_split_column(self) -> bool:
        """Can the current column be split in two?"""
        if len(self._tab_columns) >= 6:
            return False

        current_column_number = self._get_current_column()
        current_column = self._tab_columns[current_column_number]
        return current_column.count() > 1

    def split_column(self, split_left: bool) -> None:
        """Split the current column in two."""
        if len(self._tab_columns) >= 6:
            return

        current_column_number = self._get_current_column()
        current_column = self._tab_columns[current_column_number]
        if current_column.count() <= 1:
            return

        target_column_number = current_column_number + (0 if split_left else 1)
        target_column = self._create_column(target_column_number)

        # Move the current tab to the new column
        current_tab = self._get_current_tab()
        if not current_tab:
            return

        self._move_tab_between_columns(current_tab, current_column, target_column)

        # Resize splitter
        num_columns = len(self._tab_columns)
        sizes = [(self.width() // num_columns) for _ in range(num_columns)]
        self._column_splitter.setSizes(sizes)

        self._active_column = target_column

        # Emit signal about column state change
        self._update_tabs()

    def can_merge_column(self, merge_left: bool) -> bool:
        """Can the current column be merged?"""
        if len(self._tab_columns) <= 1:
            return False

        current_column_number = self._get_current_column()
        if (merge_left and current_column_number == 0) or (not merge_left and current_column_number == len(self._tab_columns) -1):
            return False

        return True

    def merge_column(self, merge_left: bool) -> None:
        """Merge with adjacent column."""
        if len(self._tab_columns) <= 1:
            return

        current_column_number = self._get_current_column()
        if (merge_left and current_column_number == 0) or (not merge_left and current_column_number == len(self._tab_columns) -1):
            return

        target_column_number = current_column_number + (-1 if merge_left else 1)
        target_column = self._tab_columns[target_column_number]
        current_column = self._active_column

        # Move all tabs to target column
        while current_column.count() > 0:
            tab = current_column.widget(0)
            self._move_tab_between_columns(tab, current_column, target_column)

        self._active_column = target_column
        column_number = self._tab_columns.index(current_column)
        self._remove_column_and_resize(column_number, current_column)

        # Emit signal about column state change
        self._update_tabs()

    def can_swap_column(self, swap_left: bool) -> bool:
        """
        Check if the current column can be swapped with the column to its left or right.

        Args:
            swap_left: If True, try to swap with the column to the left.
                    If False, try to swap with the column to the right.

        Returns:
            bool: True if the column can be swapped, False otherwise
        """
        if len(self._tab_columns) <= 1:
            return False

        current_column_number = self._get_current_column()

        if swap_left and current_column_number == 0:
            return False

        if not swap_left and current_column_number == len(self._tab_columns) - 1:
            return False

        return True

    def swap_column(self, swap_left: bool) -> None:
        """
        Swap the current column with the column to its left or right.

        Args:
            swap_left: If True, swap with the column to the left.
                    If False, swap with the column to the right.
        """
        if len(self._tab_columns) <= 1:
            return

        current_column_number = self._get_current_column()

        if swap_left and current_column_number == 0:
            return

        if not swap_left and current_column_number == len(self._tab_columns) - 1:
            return

        target_column_number = current_column_number + (-1 if swap_left else 1)

        # Get the source and target columns
        source_column = self._tab_columns[current_column_number]
        target_column = self._tab_columns[target_column_number]

        # Create temporary widgets to help with swapping
        temp_source = QWidget()
        temp_target = QWidget()

        # Replace the source and target columns with temporary widgets
        index_source = self._column_splitter.indexOf(source_column)
        index_target = self._column_splitter.indexOf(target_column)

        self._column_splitter.replaceWidget(index_source, temp_source)
        self._column_splitter.replaceWidget(index_target, temp_target)

        # Now swap the actual columns in the splitter
        self._column_splitter.replaceWidget(index_source, target_column)
        self._column_splitter.replaceWidget(index_target, source_column)

        # Update the _tab_columns list to reflect the swap
        self._tab_columns[current_column_number], self._tab_columns[target_column_number] = \
            self._tab_columns[target_column_number], self._tab_columns[current_column_number]

        # Clean up temporary widgets
        temp_source.deleteLater()
        temp_target.deleteLater()

        # Emit signal about column state change
        self._update_tabs()

    def _get_current_column(self) -> int:
        """Get index of currently active column."""
        return self._tab_columns.index(self._active_column)

    def find_conversation_tab_by_id(self, conversation_id: str) -> Optional[ConversationTab]:
        """
        Find a conversation tab by its conversation ID.

        Args:
            conversation_id: The ID to search for

        Returns:
            The ConversationTab if found, None otherwise
        """
        tab = self._tabs.get(conversation_id)
        return tab if isinstance(tab, ConversationTab) else None

    def find_editor_tab_by_filename(self, filename: str) -> Optional[EditorTab]:
        """
        Find an editor tab by its filename.

        Args:
            filename: The filename to search for

        Returns:
            The EditorTab if found, None otherwise
        """
        for tab in self._tabs.values():
            if isinstance(tab, EditorTab) and tab.filename == filename:
                return tab
        return None

    def new_file(self) -> EditorTab:
        """Create a new empty editor tab."""
        self._untitled_count += 1
        tab_id = str(uuid.uuid4())
        editor = EditorTab(tab_id, self)
        editor.set_filename(None, self._untitled_count)

        self._connect_editor_signals(editor)
        self.add_tab(editor, f"Untitled-{self._untitled_count}")
        return editor

    def open_file(self, path: str) -> Optional[EditorTab]:
        """Open a file in a new or existing editor tab."""
        # Check if file is already open
        existing_tab = self.find_editor_tab_by_filename(path)
        if existing_tab:
            self._set_current_tab(existing_tab.tab_id)
            return existing_tab

        tab_id = str(uuid.uuid4())
        editor = EditorTab(tab_id, self)
        editor.set_filename(path)

        self._connect_editor_signals(editor)
        self.add_tab(editor, os.path.basename(path))
        return editor

    def new_conversation(self, mindspace_path: str) -> Optional[str]:
        """Create a new conversation tab and return its ID."""
        # Generate timestamp for ID
        timestamp = datetime.utcnow()
        conversation_id = timestamp.strftime("%Y-%m-%d-%H-%M-%S-%f")[:23]

        # Create path relative to mindspace
        filename = os.path.join("conversations", f"{conversation_id}.conv")
        full_path = os.path.join(mindspace_path, filename)

        conversation_tab = ConversationTab(
            conversation_id,
            full_path,
            timestamp,
            self
        )
        conversation_tab.forkRequested.connect(self._fork_conversation)

        # Set model based on mindspace settings
        settings = self._mindspace_manager.settings
        conversation_settings = AIConversationSettings(
            model=settings.model,
            temperature=settings.temperature if AIConversationSettings.supports_temperature(settings.model) else None,
            reasoning=settings.reasoning
        )
        conversation_tab.update_conversation_settings(conversation_settings)

        self.add_tab(conversation_tab, f"Conv: {conversation_id}")
        return conversation_id

    def open_conversation(self, path: str) -> Optional[ConversationTab]:
        """Open an existing conversation file."""
        conversation_id = os.path.splitext(os.path.basename(path))[0]

        # Check if already open
        existing_tab = self.find_conversation_tab_by_id(conversation_id)
        if existing_tab:
            self._set_current_tab(conversation_id)
            return existing_tab

        try:
            conversation_tab = ConversationTab.load_from_file(
                path,
                self
            )
            conversation_tab.forkRequested.connect(self._fork_conversation)
            self.add_tab(conversation_tab, f"Conv: {conversation_id}")
            return conversation_tab

        except ConversationError as e:
            self._logger.exception("Failed to open conversation: %s", str(e))
            raise

    def can_fork_conversation(self) -> bool:
        tab = self._get_current_tab()
        if not tab or not isinstance(tab, ConversationTab):
            return False

        return True

    async def fork_conversation(self) -> None:
        """Fork an existing conversation into a new tab."""
        conversation_tab = self._get_current_tab()
        if not isinstance(conversation_tab, ConversationTab):
            return

        try:
            # Fork the conversation
            new_tab = await conversation_tab.fork_conversation()

            # Add new tab to manager
            self.add_tab(new_tab, f"Conv: {new_tab.tab_id}")

        except ConversationError as e:
            self._logger.exception("Failed to fork conversation: %s", str(e))
            raise

    def _fork_conversation(self):
        """Create a new conversation tab with the history of the current conversation."""
        async def fork_and_handle_errors():
            try:
                await self.fork_conversation()
            except ConversationError as e:
                strings = self._language_manager.strings
                MessageBox.show_message(
                    self,
                    MessageBoxType.CRITICAL,
                    strings.conversation_error_title,
                    strings.error_forking_conversation.format(str(e))
                )

        # Create task to fork conversation
        asyncio.create_task(fork_and_handle_errors())

    def new_terminal(self, command: Optional[str] = None) -> TerminalTab:
        """Create new terminal tab.

        Args:
            command: Optional command to run in terminal

        Returns:
            Created terminal tab
        """
        tab_id = str(uuid.uuid4())
        terminal = TerminalTab(tab_id, command, self)

        if command:
            title = f"Term: {os.path.basename(command)}"
        else:
            title = "Terminal"

        self.add_tab(terminal, title)
        return terminal

    def save_state(self) -> Dict:
        """Get current state of all tabs."""
        tab_columns = []
        for column in self._tab_columns:
            tab_states = []
            for index in range(column.count()):
                tab = column.widget(index)
                try:
                    state = tab.get_state(False)
                    state_dict = state.to_dict()
                    tab_states.append(state_dict)
                except Exception as e:
                    self._logger.exception("Failed to save tab manager state: %s", str(e))
                    continue
            tab_columns.append(tab_states)

        return {
            'columns': tab_columns,
        }

    def restore_state(self, saved_state: Dict) -> None:
        """Restore tabs from saved state."""
        saved_columns = saved_state.get('columns', [])

        num_columns = len(saved_columns)
        for index in range(1, num_columns):
            self._create_column(index)

        for column_index, tab_state in enumerate(saved_columns):
            self._restore_column_state(column_index, tab_state)

        self.show_all_columns()

    def _restore_column_state(self, column_index: int, tab_states: List[Dict]) -> None:
        """Restore state for a single column of tabs."""
        for state_dict in tab_states:
            try:
                state = TabState.from_dict(state_dict)

                if not os.path.isabs(state.path):
                    state.path = self._mindspace_manager.get_mindspace_path(state.path)

                tab = self._restore_tab_from_state(state)
                if not tab:
                    continue

                self._active_column = self._tab_columns[column_index]
                title = self._get_tab_title(tab, state)
                self.add_tab(tab, title)

            except Exception as e:
                self._logger.exception("Failed to restore tab manager state: %s", str(e))
                continue

    def _restore_tab_from_state(self, state: TabState) -> Optional[TabBase]:
        """Create appropriate tab type from state."""
        if state.type == TabType.CONVERSATION:
            tab = ConversationTab.restore_from_state(state, self)
            tab.forkRequested.connect(self._fork_conversation)
            return tab

        if state.type == TabType.EDITOR:
            tab = EditorTab.restore_from_state(state, self)
            self._connect_editor_signals(tab)
            return tab

        if state.type == TabType.TERMINAL:
            return TerminalTab.restore_from_state(state, self)

        return None

    def _connect_editor_signals(self, editor: EditorTab) -> None:
        """Connect standard editor tab signals."""
        editor.title_changed.connect(self._handle_tab_title_changed)
        editor.modified_state_changed.connect(self._handle_tab_modified)

    def _get_tab_title(self, tab: TabBase, state: TabState) -> str:
        """Get appropriate title for tab type."""
        if isinstance(tab, ConversationTab):
            return f"Conv: {tab.tab_id}"

        if isinstance(tab, TerminalTab):
            if state.metadata and "command" in state.metadata:
                return f"Term: {os.path.basename(state.metadata['command'])}"

            return "Terminal"

        return os.path.basename(state.path)

    def _handle_style_changed(self) -> None:
        """
        Handle style changes from StyleManager.
        """
        for column in self._tab_columns:
            selected_border = ColorRole.TAB_BORDER_ACTIVE if column == self._active_column else ColorRole.TAB_BACKGROUND_ACTIVE

            style = f"""
                QTabBar::tab {{
                    background: {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_INACTIVE)};
                    border: none;
                    margin-right: 2px;
                    border-bottom: 1px solid {self._style_manager.get_color_str(ColorRole.BACKGROUND_PRIMARY)};
                }}
                QTabBar::tab:selected {{
                    background: {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
                    border-top: 2px solid {self._style_manager.get_color_str(selected_border)};
                    border-bottom: none;
                }}
                QTabBar::tab:hover {{
                    background: {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_HOVER)};
                }}
                QTabBar::scroller {{
                    width: 20px;
                }}
                QTabBar QToolButton {{
                    background-color: {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_INACTIVE)};
                    border: none;
                }}
                QTabBar QToolButton::right-arrow {{
                    image: url({self._style_manager.get_icon_path('arrow-right')});
                    width: 12px;
                    height: 12px;
                }}
                QTabBar QToolButton::left-arrow {{
                    image: url({self._style_manager.get_icon_path('arrow-left')});
                    width: 12px;
                    height: 12px;
                }}
            """
            column.setStyleSheet(style)

        # Update all tab labels, setting active state only for the current active tab.
        for tab_id, label in self._tab_labels.items():
            tab = self._tabs[tab_id]
            column = self._find_column_for_tab(tab)
            is_label_active = column == self._active_column and tab == column.currentWidget()
            label.handle_style_changed(is_label_active)

        self._column_splitter.setStyleSheet(f"""
            QSplitter::handle {{
                background-color: {self._style_manager.get_color_str(ColorRole.SPLITTER)};
                margin: 0;
                width: 1px;
            }}
        """)

    def can_undo(self) -> bool:
        tab = self._get_current_tab()
        return False if not tab else tab.can_undo()

    def undo(self):
        self._get_current_tab().undo()

    def can_redo(self) -> bool:
        tab = self._get_current_tab()
        return False if not tab else tab.can_redo()

    def redo(self):
        self._get_current_tab().redo()

    def can_cut(self) -> bool:
        tab = self._get_current_tab()
        return False if not tab else tab.can_cut()

    def cut(self):
        self._get_current_tab().cut()

    def can_copy(self) -> bool:
        tab = self._get_current_tab()
        return False if not tab else tab.can_copy()

    def copy(self):
        self._get_current_tab().copy()

    def can_paste(self) -> bool:
        tab = self._get_current_tab()
        return False if not tab else tab.can_paste()

    def paste(self):
        self._get_current_tab().paste()

    def can_find(self) -> bool:
        tab = self._get_current_tab()
        return tab is not None

    def find(self):
        tab = self._get_current_tab()
        tab.show_find()

    def close_deleted_file(self, path: str):
        """
        Close any open tabs related to a file being deleted.

        Args:
            path: Path of file being deleted
        """
        # Find and close any editor tab for this file
        editor = self.find_editor_tab_by_filename(path)
        if editor:
            self._close_tab_by_id(editor.tab_id, True)

        # Also check for conversation files
        if path.endswith('.conv'):
            conversation_id = os.path.splitext(os.path.basename(path))[0]
            conversation = self.find_conversation_tab_by_id(conversation_id)
            if conversation:
                self._close_tab_by_id(conversation.tab_id, True)

    def can_close_all_tabs(self) -> bool:
        """Can we close all the tabs that are open?"""
        all_tabs = list(self._tabs.values())
        for tab in all_tabs:
            if tab.is_modified and not tab.can_close():
                return False

        return True

    def close_all_tabs(self) -> None:
        """Close all open tabs."""
        all_tabs = list(self._tabs.values())
        for tab in all_tabs:
            self._close_tab_by_id(tab.tab_id)

    def can_close_tab(self) -> bool:
        tab = self._get_current_tab()
        return False if not tab else True

    def close_tab(self) -> None:
        """Close the currently active tab."""
        tab = self._get_current_tab()
        if not tab:
            return

        self._close_tab_by_id(tab.tab_id)

    def can_save_file(self) -> bool:
        tab = self._get_current_tab()
        return False if not tab else tab.can_save()

    def save_file(self) -> None:
        """Save the current file."""
        current_tab = self._get_current_tab()
        if isinstance(current_tab, EditorTab):
            current_tab.save()

    def can_save_file_as(self) -> bool:
        tab = self._get_current_tab()
        return False if not tab else tab.can_save_as()

    def save_file_as(self) -> None:
        """Save the current file with a new name."""
        current_tab = self._get_current_tab()
        if isinstance(current_tab, EditorTab):
            current_tab.save_as()

    def can_show_all_columns(self) -> bool:
        return False if len(self._tab_columns) == 0 else True

    def show_all_columns(self) -> None:
        if len(self._tab_columns) < 1:
            return

        num_columns = len(self._tab_columns)
        sizes = [(self.width() // num_columns) for _ in range(num_columns)]
        self._column_splitter.setSizes(sizes)

    def can_submit_message(self) -> bool:
        tab = self._get_current_tab()
        return False if not tab else tab.can_submit()

    def submit_message(self) -> None:
        """Handle message submission."""
        tab = self._get_current_tab()
        if not tab or not tab.can_submit():
            return

        tab.submit()

    def can_show_conversation_settings_dialog(self) -> bool:
        tab = self._get_current_tab()
        if not tab or not isinstance(tab, ConversationTab):
            return False

        return True

    def show_conversation_settings_dialog(self) -> None:
        """Show the conversation settings dialog."""
        tab = self._get_current_tab()
        if not tab or not isinstance(tab, ConversationTab):
            return

        tab.show_conversation_settings_dialog()

    def handle_esc_key(self) -> bool:
        """Handle processing of the "Esc" key."""
        tab = self._get_current_tab()
        if not tab or not isinstance(tab, ConversationTab):
            return False

        tab.cancel_current_tasks()
        return True

    def can_toggle_bookmark(self) -> bool:
        """Can we toggle a bookmark?"""
        tab = self._get_current_tab()
        if not tab or not isinstance(tab, ConversationTab):
            return False

        return tab.can_toggle_bookmark()

    def is_checked_bookmark(self) -> bool:
        """Is the current bookmark set (checked)?"""
        tab = self._get_current_tab()
        if not tab or not isinstance(tab, ConversationTab):
            return False

        return tab.is_checked_bookmark()

    def toggle_bookmark(self) -> None:
        """Handle toggling a bookmark."""
        tab = self._get_current_tab()
        if not tab or not isinstance(tab, ConversationTab):
            return

        tab.toggle_bookmark()

    def can_next_bookmark(self) -> bool:
        """Can we move to the next bookmark?"""
        tab = self._get_current_tab()
        if not tab or not isinstance(tab, ConversationTab):
            return False

        return tab.can_next_bookmark()

    def next_bookmark(self) -> None:
        """Handle navigating to the next bookmark."""
        tab = self._get_current_tab()
        if not tab or not isinstance(tab, ConversationTab):
            return

        tab.next_bookmark()

    def can_previous_bookmark(self) -> bool:
        """Can we move to the next bookmark?"""
        tab = self._get_current_tab()
        if not tab or not isinstance(tab, ConversationTab):
            return False

        return tab.can_previous_bookmark()

    def previous_bookmark(self) -> None:
        """Handle navigating to the next bookmark."""
        tab = self._get_current_tab()
        if not tab or not isinstance(tab, ConversationTab):
            return

        tab.previous_bookmark()
