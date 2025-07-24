from datetime import datetime, timezone
import logging
import os
from typing import Dict, List, cast

from PySide6.QtWidgets import QTabBar, QWidget, QVBoxLayout, QStackedWidget, QApplication
from PySide6.QtCore import Signal, QTimer

from ai.ai_conversation_settings import AIConversationSettings
from ai.ai_model import ReasoningCapability

from humbug.color_role import ColorRole
from humbug.column_splitter import ColumnSplitter
from humbug.column_widget import ColumnWidget
from humbug.message_box import MessageBox, MessageBoxType
from humbug.language.language_manager import LanguageManager
from humbug.mindspace.mindspace_log_level import MindspaceLogLevel
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.mindspace.mindspace_settings import MindspaceSettings
from humbug.status_message import StatusMessage
from humbug.style_manager import StyleManager
from humbug.tabs.conversation.conversation_error import ConversationError
from humbug.tabs.conversation.conversation_tab import ConversationTab
from humbug.tabs.editor.editor_tab import EditorTab
from humbug.tabs.log.log_tab import LogTab
from humbug.tabs.shell.shell_tab import ShellTab
from humbug.tabs.tab_bar import TabBar
from humbug.tabs.tab_base import TabBase
from humbug.tabs.tab_label import TabLabel
from humbug.tabs.tab_state import TabState
from humbug.tabs.tab_type import TabType
from humbug.tabs.terminal.terminal_tab import TerminalTab
from humbug.tabs.wiki.wiki_error import WikiError
from humbug.tabs.wiki.wiki_tab import WikiTab
from humbug.welcome_widget import WelcomeWidget


class TabData:
    """Encapsulates data related to a tab."""
    def __init__(self, tab: TabBase, title: str, tool_tip: str) -> None:
        """
        Initialize tab data.

        Args:
            tab: The tab widget
            title: Initial title for the tab
            tool_tip: Tooltip text for the tab
            ephemeral: Whether the tab is ephemeral (temporary)
        """
        icon = ""
        if isinstance(tab, ConversationTab):
            icon = "conversation"

        elif isinstance(tab, EditorTab):
            icon = "editor"

        elif isinstance(tab, LogTab):
            icon = "log"

        elif isinstance(tab, ShellTab):
            icon = "shell"

        elif isinstance(tab, TerminalTab):
            icon = "terminal"

        elif isinstance(tab, WikiTab):
            icon = "wiki"

        self.tab = tab
        self.tab_id = tab.tab_id()
        self.label = TabLabel(self.tab_id, icon, title, tool_tip)


class ColumnManager(QWidget):
    """Manages multiple tabs across multiple columns."""

    status_message = Signal(StatusMessage)
    tab_changed = Signal()

    def __init__(self, parent: QWidget | None = None) -> None:
        """Initialize the tab manager."""
        super().__init__(parent)

        self._untitled_count = 0
        self._mindspace_manager = MindspaceManager()
        self._logger = logging.getLogger("ColumnManager")

        self._language_manager = LanguageManager()

        # Track MRU order for each column
        self._column_mru_order: Dict[ColumnWidget, List[str]] = {}

        # Create main layout
        main_layout = QVBoxLayout(self)
        main_layout.setContentsMargins(0, 0, 0, 0)
        main_layout.setSpacing(0)

        # Create stack widget for tab container and welcome message
        self._stack = QStackedWidget()
        main_layout.addWidget(self._stack)

        # Create welcome widget
        self._welcome_widget = WelcomeWidget()
        self._welcome_widget.path_dropped.connect(self._handle_welcome_tree_drop)
        self._stack.addWidget(self._welcome_widget)

        # Create widget to hold columns
        self._columns_widget = QWidget()
        self._columns_layout = QVBoxLayout(self._columns_widget)
        self._columns_layout.setContentsMargins(0, 0, 0, 0)
        self._columns_layout.setSpacing(0)
        self._stack.addWidget(self._columns_widget)

        # Create splitter for columns
        self._column_splitter = ColumnSplitter()
        self._column_splitter.setHandleWidth(1)
        self._columns_layout.addWidget(self._column_splitter)

        # Connect to the splitter's moved signal
        self._column_splitter.splitterMoved.connect(self._handle_splitter_moved)

        # Create initial column
        self._tab_columns: List[ColumnWidget] = []
        self._create_column(0)

        # Track active column
        self._active_column = self._tab_columns[0]

        # Activation timer used to ensure tabs are activated correctly.  We only allow
        # one activation at a time so we don't get races when we open a new tab but
        # return back to the original.
        self._activation_timer = QTimer(self)
        self._activation_timer.setSingleShot(True)
        self._activation_timer.timeout.connect(self._activate_current_tab)
        self._activation_timer.setInterval(1)

        # Set initial state
        self._stack.setCurrentWidget(self._welcome_widget)

        # Track tabs
        self._tabs: Dict[str, TabBase] = {}
        self._tab_labels: Dict[str, TabLabel] = {}

        # Are we protecting the current tab against being ovewrwritten?
        self._protect_current_tab = False
        self._protected_tab: TabBase | None = None

        self._style_manager = StyleManager()
        self._style_manager.style_changed.connect(self._handle_style_changed)

        self._current_status_tab: TabBase | None = None

        self._handle_style_changed()

    def protect_current_tab(self, protect: bool) -> None:
        """Set whether to protect the current tab from being overwritten."""
        self._protect_current_tab = protect

        # If protecting, store the current tab
        if protect:
            self._protected_tab = self.get_current_tab()
            return

        # Unprotecting - ensure the protected tab is kept active!
        if not self._protected_tab:
            return

        self._set_current_tab(self._protected_tab, False)
        self._protected_tab = None

    def num_colunns(self) -> int:
        """Get the number of columns currently in use."""
        return len(self._tab_columns)

    def _get_tab_info(self, tab: TabBase) -> Dict[str, str | int | bool]:
        """
        Get detailed information about a specific tab.

        Args:
            tab: The tab we want information about

        Returns:
            Dictionary containing tab information:
            - tab_id: Unique identifier for the tab
            - title: Display title of the tab
            - type: Type of tab (conversation, editor, wiki, etc.)
            - path: File path (if applicable, relative to mindspace)
            - column_index: Index of the column containing the tab
            - is_active: Whether this tab is currently active in its column
            - is_active_column: Whether this tab's column is the active column
            - is_modified: Whether the tab has unsaved changes
            - is_ephemeral: Whether the tab is temporary
        """
        tab_id = tab.tab_id()
        label = self._tab_labels.get(tab_id)

        # Determine tab type
        tab_type = "unknown"
        if isinstance(tab, ConversationTab):
            tab_type = "conversation"

        elif isinstance(tab, EditorTab):
            tab_type = "editor"

        elif isinstance(tab, LogTab):
            tab_type = "log"

        elif isinstance(tab, ShellTab):
            tab_type = "shell"

        elif isinstance(tab, TerminalTab):
            tab_type = "terminal"

        elif isinstance(tab, WikiTab):
            tab_type = "wiki"

        # Get relative path if available
        path = tab.path()
        relative_path = ""
        if path:
            relative_path = self._mindspace_manager.get_relative_path(path)

        # Find the column containing this tab
        column_index = -1
        for index, column in enumerate(self._tab_columns):
            if column.indexOf(tab) != -1:
                column_index = index
                break

        return {
            "tab_id": tab_id,
            "title": label.text() if label else "",
            "type": tab_type,
            "path": relative_path,
            "column_index": column_index,
            "is_modified": tab.is_modified(),
            "is_ephemeral": tab.is_ephemeral()
        }

    def get_tab_info_by_id(self, tab_id: str) -> Dict[str, str | int | bool]:
        """
        Get information about a specific tab or the current tab.

        Args:
            tab_id: ID of the tab to get information for. If None, uses the current tab.

        Returns:
            Dictionary containing tab information
        """
        # Find the tab by ID
        tab = self._tabs.get(tab_id)
        if not tab:
            raise ValueError(f"Tab with ID '{tab_id}' not found")

        return self._get_tab_info(tab)

    def get_tab_by_id(self, tab_id: str) -> TabBase | None:
        """
        Get a tab by its ID.

        Args:
            tab_id: ID of the tab to retrieve

        Returns:
            The TabBase instance for the specified tab ID or None if not found
        """
        tab = self._tabs.get(tab_id)
        if not tab:
            return None

        return tab

    def list_all_tabs(self) -> List[Dict[str, str | int | bool]]:
        """
        Get information about all currently open tabs across all columns.

        Returns:
            List of dictionaries containing tab information
        """
        tab_info = []

        for column in self._tab_columns:
            for tab_index in range(column.count()):
                tab = cast(TabBase, column.widget(tab_index))
                tab_info.append(self._get_tab_info(tab))

        return tab_info

    def move_tab_to_column(self, tab_id: str, target_column_index: int) -> bool:
        """
        Move a tab to a specific column by index.

        Args:
            tab_id: ID of the tab to move
            target_column_index: Index of the target column (0-based)

        Returns:
            True if the tab was successfully moved, False otherwise

        Raises:
            ValueError: If target_column_index is invalid or tab_id doesn't exist
        """
        # Validate tab exists
        tab = self._tabs.get(tab_id)
        if not tab:
            raise ValueError(f"Tab with ID '{tab_id}' not found")

        # Find source column
        source_column = self._find_column_for_tab(tab)
        if source_column is None:
            raise ValueError(f"Could not find column for tab '{tab_id}'")

        # Validate target column index
        if target_column_index < 0:
            raise ValueError(f"Target column index must be non-negative, got {target_column_index}")

        # Validate target column index
        if target_column_index >= 6:
            raise ValueError(f"Target column index must be less than 6, got {target_column_index}")

        # Create new columns if necessary (up to maximum of 6)
        if target_column_index >= len(self._tab_columns):
            self._create_column(len(self._tab_columns))
            target_column_index = len(self._tab_columns) - 1

        target_column = self._tab_columns[target_column_index]

        # Don't move if already in target column
        if source_column == target_column:
            return False

        # Move the tab
        self._move_tab_between_columns(tab, source_column, target_column)

        # Update active column to target
        self._active_column = target_column

        # If source column is now empty, remove it (unless it's the last column)
        if source_column.count() == 0 and len(self._tab_columns) > 1:
            source_column_index = self._tab_columns.index(source_column)
            self._remove_column_and_resize(source_column_index, source_column)

            # Adjust target column index if we removed a column before it
            if source_column_index < target_column_index:
                target_column_index -= 1

        # Resize columns to distribute space evenly
        self.show_all_columns()

        # Update tab states
        self._update_tabs()

        return True

    def _update_mru_order(self, tab: TabBase, column: ColumnWidget) -> None:
        """
        Update the MRU order when a tab is activated.

        Args:
            tab: The tab that was activated
            column: The column containing the tab
        """
        tab_id = tab.tab_id()
        mru_list = self._column_mru_order[column]

        # Remove tab_id if it already exists in the list
        if tab_id in mru_list:
            mru_list.remove(tab_id)

        # Add to front of list (most recent)
        mru_list.insert(0, tab_id)

    def _get_next_mru_tab(self, column: ColumnWidget, excluding_tab_id: str | None = None) -> TabBase | None:
        """
        Get the next most recently used tab in a column.

        Args:
            column: The column to search in
            excluding_tab_id: Tab ID to exclude from selection

        Returns:
            The next MRU tab or None if no suitable tab found
        """
        mru_list = self._column_mru_order[column]

        for tab_id in mru_list:
            if excluding_tab_id and tab_id == excluding_tab_id:
                continue

            if tab_id in self._tabs:
                tab = self._tabs[tab_id]
                # Verify tab is still in this column
                if self._find_column_for_tab(tab) == column:
                    return tab

        return None

    def _close_ephemeral_tab_in_column(self, column: ColumnWidget, new_tab: TabBase) -> None:
        """Close any ephemeral tab in the specified column, except for the new one."""
        for i in range(column.count()):
            tab = cast(TabBase, column.widget(i))
            if tab.tab_id() == new_tab.tab_id():
                continue

            if tab.is_ephemeral():
                self.close_tab_by_id(tab.tab_id(), force_close=True)
                break  # Only one ephemeral tab per column

    def _create_tab_data(self, tab: TabBase, title: str) -> TabData:
        """
        Create TabData instance and connect signals.

        Args:
            tab: The tab widget to add
            title: Initial title for the tab

        Returns:
            TabData instance for the tab
        """
        tool_tip = tab.path()
        if tool_tip:
            tool_tip = self._mindspace_manager.get_relative_path(tool_tip)

        data = TabData(tab, title, tool_tip)
        data.label.close_clicked.connect(lambda: self._handle_close_clicked(data.tab_id))
        tab.activated.connect(lambda: self._handle_tab_activated(tab))
        tab.updated_state_changed.connect(self._handle_tab_updated)
        tab.modified_state_changed.connect(self._handle_tab_modified)
        return data

    def _handle_close_clicked(self, tab_id: str) -> None:
        """
        Handle close button click on a tab label.

        Args:
            tab_id: ID of the tab to close
        """
        self.close_tab_by_id(tab_id)
        self._mindspace_manager.add_interaction(
            MindspaceLogLevel.INFO,
            f"User closed tab, tab ID: {tab_id}"
        )

    def _handle_tab_updated(self, tab_id: str, is_updated: bool) -> None:
        """
        Update a tab's updated state.

        Args:
            tab_id: ID of the tab to update
            is_updated: Whether the tab has updated content
        """
        label = self._tab_labels.get(tab_id)
        if label:
            label.set_updated(is_updated)

        self._update_tabs(change_focus=False)

    def _remove_tab_from_column(self, tab: TabBase, column: ColumnWidget) -> None:
        """
        Remove a tab from a column and clean up associated data.

        Args:
            tab: Tab to remove
            column: Column containing the tab
        """
        tab_id = tab.tab_id()

        # Remove from MRU order
        mru_list = self._column_mru_order[column]
        if tab_id in mru_list:
            mru_list.remove(tab_id)

        tab_label = self._tab_labels.pop(tab_id)
        del self._tabs[tab_id]
        index = column.indexOf(tab)
        column.removeTab(index)
        tab_label.deleteLater()
        tab.deleteLater()

    def _add_tab_to_column(self, tab_data: TabData, column: ColumnWidget) -> None:
        """
        Add a tab to a column and set up associated data.

        Args:
            tab_data: TabData instance
            column: Target column
        """
        self._tabs[tab_data.tab_id] = tab_data.tab
        self._tab_labels[tab_data.tab_id] = tab_data.label

        # Set ephemeral state on label if tab is ephemeral
        if tab_data.tab.is_ephemeral():
            tab_data.label.set_ephemeral(True)

        index = column.addTab(tab_data.tab, "")
        column.tabBar().setTabButton(index, QTabBar.ButtonPosition.LeftSide, tab_data.label)
        column.setCurrentWidget(tab_data.tab)

        # Update MRU order for the new tab
        self._update_mru_order(tab_data.tab, column)

    def _move_tab_between_columns(
        self,
        tab: TabBase,
        source_column: ColumnWidget,
        target_column: ColumnWidget
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
        tab_id = tab.tab_id()
        tab_title = self._tab_labels[tab_id].text()

        # Remove from source column's MRU order
        source_mru = self._column_mru_order[source_column]
        if tab_id in source_mru:
            source_mru.remove(tab_id)

        # Remove from source
        self._remove_tab_from_column(tab, source_column)

        # Create and add new tab
        new_tab = self._restore_tab_from_state(tab_state)
        if not new_tab:
            return

        tab_data = self._create_tab_data(new_tab, tab_title)
        self._add_tab_to_column(tab_data, target_column)

    def _handle_welcome_tree_drop(self, path: str) -> None:
        """Handle mindspace tree drops when only welcome widget is visible."""
        # Create first column if it doesn't exist
        if not self._tab_columns:
            self._create_column(0)

        # Set as active column
        self._active_column = self._tab_columns[0]

        # Handle the file drop
        ext = os.path.splitext(path)[1].lower()
        if ext == '.conv':
            tab: TabBase | None = self.open_conversation(path, False)

        else:
            tab = self.open_wiki_page(path, False)

        if tab is None:
            return

        self._stack.setCurrentWidget(self._columns_widget)

    def _handle_splitter_moved(self, _pos: int, _index: int) -> None:
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
                        tab = cast(TabBase, source_column.widget(0))
                        self._move_tab_between_columns(tab, source_column, target_column)

                    # Remove the empty column
                    self._remove_column_and_resize(i, source_column)

                    # Update tab highlighting
                    self._update_tabs()
                    break

    def _handle_tab_drop(self, tab_id: str, target_column: ColumnWidget, target_index: int) -> None:
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
        if source_column is None:
            return

        source_index = source_column.indexOf(tab)

        # Don't process if dropped on itself
        if (source_column == target_column and (target_index - 1 <= source_index <= target_index)):
            return

        self._move_tab_between_columns(tab, source_column, target_column)

        # Set our new active column before we possibly delete the previous one
        self._active_column = target_column
        if source_column.count() == 0:
            column_number = self._tab_columns.index(source_column)
            self._remove_column_and_resize(column_number, source_column)

        # Update active states
        self._update_tabs()

    def _handle_path_drop(self, path: str, target_column: ColumnWidget, target_index: int) -> None:
        """
        Handle a file being dropped into a column.

        Args:
            path: Path dropped
            target_column: Column where the file was dropped
            target_index: Target position in the column
        """
        # Check file extension
        ext = os.path.splitext(path)[1].lower()

        # Set the target column as active
        self._active_column = target_column

        try:
            if ext == '.conv':
                # Open conversation file
                tab: TabBase | None = self.open_conversation(path, False)

            else:
                tab = self.open_wiki_page(path, False)

            if tab is None:
                return

            # Move the tab to the target position if not already there
            current_index = target_column.indexOf(tab)
            if current_index != target_index:
                target_column.tabBar().moveTab(current_index, target_index)

        except (ConversationError, OSError) as e:
            self._logger.exception("Failed to open dropped file '%s': %s", path, str(e))

    def _update_tab_bar_for_label_change(self, tab_id: str) -> None:
        """
        Efficiently update the tab bar after a label text change.

        Args:
            tab_id: ID of the tab whose label changed
        """
        tab = self._tabs.get(tab_id)
        if not tab:
            return

        column = self._find_column_for_tab(tab)
        if column is None:
            return

        tab_bar = cast(TabBar, column.tabBar())
        tab_bar.update_tab_size()

    def handle_file_rename(self, old_path: str, new_path: str) -> None:
        """Handle renaming of files by updating any open tabs.

        Args:
            old_path: Original path of renamed file
            new_path: New path after renaming
        """
        # Update any editor tab for this file
        editor_tab = self._find_editor_tab_by_path(old_path)
        if editor_tab:
            editor_tab.set_path(new_path)
            self._update_editor_tab_label(editor_tab)

        # Update any conversation tab for this file
        if old_path.endswith('.conv'):
            conversation_tab = self._find_conversation_tab_by_path(old_path)
            if conversation_tab:
                conversation_tab.set_path(new_path)

                # Update tab label text
                new_title = os.path.splitext(os.path.basename(new_path))[0]
                tab_id = conversation_tab.tab_id()
                label = self._tab_labels[tab_id]
                label.set_text(new_title)
                self._update_tab_bar_for_label_change(tab_id)

        # Update any wiki tab for this file
        wiki_tab = self._find_wiki_tab_by_path(old_path)
        if wiki_tab:
            wiki_tab.set_path(new_path)

            # Update tab label text
            new_title = os.path.basename(new_path)
            tab_id = wiki_tab.tab_id()
            label = self._tab_labels[tab_id]
            label.set_text(new_title)
            self._update_tab_bar_for_label_change(tab_id)

    def _create_column(self, index: int) -> ColumnWidget:
        """Create a new tab column."""
        column_widget = ColumnWidget()
        column_widget.setMinimumWidth(200)  # Set minimum width
        column_widget.currentChanged.connect(self._handle_tab_changed)
        column_widget.column_activated.connect(self._handle_column_activated)
        column_widget.tab_drop.connect(self._handle_tab_drop)
        column_widget.path_drop.connect(self._handle_path_drop)

        self._column_splitter.insertWidget(index, column_widget)
        self._tab_columns.insert(index, column_widget)

        self._column_mru_order[column_widget] = []

        return column_widget

    def _handle_tab_merge(self, dragged_tab_id: str, target_tab_id: str) -> None:
        """Handle merging tabs when one is dropped directly onto another."""
        dragged_tab = self._tabs.get(dragged_tab_id)
        target_tab = self._tabs.get(target_tab_id)

        if not dragged_tab or not target_tab:
            return

        source_column = self._find_column_for_tab(dragged_tab)
        if source_column is None:
            return

        target_column = self._find_column_for_tab(target_tab)
        if target_column is None:
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

    def _remove_column_and_resize(self, column_number: int, column: ColumnWidget) -> None:
        """
        Remove a column and resize the remaining columns.

        Args:
            column_number: Index of the column to remove
            column: Column widget to remove
        """
        if column in self._column_mru_order:
            del self._column_mru_order[column]

        del self._tab_columns[column_number]
        column.deleteLater()

        # Resize splitter to evenly distribute space
        if self._tab_columns:
            width = self.width()
            column_width = width // len(self._tab_columns)

            # Note: We add 1 to column count because deletion hasn't processed yet
            self._column_splitter.setSizes([column_width] * (len(self._tab_columns) + 1))

    def _update_tabs(self, change_focus: bool=True) -> None:
        """ Update the state of all tabs and their labels. """
        for tab_id, label in self._tab_labels.items():
            tab = self._tabs[tab_id]
            column = self._find_column_for_tab(tab)
            if column is None:
                continue

            column_index = column.currentIndex()
            is_current = column_index != -1 and tab == column.widget(column_index)
            is_active_column = column == self._active_column
            is_updated = tab.is_updated()

            # If tab becomes current, clear its updated state
            if is_current and is_updated:
                tab.set_updated(False)
                is_updated = False

            label.set_current(is_current, is_active_column, is_updated)

            # Update the tab bar state for background painting
            tab_index = column.indexOf(tab)
            if tab_index != -1 and isinstance(column.tabBar(), TabBar):
                tab_bar = cast(TabBar, column.tabBar())
                tab_bar.set_tab_state(tab_index, is_current, is_updated, is_active_column)

        # Force style refresh to show active state
        self._handle_style_changed()

        # Disconnect any existing connections to avoid duplicates
        try:
            if self._current_status_tab is not None:
                self._current_status_tab.status_message.disconnect()
                self._current_status_tab = None

        except RuntimeError:
            pass  # No existing connections

        # Connect the current tab to the status message signal
        current_tab = self.get_current_tab()
        if not current_tab:
            return

        current_tab.status_message.connect(self.status_message)
        self._current_status_tab = current_tab
        current_tab.update_status()

        if change_focus:
            current_tab.setFocus()

            # Ensure the new tab is activated, but we have to give a slight delay to allow for
            # all signals to process correctly
            if self._activation_timer.isActive():
                self._activation_timer.stop()

            self._activation_timer.start()

        self.tab_changed.emit()

    def _activate_current_tab(self) -> None:
        """Activate the current tab."""
        current_tab = self.get_current_tab()
        if not current_tab:
            return

        current_tab.activate()

    def _handle_tab_changed(self, _index: int) -> None:
        """
        Handle tab selection changes.

        Args:
            index: Index of the newly selected tab
        """
        # Find which column triggered the change
        sender = self.sender()
        column = cast(ColumnWidget, sender)
        self._active_column = column

        # Update MRU order for the newly selected tab.  Also check if we need to update focus.  We don't
        # want to change focus if the current tab is ephemeral.
        current_tab = self.get_current_tab()
        update_focus = True
        if current_tab:
            self._update_mru_order(current_tab, column)
            update_focus = not current_tab.is_ephemeral()

        self._update_tabs(change_focus=update_focus)

    def _handle_tab_activated(self, tab: TabBase) -> None:
        """
        Handle tab activation from widget focus.

        Args:
            tab: The tab that was activated
        """
        # Find which column contains the tab
        column = self._find_column_for_tab(tab)
        if column is None:
            return

        if column == self._active_column:
            return

        # Update active column
        self._active_column = column
        self._update_mru_order(tab, column)
        self._update_tabs()

    def _handle_column_activated(self, column: ColumnWidget) -> None:
        """Handle column activation."""
        if column not in self._tab_columns:
            return

        if column == self._active_column:
            return

        self._active_column = column
        self._update_tabs()

    def _get_target_column_for_new_tab(self) -> ColumnWidget:
        """
        Determine which column should receive a new tab.

        If the current active tab is protected, use an adjacent column.

        Returns:
            The column widget where the new tab should be added
        """
        # If the current tab in not protected, we can use the normal behavior
        if not self._protect_current_tab:
            return self._active_column

        # Get the current column index
        current_column_number = self._tab_columns.index(self._active_column)

        # If there's only one column, create a new one to the right
        if len(self._tab_columns) == 1:
            new_column = self._create_column(1)

            # Set column sizes
            width = self.width()
            self._column_splitter.setSizes([width // 2, width // 2])

            return new_column

        # Try to use the column to the right if possible
        if current_column_number < len(self._tab_columns) - 1:
            return self._tab_columns[current_column_number + 1]

        # Otherwise use the column to the left
        return self._tab_columns[current_column_number - 1]

    def _add_tab(self, tab: TabBase, title: str) -> None:
        """
        Add a new tab to the manager.

        Args:
            tab: The tab widget to add
            title: Initial title for the tab
        """
        # Determine target column
        target_column = self._get_target_column_for_new_tab()

        tab_data = self._create_tab_data(tab, title)
        self._add_tab_to_column(tab_data, target_column)

        # Close any ephemeral tab in target column because we've just added a new one
        self._close_ephemeral_tab_in_column(target_column, tab)

        # Set initial state
        if len(self._tabs) == 1:  # If this is the first tab
            tab_data.label.set_current(True, True)
            self._stack.setCurrentWidget(self._columns_widget)

    def close_tab_by_id(self, tab_id: str, force_close: bool=False) -> None:
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
        if not force_close and not tab.can_close_tab():
            return

        tab.close_tab()

        # Find which column contains the tab
        column = self._find_column_for_tab(tab)
        if column is None:
            return

        # Before removing, select the next MRU tab if this was the active tab
        was_current = column.currentWidget() == tab
        next_tab = None

        if was_current and column.count() > 1:
            next_tab = self._get_next_mru_tab(column, excluding_tab_id=tab_id)
            if next_tab:
                column.setCurrentWidget(next_tab)

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

    def _find_column_for_tab(self, tab: TabBase) -> ColumnWidget | None:
        """Find which column contains the given tab."""
        for column in self._tab_columns:
            if column.indexOf(tab) != -1:
                return column

        return None

    def get_current_tab(self) -> TabBase | None:
        """
        Get the currently active tab.

        Returns:
            The current tab or None if no tabs exist
        """
        widget = self._active_column.currentWidget()
        return cast(TabBase, widget) if widget else None

    def _set_current_tab(self, tab: TabBase, ephemeral: bool) -> None:
        """
        Set the current active tab.

        Args:
            tab: The tab to make current
            ephemeral: Whether the tab is ephemeral (temporary)
        """
        # Find which column contains the tab
        column = self._find_column_for_tab(tab)
        if column is None:
            return

        # We need to set the current widget for the column to the tab, but we don't want this to
        # change the focus widget when we do.
        focus_widget = QApplication.focusWidget()
        column.setCurrentWidget(tab)
        if focus_widget is not None:
            focus_widget.setFocus()

        self._active_column = column
        self._update_mru_order(tab, column)
        self._update_tabs(not ephemeral)

    def _make_tab_permanent(self, tab: TabBase) -> None:
        """Convert an ephemeral tab to permanent."""
        tab.set_ephemeral(False)
        label = self._tab_labels.get(tab.tab_id())
        if label:
            label.set_ephemeral(False)

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

        if isinstance(tab, ConversationTab):
            if modified:
                self._make_tab_permanent(tab)

            return

        if isinstance(tab, EditorTab):
            label = self._tab_labels.get(tab_id)
            if label:
                current_text = label.text()
                if modified and not current_text.endswith('*'):
                    label.set_text(f"{current_text}*")

                elif not modified and current_text.endswith('*'):
                    label.set_text(current_text[:-1])

                self._update_tab_bar_for_label_change(tab_id)

    def current_tab_path(self) -> str:
        """
        Get the path of the currently active tab.

        Returns:
            The path of the current tab, or an empty string if no tab is active
        """
        current_tab = self.get_current_tab()
        if not current_tab:
            return ""

        return current_tab.path()

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
        current_tab = self.get_current_tab()
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
            tab = cast(TabBase, current_column.widget(0))
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

    def find_tab_by_id(self, tab_id: str) -> TabBase | None:
        """
        Find a tab by its ID.

        Args:
            tab_id: The ID of the tab to search for

        Returns:
            The TabBase instance if found, None otherwise
        """
        return self._tabs.get(tab_id)

    def _find_conversation_tab_by_path(self, path: str) -> ConversationTab | None:
        """
        Find a conversation tab by its conversation path.

        Args:
            conversation_path: The path to search for

        Returns:
            The ConversationTab if found, None otherwise
        """
        for tab in self._tabs.values():
            if isinstance(tab, ConversationTab) and tab.path() == path:
                return tab

        return None

    def _find_editor_tab_by_path(self, path: str) -> EditorTab | None:
        """
        Find an editor tab by its path.

        Args:
            path: The path to search for

        Returns:
            The EditorTab if found, None otherwise
        """
        for tab in self._tabs.values():
            if isinstance(tab, EditorTab) and tab.path() == path:
                return tab

        return None

    def _find_wiki_tab_by_path(self, path: str) -> WikiTab | None:
        """
        Find a wiki tab by its path.

        Args:
            path: The wiki path to search for

        Returns:
            The WikiTab if found, None otherwise
        """
        for tab in self._tabs.values():
            if isinstance(tab, WikiTab) and tab.path() == path:
                return tab

        return None

    def show_system_log(self) -> LogTab:
        """
            Show the system log tab.

        Returns:
            The system log tab
        """
        for tab in self._tabs.values():
            if isinstance(tab, LogTab):
                self._set_current_tab(tab, False)
                return tab

        log_tab = LogTab("", self)

        # Use language strings for the tab title
        self._add_tab(log_tab, "Mindspace Log")
        return log_tab

    def show_system_shell(self) -> ShellTab:
        """
            Show the system shell tab.

        Returns:
            The system shell tab
        """
        for tab in self._tabs.values():
            if isinstance(tab, ShellTab):
                self._set_current_tab(tab, False)
                return tab

        shell_tab = ShellTab("", self)

        # Use language strings for the tab title
        self._add_tab(shell_tab, "Humbug Shell")
        return shell_tab

    def clear_shell_history(self) -> None:
        """Clear the shell tab history."""
        for tab in self._tabs.values():
            if isinstance(tab, ShellTab):
                tab.clear_history()
                return

    def new_file(self) -> EditorTab:
        """Create a new empty editor tab."""
        self._untitled_count += 1
        editor = EditorTab("", "", self._untitled_count, self)
        self._add_tab(editor, f"Untitled-{self._untitled_count}")
        return editor

    def open_file(self, path: str) -> EditorTab:
        """Open a file in a new or existing editor tab."""
        assert os.path.isabs(path), "Path must be absolute"

        # Check if file is already open
        existing_tab = self._find_editor_tab_by_path(path)
        if existing_tab is not None:
            self._set_current_tab(existing_tab, False)
            return existing_tab

        editor = EditorTab("", path, None, self)
        self._add_tab(editor, os.path.basename(path))
        return editor

    def new_conversation(
        self,
        parent: ConversationTab | None = None,
        model: str | None = None,
        temperature: float | None = None,
        reasoning: ReasoningCapability | None = None
    ) -> ConversationTab:
        """Create a new conversation tab and return its ID."""
        # Generate timestamp for ID
        timestamp = datetime.now(timezone.utc)
        conversation_title = timestamp.strftime("%Y-%m-%d-%H-%M-%S-%f")[:23]
        prefix = "dAI-" if parent is not None else ""
        conversation_title = f"{prefix}{conversation_title}"
        filename = os.path.join("conversations", f"{conversation_title}.conv")
        full_path = self._mindspace_manager.get_absolute_path(filename)

        conversation_tab = ConversationTab("", full_path, self)
        conversation_tab.forkRequested.connect(self.fork_conversation)
        conversation_tab.forkFromIndexRequested.connect(self._fork_conversation_from_index)

        # Set model based on mindspace settings
        settings = cast(MindspaceSettings, self._mindspace_manager.settings())
        if model is None:
            model = settings.model

        if temperature is None:
            temperature = settings.temperature

        if reasoning is None:
            reasoning = settings.reasoning

        conversation_settings = AIConversationSettings(
            model=model,
            temperature=temperature if AIConversationSettings.supports_temperature(model) else None,
            reasoning=reasoning
        )
        conversation_tab.update_conversation_settings(conversation_settings)
        self._add_tab(conversation_tab, conversation_title)
        return conversation_tab

    def open_conversation(self, path: str, ephemeral: bool) -> ConversationTab | None:
        """Open an existing conversation file."""
        assert os.path.isabs(path), "Path must be absolute"

        # Check if already open
        abs_path = self._mindspace_manager.get_absolute_path(path)
        existing_tab = self._find_conversation_tab_by_path(abs_path)
        if existing_tab:
            if existing_tab.is_ephemeral() and not ephemeral:
                # If the existing tab is ephemeral, convert it to permanent
                self._make_tab_permanent(existing_tab)

            self._set_current_tab(existing_tab, ephemeral)
            return existing_tab

        try:
            conversation_tab = ConversationTab("", abs_path, self)
            conversation_tab.forkRequested.connect(self.fork_conversation)
            conversation_tab.forkFromIndexRequested.connect(self._fork_conversation_from_index)
            conversation_title = os.path.splitext(os.path.basename(abs_path))[0]
            conversation_tab.set_ephemeral(ephemeral)
            self._add_tab(conversation_tab, conversation_title)
            return conversation_tab

        except ConversationError as e:
            self._logger.exception("Failed to open conversation: %s", str(e))
            raise

    def can_fork_conversation(self) -> bool:
        """Check if the current tab can be forked."""
        tab = self.get_current_tab()
        if not tab or not isinstance(tab, ConversationTab):
            return False

        return True

    def fork_conversation(self) -> None:
        """Create a new conversation tab with the history of the current conversation."""
        self._fork_conversation_from_index(None)

    def _fork_conversation_from_index(self, message_index: int | None) -> None:
        """Create a new conversation tab with the history from an index in the current conversation."""
        try:
            self.protect_current_tab(True)
            conversation_tab = self.get_current_tab()
            if not isinstance(conversation_tab, ConversationTab):
                return

            new_tab = conversation_tab.fork_conversation_from_index(message_index)
            new_tab.forkRequested.connect(self.fork_conversation)
            new_tab.forkFromIndexRequested.connect(self._fork_conversation_from_index)
            self._add_tab(new_tab, os.path.splitext(os.path.basename(new_tab.path()))[0])

        except ConversationError as e:
            strings = self._language_manager.strings()
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.conversation_error_title,
                strings.error_forking_conversation.format(str(e))
            )

        finally:
            self.protect_current_tab(False)

    def new_terminal(self, command: str | None = None) -> TerminalTab:
        """Create new terminal tab.

        Args:
            command: Optional command to run in terminal

        Returns:
            Created terminal tab
        """
        terminal = TerminalTab("", command, self)

        if command:
            title = os.path.basename(command)

        else:
            title = "Terminal"

        self._add_tab(terminal, title)
        return terminal

    def handle_wiki_link(self, path: str) -> WikiTab:
        """
        Handle a wiki link click.

        Args:
            path: Path to the wiki page
        Returns:
            The opened WikiTab
        """
        return self.open_wiki_page(path, True)

    def open_wiki_page(self, path: str, ephemeral: bool) -> WikiTab:
        """Open a wiki page."""
        assert os.path.isabs(path), "Path must be absolute"

        path_minus_anchor = path
        anchor = None
        if '#' in path:
            path_minus_anchor, anchor = path.split('#', 1)

        # Check if already open
        existing_tab = self._find_wiki_tab_by_path(path_minus_anchor)
        if existing_tab:
            if existing_tab.is_ephemeral() and not ephemeral:
                # If the existing tab is ephemeral, convert it to permanent
                self._make_tab_permanent(existing_tab)

            self._set_current_tab(existing_tab, ephemeral)

            # If there's an anchor, scroll to it
            if anchor:
                existing_tab.scroll_to_anchor(anchor)

            return existing_tab

        try:
            wiki_tab = WikiTab("", path_minus_anchor, self)
            wiki_tab.open_wiki_path.connect(self.handle_wiki_link)
            wiki_tab.edit_file.connect(self._edit_file_from_wiki_page)
            wiki_tab.set_ephemeral(ephemeral)
            self._add_tab(wiki_tab, os.path.basename(path_minus_anchor))

            # If there's an anchor, scroll to it
            if anchor:
                wiki_tab.scroll_to_anchor(anchor)

            return wiki_tab

        except WikiError as e:
            self._logger.exception("Failed to open wiki page: %s", str(e))
            raise

    def _edit_file_from_wiki_page(self, path: str) -> EditorTab:
        """
        Edit a file from a wiki page.

        Args:
            path: Path to the file
        Returns:
            The opened editor tab
        """
        return self.open_file(path)

    def save_state(self) -> Dict:
        """Get current state of all tabs and columns."""
        tab_columns = []
        active_column_index = self._tab_columns.index(self._active_column)

        for column in self._tab_columns:
            tab_states = []
            active_tab_id = None

            # Get active tab ID for this column
            current_index = column.currentIndex()
            if current_index != -1:
                current_tab = cast(TabBase, column.widget(current_index))
                active_tab_id = current_tab.tab_id()

            for index in range(column.count()):
                tab = cast(TabBase, column.widget(index))
                try:
                    state = tab.get_state(False)
                    state_dict = state.to_dict()
                    tab_states.append(state_dict)

                except Exception as e:
                    self._logger.exception("Failed to save tab manager state: %s", str(e))
                    continue

            tab_columns.append({
                'tabs': tab_states,
                'active_tab_id': active_tab_id
            })

        return {
            'columns': tab_columns,
            'active_column_index': active_column_index
        }

    def _restore_tab_from_state(self, state: TabState) -> TabBase | None:
        """Create appropriate tab type from state."""
        match state.type:
            case TabType.CONVERSATION:
                conversation_tab = ConversationTab.restore_from_state(state, self)
                conversation_tab.forkRequested.connect(self.fork_conversation)
                conversation_tab.forkFromIndexRequested.connect(self._fork_conversation_from_index)
                return conversation_tab

            case TabType.EDITOR:
                editor_tab = EditorTab.restore_from_state(state, self)
                return editor_tab

            case TabType.LOG:
                return LogTab.restore_from_state(state, self)

            case TabType.SHELL:
                return ShellTab.restore_from_state(state, self)

            case TabType.TERMINAL:
                return TerminalTab.restore_from_state(state, self)

            case TabType.WIKI:
                wiki_tab = WikiTab.restore_from_state(state, self)
                wiki_tab.open_wiki_path.connect(self.handle_wiki_link)
                wiki_tab.edit_file.connect(self._edit_file_from_wiki_page)
                return wiki_tab

        return None

    def _get_tab_title(self, tab: TabBase, state: TabState) -> str:
        """Get appropriate title for tab type."""
        if isinstance(tab, ConversationTab):
            return os.path.splitext(os.path.basename(state.path))[0]

        if isinstance(tab, LogTab):
            return "Mindspace Log"

        if isinstance(tab, ShellTab):
            return "Humbug Shell"

        if isinstance(tab, TerminalTab):
            if state.metadata and "command" in state.metadata:
                return os.path.basename(state.metadata['command'])

            return "Terminal"

        if isinstance(tab, WikiTab):
            return os.path.basename(state.path)

        return os.path.basename(state.path)

    def _restore_column_state(self, column_index: int, tab_states: List[Dict]) -> List[str]:
        """
        Restore state for a single column of tabs.
        Args:
            column_index: Index of the column to restore
            tab_states: List of tab states to restore in this column
        Returns:
            List of restored tab paths
        """
        paths = []

        for state_dict in tab_states:
            try:
                state = TabState.from_dict(state_dict)
                state.path = self._mindspace_manager.get_absolute_path(state.path)

                tab = self._restore_tab_from_state(state)
                if not tab:
                    continue

                tab.set_ephemeral(state.is_ephemeral)

                self._active_column = self._tab_columns[column_index]
                title = self._get_tab_title(tab, state)
                self._add_tab(tab, title)

                path = tab.path()
                if path:
                    paths.append(path)

            except Exception as e:
                self._logger.exception("Failed to restore tab manager state: %s", str(e))
                continue

        return paths

    def _deferred_set_active_column(self, active_column_index: int, active_tab_ids: List[str]) -> None:
        """
        Set the active column and tab after UI has settled.

        Args:
            active_column_index: Index of the column to make active
            active_tab_ids: List of active tab IDs for each column
        """
        # Set the active column
        if 0 <= active_column_index < len(self._tab_columns):
            self._active_column = self._tab_columns[active_column_index]

            # If there's an active tab in this column, ensure it has focus
            if active_tab_ids and active_column_index < len(active_tab_ids):
                active_tab_id = active_tab_ids[active_column_index]
                if active_tab_id in self._tabs:
                    tab = self._tabs[active_tab_id]
                    tab.setFocus()

        # Update tab states to show correct active highlighting
        self._update_tabs()

    def restore_state(self, saved_state: Dict) -> List[str]:
        """
        Restore tabs and active states from saved state.
        Args:
            saved_state: Dictionary containing saved state of columns and tabs
        Returns:
            List of restored tab paths
        """
        saved_columns = saved_state.get('columns', [])
        active_column_index = saved_state.get('active_column_index', 0)

        # Create necessary columns
        num_columns = len(saved_columns)
        for index in range(1, num_columns):
            self._create_column(index)

        # First pass: restore all tabs in all columns
        restored_paths = []
        for column_index, column_state in enumerate(saved_columns):
            tab_states = column_state.get('tabs', [])
            paths = self._restore_column_state(column_index, tab_states)
            restored_paths.extend(paths)

        # Second pass: set active tabs in each column
        active_tab_ids = []
        for column_index, column_state in enumerate(saved_columns):
            active_tab_id = column_state.get('active_tab_id')
            if active_tab_id and active_tab_id in self._tabs:
                column = self._tab_columns[column_index]
                tab = self._tabs[active_tab_id]
                column.setCurrentWidget(tab)
                active_tab_ids.append(active_tab_id)

        # Show all columns with appropriate sizes
        self.show_all_columns()

        # Defer setting the active column to ensure it's not overridden by other UI operations
        QTimer.singleShot(0, lambda: self._deferred_set_active_column(active_column_index, active_tab_ids))

        return restored_paths

    def _handle_style_changed(self) -> None:
        """
        Handle style changes from StyleManager.
        """
        # Apply simplified stylesheet since custom painting handles backgrounds
        for column in self._tab_columns:
            style = f"""
                QTabBar {{
                    border: none;
                    margin: 0px;
                    background-color: {self._style_manager.get_color_str(ColorRole.TAB_BAR_BACKGROUND)};
                }}
                QTabBar::tab {{
                    border: none;
                    margin: 1px;
                    padding: 0px;
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

            # Trigger repaint of the tab bar to show updated colors
            if isinstance(column.tabBar(), TabBar):
                column.tabBar().update()

        # Update all tab labels, setting active state only for the current active tab.
        for tab_id, label in self._tab_labels.items():
            tab = self._tabs[tab_id]
            tab_column = self._find_column_for_tab(tab)
            if tab_column is None:
                continue

            label.handle_style_changed()

        self._column_splitter.setStyleSheet(f"""
            QSplitter::handle {{
                background-color: {self._style_manager.get_color_str(ColorRole.SPLITTER)};
                margin: 0;
                width: 1px;
            }}
        """)

    def can_undo(self) -> bool:
        """Check if the last action can be undone."""
        tab = self.get_current_tab()
        return False if tab is None else tab.can_undo()

    def undo(self) -> None:
        """Undo the last action."""
        tab = self.get_current_tab()
        if tab is None:
            return

        tab.undo()

    def can_redo(self) -> bool:
        """Check if the last action can be redone."""
        tab = self.get_current_tab()
        return False if tab is None else tab.can_redo()

    def redo(self) -> None:
        """Redo the last undone action."""
        tab = self.get_current_tab()
        if tab is None:
            return

        tab.redo()

    def can_cut(self) -> bool:
        """Check if the current selection can be cut."""
        tab = self.get_current_tab()
        return False if tab is None else tab.can_cut()

    def cut(self) -> None:
        """Cut the current selection."""
        tab = self.get_current_tab()
        if tab is None:
            return

        tab.cut()

    def can_copy(self) -> bool:
        """Check if the current selection can be copied."""
        tab = self.get_current_tab()
        return False if tab is None else tab.can_copy()

    def copy(self) -> None:
        """Copy the current selection."""
        tab = self.get_current_tab()
        if tab is None:
            return

        tab.copy()

    def can_paste(self) -> bool:
        """Check if the current selection can be pasted."""
        tab = self.get_current_tab()
        return False if not tab else tab.can_paste()

    def paste(self) -> None:
        """Paste the current selection."""
        tab = self.get_current_tab()
        if tab is None:
            return

        tab.paste()

    def can_show_find(self) -> bool:
        """Check if the current tab can show the find dialog."""
        tab = self.get_current_tab()
        return tab is not None

    def show_find(self) -> None:
        """Show the find dialog for the current tab."""
        tab = self.get_current_tab()
        if tab is None:
            return

        tab.show_find()

    def close_deleted_file(self, path: str) -> None:
        """
        Close any open tabs related to a file being deleted.

        Args:
            path: Path of file being deleted
        """
        # Find and close any editor tab for this file
        editor_tab = self._find_editor_tab_by_path(path)
        if editor_tab:
            self.close_tab_by_id(editor_tab.tab_id(), True)

        # Also check for conversation files
        if path.endswith('.conv'):
            conversation_tab = self._find_conversation_tab_by_path(path)
            if conversation_tab:
                self.close_tab_by_id(conversation_tab.tab_id(), True)

        # Close any wiki page we may have had for this file
        wiki_tab = self._find_wiki_tab_by_path(path)
        if wiki_tab:
            self.close_tab_by_id(wiki_tab.tab_id(), True)

    def can_close_all_tabs(self) -> bool:
        """Can we close all the tabs that are open?"""
        all_tabs = list(self._tabs.values())
        for tab in all_tabs:
            if tab.is_modified() and not tab.can_close_tab():
                return False

        return True

    def close_all_tabs(self) -> None:
        """Close all open tabs."""
        all_tabs = list(self._tabs.values())
        for tab in all_tabs:
            self.close_tab_by_id(tab.tab_id())

    def can_close_tab(self) -> bool:
        """Can we close the currently active tab?"""
        tab = self.get_current_tab()

        # We only care if a tab is open.  Actually closing it may mean taking other
        # actions such as saving.
        return tab is not None

    def close_tab(self) -> None:
        """Close the currently active tab."""
        tab = self.get_current_tab()
        if tab is None:
            return

        self.close_tab_by_id(tab.tab_id())

    def can_save_file(self) -> bool:
        """Check if the current file can be saved."""
        tab = self.get_current_tab()
        if tab is None:
            return False

        return tab.can_save()

    def save_file(self) -> str:
        """Save the current file."""
        current_tab = self.get_current_tab()
        if not isinstance(current_tab, EditorTab):
            return ""

        current_tab.save()
        self._update_editor_tab_label(current_tab)

        return current_tab.path()

    def can_save_file_as(self) -> bool:
        """Check if the current file can be saved as a new file."""
        tab = self.get_current_tab()
        if tab is None:
            return False

        return tab.can_save_as()

    def save_file_as(self) -> str:
        """Save the current file with a new name."""
        current_tab = self.get_current_tab()
        if not isinstance(current_tab, EditorTab):
            return ""

        current_tab.save_as()
        self._update_editor_tab_label(current_tab)

        return current_tab.path()

    def _update_editor_tab_label(self, tab: EditorTab) -> None:
        """
        Update the tab label.

        Args:
            current_tab: Tab to update
        """
        title = os.path.basename(tab.path())
        if tab.is_modified():
            title += "*"

        tab_id = tab.tab_id()
        label = self._tab_labels.get(tab_id)
        cast(TabLabel, label).set_text(title)
        self._update_tab_bar_for_label_change(tab_id)

    def can_show_all_columns(self) -> bool:
        """Check if all columns can be shown."""
        return len(self._tab_columns) != 0

    def show_all_columns(self) -> None:
        """Show all columns in the tab manager."""
        if len(self._tab_columns) == 0:
            return

        num_columns = len(self._tab_columns)
        sizes = [(self.width() // num_columns) for _ in range(num_columns)]
        self._column_splitter.setSizes(sizes)

    def can_submit_message(self) -> bool:
        """Check if the current tab can submit a message."""
        tab = self.get_current_tab()
        if tab is None:
            return False

        return tab.can_submit()

    def submit_message(self) -> None:
        """Handle message submission."""
        tab = self.get_current_tab()
        if tab is None:
            return

        tab.submit()

    def can_show_conversation_settings_dialog(self) -> bool:
        """Check if the conversation settings dialog can be shown."""
        tab = self.get_current_tab()
        return isinstance(tab, ConversationTab)

    def show_conversation_settings_dialog(self) -> None:
        """Show the conversation settings dialog."""
        tab = self.get_current_tab()
        if not isinstance(tab, ConversationTab):
            return

        tab.show_conversation_settings_dialog()

    def handle_esc_key(self) -> bool:
        """Handle processing of the "Esc" key."""
        tab = self.get_current_tab()
        if not isinstance(tab, ConversationTab):
            return False

        tab.cancel_current_tasks()
        return True

    def can_navigate_next_message(self) -> bool:
        """Check if next message navigation is possible."""
        tab = self.get_current_tab()
        if not isinstance(tab, ConversationTab | LogTab | ShellTab):
            return False

        return tab.can_navigate_next_message()

    def navigate_next_message(self) -> None:
        """Navigate to next message in the tab."""
        tab = self.get_current_tab()
        if not isinstance(tab, ConversationTab | LogTab | ShellTab):
            return

        tab.navigate_next_message()

    def can_navigate_previous_message(self) -> bool:
        """Check if previous message navigation is possible."""
        tab = self.get_current_tab()
        if not isinstance(tab, ConversationTab | LogTab | ShellTab):
            return False

        return tab.can_navigate_previous_message()

    def navigate_previous_message(self) -> None:
        """Navigate to previous message in the tab."""
        tab = self.get_current_tab()
        if not isinstance(tab, ConversationTab | LogTab | ShellTab):
            return

        tab.navigate_previous_message()

    def can_toggle_bookmark(self) -> bool:
        """Can we toggle a bookmark?"""
        tab = self.get_current_tab()
        if not isinstance(tab, ConversationTab):
            return False

        return tab.can_toggle_bookmark()

    def is_checked_bookmark(self) -> bool:
        """Is the current bookmark set (checked)?"""
        tab = self.get_current_tab()
        if not isinstance(tab, ConversationTab):
            return False

        return tab.is_checked_bookmark()

    def toggle_bookmark(self) -> None:
        """Handle toggling a bookmark."""
        tab = self.get_current_tab()
        if not isinstance(tab, ConversationTab):
            return

        tab.toggle_bookmark()

    def can_navigate_next_bookmark(self) -> bool:
        """Can we move to the next bookmark?"""
        tab = self.get_current_tab()
        if not isinstance(tab, ConversationTab):
            return False

        return tab.can_navigate_next_bookmark()

    def navigate_next_bookmark(self) -> None:
        """Handle navigating to the next bookmark."""
        tab = self.get_current_tab()
        if not isinstance(tab, ConversationTab):
            return

        tab.navigate_next_bookmark()

    def can_navigate_previous_bookmark(self) -> bool:
        """Can we move to the previous bookmark?"""
        tab = self.get_current_tab()
        if not isinstance(tab, ConversationTab):
            return False

        return tab.can_navigate_previous_bookmark()

    def navigate_previous_bookmark(self) -> None:
        """Handle navigating to the previous bookmark."""
        tab = self.get_current_tab()
        if not isinstance(tab, ConversationTab):
            return

        tab.navigate_previous_bookmark()
