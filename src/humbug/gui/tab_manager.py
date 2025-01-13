from datetime import datetime
import logging
import os
from typing import Optional, Dict, List, cast
import uuid

from PySide6.QtWidgets import QDialog, QTabBar, QWidget, QVBoxLayout, QSplitter, QStackedWidget
from PySide6.QtCore import Signal, Qt

from humbug.ai.ai_backend import AIBackend
from humbug.gui.conversation_error import ConversationError
from humbug.gui.conversation_settings_dialog import ConversationSettingsDialog
from humbug.gui.conversation_tab import ConversationTab
from humbug.gui.color_role import ColorRole
from humbug.gui.editor_tab import EditorTab
from humbug.gui.status_message import StatusMessage
from humbug.gui.style_manager import StyleManager
from humbug.gui.tab_base import TabBase
from humbug.gui.tab_column import TabColumn
from humbug.gui.tab_label import TabLabel
from humbug.gui.tab_state import TabState
from humbug.gui.tab_type import TabType
from humbug.gui.welcome_widget import WelcomeWidget
from humbug.workspace.workspace_manager import WorkspaceManager


class TabManager(QWidget):
    """Manages multiple tabs across one or two columns."""

    column_state_changed = Signal()
    status_message = Signal(StatusMessage)

    def __init__(self, ai_backends: Dict[str, AIBackend], parent=None):
        """Initialize the tab manager."""
        super().__init__(parent)

        self._untitled_count = 0
        self._ai_backends = ai_backends
        self._workspace_manager = WorkspaceManager()
        self._logger = logging.getLogger("TabManager")

        # Create main layout
        main_layout = QVBoxLayout(self)
        main_layout.setContentsMargins(0, 0, 0, 0)
        main_layout.setSpacing(0)

        # Create stack widget for tab container and welcome message
        self._stack = QStackedWidget()
        main_layout.addWidget(self._stack)

        # Create welcome widget
        self._welcome_widget = WelcomeWidget()
        self._stack.addWidget(self._welcome_widget)

        # Create widget to hold columns
        self._columns_widget = QWidget()
        self._columns_layout = QVBoxLayout(self._columns_widget)
        self._columns_layout.setContentsMargins(0, 0, 0, 0)
        self._columns_layout.setSpacing(0)
        self._stack.addWidget(self._columns_widget)

        # Create splitter for columns
        self._column_splitter = QSplitter(Qt.Horizontal)
        self._columns_layout.addWidget(self._column_splitter)

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

        self._handle_style_changed(self._style_manager.zoom_factor)

    def _create_column(self, index: int) -> TabColumn:
        """Create a new tab column."""
        tab_widget = TabColumn()
        tab_widget.currentChanged.connect(self._handle_tab_changed)
        tab_widget.column_activated.connect(self._handle_column_activated)

        self._column_splitter.insertWidget(index, tab_widget)
        self._tab_columns.insert(index, tab_widget)
        self._logger.debug(f"insertd new column at {index}")

        return tab_widget

    def _update_tabs(self) -> None:
        # Update current states for all tabs
        for tab_id, label in self._tab_labels.items():
            tab = self._tabs[tab_id]
            column = self._find_column_for_tab(tab)
            self._logger.debug(f"update tabs: {column}: {tab_id}")
            column_index = column.currentIndex()
            is_current = column_index != -1 and tab == column.widget(column_index) and column == self._active_column
            label.set_current(is_current)

        # Force style refresh to show active state
        self._handle_style_changed(self._style_manager.zoom_factor)

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

    def _handle_tab_changed(self, _index: int) -> None:
        """
        Handle tab selection changes.

        Args:
            index: Index of the newly selected tab
        """
        # Find which column triggered the change
        sender = self.sender()
        if not sender:
            self._logger.debug("SENDER None")
            return

        self._active_column = sender
        self._logger.debug("TM: tab changed")
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
        self._logger.debug("TM: tab activated")
        self._update_tabs()

    def _handle_column_activated(self, column: TabColumn) -> None:
        """Handle column activation."""
        if column not in self._tab_columns:
            return

        if column == self._active_column:
            return

        self._active_column = column
        self._logger.debug("TM: column activated")
        self._update_tabs()

    def add_tab(self, tab: TabBase, title: str) -> None:
        """
        Add a new tab to the manager.

        Args:
            tab: The tab widget to add
            title: Initial title for the tab
        """
        tab_id = tab.tab_id
        self._tabs[tab_id] = tab

        tab.activated.connect(lambda: self._handle_tab_activated(tab))

        # Create custom tab label
        tab_label = TabLabel(title)
        tab_label.close_clicked.connect(lambda: self._close_tab_by_id(tab_id))
        self._tab_labels[tab_id] = tab_label

        # Add tab with custom label to active column
        index = self._active_column.addTab(tab, "")
        self._active_column.tabBar().setTabButton(index, QTabBar.LeftSide, tab_label)

        # Set initial state
        if len(self._tabs) == 1:  # If this is the first tab
            tab_label.set_current(True)
            self._stack.setCurrentWidget(self._columns_widget)

        self._active_column.setCurrentWidget(tab)

    def _close_tab_by_id(self, tab_id: str) -> None:
        """
        Close a tab by its ID.

        Args:
            tab_id: ID of the tab to close
        """
        tab = self._tabs.get(tab_id)
        if not tab:
            return

        # Check if tab can be closed
        if not tab.can_close():
            return

        # Find which column contains the tab
        column = self._find_column_for_tab(tab)
        if not column:
            return

        # Remove tab
        tab_label = self._tab_labels.pop(tab_id)
        del self._tabs[tab_id]
        index = column.indexOf(tab)
        column.removeTab(index)
        tab_label.deleteLater()
        tab.deleteLater()

        # If we closed the last tab in the column, close the column unless it's the last column
        if column.count() == 0:
            if len(self._tab_columns) > 1:
                column_number = self._tab_columns.index(column)
                del self._tab_columns[column_number]
                column.deleteLater()

                if self._active_column == column:
                    new_active_column = 0 if column_number == 0 else column_number - 1
                    self._active_column = self._tab_columns[new_active_column]

                self.column_state_changed.emit()

        # Show welcome message if no tabs remain
        if not self._tabs:
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

            self.adjustSize()

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

        self._logger.debug("split column")
        self._logger.debug(f"{self._tab_columns}")

        current_column_number = self._get_current_column()
        current_column = self._tab_columns[current_column_number]
        if current_column.count() <= 1:
            return

        target_column_number = current_column_number + (0 if split_left else 1)
        target_column = self._create_column(target_column_number)

        # Delete the current tab from the current column and recreate it in the new column
        old_tab = self._get_current_tab()
        old_tab_id = old_tab.tab_id
        old_tab_state = old_tab.get_state()
        self._logger.debug(f"{old_tab_id}")
        self._logger.debug(f"{self._tab_labels}")
        old_tab_title = self._tab_labels[old_tab_id].text()

        tab_label = self._tab_labels.pop(old_tab_id)
        del self._tabs[old_tab_id]
        index = current_column.indexOf(old_tab)
        current_column.removeTab(index)
        tab_label.deleteLater()
        old_tab.deleteLater()

        # Create appropriate tab type
        if old_tab_state.type == TabType.CONVERSATION:
            tab = ConversationTab.restore_from_state(old_tab_state, self, ai_backends=self._ai_backends)
        elif old_tab_state.type == TabType.EDITOR:
            tab = EditorTab.restore_from_state(old_tab_state, self)
        else:
            return

        # Add to first column
        self._tabs[old_tab_id] = tab

        # Create new label
        tab_label = TabLabel(old_tab_title)
        tab_label.close_clicked.connect(lambda tid=old_tab_id: self._close_tab_by_id(tid))
        self._tab_labels[old_tab_id] = tab_label

        # Add to column
        index = target_column.addTab(tab, "")
        target_column.tabBar().setTabButton(index, QTabBar.LeftSide, tab_label)

        if isinstance(tab, EditorTab):
            self._connect_editor_signals(tab)

        # Resize splitter
        num_columns = len(self._tab_columns)
        sizes = [(self.width() // num_columns) for _ in range(num_columns)]
        self._column_splitter.setSizes(sizes)

        self._active_column = target_column

        self._logger.debug("post split column")
        self._logger.debug(f"{self._tab_columns}")

        # Emit signal about column state change
        self.column_state_changed.emit()

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

        self._logger.debug("merge column")
        self._logger.debug(f"{self._tab_columns}")

        current_column_number = self._get_current_column()
        if (merge_left and current_column_number == 0) or (not merge_left and current_column_number == len(self._tab_columns) -1):
            return

        target_column_number = current_column_number + (-1 if merge_left else 1)
        target_column = self._tab_columns[target_column_number]
        current_column = self._active_column
        self._logger.debug(f"current {current_column_number}, target {target_column_number}, {self._active_column}")
        self._logger.debug(f"{self._tab_columns}")

        # Ensure target column is active
        self._active_column = target_column

        # Record tab states and labels from current column
        tab_states = []
        for i in range(current_column.count()):
            tab = current_column.widget(i)
            self._logger.debug(f"Tab ID: {tab.tab_id}")
            tab_states.append((
                tab.tab_id,
                tab.get_state(),
                self._tab_labels[tab.tab_id].text()
            ))

        # Delete all widgets in current column
        while current_column.count() > 0:
            tab = current_column.widget(0)
            tab_label = self._tab_labels.pop(tab.tab_id)
            del self._tabs[tab.tab_id]
            current_column.removeTab(0)
            tab_label.deleteLater()
            tab.deleteLater()

        # Remove and delete current column widget
        current_column.deleteLater()
        del self._tab_columns[current_column_number]

        # Recreate each tab in target column
        for tab_id, state, title in tab_states:
            # Create appropriate tab type
            if state.type == TabType.CONVERSATION:
                tab = ConversationTab.restore_from_state(state, self, ai_backends=self._ai_backends)
            elif state.type == TabType.EDITOR:
                tab = EditorTab.restore_from_state(state, self)
            else:
                continue

            # Add to first column
            self._tabs[tab_id] = tab

            # Create new label
            tab_label = TabLabel(title)
            tab_label.close_clicked.connect(lambda tid=tab_id: self._close_tab_by_id(tid))
            self._tab_labels[tab_id] = tab_label

            # Add to column
            index = target_column.addTab(tab, "")
            target_column.tabBar().setTabButton(index, QTabBar.LeftSide, tab_label)

            if isinstance(tab, EditorTab):
                self._connect_editor_signals(tab)

        # Resize splitter
        num_columns = len(self._tab_columns)
        sizes = [(self.width() // num_columns) for _ in range(num_columns + 1)]
        self._column_splitter.setSizes(sizes)

        self._logger.debug(f"new active col: {target_column}")

        self._logger.debug("post merge column")
        self._logger.debug(f"{self._tab_columns}")

        # Emit signal about column state change
        self.column_state_changed.emit()

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

    def new_conversation(self, workspace_path: str) -> Optional[str]:
        """Create a new conversation tab and return its ID."""
        # Generate timestamp for ID
        timestamp = datetime.utcnow()
        conversation_id = timestamp.strftime("%Y-%m-%d-%H-%M-%S-%f")[:23]

        # Create path relative to workspace
        filename = os.path.join("conversations", f"{conversation_id}.conv")
        full_path = os.path.join(workspace_path, filename)

        conversation_tab = ConversationTab(
            conversation_id,
            full_path,
            timestamp,
            self._ai_backends,
            self
        )

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
                self._ai_backends,
                self
            )
            self.add_tab(conversation_tab, f"Conv: {conversation_id}")
            return conversation_tab
        except ConversationError:
            raise

    def can_fork_conversation(self) -> bool:
        tab = self._get_current_tab()
        if not tab or not isinstance(tab, ConversationTab):
            return False

        return True

    async def fork_conversation(self) -> None:
        """Fork an existing conversation into a new tab.

        Args:
            conversation_tab: The conversation tab to fork
        """
        conversation_tab = self._get_current_tab()
        if not isinstance(conversation_tab, ConversationTab):
            return

        try:
            # Fork the conversation
            new_tab = await conversation_tab.fork_conversation()

            # Add new tab to manager
            self.add_tab(new_tab, f"Conv: {new_tab.tab_id}")

        except ConversationError as e:
            self._logger.error("Failed to fork conversation: %s", str(e))
            raise

    def save_state(self) -> Dict:
        """Get current state of all tabs."""
        tab_columns = []
        for column in self._tab_columns:
            tab_states = []
            for index in range(column.count()):
                tab = column.widget(index)
                try:
                    state = tab.get_state()
                    state_dict = state.to_dict()
                    tab_states.append(state_dict)
                except Exception as e:
                    self._logger.error("Failed to save tab manager state: %s", str(e))
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

    def _restore_column_state(self, column_index: int, tab_states: List[Dict]) -> None:
        """Restore state for a single column of tabs."""
        for state_dict in tab_states:
            try:
                state = TabState.from_dict(state_dict)

                if not os.path.isabs(state.path):
                    state.path = self._workspace_manager.get_workspace_path(state.path)

                tab = self._restore_tab_from_state(state)
                if not tab:
                    continue

                self._active_column = self._tab_columns[column_index]
                title = self._get_tab_title(tab, state)
                self.add_tab(tab, title)

            except Exception as e:
                self._logger.error("Failed to restore tab manager state: %s", str(e))
                continue

    def _restore_tab_from_state(self, state: TabState) -> Optional[TabBase]:
        """Create appropriate tab type from state."""
        if state.type == TabType.CONVERSATION:
            return ConversationTab.restore_from_state(
                state, self, ai_backends=self._ai_backends
            )
        elif state.type == TabType.EDITOR:
            tab = EditorTab.restore_from_state(state, self)
            self._connect_editor_signals(tab)
            return tab
        return None

    def _connect_editor_signals(self, editor: EditorTab) -> None:
        """Connect standard editor tab signals."""
        editor.title_changed.connect(self._handle_tab_title_changed)
        editor.modified_state_changed.connect(self._handle_tab_modified)

    def _get_tab_title(self, tab: TabBase, state: TabState) -> str:
        """Get appropriate title for tab type."""
        if isinstance(tab, ConversationTab):
            return f"Conv: {tab.tab_id}"
        return os.path.basename(state.path)

    def _handle_style_changed(self, factor: float = 1.0) -> None:
        """
        Handle style changes from StyleManager.

        Args:
            factor: New zoom factor
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
            label.handle_style_changed(factor, is_label_active)

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
        return tab and isinstance(tab, ConversationTab)

    def show_conversation_settings_dialog(self) -> None:
        """Show the conversation settings dialog."""
        tab = self._get_current_tab()
        if not tab or not isinstance(tab, ConversationTab):
            return

        dialog = ConversationSettingsDialog(self, self._ai_backends)
        dialog.set_settings(tab.get_settings())

        if dialog.exec() == QDialog.Accepted:
            tab.update_conversation_settings(dialog.get_settings())

    def handle_esc_key(self) -> bool:
        """Handle processing of the "Esc" key."""
        tab = self._get_current_tab()
        if not tab or not isinstance(tab, ConversationTab):
            return False

        tab.cancel_current_tasks()
        return True
