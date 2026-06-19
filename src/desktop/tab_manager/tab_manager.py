import os
import logging
from typing import Callable, Dict, List, cast

from PySide6.QtWidgets import QWidget, QVBoxLayout, QHBoxLayout, QStackedWidget, QApplication
from PySide6.QtCore import Signal, QTimer, QPoint
from PySide6.QtGui import QPixmap, QResizeEvent

from context.context_info import ContextInfo
from context.context_registry import ContextEvent, ContextRegistry

from desktop.language.language_manager import LanguageManager
from desktop.mindspace.mindspace_manager import MindspaceManager
from desktop.status_message import StatusMessage
from desktop.style_manager import StyleManager
from desktop.tab.tab_base import TabBase
from desktop.tab.tab_state import TabState
from desktop.tab_manager.column_splitter import ColumnSplitter
from desktop.tab_manager.column_widget import ColumnWidget
from desktop.tab_manager.spacer_drop_widget import SpacerDropWidget
from desktop.tab_manager.tab_bar import TabBar
from desktop.tab_manager.tab_carousel import TabCarouselWidget
from desktop.tab_manager.tab_manager_error import TabManagerError
from desktop.tab_manager.tab_overview import TabOverviewEntry, TabOverviewWidget
from desktop.tab_manager.tab_style import build_tab_manager_stylesheet, build_tab_bar_stylesheet
from desktop.tab_manager.welcome_widget import WelcomeWidget
from desktop.user.user_settings import UserSettings

TabFactory = Callable[[TabState, QWidget], "TabBase | None"]
ContextFactory = Callable[[ContextInfo, ContextRegistry, QWidget], "TabBase | None"]


class TabManager(QWidget):
    """Manages multiple tabs across multiple columns."""

    status_message = Signal(StatusMessage)
    tab_changed = Signal()
    tab_closed = Signal(str)
    user_settings_requested = Signal()

    def __init__(
        self,
        open_path: Callable[[str, str], str | None],
        parent: QWidget | None = None
    ) -> None:
        """Initialize the tab manager."""
        super().__init__(parent)

        self._mindspace_manager = MindspaceManager()
        self._language_manager = LanguageManager()
        self._logger = logging.getLogger("TabManager")

        # Subscribe to mindspace open/close so we can wire registry callbacks
        self._mindspace_manager.settings_changed.connect(self._on_mindspace_settings_changed)
        self._registry_subscribed = False

        self.setObjectName("TabManager")

        self._open_path = open_path

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
        self._welcome_widget.path_dropped.connect(self._on_welcome_widget_path_dropped)
        self._welcome_widget.user_settings_requested.connect(self.user_settings_requested.emit)
        self._stack.addWidget(self._welcome_widget)

        # Create widget to hold columns
        self._columns_widget = QWidget()

        # Create splitter for columns before the layout so we can add it immediately
        self._column_splitter = ColumnSplitter()
        self._column_splitter.setHandleWidth(1)

        # Wrap splitter in an HBox with fixed spacers either side for centring
        self._columns_layout = QHBoxLayout(self._columns_widget)
        self._columns_layout.setContentsMargins(0, 0, 0, 0)
        self._columns_layout.setSpacing(0)
        self._left_spacer = SpacerDropWidget()
        self._left_spacer.setFixedWidth(0)
        self._right_spacer = SpacerDropWidget()
        self._right_spacer.setFixedWidth(0)
        self._left_spacer.path_dropped.connect(self._on_left_spacer_path_dropped)
        self._right_spacer.path_dropped.connect(self._on_right_spacer_path_dropped)
        self._left_spacer.tab_dropped.connect(self._on_left_spacer_tab_dropped)
        self._right_spacer.tab_dropped.connect(self._on_right_spacer_tab_dropped)
        self._columns_layout.addWidget(self._left_spacer)
        self._columns_layout.addWidget(self._column_splitter)
        self._columns_layout.addWidget(self._right_spacer)
        self._stack.addWidget(self._columns_widget)

        # Connect to the splitter's moved signal
        self._column_splitter.splitterMoved.connect(self._on_column_splitter_splitter_moved)

        self._style_manager = StyleManager()

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

        self._tab_factories: Dict[str, TabFactory] = {}
        self._context_factories: Dict[str, ContextFactory] = {}
        self._current_status_tab: TabBase | None = None

        # Lazily-created overlays: grid overview and carousel ("recents screens")
        self._tab_overview: TabOverviewWidget | None = None
        self._tab_carousel: TabCarouselWidget | None = None

    def register_tab_factory(self, tool_name: str, factory: TabFactory) -> None:
        """Register a tab factory for session restore.

        Args:
            tool_name: The tool name string (e.g. 'editor', 'conversation').
            factory: Callable(state, parent) -> TabBase | None.
        """
        self._tab_factories[tool_name] = factory

    def register_context_factory(self, tool_name: str, factory: ContextFactory) -> None:
        """Register a context factory for live context-open events.

        Args:
            tool_name: The tool name string (e.g. 'editor', 'conversation').
            factory: Callable(info, registry, parent) -> TabBase | None.
        """
        self._context_factories[tool_name] = factory

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
            - type: Type of tab (conversation, editor, preview, etc.)
            - path: File path (if applicable, relative to mindspace)
            - column_index: Index of the column containing the tab
            - is_active: Whether this tab is currently active in its column
            - is_active_column: Whether this tab's column is the active column
            - is_modified: Whether the tab has unsaved changes
            - is_ephemeral: Whether the tab is temporary
        """
        tab_id = tab.tab_id()
        tab_index, tab_bar = self._find_tab_bar_and_index(tab)

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
            "title": tab_bar.get_tab_text(tab_index) if tab_bar and tab_index != -1 else "",
            "type": tab.tool_name(),
            "path": relative_path,
            "column_index": column_index,
            "is_modified": tab.is_modified(),
            "is_ephemeral": tab.is_ephemeral()
        }

    def get_tab_info_by_id(self, tab_id: str) -> Dict[str, str | int | bool] | None:
        """
        Get information about a specific tab or the current tab.

        Args:
            tab_id: ID of the tab to get information for. If None, uses the current tab.

        Returns:
            Dictionary containing tab information or None if tab not found
        """
        tab = self._tabs.get(tab_id)
        if not tab:
            return None

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

    def get_all_tabs(self) -> List[TabBase]:
        """Return all currently open tabs."""
        return list(self._tabs.values())

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

    def move_tab_to_column(self, tab_id: str, target_column_index: int) -> None:
        """
        Move a tab to a specific column by index.

        Args:
            tab_id: ID of the tab to move
            target_column_index: Index of the target column (0-based)

        Raises:
            TabManagerError: If target_column_index is invalid or tab_id doesn't exist
        """
        tab = self._tabs.get(tab_id)
        if not tab:
            raise TabManagerError(f"Tab with ID '{tab_id}' not found")

        source_column = self._find_column_for_tab(tab)
        if source_column is None:
            raise TabManagerError(f"Could not find column for tab '{tab_id}'")

        if target_column_index < 0:
            raise TabManagerError(f"Target column index must be non-negative, got {target_column_index}")

        if target_column_index >= 6:
            raise TabManagerError(f"Target column index must be less than 6, got {target_column_index}")

        if target_column_index >= len(self._tab_columns):
            self._create_column(len(self._tab_columns))
            target_column_index = len(self._tab_columns) - 1

        target_column = self._tab_columns[target_column_index]

        if source_column == target_column:
            return

        self._move_tab_between_columns(tab, source_column, target_column)

        self._active_column = target_column
        self.show_all_columns()
        self._update_tabs()

    def _update_mru_order(self, tab: TabBase, column: ColumnWidget) -> None:
        """
        Update the MRU order when a tab is activated.

        Args:
            tab: The tab that was activated
            column: The column containing the tab
        """
        tab_id = tab.tab_id()
        mru_list = self._column_mru_order[column]

        if tab_id in mru_list:
            mru_list.remove(tab_id)

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
                break

    def _on_tab_label_close_clicked(self, tab_id: str) -> None:
        """
        Handle close button click on a tab label.

        Args:
            tab_id: ID of the tab to close
        """
        self.close_tab_by_id(tab_id)
        self.tab_closed.emit(tab_id)

    def _on_tab_label_double_clicked(self, tab_id: str) -> None:
        """
        Handle double-click on a tab label, making ephemeral tabs persistent.

        Args:
            tab_id: ID of the tab that was double-clicked
        """
        tab = self._tabs.get(tab_id)
        if tab is None:
            return

        if tab.is_ephemeral():
            self._make_tab_permanent(tab)

    def _on_tab_label_context_menu(self, tab_id: str, global_pos: QPoint) -> None:
        """
        Show the context menu for a tab label.

        Args:
            tab_id: ID of the tab that was right-clicked
            global_pos: Global position at which to show the menu
        """
        tab = self._tabs.get(tab_id)
        if tab is None:
            return

        column = self._find_column_for_tab(tab)
        if column is None:
            return

        tab_index = column.indexOf(tab)
        tab_count = column.count()

        strings = self._language_manager.strings()
        menu = self._style_manager.create_menu(self)

        # In right-to-left layouts the visual order is mirrored, so "left"
        # corresponds to higher tab indexes and "right" to lower ones
        left_to_right = self._language_manager.left_to_right()
        has_tabs_before = tab_index > 0
        has_tabs_after = tab_index < tab_count - 1

        close_left_action = menu.addAction(strings.close_tabs_to_left)
        close_left_action.setEnabled(has_tabs_before if left_to_right else has_tabs_after)

        close_right_action = menu.addAction(strings.close_tabs_to_right)
        close_right_action.setEnabled(has_tabs_after if left_to_right else has_tabs_before)

        close_others_action = menu.addAction(strings.close_other_tabs)
        close_others_action.setEnabled(tab_count > 1)

        action = menu.exec_(global_pos)
        if action is None:
            return

        close_before_action = close_left_action if left_to_right else close_right_action
        close_after_action = close_right_action if left_to_right else close_left_action

        if action == close_before_action:
            self._close_tabs_in_column(column, list(range(0, tab_index)))

        elif action == close_after_action:
            self._close_tabs_in_column(column, list(range(tab_index + 1, tab_count)))

        elif action == close_others_action:
            self._close_tabs_in_column(column, [i for i in range(tab_count) if i != tab_index])

    def _close_tabs_in_column(self, column: ColumnWidget, indexes: List[int]) -> None:
        """
        Close the tabs at the given indexes within a column.

        Args:
            column: Column containing the tabs
            indexes: Tab indexes to close, relative to the column's current layout
        """
        tab_ids = [cast(TabBase, column.widget(i)).tab_id() for i in indexes]
        for close_id in tab_ids:
            self.close_tab_by_id(close_id)
            if close_id not in self._tabs:
                self.tab_closed.emit(close_id)

    def _on_tab_updated_state_changed(self, _tab_id: str, _is_updated: bool) -> None:
        """
        Update a tab's updated state.

        Args:
            tab_id: ID of the tab to update
            is_updated: Whether the tab has updated content
        """
        self._update_tabs(change_focus=False)

    def _on_tab_file_state_changed(self, tab_id: str, file_exists: bool) -> None:
        """
        Update a tab's file state.

        Args:
            tab_id: ID of the tab to update
            file_exists: Whether the tab's file exists
        """
        tab = self._tabs.get(tab_id)
        if tab:
            tab_index, tab_bar = self._find_tab_bar_and_index(tab)
            if tab_bar and tab_index != -1:
                tab_bar.set_tab_file_missing(tab_index, not file_exists)

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

        del self._tabs[tab_id]
        self._mindspace_manager.mindspace().contexts().close(tab_id)
        index = column.indexOf(tab)
        tab_bar = column.tabBar()
        if isinstance(tab_bar, TabBar):
            tab_bar.remove_tab_data(tab_id)

        column.removeTab(index)
        tab.deleteLater()
        QTimer.singleShot(0, self.show_all_columns)

    def _on_mindspace_settings_changed(self) -> None:
        """Wire or unwire registry callbacks when a mindspace opens or closes."""
        if self._mindspace_manager.has_mindspace():
            self._subscribe_to_registry()

        else:
            self._unsubscribe_from_registry()

    def _subscribe_to_registry(self) -> None:
        """Register TabManager as a subscriber to the active ContextRegistry."""
        if self._registry_subscribed:
            return

        registry = self._mindspace_manager.mindspace().contexts()
        registry.register_callback(ContextEvent.OPENED, self._on_context_opened)
        registry.register_callback(ContextEvent.CLOSED, self._on_context_closed)
        registry.register_callback(ContextEvent.UPDATED, self._on_context_updated)
        registry.register_callback(ContextEvent.FOCUSED, self._on_context_focused)
        self._registry_subscribed = True

    def _unsubscribe_from_registry(self) -> None:
        """Unregister TabManager from the ContextRegistry."""
        if not self._registry_subscribed:
            return

        if self._mindspace_manager.has_mindspace():
            registry = self._mindspace_manager.mindspace().contexts()
            registry.unregister_callback(ContextEvent.OPENED, self._on_context_opened)
            registry.unregister_callback(ContextEvent.CLOSED, self._on_context_closed)
            registry.unregister_callback(ContextEvent.UPDATED, self._on_context_updated)
            registry.unregister_callback(ContextEvent.FOCUSED, self._on_context_focused)

        self._registry_subscribed = False

    def _on_context_opened(self, info: ContextInfo, is_ephemeral: bool, requester_id: str = "") -> None:
        """
        React to a context being opened in the registry.

        If the tab_id is already tracked (e.g. session restore called _add_tab
        before calling registry.open), we skip tab creation to avoid duplicates.
        We do still register context models for tabs created via the restore path.
        """
        context_id = info.context_id

        if context_id in self._tabs:
            # Tab already exists (session restore path) — just ensure context
            # models are registered for this tab.
            existing_tab = self._tabs[context_id]
            self._apply_context_models(existing_tab)
            return

        registry = self._mindspace_manager.mindspace().contexts()
        try:
            factory = self._context_factories.get(info.context_type)
            if factory is None:
                self._logger.warning("No context factory registered for type '%s'", info.context_type)
                return

            new_tab = factory(info, registry, self)

        except Exception:
            self._logger.exception(
                "Failed to create tab for context %s (%s)", context_id, info.context_type
            )
            return

        if new_tab is None:
            return

        new_tab.set_ephemeral(is_ephemeral)
        title = new_tab.tab_title_from_path() or info.title
        self._add_tab(new_tab, title, requester_id)
        self._apply_context_models(new_tab)

    def _apply_context_models(self, tab: TabBase) -> None:
        """Register context models for a tab by delegating to the tab itself."""
        if not self._mindspace_manager.has_mindspace():
            return

        tab.register_context_models(self._mindspace_manager.mindspace().contexts())

    def _on_context_closed(self, context_id: str) -> None:
        """React to a context being closed in the registry — close the Qt tab."""
        if context_id in self._tabs:
            self.close_tab_by_id(context_id, force_close=True)

    def _on_context_updated(self, info: ContextInfo) -> None:
        """React to a context being updated — currently a no-op at the Qt layer."""

    def _on_context_focused(self, context_id: str) -> None:
        """React to a context focus request — bring the Qt tab to front."""
        tab = self._tabs.get(context_id)
        if tab is not None:
            self._set_current_tab(tab, False)

    def _add_tab_to_column(self, tab: TabBase, title: str, column: ColumnWidget) -> None:
        """
        Add a tab to a column and set up associated data.

        Args:
            tab: Tab widget to add
            title: Initial title for the tab
            column: Target column
        """
        tool_tip = tab.path()
        if tool_tip:
            tool_tip = self._mindspace_manager.get_relative_path(tool_tip)

        tab_id = tab.tab_id()
        tab.activated.connect(lambda: self._on_tab_activated(tab))
        tab.updated_state_changed.connect(self._on_tab_updated_state_changed)
        tab.modified_state_changed.connect(self._on_tab_modified_state_changed)
        tab.file_state_changed.connect(self._on_tab_file_state_changed)
        tab.tab_label_changed.connect(self._on_tab_label_changed)
        tab.close_requested.connect(lambda: self.close_tab_by_id(tab_id, force_close=True))

        self._tabs[tab_id] = tab

        index = column.addTab(tab, "")
        tab_bar = column.tabBar()
        assert isinstance(tab_bar, TabBar)
        tab_bar.add_tab_data(
            index, tab_id, tab.tab_icon(), title, tool_tip,
            is_ephemeral=tab.is_ephemeral(),
            is_file_missing=tab.is_path_missing(),
        )
        focus_widget = QApplication.focusWidget()
        column.setCurrentWidget(tab)

        self._update_tabs(change_focus=False)
        self._update_mru_order(tab, column)
        QTimer.singleShot(0, self.show_all_columns)

        # Only restore focus to the prior widget if it was outside the target column
        # (e.g. the sidebar).  If focus was already inside the column, let it move to the new tab.
        if focus_widget is not None and not column.isAncestorOf(focus_widget):
            self._activation_timer.stop()
            QTimer.singleShot(0, focus_widget.setFocus)

    def _move_tab_between_columns(
        self,
        tab: TabBase,
        source_column: ColumnWidget,
        target_column: ColumnWidget,
        remove_if_empty: bool = True
    ) -> None:
        """
        Move a tab from one column to another.

        Args:
            tab: Tab to move
            source_column: Source column
            target_column: Target column
            remove_if_empty: Whether to remove the source column if it becomes empty
        """
        tab_state = tab.get_state(True)

        # Moving a tab is a deliberate user action, so it is no longer ephemeral
        tab_state.is_ephemeral = False

        tab_id = tab.tab_id()
        src_index, src_tab_bar = self._find_tab_bar_and_index(tab)
        tab_title = src_tab_bar.get_tab_text(src_index) if src_tab_bar and src_index != -1 else ""

        source_mru = self._column_mru_order[source_column]
        if tab_id in source_mru:
            source_mru.remove(tab_id)

        tab.stop_file_watching()

        self._remove_tab_from_column(tab, source_column)

        new_tab = self._restore_tab_from_state(tab_state)
        if not new_tab:
            return

        self._add_tab_to_column(new_tab, tab_title, target_column)
        self._apply_context_models(new_tab)

        if remove_if_empty and source_column.count() == 0 and len(self._tab_columns) > 1:
            source_column_index = self._tab_columns.index(source_column)
            self._remove_column_and_resize(source_column_index, source_column)

    def _on_welcome_widget_path_dropped(self, source_type: str, path: str) -> None:
        """Handle mindspace tree drops when only welcome widget is visible."""
        if not self._tab_columns:
            self._create_column(0)

        self._active_column = self._tab_columns[0]

        existing = self._find_existing_tab(source_type, path)
        context_id = existing.tab_id() if existing else self._open_path(source_type, path)
        if context_id is not None:
            self._stack.setCurrentWidget(self._columns_widget)

    def _on_left_spacer_path_dropped(self, source_type: str, path: str) -> None:
        """Handle a path dropped onto the left spacer, opening it in a new leftmost column."""
        new_column = self._create_column(0)
        self._active_column = new_column
        self._stack.setCurrentWidget(self._columns_widget)

        existing = self._find_existing_tab(source_type, path)
        if existing is not None:
            source_column = self._find_column_for_tab(existing)
            if source_column and source_column != new_column:
                self._move_tab_between_columns(existing, source_column, new_column)

            self._update_tabs()

        else:
            context_id = self._open_path(source_type, path)
            if context_id is not None:
                self._update_tabs()

            elif new_column.count() == 0 and len(self._tab_columns) > 1:
                self._remove_column_and_resize(0, new_column)

    def _on_right_spacer_path_dropped(self, source_type: str, path: str) -> None:
        """Handle a path dropped onto the right spacer, opening it in a new rightmost column."""
        new_index = len(self._tab_columns)
        new_column = self._create_column(new_index)
        self._active_column = new_column
        self._stack.setCurrentWidget(self._columns_widget)

        existing = self._find_existing_tab(source_type, path)
        if existing is not None:
            source_column = self._find_column_for_tab(existing)
            if source_column and source_column != new_column:
                self._move_tab_between_columns(existing, source_column, new_column)

            self._update_tabs()
        else:
            context_id = self._open_path(source_type, path)
            if context_id is not None:
                self._update_tabs()

            elif new_column.count() == 0 and len(self._tab_columns) > 1:
                self._remove_column_and_resize(new_index, new_column)

    def _on_left_spacer_tab_dropped(self, tab_id: str) -> None:
        """Handle a tab dropped onto the left spacer, moving it to a new leftmost column."""
        tab = self._tabs.get(tab_id)
        if not tab:
            return

        source_column = self._find_column_for_tab(tab)
        if source_column is None:
            return

        new_column = self._create_column(0)
        self._active_column = new_column

        self._move_tab_between_columns(tab, source_column, new_column)

        self.show_all_columns()
        self._update_tabs()

    def _on_right_spacer_tab_dropped(self, tab_id: str) -> None:
        """Handle a tab dropped onto the right spacer, moving it to a new rightmost column."""
        tab = self._tabs.get(tab_id)
        if not tab:
            return

        source_column = self._find_column_for_tab(tab)
        if source_column is None:
            return

        new_index = len(self._tab_columns)
        new_column = self._create_column(new_index)
        self._active_column = new_column

        self._move_tab_between_columns(tab, source_column, new_column)

        self.show_all_columns()
        self._update_tabs()

    def _on_column_splitter_splitter_moved(self, _pos: int, _index: int) -> None:
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

                    while source_column.count() > 0:
                        tab = cast(TabBase, source_column.widget(0))
                        self._move_tab_between_columns(tab, source_column, target_column, remove_if_empty=False)

                    self._remove_column_and_resize(i, source_column)
                    self._update_tabs()
                    break

    def _on_column_widget_tab_dropped(self, tab_id: str, target_column: ColumnWidget, target_index: int) -> None:
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

        if source_column == target_column:
            # Simple reorder within the same column — no teardown needed
            adjusted_index = target_index if source_index > target_index else target_index - 1
            if adjusted_index != source_index:
                target_column.tabBar().moveTab(source_index, adjusted_index)
        else:
            self._move_tab_between_columns(tab, source_column, target_column)
            new_tab = self._tabs.get(tab_id)
            current_index = target_column.indexOf(new_tab) if new_tab else -1
            if current_index not in (-1, target_index):
                target_column.tabBar().moveTab(current_index, target_index)

        self._active_column = target_column
        self._update_tabs()

    def _on_column_widget_path_dropped(
        self,
        source_type: str,
        path: str,
        target_column: ColumnWidget,
        target_index: int
    ) -> None:
        """
        Handle a file being dropped into a column.

        Args:
            source_type: Source view type if available
            path: Path dropped
            target_column: Column where the file was dropped
            target_index: Target position in the column
        """
        self._active_column = target_column

        target_column_index = self._tab_columns.index(target_column)
        existing = self._find_existing_tab(source_type, path)
        context_id = existing.tab_id() if existing else self._open_path(source_type, path)
        if context_id is None:
            return

        if existing is not None:
            source_column = self._find_column_for_tab(existing)
            if source_column and source_column != target_column:
                self._move_tab_between_columns(existing, source_column, target_column)

        self.reposition_tab(context_id, target_column_index, target_index)
        self._update_tabs()

    def reposition_tab(self, tab_id: str, column_index: int, target_index: int) -> None:
        """Reposition a tab to a specific index within a specific column.

        Args:
            tab_id: ID of the tab to reposition.
            column_index: Index of the column containing the tab.
            target_index: Target position within the column's tab bar.
        """
        if column_index < 0 or column_index >= len(self._tab_columns):
            return

        column = self._tab_columns[column_index]
        tab = self._tabs.get(tab_id)
        if tab is None:
            return

        current_index = column.indexOf(tab)
        if current_index not in (-1, target_index):
            column.tabBar().moveTab(current_index, target_index)

    def toggle_tab_overview(self) -> None:
        """Show the tab overview, or cycle its selection if it is already visible."""
        if self._tab_overview is not None and self._tab_overview.isVisible():
            self._tab_overview.cycle_selection()
            return

        self.show_tab_overview()

    def show_tab_overview(self) -> None:
        """Show an overview of all open tabs as a grid of thumbnail cards."""
        entries = self._build_overview_entries()
        if not entries:
            return

        self._hide_tab_carousel()

        if self._tab_overview is None:
            self._tab_overview = TabOverviewWidget(self)
            self._tab_overview.tab_activated.connect(self._on_overview_tab_activated)
            self._tab_overview.tab_close_requested.connect(self._on_overview_tab_close_requested)
            self._tab_overview.dismissed.connect(self._hide_tab_overview)

        self._tab_overview.set_entries(entries)
        self._tab_overview.setGeometry(self.rect())
        self._tab_overview.show()
        self._tab_overview.raise_()
        self._tab_overview.setFocus()

    def _hide_tab_overview(self) -> None:
        """Hide the tab overview overlay and return focus to the current tab."""
        if self._tab_overview is None or not self._tab_overview.isVisible():
            return

        self._tab_overview.hide()
        self._activation_timer.start()

    def toggle_tab_carousel(self) -> None:
        """Show the tab carousel, or advance it if it is already visible."""
        if self._tab_carousel is not None and self._tab_carousel.isVisible():
            self._tab_carousel.select_next()
            return

        self.show_tab_carousel()

    def show_tab_carousel(self) -> None:
        """Show a carousel of all open tabs, centred on the current tab."""
        entries = self._build_overview_entries()
        if not entries:
            return

        self._hide_tab_overview()

        if self._tab_carousel is None:
            self._tab_carousel = TabCarouselWidget(self)
            self._tab_carousel.tab_activated.connect(self._on_carousel_tab_activated)
            self._tab_carousel.tab_close_requested.connect(self._on_carousel_tab_close_requested)
            self._tab_carousel.dismissed.connect(self._hide_tab_carousel)

        self._tab_carousel.set_entries(entries)
        self._tab_carousel.setGeometry(self.rect())
        self._tab_carousel.show()
        self._tab_carousel.raise_()
        self._tab_carousel.setFocus()

    def _hide_tab_carousel(self) -> None:
        """Hide the tab carousel overlay and return focus to the current tab."""
        if self._tab_carousel is None or not self._tab_carousel.isVisible():
            return

        self._tab_carousel.hide()
        self._activation_timer.start()

    def _hide_tab_overlays(self) -> None:
        """Hide both the overview and carousel overlays."""
        self._hide_tab_overview()
        self._hide_tab_carousel()

    def _on_carousel_tab_activated(self, tab_id: str) -> None:
        """Switch to the tab whose carousel card was clicked or chosen."""
        self._hide_tab_carousel()
        tab = self._tabs.get(tab_id)
        if tab is not None:
            self._set_current_tab(tab, False)

    def _on_carousel_tab_close_requested(self, tab_id: str) -> None:
        """Close the tab whose carousel close button was clicked."""
        self.close_tab_by_id(tab_id)
        if tab_id not in self._tabs:
            self.tab_closed.emit(tab_id)

    def _build_overview_entries(self) -> List[TabOverviewEntry]:
        """Build display records for every open tab, in column then tab order."""
        entries: List[TabOverviewEntry] = []
        for column in self._tab_columns:
            is_active_column = column == self._active_column
            tab_bar = column.tabBar()
            original_current = column.currentWidget()

            for i in range(column.count()):
                tab = cast(TabBase, column.widget(i))
                title = tab_bar.get_tab_text(i) if isinstance(tab_bar, TabBar) else ""
                is_current = is_active_column and column.currentWidget() is tab
                if tab.thumbnail() is None:
                    column.setCurrentWidget(tab)
                    tab.capture_thumbnail()

                thumbnail = tab.grab() if is_current else tab.thumbnail()
                entries.append(TabOverviewEntry(
                    tab_id=tab.tab_id(),
                    icon_name=tab.tab_icon(),
                    title=title,
                    thumbnail=thumbnail or QPixmap(),
                    is_current=is_current,
                ))

            if column.currentWidget() is not original_current:
                column.setCurrentWidget(original_current)

        return entries

    def _on_overview_tab_activated(self, tab_id: str) -> None:
        """Switch to the tab whose overview card was clicked."""
        self._hide_tab_overview()
        tab = self._tabs.get(tab_id)
        if tab is not None:
            self._set_current_tab(tab, False)

    def _on_overview_tab_close_requested(self, tab_id: str) -> None:
        """Close the tab whose overview card was swiped away."""
        self.close_tab_by_id(tab_id)
        if tab_id not in self._tabs:
            self.tab_closed.emit(tab_id)
            return

        # The close was vetoed (e.g. the user cancelled a save prompt)
        if self._tab_overview is not None:
            self._tab_overview.restore_card(tab_id)

    def _update_tab_bar_for_label_change(self, tab: TabBase) -> None:
        """
        Efficiently update the tab bar after a label text change.

        Args:
            tab: The tab whose label changed
        """
        column = self._find_column_for_tab(tab)
        if column is None:
            return

        tab_bar = cast(TabBar, column.tabBar())
        tab_bar.updateGeometry()

    def handle_file_rename(self, old_path: str, new_path: str) -> None:
        """
        Handle renaming of files by updating any open tabs.

        Args:
            old_path: Original path of renamed file
            new_path: New path after renaming
        """
        for tab in self._tabs.values():
            if tab.path() == old_path:
                tab.on_path_renamed(new_path)
                contexts = self._mindspace_manager.mindspace().contexts()
                contexts.update(
                    tab.tab_id(),
                    path=new_path,
                    title=os.path.basename(new_path),
                )

    def _create_column(self, index: int) -> ColumnWidget:
        """Create a new tab column."""
        column_widget = ColumnWidget()
        column_widget.setMinimumWidth(200)
        column_widget.currentChanged.connect(self._on_column_widget_current_changed)
        column_widget.column_activated.connect(self._on_column_widget_column_activated)
        column_widget.tab_dropped.connect(self._on_column_widget_tab_dropped)
        column_widget.path_dropped.connect(self._on_column_widget_path_dropped)

        self._column_splitter.insertWidget(index, column_widget)
        self._tab_columns.insert(index, column_widget)

        self._column_mru_order[column_widget] = []

        tab_bar = column_widget.tabBar()
        if tab_bar:
            tab_bar.setStyleSheet(build_tab_bar_stylesheet(self._style_manager))

        tab_bar = column_widget.tabBar()
        if isinstance(tab_bar, TabBar):
            tab_bar.close_clicked.connect(self._on_tab_label_close_clicked)
            tab_bar.double_clicked.connect(self._on_tab_label_double_clicked)
            tab_bar.context_menu_requested.connect(self._on_tab_label_context_menu)

        self.show_all_columns()
        return column_widget

    def _remove_column_and_resize(self, column_number: int, column: ColumnWidget) -> None:
        """
        Remove a column and resize the remaining columns.

        Args:
            column_number: Index of the column to remove
            column: Column widget to remove
        """
        if column in self._column_mru_order:
            del self._column_mru_order[column]

        if self._active_column == column and len(self._tab_columns) > 1:
            new_active_index = column_number - 1 if column_number > 0 else 1
            self._active_column = self._tab_columns[new_active_index]

        del self._tab_columns[column_number]
        column.deleteLater()

        # Defer resizing to allow the column to be removed first.
        QTimer.singleShot(0, self.show_all_columns)

    def _update_tabs(self, change_focus: bool=True) -> None:
        """ Update the state of all tabs and their labels. """
        for tab in self._tabs.values():
            column = self._find_column_for_tab(tab)
            if column is None:
                continue

            column_index = column.currentIndex()
            is_current = column_index != -1 and tab == column.widget(column_index)
            is_active_column = column == self._active_column
            is_updated = tab.is_updated()
            has_seen_latest_update = tab.has_seen_latest_update()

            # If tab becomes current, clear its updated state
            if is_current and is_updated and has_seen_latest_update:
                tab.set_updated(False)
                is_updated = False

            tab_index = column.indexOf(tab)
            if tab_index != -1 and isinstance(column.tabBar(), TabBar):
                tab_bar = cast(TabBar, column.tabBar())
                tab_bar.set_tab_state(tab_index, is_current, is_updated, is_active_column)

        for column in self._tab_columns:
            if isinstance(column.tabBar(), TabBar):
                column.tabBar().update()

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
        current_tab.capture_thumbnail()

    def _refresh_current_tab_thumbnails(self) -> None:
        """Invalidate all cached thumbnails after a resize so the next overview rebuild re-grabs them all."""
        for tab in self._tabs.values():
            tab.invalidate_thumbnail()

    def _on_column_widget_current_changed(self, _index: int) -> None:
        """
        Handle tab selection changes.

        Args:
            index: Index of the newly selected tab
        """
        # Find which column triggered the change
        sender = self.sender()
        column = cast(ColumnWidget, sender)
        self._active_column = column

        # Update MRU order for the newly selected tab.  Also check if we need to update focus.
        # We don't want to change focus if the current tab is ephemeral.
        current_tab = self.get_current_tab()
        update_focus = True
        if current_tab:
            self._update_mru_order(current_tab, column)
            update_focus = not current_tab.is_ephemeral()

        self._update_tabs(change_focus=update_focus)

    def _on_tab_activated(self, tab: TabBase) -> None:
        """
        Handle tab activation from widget focus.

        Args:
            tab: The tab that was activated
        """
        column = self._find_column_for_tab(tab)
        if column is None:
            return

        if column == self._active_column:
            return

        self._active_column = column
        self._update_mru_order(tab, column)
        self._update_tabs()

    def _on_column_widget_column_activated(self, column: ColumnWidget) -> None:
        """Handle column activation."""
        if column not in self._tab_columns:
            return

        if column == self._active_column:
            return

        self._active_column = column
        self._update_tabs()

    def _get_target_column_for_new_tab(self, requester_id: str = "") -> ColumnWidget:
        """
        Determine which column should receive a new tab.

        If a requester_id is provided, place the new tab in a column adjacent
        to the requester rather than in the same column.

        Returns:
            The column widget where the new tab should be added
        """
        if not requester_id:
            return self._active_column

        requester_tab = self._tabs.get(requester_id)
        requester_column = self._find_column_for_tab(requester_tab) if requester_tab else None
        current_column_number = self._tab_columns.index(requester_column) if \
            requester_column else self._tab_columns.index(self._active_column)

        num_columns = len(self._tab_columns)
        if current_column_number == (num_columns - 1) and num_columns < 6:
            new_column = self._create_column(num_columns)
            self.show_all_columns()
            return new_column

        if current_column_number < num_columns - 1:
            return self._tab_columns[current_column_number + 1]

        return self._tab_columns[current_column_number - 1]

    def _add_tab(self, tab: TabBase, title: str, requester_id: str = "") -> None:
        """
        Add a new tab to the manager.

        Args:
            tab: The tab widget to add
            title: Initial title for the tab
            requester_id: Optional ID of the tab requesting this open, used
                          to determine placement column.
        """
        if len(self._tabs) == 0:
            # If no tabs exist, we need to switch to the columns widget
            self._stack.setCurrentWidget(self._columns_widget)

        # A newly opened tab supersedes the overlay views
        self._hide_tab_overlays()

        prior_active_column = self._active_column
        target_column = self._get_target_column_for_new_tab(requester_id)

        self._add_tab_to_column(tab, title, target_column)

        # If the new tab landed in a column that wasn't already active, restore the previously
        # active column so that opening a tab doesn't implicitly steal column activation
        if target_column != prior_active_column:
            self._active_column = prior_active_column
            self._update_tabs(change_focus=False)

        # Close any ephemeral tab in target column because we've just added a new one
        self._close_ephemeral_tab_in_column(target_column, tab)

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

        if not force_close and not tab.can_close_tab():
            return

        tab.close_tab()

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

        # Keep the overlay views in sync if they're showing
        if self._tab_overview is not None and self._tab_overview.isVisible():
            self._tab_overview.remove_card(tab_id)
            if self._tab_overview.card_count() == 0:
                self._hide_tab_overview()

        if self._tab_carousel is not None and self._tab_carousel.isVisible():
            self._tab_carousel.remove_entry(tab_id)
            if self._tab_carousel.entry_count() == 0:
                self._hide_tab_carousel()

        # If we closed the last tab in the column, close the column unless it's the last column
        if column.count() == 0:
            if len(self._tab_columns) > 1:
                column_number = self._tab_columns.index(column)
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

    def _find_tab_bar_and_index(self, tab: TabBase) -> tuple[int, TabBar | None]:
        """Find the TabBar and index for the given tab.

        Returns:
            Tuple of (index, TabBar) or (-1, None) if not found.
        """
        column = self._find_column_for_tab(tab)
        if column is None:
            return -1, None

        tab_bar = column.tabBar()
        if not isinstance(tab_bar, TabBar):
            return -1, None

        return column.indexOf(tab), tab_bar

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
        tab_index, tab_bar = self._find_tab_bar_and_index(tab)
        if tab_bar and tab_index != -1:
            tab_bar.set_tab_ephemeral(tab_index, False)

    def make_tab_permanent(self, tab_id: str) -> None:
        """Make an ephemeral tab permanent.

        Args:
            tab_id: ID of the tab to make permanent.
        """
        tab = self._tabs.get(tab_id)
        if tab is None or not tab.is_ephemeral():
            return

        self._make_tab_permanent(tab)

    def _move_tab_to_active_column(self, tab: TabBase) -> None:
        """
        Move a tab to the active column if it is not already there.

        If the move leaves the source column empty it is removed.

        Args:
            tab: The tab to move
        """
        source_column = self._find_column_for_tab(tab)
        if source_column is None or source_column == self._active_column:
            return

        self._move_tab_between_columns(tab, source_column, self._active_column)

    def _on_tab_modified_state_changed(self, tab_id: str, modified: bool) -> None:
        """
        Update a tab's modified state.

        Args:
            tab_id: ID of the tab to update
            modified: Whether the tab is modified
        """
        tab = self._tabs.get(tab_id)
        if not tab:
            return

        if modified:
            self._make_tab_permanent(tab)

        tab.on_modified_changed(modified)

        if self._mindspace_manager.has_mindspace():
            self._mindspace_manager.mindspace().contexts().update(tab_id, is_modified=modified)

    def _on_tab_label_changed(self, tab_id: str, new_title: str) -> None:
        """Update the tab bar label when a tab signals its title has changed."""
        tab = self._tabs.get(tab_id)
        if not tab:
            return

        tab_index, tab_bar = self._find_tab_bar_and_index(tab)
        if tab_bar and tab_index != -1:
            tab_bar.set_tab_text(tab_index, new_title)

        self._update_tab_bar_for_label_change(tab)

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

        current_tab = self.get_current_tab()
        if not current_tab:
            return

        self._move_tab_between_columns(current_tab, current_column, target_column)
        self.show_all_columns()

        self._active_column = target_column
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

        while current_column.count() > 0:
            tab = cast(TabBase, current_column.widget(0))
            self._move_tab_between_columns(tab, current_column, target_column, remove_if_empty=False)

        self._active_column = target_column
        column_number = self._tab_columns.index(current_column)
        self._remove_column_and_resize(column_number, current_column)

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

        self._tab_columns[current_column_number], self._tab_columns[target_column_number] = \
            self._tab_columns[target_column_number], self._tab_columns[current_column_number]

        temp_source.deleteLater()
        temp_target.deleteLater()

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

    def _find_tab_by_path(self, tool_name: str, path: str) -> TabBase | None:
        """Find an open tab by tool name and path.

        Args:
            tool_name: The tool name to match (e.g. 'editor', 'conversation').
            path: The path to search for.

        Returns:
            The matching tab if found, None otherwise.
        """
        for tab in self._tabs.values():
            if tab.tool_name() == tool_name and tab.path() == path:
                return tab

        return None

    def _find_existing_tab(self, source_type: str, path: str) -> TabBase | None:
        """Find an already-open tab matching the given drag-drop source type and path.

        Handles the source_type to tool_name mapping and any path transformation
        needed (e.g. relative→absolute for conversations), then delegates to
        _find_tab_by_path.  Does not open, focus, or modify anything.

        Args:
            source_type: Source view type string ('conversations', 'vcs', 'preview', 'files').
            path: File path as carried in the drop mime data.

        Returns:
            The matching TabBase if found, None otherwise.
        """
        if source_type == "conversations":
            return self._find_tab_by_path(
                "conversation", self._mindspace_manager.get_absolute_path(path)
            )

        if source_type == "vcs":
            return self._find_tab_by_path("diff", path)

        if source_type == "preview":
            return self._find_tab_by_path("preview", path)

        return self._find_tab_by_path("editor", path)

    def save_state(self) -> Dict:
        """Get current state of all tabs and columns."""
        tab_columns = []
        active_column_index = self._tab_columns.index(self._active_column)

        for column in self._tab_columns:
            tab_states = []
            active_tab_id = None

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
        """Create a tab from a saved state using the registered factory."""
        factory = self._tab_factories.get(state.type)
        if factory is None:
            self._logger.warning("No factory registered for tool '%s'", state.type)
            return None

        return factory(state, self)

    def _restore_column_state(self, column_index: int, tab_states: List[Dict]) -> None:
        """
        Restore state for a single column of tabs.

        Args:
            column_index: Index of the column to restore
            tab_states: List of tab states to restore in this column
        """
        for state_dict in tab_states:
            try:
                state = TabState.from_dict(state_dict)
                state.path = self._mindspace_manager.get_absolute_path(state.path)

                factory = self._tab_factories.get(state.type)
                if factory is None:
                    self._logger.info(
                        "Skipping tab restore for type '%s', path: %s", state.type, state.path
                    )
                    continue

                cls = getattr(factory, '__self__', None)
                if cls is not None and callable(getattr(cls, 'can_restore', None)) and not cls.can_restore(state.path):
                    self._logger.info(
                        "Skipping tab restore for type '%s', path: %s", state.type, state.path
                    )
                    continue

                tab = self._restore_tab_from_state(state)
                if not tab:
                    continue

                tab.set_ephemeral(state.is_ephemeral)

                self._active_column = self._tab_columns[column_index]
                title = tab.tab_title_from_path()
                self._add_tab(tab, title)

                if self._mindspace_manager.has_mindspace():
                    self._mindspace_manager.mindspace().contexts().open(
                        context_type=tab.tool_name(),
                        path=tab.path(),
                        title=title,
                        context_id=tab.tab_id(),
                    )

            except Exception as e:
                self._logger.exception("Failed to restore tab manager state: %s", str(e))
                continue

    def _deferred_set_active_column(self, active_column_index: int, active_tab_ids: List[str]) -> None:
        """
        Set the active column and tab after UI has settled.

        Args:
            active_column_index: Index of the column to make active
            active_tab_ids: List of active tab IDs for each column
        """
        # Show all columns with appropriate sizes
        self.show_all_columns()

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

    def restore_state(self, saved_state: Dict) -> None:
        """
        Restore tabs and active states from saved state.

        Args:
            saved_state: Dictionary containing saved state of columns and tabs
        """
        saved_columns = saved_state.get('columns', [])
        active_column_index = saved_state.get('active_column_index', 0)

        # Create necessary columns
        num_columns = len(saved_columns)
        for index in range(1, num_columns):
            self._create_column(index)

        # First pass: restore all tabs in all columns
        for column_index, column_state in enumerate(saved_columns):
            tab_states = column_state.get('tabs', [])
            self._restore_column_state(column_index, tab_states)

        # Second pass: set active tabs in each column
        active_tab_ids = []
        for column_index, column_state in enumerate(saved_columns):
            active_tab_id = column_state.get('active_tab_id')
            if active_tab_id and active_tab_id in self._tabs:
                column = self._tab_columns[column_index]
                tab = self._tabs[active_tab_id]
                column.setCurrentWidget(tab)
                active_tab_ids.append(active_tab_id)

        # Defer setting the active column to ensure it's not overridden by other UI operations
        QTimer.singleShot(0, lambda: self._deferred_set_active_column(active_column_index, active_tab_ids))

    def apply_style(self) -> None:
        """Apply style changes from StyleManager."""
        new_stylesheet = build_tab_manager_stylesheet(self._style_manager)
        if new_stylesheet != self.styleSheet():
            self.setStyleSheet(new_stylesheet)

        self._welcome_widget.apply_style()

        for column in self._tab_columns:
            tab_bar = column.tabBar()
            if tab_bar:
                assert isinstance(tab_bar, TabBar)
                tab_bar.handle_style_changed()
                new_tab_bar_stylesheet = build_tab_bar_stylesheet(self._style_manager)
                if new_tab_bar_stylesheet != tab_bar.styleSheet():
                    tab_bar.setStyleSheet(new_tab_bar_stylesheet)

        for tab in self._tabs.values():
            tab.apply_style()

        # Update columns after all other styling has been applied
        self.show_all_columns()

        # Thumbnails and metrics are stale after a style change, so close the overlays
        self._hide_tab_overlays()

    def update_welcome_widget(self, user_settings: UserSettings) -> None:
        """
        Update the welcome widget with current user settings.

        Args:
            user_settings: Current user settings
        """
        self._welcome_widget.set_user_settings(user_settings)

    def close_deleted_file(self, path: str) -> None:
        """
        Close any open tabs related to a file being deleted.

        Args:
            path: Path of file being deleted
        """
        for tab in list(self._tabs.values()):
            if tab.path() == path:
                self.close_tab_by_id(tab.tab_id(), True)

    def close_all_tabs(self) -> bool:
        """
        Close all open tabs.

        Returns:
            True if all tabs were closed successfully, False otherwise.
        """
        all_tabs = list(self._tabs.values())
        for tab in all_tabs:
            if tab.is_modified() and not tab.can_close_tab():
                return False

            self.close_tab_by_id(tab.tab_id(), force_close=True)

        return True

    def can_show_all_columns(self) -> bool:
        """Check if all columns can be shown."""
        return len(self._tab_columns) != 0

    def resizeEvent(self, event: QResizeEvent) -> None:  # type: ignore[override]
        """Reapply column sizing and margins when the widget is resized."""
        super().resizeEvent(event)

        if self._tab_columns:
            self.show_all_columns()

        QTimer.singleShot(0, self._refresh_current_tab_thumbnails)

        if self._tab_overview is not None and self._tab_overview.isVisible():
            self.show_tab_overview()

        elif self._tab_carousel is not None and self._tab_carousel.isVisible():
            self.show_tab_carousel()

    def show_all_columns(self) -> None:
        """Show all columns, sizing each to its preferred width where possible.

        Each column is sized to its preferred width. Tabs that return None from preferred_width()
        are treated as having the zoom-scaled default preferred width. If the total of all column
        widths is less than the available width, symmetric spacers centre the content.
        """
        if len(self._tab_columns) == 0:
            return

        min_col_width = 200
        style_manager = self._style_manager
        default_col_width = int(style_manager.nice_tab_width() * style_manager.zoom_factor())
        available = self.width()

        # Compute each column's preferred width. Tabs returning None use the default.
        col_preferred: List[int] = []
        for column in self._tab_columns:
            pref: int | None = None
            for i in range(column.count()):
                tab = cast(TabBase, column.widget(i))
                tab_pref = tab.preferred_width()
                if tab_pref is not None:
                    pref = tab_pref if pref is None else max(pref, tab_pref)

            col_preferred.append(pref if pref is not None else default_col_width)

        total_preferred = sum(max(min_col_width, p) for p in col_preferred)
        sizes = [min_col_width] * len(self._tab_columns)

        total_preferred += self._column_splitter.handleWidth() * (len(self._tab_columns) - 1)

        if total_preferred <= available:
            # Enough room: give each column its preferred width and centre with spacers.
            for i, p in enumerate(col_preferred):
                sizes[i] = max(min_col_width, p)

            margin = (available - total_preferred) // 2
            self._left_spacer.setFixedWidth(margin)
            self._right_spacer.setFixedWidth(margin)

        else:
            # Not enough room: scale proportionally, no spacers.
            scale = available / total_preferred
            for i, p in enumerate(col_preferred):
                sizes[i] = max(min_col_width, int(p * scale))

            self._left_spacer.setFixedWidth(0)
            self._right_spacer.setFixedWidth(0)

        self._column_splitter.setSizes(sizes)
        self._columns_layout.invalidate()
