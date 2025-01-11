from typing import Optional, Dict, List, cast

from PySide6.QtWidgets import QTabWidget, QTabBar, QWidget, QVBoxLayout, QSplitter, QStackedWidget
from PySide6.QtCore import Signal, Qt

from humbug.gui.conversation_tab import ConversationTab
from humbug.gui.color_role import ColorRole
from humbug.gui.editor_tab import EditorTab
from humbug.gui.style_manager import StyleManager
from humbug.gui.tab_base import TabBase
from humbug.gui.tab_label import TabLabel
from humbug.gui.welcome_widget import WelcomeWidget


class ColumnTabWidget(QTabWidget):
    """Enhanced QTabWidget for use in columns."""

    def __init__(self, parent=None):
        """Initialize the tab widget."""
        super().__init__(parent)
        self.setMovable(True)
        self.setDocumentMode(True)

        # Configure tab bar
        tab_bar = self.tabBar()
        tab_bar.setDrawBase(False)
        tab_bar.setUsesScrollButtons(True)

    def mousePressEvent(self, event):
        """Handle mouse press to activate column."""
        super().mousePressEvent(event)
        # Inform parent TabManager that we want to be active
        parent = self.parent()
        while parent and not isinstance(parent, TabManager):
            parent = parent.parent()
        if parent:
            parent.activate_column(self)


class TabManager(QWidget):
    """Manages multiple tabs across one or two columns."""

    tab_closed = Signal(str)  # Emits tab_id
    current_tab_changed = Signal(TabBase)
    column_state_changed = Signal(bool)  # Emits True for two columns, False for one

    def __init__(self, parent=None):
        """Initialize the tab manager."""
        super().__init__(parent)

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
        self._tab_columns: List[ColumnTabWidget] = []
        self._create_column()

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

    def _create_column(self) -> ColumnTabWidget:
        """Create a new tab column."""
        tab_widget = ColumnTabWidget()
        tab_widget.currentChanged.connect(self._on_tab_changed)

        self._column_splitter.addWidget(tab_widget)
        self._tab_columns.append(tab_widget)

        return tab_widget

    def activate_column(self, column: ColumnTabWidget) -> None:
        """
        Make the specified column active.

        Args:
            column: The column widget to activate
        """
        if column in self._tab_columns and column != self._active_column:
            self._active_column = column
            # Update current states for all tabs in all columns
            for tab_id, label in self._tab_labels.items():
                tab = self._tabs[tab_id]
                for col in self._tab_columns:
                    is_current = (tab == col.widget(col.currentIndex()) and
                                col == self._active_column)
                    label.set_current(is_current)

            # Force style refresh to show active state
            self._handle_style_changed(self._style_manager.zoom_factor)

            # Emit signal with current tab
            current_tab = self.get_current_tab()
            self.current_tab_changed.emit(current_tab)

    def add_tab(self, tab: TabBase, title: str) -> None:
        """
        Add a new tab to the manager.

        Args:
            tab: The tab widget to add
            title: Initial title for the tab
        """
        tab_id = tab.tab_id
        self._tabs[tab_id] = tab

        # Create custom tab label
        tab_label = TabLabel(title)
        tab_label.close_clicked.connect(lambda: self.close_tab(tab_id))
        self._tab_labels[tab_id] = tab_label

        # Add tab with custom label to active column
        index = self._active_column.addTab(tab, "")
        self._active_column.tabBar().setTabButton(index, QTabBar.LeftSide, tab_label)

        # Set initial state
        if len(self._tabs) == 1:  # If this is the first tab
            tab_label.set_current(True)
            self._stack.setCurrentWidget(self._columns_widget)

        self._active_column.setCurrentWidget(tab)

    def close_tab(self, tab_id: str) -> None:
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
        index = column.indexOf(tab)
        column.removeTab(index)
        del self._tabs[tab_id]

        # Clean up label
        if tab_id in self._tab_labels:
            self._tab_labels[tab_id].deleteLater()
            del self._tab_labels[tab_id]

        # Emit signal
        self.tab_closed.emit(tab_id)
        tab.deleteLater()

        # Check if we need to remove empty column
        if column.count() == 0 and len(self._tab_columns) > 1:
            self.switch_to_single_column()

        # Show welcome message if no tabs remain
        if not self._tabs:
            self._stack.setCurrentWidget(self._welcome_widget)

    def _find_column_for_tab(self, tab: TabBase) -> Optional[ColumnTabWidget]:
        """Find which column contains the given tab."""
        for column in self._tab_columns:
            if column.indexOf(tab) != -1:
                return column
        return None

    def get_current_tab(self) -> Optional[TabBase]:
        """
        Get the currently active tab.

        Returns:
            The current tab or None if no tabs exist
        """
        widget = self._active_column.currentWidget()
        return cast(TabBase, widget) if widget else None

    def get_tab(self, tab_id: str) -> Optional[TabBase]:
        """
        Get a tab by its ID.

        Args:
            tab_id: ID of the tab to retrieve

        Returns:
            The tab or None if not found
        """
        return self._tabs.get(tab_id)

    def get_all_tabs(self) -> List[TabBase]:
        """
        Get all tabs.

        Returns:
            List of all tab instances
        """
        return list(self._tabs.values())

    def set_current_tab(self, tab_id: str) -> None:
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

    def update_tab_title(self, tab_id: str, title: str) -> None:
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

    def set_tab_modified(self, tab_id: str, modified: bool) -> None:
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

    def _on_tab_changed(self, index: int) -> None:
        """
        Handle tab selection changes.

        Args:
            index: Index of the newly selected tab
        """
        # Find which column triggered the change
        sender = self.sender()
        if isinstance(sender, ColumnTabWidget):
            self._active_column = sender

        # Update current states for all tabs in all columns
        for column in self._tab_columns:
            for tab_id, label in self._tab_labels.items():
                tab = self._tabs[tab_id]
                is_current = tab == column.widget(column.currentIndex()) and column == self._active_column
                label.set_current(is_current)

        # Emit our new signal with current tab
        current_tab = self.get_current_tab()
        self.current_tab_changed.emit(current_tab)

    def switch_to_single_column(self) -> None:
        """Convert from two columns to one."""
        if len(self._tab_columns) <= 1:
            return

        # Move all tabs from second column to first
        while self._tab_columns[1].count() > 0:
            tab = self._tab_columns[1].widget(0)
            index = self._tab_columns[0].addTab(tab, "")
            # Restore tab label
            if isinstance(tab, TabBase):
                label = self._tab_labels.get(tab.tab_id)
                if label:
                    self._tab_columns[0].tabBar().setTabButton(index, QTabBar.LeftSide, label)

        # Remove and delete second column
        self._tab_columns[1].deleteLater()
        self._tab_columns.pop(1)

        # Ensure first column is active
        self._active_column = self._tab_columns[0]

        # Emit signal about column state change
        self.column_state_changed.emit(False)

    def switch_to_double_column(self) -> None:
        """Convert from one column to two."""
        if len(self._tab_columns) > 1:
            return

        # Create second column
        new_column = self._create_column()

        # Move active tab to new column if we have one
        current_tab = self.get_current_tab()
        if current_tab:
            # Get the tab label
            label = self._tab_labels.get(current_tab.tab_id)

            # Remove from first column
            self._tab_columns[0].removeTab(self._tab_columns[0].indexOf(current_tab))

            # Add to second column
            index = new_column.addTab(current_tab, "")
            if label:
                new_column.tabBar().setTabButton(index, QTabBar.LeftSide, label)

            # Make second column active
            self._active_column = new_column
            new_column.setCurrentWidget(current_tab)

        # Set initial splitter sizes
        self._column_splitter.setSizes([self.width() // 2, self.width() // 2])

        # Emit signal about column state change
        self.column_state_changed.emit(True)

    def get_current_column(self) -> int:
        """Get index of currently active column."""
        return self._tab_columns.index(self._active_column)

    def get_tab_column(self, tab: TabBase) -> Optional[int]:
        """Get column index containing the specified tab."""
        for i, column in enumerate(self._tab_columns):
            if column.indexOf(tab) != -1:
                return i
        return None

    def get_conversation_tabs(self) -> List[ConversationTab]:
        """
        Get all conversation tabs.

        Returns:
            List of all ConversationTab instances
        """
        return [tab for tab in self._tabs.values() if isinstance(tab, ConversationTab)]

    def get_editor_tabs(self) -> List[EditorTab]:
        """
        Get all editor tabs.

        Returns:
            List of all EditorTab instances
        """
        return [tab for tab in self._tabs.values() if isinstance(tab, EditorTab)]

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

    def _handle_style_changed(self, factor: float = 1.0) -> None:
        """
        Handle style changes from StyleManager.

        Args:
            factor: New zoom factor
        """
        for column in self._tab_columns:
            is_active = column == self._active_column
            column_bg = (ColorRole.TAB_BACKGROUND_ACTIVE if is_active
                        else ColorRole.BACKGROUND_PRIMARY)

            style = f"""
                QTabWidget::pane {{
                    border: none;
                    background: {self._style_manager.get_color_str(column_bg)};
                }}
                QTabBar::tab {{
                    background: {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_INACTIVE)};
                    border: none;
                    margin-right: 2px;
                    border-bottom: 1px solid {self._style_manager.get_color_str(ColorRole.BACKGROUND_PRIMARY)};
                }}
                QTabBar::tab:selected {{
                    background: {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
                    border-top: 2px solid {self._style_manager.get_color_str(ColorRole.TAB_BORDER_ACTIVE)};
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

        # Update all tab labels
        for label in self._tab_labels.values():
            label.handle_style_changed(factor)

        self._column_splitter.setStyleSheet(f"""
            QSplitter::handle {{
                background-color: {self._style_manager.get_color_str(ColorRole.SPLITTER)};
                margin: 0;
                width: 1px;
            }}
        """)
