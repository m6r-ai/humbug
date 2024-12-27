"""Updated TabManager implementation to support both chat and editor tabs"""

from typing import Optional, Dict, List, cast
from PySide6.QtWidgets import QTabWidget, QTabBar, QWidget, QVBoxLayout, QStackedWidget
from PySide6.QtCore import Signal

from humbug.gui.tab_base import TabBase
from humbug.gui.tab_label import TabLabel
from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager
from humbug.gui.welcome_widget import WelcomeWidget


class TabManager(QWidget):
    """Manages multiple tabs for conversations and editors."""

    tab_closed = Signal(str)  # Emits tab_id

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

        # Create tab widget
        self._tab_widget = QTabWidget()
        self._tab_widget.setMovable(True)
        self._tab_widget.setDocumentMode(True)
        self._stack.addWidget(self._tab_widget)

        # Set initial state
        self._stack.setCurrentWidget(self._welcome_widget)

        # Track tabs
        self._tabs: Dict[str, TabBase] = {}
        self._tab_labels: Dict[str, TabLabel] = {}

        self._style_manager = StyleManager()
        self._style_manager.style_changed.connect(self._handle_style_changed)

        # Configure tab bar
        tab_bar = self._tab_widget.tabBar()
        tab_bar.setDrawBase(False)
        tab_bar.setUsesScrollButtons(True)

        # Connect signals
        self._tab_widget.currentChanged.connect(self._on_tab_changed)

        self._handle_style_changed(self._style_manager.zoom_factor)

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

        # Add tab with custom label
        index = self._tab_widget.addTab(tab, "")
        self._tab_widget.tabBar().setTabButton(index, QTabBar.LeftSide, tab_label)

        # Set initial state
        if self._tab_widget.count() == 1:  # If this is the first tab
            tab_label.set_current(True)
            self._stack.setCurrentWidget(self._tab_widget)

        self._tab_widget.setCurrentWidget(tab)

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

        # Remove tab
        index = self._tab_widget.indexOf(tab)
        self._tab_widget.removeTab(index)
        del self._tabs[tab_id]

        # Clean up label
        if tab_id in self._tab_labels:
            self._tab_labels[tab_id].deleteLater()
            del self._tab_labels[tab_id]

        # Emit signal
        self.tab_closed.emit(tab_id)
        tab.deleteLater()

        # Show welcome message if no tabs remain
        if not self._tabs:
            self._stack.setCurrentWidget(self._welcome_widget)

    def get_current_tab(self) -> Optional[TabBase]:
        """
        Get the currently active tab.
        
        Returns:
            The current tab or None if no tabs exist
        """
        widget = self._tab_widget.currentWidget()
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
        if tab:
            self._tab_widget.setCurrentWidget(tab)

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
        # Update current states for all tabs
        for tab_id, label in self._tab_labels.items():
            tab = self._tabs[tab_id]
            is_current = tab == self._tab_widget.widget(index)
            label.set_current(is_current)

    def _handle_style_changed(self, factor: float = 1.0) -> None:
        """
        Handle style changes from StyleManager.

        Args:
            factor: New zoom factor
        """
        self._tab_widget.setStyleSheet(f"""
            QTabWidget::pane {{
                border: none;
                background: {self._style_manager.get_color_str(ColorRole.BACKGROUND_PRIMARY)};
            }}
            QTabBar::tab {{
                background: {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_INACTIVE)};
                border: none;
                margin-right: 2px;
                border-bottom: 1px solid {self._style_manager.get_color_str(ColorRole.BACKGROUND_PRIMARY)};
            }}
            QTabBar::tab:selected {{
                background: {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
                border-bottom: none;
            }}
            QTabBar::tab:hover {{
                background: {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_HOVER)};
            }}
        """)

        # Update all tab labels
        for label in self._tab_labels.values():
            label.handle_style_changed(factor)
