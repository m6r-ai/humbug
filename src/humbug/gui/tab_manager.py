"""Updated TabManager implementation to support both chat and editor tabs"""

from typing import Optional, Dict, List, cast
from PySide6.QtWidgets import QTabWidget, QTabBar
from PySide6.QtCore import Signal

from humbug.gui.tab_base import TabBase
from humbug.gui.tab_label import TabLabel
from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager


class TabManager(QTabWidget):
    """Manages multiple tabs for conversations and editors."""

    tab_closed = Signal(str)  # Emits tab_id

    def __init__(self, parent=None):
        """Initialize the tab manager."""
        super().__init__(parent)
        self.setMovable(True)
        self.setDocumentMode(True)

        # Track tabs
        self._tabs: Dict[str, TabBase] = {}
        self._tab_labels: Dict[str, TabLabel] = {}

        self._style_manager = StyleManager()
        self._style_manager.style_changed.connect(self._handle_style_changed)

        # Configure tab bar
        tab_bar = self.tabBar()
        tab_bar.setDrawBase(False)
        tab_bar.setUsesScrollButtons(True)

        # Connect signals
        self.currentChanged.connect(self._on_tab_changed)

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
        index = self.addTab(tab, "")
        self.tabBar().setTabButton(index, QTabBar.LeftSide, tab_label)

        # Set initial state
        if self.count() == 1:  # If this is the first tab
            tab_label.set_current(True)

        self.setCurrentWidget(tab)

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
        index = self.indexOf(tab)
        self.removeTab(index)
        del self._tabs[tab_id]

        # Clean up label
        if tab_id in self._tab_labels:
            self._tab_labels[tab_id].deleteLater()
            del self._tab_labels[tab_id]

        # Emit signal
        self.tab_closed.emit(tab_id)
        tab.deleteLater()

    def get_current_tab(self) -> Optional[TabBase]:
        """
        Get the currently active tab.
        
        Returns:
            The current tab or None if no tabs exist
        """
        widget = self.currentWidget()
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
            self.setCurrentWidget(tab)

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

    def _on_tab_changed(self, index: int) -> None:
        """
        Handle tab selection changes.

        Args:
            index: Index of the newly selected tab
        """
        # Update current states for all tabs
        for tab_id, label in self._tab_labels.items():
            tab = self._tabs[tab_id]
            is_current = tab == self.widget(index)
            label.set_current(is_current)

    def _handle_style_changed(self, factor: float = 1.0) -> None:
        """
        Handle style changes from StyleManager.

        Args:
            factor: New zoom factor
        """
        self.setStyleSheet(f"""
            QTabWidget::pane {{
                border: none;
                background: {self._style_manager.get_color_str(ColorRole.BACKGROUND_PRIMARY)};
            }}
            QTabBar::tab {{
                background: {self._style_manager.get_color_str(ColorRole.TAB_INACTIVE)};
                border: none;
                margin-right: 2px;
                border-bottom: 1px solid {self._style_manager.get_color_str(ColorRole.BACKGROUND_PRIMARY)};
            }}
            QTabBar::tab:selected {{
                background: {self._style_manager.get_color_str(ColorRole.TAB_ACTIVE)};
                border-bottom: none;
            }}
            QTabBar::tab:hover {{
                background: {self._style_manager.get_color_str(ColorRole.TAB_HOVER)};
            }}
        """)

        # Update all tab labels
        for label in self._tab_labels.values():
            label.handle_style_changed(factor)
