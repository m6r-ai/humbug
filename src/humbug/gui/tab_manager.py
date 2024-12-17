"""Tab management for the Humbug application."""

from typing import Optional

from PySide6.QtWidgets import QTabWidget, QTabBar
from PySide6.QtCore import Signal

from humbug.gui.chat_view import ChatView
from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager
from humbug.gui.tab_label import TabLabel


class TabManager(QTabWidget):
    """Manages conversation tabs with custom labels."""

    conversation_closed = Signal(str)  # Emits conversation_id

    def __init__(self, parent=None):
        """Initialize the tab manager.

        Args:
            parent: Optional parent widget
        """
        super().__init__(parent)
        self.setMovable(True)  # Allow tab reordering
        self.setDocumentMode(True)  # Better visual integration

        # Track conversations and their labels
        self._conversations = {}  # conversation_id -> ChatView
        self._tab_labels = {}    # conversation_id -> TabLabel

        self._style_manager = StyleManager()
        self.setStyleSheet(f"""
            QTabBar::scroller {{
                width: 40px;
            }}
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

        # Connect tab change signals
        self.currentChanged.connect(self._on_tab_changed)
        tab_bar = self.tabBar()
        tab_bar.setDrawBase(False)  # Remove line under tabs
        tab_bar.setUsesScrollButtons(True)

        self._style_manager.zoom_changed.connect(self._handle_zoom_changed)

    def create_conversation(self, conversation_id: str, title: str) -> 'ChatView':
        """Create a new conversation tab.

        Args:
            conversation_id: Unique identifier for the conversation
            title: Title to display in the tab

        Returns:
            The created ChatView instance
        """
        chat_view = ChatView(conversation_id, self)
        self._conversations[conversation_id] = chat_view

        # Create custom tab label
        tab_label = TabLabel(title)
        tab_label.close_clicked.connect(lambda: self.close_conversation(conversation_id))
        self._tab_labels[conversation_id] = tab_label

        # Add tab with custom label widget
        index = self.addTab(chat_view, "")
        self.tabBar().setTabButton(index, QTabBar.LeftSide, tab_label)

        # Set initial state for new tab
        if self.count() == 1:  # If this is the first tab
            tab_label.set_current(True)

        self.setCurrentWidget(chat_view)
        return chat_view

    def get_chat_view(self, conversation_id: str) -> Optional['ChatView']:
        """Get chat view by conversation ID.

        Args:
            conversation_id: The ID of the conversation to retrieve

        Returns:
            The ChatView instance or None if not found
        """
        return self._conversations.get(conversation_id)

    def get_current_chat(self) -> Optional['ChatView']:
        """Get the currently active chat view.

        Returns:
            The current ChatView instance or None if no tabs exist
        """
        return self.currentWidget()

    def close_conversation(self, conversation_id: str):
        """Handle conversation closure.

        Args:
            conversation_id: ID of the conversation to close
        """
        chat_view = self._conversations.get(conversation_id)
        if chat_view:
            index = self.indexOf(chat_view)
            self.removeTab(index)
            del self._conversations[conversation_id]
            if conversation_id in self._tab_labels:
                self._tab_labels[conversation_id].deleteLater()
                del self._tab_labels[conversation_id]
            self.conversation_closed.emit(conversation_id)
            chat_view.deleteLater()

    def _on_tab_changed(self, index: int):
        """Handle tab selection changes.

        Args:
            index: Index of the newly selected tab
        """
        # Update current states for all tabs
        for conv_id, label in self._tab_labels.items():
            widget = self._conversations[conv_id]
            is_current = (widget == self.widget(index))
            label.set_current(is_current)

    def tabInserted(self, index: int):
        """Handle tab insertion.

        Args:
            index: Index where the tab was inserted
        """
        super().tabInserted(index)
        self._update_all_tab_states()

    def tabRemoved(self, index: int):
        """Handle tab removal.

        Args:
            index: Index where the tab was removed
        """
        super().tabRemoved(index)
        self._update_all_tab_states()

    def _update_all_tab_states(self):
        """Update states for all tab labels."""
        current = self.currentWidget()
        for conv_id, label in self._tab_labels.items():
            widget = self._conversations[conv_id]
            label.set_current(widget == current)

    def _handle_zoom_changed(self, factor: float):
        """Handle zoom factor changes from StyleManager.

        Args:
            factor: New zoom factor
        """
        for label in self._tab_labels.values():
            label.handle_zoom_changed(factor)
