"""Tab management for the Humbug application."""

from typing import Optional

from PySide6.QtWidgets import QTabWidget, QTabBar
from PySide6.QtCore import Signal

from humbug.gui.chat_view import ChatView
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

        self.setStyleSheet("""
            QTabWidget::pane {
                border: none;
                background: #1e1e1e;
            }
            QTabBar::tab {
                background: #2d2d2d;
                border: none;
                margin-right: 2px;
            }
            QTabBar::tab:selected {
                background: #3d3d3d;
            }
            QTabBar::tab:hover {
                background: #4d4d4d;
            }
        """)

        # Connect tab change signals
        self.currentChanged.connect(self._on_tab_changed)
        self.tabBar().setDrawBase(False)  # Remove line under tabs

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
        tab_label._close_clicked.connect(lambda: self.close_conversation(conversation_id))
        self._tab_labels[conversation_id] = tab_label

        # Add tab with custom label widget
        index = self.addTab(chat_view, "")
        self.setTabText(index, "")  # Clear default text
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
