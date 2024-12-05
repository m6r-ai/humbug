"""Tab management for the Humbug application."""

from typing import Optional

from PySide6.QtWidgets import (
    QTabWidget, QWidget, QLabel, QPushButton, QHBoxLayout,
    QTabBar, QStyle, QStyleOption
)
from PySide6.QtCore import Signal, Qt, QSize, QEvent
from PySide6.QtGui import QPainter, QColor

from humbug.gui.chat_view import ChatView


class TabLabel(QWidget):
    """Custom widget for tab labels with close button."""

    close_clicked = Signal()

    def __init__(self, text: str, parent=None):
        """Initialize the tab label widget.

        Args:
            text: The text to display in the tab
            parent: Optional parent widget
        """
        super().__init__(parent)

        # Create layout
        layout = QHBoxLayout()
        layout.setSpacing(4)  # Space between label and button
        layout.setContentsMargins(8, 4, 4, 4)  # Left, top, right, bottom margins

        # Add label
        self.label = QLabel(text)
        self.label.setStyleSheet("color: white;")
        layout.addWidget(self.label)

        # Add close button
        self.close_button = QPushButton()
        self.close_button.setFixedSize(10, 10)
        self.close_button.clicked.connect(self.close_clicked)
        self.close_button.setStyleSheet("""
            QPushButton {
                border: 1px solid transparent;
                border-radius: 2px;
                background: transparent;
            }
            QPushButton:hover {
                border: 1px solid #ff4444;
                background: #ff4444;
            }
        """)
        layout.addWidget(self.close_button)

        self.setLayout(layout)

    def showCloseButton(self, show: bool):
        """Update close button border based on tab state.

        Args:
            show: Whether to show the white border around the close button
        """
        self.close_button.setStyleSheet("""
            QPushButton {
                border: 1px solid %s;
                border-radius: 2px;
                background: transparent;
            }
            QPushButton:hover {
                border: 1px solid #ff4444;
                background: #ff4444;
            }
        """ % ("white" if show else "transparent"))

    def setText(self, text: str):
        """Update the tab text.

        Args:
            text: The new text to display
        """
        self.label.setText(text)


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
        self.currentChanged.connect(self._handle_tab_changed)
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
        tab_label.close_clicked.connect(lambda: self._handle_conversation_close(conversation_id))
        self._tab_labels[conversation_id] = tab_label

        # Add tab with custom label widget
        index = self.addTab(chat_view, "")
        self.setTabText(index, "")  # Clear default text
        self.tabBar().setTabButton(index, QTabBar.LeftSide, tab_label)

        # Show close button for initial tab if it's the only one
        if self.count() == 1:
            tab_label.showCloseButton(True)

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

    def _handle_tab_close(self, index: int):
        """Handle tab close button clicks.

        Args:
            index: Index of the tab being closed
        """
        widget = self.widget(index)
        if isinstance(widget, ChatView):
            self._handle_conversation_close(widget.conversation_id)

    def _handle_conversation_close(self, conversation_id: str):
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

    def _handle_tab_changed(self, index: int):
        """Handle tab selection changes.

        Args:
            index: Index of the newly selected tab
        """
        # Hide close button border on all tabs
        for label in self._tab_labels.values():
            label.showCloseButton(False)

        # Show close button border on current tab
        if index >= 0:
            current_widget = self.widget(index)
            for conv_id, widget in self._conversations.items():
                if widget == current_widget:
                    self._tab_labels[conv_id].showCloseButton(True)
                    break

    def enterEvent(self, event):
        """Handle mouse entering the tab widget."""
        super().enterEvent(event)
        # Ensure current tab shows close button
        current = self.currentWidget()
        if current:
            for conv_id, widget in self._conversations.items():
                if widget == current:
                    self._tab_labels[conv_id].showCloseButton(True)
                    break

    def leaveEvent(self, event):
        """Handle mouse leaving the tab widget."""
        super().leaveEvent(event)
        # Only keep close button visible on current tab
        current = self.currentWidget()
        for conv_id, widget in self._conversations.items():
            if widget == current:
                self._tab_labels[conv_id].showCloseButton(True)
            else:
                self._tab_labels[conv_id].showCloseButton(False)

    def tabInserted(self, index: int):
        """Handle tab insertion.

        Args:
            index: Index where the tab was inserted
        """
        super().tabInserted(index)
        self._update_tab_close_buttons()

    def tabRemoved(self, index: int):
        """Handle tab removal.

        Args:
            index: Index where the tab was removed
        """
        super().tabRemoved(index)
        self._update_tab_close_buttons()

    def _update_tab_close_buttons(self):
        """Update close button visibility based on current state."""
        current = self.currentWidget()
        for conv_id, widget in self._conversations.items():
            self._tab_labels[conv_id].showCloseButton(widget == current)
