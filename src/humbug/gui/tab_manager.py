"""Tab management for the Humbug application."""

from typing import Optional

from PySide6.QtWidgets import (
    QTabWidget, QWidget, QLabel, QPushButton, QHBoxLayout,
    QTabBar, QSizePolicy
)
from PySide6.QtCore import Signal, QSize

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
        self.label_text = text

        # Set size policy to ensure proper expansion
        self.setSizePolicy(QSizePolicy.Minimum, QSizePolicy.Fixed)

        # Create layout with proper spacing
        layout = QHBoxLayout()
        layout.setSpacing(6)  # Increased spacing between items
        layout.setContentsMargins(8, 4, 8, 4)  # Added right margin

        # Add label with size policy
        self.label = QLabel(text)
        self.label.setStyleSheet("color: white;")
        self.label.setSizePolicy(QSizePolicy.Minimum, QSizePolicy.Preferred)
        self.label.setMinimumWidth(50)  # Ensure minimum space for text
        layout.addWidget(self.label)

        # Add stretching space to push close button right
        layout.addStretch(1)

        # Add close button
        self.close_button = QPushButton(parent=self)
        self.close_button.setFixedSize(16, 16)  # Slightly larger for better visibility
        self.close_button.clicked.connect(self.close_clicked)
        self.close_button.setText("×")  # Use proper multiplication symbol
        self.close_button.setStyleSheet("""
            QPushButton {
                color: white;
                border: none;
                border-radius: 2px;
                background: #404040;
                margin: 0px;
                padding: 0px;
            }
            QPushButton:hover {
                background: #ff4444;
            }
        """)
        # Set size policy for close button
        self.close_button.setSizePolicy(QSizePolicy.Fixed, QSizePolicy.Fixed)
        layout.addWidget(self.close_button)

        self.setLayout(layout)

        # Track states
        self.is_current = False
        self.is_hovered = False

        # Enable mouse tracking
        self.setMouseTracking(True)

    def sizeHint(self) -> QSize:
        """Provide size hint for the tab."""
        width = (
            self.label.sizeHint().width() +  # Label width
            self.close_button.sizeHint().width() +  # Button width
            20 +  # Layout spacing and margins
            16   # Extra padding
        )
        height = max(
            self.label.sizeHint().height(),
            self.close_button.sizeHint().height()
        ) + 8  # Vertical padding
        return QSize(width, height)

    def minimumSizeHint(self) -> QSize:
        """Provide minimum size hint for the tab."""
        return QSize(
            self.label.minimumWidth() + self.close_button.width() + 24,
            max(self.label.minimumHeight(), self.close_button.height()) + 8
        )

    def enterEvent(self, event):
        """Handle mouse entering the tab label."""
        super().enterEvent(event)
        self.is_hovered = True
        self._update_close_button()

    def leaveEvent(self, event):
        """Handle mouse leaving the tab label."""
        super().leaveEvent(event)
        self.is_hovered = False
        self._update_close_button()

    def _update_close_button(self):
        """Update close button visibility and style based on current state."""
        visible = self.is_current or self.is_hovered

        # Try both show/hide and setVisible
        if visible:
            self.close_button.show()
        else:
            self.close_button.hide()

        self.close_button.setVisible(visible)

        # Force immediate update
        self.close_button.update()

        style = """
            QPushButton {
                color: white;
                border: none;
                border-radius: 2px;
                background: #404040;
                margin: 0px;
                padding: 0px;
            }
            QPushButton:hover {
                background: #ff4444;
            }
        """
        self.close_button.setStyleSheet(style)

    def set_current(self, is_current: bool):
        """Update the current state of the tab."""
        self.is_current = is_current
        self._update_close_button()

    def setText(self, text: str):
        """Update the tab text.

        Args:
            text: The new text to display
        """
        old_text = self.label_text
        self.label_text = text
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
