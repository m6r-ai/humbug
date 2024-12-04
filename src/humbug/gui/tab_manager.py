"""Tab management for the Humbug application."""

from typing import Optional

from PySide6.QtWidgets import QTabWidget
from PySide6.QtCore import Signal

from humbug.gui.chat_view import ChatView


class TabManager(QTabWidget):
    """Manages conversation tabs."""

    # Signal emitted when a conversation is closed
    conversation_closed = Signal(str)  # Emits conversation_id

    def __init__(self, parent=None):
        """Initialize the tab manager."""
        super().__init__(parent)
        self.setTabsClosable(True)
        self.setMovable(True)  # Allow tab reordering
        self.setDocumentMode(True)  # Better visual integration

        # Connect close button signal
        self.tabCloseRequested.connect(self._handle_tab_close)

        # Track conversations
        self._conversations = {}  # conversation_id -> ChatView

        # Style for tabs
        self.setStyleSheet("""
            QTabWidget::pane {
                border: none;
                background: #1e1e1e;
            }
            QTabBar::tab {
                background: #2d2d2d;
                color: #ffffff;
                padding: 8px 16px;
                border: none;
                margin-right: 2px;
            }
            QTabBar::tab:selected {
                background: #3d3d3d;
            }
            QTabBar::tab:hover {
                background: #4d4d4d;
            }
            QTabBar::close-button {
                image: url(data:image/svg+xml;base64,PHN2ZyB3aWR0aD0iMTIiIGhlaWdodD0iMTIiIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyI+PHBhdGggZD0iTTEgMUwxMSAxMU0xIDExTDExIDEiIHN0cm9rZT0iI2ZmZmZmZiIgc3Ryb2tlLXdpZHRoPSIyIi8+PC9zdmc+);
                margin: 2px;
            }
            QTabBar::close-button:hover {
                background: #ff4444;
                border-radius: 2px;
            }
        """)

    def create_conversation(self, conversation_id: str, title: str) -> ChatView:
        """Create a new conversation tab."""
        chat_view = ChatView(conversation_id, self)
        self._conversations[conversation_id] = chat_view

        # Connect the close request signal
        chat_view.close_requested.connect(self._handle_conversation_close)

        self.addTab(chat_view, title)
        self.setCurrentWidget(chat_view)
        return chat_view

    def get_chat_view(self, conversation_id: str) -> Optional[ChatView]:
        """Get chat view by conversation ID."""
        return self._conversations.get(conversation_id)

    def get_current_chat(self) -> Optional[ChatView]:
        """Get the currently active chat view."""
        return self.currentWidget()

    def _handle_tab_close(self, index: int):
        """Handle tab close button clicks."""
        widget = self.widget(index)
        if isinstance(widget, ChatView):
            self._handle_conversation_close(widget.conversation_id)

    def _handle_conversation_close(self, conversation_id: str):
        """Handle conversation closure."""
        chat_view = self._conversations.get(conversation_id)
        if chat_view:
            index = self.indexOf(chat_view)
            self.removeTab(index)
            del self._conversations[conversation_id]
            self.conversation_closed.emit(conversation_id)
            chat_view.deleteLater()
