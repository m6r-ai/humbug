"""Tab management for the Humbug application."""

from PySide6.QtWidgets import QTabWidget, QWidget
from PySide6.QtCore import Qt

from humbug.gui.chat_view import ChatView


class TabManager(QTabWidget):
    """Manages conversation tabs."""

    def __init__(self, parent=None):
        """Initialize the tab manager."""
        super().__init__(parent)
        self.setTabsClosable(True)
        self.setMovable(True)  # Allow tab reordering
        self.setDocumentMode(True)  # Better visual integration
        
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

    def create_tab(self, title: str) -> ChatView:
        """Create a new conversation tab."""
        chat_view = ChatView(self)
        self.addTab(chat_view, title)
        return chat_view

    def get_current_chat(self) -> ChatView:
        """Get the currently active chat view."""
        return self.currentWidget()

