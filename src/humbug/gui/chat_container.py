"""Unified container for chat input and history."""

from typing import Optional

from PySide6.QtWidgets import (
    QVBoxLayout, QWidget, QSizePolicy
)
from PySide6.QtCore import QSize, Signal
from PySide6.QtGui import QResizeEvent

from humbug.gui.color_role import ColorRole
from humbug.gui.history_view import HistoryView
from humbug.gui.live_input_widget import LiveInputWidget
from humbug.gui.style_manager import StyleManager


class ChatContainer(QWidget):
    # Signal to notify when container needs scroll adjustment
    scroll_requested = Signal(QSize)

    """Container widget that manages the history and input views."""

    def __init__(self, parent: Optional[QWidget] = None) -> None:
        """Initialize the container widget."""
        super().__init__(parent)

        # Create child widgets
        self.history = HistoryView(self)
        self.input = LiveInputWidget(self)

        vbox = QVBoxLayout()
        vbox.setSpacing(0)
        vbox.setContentsMargins(0, 0, 0, 0)
        vbox.addWidget(self.history)
        vbox.addWidget(self.input)
        self.setLayout(vbox)

        # Set size policy for container
        self.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.MinimumExpanding)
        self.setMinimumWidth(200)

        # Set background color
        style_manager = StyleManager()
        self.setStyleSheet(f"""
            ChatContainer {{
                background-color: {style_manager.get_color_str(ColorRole.TAB_ACTIVE)};
            }}
        """)

    def resizeEvent(self, event: QResizeEvent) -> None:
        """Handle widget resize events."""
        # Get the old and new sizes
        old_size: QSize = event.oldSize()
        self.scroll_requested.emit(old_size)

        super().resizeEvent(event)
