"""Widget for handling find operations in editor."""

from PySide6.QtWidgets import (
    QWidget, QHBoxLayout, QLineEdit, QToolButton, QLabel
)
from PySide6.QtCore import Signal, Qt, QSize
from PySide6.QtGui import QIcon

from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager


class FindWidget(QWidget):
    """Non-modal widget for text find operations."""

    closed = Signal()
    find_next = Signal()
    find_previous = Signal()

    def __init__(self, parent=None):
        """Initialize the find widget."""
        super().__init__(parent)
        self._style_manager = StyleManager()

        # Create layout
        layout = QHBoxLayout()
        layout.setContentsMargins(4, 4, 4, 4)
        layout.setSpacing(4)
        self.setLayout(layout)

        # Create search input
        self._search_input = QLineEdit()
        self._search_input.setPlaceholderText("Find")
        self._search_input.textChanged.connect(self._handle_text_changed)
        self._search_input.returnPressed.connect(self.find_next)
        layout.addWidget(self._search_input)

        # Add status label
        self._status_label = QLabel()
        layout.addWidget(self._status_label)

        # Create navigation buttons
        self._prev_button = QToolButton()
        self._prev_button.clicked.connect(self.find_previous)
        layout.addWidget(self._prev_button)

        self._next_button = QToolButton()
        self._next_button.clicked.connect(self.find_next)
        layout.addWidget(self._next_button)

        # Add close button
        self._close_button = QToolButton()
        self._close_button.clicked.connect(self.close)
        layout.addWidget(self._close_button)

        # Track current state
        self._matches = 0
        self._current_match = 0

        # Apply styling
        self._update_match_status()
        self._handle_style_changed(self._style_manager.zoom_factor)
        self._style_manager.style_changed.connect(self._handle_style_changed)

    def _handle_style_changed(self, factor: float) -> None:
        """Update styling when application style changes."""
        # Update font size
        font = self.font()
        base_font_size = self._style_manager.base_font_size
        font_size = base_font_size * factor
        font.setPointSizeF(font_size)
        self.setFont(font)

        self._prev_button.setIcon(QIcon(self._style_manager.scale_icon(
            self._style_manager.get_icon_path("arrow-left"), 16
        )))
        self._next_button.setIcon(QIcon(self._style_manager.scale_icon(
            self._style_manager.get_icon_path("arrow-right"), 16
        )))
        self._close_button.setIcon(QIcon(self._style_manager.scale_icon(
            self._style_manager.get_icon_path("close"), 16
        )))

        icon_size = QSize(16 * factor, 16 * factor)
        self._prev_button.setIconSize(icon_size)
        self._next_button.setIconSize(icon_size)
        self._close_button.setIconSize(icon_size)

        self.setStyleSheet(f"""
            QWidget {{
                background-color: {self._style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                font-size: {font_size}pt;
            }}
            QLineEdit {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND)};
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: none;
                padding: 2px;
                selection-background-color: {self._style_manager.get_color_str(ColorRole.TEXT_SELECTED)};
            }}
            QToolButton {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND)};
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: none;
                padding: 2px 4px;
            }}
            QToolButton:hover {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_HOVER)};
            }}
            QToolButton:pressed {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_PRESSED)};
            }}
            QToolButton:disabled {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_DISABLED)};
                color: {self._style_manager.get_color_str(ColorRole.TEXT_DISABLED)};
            }}
            QLabel {{
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: none;
                padding: 2px;
            }}
        """)

    def keyPressEvent(self, event):
        """Handle key events."""
        if event.key() == Qt.Key_Escape:
            self.close()
            event.accept()
            return

        super().keyPressEvent(event)

    def showEvent(self, event):
        """Focus search input when shown."""
        super().showEvent(event)
        self._search_input.setFocus()
        self._search_input.selectAll()

    def closeEvent(self, event):
        """Emit closed signal when closing."""
        super().closeEvent(event)
        self.closed.emit()

    def _update_match_status(self):
        """Update the match status display."""

        if self._matches > 0:
            self._status_label.setText(f"{self._current_match} of {self._matches}")
            self._status_label.show()
        else:
            self._status_label.setText("No matches")
            self._status_label.show()

        # Update button states
        self._prev_button.setEnabled(self._matches > 0)
        self._next_button.setEnabled(self._matches > 0)

    def set_match_status(self, current: int, total: int):
        """Set the match status display.
        
        Args:
            current: Current match index (1-based)
            total: Total number of matches
        """
        self._matches = total
        self._current_match = current
        self._update_match_status()

    def _handle_text_changed(self):
        """Handle changes to search text."""
        self.find_next.emit()

    def get_search_text(self) -> str:
        """Get the current search text."""
        return self._search_input.text()
