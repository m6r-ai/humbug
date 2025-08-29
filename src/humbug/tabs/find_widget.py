"""Widget for handling find operations in editor."""

from typing import Any, Dict

from PySide6.QtWidgets import (
    QWidget, QHBoxLayout, QLineEdit, QToolButton, QLabel
)
from PySide6.QtCore import Signal, Qt, QSize
from PySide6.QtGui import QIcon, QFocusEvent, QKeyEvent, QCloseEvent

from humbug.color_role import ColorRole
from humbug.language.language_manager import LanguageManager
from humbug.style_manager import StyleManager


class FindWidget(QWidget):
    """Non-modal widget for text find operations."""

    closed = Signal()
    find_next = Signal()
    find_previous = Signal()

    def __init__(self, parent: QWidget | None = None):
        """Initialize the find widget."""
        super().__init__(parent)
        self._style_manager = StyleManager()
        self._language_manager = LanguageManager()

        # Create layout
        layout = QHBoxLayout()
        layout.setContentsMargins(4, 4, 4, 4)
        layout.setSpacing(4)
        self.setLayout(layout)

        # Create search input
        self._search_input = QLineEdit()
        self._search_input.textChanged.connect(self._on_text_changed)
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

        self._close_button = QToolButton()
        self._close_button.clicked.connect(self.close)
        layout.addWidget(self._close_button)

        # Track current state
        self._matches = 0
        self._current_match = 0

        self._on_language_changed()

        self._style_manager.style_changed.connect(self._on_style_changed)
        self._language_manager.language_changed.connect(self._on_language_changed)

    def _on_language_changed(self) -> None:
        strings = self._language_manager.strings()
        self._search_input.setPlaceholderText(strings.find_placeholder)
        self._update_match_status()
        self._on_style_changed()

    def _on_style_changed(self) -> None:
        """Update styling when application style changes."""
        # Update font size
        factor = self._style_manager.zoom_factor()
        font = self.font()
        base_font_size = self._style_manager.base_font_size()
        font_size = base_font_size * factor
        font.setPointSizeF(font_size)
        self.setFont(font)

        prev_icon = "arrow-left" if self.layoutDirection() == Qt.LayoutDirection.LeftToRight else "arrow-right"
        self._prev_button.setIcon(QIcon(self._style_manager.scale_icon(
            self._style_manager.get_icon_path(prev_icon), 15
        )))
        next_icon = "arrow-right" if self.layoutDirection() == Qt.LayoutDirection.LeftToRight else "arrow-left"
        self._next_button.setIcon(QIcon(self._style_manager.scale_icon(
            self._style_manager.get_icon_path(next_icon), 15
        )))
        self._close_button.setIcon(QIcon(self._style_manager.scale_icon(
            self._style_manager.get_icon_path("close"), 15
        )))

        scaled_size = int(15 * factor)
        icon_size = QSize(scaled_size, scaled_size)
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
                padding: 2px;
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

    def keyPressEvent(self, event: QKeyEvent) -> None:
        """Handle key events."""
        if event.key() == Qt.Key.Key_Escape:
            self.close()
            event.accept()
            return

        super().keyPressEvent(event)

    def closeEvent(self, event: QCloseEvent) -> None:
        """Emit closed signal when closing."""
        super().closeEvent(event)
        self.closed.emit()

    def focusInEvent(self, _event: QFocusEvent) -> None:  # type: ignore[override]
        """Handle focus events."""
        self._search_input.setFocus()
        self._search_input.selectAll()

    def _update_match_status(self) -> None:
        """Update the match status display."""

        strings = self._language_manager.strings()
        if self._matches > 0:
            self._status_label.setText(strings.find_match_count.format(
                current=self._current_match,
                total=self._matches
            ))

        else:
            self._status_label.setText(strings.find_no_matches)

        self._status_label.show()

        # Update button states
        self._prev_button.setEnabled(self._matches > 0)
        self._next_button.setEnabled(self._matches > 0)

    def set_match_status(self, current: int, total: int) -> None:
        """Set the match status display.

        Args:
            current: Current match index (1-based)
            total: Total number of matches
        """
        self._matches = total
        self._current_match = current
        self._update_match_status()

    def _on_text_changed(self) -> None:
        """Handle changes to search text."""
        self.find_next.emit()

    def get_search_text(self) -> str:
        """Get the current search text."""
        return self._search_input.text()

    def set_search_text(self, text: str) -> None:
        """Set the search text and trigger a search."""
        self._search_input.setText(text)
        self._search_input.selectAll()

    def create_state_metadata(self) -> Dict[str, Any]:
        """
        Create state metadata for persistence during tab moves.

        Returns:
            Dictionary containing find widget state
        """
        return {
            'search_text': self.get_search_text(),
            'is_visible': not self.isHidden()
        }

    def restore_from_metadata(self, metadata: Dict[str, Any]) -> None:
        """
        Restore state from metadata after tab moves.

        Args:
            metadata: Dictionary containing find widget state
        """
        try:
            if 'search_text' in metadata:
                self.set_search_text(metadata['search_text'])

            # Reset match status since we're not re-executing searches
            self.set_match_status(0, 0)

            if metadata.get('is_visible', False):
                self.show()

            else:
                self.hide()

        except Exception:
            # Fail silently if metadata is malformed
            pass
