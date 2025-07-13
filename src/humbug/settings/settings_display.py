"""
Read-only display field for showing information.

This module defines the SettingsDisplay class which provides a read-only display
field widget.
"""

from PySide6.QtWidgets import QWidget, QVBoxLayout, QLabel
from PySide6.QtCore import Qt

from .settings_field import SettingsField
from humbug.style_manager import ColorRole


class SettingsDisplay(SettingsField):
    """
    Read-only display field for showing information.

    Attributes:
        _display (QLabel): The display label
    """

    def __init__(
        self,
        label_text: str,
        value: str = "",
        parent: QWidget | None = None
    ) -> None:
        """
        Initialize a display field setting.

        Args:
            label_text: Text for the setting label
            value: Initial display value
            parent: Parent widget
        """
        super().__init__(label_text, parent)

        self._display = QLabel(value)
        self._display.setWordWrap(True)
        self._display.setTextInteractionFlags(Qt.TextInteractionFlag.TextSelectableByMouse)

        self._layout.addWidget(self._display)
        self._handle_style_changed()

    def get_value(self) -> str:
        """Get the current display text."""
        return self._display.text()

    def set_value(self, value: str) -> None:
        """Set the display text."""
        self._display.setText(value)

    def _handle_style_changed(self) -> None:
        """Update display field styling."""
        super()._handle_style_changed()

        # Set minimum size based on zoom factor
        zoom_factor = self._style_manager.zoom_factor()
        min_height = int(30 * zoom_factor)
        self._display.setMinimumHeight(min_height)

        value_color = self._style_manager.get_color_str(ColorRole.TEXT_DISABLED)
        background = self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_DISABLED)

        self._display.setStyleSheet(f"""
            QLabel {{
                color: {value_color};
                background-color: {background};
                border-radius: 4px;
                padding: 4px;
            }}
        """)
