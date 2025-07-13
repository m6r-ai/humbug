"""
Section header for grouping related settings.

This module defines the SettingsSection class which provides a section header
widget for grouping related settings.
"""

from PySide6.QtWidgets import QWidget, QVBoxLayout, QLabel

from .settings_item import SettingsItem
from humbug.style_manager import ColorRole


class SettingsSection(SettingsItem):
    """
    Section header for grouping related settings.

    Attributes:
        _label (QLabel): The section title label
    """

    def __init__(self, title: str, parent: QWidget | None = None) -> None:
        """
        Initialize a section header with the specified title.

        Args:
            title: Text to display in the section header
            parent: Parent widget
        """
        super().__init__(parent)

        layout = QVBoxLayout()
        zoom_factor = self._style_manager.zoom_factor()
        layout.setContentsMargins(0, 0, 0, int(4 * zoom_factor))

        self._label = QLabel(title)
        layout.addWidget(self._label)

        self.setLayout(layout)
        self._handle_style_changed()

    def set_label(self, text: str) -> None:
        """Set the header label text."""
        self._label.setText(text)

    def _handle_style_changed(self) -> None:
        """Update section header styling."""
        font_size = self._style_manager.base_font_size()
        zoom_factor = self._style_manager.zoom_factor()
        scaled_font_size = int(font_size * zoom_factor)

        color = self._style_manager.get_color_str(ColorRole.TEXT_BRIGHT)
        self._label.setStyleSheet(f"""
            QLabel {{
                font-size: {scaled_font_size}pt;
                font-weight: bold;
                color: {color};
            }}
        """)
