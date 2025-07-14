"""
Section header for grouping related settings.

This module defines the SettingsSection class which provides a section header
widget for grouping related settings.
"""

from PySide6.QtWidgets import QWidget, QVBoxLayout, QLabel

from humbug.settings.settings_item import SettingsItem
from humbug.style_manager import ColorRole


class SettingsSection(SettingsItem):
    """
    Section header for grouping related settings.

    Attributes:
        _title_label (QLabel): The section title label
    """

    def __init__(self, title: str, description: str | None = None, parent: QWidget | None = None) -> None:
        """
        Initialize a section header with the specified title.

        Args:
            title: Text to display in the section header
            parent: Parent widget
        """
        super().__init__(parent)

        layout = QVBoxLayout()
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        self._title_label = QLabel(title)
        self._title_label.setIndent(0)
        layout.addWidget(self._title_label)

        self._description_label: QLabel | None = None
        if description:
            self._description_label = QLabel(description)
            self._description_label.setIndent(0)
            layout.addWidget(self._description_label)

        self.setLayout(layout)
        self._handle_style_changed()

    def set_label(self, text: str) -> None:
        """Set the header label text."""
        self._title_label.setText(text)

    def _handle_style_changed(self) -> None:
        """Update section header styling."""
        font_size = self._style_manager.base_font_size()
        zoom_factor = self._style_manager.zoom_factor()
        title_scaled_font_size = int(font_size * zoom_factor)
        color = self._style_manager.get_color_str(ColorRole.TEXT_BRIGHT)
        bottom_margin = "0.3em" if self._description_label else "1.1em"
        self._title_label.setStyleSheet(f"""
            QLabel {{
                color: {color};
                font-size: {title_scaled_font_size}pt;
                font-weight: bold;
                border-radius: none;
                padding: 0px;
                margin: 0em 0em {bottom_margin} 0em;
            }}
        """)

        if self._description_label:
            description_scaled_font_size = int(font_size * zoom_factor * 0.85)
            self._description_label.setStyleSheet(f"""
                QLabel {{
                    font-size: {description_scaled_font_size}pt;
                    border-radius: none;
                    border: none;
                    padding: 0px;
                    margin: 0em 0em 1.5em 0em;
                }}
            """)
