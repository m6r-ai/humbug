"""
Base class for settings that have a label and control.

This module defines the SettingsField class which provides a foundation for settings
that include a label and an interactive control.
"""

from PySide6.QtWidgets import QWidget, QVBoxLayout, QLabel, QSizePolicy

from humbug.color_role import ColorRole
from humbug.settings.settings_item import SettingsItem


class SettingsField(SettingsItem):
    """
    Base class for settings that have a label and control.

    Attributes:
        _label (QLabel): The setting label
        _control (QWidget): The interactive control widget
        _layout (QVBoxLayout): Layout for the setting
    """

    def __init__(self, label_text: str, parent: QWidget | None = None) -> None:
        """
        Initialize a field setting with label and control.

        Args:
            label_text: Text for the setting label
            parent: Parent widget
        """
        super().__init__(parent)
        self.setObjectName("SettingsField")
        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Preferred)

        self._layout = QVBoxLayout()
        self._layout.setSpacing(6)
        self._layout.setContentsMargins(16, 12, 16, 12)

        self._label = QLabel(label_text)
        self._label.setIndent(0)
        self._layout.addWidget(self._label)

        self.setLayout(self._layout)

    def _on_style_changed(self) -> None:
        """Update field label styling."""
        font_size = self._style_manager.base_font_size()
        zoom_factor = self._style_manager.zoom_factor()
        label_font_size = int(font_size * zoom_factor * 0.92)
        color = self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)
        self._label.setStyleSheet(f"""
            QLabel {{
                color: {color};
                font-size: {label_font_size}pt;
                font-weight: 600;
                background: transparent;
                border: none;
                padding: 0px;
                margin: 0px;
            }}
        """)

    def set_label(self, text: str) -> None:
        """Set the header label text."""
        self._label.setText(text)
