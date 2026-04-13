"""
Base class for settings that have a label and control.

This module defines the SettingsField class which provides a foundation for settings
that include a label and an interactive control.
"""

from PySide6.QtWidgets import QWidget, QVBoxLayout, QLabel

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

        self._layout = QVBoxLayout()
        self._layout.setContentsMargins(0, 4, 0, 4)

        self._label = QLabel(label_text)
        self._label.setIndent(0)
        self._layout.addWidget(self._label)

        self.setLayout(self._layout)

    def set_label(self, text: str) -> None:
        """Set the header label text."""
        self._label.setText(text)
