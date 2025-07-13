"""
Container for organized settings with consistent layout and styling.

This module defines the SettingsContainer class which serves as the main widget to hold
all settings items with proper spacing, scrolling, and organization.
"""

from typing import List

from PySide6.QtWidgets import QWidget, QVBoxLayout
from PySide6.QtCore import Signal

from .settings_item import SettingsItem
from humbug.style_manager import StyleManager


class SettingsContainer(QWidget):
    """
    Container for organized settings with consistent layout and styling.

    This serves as the main widget to hold all settings items with proper
    spacing, scrolling, and organization.

    Attributes:
        value_changed (Signal): Emitted when any contained setting changes
        _style_manager (StyleManager): Reference to the style manager singleton
        _settings (List[SettingsItem]): List of all settings in this container
        _layout (QVBoxLayout): Main layout for the container
    """

    value_changed = Signal()

    def __init__(self, parent: QWidget | None = None) -> None:
        """Initialize the settings container."""
        super().__init__(parent)

        self._style_manager = StyleManager()
        self._settings: List[SettingsItem] = []

        # Create main layout with proper spacing
        self._layout = QVBoxLayout()
        self._layout.setSpacing(0)
        self._layout.setContentsMargins(0, 0, 0, 0)

        self.setLayout(self._layout)

    def add_setting(self, setting: SettingsItem) -> None:
        """
        Add a setting to the container.

        Args:
            setting: The setting item to add
        """
        self._layout.addWidget(setting)
        self._settings.append(setting)
        setting.value_changed.connect(self.value_changed)

    def add_stretch(self) -> None:
        """Add a stretch to push all content upward."""
        self._layout.addStretch()

    def is_modified(self) -> bool:
        """Check if any settings in the container are modified."""
        return any(setting.is_modified() for setting in self._settings)

    def reset_modified_state(self) -> None:
        """Reset the modified state of all settings."""
        for setting in self._settings:
            setting.reset_modified_state()
