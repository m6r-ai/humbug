"""
A simple spacer widget that implements the SettingsItem interface.

This module defines the SettingsSpacer class which provides a spacer widget
for consistent spacing in settings dialogs.
"""

from PySide6.QtWidgets import QWidget

from .settings_item import SettingsItem


class SettingsSpacer(SettingsItem):
    """A simple spacer widget that implements the SettingsItem interface."""

    def __init__(self, height: int = 16, parent: QWidget | None = None) -> None:
        """Initialize a spacer with the specified height."""
        super().__init__(parent)
        zoom_factor = self._style_manager.zoom_factor()
        self.setFixedHeight(int(zoom_factor * height))
