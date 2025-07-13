"""
SettingsItem base class for consistent styling.

This module defines the SettingsItem class which provides a common foundation for all
settings widgets to ensure consistent styling, spacing, and behavior.
"""

from typing import Any

from PySide6.QtWidgets import QWidget
from PySide6.QtCore import Signal

from humbug.style_manager import StyleManager


class SettingsItem(QWidget):
    """
    Base class for all settings components with consistent styling.

    This provides a common foundation for all settings widgets to ensure
    consistent styling, spacing, and behavior.

    Attributes:
        value_changed (Signal): Emitted when the value of this setting changes
        _style_manager (StyleManager): Reference to the style manager singleton
    """

    value_changed = Signal()

    def __init__(self, parent: QWidget | None = None) -> None:
        """Initialize the settings item with consistent styling."""
        super().__init__(parent)
        self._style_manager = StyleManager()
        self._style_manager.style_changed.connect(self._handle_style_changed)

    def _handle_style_changed(self) -> None:
        """Update styling when application style changes."""

    def is_modified(self) -> bool:
        """Check if this setting has been modified from its initial value."""
        return False

    def reset_modified_state(self) -> None:
        """Reset the modified state after applying changes."""

    def get_value(self) -> Any:
        """Get the current value of this setting."""
        return None

    def set_value(self, value: Any) -> None:
        """Set the current value of this setting."""
