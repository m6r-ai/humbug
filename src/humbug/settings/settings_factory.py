"""
Factory to create settings items with consistent styling and behavior.

This module defines the SettingsFactory class which provides helper methods to create
various types of settings items with default styling and behavior.
"""

from typing import List, Any, Tuple
from PySide6.QtWidgets import QWidget

from humbug.settings.settings_spacer import SettingsSpacer
from humbug.settings.settings_header import SettingsHeader
from humbug.settings.settings_section import SettingsSection
from humbug.settings.settings_checkbox import SettingsCheckbox
from humbug.settings.settings_combo import SettingsCombo
from humbug.settings.settings_spinbox import SettingsSpinBox
from humbug.settings.settings_double_spinbox import SettingsDoubleSpinBox
from humbug.settings.settings_text_field import SettingsTextField
from humbug.settings.settings_text_area import SettingsTextArea
from humbug.settings.settings_display import SettingsDisplay


class SettingsFactory:
    """
    Factory to create settings items with consistent styling and behavior.

    This provides helper methods to create various types of settings items
    with default styling and behavior.
    """

    @staticmethod
    def create_spacer(height: int, parent: QWidget | None = None) -> SettingsSpacer:
        """Create a spacer."""
        return SettingsSpacer(height, parent)

    @staticmethod
    def create_header(title: str, parent: QWidget | None = None) -> SettingsHeader:
        """Create a header."""
        return SettingsHeader(title, parent)

    @staticmethod
    def create_section(title: str, description: str | None = None, parent: QWidget | None = None) -> SettingsSection:
        """Create a section header."""
        return SettingsSection(title, description, parent)

    @staticmethod
    def create_checkbox(text: str, parent: QWidget | None = None) -> SettingsCheckbox:
        """Create a checkbox setting."""
        return SettingsCheckbox(text, parent)

    @staticmethod
    def create_combo(
        label_text: str,
        items: List[Tuple[str, Any]] | None = None,
        parent: QWidget | None = None
    ) -> SettingsCombo:
        """Create a combo box setting."""
        return SettingsCombo(label_text, items, parent)

    @staticmethod
    def create_spinbox(
        label_text: str,
        min_value: int = 0,
        max_value: int = 100,
        step: int = 1,
        parent: QWidget | None = None
    ) -> SettingsSpinBox:
        """Create a spin box setting."""
        return SettingsSpinBox(label_text, min_value, max_value, step, parent)

    @staticmethod
    def create_double_spinbox(
        label_text: str,
        min_value: float = 0.0,
        max_value: float = 100.0,
        step: float = 1.0,
        decimals: int = 2,
        parent: QWidget | None = None
    ) -> SettingsDoubleSpinBox:
        """Create a double spin box setting."""
        return SettingsDoubleSpinBox(label_text, min_value, max_value, step, decimals, parent)

    @staticmethod
    def create_text_field(
        label_text: str,
        placeholder: str = "",
        parent: QWidget | None = None
    ) -> SettingsTextField:
        """Create a text field setting."""
        return SettingsTextField(label_text, placeholder, parent)

    @staticmethod
    def create_text_area(
        label_text: str,
        placeholder: str = "",
        parent: QWidget | None = None
    ) -> SettingsTextArea:
        """Create a text area setting."""
        return SettingsTextArea(label_text, placeholder, parent)

    @staticmethod
    def create_display(
        label_text: str,
        value: str = "",
        parent: QWidget | None = None
    ) -> SettingsDisplay:
        """Create a display field."""
        return SettingsDisplay(label_text, value, parent)
