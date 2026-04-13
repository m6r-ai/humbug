"""
Checkbox setting with label for boolean options.

This module defines the SettingsCheckbox class which provides a checkbox
setting widget with label.
"""

from PySide6.QtWidgets import QWidget, QCheckBox

from humbug.settings.settings_field import SettingsField


class SettingsCheckbox(SettingsField):
    """
    Checkbox setting with label for boolean options.

    Attributes:
        _checkbox (QCheckBox): The checkbox control
        _label (QLabel): The description label
        _initial_value (bool): The initial value for detecting changes
    """

    def __init__(self, label_text: str, parent: QWidget | None = None) -> None:
        """
        Initialize a checkbox setting.

        Args:
            text: Label text for the checkbox
            parent: Parent widget
        """
        super().__init__(label_text, parent)

        self._checkbox = QCheckBox()
        self._checkbox.stateChanged.connect(self._on_state_changed)

        self._layout.addWidget(self._checkbox)
        self._initial_value = False
        self._on_style_changed()

    def _on_state_changed(self) -> None:
        """Handle checkbox state changes."""
        self.value_changed.emit()

    def is_modified(self) -> bool:
        """Check if checkbox state has changed."""
        return self._checkbox.isChecked() != self._initial_value

    def reset_modified_state(self) -> None:
        """Reset the initial value to current value."""
        self._initial_value = self._checkbox.isChecked()

    def get_value(self) -> bool:
        """Get the current checkbox state."""
        return self._checkbox.isChecked()

    def set_value(self, value: bool) -> None:
        """Set the checkbox state."""
        self._checkbox.setChecked(value)
        self._initial_value = value

    def set_label(self, text: str) -> None:
        """Set the header label text."""
        self._label.setText(text)

    def _on_style_changed(self) -> None:
        """Update checkbox styling."""
        zoom_factor = self._style_manager.zoom_factor()
        min_height = int(18 * zoom_factor)
        self._checkbox.setMinimumHeight(min_height)
