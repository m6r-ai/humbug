"""
Spin box setting for integer values.

This module defines the SettingsSpinBox class which provides an integer spin box
setting widget.
"""

from PySide6.QtWidgets import QWidget, QSpinBox

from humbug.settings.settings_field import SettingsField


class SettingsSpinBox(SettingsField):
    """
    Spin box setting for integer values.

    Attributes:
        _spin (QSpinBox): The spin box control
        _initial_value (int): The initial value
    """

    def __init__(
        self,
        label_text: str,
        min_value: int = 0,
        max_value: int = 100,
        step: int = 1,
        parent: QWidget | None = None
    ) -> None:
        """
        Initialize a spin box setting.

        Args:
            label_text: Text for the setting label
            min_value: Minimum allowed value
            max_value: Maximum allowed value
            step: Step size for incrementing/decrementing
            parent: Parent widget
        """
        super().__init__(label_text, parent)

        self._spin = QSpinBox()
        self._spin.setRange(min_value, max_value)
        self._spin.setSingleStep(step)
        self._spin.valueChanged.connect(self._handle_changed)

        self._layout.addWidget(self._spin)
        self._initial_value = self._spin.value()
        self._on_style_changed()

    def _handle_changed(self) -> None:
        """Handle spin box value changes."""
        self.value_changed.emit()

    def is_modified(self) -> bool:
        """Check if spin box value has changed."""
        return self._spin.value() != self._initial_value

    def reset_modified_state(self) -> None:
        """Reset the initial value to current value."""
        self._initial_value = self._spin.value()

    def get_value(self) -> int:
        """Get the current spin box value."""
        return self._spin.value()

    def set_value(self, value: int) -> None:
        """Set the spin box value."""
        self._spin.setValue(value)
        self._initial_value = value

    def _on_style_changed(self) -> None:
        """Update spin box styling."""
        super()._on_style_changed()

        # Set minimum size based on zoom factor
        zoom_factor = self._style_manager.zoom_factor()
        min_height = int(30 * zoom_factor)
        self._spin.setMinimumHeight(min_height)

    def set_enabled(self, enabled: bool) -> None:
        """Enable or disable the spin box."""
        self._spin.setEnabled(enabled)
