"""
Double spin box setting for float values.

This module defines the SettingsDoubleSpinBox class which provides a double spin box
setting widget for float values.
"""

from PySide6.QtWidgets import QWidget, QDoubleSpinBox

from humbug.settings.settings_field import SettingsField


class SettingsDoubleSpinBox(SettingsField):
    """
    Double spin box setting for float values.

    Attributes:
        _spin (QDoubleSpinBox): The double spin box control
        _initial_value (float): The initial value
    """

    def __init__(
        self,
        label_text: str,
        min_value: float = 0.0,
        max_value: float = 100.0,
        step: float = 1.0,
        decimals: int = 2,
        parent: QWidget | None = None
    ) -> None:
        """
        Initialize a double spin box setting.

        Args:
            label_text: Text for the setting label
            min_value: Minimum allowed value
            max_value: Maximum allowed value
            step: Step size for incrementing/decrementing
            decimals: Number of decimal places to display
            parent: Parent widget
        """
        super().__init__(label_text, parent)

        self._spin = QDoubleSpinBox()
        self._spin.setRange(min_value, max_value)
        self._spin.setSingleStep(step)
        self._spin.setDecimals(decimals)
        self._spin.valueChanged.connect(self._handle_changed)

        self._layout.addWidget(self._spin)
        self._initial_value = self._spin.value()
        self._handle_style_changed()

    def _handle_changed(self) -> None:
        """Handle double spin box value changes."""
        self.value_changed.emit()

    def is_modified(self) -> bool:
        """Check if double spin box value has changed."""
        # Use a small epsilon for float comparison
        return abs(self._spin.value() - self._initial_value) > 0.001

    def reset_modified_state(self) -> None:
        """Reset the initial value to current value."""
        self._initial_value = self._spin.value()

    def get_value(self) -> float:
        """Get the current double spin box value."""
        return self._spin.value()

    def set_value(self, value: float) -> None:
        """Set the double spin box value."""
        self._spin.setValue(value)
        self._initial_value = value

    def _handle_style_changed(self) -> None:
        """Update double spin box styling."""
        super()._handle_style_changed()

        # Set minimum size based on zoom factor
        zoom_factor = self._style_manager.zoom_factor()
        min_height = int(30 * zoom_factor)
        self._spin.setMinimumHeight(min_height)

    def set_enabled(self, enabled: bool) -> None:
        """Enable or disable the double spin box."""
        self._spin.setEnabled(enabled)
