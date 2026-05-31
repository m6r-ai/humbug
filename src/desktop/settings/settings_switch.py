"""Switch setting with label for boolean options."""

from PySide6.QtWidgets import QWidget

from desktop.settings.settings_field import SettingsField
from desktop.widgets.switch import Switch


class SettingsSwitch(SettingsField):
    """Switch setting with label for boolean options."""

    def __init__(self, label_text: str, parent: QWidget | None = None) -> None:
        """
        Initialize a boolean setting.

        Args:
            label_text: Label text for the setting
            parent: Parent widget
        """
        super().__init__(label_text, parent)

        self._switch = Switch()
        self._switch.toggled.connect(self._on_state_changed)

        self._layout.addWidget(self._switch)
        self._initial_value = False
        self._on_style_changed()

    def _on_state_changed(self) -> None:
        """Handle switch state changes."""
        self.value_changed.emit()

    def is_modified(self) -> bool:
        """Check if switch state has changed."""
        return self._switch.isChecked() != self._initial_value

    def reset_modified_state(self) -> None:
        """Reset the initial value to current value."""
        self._initial_value = self._switch.isChecked()

    def get_value(self) -> bool:
        """Get the current switch state."""
        return self._switch.isChecked()

    def set_value(self, value: bool) -> None:
        """Set the switch state."""
        self._switch.setChecked(value)
        self._initial_value = value

    def set_label(self, text: str) -> None:
        """Set the header label text."""
        self._label.setText(text)

    def _on_style_changed(self) -> None:
        """Update switch styling."""
        self._switch.apply_style(self._style_manager)
