"""
Text field setting for string values.

This module defines the SettingsTextField class which provides a text field
setting widget.
"""

from PySide6.QtWidgets import QWidget, QLineEdit

from humbug.settings.settings_field import SettingsField


class SettingsTextField(SettingsField):
    """
    Text field setting for string values.

    Attributes:
        _text_field (QLineEdit): The text field control
        _initial_value (str): The initial value
    """

    def __init__(
        self,
        label_text: str,
        placeholder: str = "",
        parent: QWidget | None = None
    ) -> None:
        """
        Initialize a text field setting.

        Args:
            label_text: Text for the setting label
            placeholder: Placeholder text for empty field
            parent: Parent widget
        """
        super().__init__(label_text, parent)

        self._text_field = QLineEdit()
        self._text_field.setPlaceholderText(placeholder)
        self._text_field.textChanged.connect(self._handle_changed)

        self._layout.addWidget(self._text_field)
        self._initial_value = self._text_field.text()
        self._handle_style_changed()

    def _handle_changed(self) -> None:
        """Handle text field changes."""
        self.value_changed.emit()

    def is_modified(self) -> bool:
        """Check if text field value has changed."""
        return self._text_field.text() != self._initial_value

    def reset_modified_state(self) -> None:
        """Reset the initial value to current value."""
        self._initial_value = self._text_field.text()

    def get_value(self) -> str:
        """Get the current text field value."""
        return self._text_field.text().strip("\n\r")

    def set_value(self, value: str) -> None:
        """Set the text field value."""
        text = value.strip("\n\r")
        self._text_field.setText(text)
        self._initial_value = text

    def _handle_style_changed(self) -> None:
        """Update text field styling."""
        super()._handle_style_changed()

        # Set minimum size based on zoom factor
        zoom_factor = self._style_manager.zoom_factor()
        min_height = int(30 * zoom_factor)
        self._text_field.setMinimumHeight(min_height)

    def set_enabled(self, enabled: bool) -> None:
        """Enable or disable the text field."""
        self._text_field.setEnabled(enabled)
