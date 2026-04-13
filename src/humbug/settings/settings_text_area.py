"""
Text area setting for multiline string values.

This module defines the SettingsTextArea class which provides a multiline text area
setting widget.
"""

from typing import List
from PySide6.QtWidgets import QWidget

from humbug.min_height_plain_text_edit import MinHeightPlainTextEdit
from humbug.settings.settings_field import SettingsField


class SettingsTextArea(SettingsField):
    """
    Text area setting for multiline string values.

    Attributes:
        _text_area (QPlainTextEdit): The text area control
        _initial_value (List[str]): The initial value as list
    """

    def __init__(
        self,
        label_text: str,
        placeholder: str = "",
        parent: QWidget | None = None
    ) -> None:
        """
        Initialize a text area setting.

        Args:
            label_text: Text for the setting label
            placeholder: Placeholder text for empty field
            parent: Parent widget
        """
        super().__init__(label_text, parent)

        self._text_area = MinHeightPlainTextEdit()
        self._text_area.setPlaceholderText(placeholder)
        self._text_area.textChanged.connect(self._on_text_changed)
        self._text_area.setObjectName("SettingsTextArea")

        self._layout.addWidget(self._text_area)
        self._initial_value: List[str] = []
        self._on_style_changed()

    def _on_text_changed(self) -> None:
        """Handle text area changes."""
        self.value_changed.emit()

    def is_modified(self) -> bool:
        """Check if text area value has changed."""
        return self.get_value() != self._initial_value

    def reset_modified_state(self) -> None:
        """Reset the initial value to current value."""
        self._initial_value = self.get_value()

    def get_value(self) -> List[str]:
        """Get the current text area value as a list of strings."""
        text = self._text_area.toPlainText()
        # Split by newlines and filter out empty lines
        return [line.strip() for line in text.split('\n') if line.strip()]

    def set_value(self, value: List[str]) -> None:
        """Set the text area value from a list of strings."""
        if not value:
            text = ""

        else:
            text = '\n'.join(value)

        self._text_area.setPlainText(text)
        self._initial_value = value.copy() if value else []

    def set_enabled(self, enabled: bool) -> None:
        """Enable or disable the text area."""
        self._text_area.setEnabled(enabled)
