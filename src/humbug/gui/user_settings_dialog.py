"""
Dialog for configuring user-specific settings.

This dialog allows users to configure settings that apply across all mindspaces,
such as API keys for different AI backends.
"""

import logging
from typing import Dict, Optional

from PySide6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QLabel,
    QPushButton, QLineEdit
)
from PySide6.QtCore import Signal

from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager
from humbug.language.language_manager import LanguageManager
from humbug.user.user_settings import UserSettings


class UserSettingsDialog(QDialog):
    """Dialog for editing user-specific settings."""

    settings_changed = Signal(UserSettings)

    def __init__(self, parent=None):
        """Initialize the user settings dialog.

        Args:
            parent: Parent widget, typically the main window.
        """
        super().__init__(parent)
        self._language_manager = LanguageManager()
        strings = self._language_manager.strings

        self.setWindowTitle(strings.user_settings_dialog_title)
        self.setMinimumWidth(750)
        self.setModal(True)

        self._initial_settings: Optional[UserSettings] = None
        self._current_settings: Optional[UserSettings] = None
        self._api_key_entries: Dict[str, QLineEdit] = {}
        self._api_key_labels: Dict[str, QLabel] = {}
        self._logger = logging.getLogger(__name__)

        self._style_manager = StyleManager()

        # Main layout with proper spacing
        layout = QVBoxLayout()
        layout.setSpacing(12)
        layout.setContentsMargins(20, 20, 20, 20)

        # API key fields
        api_keys = [
            ("ANTHROPIC_API_KEY", strings.anthropic_api_key),
            ("DEEPSEEK_API_KEY", strings.deepseek_api_key),
            ("GOOGLE_API_KEY", strings.google_api_key),
            ("M6R_API_KEY", strings.m6r_api_key),
            ("MISTRAL_API_KEY", strings.mistral_api_key),
            ("OPENAI_API_KEY", strings.openai_api_key)
        ]

        for key, label_text in api_keys:
            key_layout, label, line_edit = self._create_api_key_field(label_text)
            self._api_key_labels[key] = label
            self._api_key_entries[key] = line_edit
            layout.addLayout(key_layout)

        # Add spacing before buttons
        layout.addSpacing(24)
        layout.addStretch()

        # Button row
        button_layout = QHBoxLayout()
        button_layout.setSpacing(8)

        self.ok_button = QPushButton(strings.ok)
        self.cancel_button = QPushButton(strings.cancel)
        self.apply_button = QPushButton(strings.apply)

        self.ok_button.clicked.connect(self._handle_ok)
        self.cancel_button.clicked.connect(self.reject)
        self.apply_button.clicked.connect(self._handle_apply)

        # Set minimum button sizes
        min_button_width = 90
        min_button_height = 40
        for button in [self.ok_button, self.cancel_button, self.apply_button]:
            button.setMinimumWidth(min_button_width)
            button.setMinimumHeight(min_button_height)
            button.setContentsMargins(8, 8, 8, 8)
            button_layout.addWidget(button)

        layout.addLayout(button_layout)
        self.setLayout(layout)

        zoom_factor = self._style_manager.zoom_factor
        base_font_size = self._style_manager.base_font_size

        # Apply consistent dialog styling
        self.setStyleSheet(f"""
            QDialog {{
                background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_DIALOG)};
                font-size: {base_font_size * zoom_factor}pt;
            }}
            QLabel {{
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_DIALOG)};
                font-size: {base_font_size * zoom_factor}pt;
            }}
            QLineEdit {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND)};
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: none;
                border-radius: 4px;
                padding: 8px;
                font-size: {base_font_size * zoom_factor}pt;
            }}
            QLineEdit:disabled {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_DISABLED)};
                color: {self._style_manager.get_color_str(ColorRole.TEXT_DISABLED)};
            }}
            QPushButton {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND)};
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: none;
                border-radius: 4px;
                padding: 8px;
                font-size: {base_font_size * zoom_factor}pt;
            }}
            QPushButton:hover {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_HOVER)};
            }}
            QPushButton:pressed {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_PRESSED)};
            }}
            QPushButton:disabled {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_DISABLED)};
                color: {self._style_manager.get_color_str(ColorRole.TEXT_DISABLED)};
            }}
        """)

    def _create_api_key_field(self, label_text: str) -> tuple[QHBoxLayout, QLabel, QLineEdit]:
        """Create an API key field with label and text input.

        Args:
            label_text: The label text to display

        Returns:
            Tuple of (layout containing the field, label widget, line edit widget)
        """
        layout = QHBoxLayout()
        label = QLabel(label_text)
        label.setMinimumHeight(40)
        line_edit = QLineEdit()
        line_edit.setMinimumWidth(550)
        line_edit.setMinimumHeight(40)
        line_edit.textChanged.connect(self._handle_value_change)

        layout.addWidget(label)
        layout.addStretch()
        layout.addWidget(line_edit)

        return layout, label, line_edit

    def _handle_value_change(self) -> None:
        """Handle changes to any API key value."""
        if not self._current_settings:
            return

        # Check if any value has changed from current settings
        changed = False
        for key, line_edit in self._api_key_entries.items():
            if line_edit.text() != self._current_settings.api_keys.get(key, ""):
                changed = True
                break

        self.apply_button.setEnabled(changed)

    def get_settings(self) -> UserSettings:
        """Get current API key settings from dialog."""
        api_keys = {
            key: line_edit.text()
            for key, line_edit in self._api_key_entries.items()
        }

        # Create a new UserSettings object with the updated API keys
        settings = UserSettings(api_keys=api_keys)
        return settings

    def set_settings(self, settings: UserSettings) -> None:
        """Update dialog with current API key settings.

        Args:
            settings: UserSettings object with current settings
        """
        self._initial_settings = UserSettings(api_keys=settings.api_keys.copy())
        self._current_settings = UserSettings(api_keys=settings.api_keys.copy())

        # Set values in UI elements
        for key, value in settings.api_keys.items():
            if key in self._api_key_entries:
                self._api_key_entries[key].setText(value)

        # Reset the apply button state
        self.apply_button.setEnabled(False)

    def _handle_apply(self) -> None:
        """Handle Apply button click."""
        settings = self.get_settings()
        self._current_settings = settings
        self.settings_changed.emit(settings)
        self.apply_button.setEnabled(False)

    def _handle_ok(self) -> None:
        """Handle OK button click."""
        self._handle_apply()
        self.accept()

    def reject(self) -> None:
        """Handle Cancel button click."""
        if self._initial_settings and self._current_settings != self._initial_settings:
            self.settings_changed.emit(self._initial_settings)

        super().reject()
