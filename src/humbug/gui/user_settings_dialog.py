"""
Dialog for configuring user-specific settings.

This dialog allows users to configure settings that apply across all mindspaces,
such as API keys for different AI backends.
"""

import json
import logging
import os
from typing import Dict, Optional

from PySide6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QLabel,
    QPushButton, QLineEdit, QListView
)
from PySide6.QtCore import Signal

from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager
from humbug.language.language_manager import LanguageManager


class UserSettingsDialog(QDialog):
    """Dialog for editing user-specific settings."""

    settings_changed = Signal(dict)

    def __init__(self, parent=None):
        """Initialize the user settings dialog.

        Args:
            parent: Parent widget, typically the main window.
        """
        super().__init__(parent)
        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._handle_language_changed)
        strings = self._language_manager.strings

        self.setWindowTitle(strings.user_settings_dialog_title)
        self.setMinimumWidth(750)
        self.setModal(True)

        self._initial_settings: Optional[Dict[str, str]] = None
        self._current_settings: Optional[Dict[str, str]] = None
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
            key_layout, label, line_edit = self._create_api_key_field(key, label_text)
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

        # Apply consistent dialog styling
        self.setStyleSheet(f"""
            QDialog {{
                background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_DIALOG)};
            }}
            QLabel {{
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_DIALOG)};
            }}
            QLineEdit {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND)};
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: none;
                border-radius: 4px;
                padding: 8px;
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

    def _create_api_key_field(self, key: str, label_text: str) -> tuple[QHBoxLayout, QLabel, QLineEdit]:
        """Create an API key field with label and text input.

        Args:
            key: The API key identifier
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

    def _handle_language_changed(self) -> None:
        """Update all dialog texts with current language strings."""
        strings = self._language_manager.strings
        self.setWindowTitle(strings.user_settings_dialog_title)

        # Update API key labels
        api_key_texts = {
            "ANTHROPIC_API_KEY": strings.anthropic_api_key,
            "DEEPSEEK_API_KEY": strings.deepseek_api_key,
            "GOOGLE_API_KEY": strings.google_api_key,
            "M6R_API_KEY": strings.m6r_api_key,
            "MISTRAL_API_KEY": strings.mistral_api_key,
            "OPENAI_API_KEY": strings.openai_api_key
        }

        for key, text in api_key_texts.items():
            if key in self._api_key_labels:
                self._api_key_labels[key].setText(text)

        # Update buttons
        self.ok_button.setText(strings.ok)
        self.cancel_button.setText(strings.cancel)
        self.apply_button.setText(strings.apply)

        # Adjust dialog size to fit new content
        self.adjustSize()
        size_hint = self.sizeHint()
        new_width = max(750, size_hint.width())
        self.resize(new_width, size_hint.height())

    def _handle_value_change(self) -> None:
        """Handle changes to any API key value."""
        if not self._current_settings:
            return

        # Check if any value has changed from current settings
        changed = False
        for key, line_edit in self._api_key_entries.items():
            if line_edit.text() != self._current_settings.get(key, ""):
                changed = True
                break

        self.apply_button.setEnabled(changed)

    def get_settings(self) -> Dict[str, str]:
        """Get current API key settings from dialog."""
        return {
            key: line_edit.text()
            for key, line_edit in self._api_key_entries.items()
        }

    def set_settings(self, settings: Dict[str, str]) -> None:
        """Update dialog with current API key settings.

        Args:
            settings: Dictionary of API key settings
        """
        self._initial_settings = settings.copy()
        self._current_settings = settings.copy()

        # Set values in UI elements
        for key, value in settings.items():
            if key in self._api_key_entries:
                self._api_key_entries[key].setText(value)

        # Reset the apply button state
        self.apply_button.setEnabled(False)

    def _handle_apply(self) -> None:
        """Handle Apply button click."""
        settings = self.get_settings()
        self._current_settings = settings.copy()
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

    @staticmethod
    def save_settings(settings: Dict[str, str]) -> None:
        """Save API key settings to configuration file.

        Args:
            settings: Dictionary of API key settings to save

        Raises:
            OSError: If there's an issue creating the directory or writing the file
            PermissionError: If the user doesn't have permission to create or write the file
        """
        config_dir = os.path.expanduser("~/.humbug")
        config_file = os.path.join(config_dir, "api-keys.json")

        # Create directory if it doesn't exist
        os.makedirs(config_dir, mode=0o700, exist_ok=True)

        # Write the settings to the file
        with open(config_file, 'w', encoding='utf-8') as f:
            json.dump(settings, f, indent=4)

        # Set secure permissions
        os.chmod(config_file, 0o600)

    @staticmethod
    def load_settings() -> Dict[str, str]:
        """Load API key settings from configuration file.

        Returns:
            Dictionary containing API key settings

        Raises:
            json.JSONDecodeError: If the file contains invalid JSON
        """
        config_dir = os.path.expanduser("~/.humbug")
        config_file = os.path.join(config_dir, "api-keys.json")

        # Initialize with empty values
        settings = {
            "ANTHROPIC_API_KEY": "",
            "DEEPSEEK_API_KEY": "",
            "GOOGLE_API_KEY": "",
            "M6R_API_KEY": "",
            "MISTRAL_API_KEY": "",
            "OPENAI_API_KEY": ""
        }

        # Read from file if it exists and is not empty
        if os.path.exists(config_file):
            with open(config_file, 'r', encoding='utf-8') as f:
                file_content = f.read()
                if file_content.strip():  # Only parse if file is not empty
                    file_settings = json.loads(file_content)
                    # Update settings with values from file
                    for key in settings:
                        if key in file_settings and file_settings[key]:
                            settings[key] = file_settings[key]

        return settings
