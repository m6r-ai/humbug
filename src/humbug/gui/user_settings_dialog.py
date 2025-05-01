"""
Dialog for configuring user-specific settings.

This dialog allows users to configure settings that apply across all mindspaces,
such as API keys for different AI backends.
"""

import logging
from typing import Dict, List, Tuple

from PySide6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QLabel, QWidget,
    QPushButton, QLineEdit, QDoubleSpinBox, QComboBox, QListView
)
from PySide6.QtCore import Signal

from humbug.gui.style_manager import StyleManager, ColorMode
from humbug.language.language_code import LanguageCode
from humbug.language.language_manager import LanguageManager
from humbug.user.user_settings import UserSettings


class UserSettingsDialog(QDialog):
    """Dialog for editing user-specific settings."""

    settings_changed = Signal(UserSettings)

    def __init__(self, parent: QWidget | None = None) -> None:
        """
        Initialize the user settings dialog.

        Args:
            parent: Parent widget, typically the main window.
        """
        super().__init__(parent)
        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._handle_language_changed)
        strings = self._language_manager.strings()

        self.setWindowTitle(strings.user_settings)
        self.setMinimumWidth(750)
        self.setModal(True)

        self._initial_settings: UserSettings | None = None
        self._current_settings: UserSettings | None = None
        self._api_key_entries: Dict[str, QLineEdit] = {}
        self._api_key_labels: Dict[str, QLabel] = {}
        self._logger = logging.getLogger(__name__)

        self._style_manager = StyleManager()
        self._style_manager.style_changed.connect(self._handle_style_changed)

        # Main layout with proper spacing
        layout = QVBoxLayout()
        layout.setSpacing(12)
        layout.setContentsMargins(20, 20, 20, 20)

        # Create API key fields using the current language
        api_key_mapping = self._get_api_key_mapping()

        # API key fields
        for key, label_text in api_key_mapping:
            key_layout, label, line_edit = self._create_api_key_field(label_text)
            self._api_key_labels[key] = label
            self._api_key_entries[key] = line_edit
            layout.addLayout(key_layout)

        # Add language selector
        language_layout, self._language_combo = self._create_language_selector(self)
        layout.addLayout(language_layout)

        # Connect language change handler
        self._language_combo.currentIndexChanged.connect(self._handle_value_change)

        # Add font size selector
        font_size_layout = QHBoxLayout()
        self._font_size_label = QLabel(strings.font_size)
        self._font_size_label.setMinimumHeight(40)
        self._font_size_spin = QDoubleSpinBox()
        self._font_size_spin.setRange(8.0, 24.0)
        self._font_size_spin.setSingleStep(0.5)
        self._font_size_spin.setDecimals(1)
        self._font_size_spin.setMinimumWidth(550)
        self._font_size_spin.setMinimumHeight(40)
        self._font_size_spin.setContentsMargins(8, 8, 8, 8)
        self._font_size_spin.valueChanged.connect(self._handle_value_change)
        font_size_layout.addWidget(self._font_size_label)
        font_size_layout.addStretch()
        font_size_layout.addWidget(self._font_size_spin)
        layout.addLayout(font_size_layout)

        # Add theme selector
        theme_layout = QHBoxLayout()
        self._theme_label = QLabel(strings.display_theme)
        self._theme_label.setMinimumHeight(40)
        self._theme_combo = QComboBox(self)
        self._theme_combo.setView(QListView())  # Workaround to get styles to work
        self._theme_combo.setMinimumWidth(550)
        self._theme_combo.setMinimumHeight(40)

        # Add theme options
        self._theme_combo.addItem(strings.theme_dark, ColorMode.DARK)
        self._theme_combo.addItem(strings.theme_light, ColorMode.LIGHT)

        # Connect theme change handler
        self._theme_combo.currentIndexChanged.connect(self._handle_value_change)

        theme_layout.addWidget(self._theme_label)
        theme_layout.addStretch()
        theme_layout.addWidget(self._theme_combo)
        layout.addLayout(theme_layout)

        # Add spacing before buttons
        layout.addSpacing(24)
        layout.addStretch()

        # Button row
        button_layout = QHBoxLayout()
        button_layout.setSpacing(8)

        self.ok_button = QPushButton(strings.ok)
        self.ok_button.clicked.connect(self._handle_ok)
        self.ok_button.setProperty("recommended", True)

        self.cancel_button = QPushButton(strings.cancel)
        self.cancel_button.clicked.connect(self.reject)

        self.apply_button = QPushButton(strings.apply)
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
        self._handle_style_changed()

    def _handle_style_changed(self) -> None:
        self.setStyleSheet(self._style_manager.get_dialog_stylesheet())

    def _create_api_key_field(self, label_text: str) -> tuple[QHBoxLayout, QLabel, QLineEdit]:
        """
        Create an API key field with label and text input.

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

    def _create_language_selector(self, parent: QWidget) -> tuple[QHBoxLayout, QComboBox]:
        """
        Create language selection UI elements.

        Args:
            parent: Parent widget for the selector

        Returns:
            Tuple of (layout containing selector, combo box for language selection)
        """
        language_manager = LanguageManager()

        layout = QHBoxLayout()
        self._language_label = QLabel(language_manager.strings().select_language)
        self._language_label.setMinimumHeight(40)
        combo = QComboBox(parent)
        combo.setView(QListView())  # Weird workaround to get styles to work!
        combo.setMinimumWidth(550)
        combo.setMinimumHeight(40)

        # Add language options
        language_names = {
            LanguageCode.EN: "English",
            LanguageCode.FR: "Français",
            LanguageCode.AR: "العربية"
        }

        for code in LanguageCode:
            combo.addItem(language_names[code], code)

        # Set current language
        current_index = combo.findData(language_manager.current_language())
        combo.setCurrentIndex(current_index)

        layout.addWidget(self._language_label)
        layout.addStretch()
        layout.addWidget(combo)

        return layout, combo

    def _get_api_key_mapping(self) -> List[Tuple[str, str]]:
        """
        Get the API key mapping with the current language strings.

        Returns:
            List of tuples with (key_name, localized_label_text)
        """
        strings = self._language_manager.strings()
        return [
            ("ANTHROPIC_API_KEY", strings.anthropic_api_key),
            ("DEEPSEEK_API_KEY", strings.deepseek_api_key),
            ("GOOGLE_API_KEY", strings.google_api_key),
            ("M6R_API_KEY", strings.m6r_api_key),
            ("MISTRAL_API_KEY", strings.mistral_api_key),
            ("OPENAI_API_KEY", strings.openai_api_key),
            ("XAI_API_KEY", strings.xai_api_key)
        ]

    def _handle_language_changed(self) -> None:
        """Update all dialog texts with current language strings."""
        strings = self._language_manager.strings()
        self.setWindowTitle(strings.user_settings)

        # Update labels
        self._language_label.setText(strings.select_language)
        self._font_size_label.setText(strings.font_size)
        self._theme_label.setText(strings.display_theme)

        # Update theme combo box items
        current_theme = self._theme_combo.currentData()
        self._theme_combo.clear()
        self._theme_combo.addItem(strings.theme_dark, ColorMode.DARK)
        self._theme_combo.addItem(strings.theme_light, ColorMode.LIGHT)
        theme_index = self._theme_combo.findData(current_theme)
        self._theme_combo.setCurrentIndex(theme_index)

        # Update API key labels with current language strings
        api_key_mapping = self._get_api_key_mapping()

        # Update the labels with new text
        for key, label_text in api_key_mapping:
            if key in self._api_key_labels:
                self._api_key_labels[key].setText(label_text)

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
        current_settings = self._current_settings
        if not current_settings:
            return

        # Check if any value has changed from current settings
        api_keys_changed = False
        for key, line_edit in self._api_key_entries.items():
            if line_edit.text() != current_settings.api_keys.get(key, ""):
                api_keys_changed = True
                break

        language_changed = self._language_combo.currentData() != current_settings.language
        font_size_changed = self._font_size_spin.value() != (current_settings.font_size or self._style_manager.base_font_size)
        theme_changed = self._theme_combo.currentData() != current_settings.theme

        self.apply_button.setEnabled(
            api_keys_changed or language_changed or font_size_changed or theme_changed
        )

    def get_settings(self) -> UserSettings:
        """Get current settings from dialog."""
        api_keys = {
            key: line_edit.text()
            for key, line_edit in self._api_key_entries.items()
        }

        # Create a new UserSettings object with the updated settings
        settings = UserSettings(
            api_keys=api_keys,
            language=self._language_combo.currentData(),
            font_size=self._font_size_spin.value(),
            theme=self._theme_combo.currentData()
        )
        return settings

    def set_settings(self, settings: UserSettings) -> None:
        """
        Update dialog with current settings.

        Args:
            settings: UserSettings object with current settings
        """
        self._initial_settings = settings
        self._current_settings = UserSettings(
            api_keys=settings.api_keys.copy(),
            language=settings.language,
            font_size=settings.font_size,
            theme=settings.theme
        )

        # Set values in UI elements
        for key, value in settings.api_keys.items():
            if key in self._api_key_entries:
                self._api_key_entries[key].setText(value)

        # Set initial language selection
        current_index = self._language_combo.findData(self._language_manager.current_language())
        self._language_combo.setCurrentIndex(current_index)

        # Set font size
        self._font_size_spin.setValue(
            settings.font_size if settings.font_size is not None else self._style_manager.base_font_size()
        )

        # Set theme
        theme_index = self._theme_combo.findData(settings.theme)
        self._theme_combo.setCurrentIndex(theme_index)

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
