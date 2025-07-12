"""
Dialog for configuring user-specific settings using the settings framework.

This dialog allows users to configure settings that apply across all mindspaces,
such as API keys for different AI backends.
"""

import logging
from typing import Dict, cast

from PySide6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QPushButton, QScrollArea, QWidget, QFrame
)
from PySide6.QtCore import Signal

from ai.ai_backend_settings import AIBackendSettings
from ai.ai_manager import AIManager

from humbug.gui.style_manager import StyleManager, ColorMode
from humbug.language.language_code import LanguageCode
from humbug.language.language_manager import LanguageManager
from humbug.user.user_settings import UserSettings
from humbug.gui.settings.settings_components import (
    SettingsContainer, SettingsFactory, SettingsSection,
    SettingsCheckbox, SettingsTextField
)


class UserSettingsDialog(QDialog):
    """Dialog for editing user-specific settings using the settings framework."""

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
        self.setMinimumWidth(800)
        self.setMinimumHeight(700)
        self.setModal(True)

        self._initial_settings: UserSettings | None = None
        self._current_settings: UserSettings | None = None
        self._ai_backend_controls: Dict[str, Dict[str, QWidget]] = {}
        self._logger = logging.getLogger(__name__)

        self._style_manager = StyleManager()
        self._ai_manager = AIManager()

        # Main layout
        main_layout = QVBoxLayout()
        main_layout.setSpacing(12)
        main_layout.setContentsMargins(20, 20, 20, 20)

        # Create a scroll area
        scroll_area = QScrollArea()
        scroll_area.setWidgetResizable(True)
        scroll_area.setFrameShape(QFrame.Shape.NoFrame)

        # Create settings container
        self._settings_container = SettingsContainer()

        # General settings section
        self._general_section = SettingsFactory.create_header(strings.general_settings)
        self._settings_container.add_setting(self._general_section)

        self._display_section = SettingsFactory.create_section(strings.display_settings)
        self._settings_container.add_setting(self._display_section)

        # Language selection
        self._language_combo = SettingsFactory.create_combo(strings.select_language)
        self._settings_container.add_setting(self._language_combo)

        # Populate language options
        language_names = {
            LanguageCode.EN: "English",
            LanguageCode.FR: "Français",
            LanguageCode.AR: "العربية"
        }

        language_items = []
        for code in LanguageCode:
            language_items.append((language_names[code], code))

        self._language_combo.set_items(language_items)
        self._language_combo.set_value(self._language_manager.current_language())

        # Font size selection
        self._font_size_spin = SettingsFactory.create_double_spinbox(
            strings.font_size, 8.0, 24.0, 0.5, 1
        )
        self._settings_container.add_setting(self._font_size_spin)

        # Theme selection
        self._theme_combo = SettingsFactory.create_combo(strings.display_theme)
        self._settings_container.add_setting(self._theme_combo)

        # Add theme options
        theme_items = [
            (strings.theme_dark, ColorMode.DARK),
            (strings.theme_light, ColorMode.LIGHT)
        ]
        self._theme_combo.set_items(theme_items)

        # Add some spacing between backends
        spacer = SettingsFactory.create_spacer(24)
        self._settings_container.add_setting(spacer)

        # AI backends section
        self._backends_section = SettingsFactory.create_header(strings.ai_backend_config)
        self._settings_container.add_setting(self._backends_section)

        # Create AI backend settings
        ai_backend_mapping = [
            ("anthropic", strings.anthropic_backend),
            ("deepseek", strings.deepseek_backend),
            ("google", strings.google_backend),
            ("m6r", strings.m6r_backend),
            ("mistral", strings.mistral_backend),
            ("ollama", strings.ollama_backend),
            ("openai", strings.openai_backend),
            ("xai", strings.xai_backend)
        ]

        for backend_id, backend_name in ai_backend_mapping:
            # Add a backend title subsection
            backend_title = SettingsFactory.create_section(backend_name)
            self._settings_container.add_setting(backend_title)

            # Enable checkbox
            enable_checkbox = SettingsFactory.create_checkbox(strings.enable_backend)
            self._settings_container.add_setting(enable_checkbox)

            # API Key field
            api_key_field = SettingsFactory.create_text_field(strings.api_key)
            self._settings_container.add_setting(api_key_field)

            # URL field
            default_url = self._ai_manager.get_default_url(backend_id)
            url_field = SettingsFactory.create_text_field(
                strings.api_url,
                placeholder=default_url
            )
            self._settings_container.add_setting(url_field)

            # Store controls for this backend
            self._ai_backend_controls[backend_id] = {
                "enable": enable_checkbox,
                "key": api_key_field,
                "url": url_field,
                "title": backend_title
            }

            # Connect checkbox to enable/disable fields
            enable_checkbox.value_changed.connect(
                lambda backend_id=backend_id: self._handle_backend_enabled(backend_id)
            )

            # Add some spacing between backends
            spacer = SettingsFactory.create_spacer(24)
            self._settings_container.add_setting(spacer)

        # Add stretch at the end to push all content up
        self._settings_container.add_stretch()

        # Connect value changed signal
        self._settings_container.value_changed.connect(self._handle_value_change)

        # Set the scroll content
        scroll_area.setWidget(self._settings_container)
        main_layout.addWidget(scroll_area)
        main_layout.addSpacing(20)

        # Button row
        button_layout = QHBoxLayout()
        button_layout.setSpacing(8)
        button_layout.addStretch()

        self.ok_button = QPushButton(strings.ok)
        self.ok_button.clicked.connect(self._handle_ok)
        self.ok_button.setProperty("recommended", True)

        self.apply_button = QPushButton(strings.apply)
        self.apply_button.clicked.connect(self._handle_apply)

        self.cancel_button = QPushButton(strings.cancel)
        self.cancel_button.clicked.connect(self.reject)

        # Set minimum button sizes
        zoom_factor = self._style_manager.zoom_factor()
        min_button_width = int(90 * zoom_factor)
        min_button_height = 40
        for button in [self.ok_button, self.apply_button, self.cancel_button]:
            button.setMinimumWidth(min_button_width)
            button.setMinimumHeight(min_button_height)
            button.setContentsMargins(8, 8, 8, 8)
            button_layout.addWidget(button)

        button_layout.addStretch()
        main_layout.addLayout(button_layout)
        self.setLayout(main_layout)

        # Apply consistent dialog styling
        self._style_manager.style_changed.connect(self._handle_style_changed)
        self._handle_style_changed()

    def _handle_style_changed(self) -> None:
        """Update dialog style when the application style changes."""
        self.setStyleSheet(self._style_manager.get_dialog_stylesheet())

    def _handle_backend_enabled(self, backend_id: str) -> None:
        """
        Enable or disable backend fields based on checkbox state.

        Args:
            backend_id: ID of the backend whose enabled state changed
        """
        controls = self._ai_backend_controls[backend_id]
        enabled = cast(SettingsCheckbox, controls["enable"]).get_value()

        cast(SettingsTextField, controls["key"]).set_enabled(enabled)
        cast(SettingsTextField, controls["url"]).set_enabled(enabled)

    def _handle_language_changed(self) -> None:
        """Update all dialog texts with current language strings."""
        strings = self._language_manager.strings()
        self.setWindowTitle(strings.user_settings)

        # Update language combo items
        current_value = self._language_combo.get_value()
        language_names = {
            LanguageCode.EN: "English",
            LanguageCode.FR: "Français",
            LanguageCode.AR: "العربية"
        }

        language_items = []
        for code in LanguageCode:
            language_items.append((language_names[code], code))

        self._language_combo.set_label(strings.select_language)
        self._language_combo.set_items(language_items)
        self._language_combo.set_value(current_value)

        self._font_size_spin.set_label(strings.font_size)

        # Update theme combo items
        current_theme = self._theme_combo.get_value()
        theme_items = [
            (strings.theme_dark, ColorMode.DARK),
            (strings.theme_light, ColorMode.LIGHT)
        ]
        self._theme_combo.set_label(strings.display_theme)
        self._theme_combo.set_items(theme_items)
        self._theme_combo.set_value(current_theme)

        # Update buttons
        self.ok_button.setText(strings.ok)
        self.cancel_button.setText(strings.cancel)
        self.apply_button.setText(strings.apply)

        self._general_section.set_label(strings.general_settings)
        self._display_section.set_label(strings.display_settings)
        self._backends_section.set_label(strings.ai_backend_config)

        # Update AI backend titles and fields
        backend_mapping = {
            "anthropic": strings.anthropic_backend,
            "deepseek": strings.deepseek_backend,
            "google": strings.google_backend,
            "m6r": strings.m6r_backend,
            "mistral": strings.mistral_backend,
            "ollama": strings.ollama_backend,
            "openai": strings.openai_backend,
            "xai": strings.xai_backend
        }

        for backend_id, controls in self._ai_backend_controls.items():
            cast(SettingsSection, controls["title"]).set_label(backend_mapping[backend_id])
            cast(SettingsCheckbox, controls["enable"]).set_label(strings.enable_backend)
            cast(SettingsTextField, controls["key"]).set_label(strings.api_key)
            cast(SettingsTextField, controls["url"]).set_label(strings.api_url)

        # Adjust dialog size to fit new content
        self.adjustSize()

    def _handle_value_change(self) -> None:
        """Handle changes to any settings value."""
        if not self._current_settings:
            return

        self.apply_button.setEnabled(self._settings_container.is_modified())

    def get_settings(self) -> UserSettings:
        """Get current settings from dialog."""
        # Create AI backend settings
        ai_backends = {}
        for backend_id, controls in self._ai_backend_controls.items():
            enabled = cast(SettingsCheckbox, controls["enable"]).get_value()
            api_key = cast(SettingsTextField, controls["key"]).get_value()
            url = cast(SettingsTextField, controls["url"]).get_value()

            ai_backends[backend_id] = AIBackendSettings(
                enabled=enabled,
                api_key=api_key,
                url=url
            )

        # Create a new UserSettings object with the updated settings
        settings = UserSettings(
            ai_backends=ai_backends,
            language=self._language_combo.get_value(),
            font_size=self._font_size_spin.get_value(),
            theme=self._theme_combo.get_value()
        )
        return settings

    def set_settings(self, settings: UserSettings) -> None:
        """
        Update dialog with current settings.

        Args:
            settings: UserSettings object with current settings
        """
        self._initial_settings = settings

        # Create a deep copy for current settings
        self._current_settings = UserSettings(
            ai_backends={k: AIBackendSettings(
                enabled=v.enabled,
                api_key=v.api_key,
                url=v.url
            ) for k, v in settings.ai_backends.items()},
            language=settings.language,
            font_size=settings.font_size,
            theme=settings.theme
        )

        # Initialize API backend settings
        for backend_id, controls in self._ai_backend_controls.items():
            backend_settings = settings.ai_backends.get(backend_id, AIBackendSettings())

            cast(SettingsCheckbox, controls["enable"]).set_value(backend_settings.enabled)
            cast(SettingsTextField, controls["key"]).set_value(backend_settings.api_key)
            cast(SettingsTextField, controls["url"]).set_value(backend_settings.url)

            # Update enabled state
            cast(SettingsTextField, controls["key"]).set_enabled(backend_settings.enabled)
            cast(SettingsTextField, controls["url"]).set_enabled(backend_settings.enabled)

        # Set initial language selection
        self._language_combo.set_value(settings.language)

        # Set font size
        self._font_size_spin.set_value(
            settings.font_size if settings.font_size is not None else self._style_manager.base_font_size()
        )

        # Set theme
        self._theme_combo.set_value(settings.theme)

        # Reset the modified state
        self._settings_container.reset_modified_state()
        self.apply_button.setEnabled(False)

    def _handle_apply(self) -> None:
        """Handle Apply button click."""
        settings = self.get_settings()
        self._current_settings = settings
        self.settings_changed.emit(settings)
        self._settings_container.reset_modified_state()
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
