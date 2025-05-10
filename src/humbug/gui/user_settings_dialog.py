"""
Dialog for configuring user-specific settings.

This dialog allows users to configure settings that apply across all mindspaces,
such as API keys for different AI backends.
"""

import logging
from typing import Dict

from PySide6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QLabel, QWidget,
    QPushButton, QLineEdit, QDoubleSpinBox, QComboBox, QListView,
    QScrollArea, QFrame, QGroupBox, QCheckBox
)
from PySide6.QtCore import Signal, Qt

from humbug.ai.ai_backend_settings import AIBackendSettings
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
        self.setMinimumWidth(800)
        self.setMinimumHeight(600)
        self.setModal(True)

        self._initial_settings: UserSettings | None = None
        self._current_settings: UserSettings | None = None
        self._ai_backend_controls: Dict[str, Dict[str, QWidget]] = {}
        self._ai_backend_group_boxes: Dict[str, QGroupBox] = {}
        self._logger = logging.getLogger(__name__)

        self._style_manager = StyleManager()
        self._style_manager.style_changed.connect(self._handle_style_changed)

        # Main layout with proper spacing
        main_layout = QVBoxLayout()
        main_layout.setSpacing(12)
        main_layout.setContentsMargins(20, 20, 20, 20)

        # Create a scroll area for the settings
        scroll_area = QScrollArea()
        scroll_area.setWidgetResizable(True)
        scroll_area.setFrameShape(QFrame.Shape.NoFrame)

        # Container widget for the scroll area
        scroll_content = QWidget()
        scroll_layout = QVBoxLayout(scroll_content)
        scroll_layout.setSpacing(12)
        scroll_layout.setContentsMargins(0, 0, 8, 0)

        # Section title for AI backends
        ai_backends_title = QLabel(strings.ai_backends_title)
        ai_backends_title.setStyleSheet("font-weight: bold; font-size: 14pt;")
        self._ai_backends_title_label = ai_backends_title
        scroll_layout.addWidget(ai_backends_title)

        # Create AI backend settings groups
        ai_backend_mapping = [
            ("anthropic", strings.anthropic_backend),
            ("deepseek", strings.deepseek_backend),
            ("google", strings.google_backend),
            ("m6r", strings.m6r_backend),
            ("mistral", strings.mistral_backend),
            ("openai", strings.openai_backend),
            ("ollama", strings.ollama_backend),
            ("xai", strings.xai_backend)
        ]

        # Fixed width for labels to ensure alignment
        label_width = 125
        field_width = 550

        for backend_id, backend_name in ai_backend_mapping:
            group_box = QGroupBox(backend_name)
            self._ai_backend_group_boxes[backend_id] = group_box
            group_layout = QVBoxLayout()

            # Enable checkbox
            enable_layout = QHBoxLayout()
            enable_label = QLabel(strings.enable_backend)
            enable_label.setMinimumWidth(label_width)
            enable_checkbox = QCheckBox()
            enable_checkbox.setMinimumHeight(40)

            # Create a container layout for the checkbox to align with text fields
            checkbox_container = QHBoxLayout()
            checkbox_container.addWidget(enable_checkbox)
            checkbox_container.addStretch()

            enable_layout.addWidget(enable_label)
            enable_layout.addLayout(checkbox_container)

            group_layout.addLayout(enable_layout)

            # API Key field
            key_layout = QHBoxLayout()
            key_label = QLabel(strings.api_key)
            key_label.setMinimumWidth(label_width)
            key_input = QLineEdit()
            key_input.setMinimumWidth(field_width)
            key_input.setMinimumHeight(40)
            key_layout.addWidget(key_label)
            key_layout.addWidget(key_input)
            group_layout.addLayout(key_layout)

            # URL field
            url_layout = QHBoxLayout()
            url_label = QLabel(strings.api_url)
            url_label.setMinimumWidth(label_width)
            url_input = QLineEdit()
            url_input.setMinimumWidth(field_width)
            url_input.setMinimumHeight(40)
            url_layout.addWidget(url_label)
            url_layout.addWidget(url_input)
            group_layout.addLayout(url_layout)

            # Store controls for this backend
            self._ai_backend_controls[backend_id] = {
                "enable": enable_checkbox,
                "key": key_input,
                "url": url_input,
                "enable_label": enable_label,
                "key_label": key_label,
                "url_label": url_label
            }

            # Connect checkbox to enable/disable fields
            enable_checkbox.stateChanged.connect(
                lambda state, k=key_input, u=url_input: self._handle_backend_enabled(state, k, u)
            )

            # Connect signals for detecting changes
            enable_checkbox.stateChanged.connect(self._handle_value_change)
            key_input.textChanged.connect(self._handle_value_change)
            url_input.textChanged.connect(self._handle_value_change)

            group_box.setLayout(group_layout)
            scroll_layout.addWidget(group_box)

        # Add spacing between section
        scroll_layout.addSpacing(24)

        # Section title for general settings
        general_title = QLabel(strings.general_settings)
        general_title.setStyleSheet("font-weight: bold; font-size: 14pt;")
        self._general_settings_title_label = general_title
        scroll_layout.addWidget(general_title)

        # Add language selector
        language_layout, self._language_combo = self._create_language_selector(self)
        scroll_layout.addLayout(language_layout)

        # Connect language change handler
        self._language_combo.currentIndexChanged.connect(self._handle_value_change)

        # Add font size selector
        font_size_layout = QHBoxLayout()
        self._font_size_label = QLabel(strings.font_size)
        self._font_size_label.setMinimumWidth(label_width)
        self._font_size_label.setMinimumHeight(40)
        self._font_size_spin = QDoubleSpinBox()
        self._font_size_spin.setRange(8.0, 24.0)
        self._font_size_spin.setSingleStep(0.5)
        self._font_size_spin.setDecimals(1)
        self._font_size_spin.setMinimumWidth(field_width)
        self._font_size_spin.setMinimumHeight(40)
        self._font_size_spin.setContentsMargins(8, 8, 8, 8)
        self._font_size_spin.valueChanged.connect(self._handle_value_change)
        font_size_layout.addWidget(self._font_size_label)
        font_size_layout.addWidget(self._font_size_spin)
        scroll_layout.addLayout(font_size_layout)

        # Add theme selector
        theme_layout = QHBoxLayout()
        self._theme_label = QLabel(strings.display_theme)
        self._theme_label.setMinimumWidth(label_width)
        self._theme_label.setMinimumHeight(40)
        self._theme_combo = QComboBox(self)
        self._theme_combo.setView(QListView())  # Workaround to get styles to work
        self._theme_combo.setMinimumWidth(field_width)
        self._theme_combo.setMinimumHeight(40)

        # Add theme options
        self._theme_combo.addItem(strings.theme_dark, ColorMode.DARK)
        self._theme_combo.addItem(strings.theme_light, ColorMode.LIGHT)

        # Connect theme change handler
        self._theme_combo.currentIndexChanged.connect(self._handle_value_change)

        theme_layout.addWidget(self._theme_label)
        theme_layout.addWidget(self._theme_combo)
        scroll_layout.addLayout(theme_layout)

        # Add stretch at the end to push all content up
        scroll_layout.addStretch()

        # Set the scroll content and add to main layout
        scroll_content.setLayout(scroll_layout)
        scroll_area.setWidget(scroll_content)
        main_layout.addWidget(scroll_area)

        # Button row (not in scroll area)
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

        self._handle_style_changed()

    def _handle_style_changed(self) -> None:
        """Update dialog style when the application style changes."""
        self.setStyleSheet(self._style_manager.get_dialog_stylesheet())

    def _handle_backend_enabled(self, state: int, key_input: QLineEdit, url_input: QLineEdit) -> None:
        """Enable or disable the key and URL fields based on checkbox state."""
        enabled = state == Qt.CheckState.Checked.value
        key_input.setEnabled(enabled)
        url_input.setEnabled(enabled)

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
        self._language_label.setMinimumWidth(125)  # Fixed width for alignment
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
        layout.addWidget(combo)

        return layout, combo

    def _handle_language_changed(self) -> None:
        """Update all dialog texts with current language strings."""
        strings = self._language_manager.strings()
        self.setWindowTitle(strings.user_settings)

        # Update section titles
        self._ai_backends_title_label.setText(strings.ai_backends_title)
        self._general_settings_title_label.setText(strings.general_settings)

        # Update AI backend group boxes and their labels
        backend_mapping = {
            "anthropic": strings.anthropic_backend,
            "deepseek": strings.deepseek_backend,
            "google": strings.google_backend,
            "m6r": strings.m6r_backend,
            "mistral": strings.mistral_backend,
            "openai": strings.openai_backend,
            "ollama": strings.ollama_backend,
            "xai": strings.xai_backend
        }

        for backend_id, controls in self._ai_backend_controls.items():
            # Update group box title
            if backend_id in self._ai_backend_group_boxes:
                self._ai_backend_group_boxes[backend_id].setTitle(backend_mapping[backend_id])

            # Update control labels
            controls["enable_label"].setText(strings.enable_backend)
            controls["key_label"].setText(strings.api_key)
            controls["url_label"].setText(strings.api_url)

        # Update other labels
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
        """Handle changes to any settings value."""
        current_settings = self._current_settings
        if not current_settings:
            return

        # Check if any backend settings have changed
        backends_changed = False
        for backend_id, controls in self._ai_backend_controls.items():
            backend_settings = current_settings.ai_backends.get(backend_id, AIBackendSettings())

            if controls["enable"].isChecked() != backend_settings.enabled:
                backends_changed = True
                break

            if controls["key"].text() != backend_settings.api_key:
                backends_changed = True
                break

            if controls["url"].text() != backend_settings.url:
                backends_changed = True
                break

        language_changed = self._language_combo.currentData() != current_settings.language
        font_size_changed = self._font_size_spin.value() != (current_settings.font_size or self._style_manager.base_font_size())
        theme_changed = self._theme_combo.currentData() != current_settings.theme

        self.apply_button.setEnabled(
            backends_changed or language_changed or font_size_changed or theme_changed
        )

    def get_settings(self) -> UserSettings:
        """Get current settings from dialog."""
        # Create AI backend settings
        ai_backends = {}
        for backend_id, controls in self._ai_backend_controls.items():
            enabled = controls["enable"].isChecked()
            api_key = controls["key"].text()
            url = controls["url"].text()

            ai_backends[backend_id] = AIBackendSettings(
                enabled=enabled,
                api_key=api_key,
                url=url
            )

        # Create a new UserSettings object with the updated settings
        settings = UserSettings(
            ai_backends=ai_backends,
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

            controls["enable"].setChecked(backend_settings.enabled)
            controls["key"].setText(backend_settings.api_key)
            controls["url"].setText(backend_settings.url)

            # Update enabled state
            controls["key"].setEnabled(backend_settings.enabled)
            controls["url"].setEnabled(backend_settings.enabled)

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
