"""
Dialog for configuring mindspace-specific settings using the settings framework.

This dialog allows users to configure mindspace settings such as tab behavior and size.
Settings are persisted to the mindspace's settings.json file.
"""

from typing import cast

from PySide6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QPushButton, QScrollArea, QWidget, QFrame
)
from PySide6.QtCore import Signal

from ai import AIConversationSettings, AIReasoningCapability
from ai_tool import AIToolManager

from humbug.language.language_manager import LanguageManager
from humbug.mindspace.mindspace_settings import MindspaceSettings
from humbug.settings.settings_checkbox import SettingsCheckbox
from humbug.settings.settings_container import SettingsContainer
from humbug.settings.settings_factory import SettingsFactory
from humbug.style_manager import StyleManager
from humbug.user.user_manager import UserManager


class MindspaceSettingsDialog(QDialog):
    """Dialog for editing mindspace settings using the settings framework."""

    settings_changed = Signal(MindspaceSettings)

    def __init__(self, parent: QWidget | None = None) -> None:
        """
        Initialize the mindspace settings dialog.

        Args:
            parent: Parent widget, typically the main window.
        """
        super().__init__(parent)
        self._language_manager = LanguageManager()
        strings = self._language_manager.strings()

        self.setWindowTitle(strings.mindspace_settings)
        self.setMinimumWidth(800)
        self.setMinimumHeight(700)
        self.setModal(True)

        self._initial_settings: MindspaceSettings | None = None
        self._current_settings: MindspaceSettings | None = None
        self._tool_checkboxes: dict[str, QWidget] = {}

        self._tool_manager = AIToolManager()

        style_manager = StyleManager()

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

        # Model section
        model_section = SettingsFactory.create_section(strings.model_settings)
        self._settings_container.add_setting(model_section)

        # Model selection
        self._model_combo = SettingsFactory.create_combo(strings.settings_model_label)
        self._settings_container.add_setting(self._model_combo)

        # Temperature setting
        self._temp_spin = SettingsFactory.create_double_spinbox(
            strings.settings_temp_label, 0.0, 1.0, 0.1, 1
        )
        self._settings_container.add_setting(self._temp_spin)

        # Reasoning capabilities
        self._reasoning_combo = SettingsFactory.create_combo(strings.settings_reasoning_label)
        self._settings_container.add_setting(self._reasoning_combo)

        # AI tools section
        spacer = SettingsFactory.create_spacer(24)
        self._settings_container.add_setting(spacer)

        # Tools section
        tools_section = SettingsFactory.create_section(strings.tool_settings, strings.tools_description)
        self._settings_container.add_setting(tools_section)

        # Create checkboxes for each tool
        self._create_tool_checkboxes()

        spacer = SettingsFactory.create_spacer(24)
        self._settings_container.add_setting(spacer)

        # Editor section
        editor_section = SettingsFactory.create_section(strings.editor_settings)
        self._settings_container.add_setting(editor_section)

        # Soft tabs setting
        self._soft_tabs_check = SettingsFactory.create_checkbox(strings.use_soft_tabs)
        self._settings_container.add_setting(self._soft_tabs_check)

        # Tab size setting
        self._tab_size_spin = SettingsFactory.create_spinbox(
            strings.tab_size, 1, 8, 1
        )
        self._settings_container.add_setting(self._tab_size_spin)

        spacer = SettingsFactory.create_spacer(24)
        self._settings_container.add_setting(spacer)

        # Backup section
        backup_section = SettingsFactory.create_section(strings.backup_settings)
        self._settings_container.add_setting(backup_section)

        # Auto-backup setting
        self._auto_backup_check = SettingsFactory.create_checkbox(strings.auto_backup)
        self._settings_container.add_setting(self._auto_backup_check)

        # Backup interval setting
        self._backup_interval_spin = SettingsFactory.create_spinbox(
            strings.backup_interval, 60, 3600, 60
        )
        self._settings_container.add_setting(self._backup_interval_spin)

        # Add stretch to push content up
        self._settings_container.add_stretch()

        # Connect change handlers
        self._auto_backup_check.value_changed.connect(self._handle_auto_backup_change)
        self._model_combo.value_changed.connect(self._handle_model_change)
        self._settings_container.value_changed.connect(self._handle_value_change)

        # Get AI backends for model options
        self._user_manager = UserManager()

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
        zoom_factor = style_manager.zoom_factor()
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
        self.setStyleSheet(style_manager.get_dialog_stylesheet())

    def _create_tool_checkboxes(self) -> None:
        """Create checkboxes for each available tool."""
        tool_configs = self._tool_manager.get_all_tool_configs()
        for config in tool_configs:
            checkbox = SettingsFactory.create_checkbox(config.display_name)
            self._tool_checkboxes[config.name] = checkbox
            self._settings_container.add_setting(checkbox)

    def _handle_auto_backup_change(self) -> None:
        """Handle changes to auto backup checkbox."""
        auto_backup_checked = self._auto_backup_check.get_value()
        self._backup_interval_spin.set_enabled(auto_backup_checked)

    def _handle_model_change(self) -> None:
        """Handle model selection changes."""
        current_model = self._model_combo.get_text()
        self._update_model_capabilities(current_model)

    def _update_reasoning_combo(self, model: str) -> None:
        """Update the reasoning combo box based on the current model's capabilities."""
        # Get model's reasoning capabilities
        capabilities = AIConversationSettings.get_reasoning_capability(model)
        strings = self._language_manager.strings()

        # Create items for the combo box
        items = []

        # Add NO_REASONING if supported
        if capabilities & AIReasoningCapability.NO_REASONING:
            items.append((strings.settings_no_reasoning, AIReasoningCapability.NO_REASONING))

        # Add HIDDEN_REASONING if supported
        if capabilities & AIReasoningCapability.HIDDEN_REASONING:
            items.append((strings.settings_hidden_reasoning, AIReasoningCapability.HIDDEN_REASONING))

        # Add VISIBLE_REASONING if supported
        if capabilities & AIReasoningCapability.VISIBLE_REASONING:
            items.append((strings.settings_visible_reasoning, AIReasoningCapability.VISIBLE_REASONING))

        # Update the combo box items
        self._reasoning_combo.set_items(items)

        # Disable combo box if only one option
        self._reasoning_combo.setEnabled(len(items) > 1)

    def _update_model_capabilities(self, model: str) -> None:
        """Update settings based on model capabilities."""
        # Update reasoning capabilities dropdown
        self._update_reasoning_combo(model)

        # Update temperature setting
        supports_temp = AIConversationSettings.supports_temperature(model)
        self._temp_spin.set_enabled(supports_temp)

    def _handle_value_change(self) -> None:
        """Handle changes to any setting value."""
        if not self._current_settings:
            return

        self.apply_button.setEnabled(self._settings_container.is_modified())

    def get_settings(self) -> MindspaceSettings:
        """Get current settings from dialog."""
        # Get enabled tools from checkboxes
        enabled_tools = {}
        for tool_name, checkbox in self._tool_checkboxes.items():
            enabled_tools[tool_name] = cast(SettingsCheckbox, checkbox).get_value()

        return MindspaceSettings(
            use_soft_tabs=self._soft_tabs_check.get_value(),
            tab_size=self._tab_size_spin.get_value(),
            auto_backup=self._auto_backup_check.get_value(),
            auto_backup_interval=self._backup_interval_spin.get_value(),
            model=self._model_combo.get_text(),
            temperature=self._temp_spin.get_value(),
            reasoning=self._reasoning_combo.get_value(),
            enabled_tools=enabled_tools
        )

    def set_settings(self, settings: MindspaceSettings) -> None:
        """Update dialog with current settings."""
        self._initial_settings = settings
        self._current_settings = MindspaceSettings(
            use_soft_tabs=settings.use_soft_tabs,
            tab_size=settings.tab_size,
            auto_backup=settings.auto_backup,
            auto_backup_interval=settings.auto_backup_interval,
            model=settings.model,
            temperature=settings.temperature,
            reasoning=settings.reasoning,
            enabled_tools=settings.enabled_tools.copy()
        )

        # Editor settings
        self._soft_tabs_check.set_value(settings.use_soft_tabs)
        self._tab_size_spin.set_value(settings.tab_size)
        self._auto_backup_check.set_value(settings.auto_backup)
        self._backup_interval_spin.set_value(settings.auto_backup_interval)
        self._backup_interval_spin.set_enabled(settings.auto_backup)

        # Populate model combo
        ai_backends = self._user_manager.get_ai_backends()
        models = []
        for model in AIConversationSettings.iter_models_by_backends(ai_backends):
            models.append((model, model))  # (display_text, data_value)

        self._model_combo.set_items(models)
        self._model_combo.set_value(settings.model)

        # Set temperature
        self._temp_spin.set_value(settings.temperature)

        # Update reasoning options and select current value
        self._update_model_capabilities(settings.model)
        self._reasoning_combo.set_value(settings.reasoning)

        # Set tool checkboxes
        for tool_name, checkbox in self._tool_checkboxes.items():
            enabled = settings.enabled_tools.get(tool_name, True)
            cast(SettingsCheckbox, checkbox).set_value(enabled)

        # Reset the modified state
        self._settings_container.reset_modified_state()
        self.apply_button.setEnabled(False)

    def _handle_apply(self) -> None:
        """Handle Apply button click."""
        settings = self.get_settings()
        self._current_settings = settings

        # Update the tool manager with the new settings
        self._tool_manager.set_tool_enabled_states(settings.enabled_tools)

        self.settings_changed.emit(settings)
        self._settings_container.reset_modified_state()
        self.apply_button.setEnabled(False)

    def _handle_ok(self) -> None:
        """Handle OK button click."""
        self._handle_apply()
        self.accept()

    def reject(self) -> None:
        """Handle Cancel button click."""
        if self._initial_settings:
            # Restore original tool settings
            self._tool_manager.set_tool_enabled_states(self._initial_settings.enabled_tools)

            self.settings_changed.emit(self._initial_settings)

        super().reject()
