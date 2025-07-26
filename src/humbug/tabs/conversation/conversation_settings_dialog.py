"""
Dialog for configuring conversation-specific settings using the settings framework.
"""

from PySide6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QPushButton, QScrollArea, QWidget, QFrame
)
from PySide6.QtCore import Signal

from ai import AIConversationSettings, ReasoningCapability

from humbug.language.language_manager import LanguageManager
from humbug.settings.settings_container import SettingsContainer
from humbug.settings.settings_factory import SettingsFactory
from humbug.style_manager import StyleManager
from humbug.user.user_manager import UserManager


class ConversationSettingsDialog(QDialog):
    """Dialog for editing conversation settings using the settings framework."""

    settings_changed = Signal(AIConversationSettings)

    def __init__(self, parent: QWidget | None = None) -> None:
        """Initialize the conversation settings dialog."""
        super().__init__(parent)
        self._language_manager = LanguageManager()
        strings = self._language_manager.strings()

        self.setWindowTitle(strings.conversation_settings)
        self.setMinimumWidth(800)
        self.setModal(True)

        self._user_manager = UserManager()
        self._ai_backends = self._user_manager.get_ai_backends()
        self._initial_settings: AIConversationSettings | None = None
        self._current_settings: AIConversationSettings | None = None

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

        # Add a section title
        model_section = SettingsFactory.create_section(strings.model_settings)
        self._settings_container.add_setting(model_section)

        # Create model selection
        self._model_combo = SettingsFactory.create_combo(strings.settings_model_label)
        self._settings_container.add_setting(self._model_combo)

        # Create temperature setting
        self._temp_spin = SettingsFactory.create_double_spinbox(
            strings.settings_temp_label, 0.0, 1.0, 0.1, 1
        )
        self._settings_container.add_setting(self._temp_spin)

        # Create reasoning capabilities
        self._reasoning_combo = SettingsFactory.create_combo(strings.settings_reasoning_label)
        self._settings_container.add_setting(self._reasoning_combo)

        spacer = SettingsFactory.create_spacer(24)
        self._settings_container.add_setting(spacer)

        # Add model info section
        info_section = SettingsFactory.create_section(strings.model_info)
        self._settings_container.add_setting(info_section)

        # Create context window display
        self._context_display = SettingsFactory.create_display(strings.settings_context_label)
        self._settings_container.add_setting(self._context_display)

        # Create max output display
        self._output_display = SettingsFactory.create_display(strings.settings_max_output_label)
        self._settings_container.add_setting(self._output_display)

        # Add stretch to push content to top
        self._settings_container.add_stretch()

        # Connect change handlers
        self._model_combo.value_changed.connect(self._handle_model_change)
        self._settings_container.value_changed.connect(self._handle_value_change)

        # Set the scroll content
        scroll_area.setWidget(self._settings_container)
        main_layout.addWidget(scroll_area)
        main_layout.addSpacing(20)

        # Button row with proper spacing and alignment
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

        # Set minimum button widths and heights
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

    def _handle_model_change(self) -> None:
        """Handle model selection changes."""
        current_model = self._model_combo.get_text()
        self._update_model_displays(current_model)

    def _update_reasoning_combo(self, model: str) -> None:
        """Update the reasoning combo box based on the current model's capabilities."""
        # Get model's reasoning capabilities
        capabilities = AIConversationSettings.get_reasoning_capability(model)
        strings = self._language_manager.strings()

        # Create items for the combo box
        items = []

        # Add NO_REASONING if supported
        if capabilities & ReasoningCapability.NO_REASONING:
            items.append((strings.settings_no_reasoning, ReasoningCapability.NO_REASONING))

        # Add HIDDEN_REASONING if supported
        if capabilities & ReasoningCapability.HIDDEN_REASONING:
            items.append((strings.settings_hidden_reasoning, ReasoningCapability.HIDDEN_REASONING))

        # Add VISIBLE_REASONING if supported
        if capabilities & ReasoningCapability.VISIBLE_REASONING:
            items.append((strings.settings_visible_reasoning, ReasoningCapability.VISIBLE_REASONING))

        # Update the combo box items
        self._reasoning_combo.set_items(items)

        # Disable combo box if only one option
        self._reasoning_combo.setEnabled(len(items) > 1)

    def _update_model_displays(self, model: str) -> None:
        """Update the model-specific displays with proper localization."""
        strings = self._language_manager.strings()
        limits = AIConversationSettings.get_model_limits(model)

        # Update reasoning capabilities dropdown
        self._update_reasoning_combo(model)

        # Update temperature setting
        supports_temp = AIConversationSettings.supports_temperature(model)
        self._temp_spin.set_enabled(supports_temp)

        # Update context window display
        self._context_display.set_value(
            f"{limits['context_window']:,} {strings.settings_tokens_label}"
        )

        # Update max output tokens display
        self._output_display.set_value(
            f"{limits['max_output_tokens']:,} {strings.settings_tokens_label}"
        )

    def _handle_value_change(self) -> None:
        """Handle changes to any setting value."""
        if not self._current_settings:
            return

        self.apply_button.setEnabled(self._settings_container.is_modified())

    def get_settings(self) -> AIConversationSettings:
        """Get the current settings from the dialog."""
        model = self._model_combo.get_text()
        temperature = self._temp_spin.get_value()
        reasoning = self._reasoning_combo.get_value()
        return AIConversationSettings(
            model=model,
            temperature=temperature,
            reasoning=reasoning
        )

    def set_settings(self, settings: AIConversationSettings) -> None:
        """Set the current settings in the dialog."""
        self._initial_settings = AIConversationSettings(
            model=settings.model,
            temperature=settings.temperature,
            reasoning=settings.reasoning
        )
        self._current_settings = AIConversationSettings(
            model=settings.model,
            temperature=settings.temperature,
            reasoning=settings.reasoning
        )

        # Populate model combo
        models = []
        for model in AIConversationSettings.iter_models_by_backends(self._ai_backends):
            models.append((model, model))  # (display_text, data_value)

        self._model_combo.set_items(models)
        self._model_combo.set_value(settings.model)

        # Set temperature
        self._temp_spin.set_value(settings.temperature)

        # Set reasoning and update model displays
        self._update_model_displays(settings.model)
        self._reasoning_combo.set_value(settings.reasoning)

        # Reset modified state
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
        if self._initial_settings:
            self.settings_changed.emit(self._initial_settings)

        super().reject()
