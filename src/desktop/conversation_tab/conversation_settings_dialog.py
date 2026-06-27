"""Dialog for configuring conversation-specific settings using the settings framework."""


from PySide6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QPushButton, QScrollArea, QWidget, QFrame
)
from PySide6.QtCore import Qt, Signal

from ai import AIConversationSettings, AIReasoningCapability, AIManager
from ai.ai_model import AIReasoningEffort

from desktop.language.language_manager import LanguageManager
from desktop.ai_backend_display import get_all_backend_display_names, get_backend_display_name
from desktop.settings.settings_container import SettingsContainer
from desktop.settings.settings_factory import SettingsFactory
from desktop.style_manager import StyleManager


class ConversationSettingsDialog(QDialog):
    """Dialog for editing conversation settings using the settings framework."""

    settings_changed = Signal(AIConversationSettings)

    def __init__(self, parent: QWidget | None = None) -> None:
        """Initialize the conversation settings dialog."""
        super().__init__(parent)
        self.setWindowModality(Qt.WindowModality.WindowModal)
        self._language_manager = LanguageManager()
        strings = self._language_manager.strings()

        self.setWindowTitle(strings.conversation_settings)
        self.setMinimumWidth(800)
        self.setMinimumHeight(600)

        self._ai_manager = AIManager()
        self._ai_backends = self._ai_manager.get_backends()
        self._initial_settings: AIConversationSettings | None = None
        self._current_settings: AIConversationSettings | None = None

        style_manager = StyleManager()
        zoom_factor = style_manager.zoom_factor()
        spacing = int(style_manager.message_bubble_spacing() * zoom_factor)

        # Main layout
        main_layout = QVBoxLayout()
        main_layout.setSpacing(0)
        main_layout.setContentsMargins(0, 0, 0, 0)

        # Create a scroll area
        scroll_area = QScrollArea()
        scroll_area.setWidgetResizable(True)
        scroll_area.setFrameShape(QFrame.Shape.NoFrame)

        # Create settings container
        self._settings_container = SettingsContainer()

        self._settings_container.setContentsMargins(spacing, spacing, spacing, spacing)

        # Page heading and model subsection
        page_heading = SettingsFactory.create_page_heading(strings.conversation_settings)
        self._settings_container.add_setting(page_heading)

        model_section = SettingsFactory.create_section(strings.model_settings)
        self._settings_container.add_setting(model_section)

        # Provider filter
        self._model_filter_combo = SettingsFactory.create_combo("Provider")
        self._settings_container.add_setting(self._model_filter_combo)

        # Create model selection (searchable, grouped by provider)
        self._model_combo = SettingsFactory.create_combo(strings.settings_model_label)
        self._model_combo.set_searchable(True)
        self._settings_container.add_setting(self._model_combo)

        # Create reasoning capabilities
        self._reasoning_combo = SettingsFactory.create_combo(strings.settings_reasoning_label)
        self._settings_container.add_setting(self._reasoning_combo)

        # Create reasoning effort selection
        self._effort_combo = SettingsFactory.create_combo(strings.settings_reasoning_effort_label)
        self._settings_container.add_setting(self._effort_combo)

        # Create temperature setting
        self._temp_spin = SettingsFactory.create_double_spinbox(
            strings.settings_temp_label, 0.0, 1.0, 0.1, 1
        )
        self._settings_container.add_setting(self._temp_spin)

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
        self._model_filter_combo.value_changed.connect(self._on_model_filter_changed)
        self._model_combo.value_changed.connect(self._on_model_value_changed)
        self._effort_combo.value_changed.connect(self._on_effort_value_changed)
        self._settings_container.value_changed.connect(self._on_settings_value_changed)

        # Set the scroll content
        scroll_area.setWidget(self._settings_container)
        main_layout.addWidget(scroll_area)

        # Separator line above buttons
        separator = QFrame()
        separator.setFrameShape(QFrame.Shape.HLine)
        separator.setObjectName("SettingsSeparator")
        main_layout.addWidget(separator)

        # Button row with proper spacing and alignment
        button_layout = QHBoxLayout()
        button_layout.setSpacing(spacing)
        button_layout.setContentsMargins(20, 12, 20, 12)
        button_layout.addStretch()

        self.ok_button = QPushButton(strings.ok)
        self.ok_button.clicked.connect(self._on_ok_clicked)
        self.ok_button.setProperty("recommended", True)

        self.apply_button = QPushButton(strings.apply)
        self.apply_button.clicked.connect(self._on_apply_clicked)

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

    def _populate_model_filter_combo(self) -> None:
        strings = self._language_manager.strings()
        seen: set = set()
        items: list[tuple] = [("All Providers", None)]
        for (_, provider) in AIConversationSettings.iter_models_by_backends(self._ai_backends):
            if provider not in seen:
                seen.add(provider)
                items.append((get_backend_display_name(provider, strings), provider))

        self._model_filter_combo.set_items(items)

    def _populate_model_combo(self, filter_provider: str | None) -> None:
        provider_names = get_all_backend_display_names(self._language_manager.strings())
        grouped: dict[str, list[tuple[str, tuple[str, str]]]] = {}
        for (model, provider) in AIConversationSettings.iter_models_by_backends(self._ai_backends):
            if filter_provider and provider != filter_provider:
                continue

            display = AIConversationSettings.get_display_name(model, provider)
            grouped.setdefault(provider, []).append((display, (model, provider)))

        if filter_provider:
            items = [item for entries in grouped.values() for item in entries]
            self._model_combo.set_items(items)

        else:
            groups = [
                (provider_names.get(p, p), entries)
                for p, entries in grouped.items()
            ]
            self._model_combo.set_grouped_items(groups)

    def _on_model_filter_changed(self) -> None:
        self._populate_model_combo(self._model_filter_combo.get_value())

    def _on_model_value_changed(self) -> None:
        """Handle model selection changes."""
        key = self._model_combo.get_value()
        if not isinstance(key, tuple):
            return

        model, provider = key
        self._update_model_displays(model, provider)

    def _on_effort_value_changed(self) -> None:
        """Handle reasoning effort changes — temperature support may change."""
        key = self._model_combo.get_value()
        if not isinstance(key, tuple):
            return

        model, provider = key
        effort = self._effort_combo.get_value() if AIConversationSettings.get_supported_reasoning_efforts(model, provider) else None
        supports_temp = AIConversationSettings.supports_temperature(model, provider, effort)
        self._temp_spin.set_enabled(supports_temp)

    def _get_effort_label(self, effort: str) -> str:
        """Map an AIReasoningEffort constant to its localised display label."""
        strings = self._language_manager.strings()
        effort_labels = {
            AIReasoningEffort.NONE: strings.settings_effort_none,
            AIReasoningEffort.MINIMAL: strings.settings_effort_minimal,
            AIReasoningEffort.LOW: strings.settings_effort_low,
            AIReasoningEffort.MEDIUM: strings.settings_effort_medium,
            AIReasoningEffort.HIGH: strings.settings_effort_high,
            AIReasoningEffort.XHIGH: strings.settings_effort_xhigh,
            AIReasoningEffort.MAX: strings.settings_effort_max,
        }
        return effort_labels.get(effort, effort)

    def _update_reasoning_combo(self, model: str, provider: str) -> None:
        """Update the reasoning combo box based on the current model's capabilities."""
        capabilities = AIConversationSettings.get_reasoning_capability(model, provider)
        strings = self._language_manager.strings()

        items = []

        if capabilities & AIReasoningCapability.NO_REASONING:
            items.append((strings.settings_no_reasoning, AIReasoningCapability.NO_REASONING))

        if capabilities & AIReasoningCapability.HIDDEN_REASONING:
            items.append((strings.settings_hidden_reasoning, AIReasoningCapability.HIDDEN_REASONING))

        if capabilities & AIReasoningCapability.VISIBLE_REASONING:
            items.append((strings.settings_visible_reasoning, AIReasoningCapability.VISIBLE_REASONING))

        self._reasoning_combo.set_items(items)
        self._reasoning_combo.setEnabled(len(items) > 1)

    def _update_effort_combo(self, model: str, provider: str) -> None:
        """Update the reasoning effort combo box based on the current model's supported efforts."""
        efforts = AIConversationSettings.get_supported_reasoning_efforts(model, provider)

        if not efforts:
            self._effort_combo.set_items([])
            self._effort_combo.setEnabled(False)
            self._effort_combo.setVisible(False)
            return

        items = [(self._get_effort_label(e), e) for e in efforts]
        self._effort_combo.set_items(items)
        self._effort_combo.setEnabled(len(items) > 1)
        self._effort_combo.setVisible(True)

    def _update_model_displays(self, model: str, provider: str) -> None:
        """Update the model-specific displays with proper localization."""
        strings = self._language_manager.strings()
        limits = AIConversationSettings.get_model_limits(model, provider)

        # Update reasoning capabilities dropdown
        self._update_reasoning_combo(model, provider)

        # Update reasoning effort dropdown
        self._update_effort_combo(model, provider)

        # Update temperature setting — effort level may affect temperature support
        effort = self._effort_combo.get_value() if AIConversationSettings.get_supported_reasoning_efforts(model, provider) else None
        supports_temp = AIConversationSettings.supports_temperature(model, provider, effort)
        self._temp_spin.set_enabled(supports_temp)

        # Update context window display
        self._context_display.set_value(
            f"{limits['context_window']:,} {strings.settings_tokens_label}"
        )

        # Update max output tokens display
        self._output_display.set_value(
            f"{limits['max_output_tokens']:,} {strings.settings_tokens_label}"
        )

    def _on_settings_value_changed(self) -> None:
        """Handle changes to any setting value."""
        if not self._current_settings:
            return

        self.apply_button.setEnabled(self._settings_container.is_modified())

    def get_settings(self) -> AIConversationSettings:
        """Get the current settings from the dialog."""
        key = self._model_combo.get_value()
        model, provider = key if isinstance(key, tuple) else ("", "")
        temperature = self._temp_spin.get_value()
        reasoning = self._reasoning_combo.get_value()
        effort = self._effort_combo.get_value() if AIConversationSettings.get_supported_reasoning_efforts(model, provider) else None
        return AIConversationSettings(
            model=model,
            provider=provider,
            temperature=temperature,
            reasoning=reasoning,
            reasoning_effort=effort,
        )

    def set_settings(self, settings: AIConversationSettings) -> None:
        """Set the current settings in the dialog."""
        self._initial_settings = AIConversationSettings(
            model=settings.model,
            provider=settings.provider,
            temperature=settings.temperature,
            reasoning=settings.reasoning,
            reasoning_effort=settings.reasoning_effort,
        )
        self._current_settings = AIConversationSettings(
            model=settings.model,
            provider=settings.provider,
            temperature=settings.temperature,
            reasoning=settings.reasoning,
            reasoning_effort=settings.reasoning_effort,
        )

        # Populate filter then model combo (grouped by provider)
        self._populate_model_filter_combo()
        self._populate_model_combo(filter_provider=None)
        self._model_combo.set_value((settings.model, settings.provider))

        # Set temperature
        self._temp_spin.set_value(settings.temperature)

        # Update model displays (populates both combos with correct items)
        self._update_model_displays(settings.model, settings.provider)

        # Restore saved effort selection (must happen after _update_effort_combo populates items)
        if settings.reasoning_effort is not None:
            self._effort_combo.set_value(settings.reasoning_effort)

        # Set reasoning capability
        self._reasoning_combo.set_value(settings.reasoning)

        # Reset modified state
        self._settings_container.reset_modified_state()
        self.apply_button.setEnabled(False)

    def _on_apply_clicked(self) -> None:
        """Handle Apply button click."""
        settings = self.get_settings()
        self._current_settings = settings
        self.settings_changed.emit(settings)
        self._settings_container.reset_modified_state()
        self.apply_button.setEnabled(False)

    def _on_ok_clicked(self) -> None:
        """Handle OK button click."""
        self._on_apply_clicked()
        self.accept()

    def reject(self) -> None:
        """Handle Cancel button click."""
        if self._initial_settings:
            self.settings_changed.emit(self._initial_settings)

        super().reject()
