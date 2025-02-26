"""Dialog for configuring conversation-specific settings."""

from typing import Dict, List

from PySide6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QLabel, QPushButton,
    QComboBox, QDoubleSpinBox, QListView
)
from PySide6.QtCore import Signal, Qt

from humbug.ai.ai_backend import AIBackend
from humbug.ai.conversation_settings import ConversationSettings, ReasoningCapability
from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager
from humbug.language.language_manager import LanguageManager


class ConversationSettingsDialog(QDialog):
    """Dialog for editing conversation settings."""

    settings_changed = Signal(ConversationSettings)

    def __init__(self, ai_backends: Dict[str, AIBackend], parent=None):
        """Initialize the conversation settings dialog."""
        super().__init__(parent)
        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._handle_language_changed)
        strings = self._language_manager.strings

        self.setWindowTitle(strings.conversation_settings)
        self.setMinimumWidth(500)
        self.setModal(True)

        self._ai_backends = ai_backends
        self._available_models: List[str] = []
        self._initial_settings = None
        self._current_settings = None

        style_manager = StyleManager()

        # Main layout with proper spacing
        layout = QVBoxLayout()
        layout.setSpacing(12)
        layout.setContentsMargins(20, 20, 20, 20)

        # Model selection
        model_layout = QHBoxLayout()
        self._model_label = QLabel(strings.settings_model_label)
        self._model_label.setMinimumHeight(40)
        self._model_combo = QComboBox()
        self._model_combo.setView(QListView())  # Weird workaround to get styles to work!
        self._model_combo.setMinimumWidth(300)
        self._model_combo.setMinimumHeight(40)
        self._model_combo.currentTextChanged.connect(self._handle_value_change)
        model_layout.addWidget(self._model_label)
        model_layout.addStretch()
        model_layout.addWidget(self._model_combo)
        layout.addLayout(model_layout)

        # Temperature setting
        temp_layout = QHBoxLayout()
        self._temp_label = QLabel(strings.settings_temp_label)
        self._temp_label.setMinimumHeight(40)
        self._temp_spin = QDoubleSpinBox()
        self._temp_spin.setRange(0.0, 1.0)
        self._temp_spin.setSingleStep(0.100000000001)  # Increased step size to avoid FP issues
        self._temp_spin.setDecimals(1)
        self._temp_spin.setMinimumWidth(300)
        self._temp_spin.setMinimumHeight(40)
        self._temp_spin.valueChanged.connect(self._handle_value_change)
        temp_layout.addWidget(self._temp_label)
        temp_layout.addStretch()
        temp_layout.addWidget(self._temp_spin)
        layout.addLayout(temp_layout)

        # Reasoning capabilities
        reasoning_layout = QHBoxLayout()
        self._reasoning_label = QLabel(strings.settings_reasoning_label)
        self._reasoning_label.setMinimumHeight(40)
        self._reasoning_combo = QComboBox()
        self._reasoning_combo.setView(QListView())
        self._reasoning_combo.setMinimumWidth(300)
        self._reasoning_combo.setMinimumHeight(40)
        self._reasoning_combo.currentIndexChanged.connect(self._handle_value_change)
        reasoning_layout.addWidget(self._reasoning_label)
        reasoning_layout.addStretch()
        reasoning_layout.addWidget(self._reasoning_combo)
        layout.addLayout(reasoning_layout)

        # Context window display
        context_layout = QHBoxLayout()
        self._context_label = QLabel(strings.settings_context_label)
        self._context_label.setMinimumHeight(40)
        self._context_value = QLabel()
        self._context_value.setMinimumWidth(300)
        self._context_value.setMinimumHeight(40)
        self._context_value.setAlignment(Qt.AlignLeft | Qt.AlignVCenter)
        context_layout.addWidget(self._context_label)
        context_layout.addStretch()
        context_layout.addWidget(self._context_value)
        layout.addLayout(context_layout)

        # Max output display
        output_layout = QHBoxLayout()
        self._output_label = QLabel(strings.settings_max_output_label)
        self._output_label.setMinimumHeight(40)
        self._output_value = QLabel()
        self._output_value.setMinimumWidth(300)
        self._output_value.setMinimumHeight(40)
        self._output_value.setAlignment(Qt.AlignLeft | Qt.AlignVCenter)
        output_layout.addWidget(self._output_label)
        output_layout.addStretch()
        output_layout.addWidget(self._output_value)
        layout.addLayout(output_layout)

        # Add extra spacing before buttons
        layout.addSpacing(24)
        layout.addStretch()

        # Button row with proper spacing and alignment
        button_layout = QHBoxLayout()
        button_layout.setSpacing(8)

        self.ok_button = QPushButton(strings.ok)
        self.cancel_button = QPushButton(strings.cancel)
        self.apply_button = QPushButton(strings.apply)

        self.ok_button.clicked.connect(self._handle_ok)
        self.cancel_button.clicked.connect(self.reject)
        self.apply_button.clicked.connect(self._handle_apply)

        # Set minimum button widths and heights
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
                background-color: {style_manager.get_color_str(ColorRole.BACKGROUND_DIALOG)};
            }}
            QLabel {{
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                background-color: {style_manager.get_color_str(ColorRole.BACKGROUND_DIALOG)};
                border: none;
            }}
            QComboBox {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND)};
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: none;
                border-radius: 4px;
                padding: 8px;
            }}
            QComboBox:disabled {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_DISABLED)};
                color: {style_manager.get_color_str(ColorRole.TEXT_DISABLED)};
            }}
            QComboBox::drop-down {{
                border: none;
                width: 20px;
            }}
            QComboBox::down-arrow {{
                image: url({style_manager.get_icon_path("arrow-down")});
                width: 12px;
                height: 12px;
            }}
            QComboBox::down-arrow:on {{
                image: url({style_manager.get_icon_path('arrow-up')});
                width: 12px;
                height: 12px;
            }}
            QComboBox::down-arrow:disabled {{
                image: none;
            }}
            QComboBox QAbstractItemView::item:selected {{
                border: none;
                background-color: {style_manager.get_color_str(ColorRole.TEXT_SELECTED)};
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
            }}
            QComboBox QListView {{
                border: none;
                background-color: {style_manager.get_color_str(ColorRole.BACKGROUND_SECONDARY)};
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
            }}
            QDoubleSpinBox {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND)};
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: none;
                border-radius: 4px;
                padding: 8px;
            }}
            QDoubleSpinBox:disabled {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_DISABLED)};
                color: {style_manager.get_color_str(ColorRole.TEXT_DISABLED)};
            }}
            QDoubleSpinBox::up-button, QDoubleSpinBox::down-button {{
                border: none;
                width: 20px;
            }}
            QDoubleSpinBox::up-arrow {{
                image: url({style_manager.get_icon_path('arrow-up')});
                width: 12px;
                height: 12px;
            }}
            QDoubleSpinBox::up-arrow:disabled, QDoubleSpinBox::up-arrow:off {{
                image: none;
            }}
            QDoubleSpinBox::down-arrow {{
                image: url({style_manager.get_icon_path('arrow-down')});
                width: 12px;
                height: 12px;
            }}
            QDoubleSpinBox::down-arrow:disabled, QDoubleSpinBox::down-arrow:off {{
                image: none;
            }}
            QLabel[valueDisplay="true"] {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_DISABLED)};
                color: {style_manager.get_color_str(ColorRole.TEXT_DISABLED)};
                border: none;
                border-radius: 4px;
                padding: 8px;
            }}
            QPushButton {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND)};
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: none;
                border-radius: 4px;
                padding: 8px;
            }}
            QPushButton:hover {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_HOVER)};
            }}
            QPushButton:pressed {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_PRESSED)};
            }}
            QPushButton:disabled {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_DISABLED)};
                color: {style_manager.get_color_str(ColorRole.TEXT_DISABLED)};
            }}
        """)

    def _handle_language_changed(self) -> None:
        """Update dialog texts when language changes."""
        strings = self._language_manager.strings

        # Update window title
        self.setWindowTitle(strings.conversation_settings)

        # Update labels
        self._model_label.setText(strings.settings_model_label)
        self._temp_label.setText(strings.settings_temp_label)
        self._reasoning_label.setText("Reasoning Capabilities")  # TODO: Add to language strings
        self._context_label.setText(strings.settings_context_label)
        self._output_label.setText(strings.settings_max_output_label)

        # Update button texts
        self.ok_button.setText(strings.ok)
        self.cancel_button.setText(strings.cancel)
        self.apply_button.setText(strings.apply)

        # Update displays with current model
        if self._model_combo.currentText():
            self._update_model_displays(self._model_combo.currentText())

    def _update_reasoning_combo(self, model: str) -> None:
        """Update the reasoning combo box based on the current model's capabilities.

        Args:
            model: The selected model name
        """
        # Remember current selection
        current_reasoning = None
        if self._reasoning_combo.currentIndex() >= 0:
            current_reasoning = self._reasoning_combo.currentData()

        # Block signals while updating
        self._reasoning_combo.blockSignals(True)
        self._reasoning_combo.clear()

        # Get model's reasoning capabilities
        capabilities = ConversationSettings.get_reasoning_capability(model)

        # Add NO_REASONING if supported
        if capabilities & ReasoningCapability.NO_REASONING:
            self._reasoning_combo.addItem(self._language_manager.strings.settings_no_reasoning, ReasoningCapability.NO_REASONING)

        # Add HIDDEN_REASONING if supported
        if capabilities & ReasoningCapability.HIDDEN_REASONING:
            self._reasoning_combo.addItem(self._language_manager.strings.settings_no_reasoning, ReasoningCapability.HIDDEN_REASONING)

        # Add VISIBLE_REASONING if supported
        if capabilities & ReasoningCapability.VISIBLE_REASONING:
            self._reasoning_combo.addItem(self._language_manager.strings.settings_visible_reasoning, ReasoningCapability.VISIBLE_REASONING)

        # Set previous selection if possible
        if current_reasoning is not None:
            for i in range(self._reasoning_combo.count()):
                if self._reasoning_combo.itemData(i) == current_reasoning:
                    self._reasoning_combo.setCurrentIndex(i)
                    break

        # Disable combo box if only one option
        self._reasoning_combo.setEnabled(self._reasoning_combo.count() > 1)

        # Unblock signals
        self._reasoning_combo.blockSignals(False)

    def _update_model_displays(self, model: str) -> None:
        """Update the model-specific displays with proper localization."""
        strings = self._language_manager.strings
        limits = ConversationSettings.get_model_limits(model)

        # Update reasoning capabilities dropdown
        self._update_reasoning_combo(model)

        # Update context window display
        self._context_value.setText(
            f"{limits['context_window']:,} {strings.settings_tokens_label}"
        )
        self._context_value.setProperty('valueDisplay', True)
        self._context_value.style().unpolish(self._context_value)
        self._context_value.style().polish(self._context_value)

        # Update max output tokens display
        self._output_value.setText(
            f"{limits['max_output_tokens']:,} {strings.settings_tokens_label}"
        )
        self._output_value.setProperty('valueDisplay', True)
        self._output_value.style().unpolish(self._output_value)
        self._output_value.style().polish(self._output_value)

    def _handle_value_change(self) -> None:
        """Handle changes to any setting value."""
        if not self._current_settings:
            return

        current_model = self._model_combo.currentText()
        current_temp = self._temp_spin.value()
        current_reasoning = self._reasoning_combo.currentData()

        supports_temp = ConversationSettings.supports_temperature(current_model)
        self._temp_spin.setEnabled(supports_temp)
        self._update_model_displays(current_model)

        temp_changed = False
        if current_temp is None and self._current_settings.temperature is None:
            temp_changed = False
        elif current_temp is None or self._current_settings.temperature is None:
            temp_changed = True
        else:
            temp_changed = abs(current_temp - self._current_settings.temperature) > 0.01

        model_changed = current_model != self._current_settings.model
        reasoning_changed = current_reasoning != self._current_settings.reasoning

        self.apply_button.setEnabled(model_changed or temp_changed or reasoning_changed)

    def get_settings(self) -> ConversationSettings:
        """Get the current settings from the dialog."""
        model = self._model_combo.currentText()
        temperature = self._temp_spin.value()
        reasoning = self._reasoning_combo.currentData()
        return ConversationSettings(
            model=model,
            temperature=temperature,
            reasoning=reasoning
        )

    def set_settings(self, settings: ConversationSettings) -> None:
        """Set the current settings in the dialog."""
        models = []
        for model in ConversationSettings.iter_models_by_backends(self._ai_backends):
            models.append(model)

        self._available_models = models
        self._model_combo.clear()
        self._model_combo.addItems(models)

        self._initial_settings = ConversationSettings(
            model=settings.model,
            temperature=settings.temperature,
            reasoning=settings.reasoning
        )
        self._current_settings = ConversationSettings(
            model=settings.model,
            temperature=settings.temperature,
            reasoning=settings.reasoning
        )

        # Model selection
        model_index = self._model_combo.findText(settings.model)
        if model_index >= 0:
            self._model_combo.setCurrentIndex(model_index)

        self._update_model_displays(settings.model)

        # Temperature setting
        supports_temp = ConversationSettings.supports_temperature(settings.model)
        self._temp_spin.setEnabled(supports_temp)
        self._temp_spin.setValue(settings.temperature)

        # Reasoning capabilities
        # Setting the model will populate the reasoning combo box
        # We need to select the current reasoning setting
        for i in range(self._reasoning_combo.count()):
            if self._reasoning_combo.itemData(i) == settings.reasoning:
                self._reasoning_combo.setCurrentIndex(i)
                break

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
        if self._initial_settings:
            self.settings_changed.emit(self._initial_settings)
        super().reject()
