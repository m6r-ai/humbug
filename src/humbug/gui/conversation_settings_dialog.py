"""Dialog for configuring conversation-specific settings."""

from typing import Dict, List

from PySide6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QLabel, QPushButton,
    QComboBox, QDoubleSpinBox, QListView
)
from PySide6.QtCore import Signal, Qt

from humbug.ai.ai_backend import AIBackend
from humbug.ai.conversation_settings import ConversationSettings
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
        self._model_temperatures = {}

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
        self._model_combo.currentTextChanged.connect(self._handle_model_change)
        model_layout.addWidget(self._model_label)
        model_layout.addStretch()
        model_layout.addWidget(self._model_combo)
        layout.addLayout(model_layout)

        # Temperature setting
        temp_layout = QHBoxLayout()
        self._temp_label = QLabel(strings.settings_temp_label)
        self._temp_label.setMinimumHeight(40)
        self.temp_spin = QDoubleSpinBox()
        self.temp_spin.setRange(0.0, 1.0)
        self.temp_spin.setSingleStep(0.100000000001)  # Increased step size to avoid FP issues
        self.temp_spin.setDecimals(1)
        self.temp_spin.setMinimumWidth(300)
        self.temp_spin.setMinimumHeight(40)
        temp_layout.addWidget(self._temp_label)
        temp_layout.addStretch()
        temp_layout.addWidget(self.temp_spin)
        layout.addLayout(temp_layout)

        # Context window display
        context_layout = QHBoxLayout()
        self._context_label = QLabel(strings.settings_context_label)
        self._context_label.setMinimumHeight(40)
        self.context_value = QLabel()
        self.context_value.setMinimumWidth(300)
        self.context_value.setMinimumHeight(40)
        self.context_value.setAlignment(Qt.AlignLeft | Qt.AlignVCenter)
        context_layout.addWidget(self._context_label)
        context_layout.addStretch()
        context_layout.addWidget(self.context_value)
        layout.addLayout(context_layout)

        # Max output display
        output_layout = QHBoxLayout()
        self._output_label = QLabel(strings.settings_max_output_label)
        self._output_label.setMinimumHeight(40)
        self.output_value = QLabel()
        self.output_value.setMinimumWidth(300)
        self.output_value.setMinimumHeight(40)
        self.output_value.setAlignment(Qt.AlignLeft | Qt.AlignVCenter)
        output_layout.addWidget(self._output_label)
        output_layout.addStretch()
        output_layout.addWidget(self.output_value)
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

        self._model_combo.currentTextChanged.connect(self._handle_value_change)
        self.temp_spin.valueChanged.connect(self._handle_value_change)

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
        self._context_label.setText(strings.settings_context_label)
        self._output_label.setText(strings.settings_max_output_label)

        # Update button texts
        self.ok_button.setText(strings.ok)
        self.cancel_button.setText(strings.cancel)
        self.apply_button.setText(strings.apply)

        # Update displays with current model
        if self._model_combo.currentText():
            self._update_model_displays(self._model_combo.currentText())

    def _update_model_displays(self, model: str) -> None:
        """Update the model-specific displays with proper localization."""
        strings = self._language_manager.strings
        limits = ConversationSettings.get_model_limits(model)

        # Update context window display
        self.context_value.setText(
            f"{limits['context_window']:,} {strings.settings_tokens_label}"
        )
        self.context_value.setProperty('valueDisplay', True)
        self.context_value.style().unpolish(self.context_value)
        self.context_value.style().polish(self.context_value)

        # Update max output tokens display
        self.output_value.setText(
            f"{limits['max_output_tokens']:,} {strings.settings_tokens_label}"
        )
        self.output_value.setProperty('valueDisplay', True)
        self.output_value.style().unpolish(self.output_value)
        self.output_value.style().polish(self.output_value)

    def _handle_model_change(self, model: str) -> None:
        """Handle model selection changes."""
        supports_temp = ConversationSettings.supports_temperature(model)
        self.temp_spin.setEnabled(supports_temp)

        # Update displays
        self._update_model_displays(model)

        if supports_temp:
            if model in self._model_temperatures:
                self.temp_spin.setValue(self._model_temperatures[model])
            else:
                # If we haven't stored a temperature for this model yet,
                # use the initial setting if it exists, otherwise default to 0.7
                if (self._initial_settings and
                    self._initial_settings.model == model and
                    self._initial_settings.temperature is not None):
                    self.temp_spin.setValue(self._initial_settings.temperature)
                else:
                    self.temp_spin.setValue(0.7)
        else:
            self.temp_spin.setValue(0.0)

        # Store the temperature for the current model if it supports it
        if supports_temp:
            self._model_temperatures[model] = self.temp_spin.value()

        self._handle_value_change()

    def _handle_value_change(self) -> None:
        """Handle changes to any setting value."""
        if not self._current_settings:
            return

        current_model = self._model_combo.currentText()
        current_temp = self.temp_spin.value() if ConversationSettings.supports_temperature(current_model) else None

        self.apply_button.setEnabled(
            current_model != self._current_settings.model or
            abs(current_temp - self._current_settings.temperature) > 0.01
        )

    def get_settings(self) -> ConversationSettings:
        """Get the current settings from the dialog."""
        model = self._model_combo.currentText()
        temperature = self.temp_spin.value() if ConversationSettings.supports_temperature(model) else None
        return ConversationSettings(model=model, temperature=temperature)

    def set_settings(self, settings: ConversationSettings) -> None:
        """Set the current settings in the dialog."""
        models = []
        for model in ConversationSettings.AVAILABLE_MODELS:
            provider = ConversationSettings.get_provider(model)
            if provider in self._ai_backends:
                models.append(model)

        self._available_models = models
        self._model_combo.clear()
        self._model_combo.addItems(models)

        self._initial_settings = ConversationSettings(
            model=settings.model,
            temperature=settings.temperature
        )
        self._current_settings = ConversationSettings(
            model=settings.model,
            temperature=settings.temperature
        )

        # Initialize temperature tracking for this dialog session
        self._model_temperatures = {
            settings.model: settings.temperature if settings.temperature is not None else 0.7
        }

        model_index = self._model_combo.findText(settings.model)
        if model_index >= 0:
            self._model_combo.setCurrentIndex(model_index)

        # Update displays for the current model
        self._update_model_displays(settings.model)

        supports_temp = ConversationSettings.supports_temperature(settings.model)
        self.temp_spin.setEnabled(supports_temp)
        if supports_temp and settings.temperature is not None:
            self.temp_spin.setValue(settings.temperature)
        else:
            self.temp_spin.setValue(0.0)

        self.apply_button.setEnabled(False)

    def _handle_apply(self) -> None:
        """Handle Apply button click."""
        current_model = self._model_combo.currentText()
        if ConversationSettings.supports_temperature(current_model):
            self._model_temperatures[current_model] = self.temp_spin.value()

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
