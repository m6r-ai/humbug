"""Dialog for configuring conversation-specific settings."""

from PySide6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QLabel, QPushButton,
    QComboBox, QDoubleSpinBox
)
from PySide6.QtCore import QSize, Signal

from humbug.ai.conversation_settings import ConversationSettings


class SettingsDialog(QDialog):
    """Dialog for editing conversation settings."""

    settings_changed = Signal(ConversationSettings)

    def __init__(self, parent=None):
        """Initialize the settings dialog."""
        super().__init__(parent)
        self.setWindowTitle("Conversation Settings")
        self.setFixedSize(QSize(400, 200))
        self.setModal(True)

        self.available_models = ConversationSettings.get_available_models()
        self.initial_settings = None
        self.current_settings = None
        self.model_temperatures = {}

        self.setup_ui()

    def setup_ui(self):
        """Set up the dialog UI."""
        layout = QVBoxLayout()
        layout.setSpacing(10)
        layout.setContentsMargins(20, 20, 20, 20)

        # Model selection
        model_layout = QHBoxLayout()
        model_label = QLabel("AI Model:")
        self.model_combo = QComboBox()
        self.model_combo.addItems(self.available_models)
        self.model_combo.currentTextChanged.connect(self._handle_model_change)
        model_layout.addWidget(model_label)
        model_layout.addWidget(self.model_combo)
        layout.addLayout(model_layout)

        # Temperature setting
        temp_layout = QHBoxLayout()
        temp_label = QLabel("Temperature:")
        self.temp_spin = QDoubleSpinBox()
        self.temp_spin.setRange(0.0, 1.0)
        self.temp_spin.setSingleStep(0.1)
        self.temp_spin.setDecimals(1)
        temp_layout.addWidget(temp_label)
        temp_layout.addWidget(self.temp_spin)
        layout.addLayout(temp_layout)

        layout.addStretch()

        # Button row
        button_layout = QHBoxLayout()
        button_layout.setSpacing(10)

        self.ok_button = QPushButton("OK")
        self.cancel_button = QPushButton("Cancel")
        self.apply_button = QPushButton("Apply")

        self.ok_button.clicked.connect(self._handle_ok)
        self.cancel_button.clicked.connect(self.reject)
        self.apply_button.clicked.connect(self._handle_apply)

        self.model_combo.currentTextChanged.connect(self._handle_value_change)
        self.temp_spin.valueChanged.connect(self._handle_value_change)

        button_layout.addStretch()
        button_layout.addWidget(self.ok_button)
        button_layout.addWidget(self.cancel_button)
        button_layout.addWidget(self.apply_button)

        layout.addLayout(button_layout)
        self.setLayout(layout)

        self.setStyleSheet("""
            QDialog {
                background-color: #2d2d2d;
                color: white;
            }
            QLabel {
                color: white;
            }
            QComboBox {
                background-color: #3d3d3d;
                color: white;
                border: 1px solid #4d4d4d;
                border-radius: 2px;
                padding: 5px;
                min-width: 200px;
            }
            QComboBox::drop-down {
                border: none;
                background-color: #4d4d4d;
            }
            QComboBox::down-arrow {
                image: none;
                border-left: 5px solid transparent;
                border-right: 5px solid transparent;
                border-top: 5px solid white;
                width: 0;
                height: 0;
                margin-right: 5px;
            }
            QDoubleSpinBox {
                background-color: #3d3d3d;
                color: white;
                border: 1px solid #4d4d4d;
                border-radius: 2px;
                padding: 5px;
                min-width: 200px;
            }
            QDoubleSpinBox:disabled {
                background-color: #2d2d2d;
                color: #808080;
                border: 1px solid #3d3d3d;
            }
            QPushButton {
                background-color: #4d4d4d;
                color: white;
                border: none;
                border-radius: 2px;
                padding: 5px 15px;
                min-width: 70px;
            }
            QPushButton:hover {
                background-color: #5d5d5d;
            }
            QPushButton:pressed {
                background-color: #3d3d3d;
            }
            QPushButton:disabled {
                background-color: #2d2d2d;
                color: #808080;
            }
        """)

    def _handle_model_change(self, model: str):
        """Handle model selection changes."""
        supports_temp = ConversationSettings.supports_temperature(model)
        self.temp_spin.setEnabled(supports_temp)

        if supports_temp:
            if model in self.model_temperatures:
                self.temp_spin.setValue(self.model_temperatures[model])
            else:
                # If we haven't stored a temperature for this model yet,
                # use the initial setting if it exists, otherwise default to 0.7
                if (self.initial_settings and 
                    self.initial_settings.model == model and 
                    self.initial_settings.temperature is not None):
                    self.temp_spin.setValue(self.initial_settings.temperature)
                else:
                    self.temp_spin.setValue(0.7)
        else:
            self.temp_spin.setValue(0.0)

        # Store the temperature for the current model if it supports it
        if supports_temp:
            self.model_temperatures[model] = self.temp_spin.value()

        self._handle_value_change()

    def _handle_value_change(self):
        """Handle changes to any setting value."""
        if not self.current_settings:
            return

        current_model = self.model_combo.currentText()
        current_temp = self.temp_spin.value() if ConversationSettings.supports_temperature(current_model) else None

        self.apply_button.setEnabled(
            current_model != self.current_settings.model or
            current_temp != self.current_settings.temperature
        )

    def get_settings(self) -> ConversationSettings:
        """Get the current settings from the dialog."""
        model = self.model_combo.currentText()
        temperature = self.temp_spin.value() if ConversationSettings.supports_temperature(model) else None
        return ConversationSettings(model=model, temperature=temperature)

    def set_settings(self, settings: ConversationSettings):
        """Set the current settings in the dialog."""
        self.initial_settings = ConversationSettings(
            model=settings.model,
            temperature=settings.temperature
        )
        self.current_settings = ConversationSettings(
            model=settings.model,
            temperature=settings.temperature
        )

        # Initialize temperature tracking for this dialog session
        self.model_temperatures = {
            settings.model: settings.temperature if settings.temperature is not None else 0.7
        }

        model_index = self.model_combo.findText(settings.model)
        if model_index >= 0:
            self.model_combo.setCurrentIndex(model_index)

        supports_temp = ConversationSettings.supports_temperature(settings.model)
        self.temp_spin.setEnabled(supports_temp)
        if supports_temp and settings.temperature is not None:
            self.temp_spin.setValue(settings.temperature)
        else:
            self.temp_spin.setValue(0.0)

        self.apply_button.setEnabled(False)

    def _handle_apply(self):
        """Handle Apply button click."""
        current_model = self.model_combo.currentText()
        if ConversationSettings.supports_temperature(current_model):
            self.model_temperatures[current_model] = self.temp_spin.value()

        settings = self.get_settings()
        self.current_settings = settings
        self.settings_changed.emit(settings)
        self.apply_button.setEnabled(False)

    def _handle_ok(self):
        """Handle OK button click."""
        self._handle_apply()
        self.accept()

    def reject(self):
        """Handle Cancel button click."""
        if self.initial_settings:
            self.settings_changed.emit(self.initial_settings)
        super().reject()
