"""Dialog for configuring conversation-specific settings."""

from PySide6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QLabel, QPushButton,
    QComboBox, QDoubleSpinBox, QWidget
)
from PySide6.QtCore import Qt, QSize

from humbug.ai.conversation_settings import ConversationSettings


class SettingsDialog(QDialog):
    """Dialog for editing conversation settings."""

    def __init__(self, parent=None, available_models=None):
        """Initialize the settings dialog.

        Args:
            parent: Parent widget
            available_models: List of available AI models
        """
        super().__init__(parent)
        self.setWindowTitle("Conversation Settings")
        self.setFixedSize(QSize(400, 200))
        self.setModal(True)

        # Store initial settings in case of cancel
        self.available_models = available_models or ["gpt-4o-mini"]
        self.initial_settings = None
        self.current_settings = None

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

        # Add stretching space before buttons
        layout.addStretch()

        # Button row
        button_layout = QHBoxLayout()
        button_layout.setSpacing(10)

        # Create buttons
        self.ok_button = QPushButton("OK")
        self.cancel_button = QPushButton("Cancel")
        self.apply_button = QPushButton("Apply")

        # Connect signals
        self.ok_button.clicked.connect(self.accept)
        self.cancel_button.clicked.connect(self.reject)
        self.apply_button.clicked.connect(self.apply_settings)

        # Add buttons to layout
        button_layout.addStretch()
        button_layout.addWidget(self.ok_button)
        button_layout.addWidget(self.cancel_button)
        button_layout.addWidget(self.apply_button)

        layout.addLayout(button_layout)
        self.setLayout(layout)

        # Style the dialog
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
        """)

    def set_settings(self, settings: ConversationSettings):
        """Set the current settings in the dialog.

        Args:
            settings: ConversationSettings object containing current settings
        """
        self.initial_settings = ConversationSettings(
            model=settings.model,
            temperature=settings.temperature
        )
        self.current_settings = ConversationSettings(
            model=settings.model,
            temperature=settings.temperature
        )

        # Update UI elements
        model_index = self.model_combo.findText(settings.model)
        if model_index >= 0:
            self.model_combo.setCurrentIndex(model_index)
        self.temp_spin.setValue(settings.temperature)

        # Disable Apply button initially
        self.apply_button.setEnabled(False)

        # Connect value change signals after setting initial values
        self.model_combo.currentTextChanged.connect(self._handle_value_change)
        self.temp_spin.valueChanged.connect(self._handle_value_change)

    def _handle_value_change(self):
        """Handle changes to settings values."""
        # Enable Apply button if values differ from current settings
        current_model = self.model_combo.currentText()
        current_temp = self.temp_spin.value()

        self.apply_button.setEnabled(
            current_model != self.current_settings.model or
            current_temp != self.current_settings.temperature
        )

    def get_settings(self) -> ConversationSettings:
        """Get the current settings from the dialog.

        Returns:
            ConversationSettings object with current values
        """
        return ConversationSettings(
            model=self.model_combo.currentText(),
            temperature=self.temp_spin.value()
        )

    def apply_settings(self):
        """Apply the current settings."""
        self.current_settings = self.get_settings()
        self.apply_button.setEnabled(False)

    def accept(self):
        """Handle OK button click."""
        self.apply_settings()
        super().accept()

    def reject(self):
        """Handle Cancel button click."""
        # Restore initial settings
        if self.initial_settings:
            self.set_settings(self.initial_settings)
        super().reject()
