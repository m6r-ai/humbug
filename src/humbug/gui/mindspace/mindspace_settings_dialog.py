"""
Dialog for configuring mindspace-specific settings.

This dialog allows users to configure mindspace settings such as tab behavior and size.
Settings are persisted to the mindspace's settings.json file.
"""

from typing import Optional

from PySide6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QLabel, QComboBox,
    QPushButton, QSpinBox, QCheckBox, QDoubleSpinBox, QListView
)
from PySide6.QtCore import Signal

from humbug.ai.ai_conversation_settings import AIConversationSettings, ReasoningCapability
from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager
from humbug.language.language_manager import LanguageManager
from humbug.mindspace.mindspace_settings import MindspaceSettings
from humbug.user.user_manager import UserManager


class MindspaceSettingsDialog(QDialog):
    """Dialog for editing mindspace settings."""

    settings_changed = Signal(MindspaceSettings)

    def __init__(self, parent=None):
        """Initialize the mindspace settings dialog.

        Args:
            parent: Parent widget, typically the main window.
        """
        super().__init__(parent)
        self._language_manager = LanguageManager()
        strings = self._language_manager.strings

        self.setWindowTitle(strings.mindspace_settings)
        self.setMinimumWidth(500)
        self.setModal(True)

        self._initial_settings: Optional[MindspaceSettings] = None
        self._current_settings: Optional[MindspaceSettings] = None

        self._style_manager = StyleManager()

        # Main layout with proper spacing
        layout = QVBoxLayout()
        layout.setSpacing(12)
        layout.setContentsMargins(20, 20, 20, 20)

        # Add model selection
        model_layout = QHBoxLayout()
        self._model_label = QLabel(strings.settings_model_label)
        self._model_label.setMinimumHeight(40)
        self._model_combo = QComboBox()
        self._model_combo.setView(QListView())
        self._model_combo.setMinimumWidth(300)
        self._model_combo.setMinimumHeight(40)

        self._user_manager = UserManager()
        ai_backends = self._user_manager.get_ai_backends()

        models = []
        for model in AIConversationSettings.iter_models_by_backends(ai_backends):
            models.append(model)

        self._model_combo.addItems(models)
        self._model_combo.currentTextChanged.connect(self._handle_value_change)
        model_layout.addWidget(self._model_label)
        model_layout.addStretch()
        model_layout.addWidget(self._model_combo)
        layout.addLayout(model_layout)

        # Add temperature setting
        temp_layout = QHBoxLayout()
        self._temp_label = QLabel(strings.settings_temp_label)
        self._temp_label.setMinimumHeight(40)
        self._temp_spin = QDoubleSpinBox()
        self._temp_spin.setRange(0.0, 1.0)
        self._temp_spin.setSingleStep(0.1)
        self._temp_spin.setDecimals(1)
        self._temp_spin.setMinimumWidth(300)
        self._temp_spin.setMinimumHeight(40)
        self._temp_spin.valueChanged.connect(self._handle_value_change)
        temp_layout.addWidget(self._temp_label)
        temp_layout.addStretch()
        temp_layout.addWidget(self._temp_spin)
        layout.addLayout(temp_layout)

        # Add reasoning capabilities
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

        # Soft tabs setting
        soft_tabs_layout = QHBoxLayout()
        self._soft_tabs_label = QLabel(strings.use_soft_tabs)
        self._soft_tabs_label.setMinimumHeight(40)
        self._soft_tabs_check = QCheckBox()
        self._soft_tabs_check.setMinimumHeight(40)
        self._soft_tabs_check.setMinimumWidth(300)
        self._soft_tabs_check.stateChanged.connect(self._handle_value_change)
        soft_tabs_layout.addWidget(self._soft_tabs_label)
        soft_tabs_layout.addStretch()
        soft_tabs_layout.addWidget(self._soft_tabs_check)
        layout.addLayout(soft_tabs_layout)

        # Tab size setting
        tab_size_layout = QHBoxLayout()
        self._tab_size_label = QLabel(strings.tab_size)
        self._tab_size_label.setMinimumHeight(40)
        self._tab_size_spin = QSpinBox()
        self._tab_size_spin.setRange(1, 8)
        self._tab_size_spin.setMinimumWidth(300)
        self._tab_size_spin.setMinimumHeight(40)
        self._tab_size_spin.valueChanged.connect(self._handle_value_change)
        tab_size_layout.addWidget(self._tab_size_label)
        tab_size_layout.addStretch()
        tab_size_layout.addWidget(self._tab_size_spin)
        layout.addLayout(tab_size_layout)

        # Add auto-backup settings
        auto_backup_layout = QHBoxLayout()
        self._auto_backup_label = QLabel(strings.auto_backup)
        self._auto_backup_label.setMinimumHeight(40)
        self._auto_backup_check = QCheckBox()
        self._auto_backup_check.setMinimumHeight(40)
        self._auto_backup_check.setMinimumWidth(300)
        self._auto_backup_check.stateChanged.connect(self._handle_value_change)
        auto_backup_layout.addWidget(self._auto_backup_label)
        auto_backup_layout.addStretch()
        auto_backup_layout.addWidget(self._auto_backup_check)
        layout.addLayout(auto_backup_layout)

        # Add auto-backup interval setting
        backup_interval_layout = QHBoxLayout()
        self._backup_interval_label = QLabel(strings.backup_interval)
        self._backup_interval_label.setMinimumHeight(40)
        self._backup_interval_spin = QSpinBox()
        self._backup_interval_spin.setRange(60, 3600)  # 1 minute to 1 hour
        self._backup_interval_spin.setMinimumWidth(300)
        self._backup_interval_spin.setMinimumHeight(40)
        self._backup_interval_spin.valueChanged.connect(self._handle_value_change)
        backup_interval_layout.addWidget(self._backup_interval_label)
        backup_interval_layout.addStretch()
        backup_interval_layout.addWidget(self._backup_interval_spin)
        layout.addLayout(backup_interval_layout)

        # Add spacing before buttons
        layout.addSpacing(24)
        layout.addStretch()

        # Button row
        button_layout = QHBoxLayout()
        button_layout.setSpacing(8)

        self.ok_button = QPushButton(strings.ok)
        self.cancel_button = QPushButton(strings.cancel)
        self.apply_button = QPushButton(strings.apply)

        self.ok_button.clicked.connect(self._handle_ok)
        self.cancel_button.clicked.connect(self.reject)
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

        zoom_factor = self._style_manager.zoom_factor
        base_font_size = self._style_manager.base_font_size

        # Apply consistent dialog styling
        self.setStyleSheet(f"""
            QDialog {{
                background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_DIALOG)};
                font-size: {base_font_size * zoom_factor}pt;
            }}
            QLabel {{
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_DIALOG)};
                font-size: {base_font_size * zoom_factor}pt;
            }}
            QCheckBox {{
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_DIALOG)};
                spacing: 8px;
            }}
            QCheckBox::indicator {{
                width: 18px;
                height: 18px;
                border: none;
                border-radius: 4px;
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND)};
            }}
            QCheckBox::indicator:checked {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND)};
                image: url({self._style_manager.get_icon_path('check')});
            }}
            QCheckBox::indicator:unchecked {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND)};
            }}
            QComboBox {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND)};
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: none;
                border-radius: 4px;
                padding: 8px;
                font-size: {base_font_size * zoom_factor}pt;
            }}
            QComboBox:disabled {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_DISABLED)};
                color: {self._style_manager.get_color_str(ColorRole.TEXT_DISABLED)};
            }}
            QComboBox::drop-down {{
                border: none;
                width: 20px;
            }}
            QComboBox::down-arrow {{
                image: url({self._style_manager.get_icon_path("arrow-down")});
                width: 12px;
                height: 12px;
            }}
            QComboBox::down-arrow:on {{
                image: url({self._style_manager.get_icon_path('arrow-up')});
                width: 12px;
                height: 12px;
            }}
            QComboBox::down-arrow:disabled {{
                image: none;
            }}
            QComboBox QAbstractItemView::item:selected {{
                border: none;
                background-color: {self._style_manager.get_color_str(ColorRole.TEXT_SELECTED)};
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
            }}
            QComboBox QListView {{
                border: none;
                background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_SECONDARY)};
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
            }}
            QDoubleSpinBox {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND)};
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: none;
                border-radius: 4px;
                padding: 8px;
                font-size: {base_font_size * zoom_factor}pt;
            }}
            QDoubleSpinBox:disabled {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_DISABLED)};
                color: {self._style_manager.get_color_str(ColorRole.TEXT_DISABLED)};
            }}
            QDoubleSpinBox::up-button, QDoubleSpinBox::down-button {{
                border: none;
                width: 20px;
            }}
            QDoubleSpinBox::up-arrow {{
                image: url({self._style_manager.get_icon_path('arrow-up')});
                width: 12px;
                height: 12px;
            }}
            QDoubleSpinBox::up-arrow:disabled, QDoubleSpinBox::up-arrow:off {{
                image: none;
            }}
            QDoubleSpinBox::down-arrow {{
                image: url({self._style_manager.get_icon_path('arrow-down')});
                width: 12px;
                height: 12px;
            }}
            QDoubleSpinBox::down-arrow:disabled, QDoubleSpinBox::down-arrow:off {{
                image: none;
            }}
            QSpinBox {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND)};
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: none;
                border-radius: 4px;
                padding: 8px;
                font-size: {base_font_size * zoom_factor}pt;
            }}
            QSpinBox:disabled {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_DISABLED)};
                color: {self._style_manager.get_color_str(ColorRole.TEXT_DISABLED)};
            }}
            QSpinBox::up-button, QSpinBox::down-button {{
                border: none;
                width: 20px;
            }}
            QSpinBox::up-arrow {{
                image: url({self._style_manager.get_icon_path('arrow-up')});
                width: 12px;
                height: 12px;
            }}
            QSpinBox::up-arrow:disabled, QSpinBox::up-arrow:off {{
                image: none;
            }}
            QSpinBox::down-arrow {{
                image: url({self._style_manager.get_icon_path('arrow-down')});
                width: 12px;
                height: 12px;
            }}
            QSpinBox::down-arrow:disabled, QSpinBox::down-arrow:off {{
                image: none;
            }}
            QPushButton {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND)};
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: none;
                border-radius: 4px;
                padding: 8px;
                font-size: {base_font_size * zoom_factor}pt;
            }}
            QPushButton:hover {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_HOVER)};
            }}
            QPushButton:pressed {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_PRESSED)};
            }}
            QPushButton:disabled {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_DISABLED)};
                color: {self._style_manager.get_color_str(ColorRole.TEXT_DISABLED)};
            }}
        """)

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
        capabilities = AIConversationSettings.get_reasoning_capability(model)

        # Add NO_REASONING if supported
        if capabilities & ReasoningCapability.NO_REASONING:
            self._reasoning_combo.addItem(self._language_manager.strings.settings_no_reasoning, ReasoningCapability.NO_REASONING)

        # Add HIDDEN_REASONING if supported
        if capabilities & ReasoningCapability.HIDDEN_REASONING:
            self._reasoning_combo.addItem(self._language_manager.strings.settings_hidden_reasoning, ReasoningCapability.HIDDEN_REASONING)

        # Add VISIBLE_REASONING if supported
        if capabilities & ReasoningCapability.VISIBLE_REASONING:
            self._reasoning_combo.addItem(self._language_manager.strings.settings_visible_reasoning, ReasoningCapability.VISIBLE_REASONING)

        # Set previous selection if possible
        if current_reasoning is not None:
            index_found = False
            for i in range(self._reasoning_combo.count()):
                if self._reasoning_combo.itemData(i) == current_reasoning:
                    self._reasoning_combo.setCurrentIndex(i)
                    index_found = True
                    break

            # If the previous reasoning isn't available for this model, default to NO_REASONING
            if not index_found:
                self._reasoning_combo.setCurrentIndex(0)  # Select "No Reasoning"

        # Disable combo box if only one option
        self._reasoning_combo.setEnabled(self._reasoning_combo.count() > 1)

        # Unblock signals
        self._reasoning_combo.blockSignals(False)

    def _handle_value_change(self) -> None:
        """Handle changes to any setting value."""
        if not self._current_settings:
            return

        auto_backup_checked = self._auto_backup_check.isChecked()
        self._backup_interval_spin.setEnabled(auto_backup_checked)

        # Get current temperature value based on model support
        current_model = self._model_combo.currentText()
        current_temp = self._temp_spin.value()
        current_reasoning = self._reasoning_combo.currentData()

        # Update reasoning capabilities when model changes
        supports_temp = AIConversationSettings.supports_temperature(current_model)
        self._temp_spin.setEnabled(supports_temp)

        # Update reasoning combo as needed
        self._update_reasoning_combo(current_model)

        # Compare temperatures accounting for None values
        temp_changed = False
        if current_temp is None and self._current_settings.temperature is None:
            temp_changed = False
        elif current_temp is None or self._current_settings.temperature is None:
            temp_changed = True
        else:
            temp_changed = abs(current_temp - self._current_settings.temperature) > 0.01

        self.apply_button.setEnabled(
            self._soft_tabs_check.isChecked() != self._current_settings.use_soft_tabs or
            self._tab_size_spin.value() != self._current_settings.tab_size or
            auto_backup_checked != self._current_settings.auto_backup or
            self._backup_interval_spin.value() != self._current_settings.auto_backup_interval or
            current_model != self._current_settings.model or
            temp_changed or
            current_reasoning != self._current_settings.reasoning
        )

    def get_settings(self) -> MindspaceSettings:
        """Get current settings from dialog."""
        return MindspaceSettings(
            use_soft_tabs=self._soft_tabs_check.isChecked(),
            tab_size=self._tab_size_spin.value(),
            auto_backup=self._auto_backup_check.isChecked(),
            auto_backup_interval=self._backup_interval_spin.value(),
            model=self._model_combo.currentText(),
            temperature=self._temp_spin.value(),
            reasoning=self._reasoning_combo.currentData()
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
            reasoning=settings.reasoning
        )

        # Editor settings
        self._soft_tabs_check.setChecked(settings.use_soft_tabs)
        self._tab_size_spin.setValue(settings.tab_size)
        self._auto_backup_check.setChecked(settings.auto_backup)
        self._backup_interval_spin.setValue(settings.auto_backup_interval)
        self._backup_interval_spin.setEnabled(settings.auto_backup)

        # Model selection
        model_index = self._model_combo.findText(settings.model)
        if model_index >= 0:
            self._model_combo.setCurrentIndex(model_index)

        # Temperature setting
        supports_temp = AIConversationSettings.supports_temperature(settings.model)
        self._temp_spin.setEnabled(supports_temp)
        self._temp_spin.setValue(settings.temperature)

        # Update reasoning options based on the selected model
        self._update_reasoning_combo(settings.model)

        # Select the current reasoning setting if possible
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
