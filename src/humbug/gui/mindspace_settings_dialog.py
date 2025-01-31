"""
Dialog for configuring mindspace-specific settings.

This dialog allows users to configure mindspace settings such as tab behavior and size.
Settings are persisted to the mindspace's settings.json file.
"""

from typing import Optional

from PySide6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QLabel, QComboBox,
    QPushButton, QSpinBox, QCheckBox, QDoubleSpinBox
)
from PySide6.QtCore import Signal

from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager
from humbug.language.language_code import LanguageCode
from humbug.language.language_manager import LanguageManager
from humbug.mindspace.mindspace_settings import MindspaceSettings


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
        self._language_manager.language_changed.connect(self._handle_language_changed)
        strings = self._language_manager.strings

        self.setWindowTitle(strings.settings_dialog_title)
        self.setMinimumWidth(500)
        self.setModal(True)

        self._initial_settings: Optional[MindspaceSettings] = None
        self._current_settings: Optional[MindspaceSettings] = None

        self._style_manager = StyleManager()

        # Main layout with proper spacing
        layout = QVBoxLayout()
        layout.setSpacing(12)
        layout.setContentsMargins(20, 20, 20, 20)

        # Add language selector
        language_layout, self._language_combo = self._create_language_selector(self)
        layout.addLayout(language_layout)

        # Connect language change handler
        self._language_combo.currentIndexChanged.connect(self._handle_value_change)

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

        font_size_layout = QHBoxLayout()
        self._font_size_label = QLabel(strings.font_size)
        self._font_size_label.setMinimumHeight(40)
        self._font_size_spin = QDoubleSpinBox()
        self._font_size_spin.setRange(8.0, 24.0)
        self._font_size_spin.setSingleStep(0.5)
        self._font_size_spin.setDecimals(1)
        self._font_size_spin.setMinimumWidth(300)
        self._font_size_spin.setMinimumHeight(40)
        self._font_size_spin.setContentsMargins(8, 8, 8, 8)
        self._font_size_spin.valueChanged.connect(self._handle_value_change)
        font_size_layout.addWidget(self._font_size_label)
        font_size_layout.addStretch()
        font_size_layout.addWidget(self._font_size_spin)
        layout.addLayout(font_size_layout)

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

        # Apply consistent dialog styling
        self.setStyleSheet(f"""
            QDialog {{
                background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_DIALOG)};
            }}
            QLabel {{
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_DIALOG)};
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
            QComboBox QAbstractItemView {{
                background: {self._style_manager.get_color_str(ColorRole.BACKGROUND_DIALOG)};
                selection-background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_HOVER)};
                selection-color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
            }}
            QSpinBox {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND)};
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: none;
                border-radius: 4px;
                padding: 8px;
            }}
            QDoubleSpinBox {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND)};
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: none;
                border-radius: 4px;
                padding: 8px;
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

    def _create_language_selector(self, parent) -> tuple[QHBoxLayout, QComboBox]:
        """Create language selection UI elements.

        Args:
            parent: Parent widget for the selector

        Returns:
            Tuple of (layout containing selector, combo box for language selection)
        """
        language_manager = LanguageManager()

        layout = QHBoxLayout()
        self._language_label = QLabel(language_manager.strings.select_language)
        self._language_label.setMinimumHeight(40)
        combo = QComboBox(parent)
        combo.setMinimumWidth(300)
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
        current_index = combo.findData(language_manager.current_language)
        combo.setCurrentIndex(current_index)

        layout.addWidget(self._language_label)
        layout.addStretch()
        layout.addWidget(combo)

        return layout, combo

    def _handle_language_changed(self) -> None:
        """Update all dialog texts with current language strings."""
        strings = self._language_manager.strings
        self.setWindowTitle(strings.settings_dialog_title)

        # Update labels
        self._language_label.setText(strings.select_language)
        self._soft_tabs_label.setText(strings.use_soft_tabs)
        self._tab_size_label.setText(strings.tab_size)
        self._font_size_label.setText(strings.font_size)
        self._auto_backup_label.setText(strings.auto_backup)
        self._backup_interval_label.setText(strings.backup_interval)

        # Update buttons
        self.ok_button.setText(strings.ok)
        self.cancel_button.setText(strings.cancel)
        self.apply_button.setText(strings.apply)

        # Adjust dialog size to fit new content
        self.adjustSize()
        size_hint = self.sizeHint()
        new_width = max(500, size_hint.width())
        self.resize(new_width, size_hint.height())

    def _handle_value_change(self) -> None:
        """Handle changes to any setting value."""
        if not self._current_settings:
            return

        self.apply_button.setEnabled(
            self._language_combo.currentData() != self._current_settings.language or
            self._soft_tabs_check.isChecked() != self._current_settings.use_soft_tabs or
            self._tab_size_spin.value() != self._current_settings.tab_size or
            self._auto_backup_check.isChecked() != self._current_settings.auto_backup or
            self._backup_interval_spin.value() != self._current_settings.auto_backup_interval or
            self._font_size_spin.value() != (self._current_settings.font_size or self._style_manager.base_font_size)
        )

    def get_settings(self) -> MindspaceSettings:
        """Get the current settings from the dialog."""
        return MindspaceSettings(
            language=self._language_combo.currentData(),
            use_soft_tabs=self._soft_tabs_check.isChecked(),
            tab_size=self._tab_size_spin.value(),
            font_size=self._font_size_spin.value(),
            auto_backup=self._auto_backup_check.isChecked(),
            auto_backup_interval=self._backup_interval_spin.value()
        )

    def set_settings(self, settings: MindspaceSettings) -> None:
        """Set the current settings in the dialog."""
        # Store initial language for potential cancel/revert
        self._initial_settings = MindspaceSettings(
            language=settings.language,
            use_soft_tabs=settings.use_soft_tabs,
            tab_size=settings.tab_size,
            font_size=settings.font_size,
            auto_backup=settings.auto_backup,
            auto_backup_interval=settings.auto_backup_interval
        )
        self._current_settings = MindspaceSettings(
            language=settings.language,
            use_soft_tabs=settings.use_soft_tabs,
            tab_size=settings.tab_size,
            font_size=settings.font_size,
            auto_backup=settings.auto_backup,
            auto_backup_interval=settings.auto_backup_interval
        )

        self._soft_tabs_check.setChecked(settings.use_soft_tabs)
        self._tab_size_spin.setValue(settings.tab_size)
        self._font_size_spin.setValue(settings.font_size if settings.font_size is not None else self._style_manager.base_font_size)
        self._auto_backup_check.setChecked(settings.auto_backup)
        self._backup_interval_spin.setValue(settings.auto_backup_interval)

        # Set initial language selection
        current_index = self._language_combo.findData(self._language_manager.current_language)
        self._language_combo.setCurrentIndex(current_index)

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
