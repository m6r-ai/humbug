"""
Dialog for configuring workspace-specific settings.

This dialog allows users to configure workspace settings such as tab behavior and size.
Settings are persisted to the workspace's settings.json file.
"""

import os
from typing import Optional

from PySide6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QLabel,
    QPushButton, QSpinBox, QCheckBox
)
from PySide6.QtCore import Signal

from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager, ColorMode
from humbug.workspace.workspace_settings import WorkspaceSettings


class WorkspaceSettingsDialog(QDialog):
    """Dialog for editing workspace settings."""

    settings_changed = Signal(WorkspaceSettings)

    def __init__(self, parent=None):
        """Initialize the workspace settings dialog.

        Args:
            parent: Parent widget, typically the main window.
        """
        super().__init__(parent)
        self.setWindowTitle("Workspace Settings")
        self.setMinimumWidth(500)
        self.setModal(True)

        self._initial_settings: Optional[WorkspaceSettings] = None
        self._current_settings: Optional[WorkspaceSettings] = None

        style_manager = StyleManager()

        # Main layout with proper spacing
        layout = QVBoxLayout()
        layout.setSpacing(12)
        layout.setContentsMargins(20, 20, 20, 20)

        # Soft tabs setting
        soft_tabs_layout = QHBoxLayout()
        self._soft_tabs_check = QCheckBox("Use Soft Tabs")
        self._soft_tabs_check.setMinimumHeight(40)
        self._soft_tabs_check.stateChanged.connect(self._handle_value_change)
        soft_tabs_layout.addWidget(self._soft_tabs_check)
        soft_tabs_layout.addStretch()
        layout.addLayout(soft_tabs_layout)

        # Tab size setting
        tab_size_layout = QHBoxLayout()
        tab_size_label = QLabel("Tab Size:")
        tab_size_label.setMinimumHeight(40)
        self._tab_size_spin = QSpinBox()
        self._tab_size_spin.setRange(1, 8)
        self._tab_size_spin.setMinimumWidth(300)
        self._tab_size_spin.setMinimumHeight(40)
        self._tab_size_spin.valueChanged.connect(self._handle_value_change)
        tab_size_layout.addWidget(tab_size_label)
        tab_size_layout.addStretch()
        tab_size_layout.addWidget(self._tab_size_spin)
        layout.addLayout(tab_size_layout)

        # Add spacing before buttons
        layout.addSpacing(24)
        layout.addStretch()

        # Button row
        button_layout = QHBoxLayout()
        button_layout.setSpacing(8)

        self.ok_button = QPushButton("OK")
        self.cancel_button = QPushButton("Cancel")
        self.apply_button = QPushButton("Apply")

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
        icon_dir = os.path.expanduser("~/.humbug/icons")
        theme = "dark" if style_manager.color_mode == ColorMode.DARK else "light"

        self.setStyleSheet(f"""
            QDialog {{
                background-color: {style_manager.get_color_str(ColorRole.BACKGROUND_DIALOG)};
            }}
            QLabel {{
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                background-color: {style_manager.get_color_str(ColorRole.BACKGROUND_DIALOG)};
            }}
            QCheckBox {{
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                background-color: {style_manager.get_color_str(ColorRole.BACKGROUND_DIALOG)};
                spacing: 8px;
            }}
            QCheckBox::indicator {{
                width: 18px;
                height: 18px;
                border: none;
                border-radius: 4px;
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND)};
            }}
            QCheckBox::indicator:checked {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND)};
                image: url("{icon_dir}/check-{theme}.svg");
            }}
            QCheckBox::indicator:unchecked {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND)};
            }}
            QSpinBox {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND)};
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: none;
                border-radius: 4px;
                padding: 8px;
            }}
            QSpinBox::up-button, QSpinBox::down-button {{
                border: none;
                width: 20px;
            }}
            QSpinBox::up-arrow {{
                image: url("{icon_dir}/arrow-up-{theme}.svg");
                width: 12px;
                height: 12px;
            }}
            QSpinBox::up-arrow:disabled, QSpinBox::up-arrow:off {{
                image: none;
            }}
            QSpinBox::down-arrow {{
                image: url("{icon_dir}/arrow-down-{theme}.svg");
                width: 12px;
                height: 12px;
            }}
            QSpinBox::down-arrow:disabled, QSpinBox::down-arrow:off {{
                image: none;
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

    def _handle_value_change(self) -> None:
        """Handle changes to any setting value."""
        if not self._current_settings:
            return

        self.apply_button.setEnabled(
            self._soft_tabs_check.isChecked() != self._current_settings.use_soft_tabs or
            self._tab_size_spin.value() != self._current_settings.tab_size
        )

    def get_settings(self) -> WorkspaceSettings:
        """Get the current settings from the dialog."""
        return WorkspaceSettings(
            use_soft_tabs=self._soft_tabs_check.isChecked(),
            tab_size=self._tab_size_spin.value()
        )

    def set_settings(self, settings: WorkspaceSettings) -> None:
        """Set the current settings in the dialog.

        Args:
            settings: The workspace settings to display
        """
        self._initial_settings = WorkspaceSettings(
            use_soft_tabs=settings.use_soft_tabs,
            tab_size=settings.tab_size
        )
        self._current_settings = WorkspaceSettings(
            use_soft_tabs=settings.use_soft_tabs,
            tab_size=settings.tab_size
        )

        self._soft_tabs_check.setChecked(settings.use_soft_tabs)
        self._tab_size_spin.setValue(settings.tab_size)
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
