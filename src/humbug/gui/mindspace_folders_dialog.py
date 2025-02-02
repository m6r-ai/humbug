"""Dialog for configuring initial mindspace folder structure."""

from typing import List

from PySide6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QLabel, QCheckBox,
    QPushButton, QWidget
)
from PySide6.QtCore import Qt

from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager
from humbug.language.language_manager import LanguageManager


class MindspaceFoldersDialog(QDialog):
    """Dialog for selecting which folders to create in a new mindspace."""

    def __init__(self, mindspace_path: str, parent=None):
        """Initialize the mindspace folders dialog.

        Args:
            mindspace_path: Path where mindspace will be created
            parent: Parent widget
        """
        super().__init__(parent)
        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._handle_language_changed)
        strings = self._language_manager.strings

        self.setWindowTitle(strings.mindspace_folders_title)
        self.setMinimumWidth(500)
        self.setModal(True)

        self._style_manager = StyleManager()

        # Main layout with proper spacing
        layout = QVBoxLayout()
        layout.setSpacing(12)
        layout.setContentsMargins(20, 20, 20, 20)

        # Add mindspace path display that will set our alignment
        path_layout = QHBoxLayout()
        self._path_label = QLabel(strings.mindspace_path)
        self._path_label.setMinimumHeight(40)
        self._path_value = QLabel(mindspace_path)
        self._path_value.setWordWrap(True)
        self._path_value.setMinimumWidth(300)  # Match width from settings dialog
        self._path_value.setMinimumHeight(40)
        self._path_value.setProperty('valueDisplay', True)  # For styling
        path_layout.addWidget(self._path_label)
        path_layout.addStretch()
        path_layout.addWidget(self._path_value)
        layout.addLayout(path_layout)

        # Conversations folder option
        conv_layout = QHBoxLayout()
        conv_label = QLabel(strings.conversations_folder)
        conv_label.setMinimumHeight(40)
        self._conversations_check = QCheckBox()
        self._conversations_check.setChecked(True)
        self._conversations_check.setEnabled(False)
        self._conversations_check.setMinimumWidth(300)
        self._conversations_check.setMinimumHeight(40)
        conv_layout.addWidget(conv_label)
        conv_layout.addStretch()
        conv_layout.addWidget(self._conversations_check)
        layout.addLayout(conv_layout)

        # Metaphor folder option
        metaphor_layout = QHBoxLayout()
        metaphor_label = QLabel(strings.metaphor_folder)
        metaphor_label.setMinimumHeight(40)
        self._metaphor_check = QCheckBox()
        self._metaphor_check.setChecked(True)
        self._metaphor_check.setMinimumHeight(40)
        self._metaphor_check.setMinimumWidth(300)
        metaphor_layout.addWidget(metaphor_label)
        metaphor_layout.addStretch()
        metaphor_layout.addWidget(self._metaphor_check)
        layout.addLayout(metaphor_layout)

        # Source folder option
        src_layout = QHBoxLayout()
        src_label = QLabel(strings.src_folder)
        src_label.setMinimumHeight(40)
        self._src_check = QCheckBox()
        self._src_check.setChecked(False)
        self._src_check.setMinimumWidth(300)
        self._src_check.setMinimumHeight(40)
        src_layout.addWidget(src_label)
        src_layout.addStretch()
        src_layout.addWidget(self._src_check)
        layout.addLayout(src_layout)

        # Add spacing before buttons
        layout.addSpacing(24)
        layout.addStretch()

        # Button row
        button_layout = QHBoxLayout()
        button_layout.setSpacing(8)

        self.ok_button = QPushButton(strings.ok)
        self.cancel_button = QPushButton(strings.cancel)

        self.ok_button.clicked.connect(self.accept)
        self.cancel_button.clicked.connect(self.reject)

        # Set minimum button sizes
        min_button_width = 90
        min_button_height = 40
        for button in [self.ok_button, self.cancel_button]:
            button.setMinimumWidth(min_button_width)
            button.setMinimumHeight(min_button_height)
            button.setContentsMargins(8, 8, 8, 8)

        # Center the buttons
        button_layout.addStretch()
        button_layout.addWidget(self.ok_button)
        button_layout.addWidget(self.cancel_button)
        button_layout.addStretch()

        layout.addLayout(button_layout)
        self.setLayout(layout)

        # Apply consistent dialog styling
        self._handle_style_changed()
        self._style_manager.style_changed.connect(self._handle_style_changed)

    def get_selected_folders(self) -> List[str]:
        """Get list of folder names to create.

        Returns:
            List of folder names to create in mindspace
        """
        folders = ['conversations']  # Always create conversations folder
        if self._metaphor_check.isChecked():
            folders.append('metaphor')
        if self._src_check.isChecked():
            folders.append('src')
        return folders

    def _handle_language_changed(self) -> None:
        """Update all dialog texts when language changes."""
        strings = self._language_manager.strings
        self.setWindowTitle(strings.mindspace_folders_title)
        self._path_label.setText(strings.mindspace_path)

        # Update folder labels
        for checkbox, label in zip(
            [self._conversations_check, self._metaphor_check, self._src_check],
            [strings.conversations_folder, strings.metaphor_folder, strings.src_folder]
        ):
            # Find the label widget in the checkbox's parent layout
            layout = checkbox.parent().layout()
            if layout:
                label_widget = layout.itemAt(0).widget()
                if isinstance(label_widget, QLabel):
                    label_widget.setText(label)

        self.ok_button.setText(strings.ok)
        self.cancel_button.setText(strings.cancel)

    def _handle_style_changed(self):
        """Update styling when application style changes."""
        style_manager = self._style_manager
        zoom_factor = style_manager.zoom_factor
        base_font_size = style_manager.base_font_size

        # Update font sizes
        font = self.font()
        font.setPointSizeF(base_font_size * zoom_factor)
        self.setFont(font)

        self.setStyleSheet(f"""
            QDialog {{
                background-color: {style_manager.get_color_str(ColorRole.BACKGROUND_DIALOG)};
            }}
            QLabel {{
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                background-color: {style_manager.get_color_str(ColorRole.BACKGROUND_DIALOG)};
            }}
            QLabel[valueDisplay="true"] {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_DISABLED)};
                color: {style_manager.get_color_str(ColorRole.TEXT_DISABLED)};
                border: none;
                border-radius: 4px;
                padding: 8px;
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
            QCheckBox::indicator:disabled {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_DISABLED)};
            }}
            QCheckBox::indicator:checked {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND)};
                image: url({style_manager.get_icon_path('check')});
            }}
            QCheckBox::indicator:unchecked {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND)};
            }}
            QCheckBox:disabled {{
                color: {style_manager.get_color_str(ColorRole.TEXT_DISABLED)};
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
