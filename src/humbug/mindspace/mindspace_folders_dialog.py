"""Dialog for configuring initial mindspace folder structure."""

from typing import List

from PySide6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QPushButton, QWidget,
    QScrollArea, QFrame
)

from humbug.language.language_manager import LanguageManager
from humbug.settings.settings_components import (
    SettingsContainer, SettingsFactory
)
from humbug.style_manager import StyleManager


class MindspaceFoldersDialog(QDialog):
    """Dialog for selecting which folders to create in a new mindspace."""

    def __init__(self, mindspace_path: str, parent: QWidget | None = None) -> None:
        """
        Initialize the mindspace folders dialog.

        Args:
            mindspace_path: Path where mindspace will be created
            parent: Parent widget
        """
        super().__init__(parent)
        self._language_manager = LanguageManager()
        strings = self._language_manager.strings()

        self.setWindowTitle(strings.mindspace_folders_title)
        self.setMinimumWidth(500)
        self.setModal(True)

        style_manager = StyleManager()

        # Main layout with proper spacing
        layout = QVBoxLayout()
        layout.setSpacing(12)
        layout.setContentsMargins(20, 20, 20, 20)

        # Create a scroll area for the settings
        scroll_area = QScrollArea()
        scroll_area.setWidgetResizable(True)
        scroll_area.setFrameShape(QFrame.Shape.NoFrame)

        # Create settings container for folder options
        self._settings_container = SettingsContainer()

        # Path display
        self._path_display = SettingsFactory.create_display(
            strings.mindspace_path,
            mindspace_path
        )
        self._settings_container.add_setting(self._path_display)

        # Conversations folder option (required)
        self._conversations_check = SettingsFactory.create_checkbox(strings.conversations_folder)
        self._conversations_check.set_value(True)
        self._conversations_check.setEnabled(False)  # Always required
        self._settings_container.add_setting(self._conversations_check)

        # Metaphor folder option
        self._metaphor_check = SettingsFactory.create_checkbox(strings.metaphor_folder)
        self._metaphor_check.set_value(True)
        self._settings_container.add_setting(self._metaphor_check)

        # Source folder option
        self._src_check = SettingsFactory.create_checkbox(strings.src_folder)
        self._src_check.set_value(False)
        self._settings_container.add_setting(self._src_check)

        # Add stretch to push everything up
        self._settings_container.add_stretch()

        # Set the scroll content
        scroll_area.setWidget(self._settings_container)
        layout.addWidget(scroll_area)

        # Add spacing before buttons
        layout.addSpacing(24)

        # Button row
        button_layout = QHBoxLayout()
        button_layout.setSpacing(8)
        button_layout.addStretch()

        self._ok_button = QPushButton(strings.ok)
        self._ok_button.clicked.connect(self.accept)
        self._ok_button.setProperty("recommended", True)

        self._cancel_button = QPushButton(strings.cancel)
        self._cancel_button.clicked.connect(self.reject)

        # Set minimum button sizes
        zoom_factor = style_manager.zoom_factor()
        min_button_width = int(90 * zoom_factor)
        min_button_height = 40
        for button in [self._ok_button, self._cancel_button]:
            button.setMinimumWidth(min_button_width)
            button.setMinimumHeight(min_button_height)
            button.setContentsMargins(8, 8, 8, 8)
            button_layout.addWidget(button)

        button_layout.addStretch()
        layout.addLayout(button_layout)
        self.setLayout(layout)

        # Apply consistent dialog styling
        self.setStyleSheet(style_manager.get_dialog_stylesheet())

    def get_selected_folders(self) -> List[str]:
        """
        Get list of folder names to create.

        Returns:
            List of folder names to create in mindspace
        """
        folders = ['conversations']  # Always create conversations folder

        if self._metaphor_check.get_value():
            folders.append('metaphor')

        if self._src_check.get_value():
            folders.append('src')

        return folders
