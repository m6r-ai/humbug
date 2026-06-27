"""Dialog for configuring initial mindspace folder structure."""


from PySide6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QPushButton, QWidget,
    QScrollArea, QFrame
)

from desktop.language.language_manager import LanguageManager
from desktop.settings.settings_container import SettingsContainer
from desktop.settings.settings_factory import SettingsFactory
from desktop.style_manager import StyleManager


class MindspaceFoldersDialog(QDialog):
    """Dialog for selecting which folders to create in a new mindspace."""

    def __init__(self, mindspace_path: str, parent: QWidget | None = None) -> None:
        """
        Initialize the sidebar folders dialog.

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

        layout = QVBoxLayout()
        layout.setSpacing(12)
        layout.setContentsMargins(20, 20, 20, 20)

        scroll_area = QScrollArea()
        scroll_area.setWidgetResizable(True)
        scroll_area.setFrameShape(QFrame.Shape.NoFrame)

        self._settings_container = SettingsContainer()

        self._path_display = SettingsFactory.create_display(
            strings.mindspace_path,
            mindspace_path
        )
        self._settings_container.add_setting(self._path_display)

        self._conversations_check = SettingsFactory.create_switch(strings.conversations_folder)
        self._conversations_check.set_value(True)
        self._conversations_check.setEnabled(False)
        self._settings_container.add_setting(self._conversations_check)

        self._src_check = SettingsFactory.create_switch(strings.src_folder)
        self._src_check.set_value(False)
        self._settings_container.add_setting(self._src_check)

        self._settings_container.add_stretch()

        scroll_area.setWidget(self._settings_container)
        layout.addWidget(scroll_area)

        layout.addSpacing(24)

        spacing = int(style_manager.message_bubble_spacing())
        button_layout = QHBoxLayout()
        button_layout.setSpacing(spacing)
        button_layout.addStretch()

        self._ok_button = QPushButton(strings.ok)
        self._ok_button.clicked.connect(self.accept)
        self._ok_button.setProperty("recommended", True)

        self._cancel_button = QPushButton(strings.cancel)
        self._cancel_button.clicked.connect(self.reject)

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

        self.setStyleSheet(style_manager.get_dialog_stylesheet())

    def get_selected_folders(self) -> list[str]:
        """
        Get list of folder names to create.

        Returns:
            List of folder names to create in mindspace
        """
        folders = ['conversations']

        if self._src_check.get_value():
            folders.append('src')

        return folders
