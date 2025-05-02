"""Dialog for configuring initial mindspace folder structure."""

from typing import List

from PySide6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QLabel, QCheckBox,
    QPushButton, QWidget, QLineEdit
)

from humbug.gui.style_manager import StyleManager
from humbug.language.language_manager import LanguageManager


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
        zoom_factor = style_manager.zoom_factor()
        element_width = int(zoom_factor * 300)

        # Main layout with proper spacing
        layout = QVBoxLayout()
        layout.setSpacing(12)
        layout.setContentsMargins(20, 20, 20, 20)

        # Path label and value container
        path_layout = QHBoxLayout()
        self._path_label = QLabel(strings.mindspace_path)
        self._path_label.setMinimumHeight(40)
        path_edit = QLineEdit()
        path_edit.setMinimumWidth(element_width)
        path_edit.setMinimumHeight(40)
        path_edit.setEnabled(False)
        path_edit.setText(mindspace_path)
        path_layout.addWidget(self._path_label)
        path_layout.addStretch()
        path_layout.addWidget(path_edit)
        layout.addLayout(path_layout)

        # Rest of the dialog content remains the same
        # Conversations folder option
        conv_layout = QHBoxLayout()
        conv_label = QLabel(strings.conversations_folder)
        conv_label.setMinimumHeight(40)
        self._conversations_check = QCheckBox()
        self._conversations_check.setChecked(True)
        self._conversations_check.setEnabled(False)
        self._conversations_check.setMinimumWidth(element_width)
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
        self._metaphor_check.setMinimumWidth(element_width)
        self._metaphor_check.setMinimumHeight(40)
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
        self._src_check.setMinimumWidth(element_width)
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
        if self._metaphor_check.isChecked():
            folders.append('metaphor')

        if self._src_check.isChecked():
            folders.append('src')

        return folders
