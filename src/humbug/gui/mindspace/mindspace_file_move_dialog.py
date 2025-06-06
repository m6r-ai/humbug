"""Dialog for confirming file/folder move operations in mindspace."""

import os
from typing import Optional

from PySide6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QLabel, QPushButton, QWidget, QFrame
)
from PySide6.QtCore import Qt

from humbug.gui.style_manager import StyleManager
from humbug.language.language_manager import LanguageManager


class MindspaceFileMoveDialog(QDialog):
    """Dialog for confirming file/folder move operations."""

    def __init__(self, source_path: str, dest_path: str, parent: QWidget | None = None):
        """
        Initialize the move confirmation dialog.

        Args:
            source_path: Path of the file/folder being moved
            dest_path: Destination path where the item will be moved
            parent: Parent widget
        """
        super().__init__(parent)
        self._style_manager = StyleManager()
        self._language_manager = LanguageManager()
        strings = self._language_manager.strings()

        self._source_path = source_path
        self._dest_path = dest_path

        # Determine if moving a file or folder
        is_folder = os.path.isdir(source_path)
        item_name = os.path.basename(source_path)

        # Set dialog properties
        if is_folder:
            self.setWindowTitle(strings.move_folder_title)
        else:
            self.setWindowTitle(strings.move_file_title)

        self.setModal(True)
        self.setMinimumWidth(500)

        # Main layout
        layout = QVBoxLayout()
        layout.setSpacing(16)
        layout.setContentsMargins(20, 20, 20, 20)

        # Create confirmation message
        if is_folder:
            message = strings.move_folder_confirmation.format(item_name)
        else:
            message = strings.move_file_confirmation.format(item_name)

        message_label = QLabel(message)
        message_label.setWordWrap(True)
        layout.addWidget(message_label)

        # Create details frame
        details_frame = QFrame()
        details_frame.setFrameStyle(QFrame.Shape.Box)
        details_layout = QVBoxLayout(details_frame)
        details_layout.setContentsMargins(12, 12, 12, 12)
        details_layout.setSpacing(8)

        # Source path
        source_label = QLabel(strings.move_from_label)
        source_label.setProperty("detailLabel", True)
        details_layout.addWidget(source_label)

        source_path_label = QLabel(source_path)
        source_path_label.setWordWrap(True)
        source_path_label.setProperty("pathLabel", True)
        details_layout.addWidget(source_path_label)

        # Destination path
        dest_label = QLabel(strings.move_to_label)
        dest_label.setProperty("detailLabel", True)
        details_layout.addWidget(dest_label)

        dest_path_label = QLabel(dest_path)
        dest_path_label.setWordWrap(True)
        dest_path_label.setProperty("pathLabel", True)
        details_layout.addWidget(dest_path_label)

        layout.addWidget(details_frame)

        # Add spacing before buttons
        layout.addSpacing(16)

        # Button row
        button_layout = QHBoxLayout()
        button_layout.setSpacing(8)
        button_layout.addStretch()

        # Create Move and Cancel buttons
        self._move_button = QPushButton(strings.move_button)
        self._move_button.clicked.connect(self.accept)
        self._move_button.setProperty("recommended", True)

        cancel_button = QPushButton(strings.cancel)
        cancel_button.clicked.connect(self.reject)

        # Set minimum button sizes
        zoom_factor = self._style_manager.zoom_factor()
        min_button_width = int(90 * zoom_factor)
        min_button_height = 40
        for button in [self._move_button, cancel_button]:
            button.setMinimumWidth(min_button_width)
            button.setMinimumHeight(min_button_height)
            button.setContentsMargins(8, 8, 8, 8)
            button_layout.addWidget(button)

        button_layout.addStretch()
        layout.addLayout(button_layout)
        self.setLayout(layout)

        self._apply_styling()

    def _apply_styling(self) -> None:
        """Apply consistent styling to the dialog."""
        base_stylesheet = self._style_manager.get_dialog_stylesheet()

        # Add custom styles for the move dialog
        custom_styles = """
            QLabel[detailLabel="true"] {
                font-weight: bold;
                margin-top: 8px;
            }
            QLabel[pathLabel="true"] {
                background-color: rgba(128, 128, 128, 0.1);
                padding: 4px 8px;
                border-radius: 4px;
                font-family: monospace;
            }
            QFrame {
                background-color: rgba(128, 128, 128, 0.05);
            }
        """

        self.setStyleSheet(base_stylesheet + custom_styles)

    def get_source_path(self) -> str:
        """Get the source path being moved."""
        return self._source_path

    def get_destination_path(self) -> str:
        """Get the destination path."""
        return self._dest_path
