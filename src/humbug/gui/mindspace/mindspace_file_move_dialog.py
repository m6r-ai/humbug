"""Dialog for confirming file/folder move operations in mindspace."""

import os

from PySide6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QGridLayout, QLabel, QPushButton, QWidget
)
from PySide6.QtGui import QPixmap
from PySide6.QtCore import Qt

from humbug.gui.style_manager import StyleManager
from humbug.language.language_manager import LanguageManager
from humbug.mindspace.mindspace_manager import MindspaceManager


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
        self._mindspace_manager = MindspaceManager()
        strings = self._language_manager.strings()

        self._source_path = source_path
        self._dest_path = dest_path

        # Convert absolute paths to relative paths for display
        self._display_source_path = self._get_display_path(source_path)
        self._display_dest_path = self._get_display_path(dest_path)

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

        # Create header layout for icon and content
        header_layout = QHBoxLayout()
        header_layout.setSpacing(16)
        header_layout.setContentsMargins(0, 0, 0, 0)

        # Add icon (using question icon for move confirmation)
        icon = self._create_icon()
        if icon:
            icon_label = QLabel()
            icon_label.setPixmap(icon)
            header_layout.addWidget(icon_label, alignment=Qt.AlignmentFlag.AlignTop)

        # Create vertical layout for message and path details
        content_layout = QVBoxLayout()
        content_layout.setSpacing(12)
        content_layout.setContentsMargins(0, 0, 0, 0)

        # Create confirmation message
        if is_folder:
            message = strings.move_folder_confirmation.format(item_name)

        else:
            message = strings.move_file_confirmation.format(item_name)

        message_label = QLabel(message)
        message_label.setWordWrap(True)
        content_layout.addWidget(message_label)

        # Create grid layout for path information
        grid_layout = QGridLayout()
        grid_layout.setSpacing(8)
        grid_layout.setContentsMargins(0, 0, 0, 0)
        grid_layout.setColumnStretch(1, 1)  # Make the path column expandable

        # Source path row
        source_label = QLabel(strings.move_from_label)
        source_label.setProperty("detailLabel", True)
        source_label.setAlignment(Qt.AlignmentFlag.AlignTop)
        grid_layout.addWidget(source_label, 0, 0)

        source_path_label = QLabel(self._display_source_path)
        source_path_label.setWordWrap(True)
        grid_layout.addWidget(source_path_label, 0, 1)

        # Destination path row
        dest_label = QLabel(strings.move_to_label)
        dest_label.setProperty("detailLabel", True)
        dest_label.setAlignment(Qt.AlignmentFlag.AlignTop)
        grid_layout.addWidget(dest_label, 1, 0)

        dest_path_label = QLabel(self._display_dest_path)
        dest_path_label.setWordWrap(True)
        grid_layout.addWidget(dest_path_label, 1, 1)

        # Add grid layout to content layout
        content_layout.addLayout(grid_layout)

        # Add content layout to header layout
        header_layout.addLayout(content_layout, stretch=1)

        # Add header layout to main layout
        layout.addLayout(header_layout)

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

    def _create_icon(self) -> QPixmap | None:
        """
        Create appropriate icon for the move dialog.

        Returns:
            QPixmap icon or None if icon cannot be created
        """
        try:
            # Use question icon for move confirmation
            icon_path = self._style_manager.get_icon_path("question")
            return self._style_manager.scale_icon(icon_path, 64)

        except Exception:
            # If icon creation fails, return None (no icon will be shown)
            return None

    def _get_display_path(self, path: str) -> str:
        """
        Get the display path for the dialog (relative to mindspace if possible).

        Args:
            path: Absolute path to convert

        Returns:
            Relative path if within mindspace, otherwise absolute path
        """
        try:
            if self._mindspace_manager.has_mindspace():
                return self._mindspace_manager.get_relative_path(path)

        except Exception:
            # If conversion fails, fall back to absolute path
            pass

        return path

    def _apply_styling(self) -> None:
        """Apply consistent styling to the dialog."""
        base_stylesheet = self._style_manager.get_dialog_stylesheet()

        # Add custom styles for the move dialog
        custom_styles = """
            QLabel[detailLabel="true"] {
                font-weight: bold;
            }
        """

        self.setStyleSheet(base_stylesheet + custom_styles)

    def get_source_path(self) -> str:
        """Get the source path being moved."""
        return self._source_path

    def get_destination_path(self) -> str:
        """Get the destination path."""
        return self._dest_path
