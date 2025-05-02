from PySide6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QLabel, QLineEdit, QPushButton, QWidget
)

from humbug.gui.style_manager import StyleManager
from humbug.language.language_manager import LanguageManager


class MindspaceConversationRenameDialog(QDialog):
    """Dialog for renaming conversations."""

    def __init__(self, old_name: str, parent: QWidget | None = None):
        super().__init__(parent)
        self._style_manager = StyleManager()

        self._language_manager = LanguageManager()
        strings = self._language_manager.strings()

        self.setWindowTitle(strings.rename_conversation)
        self.setModal(True)
        self.setMinimumWidth(400)

        layout = QVBoxLayout()
        layout.setSpacing(12)
        layout.setContentsMargins(20, 20, 20, 20)

        zoom_factor = self._style_manager.zoom_factor()
        element_width = int(zoom_factor * 300)

        name_layout = QHBoxLayout()
        name_label = QLabel(strings.conversation_name)
        name_label.setMinimumHeight(40)
        self._name_input = QLineEdit()
        self._name_input.setText(old_name)
        self._name_input.setMinimumHeight(40)
        self._name_input.setMinimumWidth(element_width)
        self._name_input.textChanged.connect(self._validate_input)
        name_layout.addWidget(name_label)
        name_layout.addStretch()
        name_layout.addWidget(self._name_input)
        layout.addLayout(name_layout)

        # Add spacing before buttons
        layout.addSpacing(24)

        # Button row
        button_layout = QHBoxLayout()
        button_layout.setSpacing(8)
        button_layout.addStretch()

        # Create OK and Cancel buttons
        self._ok_button = QPushButton(strings.ok)
        self._ok_button.clicked.connect(self.accept)
        self._ok_button.setProperty("recommended", True)

        cancel_button = QPushButton(strings.cancel)
        cancel_button.clicked.connect(self.reject)

        # Set minimum button sizes
        zoom_factor = self._style_manager.zoom_factor()
        min_button_width = int(90 * zoom_factor)
        min_button_height = 40
        for button in [self._ok_button, cancel_button]:
            button.setMinimumWidth(min_button_width)
            button.setMinimumHeight(min_button_height)
            button.setContentsMargins(8, 8, 8, 8)
            button_layout.addWidget(button)

        button_layout.addStretch()
        layout.addLayout(button_layout)
        self.setLayout(layout)

        self._apply_styling()
        self._validate_input()

        # Select text for easy editing
        self._name_input.selectAll()

    def _validate_input(self) -> None:
        """
        Validate the input and enable/disable OK button.

        Ensures the filename doesn't contain invalid characters for a file system.
        """
        text = self._name_input.text().strip()
        valid = bool(text and not any(c in r'\/:*?"<>|' for c in text))

        # Enable/disable the OK button based on validation
        self._ok_button.setEnabled(valid)

    def _apply_styling(self) -> None:
        """Apply consistent styling to the dialog."""
        self.setStyleSheet(self._style_manager.get_dialog_stylesheet())

    def get_name(self) -> str:
        """Get the entered name."""
        return self._name_input.text().strip()
