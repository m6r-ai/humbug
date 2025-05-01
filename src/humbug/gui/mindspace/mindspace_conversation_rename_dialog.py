from PySide6.QtWidgets import (
    QDialog, QVBoxLayout, QLabel, QLineEdit, QDialogButtonBox, QWidget
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

        # Add name input
        self._name_input = QLineEdit()
        self._name_input.setText(old_name)
        self._name_input.setMinimumHeight(40)
        self._name_input.textChanged.connect(self._validate_input)

        # Add label and input
        name_label = QLabel(strings.conversation_name)
        name_label.setMinimumHeight(40)
        layout.addWidget(name_label)
        layout.addWidget(self._name_input)

        # Add buttons
        button_box = QDialogButtonBox(
            QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel
        )
        button_box.accepted.connect(self.accept)
        button_box.rejected.connect(self.reject)
        button_box.setCenterButtons(True)

        # We need to know which is the OK button so we can enable/disable it based in input validation
        self._ok_button = button_box.button(QDialogButtonBox.StandardButton.Ok)
        self._ok_button.setProperty("recommended", True)

        # Style buttons
        for button in button_box.buttons():
            button.setMinimumSize(90, 40)
            button.setContentsMargins(8, 8, 8, 8)

        layout.addSpacing(24)
        layout.addWidget(button_box)

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
