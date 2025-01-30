from PySide6.QtWidgets import (
    QDialog, QVBoxLayout, QLabel, QLineEdit, QPushButton, QDialogButtonBox
)

from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager
from humbug.language.language_manager import LanguageManager


class ConversationRenameDialog(QDialog):
    """Dialog for renaming conversations."""

    def __init__(self, old_name: str, parent=None):
        super().__init__(parent)
        self._style_manager = StyleManager()
        self._language_manager = LanguageManager()
        strings = self._language_manager.strings

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
            QDialogButtonBox.Ok | QDialogButtonBox.Cancel
        )
        button_box.accepted.connect(self.accept)
        button_box.rejected.connect(self.reject)

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

    def _validate_input(self):
        """Validate the input and enable/disable OK button."""
        text = self._name_input.text().strip()
        valid = bool(text and not any(c in '\/:*?"<>|' for c in text))

        # Find OK button and set enabled state
        for button in self.findChildren(QPushButton):
            if button.text() == "OK":
                button.setEnabled(valid)
                break

    def _apply_styling(self):
        """Apply consistent styling to the dialog."""
        self.setStyleSheet(f"""
            QDialog {{
                background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_DIALOG)};
            }}
            QLabel {{
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_DIALOG)};
            }}
            QLineEdit {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND)};
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: none;
                border-radius: 4px;
                padding: 8px;
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

    def get_name(self) -> str:
        """Get the entered name."""
        return self._name_input.text().strip()
