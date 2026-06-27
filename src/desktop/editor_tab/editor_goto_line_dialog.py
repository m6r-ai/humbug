"""Modal dialog for navigating to a specific line number in the editor."""

from PySide6.QtWidgets import (
    QDialog, QHBoxLayout, QLabel, QLineEdit, QPushButton, QVBoxLayout, QWidget
)
from PySide6.QtCore import Qt
from PySide6.QtGui import QIntValidator

from desktop.language.language_manager import LanguageManager
from desktop.style_manager import StyleManager


class EditorGotoLineDialog(QDialog):
    """Simple modal dialog that prompts the user for a line number."""

    def __init__(self, max_line: int, current_line: int, parent: QWidget | None = None) -> None:
        """
        Initialize the dialog.

        Args:
            max_line: Total number of lines in the document (used as validator upper bound).
            current_line: Current cursor line, pre-filled in the input.
            parent: Optional parent widget.
        """
        super().__init__(parent)
        self._language_manager = LanguageManager()
        self._style_manager = StyleManager()

        strings = self._language_manager.strings()
        self.setWindowTitle(strings.goto_line_title)
        self.setWindowFlags(self.windowFlags() & ~Qt.WindowType.WindowContextHelpButtonHint)
        self.setModal(True)
        self.setMinimumWidth(280)

        zoom_factor = self._style_manager.zoom_factor()
        spacing = int(self._style_manager.message_bubble_spacing())
        min_button_width = int(90 * zoom_factor)
        min_button_height = 40

        layout = QVBoxLayout(self)
        layout.setContentsMargins(20, 20, 20, 20)
        layout.setSpacing(12)

        self._label = QLabel(strings.goto_line_label)
        layout.addWidget(self._label)

        self._input = QLineEdit()
        self._input.setValidator(QIntValidator(1, max(1, max_line), self))
        self._input.setText(str(current_line))
        self._input.selectAll()
        layout.addWidget(self._input)

        layout.addSpacing(12)

        button_layout = QHBoxLayout()
        button_layout.setSpacing(spacing)
        button_layout.addStretch()

        self._cancel_button = QPushButton(strings.cancel)
        self._cancel_button.clicked.connect(self.reject)
        self._cancel_button.setMinimumWidth(min_button_width)
        self._cancel_button.setMinimumHeight(min_button_height)
        button_layout.addWidget(self._cancel_button)

        self._ok_button = QPushButton(strings.ok)
        self._ok_button.setProperty("recommended", True)
        self._ok_button.clicked.connect(self.accept)
        self._ok_button.setMinimumWidth(min_button_width)
        self._ok_button.setMinimumHeight(min_button_height)
        button_layout.addWidget(self._ok_button)

        button_layout.addStretch()
        layout.addLayout(button_layout)

        self._input.returnPressed.connect(self.accept)

        self.setStyleSheet(self._style_manager.get_dialog_stylesheet())

    def line_number(self) -> int:
        """Return the validated line number entered by the user."""
        try:
            return int(self._input.text())

        except ValueError:
            return 1
