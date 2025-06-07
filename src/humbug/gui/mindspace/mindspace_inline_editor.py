"""Inline editor widget for mindspace file tree items."""

from typing import Callable

from PySide6.QtWidgets import QLineEdit, QVBoxLayout, QLabel, QWidget
from PySide6.QtCore import Signal, Qt

from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager
from humbug.language.language_manager import LanguageManager


class MindspaceInlineEditor(QWidget):
    """Custom inline editor with validation for file/folder names."""

    edit_finished = Signal(str)  # Emits the new name when editing is confirmed
    edit_cancelled = Signal()    # Emits when editing is cancelled

    def __init__(
        self,
        initial_text: str,
        is_conversation: bool = False,
        validation_callback: Callable[[str], tuple[bool, str]] | None = None
    ):
        """
        Initialize the inline editor.

        Args:
            initial_text: The initial text to show in the editor
            is_conversation: Whether this is editing a conversation file
            validation_callback: Optional callback for additional validation
        """
        super().__init__()
        self._style_manager = StyleManager()
        self._language_manager = LanguageManager()
        self._is_conversation = is_conversation
        self._validation_callback = validation_callback

        # Create layout
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(2)

        # Create line edit
        self._line_edit = QLineEdit()
        self._line_edit.setText(initial_text)
        self._line_edit.selectAll()
        self._line_edit.textChanged.connect(self._validate_input)
        layout.addWidget(self._line_edit)

        # Create error label (initially hidden)
        self._error_label = QLabel()
        self._error_label.setWordWrap(True)
        self._error_label.hide()
        layout.addWidget(self._error_label)

        # Track validation state
        self._is_valid = True

        # Apply initial styling
        self._apply_styling()

        # Perform initial validation
        self._validate_input()

        # Install event filter to handle key events
        self._line_edit.installEventFilter(self)

    def _validate_input(self) -> None:
        """Validate the current input and update visual feedback."""
        text = self._line_edit.text().strip()

        # Check basic validation
        if not text:
            self._set_validation_state(False, self._language_manager.strings().error_empty_name)
            return

        # Check for invalid file system characters
        invalid_chars = r'\/:*?"<>|'
        if any(c in invalid_chars for c in text):
            self._set_validation_state(False, self._language_manager.strings().error_invalid_characters)
            return

        # For conversations, ensure .conv extension
        if self._is_conversation and not text.endswith('.conv'):
            text_with_ext = text + '.conv'
            self._line_edit.setText(text_with_ext)
            # Re-validate with the extension
            if any(c in invalid_chars for c in text_with_ext):
                self._set_validation_state(False, self._language_manager.strings().error_invalid_characters)
                return

        # Call additional validation callback if provided
        if self._validation_callback:
            is_valid, error_message = self._validation_callback(text)
            if not is_valid:
                self._set_validation_state(False, error_message)
                return

        # All validation passed
        self._set_validation_state(True, "")

    def _set_validation_state(self, is_valid: bool, error_message: str) -> None:
        """
        Set the validation state and update visual feedback.

        Args:
            is_valid: Whether the current input is valid
            error_message: Error message to display (empty if valid)
        """
        self._is_valid = is_valid

        if is_valid:
            self._error_label.hide()

        else:
            self._error_label.setText(error_message)
            self._error_label.show()

        self._apply_styling()

    def _apply_styling(self) -> None:
        """Apply styling based on validation state."""
        if self._is_valid:
            # Normal styling
            line_edit_style = f"""
                QLineEdit {{
                    background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_PRIMARY)};
                    color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                    border: 1px solid {self._style_manager.get_color_str(ColorRole.TEXT_DISABLED)};
                    padding: 2px;
                }}
                QLineEdit:focus {{
                    border: 1px solid {self._style_manager.get_color_str(ColorRole.TEXT_SELECTED)};
                }}
            """

        else:
            # Error styling - red background
            error_color = "#ffebee"  # Light red background
            error_border = "#f44336"  # Red border
            line_edit_style = f"""
                QLineEdit {{
                    background-color: {error_color};
                    color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                    border: 1px solid {error_border};
                    padding: 2px;
                }}
                QLineEdit:focus {{
                    border: 1px solid {error_border};
                }}
            """

        # Error label styling
        error_label_style = f"""
            QLabel {{
                color: #f44336;
                font-size: 11px;
                padding: 2px;
                background-color: transparent;
            }}
        """

        self._line_edit.setStyleSheet(line_edit_style)
        self._error_label.setStyleSheet(error_label_style)

    def eventFilter(self, obj, event) -> bool:
        """Handle key events for the line edit."""
        if obj == self._line_edit and event.type() == event.Type.KeyPress:
            key_event = event

            if key_event.key() == Qt.Key.Key_Return or key_event.key() == Qt.Key.Key_Enter:
                # Only accept if validation passes
                if self._is_valid:
                    self.edit_finished.emit(self._line_edit.text().strip())

                return True

            elif key_event.key() == Qt.Key.Key_Escape:
                self.edit_cancelled.emit()
                return True

        return super().eventFilter(obj, event)

    def get_text(self) -> str:
        """Get the current text in the editor."""
        return self._line_edit.text().strip()

    def is_valid(self) -> bool:
        """Check if the current input is valid."""
        return self._is_valid

    def focus_editor(self) -> None:
        """Set focus to the line edit."""
        self._line_edit.setFocus()
        self._line_edit.selectAll()
