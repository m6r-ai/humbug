"""Simplified inline editor widget for mindspace file tree items that works with Qt's editor system."""

import os
from typing import Callable, Any

from PySide6.QtWidgets import QLineEdit, QWidget
from PySide6.QtCore import Signal, Qt
from PySide6.QtGui import QFont, QKeyEvent

from humbug.color_role import ColorRole
from humbug.language.language_manager import LanguageManager
from humbug.style_manager import StyleManager


class MindspaceTreeInlineEditor(QLineEdit):
    """Simplified inline editor with validation for file/folder names that works with Qt's editor system."""

    edit_finished = Signal(str)  # Emits the new name when editing is confirmed
    edit_cancelled = Signal()    # Emits when editing is cancelled

    def __init__(
        self,
        initial_text: str = "",
        validation_callback: Callable[[str], tuple[bool, str]] | None = None,
        select_extension: bool = True,
        parent: QWidget | None = None
    ):
        """
        Initialize the inline editor.

        Args:
            initial_text: The initial text to show in the editor
            validation_callback: Optional callback for additional validation
            select_extension: Whether to select the file extension in addition to the name
            parent: Parent widget (should be the tree view's viewport)
        """
        super().__init__(parent)
        self._style_manager = StyleManager()
        self._language_manager = LanguageManager()
        self._validation_callback = validation_callback
        self._select_extension = select_extension

        # Set initial text
        self.setText(initial_text)

        # Track validation state
        self._is_valid = True

        # Connect text change signal for real-time validation
        self.textChanged.connect(self._validate_input)

        # Apply initial styling
        self._apply_styling()

        # Perform initial validation and selection
        self._validate_input()
        self._apply_initial_selection(initial_text, select_extension)

        # Set focus
        self.setFocus()

    def _apply_initial_selection(self, text: str, select_extension: bool) -> None:
        """
        Apply appropriate text selection based on the selection mode.

        Args:
            text: The text to apply selection to
            select_extension: Whether to include extension in selection
        """
        if select_extension:
            # Select all text (for new files)
            self.selectAll()
            return

        # Select only the filename part (for rename/duplicate)
        name_part, ext = os.path.splitext(text)
        if ext and name_part:  # Has extension
            self.setSelection(0, len(name_part))
            return

        # No extension or empty name, select all
        self.selectAll()

    def _validate_input(self) -> None:
        """Validate the current input and update visual feedback."""
        text = self.text().strip()

        # Check basic validation
        if not text:
            self._set_validation_state(False, self._language_manager.strings().error_empty_name)
            return

        # Check for invalid filesystem characters
        invalid_chars = r'\/:*?"<>|'
        if any(c in invalid_chars for c in text):
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
            self.setToolTip("")
        else:
            self.setToolTip(error_message)

        self._apply_styling()

    def _apply_styling(self) -> None:
        """Apply styling based on validation state and current zoom."""
        zoom_factor = self._style_manager.zoom_factor()
        base_font_size = self._style_manager.base_font_size()

        # Set scaled font
        font = QFont()
        font.setPointSizeF(base_font_size * zoom_factor)
        self.setFont(font)

        if self._is_valid:
            # Normal styling
            style = f"""
                QLineEdit {{
                    background-color: {self._style_manager.get_color_str(ColorRole.EDIT_BOX_BACKGROUND)};
                    color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                    border: 1px solid {self._style_manager.get_color_str(ColorRole.EDIT_BOX_BORDER)};
                    padding: 2px;
                    selection-background-color: {self._style_manager.get_color_str(ColorRole.TEXT_SELECTED)};
                    selection-color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                }}
            """

        else:
            # Error styling with red border
            style = f"""
                QLineEdit {{
                    background-color: {self._style_manager.get_color_str(ColorRole.EDIT_BOX_BACKGROUND)};
                    color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                    border: 2px solid {self._style_manager.get_color_str(ColorRole.EDIT_BOX_ERROR)};
                    padding: 1px;
                    selection-background-color: {self._style_manager.get_color_str(ColorRole.TEXT_SELECTED)};
                    selection-color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                }}
            """

        self.setStyleSheet(style)

    def get_text(self) -> str:
        """Get the current text in the editor."""
        return self.text().strip()

    def is_valid(self) -> bool:
        """Check if the current input is valid."""
        return self._is_valid

    def set_text(self, text: str) -> None:
        """Set the text in the editor."""
        self.setText(text)
