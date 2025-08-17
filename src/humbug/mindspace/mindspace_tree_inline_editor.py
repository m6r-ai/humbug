"""Simplified inline editor widget for mindspace file tree items that works with Qt's editor system."""

import os
from typing import Callable

from PySide6.QtWidgets import QLineEdit, QWidget
from PySide6.QtCore import Signal
from PySide6.QtGui import QFont

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
        self._style_manager.style_changed.connect(self._on_style_changed)

        self._language_manager = LanguageManager()
        self._validation_callback = validation_callback
        self._select_extension = select_extension

        # Set initial text
        self.setText(initial_text)

        # Track validation state
        self._is_valid = True
        self.setProperty("is_valid", self._is_valid)

        # Connect text change signal for real-time validation
        self.textChanged.connect(self._validate_input)

        # Apply initial styling
        self._on_style_changed()

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
        strings = self._language_manager.strings()

        # Check basic validation
        if not text:
            self._is_valid = False
            self.setProperty("is_valid", False)
            self.setToolTip(strings.error_empty_name)
            self.style().unpolish(self)
            self.style().polish(self)
            return

        # Check for invalid filesystem characters
        invalid_chars = r'\/:*?"<>|'
        if any(c in invalid_chars for c in text):
            self._is_valid = False
            self.setProperty("is_valid", False)
            self.setToolTip(strings.error_invalid_characters)
            self.style().unpolish(self)
            self.style().polish(self)
            return

        # Call additional validation callback if provided
        if self._validation_callback:
            is_valid, error_message = self._validation_callback(text)
            if not is_valid:
                self._is_valid = False
                self.setProperty("is_valid", False)
                self.setToolTip(error_message)
                self.style().unpolish(self)
                self.style().polish(self)
                return

        # All validation passed
        self._is_valid = True
        self.setProperty("is_valid", True)
        self.setToolTip("")
        self.style().unpolish(self)
        self.style().polish(self)

    def _on_style_changed(self) -> None:
        """Apply styling based on validation state and current zoom."""
        zoom_factor = self._style_manager.zoom_factor()
        base_font_size = self._style_manager.base_font_size()

        # Set scaled font
        font = QFont()
        font.setPointSizeF(base_font_size * zoom_factor)
        self.setFont(font)

    def get_text(self) -> str:
        """Get the current text in the editor."""
        return self.text().strip()

    def is_valid(self) -> bool:
        """Check if the current input is valid."""
        return self._is_valid

    def set_text(self, text: str) -> None:
        """Set the text in the editor."""
        self.setText(text)
