"""Input widget that matches history message styling."""

import sys
from typing import Dict

from PySide6.QtCore import Signal, Qt, QMimeData, QRect
from PySide6.QtGui import QKeyEvent, QTextCursor, QTextDocument
from PySide6.QtWidgets import QWidget

from humbug.gui.color_role import ColorRole
from humbug.gui.tab.system.system_message import SystemMessage
from humbug.language.language_manager import LanguageManager


class SystemInput(SystemMessage):
    """Widget for system message input that matches history message styling."""

    # Forward text cursor signals from the input area
    cursorPositionChanged = Signal()
    pageScrollRequested = Signal()

    def __init__(self, parent: QWidget | None = None) -> None:
        """Initialize the system input widget."""
        super().__init__(parent, is_input=True)

        # Connect text cursor signals
        self._text_area = self._sections[0].text_area()

        self._text_area.cursorPositionChanged.connect(self.cursorPositionChanged)
        self._text_area.pageScrollRequested.connect(self.pageScrollRequested)

        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._handle_language_changed)

        self._message_source = "user"  # Set default source for styling
        self._update_header_text()

    def _handle_language_changed(self) -> None:
        """Handle language change event."""
        self._update_header_text()

    def _get_submit_key_text(self) -> str:
        """Get the appropriate submit key text based on the platform."""
        if sys.platform == "darwin":
            return "⌘J"

        return "Ctrl+J"

    def _update_header_text(self) -> None:
        """Update the header text based on current state."""
        strings = self._language_manager.strings()

        # Set input prompt with submit key
        submit_key = self._get_submit_key_text()
        self._role_label.setText(strings.input_prompt.format(key=submit_key))

        self._set_role_style()

    def _set_role_style(self) -> None:
        """Set the role label color."""
        colour = ColorRole.MESSAGE_USER

        # WARNING: This needs to stay in sync with SystemMessage
        self._role_label.setStyleSheet(f"""
            QLabel {{
                color: {self._style_manager.get_color_str(colour)};
                margin: 0;
                padding: 0;
                background-color: {self._style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
            }}
        """)

    def _insert_from_mime_data(self, source: QMimeData) -> None:
        """Override default paste behavior to insert only plain text."""
        if source.hasText():
            cursor = self._text_area.textCursor()
            cursor.insertText(source.text())

    def keyPressEvent(self, event: QKeyEvent) -> None:
        """Handle special key events."""
        if event.key() == Qt.Key.Key_J and event.modifiers() & Qt.KeyboardModifier.ControlModifier:
            text = self._text_area.toPlainText().strip()
            if text:
                self.clear()
                return

        super().keyPressEvent(event)

    def clear(self) -> None:
        """Clear the input area."""
        self._text_area.clear()

    def to_plain_text(self) -> str:
        """Get the current input text."""
        return self._text_area.toPlainText()

    def set_plain_text(self, text: str) -> None:
        """Set the input text."""
        self._text_area.setPlainText(text)

    def cursor_rect(self) -> QRect:
        """Get the cursor rectangle from the input area."""
        text_cursor = self._text_area.cursorRect()
        offset = self._header.height()
        cursor = QRect(text_cursor.x(), offset + text_cursor.y(), text_cursor.width(), text_cursor.height())
        return cursor

    def setFocus(self, reason: Qt.FocusReason | None = None) -> None:
        """Set focus to the input area."""
        if reason is None:
            self._text_area.setFocus()
            return

        self._text_area.setFocus(reason)

    def hasFocus(self) -> bool:
        """Check if the input area has focus."""
        return self._text_area.hasFocus()

    def document(self) -> QTextDocument:
        """Get the document from the input area."""
        return self._text_area.document()

    def text_cursor(self) -> QTextCursor:
        """Get the text cursor from the input area."""
        return self._text_area.textCursor()

    def undo(self) -> None:
        """Undo the last edit operation."""
        self._text_area.undo()

    def redo(self) -> None:
        """Redo the last undone edit operation."""
        self._text_area.redo()

    def cut(self) -> None:
        """Cut selected text to clipboard."""
        self._text_area.cut()

    def copy(self) -> None:
        """Copy selected text to clipboard."""
        self._text_area.copy()

    def paste(self) -> None:
        """Paste text from clipboard."""
        self._text_area.paste()

    def set_cursor_position(self, position: Dict[str, int]) -> None:
        """
        Set cursor position.

        Args:
            position: Dictionary with 'line' and 'column' keys
        """
        cursor = self._text_area.textCursor()
        cursor.movePosition(QTextCursor.MoveOperation.Start)

        # Move cursor to specified position
        for _ in range(position.get("line", 0)):
            cursor.movePosition(QTextCursor.MoveOperation.NextBlock)

        cursor.movePosition(
            QTextCursor.MoveOperation.Right,
            QTextCursor.MoveMode.MoveAnchor,
            position.get("column", 0)
        )

        self._text_area.setTextCursor(cursor)

    def get_cursor_position(self) -> Dict[str, int]:
        """
        Get current cursor position.

        Returns:
            Dictionary with 'line' and 'column' keys
        """
        cursor = self._text_area.textCursor()
        return {
            "line": cursor.blockNumber(),
            "column": cursor.columnNumber()
        }
