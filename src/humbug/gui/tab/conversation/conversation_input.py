"""Input widget that matches history message styling."""

import sys
from typing import Dict

from PySide6.QtCore import Signal, Qt, QMimeData, QRect
from PySide6.QtGui import QKeyEvent, QTextCursor

from humbug.gui.color_role import ColorRole
from humbug.gui.tab.conversation.conversation_text_edit import ConversationTextEdit
from humbug.gui.tab.conversation.conversation_message import ConversationMessage
from humbug.language.language_manager import LanguageManager


class ConversationInput(ConversationMessage):
    """Widget for conversation message input that matches history message styling."""

    # Forward text cursor signals from the input area
    cursorPositionChanged = Signal()
    pageScrollRequested = Signal()

    def __init__(self, parent=None):
        """Initialize the conversation input widget."""
        self._is_streaming = False
        super().__init__(parent, is_input=True)

        # Connect text cursor signals
        self._sections[0]._text_area.cursorPositionChanged.connect(self.cursorPositionChanged)
        self._sections[0]._text_area.pageScrollRequested.connect(self.pageScrollRequested)

        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._handle_language_changed)

        self._update_header_text()

    def _handle_language_changed(self):
        self._update_header_text()

    def set_streaming(self, streaming: bool):
        """Update the streaming state and header text."""
        self._is_streaming = streaming
        self._update_header_text()

    def _get_submit_key_text(self):
        """Get the appropriate submit key text based on the platform."""
        if sys.platform == "darwin":
            return "⌘J"

        return "Ctrl+J"

    def _update_header_text(self):
        """Update the header text based on current state."""
        strings = self._language_manager.strings
        if self._is_streaming:
            self._role_label.setText(strings.processing_message)
        else:
            submit_key = self._get_submit_key_text()
            self._role_label.setText(strings.input_prompt.format(key=submit_key))

        self._set_role_style()

    def _set_role_style(self):
        """Set the role label color."""
        colour = ColorRole.TEXT_DISABLED if self._is_streaming else ColorRole.MESSAGE_USER

        # WARNING: This needs to stay in sync with ConversationMessage
        self._role_label.setStyleSheet(f"""
            QLabel {{
                color: {self._style_manager.get_color_str(colour)};
                margin: 0;
                padding: 0;
                background-color: {self._style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
            }}
        """)

    def _create_text_area(self) -> ConversationTextEdit:
        """Create and configure the input text area."""
        text_area = super()._create_text_area()
        # Override paste behavior to strip formatting
        text_area.insertFromMimeData = self._insert_from_mime_data
        return text_area

    def _insert_from_mime_data(self, source: QMimeData) -> None:
        """Override default paste behavior to insert only plain text."""
        if source.hasText():
            cursor = self._sections[0]._text_area.textCursor()
            cursor.insertText(source.text())

    def keyPressEvent(self, event: QKeyEvent):
        """Handle special key events."""
        if event.key() == Qt.Key_J and event.modifiers() == Qt.ControlModifier:
            if not self._is_streaming:
                text = self._sections[0]._text_area.toPlainText().strip()
                if text:
                    self.clear()

                return

        super().keyPressEvent(event)

    def clear(self):
        """Clear the input area."""
        self._sections[0]._text_area.clear()

    def to_plain_text(self) -> str:
        """Get the current input text."""
        return self._sections[0]._text_area.toPlainText()

    def set_plain_text(self, text: str):
        """Set the input text."""
        self._sections[0]._text_area.setPlainText(text)

    def cursor_rect(self):
        """Get the cursor rectangle from the input area."""
        text_cursor = self._sections[0]._text_area.cursorRect()
        offset = self._header.height()
        cursor = QRect(text_cursor.x(), offset + text_cursor.y(), text_cursor.width(), text_cursor.height())
        return cursor

    def setFocus(self):
        """Set focus to the input area."""
        print("set input focus")
        self._sections[0]._text_area.setFocus()

    def hasFocus(self) -> bool:
        """Check if the input area has focus."""
        return self._sections[0]._text_area.hasFocus()

    def document(self):
        """Get the document from the input area."""
        return self._sections[0]._text_area.document()

    def text_cursor(self):
        """Get the text cursor from the input area."""
        return self._sections[0]._text_area.textCursor()

    def undo(self):
        """Undo the last edit operation."""
        self._sections[0]._text_area.undo()

    def redo(self):
        """Redo the last undone edit operation."""
        self._sections[0]._text_area.redo()

    def cut(self):
        """Cut selected text to clipboard."""
        self._sections[0]._text_area.cut()

    def copy(self):
        """Copy selected text to clipboard."""
        self._sections[0]._text_area.copy()

    def paste(self):
        """Paste text from clipboard."""
        self._sections[0]._text_area.paste()

    def set_cursor_position(self, position: Dict[str, int]) -> None:
        """
        Set cursor position.

        Args:
            position: Dictionary with 'line' and 'column' keys
        """
        cursor = self._sections[0]._text_area.textCursor()
        cursor.movePosition(QTextCursor.Start)

        # Move cursor to specified position
        for _ in range(position.get("line", 0)):
            cursor.movePosition(QTextCursor.NextBlock)

        cursor.movePosition(
            QTextCursor.Right,
            QTextCursor.MoveAnchor,
            position.get("column", 0)
        )

        self._sections[0]._text_area.setTextCursor(cursor)

    def get_cursor_position(self) -> Dict[str, int]:
        """
        Get current cursor position.

        Returns:
            Dictionary with 'line' and 'column' keys
        """
        cursor = self._sections[0]._text_area.textCursor()
        return {
            "line": cursor.blockNumber(),
            "column": cursor.columnNumber()
        }
