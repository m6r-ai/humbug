"""Input widget that matches history message styling."""

import sys
from typing import Dict

from PySide6.QtCore import Signal, Qt, QRect, QSize
from PySide6.QtGui import QTextCursor, QTextDocument, QIcon
from PySide6.QtWidgets import QWidget, QToolButton

from ai import AIMessageSource

from humbug.tabs.conversation.conversation_message import ConversationMessage


class ConversationInput(ConversationMessage):
    """Widget for conversation message input that matches history message styling."""

    # Forward text cursor signals from the input area
    cursor_position_changed = Signal()
    page_key_scroll_requested = Signal()
    submit_requested = Signal()
    stop_requested = Signal()
    settings_requested = Signal()
    modified = Signal()

    def __init__(self, style: AIMessageSource, parent: QWidget | None = None) -> None:
        """Initialize the conversation input widget."""
        self._is_streaming = False
        self._current_model = ""
        self._submit_button: QToolButton | None = None
        self._stop_button: QToolButton | None = None
        self._settings_button: QToolButton | None = None

        super().__init__(style, parent=parent, is_input=True)

        # Connect text cursor signals
        self._text_area = self._sections[0].text_area()

        self._text_area.cursorPositionChanged.connect(self.cursor_position_changed)
        self._text_area.page_key_scroll_requested.connect(self.page_key_scroll_requested)

        # Create stop button (initially hidden)
        self._stop_button = QToolButton(self)
        self._stop_button.setObjectName("_stop_button")
        self._stop_button.clicked.connect(self._on_stop_button_clicked)
        self._stop_button.hide()
        self._header_layout.addWidget(self._stop_button)

        # Create submit button
        self._submit_button = QToolButton(self)
        self._submit_button.setObjectName("_submit_button")
        self._submit_button.clicked.connect(self._on_submit_button_clicked)
        self._header_layout.addWidget(self._submit_button)

        # Create settings button
        self._settings_button = QToolButton(self)
        self._settings_button.setObjectName("_settings_button")
        self._settings_button.clicked.connect(self._on_settings_button_clicked)
        self._header_layout.addWidget(self._settings_button)

        # Connect text changes to update button state
        self._text_area.textChanged.connect(self._on_text_changed)

        self._on_language_changed()

    def set_model(self, model: str) -> None:
        """Set the model name for the input prompt."""
        self._current_model = model
        self._update_header_text()

    def _on_language_changed(self) -> None:
        """Handle language change event."""
        self._update_header_text()

        strings = self._language_manager.strings()

        if self._settings_button:
            self._settings_button.setToolTip(strings.tooltip_settings_message)

        if self._stop_button:
            self._stop_button.setToolTip(strings.tooltip_stop_message)

        if self._submit_button:
            self._submit_button.setToolTip(strings.tooltip_submit_message)

    def set_streaming(self, streaming: bool) -> None:
        """Update the streaming state and header text."""
        self._is_streaming = streaming
        self._update_header_text()
        self._update_button_states()

    def _get_submit_key_text(self) -> str:
        """Get the appropriate submit key text based on the platform."""
        if sys.platform == "darwin":
            return "⌘J"

        return "Ctrl+J"

    def apply_style(self) -> None:
        """Apply style changes."""
        super().apply_style()

        # Apply icon and styling
        icon_base_size = 14
        icon_scaled_size = int(icon_base_size * self._style_manager.zoom_factor())
        icon_size = QSize(icon_scaled_size, icon_scaled_size)

        # Update submit/interrupt button
        if self._submit_button:
            self._submit_button.setIcon(QIcon(self._style_manager.scale_icon("submit", icon_base_size)))
            self._submit_button.setIconSize(icon_size)

        # Update settings button
        if self._settings_button:
            self._settings_button.setIcon(QIcon(self._style_manager.scale_icon("cog", icon_base_size)))
            self._settings_button.setIconSize(icon_size)

        # Update stop button if it exists
        if self._stop_button:
            self._stop_button.setIcon(QIcon(self._style_manager.scale_icon("stop", icon_base_size)))
            self._stop_button.setIconSize(icon_size)

    def _update_header_text(self) -> None:
        """Update the header text based on current state."""
        strings = self._language_manager.strings()
        if self._is_streaming:
            self._role_label.setText(strings.processing_message)
            self.setProperty("message_source", "ai_streaming")

        else:
            submit_key = self._get_submit_key_text()
            self._role_label.setText(strings.input_prompt.format(model=self._current_model, key=submit_key))
            self.setProperty("message_source", "user")

        self._role_label.style().unpolish(self._role_label)
        self._role_label.style().polish(self._role_label)

    def _on_text_changed(self) -> None:
        """Handle text changes in the input area."""
        self._update_button_states()
        self.modified.emit()

    def _update_button_states(self) -> None:
        """Update button enabled states and visibility based on content and streaming status."""
        has_content = bool(self.to_plain_text().strip())

        if self._is_streaming:
            # Show both stop and interrupt buttons
            if self._stop_button:
                self._stop_button.show()
                self._stop_button.setEnabled(True)

            if self._submit_button:
                # Interrupt button only enabled if has content
                self._submit_button.setEnabled(has_content)

        else:
            # Hide stop button, show only submit
            if self._stop_button:
                self._stop_button.hide()

            if self._submit_button:
                # Submit button only enabled if has content
                self._submit_button.setEnabled(has_content)

    def _on_submit_button_clicked(self) -> None:
        """Handle submit button click."""
        self.submit_requested.emit()

    def _on_stop_button_clicked(self) -> None:
        """Stop the current message processing via button click."""
        self.stop_requested.emit()

    def _on_settings_button_clicked(self) -> None:
        """Handle settings button click."""
        self.settings_requested.emit()

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
