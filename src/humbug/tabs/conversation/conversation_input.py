"""Input widget that matches history message styling."""

import sys
from typing import cast, Dict

from PySide6.QtCore import Signal, Qt, QMimeData, QRect, QSize
from PySide6.QtGui import QTextCursor, QTextDocument, QIcon
from PySide6.QtWidgets import QWidget, QToolButton

from ai import AIMessageSource

from humbug.color_role import ColorRole
from humbug.tabs.conversation.conversation_message import ConversationMessage


class ConversationInput(ConversationMessage):
    """Widget for conversation message input that matches history message styling."""

    # Forward text cursor signals from the input area
    cursor_position_changed = Signal()
    page_key_scroll_requested = Signal()
    submit_requested = Signal()
    stop_requested = Signal()
    interrupt_requested = Signal()
    modified = Signal()

    def __init__(self, style: AIMessageSource, parent: QWidget | None = None) -> None:
        """Initialize the conversation input widget."""
        self._is_streaming = False
        self._current_model = ""
        self._submit_button: QToolButton | None = None
        self._stop_button: QToolButton | None = None

        super().__init__(style, parent=parent, is_input=True)

        # Connect text cursor signals
        self._text_area = self._sections[0].text_area()

        self._text_area.cursorPositionChanged.connect(self.cursor_position_changed)
        self._text_area.page_key_scroll_requested.connect(self.page_key_scroll_requested)

        # Create stop button (initially hidden)
        self._stop_button = QToolButton(self)
        self._stop_button.clicked.connect(self._stop_message)
        self._stop_button.hide()
        self._header_layout.addWidget(self._stop_button)

        # Create submit/interrupt button
        self._submit_button = QToolButton(self)
        self._submit_button.clicked.connect(self._on_primary_button_clicked)
        self._header_layout.addWidget(self._submit_button)

        # Connect text changes to update button state
        self._text_area.textChanged.connect(self._on_text_changed)

        self._update_header_text()
        self._on_style_changed()
        self._update_button_states()

    def set_model(self, model: str) -> None:
        """Set the model name for the input prompt."""
        self._current_model = model
        self._update_header_text()

    def _on_language_changed(self) -> None:
        """Handle language change event."""
        self._update_header_text()
        self._update_button_tooltips()

    def _update_button_tooltips(self) -> None:
        """Update button tooltips based on current state."""
        strings = self._language_manager.strings()

        if self._stop_button:
            self._stop_button.setToolTip(strings.tooltip_stop_message)

        if self._submit_button:
            if self._is_streaming:
                self._submit_button.setToolTip(strings.tooltip_interrupt_message)

            else:
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

    def _on_style_changed(self) -> None:
        """Handle the style changing."""
        super()._on_style_changed()
        self._set_role_style()
        self._update_button_styling()

    def _update_button_styling(self) -> None:
        """Update button styling and icons."""
        if self._submit_button is None:
            return

        # Use the same button style pattern as other message action buttons
        background_color = self._style_manager.get_color_str(ColorRole.MESSAGE_USER_BACKGROUND)
        button_style = f"""
            QToolButton {{
                background-color: {background_color};
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: none;
                padding: 0px;
            }}
            QToolButton:hover {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_HOVER)};
            }}
            QToolButton:pressed {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_PRESSED)};
            }}
            QToolButton:disabled {{
                color: {self._style_manager.get_color_str(ColorRole.TEXT_DISABLED)};
                background-color: {background_color};
            }}
        """

        # Apply icon and styling
        icon_base_size = 14
        icon_scaled_size = int(icon_base_size * self._style_manager.zoom_factor())
        icon_size = QSize(icon_scaled_size, icon_scaled_size)

        # Update submit/interrupt button
        submit_button = cast(QToolButton, self._submit_button)
        submit_button.setIcon(QIcon(self._style_manager.scale_icon(
            self._style_manager.get_icon_path("submit"), icon_base_size
        )))
        submit_button.setIconSize(icon_size)
        submit_button.setStyleSheet(button_style)

        # Update stop button if it exists
        if self._stop_button:
            stop_button = cast(QToolButton, self._stop_button)
            stop_button.setIcon(QIcon(self._style_manager.scale_icon(
                self._style_manager.get_icon_path("stop"), icon_base_size
            )))
            stop_button.setIconSize(icon_size)
            stop_button.setStyleSheet(button_style)

    def _update_header_text(self) -> None:
        """Update the header text based on current state."""
        strings = self._language_manager.strings()
        if self._is_streaming:
            self._role_label.setText(strings.processing_message)

        else:
            submit_key = self._get_submit_key_text()
            self._role_label.setText(strings.input_prompt.format(model=self._current_model, key=submit_key))

        self._set_role_style()

    def _set_role_style(self) -> None:
        """Set the role label color."""
        colour = ColorRole.MESSAGE_STREAMING if self._is_streaming else ColorRole.MESSAGE_USER

        # WARNING: This needs to stay in sync with ConversationMessage
        self._role_label.setStyleSheet(f"""
            QLabel {{
                color: {self._style_manager.get_color_str(colour)};
                margin: 0;
                padding: 0;
                background-color: {self._style_manager.get_color_str(ColorRole.MESSAGE_USER_BACKGROUND)};
            }}
        """)

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

        # Update tooltips and styling
        self._update_button_tooltips()
        self._update_button_styling()

    def _on_primary_button_clicked(self) -> None:
        """Handle primary button (submit/interrupt) click."""
        if self._is_streaming:
            self.interrupt_requested.emit()

        else:
            self.submit_requested.emit()

    def _stop_message(self) -> None:
        """Stop the current message processing via button click."""
        self.stop_requested.emit()

    def _insert_from_mime_data(self, source: QMimeData) -> None:
        """Override default paste behavior to insert only plain text."""
        if source.hasText():
            cursor = self._text_area.textCursor()
            cursor.insertText(source.text())

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
