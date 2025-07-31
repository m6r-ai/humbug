"""Input widget that matches history message styling."""

import sys
from typing import cast, Dict

from PySide6.QtCore import Signal, Qt, QMimeData, QRect, QSize, QTimer
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
    modified = Signal()

    def __init__(self, style: AIMessageSource, parent: QWidget | None = None) -> None:
        """Initialize the conversation input widget."""
        self._is_streaming = False
        self._current_model = ""
        self._submit_button: QToolButton | None = None

        super().__init__(style, parent=parent, is_input=True)

        # Connect text cursor signals
        self._text_area = self._sections[0].text_area()

        self._text_area.cursorPositionChanged.connect(self.cursor_position_changed)
        self._text_area.page_key_scroll_requested.connect(self.page_key_scroll_requested)

        # Create submit button
        self._submit_button = QToolButton(self)
        self._submit_button.clicked.connect(self._submit_message)
        self._header_layout.addWidget(self._submit_button)

        # Connect text changes to update button state
        self._text_area.textChanged.connect(self._on_text_changed)

        # Animation state for streaming border
        self._animation_frame = 0

        # Animation parameters for smooth fade
        self._animation_steps = 32  # Steps for half cycle (start to mid)
        self._fade_direction = 1    # 1 for brightening, -1 for darkening

        # Timer intervals
        self._slow_interval_ms = int(3000 / self._animation_steps)
        self._debounce_interval_ms = int(750 / self._animation_steps)

        # Slow timer - always running during streaming to provide regular updates
        self._slow_timer = QTimer()
        self._slow_timer.setInterval(self._slow_interval_ms)
        self._slow_timer.timeout.connect(self._on_slow_timer)

        # Pending message flag and counter for smooth transition
        self._pending_message = False
        self._no_message_counter = 0
        self._max_no_message_cycles = 16  # Number of cycles before disabling debounce timer

        # Debounce timer for message notifications
        self._debounce_timer = QTimer()
        self._debounce_timer.setSingleShot(True)
        self._debounce_timer.timeout.connect(self._on_debounce_timeout)
        self._debounce_timer.setInterval(self._debounce_interval_ms)

        self._update_header_text()
        self._on_style_changed()
        self._update_submit_button_state()

    def set_model(self, model: str) -> None:
        """Set the model name for the input prompt."""
        self._current_model = model
        self._update_header_text()

    def _on_language_changed(self) -> None:
        """Handle language change event."""
        self._update_header_text()

        # Update submit button tooltip
        strings = self._language_manager.strings()
        if self._submit_button is None:
            return

        submit_button = cast(QToolButton, self._submit_button)
        if self._is_streaming:
            submit_button.setToolTip(strings.tooltip_stop_message)

        else:
            submit_button.setToolTip(strings.tooltip_submit_message)

    def set_streaming(self, streaming: bool) -> None:
        """Update the streaming state and header text."""
        was_streaming = self._is_streaming
        self._is_streaming = streaming
        self._update_header_text()
        self._update_submit_button_state()

        # Start or stop border animation based on streaming state
        if streaming and not was_streaming:
            self._start_border_animation()
            return

        if not streaming and was_streaming:
            self._stop_border_animation()

    def trigger_streaming_animation(self) -> None:
        """
        Trigger animation update due to network message received.

        This method implements debouncing - if the debounce timer is not active,
        it triggers an immediate animation update and starts the debounce timer.
        If the debounce timer is already active, it sets a pending flag to
        indicate another message was received during the debounce period.

        When a new message is received, the no-message counter is reset to zero.
        """
        if not self._is_streaming:
            return

        # Reset the no-message counter since we received a message
        self._no_message_counter = 0

        if self._debounce_timer.isActive():
            self._pending_message = True
            return

        # No debounce timer running - trigger immediate update
        self._update_border_animation()
        self._debounce_timer.start()
        self._pending_message = False

    def _on_slow_timer(self) -> None:
        """Handle slow timer timeout - provides regular animation updates."""
        if self._is_streaming:
            self._update_border_animation()

    def _on_debounce_timeout(self) -> None:
        """
        Handle debounce timer timeout.

        If there was a pending message during the debounce period, immediately
        trigger another animation update and restart the debounce timer.

        If there was no pending message, increment the no-message counter.
        If the counter reaches the maximum, disable the debounce timer.
        Otherwise, treat it as if we saw a message (for smooth transition).
        """
        if self._pending_message:
            # There was a message during debounce - trigger update and restart
            self._update_border_animation()
            self._debounce_timer.start()
            self._pending_message = False
            return

        # No message during debounce period
        self._no_message_counter += 1

        if self._no_message_counter >= self._max_no_message_cycles:
            # Reached maximum cycles - stop debounce timer
            # Animation will continue with slow timer only
            self._no_message_counter = 0
            return

        # Continue fast animation for smooth transition
        self._update_border_animation()
        self._debounce_timer.start()

    def _start_border_animation(self) -> None:
        """Start the border fade animation."""
        # Initialize animation state based on current theme
        self._animation_frame = self._animation_steps // 2
        self._fade_direction = 1
        self._pending_message = False
        self._no_message_counter = 0

        # Start the slow timer - this runs continuously
        self._slow_timer.start()
        self._update_border_style()

    def _stop_border_animation(self) -> None:
        """Stop the border animation and restore normal styling."""
        self._slow_timer.stop()
        self._debounce_timer.stop()
        self._animation_frame = 0
        self._pending_message = False
        self._no_message_counter = 0
        self._on_style_changed()  # Restore normal styling

    def _update_border_animation(self) -> None:
        """Update the border animation frame."""
        if not self._is_streaming:
            return

        # Update animation frame with direction
        self._animation_frame += self._fade_direction

        # Reverse direction at the extremes for full cycle (start→mid→start)
        if self._animation_frame >= self._animation_steps:
            self._animation_frame = self._animation_steps - 1
            self._fade_direction = -1

        elif self._animation_frame < 0:
            self._animation_frame = 0
            self._fade_direction = 1

        self._update_border_style()

    def _get_fade_color(self) -> str:
        """
        Calculate the current fade color based on animation frame.

        Returns:
            str: Hex color string for the current animation frame
        """
        # Calculate intensity (0.0 to 1.0)
        intensity = self._animation_frame / (self._animation_steps - 1)

        # Convert to RGB value (0-255)
        rgb_value = int(intensity * 255)

        # Return as hex color
        return f"#{rgb_value:02x}{rgb_value:02x}{rgb_value:02x}"

    def _build_message_frame_styles(self, colors: Dict[str, str]) -> str:
        """Build styles for the main message frame."""
        background_color = self._get_background_color()
        border_color = self._get_border_color()
        if self._is_streaming:
            # Use animated border color if streaming
            border_color = self._get_fade_color()

        border_radius = int(self._style_manager.message_bubble_spacing())

        return f"""
            QFrame#conversationMessage {{
                background-color: {background_color};
                margin: 0;
                border-radius: {border_radius}px;
                border: 2px solid {border_color};
            }}

            QWidget#messageHeader,
            QWidget#sectionsContainer {{
                background-color: {background_color};
                border: none;
                border-radius: 0;
                padding: 0;
                margin: 0;
            }}
        """

    def _update_border_style(self) -> None:
        """Update the border style with the current animation color."""
        if not self._is_streaming:
            return

        self._apply_shared_stylesheet()

    def _get_submit_key_text(self) -> str:
        """Get the appropriate submit key text based on the platform."""
        if sys.platform == "darwin":
            return "⌘J"

        return "Ctrl+J"

    def _on_style_changed(self) -> None:
        """Handle the style changing."""
        super()._on_style_changed()
        self._set_role_style()
        self._update_submit_button_styling()

        # If we're streaming, apply the animated border instead of normal styling
        if self._is_streaming:
            self._update_border_style()

    def _update_submit_button_styling(self) -> None:
        """Update submit button styling and icon."""
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

        submit_button = cast(QToolButton, self._submit_button)

        # Set appropriate icon based on streaming state
        if self._is_streaming:
            icon_name = "stop"

        else:
            icon_name = "submit"

        submit_button.setIcon(QIcon(self._style_manager.scale_icon(
            self._style_manager.get_icon_path(icon_name), icon_base_size
        )))
        submit_button.setIconSize(icon_size)
        submit_button.setStyleSheet(button_style)

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
        self._update_submit_button_state()
        self.modified.emit()

    def _update_submit_button_state(self) -> None:
        """Update submit button enabled state, functionality, and tooltip based on content and streaming status."""
        has_content = bool(self.to_plain_text().strip())
        submit_button = cast(QToolButton, self._submit_button)
        strings = self._language_manager.strings()
        if self._is_streaming:
            # When streaming, show stop button - always enabled
            submit_button.setEnabled(True)
            submit_button.setToolTip(strings.tooltip_stop_message)

            # Disconnect any existing connections and connect to stop
            try:
                submit_button.clicked.disconnect()

            except TypeError:
                pass  # No connections to disconnect

            submit_button.clicked.connect(self._stop_message)

        else:
            # When not streaming, show submit button - enabled if has content
            submit_button.setEnabled(has_content)
            submit_button.setToolTip(strings.tooltip_submit_message)

            # Disconnect any existing connections and connect to submit
            try:
                submit_button.clicked.disconnect()

            except TypeError:
                pass  # No connections to disconnect

            submit_button.clicked.connect(self._submit_message)

        # Update the button styling (which includes icon)
        self._update_submit_button_styling()

    def _submit_message(self) -> None:
        """Submit the current message via button click."""
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
