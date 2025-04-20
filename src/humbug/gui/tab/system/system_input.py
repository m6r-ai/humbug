"""Input widget that matches history message styling and acts as a command line."""

import sys
from typing import Dict, List, cast

from PySide6.QtCore import Signal, Qt, QMimeData, QRect, QEvent, QObject
from PySide6.QtGui import QKeyEvent, QTextCursor, QTextDocument
from PySide6.QtWidgets import QWidget

from humbug.gui.tab.system.system_command_completion_result import SystemCommandCompletionResult
from humbug.gui.tab.system.system_message import SystemMessage
from humbug.language.language_manager import LanguageManager
from humbug.mindspace.system.system_message_source import SystemMessageSource


class SystemInput(SystemMessage):
    """Widget for system message input that matches history styling and behaves like a terminal."""

    # Forward text cursor signals from the input area
    cursorPositionChanged = Signal()
    pageScrollRequested = Signal()

    # New signals
    command_submitted = Signal(str)
    tab_completion_requested = Signal(str, bool, bool, int)  # text, is_continuation, move_forward, cursor_position

    def __init__(self, parent: QWidget | None = None) -> None:
        """Initialize the system input widget."""
        super().__init__(parent, is_input=True)

        # Command history tracking
        self._command_history: List[str] = []
        self._history_position: int = -1
        self._current_command: str = ""

        self._tab_completion_active: bool = False

        # Connect text cursor signals
        self._text_area.cursorPositionChanged.connect(self.cursorPositionChanged)
        self._text_area.pageScrollRequested.connect(self.pageScrollRequested)

        # Install event filter on text area to intercept key events
        self._text_area.installEventFilter(self)

        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._handle_language_changed)

        self._message_source = SystemMessageSource.USER  # Set default source for styling
        self._update_header_text()

    def eventFilter(self, obj: QObject, event: QEvent) -> bool:
        """
        Filter events to intercept key presses in the text area.

        Args:
            obj: Object that received the event
            event: The event

        Returns:
            True if the event was handled, False to pass to the text area
        """
        if obj == self._text_area and event.type() == QEvent.Type.KeyPress:
            # Cast to QKeyEvent
            key_event = cast(QKeyEvent, event)

            # Handle Tab key for command completion
            if key_event.key() == Qt.Key.Key_Tab:
                # Emit signal requesting tab completion
                current_text = self._text_area.toPlainText()
                cursor = self._text_area.textCursor()
                self.tab_completion_requested.emit(current_text, self._tab_completion_active, True, cursor.position())
                self._tab_completion_active = True
                return True

            if key_event.key() == Qt.Key.Key_Backtab:
                # Handle Shift+Tab for reverse tab completion
                current_text = self._text_area.toPlainText()
                cursor = self._text_area.textCursor()
                self.tab_completion_requested.emit(current_text, self._tab_completion_active, False, cursor.position())
                self._tab_completion_active = True
                return True

            if key_event.key() == Qt.Key.Key_Shift:
                # Ignore Shift key press
                return True

            self._tab_completion_active = False

            # Handle Enter key for command submission
            if key_event.key() == Qt.Key.Key_Return and not key_event.modifiers() & Qt.KeyboardModifier.ShiftModifier:
                text = self._text_area.toPlainText().strip()
                if text:
                    self.command_submitted.emit(text)
                    self._add_to_history(text)
                    self.clear()

                return True

            # Handle Up key for history navigation
            if key_event.key() == Qt.Key.Key_Up:
                self._navigate_history_up()
                return True

            # Handle Down key for history navigation
            if key_event.key() == Qt.Key.Key_Down:
                self._navigate_history_down()
                return True

            # Handle PageUp key to move to start of input
            if key_event.key() == Qt.Key.Key_PageUp:
                cursor = self._text_area.textCursor()
                cursor.movePosition(QTextCursor.MoveOperation.Start)
                self._text_area.setTextCursor(cursor)
                return True

            # Handle PageDown key to move to end of input
            if key_event.key() == Qt.Key.Key_PageDown:
                cursor = self._text_area.textCursor()
                cursor.movePosition(QTextCursor.MoveOperation.End)
                self._text_area.setTextCursor(cursor)
                return True

        # Let the event continue to the target
        return super().eventFilter(obj, event)

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

        # Set command prompt with submit key hint
        submit_key = self._get_submit_key_text()

        # Check if command_prompt exists in strings, otherwise fall back to input_prompt
        self._role_label.setText(strings.command_prompt.format(key=submit_key))
        self._set_role_style()

    def _insert_from_mime_data(self, source: QMimeData) -> None:
        """Override default paste behavior to insert only plain text."""
        if source.hasText():
            cursor = self._text_area.textCursor()
            cursor.insertText(source.text())

    def keyPressEvent(self, event: QKeyEvent) -> None:
        """
        Handle special key events for terminal-like behavior.

        This only handles keys that bubble up to the SystemInput,
        most keys are handled by the eventFilter.
        """
        # Handle Ctrl+J or Cmd+J for command submission (original behavior)
        if event.key() == Qt.Key.Key_J and event.modifiers() & Qt.KeyboardModifier.ControlModifier:
            text = self._text_area.toPlainText().strip()
            if text:
                self.command_submitted.emit(text)
                self._add_to_history(text)
                self.clear()
                return

        super().keyPressEvent(event)

    def _navigate_history_up(self) -> None:
        """Navigate up through command history."""
        if not self._command_history:
            return

        # Save current command if we're at the end of history
        if self._history_position == -1:
            self._current_command = self._text_area.toPlainText()

        # Move up in history if possible
        if self._history_position < len(self._command_history) - 1:
            self._history_position += 1
            self._text_area.setPlainText(self._command_history[self._history_position])
            # Move cursor to end of text
            cursor = self._text_area.textCursor()
            cursor.movePosition(QTextCursor.MoveOperation.End)
            self._text_area.setTextCursor(cursor)

    def _navigate_history_down(self) -> None:
        """Navigate down through command history."""
        if self._history_position > 0:
            # Move down in history
            self._history_position -= 1
            self._text_area.setPlainText(self._command_history[self._history_position])
        elif self._history_position == 0:
            # Return to current command being edited
            self._history_position = -1
            self._text_area.setPlainText(self._current_command)

        # Move cursor to end of text
        cursor = self._text_area.textCursor()
        cursor.movePosition(QTextCursor.MoveOperation.End)
        self._text_area.setTextCursor(cursor)

    def _add_to_history(self, command: str) -> None:
        """Add command to history, avoiding duplicates at the front."""
        # Don't add empty commands
        if not command.strip():
            return

        # Remove command if it already exists to avoid duplicates
        if command in self._command_history:
            self._command_history.remove(command)

        # Add at the beginning (most recent first)
        self._command_history.insert(0, command)

        # Limit history size (e.g., to 50 commands)
        if len(self._command_history) > 50:
            self._command_history = self._command_history[:50]

        # Reset history position
        self._history_position = -1
        self._current_command = ""

    def set_command_history(self, commands: List[str]) -> None:
        """
        Set the command history from a list of commands.

        Args:
            commands: List of command strings, newest first
        """
        self._command_history = commands.copy()
        self._history_position = -1
        self._current_command = ""

    def get_command_history(self) -> List[str]:
        """
        Get the current command history.

        Returns:
            List of command strings, newest first
        """
        return self._command_history.copy()

    def apply_completion(self, result: SystemCommandCompletionResult) -> None:
        """
        Apply a tab completion to the input area.

        Args:
            result: The completion result to apply
        """
        if not result.success or result.replacement is None:
            return

        # Get the current text
        current_text = self._text_area.toPlainText()

        # Create the new text by replacing only the specified part
        new_text = current_text[:result.start_pos] + result.replacement + current_text[result.end_pos:]

        # Calculate where the cursor should end up
        new_cursor_pos = result.start_pos + len(result.replacement)

        # Apply the new text
        self._text_area.setPlainText(new_text)

        cursor = self._text_area.textCursor()

        # Add space if requested
        if result.add_space:
            cursor.setPosition(new_cursor_pos)
            cursor.movePosition(QTextCursor.MoveOperation.NextCharacter, QTextCursor.MoveMode.KeepAnchor)
            selected_char = cursor.selectedText()
            if selected_char != ' ':
                cursor.setPosition(new_cursor_pos)
                cursor.insertText(' ')

            new_cursor_pos += 1

        # Set the cursor to the proper position
        cursor.setPosition(new_cursor_pos)
        self._text_area.setTextCursor(cursor)

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
