"""Main window implementation with menu support."""

import asyncio
import logging
from typing import List
from datetime import datetime

from PySide6.QtWidgets import (QMainWindow, QWidget, QVBoxLayout, QLabel,
                             QApplication, QMenu, QMenuBar, QDialog, QTextEdit)
from PySide6.QtCore import Qt
from PySide6.QtGui import (QTextCursor, QColor, QKeyEvent, QAction, QKeySequence,
                          QTextCharFormat, QTextDocument, QPainter)

from humbug.gui.about_dialog import AboutDialog


class ProtectedTextEdit(QTextEdit):
    """Text edit widget that protects history from modification while allowing streaming updates."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self._protected_position = 0
        self.debug_id = id(self)
        self.logger = logging.getLogger(f"ProtectedTextEdit_{self.debug_id}")

    def get_user_input(self) -> str:
        """Get the current user input text."""
        cursor = QTextCursor(self.document())
        cursor.setPosition(self._protected_position)
        cursor.movePosition(QTextCursor.End, QTextCursor.KeepAnchor)
        return cursor.selectedText()

    def set_protected_position(self, position: int):
        """Set the position before which text cannot be modified."""
        doc_length = self.document().characterCount()
        self.logger.debug(f"Setting protected position to {position} (doc length: {doc_length})")
        self._protected_position = position


class UnifiedChatView(QWidget):
    """A unified view combining message history and input area."""

    def __init__(self, parent=None):
        """Initialize the unified chat view."""
        super().__init__(parent)
        self.layout = QVBoxLayout(self)
        self.layout.setContentsMargins(0, 0, 0, 0)
        self.layout.setSpacing(0)
        self.debug_id = id(self)
        self.logger = logging.getLogger(f"UnifiedChatView_{self.debug_id}")

        self.text_edit = ProtectedTextEdit()
        self.text_edit.setStyleSheet("""
            QTextEdit {
                background-color: #1e1e1e;
                color: #ffffff;
                border: none;
            }
        """)

        # Initialize document
        self.text_edit.document().setDocumentMargin(10)

        # Track the separation point between history and input
        self._input_start_position = 0
        self._input_format = QTextCharFormat()
        self._input_format.setBackground(QColor("#2d2d2d"))

        # Style map for different message types
        self.STYLE_COLORS = {
            'user': QColor('white'),
            'ai': QColor('yellow'),
            'system': QColor('green'),
            'error': QColor('red')
        }

        self.layout.addWidget(self.text_edit)

        # Track AI response state
        self.current_ai_response_start = None
        self._ai_response_start = None
        self._ai_response_length = 0

        # Initialize with empty input block
        self._ensure_input_block()

    def _ensure_input_block(self):
        """Ensure the input block exists and is properly formatted."""
        cursor = QTextCursor(self.text_edit.document())
        cursor.movePosition(QTextCursor.End)

        # If we're not at the start of a block, add a new block
        if not cursor.atBlockStart():
            cursor.insertBlock()

        # Mark the start of the input area and ensure protected position matches
        self._input_start_position = cursor.position()
        self.text_edit.set_protected_position(self._input_start_position)

        # Apply input area format
        cursor.movePosition(QTextCursor.End, QTextCursor.KeepAnchor)
        cursor.mergeCharFormat(self._input_format)

        cursor.setPosition(self._input_start_position)
        self.text_edit.setTextCursor(cursor)

    def update_ai_response(self, new_content: str):
        """Update the AI response by only inserting new content."""
        self.logger.debug("\n=== Starting update_ai_response ===")
        self.logger.debug(f"Updating with new content: {repr(new_content)}")

        # Save cursor state relative to user input
        current_input = self.text_edit.get_user_input()
        cursor = self.text_edit.textCursor()
        relative_cursor = cursor.position() - self.text_edit._protected_position

        try:
            # Only initialize if we don't have a current response
            if self._ai_response_start is None:
                cursor = QTextCursor(self.text_edit.document())
                cursor.setPosition(self.text_edit._protected_position)

                if self.text_edit._protected_position > 0:
                    cursor.insertBlock()
                self._ai_response_start = cursor.position()
                self._ai_response_length = 0
                self.logger.debug(f"Initialized new AI response at position {self._ai_response_start}")

            # Replace existing response content
            cursor = QTextCursor(self.text_edit.document())
            cursor.setPosition(self._ai_response_start)

            # Select and replace existing content
            cursor.movePosition(QTextCursor.Right, QTextCursor.KeepAnchor, self._ai_response_length)

            # Insert new content
            format = QTextCharFormat()
            format.setForeground(QColor('yellow'))
            cursor.insertText(new_content, format)

            # Update tracking
            self._ai_response_length = len(new_content)
            self.text_edit._protected_position = self._ai_response_start + self._ai_response_length

            # Reposition user input
            cursor = QTextCursor(self.text_edit.document())
            cursor.setPosition(self.text_edit._protected_position)

            # Clear everything after protected position
            cursor.movePosition(QTextCursor.End, QTextCursor.KeepAnchor)
            cursor.removeSelectedText()

            # Ensure new block for user input
            if not cursor.atBlockStart():
                cursor.insertBlock()
                self.text_edit._protected_position = cursor.position()

            # Restore user input
            if current_input:
                format = QTextCharFormat()
                format.setBackground(QColor("#2d2d2d"))
                cursor.insertText(current_input, format)

                # Restore cursor position relative to input
                new_cursor_pos = self.text_edit._protected_position + max(0, min(relative_cursor, len(current_input)))
                cursor.setPosition(new_cursor_pos)
                self.text_edit.setTextCursor(cursor)
                self.text_edit.ensureCursorVisible()

        except Exception as e:
            self.logger.error(f"Error in update_ai_response: {e}")
            import traceback
            self.logger.error(traceback.format_exc())

    def add_message(self, message: str, style: str):
        """Add a message to the history area."""
        self.log_operation(f"add_message - start ({style})")

        self.logger.debug(f"Document state before add_message:")
        self.logger.debug(f"  Document length: {self.text_edit.document().characterCount()}")
        self.logger.debug(f"  Protected position: {self.text_edit._protected_position}")
        self.logger.debug(f"  Input start position: {self._input_start_position}")

        if style == 'ai':
            # Update existing AI response
            self.update_ai_response(message[4:])  # Skip 'AI: ' prefix
        else:
            # For non-AI messages, add at the protected position
            cursor = QTextCursor(self.text_edit.document())
            cursor.setPosition(self.text_edit._protected_position)

            if self.text_edit._protected_position > 0:
                cursor.insertBlock()

            format = QTextCharFormat()
            format.setForeground(self.STYLE_COLORS.get(style, QColor('white')))
            cursor.insertText(message, format)
            cursor.insertBlock()

            # Update protected position to end of inserted message
            self.text_edit.set_protected_position(cursor.position())

            # Update input start position to match
            self._input_start_position = self.text_edit._protected_position

            # Clear any content after protected position
#            cursor.movePosition(QTextCursor.End, QTextCursor.KeepAnchor)
#            cursor.removeSelectedText()

        self.logger.debug(f"Document state after add_message:")
        self.logger.debug(f"  Document length: {self.text_edit.document().characterCount()}")
        self.logger.debug(f"  Protected position: {self.text_edit._protected_position}")
        self.logger.debug(f"  Input start position: {self._input_start_position}")

        self.log_operation(f"add_message - end ({style})")

    def get_input_text(self) -> str:
        """Get the current input text."""
        return self.text_edit.get_user_input()

    def set_input_text(self, text: str):
        """Set the input text."""
        cursor = QTextCursor(self.text_edit.document())
        cursor.setPosition(self._input_start_position)
        cursor.movePosition(QTextCursor.End, QTextCursor.KeepAnchor)
        cursor.insertText(text, self._input_format)
        self.text_edit.setTextCursor(cursor)

    def clear_input(self):
        """Clear the input area."""
        self.set_input_text("")

    def log_operation(self, operation: str):
        """Log an operation for debugging."""
        self.logger.debug(f"Operation: {operation}")
        self.logger.debug(f"  Input start position: {self._input_start_position}")
        self.logger.debug(f"  AI response start: {self.current_ai_response_start}")

    def _finish_ai_response(self):
        """Mark the current AI response as complete."""
        if self._ai_response_start is not None:
            # Calculate the end position of the response
            response_end = self._ai_response_start + self._ai_response_length

            # Move to the end of the response
            cursor = QTextCursor(self.text_edit.document())
            cursor.setPosition(response_end)

            # Ensure we end with a new block
            if not cursor.atBlockStart():
                cursor.insertBlock()

            # Set the protected position to the new block start
            self._protected_position = cursor.position()

            # Clear everything after the protected position
#            cursor.movePosition(QTextCursor.End, QTextCursor.KeepAnchor)
#            cursor.removeSelectedText()

            self.logger.debug(f"Finishing AI response:")
            self.logger.debug(f"  Response start: {self._ai_response_start}")
            self.logger.debug(f"  Response length: {self._ai_response_length}")
            self.logger.debug(f"  Response end: {response_end}")
            self.logger.debug(f"  New protected position: {self._protected_position}")
            self.logger.debug(f"  Document length: {self.text_edit.document().characterCount()}")

        # Reset AI response tracking
        self._ai_response_start = None
        self._ai_response_length = 0

    def finish_ai_response(self):
        """Mark the current AI response as complete."""
        self._finish_ai_response()
        self.current_ai_response_start = None


class HumbugMainWindow(QMainWindow):
    """Main window for the Humbug application."""

    def __init__(self, ai_backend, transcript_writer):
        super().__init__()
        self.ai_backend = ai_backend
        self.transcript_writer = transcript_writer
        self.current_response = ""
        self.token_counts = {"input": 0, "output": 0}
        self._current_task = None
        self.debug_id = id(self)
        self.logger = logging.getLogger(f"HumbugMainWindow_{self.debug_id}")

        # Create actions first
        self._create_actions()
        # Create menus using the actions
        self._create_menus()
        # Then set up the rest of the UI
        self.setup_ui()

    def _create_actions(self):
        """Create all menu actions."""
        # Humbug menu actions
        self.about_action = QAction("About Humbug", self)
        self.about_action.triggered.connect(self._show_about_dialog)

        self.quit_action = QAction("Quit Humbug", self)
        self.quit_action.setShortcut(QKeySequence("Ctrl+Q"))
        self.quit_action.triggered.connect(self.close)

        # Edit menu actions
        self.submit_action = QAction("Submit", self)
        self.submit_action.setShortcut(QKeySequence("Ctrl+J"))
        self.submit_action.triggered.connect(self.submit_message)

        self.undo_action = QAction("Undo", self)
        self.undo_action.setShortcut(QKeySequence("Ctrl+Z"))
        self.undo_action.triggered.connect(lambda: self.chat_view.text_edit.undo())

        self.redo_action = QAction("Redo", self)
        self.redo_action.setShortcut(QKeySequence("Ctrl+Shift+Z"))
        self.redo_action.triggered.connect(lambda: self.chat_view.text_edit.redo())

        self.cut_action = QAction("Cut", self)
        self.cut_action.setShortcut(QKeySequence("Ctrl+X"))
        self.cut_action.triggered.connect(lambda: self.chat_view.text_edit.cut())

        self.copy_action = QAction("Copy", self)
        self.copy_action.setShortcut(QKeySequence("Ctrl+C"))
        self.copy_action.triggered.connect(lambda: self.chat_view.text_edit.copy())

        self.paste_action = QAction("Paste", self)
        self.paste_action.setShortcut(QKeySequence("Ctrl+V"))
        self.paste_action.triggered.connect(lambda: self.chat_view.text_edit.paste())

    def _create_menus(self):
        """Create the menu bar and all menus."""
        self._menu_bar = QMenuBar(self)
        self.setMenuBar(self._menu_bar)

        # Humbug menu
        humbug_menu = self._menu_bar.addMenu("&Humbug")
        humbug_menu.addAction(self.about_action)
        humbug_menu.addSeparator()
        humbug_menu.addAction(self.quit_action)

        # Edit menu
        edit_menu = self._menu_bar.addMenu("&Edit")
        edit_menu.addAction(self.submit_action)
        edit_menu.addSeparator()
        edit_menu.addAction(self.undo_action)
        edit_menu.addAction(self.redo_action)
        edit_menu.addSeparator()
        edit_menu.addAction(self.cut_action)
        edit_menu.addAction(self.copy_action)
        edit_menu.addAction(self.paste_action)

    def _show_about_dialog(self):
        """Show the About dialog."""
        dialog = AboutDialog(self)
        dialog.exec()

    def _update_menu_states(self):
        """Update enabled/disabled state of menu items."""
        cursor = self.chat_view.text_edit.textCursor()
        has_selection = cursor.hasSelection()
        has_text = bool(self.chat_view.get_input_text())
        can_undo = self.chat_view.text_edit.document().isUndoAvailable()
        can_redo = self.chat_view.text_edit.document().isRedoAvailable()

        self.submit_action.setEnabled(has_text)
        self.undo_action.setEnabled(can_undo)
        self.redo_action.setEnabled(can_redo)
        self.cut_action.setEnabled(has_selection)
        self.copy_action.setEnabled(has_selection)

    def setup_ui(self):
        """Set up the user interface."""
        self.setWindowTitle("Humbug")
        self.setMinimumSize(800, 600)

        # Main widget and layout
        main_widget = QWidget()
        self.setCentralWidget(main_widget)
        layout = QVBoxLayout(main_widget)
        layout.setSpacing(0)
        layout.setContentsMargins(0, 0, 0, 0)

        # Unified chat view
        self.chat_view = UnifiedChatView(self)
        layout.addWidget(self.chat_view)

        # Connect text changed signal
        self.chat_view.text_edit.textChanged.connect(self._update_menu_states)

        # Status bar
        self.status_label = QLabel("Input tokens: 0 | Output tokens: 0")
        self.statusBar().addWidget(self.status_label)

        # Set dark theme
        self.setStyleSheet("""
            QMainWindow {
                background-color: #1e1e1e;
                color: #ffffff;
            }
            QMenuBar {
                background-color: #2d2d2d;
                color: #ffffff;
            }
            QMenuBar::item {
                background-color: transparent;
            }
            QMenuBar::item:selected {
                background-color: #3d3d3d;
            }
            QMenu {
                background-color: #2d2d2d;
                border: 1px solid #3d3d3d;
            }
            QMenu::item:selected {
                background-color: #3d3d3d
            }
            QStatusBar {
                background-color: #2d2d2d;
            }
        """)

    async def write_to_transcript(self, messages: List[dict]):
        """Write messages to transcript with error handling."""
        try:
            await self.transcript_writer.write(messages)
        except Exception as e:
            self.chat_view.add_message(
                f"[ERROR] Failed to write to transcript: {str(e)}", "error")

    def cancel_current_request(self):
        """Cancel the current AI request if one is in progress."""
        if self._current_task and not self._current_task.done():
            self._current_task.cancel()
            self.chat_view.add_message("System: Request cancelled by user", "system")
            self.chat_view.finish_ai_response()
            asyncio.create_task(self.write_cancellation_to_transcript())

    async def write_cancellation_to_transcript(self):
        """Write cancellation message to transcript."""
        await self.write_to_transcript([{
            "type": "system_message",
            "content": "Request cancelled by user",
            "error": {
                "code": "cancelled",
                "message": "Request cancelled by user",
                "details": {
                    "cancelled_response": self.current_response if self.current_response else "",
                    "time": datetime.utcnow().isoformat()
                }
            }
        }])

    def submit_message(self):
        """Handle message submission."""
        message = self.chat_view.get_input_text().strip()
        if not message:
            return

        # Clear input area
        self.chat_view.clear_input()

        # Handle commands
        if message.startswith('/'):
            asyncio.create_task(self.handle_command(message[1:]))
            return

        # Add user message to history and transcript
        self.chat_view.add_message(f"You: {message}", "user")
        asyncio.create_task(self.write_to_transcript([{
            "type": "user_message",
            "content": message,
            "timestamp": datetime.utcnow().isoformat()
        }]))

        # Start AI response and track the task
        self._current_task = asyncio.create_task(self.process_ai_response(message))

    async def process_ai_response(self, message: str):
        """Process AI response with streaming."""
        try:
            self.current_response = ""
            first_response = True
            self.logger.debug("\n=== Starting new AI response processing ===")

            async for response in self.ai_backend.stream_message(message, self.get_conversation_history()):
                if response.error:
                    self.logger.debug(f"Received error response: {response.error}")
                    error_msg = f"Error: {response.error['message']}"
                    self.chat_view.add_message(error_msg, "error")
                    await self.write_to_transcript([{
                        "type": "error",
                        "content": error_msg,
                        "error": response.error,
                        "timestamp": datetime.utcnow().isoformat()
                    }])
                    # Make sure to reset AI response state on error
                    self.chat_view.text_edit._ai_response_start = None
                    self.chat_view.text_edit._ai_response_length = 0
                    return

                # Update response - this is the complete response up to this point
                self.current_response = response.content
                self.logger.debug(f"Received chunk, first_response={first_response}")
                self.logger.debug(f"Current response content: {repr(self.current_response)}")
                self.logger.debug(f"AI response start: {self.chat_view._ai_response_start}")
                self.logger.debug(f"AI response length: {self.chat_view._ai_response_length}")

                # For first chunk, add new message with prefix
                if first_response:
                    self.logger.debug("Processing first response chunk")
                    self.chat_view.add_message(f"AI: {self.current_response}", "ai")
                    first_response = False
                else:
                    # For subsequent chunks, update the existing message content
                    self.logger.debug("Processing subsequent response chunk")
                    # Note: update_ai_response expects content without the "AI: " prefix
                    self.chat_view.update_ai_response(self.current_response)

                # Update token counts and handle completion
                if response.usage:
                    self.logger.debug(f"Received usage info: {response.usage}")
                    self.token_counts["input"] += response.usage.prompt_tokens
                    self.token_counts["output"] += response.usage.completion_tokens
                    self.update_status()
                    # Write final response to transcript
                    await self.write_to_transcript([{
                        "type": "ai_response",
                        "content": self.current_response,
                        "usage": response.usage.to_dict(),
                        "timestamp": datetime.utcnow().isoformat()
                    }])
                    # Mark AI response as complete and reset state
                    self.logger.debug("Completing AI response with usage info")
                    self.chat_view.finish_ai_response()
                    self.chat_view.text_edit._ai_response_start = None
                    self.chat_view.text_edit._ai_response_length = 0

        except asyncio.CancelledError:
            self.logger.debug("AI response cancelled")
            # Handle cancellation gracefully
            self.chat_view.finish_ai_response()
            self.chat_view.text_edit._ai_response_start = None
            self.chat_view.text_edit._ai_response_length = 0
            raise

        except Exception as e:
            self.logger.exception("Error processing AI response")
            error_msg = f"Error: {str(e)}"
            self.chat_view.add_message(error_msg, "error")
            await self.write_to_transcript([{
                "type": "error",
                "content": error_msg,
                "error": {
                    "code": "process_error",
                    "message": str(e),
                    "details": {"type": type(e).__name__}
                },
                "timestamp": datetime.utcnow().isoformat()
            }])
            # Make sure to reset AI response state on error
            self.chat_view.text_edit._ai_response_start = None
            self.chat_view.text_edit._ai_response_length = 0

        finally:
            self.logger.debug("=== Finished AI response processing ===")
            self._current_task = None

    def get_conversation_history(self) -> List[str]:
        """Extract conversation history from display."""
        # This is a simplified version - would need to be enhanced
        return []

    async def handle_command(self, command: str):
        """Handle application commands."""
        if command.strip().lower() == "exit":
            QApplication.quit()
            return

        self.chat_view.add_message(f"Unknown command: {command}", "system")

    def update_status(self):
        """Update the status bar with current token counts."""
        self.status_label.setText(
            f"Input tokens: {self.token_counts['input']} | "
            f"Output tokens: {self.token_counts['output']}"
        )
