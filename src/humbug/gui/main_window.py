"""Main window implementation for Humbug application."""

import asyncio
from datetime import datetime
import logging
from typing import List

from PySide6.QtWidgets import (
    QMainWindow, QWidget, QVBoxLayout, QLabel, QApplication, QMenuBar
)
from PySide6.QtCore import QEvent, Qt
from PySide6.QtGui import (
    QKeyEvent, QAction, QKeySequence
)

from humbug.conversation import ConversationHistory, Message, MessageSource, Usage
from humbug.gui.chat_view import ChatView
from humbug.gui.about_dialog import AboutDialog
from humbug.utils import sanitize_input


class MainWindow(QMainWindow):
    """Main window for the Humbug application."""

    def __init__(self, ai_backend, transcript_writer):
        """Initialize the main window."""
        super().__init__()
        self.ai_backend = ai_backend
        self.transcript_writer = transcript_writer
        self.conversation = ConversationHistory()
        self.current_response = ""
        self._current_task = None
        self.debug_id = id(self)
        self.logger = logging.getLogger(f"MainWindow_{self.debug_id}")

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
        self.undo_action.triggered.connect(lambda: self.chat_view.input.undo())

        self.redo_action = QAction("Redo", self)
        self.redo_action.setShortcut(QKeySequence("Ctrl+Shift+Z"))
        self.redo_action.triggered.connect(lambda: self.chat_view.input.redo())

        self.cut_action = QAction("Cut", self)
        self.cut_action.setShortcut(QKeySequence("Ctrl+X"))
        self.cut_action.triggered.connect(self._handle_cut)

        self.copy_action = QAction("Copy", self)
        self.copy_action.setShortcut(QKeySequence("Ctrl+C"))
        self.copy_action.triggered.connect(self._handle_copy)

        self.paste_action = QAction("Paste", self)
        self.paste_action.setShortcut(QKeySequence("Ctrl+V"))
        self.paste_action.triggered.connect(lambda: self.chat_view.input.paste())

    def _handle_cut(self):
        """Handle cut action based on focus."""
        if self.chat_view.input.hasFocus():
            self.chat_view.input.cut()

    def _handle_copy(self):
        """Handle copy action based on focus."""
        if self.chat_view.input.hasFocus():
            self.chat_view.input.copy()
        elif self.chat_view.history.hasFocus():
            self.chat_view.history.copy()

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
        has_input_selection = self.chat_view.input.textCursor().hasSelection()
        has_history_selection = self.chat_view.history.textCursor().hasSelection()
        has_text = bool(self.chat_view.get_input_text())
        can_undo = self.chat_view.input.document().isUndoAvailable()
        can_redo = self.chat_view.input.document().isRedoAvailable()
        input_focused = self.chat_view.input.hasFocus()
        history_focused = self.chat_view.history.hasFocus()

        self.submit_action.setEnabled(has_text)
        self.undo_action.setEnabled(can_undo and input_focused)
        self.redo_action.setEnabled(can_redo and input_focused)
        self.cut_action.setEnabled(has_input_selection and input_focused)
        self.copy_action.setEnabled(
            (input_focused and has_input_selection) or (history_focused and has_history_selection)
        )
        self.paste_action.setEnabled(input_focused)

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

        # Chat view
        self.chat_view = ChatView(self)
        layout.addWidget(self.chat_view)

        # Connect signals for menu state updates
        self.chat_view.input.textChanged.connect(self._update_menu_states)
        self.chat_view.input.selectionChanged.connect(self._update_menu_states)
        self.chat_view.history.selectionChanged.connect(self._update_menu_states)

        # Install event filter to catch focus changes
        self.chat_view.input.installEventFilter(self)
        self.chat_view.history.installEventFilter(self)

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

    def eventFilter(self, obj, event):
        """Handle focus events for menu state updates."""
        if event.type() in (QEvent.FocusIn, QEvent.FocusOut):
            self._update_menu_states()
        return super().eventFilter(obj, event)

    def submit_message(self):
        """Handle message submission."""
        message = sanitize_input(self.chat_view.get_input_text().strip())
        if not message:
            return

        # Clear input area and add the message
        self.chat_view.clear_input()
        self.chat_view.add_message(f"You: {message}", "user")

        # Create and store message
        user_message = Message.create(MessageSource.USER, message)
        self.conversation.add_message(user_message)

        # Write to transcript
        asyncio.create_task(self.transcript_writer.write([user_message.to_transcript_dict()]))

        # Handle commands
        if message.startswith('/'):
            asyncio.create_task(self.handle_command(message[1:]))
            return

        # Start AI response and track the task
        self._current_task = asyncio.create_task(self.process_ai_response(message))

    async def process_ai_response(self, message: str):
        """Process AI response with streaming."""
        try:
            self.current_response = ""
            first_response = True
            self.logger.debug("\n=== Starting new AI response processing ===")

            async for response in self.ai_backend.stream_message(
                message, self.conversation.get_messages_for_context()
            ):
                if response.error:
                    self.logger.debug(f"Received error response: {response.error}")
                    error_msg = f"Error: {response.error['message']}"
                    self.chat_view.add_message(error_msg, "error")

                    error_message = Message.create(
                        MessageSource.SYSTEM,
                        error_msg,
                        error=response.error
                    )
                    self.conversation.add_message(error_message)
                    await self.transcript_writer.write([error_message.to_transcript_dict()])
                    return

                # Update response
                self.current_response = response.content

                # For first chunk, add new message with prefix
                if first_response:
                    self.chat_view.add_message(f"AI: {self.current_response}", "ai")
                    first_response = False
                else:
                    self.chat_view.add_message(f"AI: {self.current_response}", "ai")

                # Update token counts and handle completion
                if response.usage:
                    usage = Usage(
                        prompt_tokens=response.usage.prompt_tokens,
                        completion_tokens=response.usage.completion_tokens,
                        total_tokens=response.usage.total_tokens
                    )
                    ai_message = Message.create(
                        MessageSource.AI,
                        self.current_response,
                        usage=usage
                    )
                    self.conversation.add_message(ai_message)
                    self.update_status()
                    # Write final response to transcript
                    await self.transcript_writer.write([ai_message.to_transcript_dict()])
                    # Mark AI response as complete
                    self.chat_view.finish_ai_response()

        except asyncio.CancelledError:
            self.logger.debug("AI response cancelled")
            # Handle cancellation gracefully
            self.chat_view.finish_ai_response()
            raise

        except Exception as e:
            self.logger.exception("Error processing AI response")
            error_msg = f"Error: {str(e)}"
            self.chat_view.add_message(error_msg, "error")
            error_message = Message.create(
                MessageSource.SYSTEM,
                error_msg,
                error={
                    "code": "process_error",
                    "message": str(e),
                    "details": {"type": type(e).__name__}
                }
            )
            self.conversation.add_message(error_message)
            await self.transcript_writer.write([error_message.to_transcript_dict()])

        finally:
            self.logger.debug("=== Finished AI response processing ===")
            self._current_task = None

    def get_conversation_history(self) -> List[str]:
        """Get conversation history for AI context."""
        return self.conversation.get_messages_for_context()

    async def handle_command(self, command: str):
        """Handle application commands."""
        if command.strip().lower() == "exit":
            QApplication.quit()
            return

        response = Message.create(
            MessageSource.SYSTEM,
            f"Unknown command: {command}"
        )
        self.conversation.add_message(response)
        self.chat_view.add_message(response.content, "system")
        await self.transcript_writer.write([response.to_transcript_dict()])

    def update_status(self):
        """Update the status bar with current token counts."""
        counts = self.conversation.get_token_counts()
        self.status_label.setText(
            f"Input tokens: {counts['input']} | "
            f"Output tokens: {counts['output']}"
        )

    def keyPressEvent(self, event: QKeyEvent):
        """Handle global key events."""
        if event.key() == Qt.Key_Escape:
            self.cancel_current_request()
        else:
            super().keyPressEvent(event)

    def cancel_current_request(self):
        """Cancel the current AI request if one is in progress."""
        if self._current_task and not self._current_task.done():
            self._current_task.cancel()
            self.chat_view.add_message("System: Request cancelled by user", "system")
            self.chat_view.finish_ai_response()
            asyncio.create_task(self.write_cancellation_to_transcript())

    async def write_cancellation_to_transcript(self):
        """Write cancellation message to transcript."""
        cancel_message = Message.create(
            MessageSource.SYSTEM,
            "Request cancelled by user",
            error={
                "code": "cancelled",
                "message": "Request cancelled by user",
                "details": {
                    "cancelled_response": self.current_response if self.current_response else "",
                    "time": datetime.utcnow().isoformat()
                }
            }
        )
        self.conversation.add_message(cancel_message)
        await self.transcript_writer.write([cancel_message.to_transcript_dict()])
