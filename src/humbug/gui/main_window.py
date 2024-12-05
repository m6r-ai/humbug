"""Main window implementation for Humbug application."""

import asyncio
from datetime import datetime
import logging
from typing import Dict, List
import uuid

from PySide6.QtWidgets import (
    QMainWindow, QDialog, QWidget, QVBoxLayout, QApplication, QMenuBar
)
from PySide6.QtCore import QEvent, Qt
from PySide6.QtGui import (
    QKeyEvent, QAction, QKeySequence
)

from humbug.conversation import Message, MessageSource, Usage
from humbug.gui.tab_manager import TabManager
from humbug.gui.about_dialog import AboutDialog
from humbug.gui.settings_dialog import SettingsDialog
from humbug.utils import sanitize_input


class MainWindow(QMainWindow):
    """Main window for the Humbug application."""

    def __init__(self, ai_backend, transcript_writer):
        """Initialize the main window."""
        super().__init__()
        self.ai_backend = ai_backend
        self.transcript_writer = transcript_writer
        self.conversation_count = 0
        self.chat_views = {}  # conversation_id -> ChatView
        self._current_tasks: Dict[str, List[asyncio.Task]] = {}
        self.logger = logging.getLogger("MainWindow")

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

        # File menu actions
        self.new_conv_action = QAction("New Conversation", self)
        self.new_conv_action.triggered.connect(self.create_conversation_tab)

        self.close_conv_action = QAction("Close Conversation", self)
        self.close_conv_action.triggered.connect(self._close_current_conversation)

        # Edit menu actions
        self.submit_action = QAction("Submit", self)
        self.submit_action.setShortcut(QKeySequence("Ctrl+J"))
        self.submit_action.triggered.connect(self.submit_message)

        self.undo_action = QAction("Undo", self)
        self.undo_action.setShortcut(QKeySequence("Ctrl+Z"))
        self.undo_action.triggered.connect(lambda: self.current_chat_view.input.undo())

        self.redo_action = QAction("Redo", self)
        self.redo_action.setShortcut(QKeySequence("Ctrl+Shift+Z"))
        self.redo_action.triggered.connect(lambda: self.current_chat_view.input.redo())

        self.cut_action = QAction("Cut", self)
        self.cut_action.setShortcut(QKeySequence("Ctrl+X"))
        self.cut_action.triggered.connect(self._handle_cut)

        self.copy_action = QAction("Copy", self)
        self.copy_action.setShortcut(QKeySequence("Ctrl+C"))
        self.copy_action.triggered.connect(self._handle_copy)

        self.paste_action = QAction("Paste", self)
        self.paste_action.setShortcut(QKeySequence("Ctrl+V"))
        self.paste_action.triggered.connect(lambda: self.current_chat_view.input.paste())

        self.settings_action = QAction("Conversation Settings", self)
        self.settings_action.setShortcut(QKeySequence("Ctrl+,"))
        self.settings_action.triggered.connect(self._show_settings_dialog)

    def _handle_cut(self):
        """Handle cut action based on focus."""
        chat_view = self.current_chat_view
        if not chat_view:
            return

        if chat_view.input.hasFocus():
            chat_view.input.cut()

    def _handle_copy(self):
        """Handle copy action based on focus."""
        chat_view = self.current_chat_view
        if not chat_view:
            return

        if chat_view.input.hasFocus():
            chat_view.input.copy()
        elif chat_view.history.hasFocus():
            chat_view.history.copy()

    def _create_menus(self):
        """Create the menu bar and all menus."""
        self._menu_bar = QMenuBar(self)
        self.setMenuBar(self._menu_bar)

        # Humbug menu
        humbug_menu = self._menu_bar.addMenu("&Humbug")
        humbug_menu.addAction(self.about_action)
        humbug_menu.addSeparator()
        humbug_menu.addAction(self.quit_action)

        # File menu
        file_menu = self._menu_bar.addMenu("&File")
        file_menu.addAction(self.new_conv_action)
        file_menu.addAction(self.close_conv_action)

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
        edit_menu.addSeparator()
        edit_menu.addAction(self.settings_action)

    def _show_about_dialog(self):
        """Show the About dialog."""
        dialog = AboutDialog(self)
        dialog.exec()

    def _update_menu_states(self):
        """Update enabled/disabled state of menu items."""
        chat_view = self.current_chat_view
        if not chat_view:
            # Disable all editing actions if no chat view is available
            self.submit_action.setEnabled(False)
            self.undo_action.setEnabled(False)
            self.redo_action.setEnabled(False)
            self.cut_action.setEnabled(False)
            self.copy_action.setEnabled(False)
            self.paste_action.setEnabled(False)
            self.close_conv_action.setEnabled(False)
            return

        has_input_selection = chat_view.input.textCursor().hasSelection()
        has_history_selection = chat_view.history.textCursor().hasSelection()
        has_text = bool(chat_view.get_input_text())
        can_undo = chat_view.input.document().isUndoAvailable()
        can_redo = chat_view.input.document().isRedoAvailable()
        input_focused = chat_view.input.hasFocus()
        history_focused = chat_view.history.hasFocus()

        self.submit_action.setEnabled(has_text)
        self.undo_action.setEnabled(can_undo and input_focused)
        self.redo_action.setEnabled(can_redo and input_focused)
        self.cut_action.setEnabled(has_input_selection and input_focused)
        self.copy_action.setEnabled(
            (input_focused and has_input_selection) or (history_focused and has_history_selection)
        )
        self.paste_action.setEnabled(input_focused)
        self.close_conv_action.setEnabled(True)
        self.settings_action.setEnabled(chat_view is not None)

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

        # Create tab manager
        self.tab_manager = TabManager(self)
        layout.addWidget(self.tab_manager)

        # Create initial conversation tab
        self.create_conversation_tab()

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
        """)

    def create_conversation_tab(self) -> str:
        """Create a new conversation tab and return its ID."""
        self.conversation_count += 1
        conversation_id = str(uuid.uuid4())
        chat_view = self.tab_manager.create_conversation(
            conversation_id,
            f"Conv {self.conversation_count}"
        )
        self.chat_views[conversation_id] = chat_view
        return conversation_id

    def _close_current_conversation(self):
        """Close the current conversation tab."""
        chat_view = self.current_chat_view
        if chat_view:
            self.tab_manager._handle_conversation_close(chat_view.conversation_id)

    def eventFilter(self, obj, event):
        """Handle focus events for menu state updates."""
        if event.type() in (QEvent.FocusIn, QEvent.FocusOut):
            self._update_menu_states()
        return super().eventFilter(obj, event)

    @property
    def current_chat_view(self):
        """Get the currently active chat view."""
        return self.tab_manager.get_current_chat()

    def submit_message(self):
        """Handle message submission."""
        chat_view = self.current_chat_view
        if not chat_view:
            return

        message = sanitize_input(chat_view.get_input_text().strip())
        if not message:
            return

        # Clear input area and add the message
        chat_view.clear_input()
        conversation_id = chat_view.conversation_id
        chat_view.add_message(f"You: {message}", "user")

        # Create and store message
        user_message = Message.create(
            conversation_id,
            MessageSource.USER,
            message
        )
        chat_view.conversation.add_message(user_message)

        # Write to transcript
        asyncio.create_task(
            self.transcript_writer.write([user_message.to_transcript_dict()])
        )

        # Handle commands
        if message.startswith('/'):
            asyncio.create_task(self.handle_command(message[1:], conversation_id))
            return

        # Start AI response and track the task
        task = asyncio.create_task(
            self.process_ai_response(message, conversation_id)
        )

        if conversation_id not in self._current_tasks:
            self._current_tasks[conversation_id] = []
        self._current_tasks[conversation_id].append(task)

        def task_done_callback(task):
            if conversation_id in self._current_tasks:
                try:
                    self._current_tasks[conversation_id].remove(task)
                except ValueError:
                    pass  # Task already removed

        task.add_done_callback(task_done_callback)

    def _show_settings_dialog(self):
        """Show the conversation settings dialog."""
        chat_view = self.current_chat_view
        if not chat_view:
            return

        dialog = SettingsDialog(self)
        dialog.set_settings(chat_view.get_settings())

        if dialog.exec() == QDialog.Accepted:
            new_settings = dialog.get_settings()
            chat_view.update_settings(new_settings)
            # Update AI backend settings for this conversation
            self.ai_backend.update_conversation_settings(
                chat_view.conversation_id,
                new_settings
            )

    async def process_ai_response(self, message: str, conversation_id: str):
        """Process AI response with streaming."""
        chat_view = self.chat_views.get(conversation_id)
        if not chat_view:
            self.logger.error(f"No chat view found for conversation {conversation_id}")
            return

        try:
            self.logger.debug(f"\n=== Starting new AI response for conv {conversation_id} ===")
            current_response = ""
            first_response = True

            async for response in self.ai_backend.stream_message(
                message,
                chat_view.conversation.get_messages_for_context(),
                conversation_id
            ):
                if response.error:
                    self.logger.debug(f"Received error response: {response.error}")
                    error_msg = f"Error: {response.error['message']}"
                    chat_view.add_message(error_msg, "error")

                    error_message = Message.create(
                        conversation_id,
                        MessageSource.SYSTEM,
                        error_msg,
                        error=response.error
                    )
                    chat_view.conversation.add_message(error_message)
                    await self.transcript_writer.write([error_message.to_transcript_dict()])

                    if response.error['code'] not in ['network_error', 'timeout']:
                        return
                    continue

                # Update response
                current_response = response.content

                # For first chunk, add new message with prefix
                if first_response:
                    chat_view.add_message(f"AI: {current_response}", "ai")
                    first_response = False
                else:
                    chat_view.add_message(f"AI: {current_response}", "ai")

                # Update token counts and handle completion
                if response.usage:
                    usage = Usage(
                        prompt_tokens=response.usage.prompt_tokens,
                        completion_tokens=response.usage.completion_tokens,
                        total_tokens=response.usage.total_tokens
                    )
                    ai_message = Message.create(
                        conversation_id,
                        MessageSource.AI,
                        current_response,
                        usage=usage
                    )
                    chat_view.conversation.add_message(ai_message)
                    self.update_status(chat_view)
                    # Write final response to transcript
                    await self.transcript_writer.write([ai_message.to_transcript_dict()])
                    # Mark AI response as complete
                    chat_view.finish_ai_response()

        except asyncio.CancelledError:
            self.logger.debug(f"AI response cancelled for conv {conversation_id}")
            if chat_view:
                chat_view.finish_ai_response()
            return

        except Exception as e:
            self.logger.exception(
                f"Error processing AI response for conv {conversation_id}"
            )
            if chat_view:
                error_msg = f"Error: {str(e)}"
                chat_view.add_message(error_msg, "error")
                error_message = Message.create(
                    conversation_id,
                    MessageSource.SYSTEM,
                    error_msg,
                    error={
                        "code": "process_error",
                        "message": str(e),
                        "details": {"type": type(e).__name__}
                    }
                )
                chat_view.conversation.add_message(error_message)
                await self.transcript_writer.write([error_message.to_transcript_dict()])

        finally:
            self.logger.debug(
                f"=== Finished AI response for conv {conversation_id} ==="
            )

    async def handle_command(self, command: str, conversation_id: str):
        """Handle application commands."""
        chat_view = self.chat_views.get(conversation_id)
        if not chat_view:
            return

        if command.strip().lower() == "exit":
            QApplication.quit()
            return

        response = Message.create(
            conversation_id,
            MessageSource.SYSTEM,
            f"Unknown command: {command}"
        )
        chat_view.conversation.add_message(response)
        chat_view.add_message(response.content, "system")
        await self.transcript_writer.write([response.to_transcript_dict()])

    def update_status(self, chat_view):
        """Update the status bar with current token counts."""
        counts = chat_view.conversation.get_token_counts()
        chat_view.update_status(counts['input'], counts['output'])

    def keyPressEvent(self, event: QKeyEvent):
        """Handle global key events."""
        if event.key() == Qt.Key_Escape:
            chat_view = self.current_chat_view
            if chat_view:
                conversation_id = chat_view.conversation_id
                if conversation_id in self._current_tasks:
                    for task in self._current_tasks[conversation_id]:
                        if not task.done():
                            task.cancel()
                    chat_view.add_message("System: Request cancelled by user", "system")
                    chat_view.finish_ai_response()
                    asyncio.create_task(
                        self.write_cancellation_to_transcript(conversation_id)
                    )
        else:
            super().keyPressEvent(event)

    async def write_cancellation_to_transcript(self, conversation_id: str):
        """Write cancellation message to transcript."""
        chat_view = self.chat_views.get(conversation_id)
        if not chat_view:
            return

        cancel_message = Message.create(
            conversation_id,
            MessageSource.SYSTEM,
            "Request cancelled by user",
            error={
                "code": "cancelled",
                "message": "Request cancelled by user",
                "details": {
                    "time": datetime.utcnow().isoformat()
                }
            }
        )
        chat_view.conversation.add_message(cancel_message)
        await self.transcript_writer.write([cancel_message.to_transcript_dict()])
