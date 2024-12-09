"""Main window implementation for Humbug application."""

import asyncio
from datetime import datetime
import logging
from typing import Dict, List
import uuid

from PySide6.QtWidgets import (
    QMainWindow, QDialog, QWidget, QVBoxLayout, QApplication, QMenuBar
)
from PySide6.QtCore import Qt, QTimer, Slot
from PySide6.QtGui import QKeyEvent, QAction, QKeySequence

from humbug.gui.about_dialog import AboutDialog
from humbug.gui.settings_dialog import SettingsDialog
from humbug.gui.style_manager import StyleManager
from humbug.gui.tab_manager import TabManager


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
        self.style_manager = StyleManager()

        self._create_actions()
        self._create_menus()
        self.setup_ui()

        # Create a timer that fires every 50ms to keep our menu states correct
        self.menu_timer = QTimer()
        self.menu_timer.setInterval(50)
        self.menu_timer.timeout.connect(self._update_menu_state)
        self.menu_timer.start()

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

        # View menu actions
        self.zoom_in_action = QAction("Zoom In", self)
        self.zoom_in_action.setShortcut(QKeySequence("Ctrl+="))
        self.zoom_in_action.triggered.connect(lambda: self._handle_zoom(1.189027))

        self.zoom_out_action = QAction("Zoom Out", self)
        self.zoom_out_action.setShortcut(QKeySequence("Ctrl+-"))
        self.zoom_out_action.triggered.connect(lambda: self._handle_zoom(1/1.189027))

        self.reset_zoom_action = QAction("Reset Zoom", self)
        self.reset_zoom_action.setShortcut(QKeySequence("Ctrl+0"))
        self.reset_zoom_action.triggered.connect(lambda: self._set_zoom(1.0))

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

        # View menu
        view_menu = self._menu_bar.addMenu("&View")
        view_menu.addAction(self.zoom_in_action)
        view_menu.addAction(self.zoom_out_action)
        view_menu.addAction(self.reset_zoom_action)

    def _show_about_dialog(self):
        """Show the About dialog."""
        dialog = AboutDialog(self)
        dialog.exec()

    @Slot()
    def _update_menu_state(self):
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
            self.settings_action.setEnabled(False)
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
        self.settings_action.setEnabled(True)

        current_zoom = self.windowHandle().devicePixelRatio()
        self.zoom_in_action.setEnabled(current_zoom < 2.0)
        self.zoom_out_action.setEnabled(current_zoom > 0.5)

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

        self.style_manager.zoom_changed.connect(self._update_styles)
        self._update_styles()

    def _update_styles(self) -> None:
        zoom_factor = self.style_manager.zoom_factor
        base_font_size = self.style_manager.base_font_size

        self.setStyleSheet(f"""
            * {{
                font-size: {base_font_size * zoom_factor}pt;
            }}
            QMainWindow {{
                background-color: #1e1e1e;
                color: #ffffff;
            }}
            QMenuBar {{
                background-color: #2d2d2d;
                color: #ffffff;
                padding: {4 * zoom_factor}px;
                font-size: {base_font_size * zoom_factor}pt;
            }}
            QMenuBar::item {{
                background-color: transparent;
                border-radius: 4px;
                padding: {4 * zoom_factor}px {8 * zoom_factor}px {4 * zoom_factor}px {8 * zoom_factor}px;
            }}
            QMenuBar::item:selected {{
                background-color: #3d3d3d;
            }}
            QMenu {{
                background-color: #2d2d2d;
                border-color: #3d3d3d;
                border-width: {1 * zoom_factor}px;
                border-style: solid;
                border-radius: {4 * zoom_factor}px;
            }}
            QMenu::item {{
                margin: {3 * zoom_factor}px {5 * zoom_factor}px;
                padding: {4 * zoom_factor}px {4 * zoom_factor}px {4 * zoom_factor}px {4 * zoom_factor}px;
            }}
            QMenu::item:selected {{
                background-color: #3d3d3d
            }}
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
            self.tab_manager.close_conversation(chat_view.conversation_id)

    @property
    def current_chat_view(self):
        """Get the currently active chat view."""
        return self.tab_manager.get_current_chat()

    def _sanitize_input(self, text: str) -> str:
        """Strip control characters from input text, preserving newlines."""
        return ''.join(char for char in text if char == '\n' or (ord(char) >= 32 and ord(char) != 127))

    def submit_message(self):
        """Handle message submission."""
        chat_view = self.current_chat_view
        if not chat_view:
            return

        message = self._sanitize_input(chat_view.get_input_text().strip())
        if not message:
            return

        # Clear input area
        chat_view.clear_input()

        # Add user message and get the message object
        user_message = chat_view.add_user_message(message)

        # Write to transcript
        asyncio.create_task(
            self.transcript_writer.write([user_message.to_transcript_dict()])
        )

        # Handle commands
        if message.startswith('/'):
            asyncio.create_task(self.handle_command(message[1:], chat_view.conversation_id))
            return

        # Start AI response
        task = asyncio.create_task(
            self.process_ai_response(message, chat_view.conversation_id)
        )

        if chat_view.conversation_id not in self._current_tasks:
            self._current_tasks[chat_view.conversation_id] = []
        self._current_tasks[chat_view.conversation_id].append(task)

        def task_done_callback(task):
            if chat_view.conversation_id in self._current_tasks:
                try:
                    self._current_tasks[chat_view.conversation_id].remove(task)
                except ValueError:
                    pass

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

            stream = self.ai_backend.stream_message(
                message,
                chat_view.get_message_context(),
                conversation_id
            )

            async for response in stream:
                try:
                    message = await chat_view.update_streaming_response(
                        content=response.content,
                        usage=response.usage,
                        error=response.error
                    )

                    # Only write AI messages that are complete (have usage info)
                    if message and (response.usage or response.error):
                        await self.transcript_writer.write([message.to_transcript_dict()])

                    # Handle retryable errors by adding them to transcript
                    if response.error:
                        if response.error['code'] in ['network_error', 'timeout']:
                            retry_message = chat_view.add_system_message(
                                response.error['message'],
                                error=response.error
                            )
                            await self.transcript_writer.write([retry_message.to_transcript_dict()])
                        else:
                            return

                except StopAsyncIteration:
                    break

        except (asyncio.CancelledError, GeneratorExit):
            self.logger.debug(f"AI response cancelled for conv {conversation_id}")
            if chat_view:
                message = chat_view.update_streaming_response(
                    content="",
                    completed=True
                )
                if message:
                    await self.transcript_writer.write([message.to_transcript_dict()])

                cancel_message = chat_view.add_system_message("Request cancelled by user")
                await self.transcript_writer.write([cancel_message.to_transcript_dict()])
            return

        except Exception as e:
            self.logger.exception(f"Error processing AI response for conv {conversation_id}")
            if chat_view:
                error = {
                    "code": "process_error",
                    "message": str(e),
                    "details": {"type": type(e).__name__}
                }
                message = chat_view.update_streaming_response(
                    content="",
                    error=error,
                    completed=True
                )
                if message:
                    await self.transcript_writer.write([message.to_transcript_dict()])

        finally:
            self.logger.debug(f"=== Finished AI response for conv {conversation_id} ===")

    async def handle_command(self, command: str, conversation_id: str):
        """Handle application commands."""
        chat_view = self.chat_views.get(conversation_id)
        if not chat_view:
            return

        if command.strip().lower() == "exit":
            QApplication.quit()
            return

        system_message = chat_view.add_system_message(f"Unknown command: {command}")
        await self.transcript_writer.write([system_message.to_transcript_dict()])

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
                    chat_view.finish_ai_response()
                    asyncio.create_task(
                        self._handle_cancellation(conversation_id)
                    )
        else:
            super().keyPressEvent(event)

    async def _handle_cancellation(self, conversation_id: str):
        """Write cancellation message to transcript."""
        chat_view = self.chat_views.get(conversation_id)
        if not chat_view:
            return

        cancel_message = chat_view.add_system_message(
            "Request cancelled by user",
            error={
                "code": "cancelled",
                "message": "Request cancelled by user",
                "details": {
                    "time": datetime.utcnow().isoformat()
                }
            }
        )
        await self.transcript_writer.write([cancel_message.to_transcript_dict()])

    def _handle_zoom(self, factor: float):
        """Handle zoom in/out requests."""
        new_zoom = self.style_manager.zoom_factor * factor
        self._set_zoom(new_zoom)

    def _set_zoom(self, zoom_level: float):
        """Set zoom level for the application."""
        self.style_manager.set_zoom(zoom_level)
