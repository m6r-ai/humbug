"""Main window implementation for Humbug application."""

import asyncio
from datetime import datetime
import json
import logging
import os
from typing import Dict, List
import uuid

from PySide6.QtWidgets import (
    QMainWindow, QDialog, QWidget, QVBoxLayout, QMenuBar, QFileDialog, QMessageBox
)
from PySide6.QtCore import Qt, QTimer, Slot
from PySide6.QtGui import QKeyEvent, QAction, QKeySequence

from humbug.ai.conversation_settings import ConversationSettings
from humbug.ai.ai_backend import AIBackend
from humbug.gui.about_dialog import AboutDialog
from humbug.gui.chat_tab import ChatTab
from humbug.gui.color_role import ColorRole
from humbug.gui.editor_tab import EditorTab
from humbug.gui.settings_dialog import SettingsDialog
from humbug.gui.style_manager import StyleManager, ColorMode
from humbug.gui.tab_manager import TabManager
from humbug.transcript.transcript_loader import TranscriptLoader
from humbug.transcript.transcript_writer import TranscriptWriter


class MainWindow(QMainWindow):
    """Main window for the Humbug application."""

    def __init__(self, ai_backends: Dict[str, AIBackend]):
        """Initialize the main window."""
        super().__init__()
        self._ai_backends = ai_backends
        self._untitled_count = 0
        self._chat_tabs = {}  # tab_id -> ChatTab
        self._current_tasks: Dict[str, List[asyncio.Task]] = {}
        self._logger = logging.getLogger("MainWindow")
        self._dark_mode = True

        # Initialize available models based on active backends
        self._available_models = self._get_available_models()

        # Humbug menu actions
        self._about_action = QAction("About Humbug", self)
        self._about_action.triggered.connect(self._show_about_dialog)

        self._quit_action = QAction("Quit Humbug", self)
        self._quit_action.setShortcut(QKeySequence("Ctrl+Q"))
        self._quit_action.triggered.connect(self.close)

        # File menu actions
        self._new_conv_action = QAction("New Conversation", self)
        self._new_conv_action.setShortcut(QKeySequence("Ctrl+Shift+N"))
        self._new_conv_action.triggered.connect(self._new_conversation)

        self._new_file_action = QAction("New File", self)
        self._new_file_action.setShortcut(QKeySequence.New)
        self._new_file_action.triggered.connect(self._new_file)

        self._close_tab_action = QAction("Close Tab", self)
        self._close_tab_action.setShortcut(QKeySequence("Ctrl+W"))
        self._close_tab_action.triggered.connect(self._close_current_tab)

        self._open_conv_action = QAction("Open Conversation...", self)
        self._open_conv_action.setShortcut(QKeySequence("Ctrl+Shift+O"))
        self._open_conv_action.triggered.connect(self._open_conversation)

        self._open_file_action = QAction("Open File...", self)
        self._open_file_action.setShortcut(QKeySequence.Open)
        self._open_file_action.triggered.connect(self._open_file)

        self._save_action = QAction("Save", self)
        self._save_action.setShortcut(QKeySequence.Save)
        self._save_action.triggered.connect(self._save_file)

        self._save_as_action = QAction("Save As...", self)
        self._save_as_action.setShortcut(QKeySequence.SaveAs)
        self._save_as_action.triggered.connect(self._save_file_as)

        # Edit menu actions
        self._submit_action = QAction("Submit", self)
        self._submit_action.setShortcut(QKeySequence("Ctrl+J"))
        self._submit_action.triggered.connect(self._submit_message)

        self._undo_action = QAction("Undo", self)
        self._undo_action.setShortcut(QKeySequence("Ctrl+Z"))
        self._undo_action.triggered.connect(self._undo)

        self._redo_action = QAction("Redo", self)
        self._redo_action.setShortcut(QKeySequence("Ctrl+Shift+Z"))
        self._redo_action.triggered.connect(self._redo)

        self._cut_action = QAction("Cut", self)
        self._cut_action.setShortcut(QKeySequence("Ctrl+X"))
        self._cut_action.triggered.connect(self._cut)

        self._copy_action = QAction("Copy", self)
        self._copy_action.setShortcut(QKeySequence("Ctrl+C"))
        self._copy_action.triggered.connect(self._copy)

        self._paste_action = QAction("Paste", self)
        self._paste_action.setShortcut(QKeySequence("Ctrl+V"))
        self._paste_action.triggered.connect(self._paste)

        self._settings_action = QAction("Conversation Settings", self)
        self._settings_action.setShortcut(QKeySequence("Ctrl+,"))
        self._settings_action.triggered.connect(self._show_settings_dialog)

        # View menu actions
        self._dark_mode_action = QAction("&Dark Mode", self)
        self._dark_mode_action.setCheckable(True)
        self._dark_mode_action.setChecked(True)
        self._dark_mode_action.triggered.connect(self._handle_dark_mode)

        self._zoom_in_action = QAction("Zoom In", self)
        self._zoom_in_action.setShortcut(QKeySequence("Ctrl+="))
        self._zoom_in_action.triggered.connect(lambda: self._handle_zoom(1.189027))

        self._zoom_out_action = QAction("Zoom Out", self)
        self._zoom_out_action.setShortcut(QKeySequence("Ctrl+-"))
        self._zoom_out_action.triggered.connect(lambda: self._handle_zoom(1/1.189027))

        self._reset_zoom_action = QAction("Reset Zoom", self)
        self._reset_zoom_action.setShortcut(QKeySequence("Ctrl+0"))
        self._reset_zoom_action.triggered.connect(lambda: self._set_zoom(1.0))

        self._menu_bar = QMenuBar(self)
        self.setMenuBar(self._menu_bar)

        # Humbug menu
        humbug_menu = self._menu_bar.addMenu("&Humbug")
        humbug_menu.addAction(self._about_action)
        humbug_menu.addSeparator()
        humbug_menu.addAction(self._quit_action)

        # File menu
        file_menu = self._menu_bar.addMenu("&File")
        file_menu.addAction(self._new_conv_action)
        file_menu.addAction(self._new_file_action)
        file_menu.addSeparator()
        file_menu.addAction(self._open_conv_action)
        file_menu.addAction(self._open_file_action)
        file_menu.addSeparator()
        file_menu.addAction(self._save_action)
        file_menu.addAction(self._save_as_action)
        file_menu.addSeparator()
        file_menu.addAction(self._close_tab_action)

        # Edit menu
        edit_menu = self._menu_bar.addMenu("&Edit")
        edit_menu.addAction(self._submit_action)
        edit_menu.addSeparator()
        edit_menu.addAction(self._undo_action)
        edit_menu.addAction(self._redo_action)
        edit_menu.addSeparator()
        edit_menu.addAction(self._cut_action)
        edit_menu.addAction(self._copy_action)
        edit_menu.addAction(self._paste_action)
        edit_menu.addSeparator()
        edit_menu.addAction(self._settings_action)

        # View menu
        view_menu = self._menu_bar.addMenu("&View")
        view_menu.addAction(self._dark_mode_action)
        view_menu.addSeparator()
        view_menu.addAction(self._zoom_in_action)
        view_menu.addAction(self._zoom_out_action)
        view_menu.addAction(self._reset_zoom_action)

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
        self._new_conversation()

        self._style_manager = StyleManager()
        self._style_manager.style_changed.connect(self._handle_style_changed)
        self._handle_style_changed()

        # Create a timer that fires every 50ms to keep our menu states correct
        self._menu_timer = QTimer()
        self._menu_timer.setInterval(50)
        self._menu_timer.timeout.connect(self._update_menu_state)
        self._menu_timer.start()

    def _undo(self):
        self.tab_manager.get_current_tab().undo()

    def _redo(self):
        self.tab_manager.get_current_tab().redo()

    def _cut(self):
        self.tab_manager.get_current_tab().cut()

    def _copy(self):
        self.tab_manager.get_current_tab().copy()

    def _paste(self):
        self.tab_manager.get_current_tab().paste()

    def _show_about_dialog(self):
        """Show the About dialog."""
        dialog = AboutDialog(self)
        dialog.exec()

    def _new_file(self):
        """Create a new empty editor tab."""
        self._untitled_count += 1
        tab_id = str(uuid.uuid4())
        editor = EditorTab(tab_id, self)
        editor.set_filename(None, self._untitled_count)

        # Connect editor signals
        editor.close_requested.connect(self._handle_tab_close_requested)
        editor.title_changed.connect(self._handle_tab_title_changed)
        editor.modified_state_changed.connect(self._handle_tab_modified)

        self.tab_manager.add_tab(editor, f"Untitled-{self._untitled_count}")
        return editor

    def _open_file(self):
        """Show open file dialog and create editor tab."""
        self._menu_timer.stop()
        file_path, _ = QFileDialog.getOpenFileName(
            self,
            "Open File",
            os.path.expanduser("~/")
        )
        self._menu_timer.start()

        if file_path:
            # Check if file is already open
            for tab in self.tab_manager.get_all_tabs():
                if isinstance(tab, EditorTab) and tab.filename == file_path:
                    self.tab_manager.set_current_tab(tab.tab_id)
                    return

            tab_id = str(uuid.uuid4())
            editor = EditorTab(tab_id, self)
            editor.set_filename(file_path)

            # Connect editor signals
            editor.close_requested.connect(self._handle_tab_close_requested)
            editor.title_changed.connect(self._handle_tab_title_changed)
            editor.modified_state_changed.connect(self._handle_tab_modified)

            self.tab_manager.add_tab(editor, os.path.basename(file_path))

    def _save_file(self):
        """Save the current file."""
        current_tab = self.tab_manager.get_current_tab()
        if isinstance(current_tab, EditorTab):
            current_tab.save()

    def _save_file_as(self):
        """Save the current file with a new name."""
        current_tab = self.tab_manager.get_current_tab()
        if isinstance(current_tab, EditorTab):
            current_tab.save_as()

    def _handle_tab_close_requested(self, tab_id: str) -> None:
        """Handle tab close request."""
        tab = self._chat_tabs.get(tab_id)
        if tab:
            tab.close()

        self.tab_manager.close_tab(tab_id)

    def _handle_tab_title_changed(self, tab_id: str, title: str) -> None:
        """Update UI to reflect a tab title has changed."""
        self.tab_manager.update_tab_title(tab_id, title)

    def _handle_tab_modified(self, tab_id: str, modified: bool) -> None:
        """Update UI to reflect tab modified state."""
        self.tab_manager.set_tab_modified(tab_id, modified)

    @Slot()
    def _update_menu_state(self):
        """Update enabled/disabled state of menu items."""
        current_tab = self.tab_manager.get_current_tab()

        # Disable all actions by default
        self._save_action.setEnabled(False)
        self._save_as_action.setEnabled(False)
        self._close_tab_action.setEnabled(False)
        self._undo_action.setEnabled(False)
        self._redo_action.setEnabled(False)
        self._cut_action.setEnabled(False)
        self._copy_action.setEnabled(False)
        self._paste_action.setEnabled(False)
        self._submit_action.setEnabled(False)
        self._settings_action.setEnabled(False)

        if not current_tab:
            return

        # Enable common edit operations based on tab state
        self._save_action.setEnabled(current_tab.can_save())
        self._save_as_action.setEnabled(current_tab.can_save_as())
        self._close_tab_action.setEnabled(True)
        self._undo_action.setEnabled(current_tab.can_undo())
        self._redo_action.setEnabled(current_tab.can_redo())
        self._cut_action.setEnabled(current_tab.can_cut())
        self._copy_action.setEnabled(current_tab.can_copy())
        self._paste_action.setEnabled(current_tab.can_paste())
        self._submit_action.setEnabled(current_tab.can_submit())

        # Enable chat-specific operations for chat tabs
        if isinstance(current_tab, ChatTab):
            self._settings_action.setEnabled(True)

        # Update zoom actions
        current_zoom = self._style_manager.zoom_factor
        self._zoom_in_action.setEnabled(current_zoom < 2.0)
        self._zoom_out_action.setEnabled(current_zoom > 0.5)

    def _handle_style_changed(self) -> None:
        style_manager = self._style_manager
        zoom_factor = style_manager.zoom_factor
        base_font_size = style_manager.base_font_size

        self.setStyleSheet(f"""
            QMainWindow {{
                background-color: {style_manager.get_color_str(ColorRole.BACKGROUND_PRIMARY)};
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
            }}
            QMenuBar {{
                background-color: {style_manager.get_color_str(ColorRole.MENU_BACKGROUND)};
                padding: {4 * zoom_factor}px;
                font-size: {base_font_size * zoom_factor}pt;
            }}
            QMenuBar::item {{
                background-color: transparent;
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                border-radius: 4px;
                padding: {4 * zoom_factor}px {8 * zoom_factor}px {4 * zoom_factor}px {8 * zoom_factor}px;
            }}
            QMenuBar::item:selected {{
                background-color: {style_manager.get_color_str(ColorRole.MENU_HOVER)};
            }}
            QMenu {{
                background-color: {style_manager.get_color_str(ColorRole.MENU_BACKGROUND)};
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                border-color: {style_manager.get_color_str(ColorRole.MENU_HOVER)};
                border-width: {1 * zoom_factor}px;
                border-style: solid;
                border-radius: {4 * zoom_factor}px;
            }}
            QMenu::item {{
                margin: {3 * zoom_factor}px {5 * zoom_factor}px;
                padding: {4 * zoom_factor}px {4 * zoom_factor}px {4 * zoom_factor}px {4 * zoom_factor}px;
            }}
            QMenu::item:disabled {{
                color: {style_manager.get_color_str(ColorRole.TEXT_DISABLED)};
            }}
            QMenu::item:selected {{
                background-color: {style_manager.get_color_str(ColorRole.MENU_HOVER)}
            }}
        """)

    def _new_conversation(self) -> str:
        """Create a new conversation tab and return its ID."""
        # Generate timestamp and use it for both ID and metadata
        timestamp = datetime.utcnow()
        conversation_id = timestamp.strftime("%Y-%m-%d-%H-%M-%S-%f")[:23]

        # Create transcript file based on same ID
        filename = f"conversations/{conversation_id}.conv"
        writer = TranscriptWriter(
            filename,
            timestamp.isoformat()
        )

        # Create tab using same ID
        chat_tab = ChatTab(conversation_id, writer, self)
        self.tab_manager.add_tab(chat_tab, f"Conv: {conversation_id}")
        self._chat_tabs[conversation_id] = chat_tab
        return conversation_id

    def _open_conversation(self):
        """Show open conversation dialog and create chat tab."""
        self._menu_timer.stop()
        file_path, _ = QFileDialog.getOpenFileName(
            self,
            "Open Conversation",
            os.path.expanduser("~/"),
            "Transcript Files (*.conv);;All Files (*.*)"
        )
        self._menu_timer.start()

        if not file_path:
            return

        try:
            messages, error, _metadata = TranscriptLoader.load_transcript(file_path)
            if error:
                msgbox = self._create_styled_message_box(
                    QMessageBox.Critical,
                    "Error Loading Conversation",
                    f"Could not load {file_path}: {error}"
                )
                msgbox.exec()
                return

            # Create new conversation with new timestamp
            timestamp = datetime.utcnow()
            conversation_id = timestamp.strftime("%Y-%m-%d-%H-%M-%S-%f")[:23]

            # Create new transcript file for this conversation
            filename = f"conversations/{conversation_id}.conv"
            writer = TranscriptWriter(
                filename,
                timestamp.isoformat()
            )

            # Create new tab
            chat_tab = ChatTab(conversation_id, writer, self)

            # Load the messages into the new conversation
            chat_tab.load_message_history(messages)

            # Add tab
            self.tab_manager.add_tab(chat_tab, f"Conv: {conversation_id}")
            self._chat_tabs[conversation_id] = chat_tab

        except (FileNotFoundError, json.JSONDecodeError, Exception) as e:
            msgbox = self._create_styled_message_box(
                QMessageBox.Critical,
                "Error Loading Conversation",
                f"Could not load {file_path}: {str(e)}"
            )
            msgbox.exec()

    def _close_current_tab(self):
        """Close the current conversation tab."""
        chat_tab = self.tab_manager.get_current_tab()
        if not chat_tab:
            return

        self.tab_manager.close_tab(chat_tab.tab_id)

    def _sanitize_input(self, text: str) -> str:
        """Strip control characters from input text, preserving newlines."""
        return ''.join(char for char in text if char == '\n' or (ord(char) >= 32 and ord(char) != 127))

    def _submit_message(self):
        """Handle message submission."""
        chat_tab = self.tab_manager.get_current_tab()
        if not chat_tab:
            return

        if not chat_tab.can_submit():
            return

        message = self._sanitize_input(chat_tab.get_input_text().strip())
        if not message:
            return

        # Clear input area
        chat_tab.clear_input()

        # Add user message and get the message object
        user_message = chat_tab.add_user_message(message)

        # Write to transcript
        asyncio.create_task(
            chat_tab._transcript_writer.write([user_message.to_transcript_dict()])
        )

        # Start AI response
        task = asyncio.create_task(
            self.process_ai_response(message, chat_tab.tab_id)
        )

        if chat_tab.tab_id not in self._current_tasks:
            self._current_tasks[chat_tab.tab_id] = []

        self._current_tasks[chat_tab.tab_id].append(task)

        def task_done_callback(task):
            if chat_tab.tab_id in self._current_tasks:
                try:
                    self._current_tasks[chat_tab.tab_id].remove(task)
                except ValueError as e:
                    self._logger.debug("Value Error: %d: %s", chat_tab.tab_id, e)

        task.add_done_callback(task_done_callback)

    def _get_available_models(self) -> List[str]:
        """Get list of available models based on active backends."""
        models = []
        for model in ConversationSettings.AVAILABLE_MODELS:
            provider = ConversationSettings.get_provider(model)
            if provider in self._ai_backends:
                models.append(model)
        return models

    def _show_settings_dialog(self):
        """Show the conversation settings dialog."""
        chat_tab = self.tab_manager.get_current_tab()
        if not chat_tab:
            return

        dialog = SettingsDialog(self)
        # Pass available models to dialog
        dialog.set_available_models(self._available_models)
        dialog.set_settings(chat_tab.get_settings())

        if dialog.exec() == QDialog.Accepted:
            new_settings = dialog.get_settings()
            chat_tab.update_settings(new_settings)
            # Get the appropriate backend for the selected model
            provider = ConversationSettings.get_provider(new_settings.model)
            backend = self._ai_backends.get(provider)
            if backend:
                backend.update_conversation_settings(
                    chat_tab.tab_id,
                    new_settings
                )

    async def process_ai_response(self, message: str, tab_id: str):
        """Process AI response with streaming."""
        chat_tab = self._chat_tabs.get(tab_id)
        if not chat_tab:
            self._logger.error("No chat tab found for conversation %s", tab_id)
            return

        try:
            self._logger.debug("=== Starting new AI response for conv %s ===", tab_id)

            # Get the appropriate backend for the conversation
            settings = chat_tab.get_settings()
            provider = ConversationSettings.get_provider(settings.model)
            backend = self._ai_backends.get(provider)

            if not backend:
                error_msg = f"No backend available for provider: {provider}"
                system_message = chat_tab.add_system_message(
                    error_msg,
                    error={"code": "backend_error", "message": error_msg}
                )
                await chat_tab._transcript_writer.write([system_message.to_transcript_dict()])
                return

            stream = backend.stream_message(
                message,
                chat_tab.get_message_context(),
                tab_id
            )

            async for response in stream:
                try:
                    message = await chat_tab.update_streaming_response(
                        content=response.content,
                        usage=response.usage,
                        error=response.error
                    )

                    # Only write AI messages that are complete (have usage info)
                    if message and (response.usage or response.error):
                        await chat_tab._transcript_writer.write([message.to_transcript_dict()])

                    # Handle retryable errors by adding them to transcript
                    if response.error:
                        if response.error['code'] in ['network_error', 'timeout']:
                            retry_message = chat_tab.add_system_message(
                                response.error['message'],
                                error=response.error
                            )
                            await chat_tab._transcript_writer.write([retry_message.to_transcript_dict()])
                        else:
                            return

                except StopAsyncIteration:
                    break

        except (asyncio.CancelledError, GeneratorExit):
            self._logger.debug("AI response cancelled for conv %s", tab_id)
            if chat_tab:
                # Complete any ongoing AI response
                message = await chat_tab.update_streaming_response(
                    content="",
                    completed=True
                )
                if message:  # Only write if we got a message back
                    await chat_tab._transcript_writer.write([message.to_transcript_dict()])

                # Add cancellation message
                system_message = chat_tab.add_system_message(
                    "Request cancelled by user",
                    error={
                        "code": "cancelled",
                        "message": "Request cancelled by user",
                        "details": {
                            "time": datetime.utcnow().isoformat()
                        }
                    }
                )
                await chat_tab._transcript_writer.write([system_message.to_transcript_dict()])

            return

        except Exception as e:
            self._logger.exception("Error processing AI response for conv %s", tab_id)
            if chat_tab:
                error = {
                    "code": "process_error",
                    "message": str(e),
                    "details": {"type": type(e).__name__}
                }
                message = chat_tab.update_streaming_response(
                    content="",
                    error=error,
                    completed=True
                )
                if message:
                    await self._transcript_writer.write([message.to_transcript_dict()])

        finally:
            self._logger.debug("=== Finished AI response for conv %s ===", tab_id)

    def keyPressEvent(self, event: QKeyEvent):
        """Handle global key events."""

        if event.key() == Qt.Key_Escape:
            chat_tab = self.tab_manager.get_current_tab()
            if chat_tab:
                tab_id = chat_tab.tab_id
                if tab_id in self._current_tasks:
                    for task in self._current_tasks[tab_id]:
                        if not task.done():
                            task.cancel()
                    chat_tab.finish_ai_response()
                    asyncio.create_task(
                        self._handle_cancellation(tab_id)
                    )
        else:
            super().keyPressEvent(event)

    async def _handle_cancellation(self, tab_id: str):
        """Write cancellation message to transcript."""
        chat_tab = self._chat_tabs.get(tab_id)
        if not chat_tab:
            return

        cancel_message = chat_tab.add_system_message(
            "Request cancelled by user",
            error={
                "code": "cancelled",
                "message": "Request cancelled by user",
                "details": {
                    "time": datetime.utcnow().isoformat()
                }
            }
        )
        await self._transcript_writer.write([cancel_message.to_transcript_dict()])

    def _handle_dark_mode(self, _):
        """Handle dark mode enable/disable requests."""
        self._dark_mode = not self._dark_mode
        self._dark_mode_action.setChecked(self._dark_mode)
        self._style_manager.set_color_mode(ColorMode.DARK if self._dark_mode else ColorMode.LIGHT)

    def _handle_zoom(self, factor: float):
        """Handle zoom in/out requests."""
        new_zoom = self._style_manager.zoom_factor * factor
        self._set_zoom(new_zoom)

    def _set_zoom(self, zoom_level: float):
        """Set zoom level for the application."""
        self._style_manager.set_zoom(zoom_level)

    def closeEvent(self, event):
        """Handle application close request."""
        # Check each tab in turn
        for tab in self.tab_manager.get_all_tabs():
            if tab.is_modified and not tab.can_close():
                event.ignore()
                return

        event.accept()
