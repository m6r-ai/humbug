from PySide6.QtWidgets import (QMainWindow, QWidget, QVBoxLayout, QTextEdit,
                             QLabel, QSplitter, QApplication, QMenu)
from PySide6.QtCore import Qt
from PySide6.QtGui import QTextCharFormat, QColor, QKeyEvent, QTextCursor, QAction
import asyncio
import qasync
from typing import List, Optional
from datetime import datetime

class MessageHistoryWidget(QTextEdit):
    """Widget for displaying message history with styled text."""

    STYLE_COLORS = {
        'user': QColor('white'),
        'ai': QColor('yellow'),
        'system': QColor('green'),
        'error': QColor('red')
    }

    def __init__(self):
        super().__init__()
        self.setReadOnly(True)
        self.setStyleSheet("""
            QTextEdit {
                background-color: #1e1e1e;
                color: #ffffff;
                border: none;
            }
        """)
        self.current_ai_response_start = None  # Track start position of current AI response
        self.setContextMenuPolicy(Qt.CustomContextMenu)
        self.customContextMenuRequested.connect(self.show_context_menu)

    def show_context_menu(self, position):
        """Show custom context menu."""
        menu = QMenu(self)

        # Add copy action if text is selected
        if self.textCursor().hasSelection():
            copy_action = QAction("Copy", self)
            copy_action.triggered.connect(self.copy)
            copy_action.setShortcut("Ctrl+C")
            menu.addAction(copy_action)

        if menu.actions():
            menu.exec_(self.mapToGlobal(position))

    def add_message(self, message: str, style: str):
        """Add a styled message to the history."""
        format = QTextCharFormat()
        format.setForeground(self.STYLE_COLORS.get(style, QColor('white')))

        cursor = self.textCursor()

        if style == 'ai' and self.current_ai_response_start is not None:
            cursor.setPosition(self.current_ai_response_start)
            cursor.movePosition(QTextCursor.End, QTextCursor.KeepAnchor)
            cursor.insertText(f"{message}\n", format)
        else:
            cursor.movePosition(QTextCursor.End)
            cursor.insertText(f"{message}\n", format)

            if style == 'ai':
                self.current_ai_response_start = cursor.position() - len(message) - 1

        self.setTextCursor(cursor)
        self.ensureCursorVisible()

    def keyPressEvent(self, event: QKeyEvent):
        """Handle key press events."""
        # Handle copy (Ctrl+C) when text is selected
        if event.key() == Qt.Key_C and event.modifiers() == Qt.ControlModifier:
            if self.textCursor().hasSelection():
                self.copy()
                return
            # If no text is selected, let the main window handle it (for cancellation)
            elif self.parent() and hasattr(self.parent(), 'cancel_current_request'):
                self.parent().cancel_current_request()
                return

        super().keyPressEvent(event)

    def finish_ai_response(self):
        """Mark the current AI response as complete."""
        self.current_ai_response_start = None

class InputWidget(QTextEdit):
    """Widget for user input with line limit."""

    def __init__(self):
        super().__init__()
        self.setMaximumHeight(200)  # Approximately 10 lines
        self.setMinimumHeight(60)   # Minimum 3 lines
        self.setStyleSheet("""
            QTextEdit {
                background-color: #2d2d2d;
                color: #ffffff;
                border: none;
            }
        """)
        self._main_window = None
        self.setContextMenuPolicy(Qt.CustomContextMenu)
        self.customContextMenuRequested.connect(self.show_context_menu)

    def show_context_menu(self, position):
        """Show custom context menu."""
        menu = QMenu(self)

        # Add cut action if text is selected
        if self.textCursor().hasSelection():
            cut_action = QAction("Cut", self)
            cut_action.triggered.connect(self.cut)
            cut_action.setShortcut("Ctrl+X")
            menu.addAction(cut_action)

            copy_action = QAction("Copy", self)
            copy_action.triggered.connect(self.copy)
            copy_action.setShortcut("Ctrl+C")
            menu.addAction(copy_action)

        # Always show paste option
        paste_action = QAction("Paste", self)
        paste_action.triggered.connect(self.paste)
        paste_action.setShortcut("Ctrl+V")
        menu.addAction(paste_action)

        if menu.actions():
            menu.exec_(self.mapToGlobal(position))

    def set_main_window(self, window):
        """Set the main window reference."""
        self._main_window = window

    def keyPressEvent(self, event: QKeyEvent):
        """Handle key press events."""
        # Ctrl+C for cancel or copy
        if event.key() == Qt.Key_C and event.modifiers() == Qt.ControlModifier:
            if self.textCursor().hasSelection():
                self.copy()
            elif self._main_window:
                self._main_window.cancel_current_request()
            return

        # Ctrl+J for submit
        if event.key() == Qt.Key_J and event.modifiers() == Qt.ControlModifier:
            if self._main_window:
                self._main_window.submit_message()
            return

        # Handle cut (Ctrl+X)
        if event.key() == Qt.Key_X and event.modifiers() == Qt.ControlModifier:
            if self.textCursor().hasSelection():
                self.cut()
                return

        # Handle paste (Ctrl+V)
        if event.key() == Qt.Key_V and event.modifiers() == Qt.ControlModifier:
            self.paste()
            return

        # Enter for newline
        if event.key() == Qt.Key_Return and event.modifiers() != Qt.ControlModifier:
            # Check if adding a newline would exceed 10 lines
            current_text = self.toPlainText()
            if current_text.count('\n') < 9:
                super().keyPressEvent(event)
            return

        super().keyPressEvent(event)


class HumbugMainWindow(QMainWindow):
    """Main window for the Humbug application."""

    def __init__(self, ai_backend, transcript_writer):
        super().__init__()
        self.ai_backend = ai_backend
        self.transcript_writer = transcript_writer
        self.current_response = ""
        self.token_counts = {"input": 0, "output": 0}
        self._current_task = None  # Track current AI task

        self.setup_ui()

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

        # Create splitter for resizable sections
        splitter = QSplitter(Qt.Vertical)

        # Message history
        self.history_view = MessageHistoryWidget()
        splitter.addWidget(self.history_view)

        # Input area
        self.input_area = InputWidget()
        self.input_area.set_main_window(self)  # Set main window reference
        splitter.addWidget(self.input_area)

        # Set splitter properties
        splitter.setStretchFactor(0, 3)  # History gets more space
        splitter.setStretchFactor(1, 1)  # Input gets less space

        layout.addWidget(splitter)

        # Status bar
        self.status_label = QLabel("Input tokens: 0 | Output tokens: 0")
        self.statusBar().addWidget(self.status_label)

        # Dark theme for status bar
        self.statusBar().setStyleSheet("""
            QStatusBar {
                background-color: #2d2d2d;
                color: #ffffff;
            }
        """)

        # Set dark theme for main window
        self.setStyleSheet("""
            QMainWindow {
                background-color: #1e1e1e;
            }
        """)

    async def write_to_transcript(self, messages: List[dict]):
        """Write messages to transcript with error handling."""
        try:
            await self.transcript_writer.write(messages)
        except Exception as e:
            self.history_view.add_message(
                f"[ERROR] Failed to write to transcript: {str(e)}", "error")

    def keyPressEvent(self, event: QKeyEvent):
        """Handle main window key events."""
        # Check for Ctrl+C
        if event.key() == Qt.Key_C and event.modifiers() == Qt.ControlModifier:
            self.cancel_current_request()
        else:
            super().keyPressEvent(event)

    def cancel_current_request(self):
        """Cancel the current AI request if one is in progress."""
        if self._current_task and not self._current_task.done():
            self._current_task.cancel()
            self.history_view.add_message("System: Request cancelled by user", "system")
            self.history_view.finish_ai_response()
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
        message = self.input_area.toPlainText().strip()
        if not message:
            return

        # Clear input area
        self.input_area.clear()

        # Handle commands
        if message.startswith('/'):
            asyncio.create_task(self.handle_command(message[1:]))
            return

        # Add user message to history and transcript
        self.history_view.add_message(f"You: {message}", "user")
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

            async for response in self.ai_backend.stream_message(message, self.get_conversation_history()):
                if response.error:
                    error_msg = f"Error: {response.error['message']}"
                    self.history_view.add_message(error_msg, "error")
                    await self.write_to_transcript([{
                        "type": "error",
                        "content": error_msg,
                        "error": response.error,
                        "timestamp": datetime.utcnow().isoformat()
                    }])
                    return

                # Update response
                self.current_response = response.content

                # For first response chunk, add new message
                if first_response:
                    self.history_view.add_message(f"AI: {self.current_response}", "ai")
                    first_response = False
                else:
                    # For subsequent chunks, update existing message
                    self.history_view.add_message(f"AI: {self.current_response}", "ai")

                # Update token counts if available
                if response.usage:
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
                    # Mark AI response as complete since we got usage info
                    self.history_view.finish_ai_response()

        except asyncio.CancelledError:
            # Handle cancellation gracefully
            self.history_view.finish_ai_response()
            raise
        except Exception as e:
            error_msg = f"Error: {str(e)}"
            self.history_view.add_message(error_msg, "error")
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
            self.history_view.finish_ai_response()  # Ensure we reset on error
        finally:
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

        self.history_view.add_message(f"Unknown command: {command}", "system")

    def update_status(self):
        """Update the status bar with current token counts."""
        self.status_label.setText(
            f"Input tokens: {self.token_counts['input']} | "
            f"Output tokens: {self.token_counts['output']}"
        )
