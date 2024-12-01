from PySide6.QtWidgets import (QMainWindow, QWidget, QVBoxLayout, QTextEdit,
                             QLabel, QSplitter, QApplication)
from PySide6.QtCore import Qt
from PySide6.QtGui import QTextCharFormat, QColor, QKeyEvent, QTextCursor
import asyncio
import qasync
from typing import List, Optional

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

    def add_message(self, message: str, style: str):
        """Add a styled message to the history."""
        format = QTextCharFormat()
        format.setForeground(self.STYLE_COLORS.get(style, QColor('white')))

        cursor = self.textCursor()

        if style == 'ai' and self.current_ai_response_start is not None:
            # Move to start of current AI response
            cursor.setPosition(self.current_ai_response_start)
            # Select to end of document
            cursor.movePosition(QTextCursor.End, QTextCursor.KeepAnchor)
            # Replace selection with new message
            cursor.insertText(f"{message}\n", format)
        else:
            # For new messages, move to end and insert
            cursor.movePosition(QTextCursor.End)
            cursor.insertText(f"{message}\n", format)

            # If this is the start of a new AI response, store its position
            if style == 'ai':
                self.current_ai_response_start = cursor.position() - len(message) - 1

        self.setTextCursor(cursor)
        self.ensureCursorVisible()

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
        self._main_window = None  # Store reference to main window

    def set_main_window(self, window):
        """Set the main window reference."""
        self._main_window = window

    def keyPressEvent(self, event: QKeyEvent):
        """Handle key press events."""
        # Ctrl+J for submit
        if event.key() == Qt.Key_J and event.modifiers() == Qt.ControlModifier:
            if self._main_window:
                self._main_window.submit_message()
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

        # Add user message to history
        self.history_view.add_message(f"You: {message}", "user")

        # Start AI response
        asyncio.create_task(self.process_ai_response(message))

    async def process_ai_response(self, message: str):
        """Process AI response with streaming."""
        try:
            self.current_response = ""
            first_response = True

            async for response in self.ai_backend.stream_message(message, self.get_conversation_history()):
                if response.error:
                    self.history_view.add_message(f"Error: {response.error['message']}", "error")
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
                    # Mark AI response as complete since we got usage info
                    self.history_view.finish_ai_response()

        except Exception as e:
            self.history_view.add_message(f"Error: {str(e)}", "error")
            self.history_view.finish_ai_response()  # Ensure we reset on error

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
