"""Chat view implementation."""

import asyncio
import logging
from typing import List, Optional, Dict
from datetime import datetime

from PySide6.QtWidgets import (QMainWindow, QWidget, QVBoxLayout, QLabel,
                             QApplication, QMenu, QMenuBar, QDialog, QTextEdit,
                             QScrollArea, QScrollBar, QFrame)
from PySide6.QtCore import Qt, Signal, QSize, QEvent
from PySide6.QtGui import (QTextCursor, QColor, QKeyEvent, QAction, QKeySequence,
                          QTextCharFormat, QTextDocument, QPainter, QResizeEvent,
                          QWheelEvent)


class HistoryView(QTextEdit):
    """Read-only view for chat history."""

    def __init__(self, parent=None):
        """Initialize the history view."""
        super().__init__(parent)
        self.setReadOnly(True)
        self.setFrameStyle(QFrame.NoFrame)
        self.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)

        # Style for different message types
        self.STYLE_COLORS = {
            'user': QColor('white'),
            'ai': QColor('yellow'),
            'system': QColor('green'),
            'error': QColor('red')
        }

        # Set dark theme
        self.setStyleSheet("""
            QTextEdit {
                background-color: #1e1e1e;
                color: #ffffff;
                border: none;
            }
        """)

        # Initialize AI response tracking
        self._ai_response_start = None
        self._ai_response_length = 0

    def append_message(self, message: str, style: str):
        """Append a message with the specified style."""
        cursor = QTextCursor(self.document())
        cursor.movePosition(QTextCursor.End)

        # Add newline if not at start of document
        if not cursor.atStart():
            cursor.insertBlock()

        # Create format with specified color
        format = QTextCharFormat()
        format.setForeground(self.STYLE_COLORS.get(style, QColor('white')))
        cursor.insertText(message, format)

        # For AI messages, track position for updates
        if style == 'ai':
            self._ai_response_start = cursor.position() - len(message)
            self._ai_response_length = len(message)
        else:
            self._ai_response_start = None
            self._ai_response_length = 0

        # Ensure new message is visible
        self.ensureCursorVisible()

    def update_last_ai_response(self, content: str) -> None:
        """Update the last AI response in the history."""
        if self._ai_response_start is None:
            # No AI response to update - append as new
            self.append_message(f"AI: {content}", 'ai')
            return

        cursor = QTextCursor(self.document())
        cursor.setPosition(self._ai_response_start)
        cursor.movePosition(QTextCursor.Right, QTextCursor.KeepAnchor,
                          self._ai_response_length)

        format = QTextCharFormat()
        format.setForeground(self.STYLE_COLORS['ai'])
        cursor.insertText(f"AI: {content}", format)

        self._ai_response_length = len(f"AI: {content}")
        self.ensureCursorVisible()

    def finish_ai_response(self):
        """Mark the current AI response as complete."""
        self._ai_response_start = None
        self._ai_response_length = 0


class InputEdit(QTextEdit):
    """Editable input area for user messages."""

    submitted = Signal(str)  # Signal emitted when message is submitted

    def __init__(self, parent=None):
        """Initialize the input edit area."""
        super().__init__(parent)
        self.setFrameStyle(QFrame.NoFrame)
        self.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        self.setMinimumHeight(40)

        policy = self.sizePolicy()
        policy.setVerticalPolicy(policy.Policy.Minimum)
        self.setSizePolicy(policy)

        # Style for input area
        self.setStyleSheet("""
            QTextEdit {
                background-color: #2d2d2d;
                color: #ffffff;
                border: none;
            }
        """)

        # History of user inputs
        self.input_history = []
        self.history_index = -1
        self.current_input = ""

    def keyPressEvent(self, event):
        """Handle special key events."""
        # Check for submit shortcut (Ctrl+J)
        if event.key() == Qt.Key_J and event.modifiers() == Qt.ControlModifier:
            text = self.toPlainText().strip()
            if text:
                self.submitted.emit(text)
                if text not in self.input_history:
                    self.input_history.append(text)
                self.history_index = -1
                self.clear()
            return

        # Handle up/down history navigation when cursor at start
        if self.textCursor().atStart() and not self.textCursor().hasSelection():
            if event.key() == Qt.Key_Up and self.input_history:
                if self.history_index == -1:
                    self.current_input = self.toPlainText()
                self.history_index = min(len(self.input_history) - 1,
                                       self.history_index + 1)
                self.setPlainText(self.input_history[-self.history_index - 1])
                return

            if event.key() == Qt.Key_Down:
                if self.history_index > 0:
                    self.history_index -= 1
                    self.setPlainText(self.input_history[-self.history_index - 1])
                elif self.history_index == 0:
                    self.history_index = -1
                    self.setPlainText(self.current_input)
                return

        super().keyPressEvent(event)

    def resizeEvent(self, event: QResizeEvent):
        """Adjust height based on content."""
        super().resizeEvent(event)
        # Calculate required height for content
        doc_height = self.document().size().height()
        # Add margins
        content_height = doc_height + 2 * self.document().documentMargin()
        # Constrain between minimum and maximum
        new_height = max(40, min(200, content_height))
        if new_height != self.height():
            self.setFixedHeight(new_height)


class ChatView(QFrame):
    """Widget combining history view and input edit with unified scrolling."""

    def __init__(self, parent=None):
        """Initialize the unified chat view."""
        super().__init__(parent)
        self.logger = logging.getLogger("ChatView")

        # Main layout
        self.layout = QVBoxLayout(self)
        self.layout.setContentsMargins(0, 0, 0, 0)
        self.layout.setSpacing(0)

        # Create scroll area to contain both widgets
        self.scroll_area = QScrollArea(self)
        self.scroll_area.setFrameStyle(QFrame.NoFrame)
        self.scroll_area.setWidgetResizable(True)
        self.scroll_area.setHorizontalScrollBarPolicy(Qt.ScrollBarAsNeeded)

        # Container widget for history and input
        self.container = QWidget(self.scroll_area)
        self.container_layout = QVBoxLayout(self.container)
        self.container_layout.setContentsMargins(0, 0, 0, 0)
        self.container_layout.setSpacing(0)

        # Create history and input widgets
        self.history = HistoryView(self.container)
        self.input = InputEdit(self.container)

        # Connect input submitted signal to parent's submit handler
        self.input.submitted.connect(lambda msg: self.parent().submit_message())

        # Add widgets to container
        self.container_layout.addWidget(self.history, 1)  # 1 = stretch factor
        self.container_layout.addWidget(self.input, 0)    # 0 = no stretch

        # Set container as scroll area widget
        self.scroll_area.setWidget(self.container)

        # Add scroll area to main layout
        self.layout.addWidget(self.scroll_area)

        # Connect input height changes to scroll adjustment
        self.input.document().contentsChanged.connect(self._adjust_scroll)

    def _adjust_scroll(self):
        """Adjust scroll position when input height changes."""
        scrollbar = self.scroll_area.verticalScrollBar()
        if scrollbar.value() == scrollbar.maximum():
            # We were at bottom - stay there
            self.scroll_area.verticalScrollBar().setValue(
                self.scroll_area.verticalScrollBar().maximum()
            )

    def add_message(self, message: str, style: str):
        """Add a message to the history area."""
        if style == 'ai' and message.startswith("AI: "):
            self.history.update_last_ai_response(message[4:])
        else:
            self.history.append_message(message, style)

    def get_input_text(self) -> str:
        """Get the current input text."""
        return self.input.toPlainText()

    def set_input_text(self, text: str):
        """Set the input text."""
        self.input.setPlainText(text)

    def clear_input(self):
        """Clear the input area."""
        self.input.clear()

    def wheelEvent(self, event: QWheelEvent):
        """Handle wheel events for smooth scrolling."""
        # Only handle vertical scrolling
        if event.angleDelta().y() != 0:
            scrollbar = self.scroll_area.verticalScrollBar()
            scrollbar.setValue(scrollbar.value() - event.angleDelta().y() / 2)

    def finish_ai_response(self):
        """Mark the current AI response as complete."""
        self.history.finish_ai_response()
