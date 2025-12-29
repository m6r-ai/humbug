"""Widget for displaying a single message in the log with header."""

from datetime import datetime
import logging
from typing import List, Tuple

from PySide6.QtWidgets import (
    QFrame, QVBoxLayout, QLabel, QHBoxLayout, QWidget, QTextEdit
)
from PySide6.QtCore import Signal, QPoint, Qt
from PySide6.QtGui import (
    QResizeEvent, QColor, QCursor, QMouseEvent, QTextCursor,
    QTextCharFormat
)

from humbug.color_role import ColorRole
from humbug.language.language_manager import LanguageManager
from humbug.mindspace.mindspace_log_level import MindspaceLogLevel
from humbug.style_manager import StyleManager
from humbug.tabs.log.log_text_edit import LogTextEdit


class LogMessage(QFrame):
    """Widget for displaying a single message in the log with header."""

    selection_changed = Signal(bool)
    scroll_requested = Signal(QPoint)
    mouse_released = Signal()

    def __init__(
        self,
        text: str,
        level: MindspaceLogLevel,
        timestamp: datetime,
        message_id: str,
        parent: QWidget | None = None
    ) -> None:
        """
        Initialize the log message widget.

        Args:
            text: The message text content
            level: The log level of the message
            timestamp: datetime object for the message timestamp
            message_id: Unique identifier for the message
            parent: Optional parent widget
        """
        super().__init__(parent)
        self.setFrameStyle(QFrame.Shape.Box | QFrame.Shadow.Plain)

        # Set object name for QSS targeting
        self.setObjectName("LogMessage")

        self._logger = logging.getLogger("LogMessage")

        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._on_language_changed)

        self._style_manager = StyleManager()

        # Create layout
        self._layout = QVBoxLayout(self)
        self.setLayout(self._layout)
        spacing = int(self._style_manager.message_bubble_spacing())
        self._layout.setSpacing(spacing)
        self._layout.setContentsMargins(spacing, spacing, spacing, spacing)

        # Create header area with horizontal layout
        self._header = QWidget(self)
        self._header.setObjectName("_header")
        self._header_layout = QHBoxLayout(self._header)
        self._header_layout.setContentsMargins(0, 0, 0, 0)
        self._header_layout.setSpacing(4)

        # Create role and timestamp labels
        self._level_label = QLabel(self)
        self._level_label.setObjectName("_level_label")
        self._level_label.setIndent(0)
        self._header_layout.addWidget(self._level_label)
        self._header_layout.addStretch()

        # Add header widget to main layout
        self._layout.addWidget(self._header)

        # Create text area
        self._text_area = LogTextEdit(self)
        self._text_area.setObjectName("_text_area")

        # Disable the standard context menu as our parent widget will handle that
        self._text_area.setContextMenuPolicy(Qt.ContextMenuPolicy.NoContextMenu)

        # Connect signals from text area
        self._text_area.selectionChanged.connect(self._on_selection_changed)
        self._text_area.mouse_pressed.connect(self._on_mouse_pressed)
        self._text_area.mouse_released.connect(self._on_mouse_released)

        self._layout.addWidget(self._text_area)

        self._is_spotlighted = False
        self._mouse_left_button_pressed = False

        self._message_id = message_id
        self._message_level = level
        self._message_timestamp = timestamp
        self._message_content = text

        # Set log level property for QSS targeting
        level_name = {
            MindspaceLogLevel.TRACE: "trace",
            MindspaceLogLevel.INFO: "info",
            MindspaceLogLevel.WARN: "warn",
            MindspaceLogLevel.ERROR: "error"
        }.get(level, "error")

        self._level_label.setProperty("log_level", level_name)

        self.setProperty("border", "default")

        # Set the content in the text area
        self._text_area.set_text(text)

        self.apply_style()
        self._on_language_changed()

    def message_id(self) -> str | None:
        """Get the message ID."""
        return self._message_id

    def is_spotlighted(self) -> bool:
        """Check if this message is spotlighted."""
        return self._is_spotlighted

    def set_spotlighted(self, spotlighted: bool) -> None:
        """Set the spotlighted state of this message."""
        if self._is_spotlighted == spotlighted:
            return

        self._is_spotlighted = spotlighted
        if spotlighted:
            self.setProperty("border", "spotlighted")
            self.setFocus()

        else:
            self.setProperty("border", "default")

        self.style().unpolish(self)
        self.style().polish(self)

    def _on_language_changed(self) -> None:
        """Update text when language changes."""
        self._update_level_text()

    def _update_level_text(self) -> None:
        """Update the level text."""
        # Map from message level to display text
        if self._message_level == MindspaceLogLevel.TRACE:
            level_text = "Trace"

        elif self._message_level == MindspaceLogLevel.INFO:
            level_text = "Info"

        elif self._message_level == MindspaceLogLevel.WARN:
            level_text = "Warning"

        else:
            level_text = "Error"

        # Format with timestamp
        if self._message_timestamp is not None:
            timestamp_str = self._message_timestamp.strftime("%Y-%m-%d %H:%M:%S.%f")[:-3]
            self._level_label.setText(f"{level_text} @ {timestamp_str}")

        else:
            self._level_label.setText(level_text)

    def _on_mouse_pressed(self, event: QMouseEvent) -> None:
        """Handle mouse press from text area."""
        if event.buttons() == Qt.MouseButton.LeftButton:
            self._mouse_left_button_pressed = True

    def _on_mouse_released(self, _event: QMouseEvent) -> None:
        """Handle mouse release from text area."""
        self._mouse_left_button_pressed = False
        self.mouse_released.emit()

    def _on_selection_changed(self) -> None:
        """Handle selection changes in the text area."""
        cursor = self._text_area.textCursor()
        has_selection = cursor.hasSelection()

        if has_selection and self._mouse_left_button_pressed:
            # Emit global mouse position for accurate scroll calculations
            self.scroll_requested.emit(QCursor.pos())

        self.selection_changed.emit(has_selection)

    def has_selection(self) -> bool:
        """Check if any section has selected text."""
        return self._text_area.textCursor().hasSelection()

    def get_selected_text(self) -> str:
        """
        Get any selected text in this message.

        Returns:
            Currently selected text or empty string
        """
        cursor = self._text_area.textCursor()
        if cursor.hasSelection():
            text = cursor.selectedText()
            # Convert Qt's special line break character
            return text.replace('\u2029', '\n')

        return ""

    def copy_selection(self) -> None:
        """Copy selected text to clipboard."""
        self._text_area.copy()

    def clear_selection(self) -> None:
        """Clear any text selection in this message."""
        cursor = self._text_area.textCursor()
        cursor.clearSelection()
        self._text_area.setTextCursor(cursor)

    def resizeEvent(self, event: QResizeEvent) -> None:
        """Handle resize events."""
        super().resizeEvent(event)

    def apply_style(self) -> None:
        """Apply style changes."""
        factor = self._style_manager.zoom_factor()
        font = self.font()
        base_font_size = self._style_manager.base_font_size()
        font.setPointSizeF(base_font_size * factor)
        self.setFont(font)

        # Apply font to components
        self._level_label.setFont(font)

        self._text_area.apply_style()

    def find_text(self, text: str) -> List[Tuple[int, int]]:
        """
        Find all instances of text in this message.

        Args:
            text: Text to search for

        Returns:
            List of (start_position, end_position) tuples for each match
        """
        document = self._text_area.document()
        matches = []
        cursor = QTextCursor(document)

        while True:
            cursor = document.find(text, cursor)
            if cursor.isNull():
                break

            matches.append((cursor.selectionStart(), cursor.selectionEnd()))

        return matches

    def highlight_matches(
        self,
        matches: List[Tuple[int, int]],
        current_match_index: int = -1,
        highlight_color: QColor | None = None,
        dim_highlight_color: QColor | None = None
    ) -> None:
        """
        Highlight matches in this message.

        Args:
            matches: List of (start, end) tuples to highlight
            current_match_index: Index of current match to highlight differently, or -1 for none
            highlight_color: QColor for current match, defaults to system highlight color
            dim_highlight_color: QColor for other matches, defaults to dimmer highlight color
        """
        # Default colors if not provided
        if not highlight_color:
            highlight_color = self._style_manager.get_color(ColorRole.TEXT_FOUND)

        if not dim_highlight_color:
            dim_highlight_color = self._style_manager.get_color(ColorRole.TEXT_FOUND_DIM)

        # Create format for current match
        current_format = QTextCharFormat()
        current_format.setBackground(highlight_color)

        # Create format for other matches
        other_format = QTextCharFormat()
        other_format.setBackground(dim_highlight_color)

        # Create selections
        selections = []
        for i, (start, end) in enumerate(matches):
            cursor = QTextCursor(self._text_area.document())
            cursor.setPosition(start)
            cursor.setPosition(end, QTextCursor.MoveMode.KeepAnchor)

            extra_selection = QTextEdit.ExtraSelection()
            extra_selection.cursor = cursor  # type: ignore
            extra_selection.format = current_format if i == current_match_index else other_format  # type: ignore

            selections.append(extra_selection)

        self._text_area.setExtraSelections(selections)

    def clear_highlights(self) -> None:
        """Clear all highlights from the message."""
        self._text_area.setExtraSelections([])

    def select_and_scroll_to_position(self, position: int) -> QPoint:
        """
        Select text at a specific position and return the cursor position relative to this widget.

        Args:
            position: Text position to scroll to

        Returns:
            QPoint: Position of the cursor relative to this widget
        """
        cursor = QTextCursor(self._text_area.document())
        cursor.setPosition(position)
        self._text_area.setTextCursor(cursor)

        # Get cursor rectangle in text area coordinates
        cursor_rect = self._text_area.cursorRect(cursor)

        # Convert to position relative to this widget
        local_pos = self._text_area.mapTo(self, cursor_rect.topLeft())

        return local_pos

    def text_area(self) -> LogTextEdit:
        """Get the text area widget."""
        return self._text_area
