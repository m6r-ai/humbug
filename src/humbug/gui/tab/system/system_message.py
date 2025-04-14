"""Widget for displaying a single message in the system history with header."""

from datetime import datetime
import logging
from typing import Dict, List, Tuple

from PySide6.QtWidgets import (
    QFrame, QVBoxLayout, QLabel, QHBoxLayout, QWidget, QTextEdit
)
from PySide6.QtCore import Signal, QPoint, Qt
from PySide6.QtGui import (
    QResizeEvent, QColor, QCursor, QMouseEvent, QTextCursor,
    QTextCharFormat, QFont
)

from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager
from humbug.gui.tab.system.system_text_edit import SystemTextEdit
from humbug.language.language_manager import LanguageManager


class SystemMessage(QFrame):
    """Widget for displaying a single message in the system history with header."""

    selectionChanged = Signal(bool)
    scrollRequested = Signal(QPoint)
    mouseReleased = Signal()

    def __init__(self, parent: QWidget | None = None, is_input: bool = False) -> None:
        """
        Initialize the message widget.

        Args:
            parent: Optional parent widget
            is_input: Whether this is an input widget (affects styling)
        """
        super().__init__(parent)
        self.setFrameStyle(QFrame.Shape.Box | QFrame.Shadow.Plain)
        self._is_input = is_input

        self._logger = logging.getLogger("SystemMessage")

        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._handle_language_changed)

        # Will store the actual message source
        self._message_source: str | None = None
        self._message_timestamp: datetime | None = None
        self._message_content = ""
        self._message_model = ""

        # Create layout
        self._layout = QVBoxLayout(self)
        self.setLayout(self._layout)
        self._layout.setSpacing(10)
        self._layout.setContentsMargins(10, 10, 10, 10)

        # Create header area with horizontal layout
        self._header = QWidget(self)
        self._header_layout = QHBoxLayout(self._header)
        self._header_layout.setContentsMargins(0, 0, 0, 0)
        self._header_layout.setSpacing(4)

        # Create role and timestamp labels
        self._role_label = QLabel(self)
        self._header_layout.addWidget(self._role_label)
        self._header_layout.addStretch()

        # Add header widget to main layout
        self._layout.addWidget(self._header)

        # Create text area
        self._text_area = SystemTextEdit()
        self._text_area.setAcceptRichText(False)
        self._text_area.setReadOnly(not is_input)
        self._text_area.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self._text_area.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        
        # Disable the standard context menu as our parent widget will handle that
        self._text_area.setContextMenuPolicy(Qt.ContextMenuPolicy.NoContextMenu)
        
        # Connect signals from text area
        self._text_area.selectionChanged.connect(self._on_selection_changed)
        self._text_area.mousePressed.connect(self._on_mouse_pressed)
        self._text_area.mouseReleased.connect(self._on_mouse_released)
        
        self._layout.addWidget(self._text_area)

        self._is_focused = False
        self._current_style: str | None = None
        self._mouse_left_button_pressed = False

        self._style_manager = StyleManager()
        self._style_manager.style_changed.connect(self._handle_style_changed)
        self._handle_style_changed()
        self._handle_language_changed()

    def is_focused(self) -> bool:
        """Check if this message is focused."""
        return self._is_focused

    def set_focused(self, focused: bool) -> None:
        """Set the focused state of this message."""
        if self._is_focused == focused:
            return

        self._is_focused = focused
        if focused:
            self.setFocus()

        self._handle_style_changed()

    def _handle_language_changed(self) -> None:
        """Update text when language changes."""
        if not self._is_input:
            # Don't update input widget headers
            self._update_role_text()

    def _update_role_text(self) -> None:
        """Update the role text based on current language."""
        if not self._message_source:
            return

        strings = self._language_manager.strings()

        # Map from message source to display text
        if self._message_source == "user":
            role_text = strings.role_you
        elif self._message_source == "system":
            role_text = strings.role_system
        else:
            # Default case - should not happen
            role_text = self._message_source.capitalize()

        # Format with timestamp
        if self._message_timestamp is not None:
            timestamp_str = self._message_timestamp.strftime("%Y-%m-%d %H:%M:%S.%f")[:-3]
            self._role_label.setText(f"{role_text} @ {timestamp_str}")
        else:
            self._role_label.setText(role_text)

    def _on_mouse_pressed(self, event: QMouseEvent) -> None:
        """Handle mouse press from text area."""
        if event.buttons() == Qt.MouseButton.LeftButton:
            self._mouse_left_button_pressed = True

    def _on_mouse_released(self, _event: QMouseEvent) -> None:
        """Handle mouse release from text area."""
        self._mouse_left_button_pressed = False
        self.mouseReleased.emit()

    def _on_selection_changed(self) -> None:
        """Handle selection changes in the text area."""
        cursor = self._text_area.textCursor()
        has_selection = cursor.hasSelection()

        if has_selection and self._mouse_left_button_pressed:
            # Emit global mouse position for accurate scroll calculations
            self.scrollRequested.emit(QCursor.pos())

        self.selectionChanged.emit(has_selection)

    def set_content(self, text: str, source: str, timestamp: datetime, model: str) -> None:
        """
        Set content with style, handling incremental updates for AI responses.

        Args:
            text: The message text content
            source: The source of the message ("user", "system", etc.)
            timestamp: datetime object for the message timestamp
            model: Model name if applicable (can be empty for system messages)
        """
        self._message_source = source
        self._message_timestamp = timestamp
        self._message_content = text
        self._message_model = model

        # Set the content in the text area
        self._text_area.set_text(text)

        # Update the header
        self._update_role_text()
        self._set_role_style()

    def _set_role_style(self) -> None:
        """Set the role label color based on message source."""
        # Map message source to color role
        if self._message_source == "user":
            colour = ColorRole.MESSAGE_USER
        elif self._message_source == "system":
            colour = ColorRole.MESSAGE_SYSTEM
        else:
            # Default case
            colour = ColorRole.TEXT_PRIMARY

        # Warning: This needs to stay in sync with SystemInput
        self._role_label.setStyleSheet(f"""
            QLabel {{
                color: {self._style_manager.get_color_str(colour)};
                margin: 0;
                padding: 0;
                background-color: {self._style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
            }}
        """)

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

    def _handle_style_changed(self) -> None:
        """Handle the style changing"""
        factor = self._style_manager.zoom_factor()
        font = self.font()
        base_font_size = self._style_manager.base_font_size()
        font.setPointSizeF(base_font_size * factor)
        self.setFont(font)

        # Map message types to role colors
        role_colours = {
            "user": ColorRole.MESSAGE_USER,
            "system": ColorRole.MESSAGE_SYSTEM
        }

        current_style = self._message_source or "user"
        role = role_colours.get(current_style, ColorRole.MESSAGE_USER)
        label_color = self._style_manager.get_color_str(role)
        background_color = self._style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)
        text_color = self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)

        # Role label styling (bold)
        self._role_label.setFont(font)
        self._role_label.setStyleSheet(f"""
            QLabel {{
                color: {label_color};
                margin: 0;
                padding: 0;
                background-color: {background_color};
            }}
        """)

        # Header widget styling
        self._header.setStyleSheet(f"""
            QWidget {{
                border: none;
                border-radius: 0;
                padding: 0;
                margin: 0;
                background-color: {background_color};
            }}
        """)

        # Apply styling to text area
        self._text_area.setFont(font)
        self._text_area.setStyleSheet(f"""
            QTextEdit {{
                color: {text_color};
                selection-background-color: {self._style_manager.get_color_str(ColorRole.TEXT_SELECTED)};
                border: none;
                border-radius: 0;
                padding: 0;
                margin: 0;
                background-color: {background_color};
            }}
            QScrollBar:horizontal {{
                height: 12px;
                background: {self._style_manager.get_color_str(ColorRole.SCROLLBAR_BACKGROUND)};
            }}
            QScrollBar::handle:horizontal {{
                background: {self._style_manager.get_color_str(ColorRole.SCROLLBAR_HANDLE)};
                min-width: 20px;
            }}
            QScrollBar::add-page:horizontal, QScrollBar::sub-page:horizontal {{
                background: none;
            }}
            QScrollBar::add-line:horizontal, QScrollBar::sub-line:horizontal {{
                width: 0px;
            }}
        """)

        # Determine border color based on state
        border = ColorRole.MESSAGE_FOCUSED if self._is_focused and self.hasFocus() else \
                ColorRole.MESSAGE_BACKGROUND

        self.setStyleSheet(f"""
            QWidget {{
                background-color: {background_color};
            }}
            QFrame {{
                background-color: {background_color};
                margin: 0;
                border-radius: 8px;
                border: 2px solid {self._style_manager.get_color_str(border)}
            }}
        """)

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
            extra_selection.cursor = cursor
            extra_selection.format = current_format if i == current_match_index else other_format

            selections.append(extra_selection)

        self._text_area.setExtraSelections(selections)

    def clear_highlights(self) -> None:
        """Clear all highlights from the message."""
        self._text_area.setExtraSelections([])

    def select_and_scroll_to_position(self, section_num: int, position: int) -> QPoint:
        """
        Select text at a specific position and return the cursor position relative to this widget.

        Args:
            section_num: Ignored (kept for backward compatibility)
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

    def text_area(self) -> SystemTextEdit:
        """Get the text area widget."""
        return self._text_area

    def has_code_block(self) -> bool:
        """Check if this section contains a code block."""
        return self._text_area.has_code_block()
