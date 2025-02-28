"""Widget for displaying individual conversation messages."""

from datetime import datetime
from typing import List, Tuple

from PySide6.QtWidgets import QFrame, QVBoxLayout, QLabel, QHBoxLayout, QWidget, QTextEdit
from PySide6.QtCore import Signal, Qt, QPoint
from PySide6.QtGui import QCursor, QMouseEvent, QTextCursor, QTextCharFormat

from humbug.conversation.message_source import MessageSource
from humbug.gui.conversation.conversation_highlighter import ConversationHighlighter
from humbug.gui.conversation.conversation_text_edit import ConversationTextEdit
from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager
from humbug.language.language_manager import LanguageManager


class MessageWidget(QFrame):
    """Widget for displaying a single message in the conversation history with header."""

    selectionChanged = Signal(bool)
    scrollRequested = Signal(QPoint)
    mouseReleased = Signal()

    def __init__(self, parent=None, is_input=False):
        """Initialize the message widget.

        Args:
            parent: Optional parent widget
            is_input: Whether this is an input widget (affects styling)
        """
        super().__init__(parent)
        self.setFrameStyle(QFrame.Box | QFrame.Plain)
        self._is_input = is_input

        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._handle_language_changed)
        self._message_source = None
        self._message_timestamp = None

        self._mouse_left_button_pressed = False

        # Create layout
        self._layout = QVBoxLayout(self)
        self.setLayout(self._layout)
        self._layout.setSpacing(8)
        self._layout.setContentsMargins(8, 8, 8, 8)

        # Create header area with horizontal layout
        self._header = QWidget(self)
        self._header_layout = QHBoxLayout(self._header)
        self._header_layout.setContentsMargins(1, 1, 1, 1)
        self._header_layout.setSpacing(0)

        # Create role and timestamp labels
        self._role_label = QLabel(self)
        self._timestamp_label = QLabel(self)
        self._header_layout.addWidget(self._role_label)
        self._header_layout.addWidget(self._timestamp_label)
        self._header_layout.addStretch()

        # Add header widget to main layout
        self._layout.addWidget(self._header)

        # Create content area using custom ConversationTextEdit
        self._text_area = self._create_text_area()
        self._text_area.setReadOnly(not self._is_input)

        # Add text area to main layout
        self._layout.addWidget(self._text_area)

        # Connect selection change signal
        self._text_area.selectionChanged.connect(self._on_selection_changed)
        self._text_area.mousePressed.connect(self._on_mouse_pressed)
        self._text_area.mouseReleased.connect(self._on_mouse_released)

        # Add conversation highlighter
        self._highlighter = ConversationHighlighter(self._text_area.document())
        self._highlighter.codeBlockStateChanged.connect(self._on_code_block_state_changed)

        self._language_manager = LanguageManager()

        # Get style manager
        self._style_manager = StyleManager()
        self._init_colour_mode = self._style_manager.color_mode

        # Add bookmark status
        self._is_bookmarked = False

        # Track current message style
        self._current_style: MessageSource = None

        self._style_manager.style_changed.connect(self._handle_style_changed)
        self._handle_style_changed()

    def _create_text_area(self) -> ConversationTextEdit:
        """Create and configure the text area.

        Returns:
            Configured ConversationTextEdit instance
        """
        text_area = ConversationTextEdit()
        text_area.setAcceptRichText(False)
        text_area.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        text_area.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOff)

        # Disable the standard context menu as our parent widget will handle that
        text_area.setContextMenuPolicy(Qt.NoContextMenu)
        return text_area

    def is_bookmarked(self) -> bool:
        """Check if this message is bookmarked."""
        return self._is_bookmarked

    def set_bookmarked(self, bookmarked: bool):
        """Set the bookmarked state."""
        self._is_bookmarked = bookmarked
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

        strings = self._language_manager.strings
        role_text = {
            MessageSource.USER: strings.role_you,
            MessageSource.AI: strings.role_assistant,
            MessageSource.REASONING: strings.role_reasoning,
            MessageSource.SYSTEM: strings.role_system
        }.get(self._message_source, "Unknown")

        # Format with timestamp
        if self._message_timestamp:
            timestamp_str = self._message_timestamp.strftime("%Y-%m-%d %H:%M:%S.%f")[:-3]
            self._role_label.setText(f"{role_text} @ {timestamp_str}")
        else:
            self._role_label.setText(role_text)

    def set_content(self, text: str, style: MessageSource, timestamp: datetime):
        """Set content with style, handling incremental updates for AI responses.

        Args:
            text: The message text content
            style: The style type ('user', 'ai', 'system', or 'error')
            timestamp: datetime object for the message timestamp
        """
        self._message_source = style
        self._message_timestamp = timestamp

        if style != self._current_style:
            # Update header text with proper role
            self._update_role_text()
            self._current_style = style
            self._handle_style_changed()
            self._text_area.clear()

        self._text_area.set_incremental_text(text)

    def _on_mouse_pressed(self, event: QMouseEvent):
        """Handle mouse press from text area."""
        if event.buttons() == Qt.MouseButton.LeftButton:
            self._mouse_left_button_pressed = True

    def _on_mouse_released(self, _event: QMouseEvent):
        """Handle mouse release from text area."""
        self._mouse_left_button_pressed = False
        self.mouseReleased.emit()

    def _on_selection_changed(self):
        """Handle selection changes in the text area."""
        cursor = self._text_area.textCursor()
        has_selection = cursor.hasSelection()

        if has_selection and self._mouse_left_button_pressed:
            # Emit global mouse position for accurate scroll calculations
            self.scrollRequested.emit(QCursor.pos())

        self.selectionChanged.emit(has_selection)

    def has_selection(self) -> bool:
        """Check if text is selected in the text area."""
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

    def copy_selection(self):
        """Copy selected text to clipboard."""
        self._text_area.copy()

    def _on_code_block_state_changed(self, has_code_block: bool):
        """Handle changes in code block state."""
        self._text_area.set_has_code_block(has_code_block)
        # Ensure proper scroll behavior
        self.updateGeometry()

    def resizeEvent(self, event):
        """Handle resize events."""
        super().resizeEvent(event)

        # If we have code blocks, allow horizontal scrolling
        if self._text_area.has_code_block():
            self._text_area.setHorizontalScrollBarPolicy(Qt.ScrollBarAsNeeded)
        else:
            self._text_area.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)

    def clear_selection(self):
        """Clear any text selection in this message."""
        cursor = self._text_area.textCursor()
        cursor.clearSelection()
        self._text_area.setTextCursor(cursor)

    def _handle_style_changed(self):
        """Handle the style changing"""
        factor = self._style_manager.zoom_factor
        font = self.font()
        base_font_size = self._style_manager.base_font_size
        font.setPointSizeF(base_font_size * factor)
        self.setFont(font)

        # Map message types to role colors
        role_colours = {
            MessageSource.USER: ColorRole.MESSAGE_USER,
            MessageSource.AI: ColorRole.MESSAGE_AI,
            MessageSource.REASONING: ColorRole.MESSAGE_REASONING,
            MessageSource.SYSTEM: ColorRole.MESSAGE_SYSTEM
        }

        role = role_colours.get(self._current_style, ColorRole.MESSAGE_USER)
        label_color = self._style_manager.get_color_str(role)

        # Role label styling (bold)
        self._role_label.setFont(font)
        self._role_label.setStyleSheet(f"""
            QLabel {{
                font-weight: bold;
                color: {label_color};
                margin: 0;
                padding: 0;
                background-color: {self._style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
            }}
        """)

        # Timestamp label styling (normal weight)
        self._timestamp_label.setFont(font)
        self._timestamp_label.setStyleSheet(f"""
            QLabel {{
                font-weight: normal;
                color: {label_color};
                padding: 0;
                margin: 0;
                background-color: {self._style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
            }}
        """)

        # Header widget styling
        self._header.setStyleSheet(f"""
            QWidget {{
                border: none;
                border-radius: 0;
                padding: 1px;
                margin: 0;
                background-color: {self._style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
            }}
        """)

        # Content area styling
        self._text_area.setFont(font)
        self._text_area.setStyleSheet(f"""
            QTextEdit {{
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                selection-background-color: {self._style_manager.get_color_str(ColorRole.TEXT_SELECTED)};
                border: none;
                border-radius: 0;
                padding: 1px;
                margin: 0;
                background-color: {self._style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
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

        # Main frame styling
        border = ColorRole.MESSAGE_BOOKMARK if self._is_bookmarked else ColorRole.MESSAGE_BACKGROUND
        self.setStyleSheet(f"""
            QFrame {{
                background-color: {self._style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
                margin: 0;
                border-radius: 8px;
                border: 2px solid {self._style_manager.get_color_str(border)}
            }}
        """)

        # If we changed colour mode then re-highlight
        if self._style_manager.color_mode != self._init_colour_mode:
            self._init_colour_mode = self._style_manager.color_mode
            self._highlighter.rehighlight()

    def find_text(self, text: str) -> List[Tuple[int, int]]:
        """
        Find all instances of text in this message.

        Args:
            text: Text to search for

        Returns:
            List of (start_position, end_position) tuples for each match
        """
        # Implementation uses _text_area internally but doesn't expose it
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
        highlight_color=None,
        dim_highlight_color=None
    ):
        """
        Highlight matches in this message.

        Args:
            matches: List of (start, end) tuples to highlight
            current_match_index: Index of current match to highlight differently, or -1 for none
            highlight_color: QColor for current match, defaults to system highlight color
            dim_highlight_color: QColor for other matches, defaults to dimmer highlight color
        """
        # Default colors if not provided
        style_manager = StyleManager()
        if not highlight_color:
            highlight_color = style_manager.get_color(ColorRole.TEXT_FOUND)

        if not dim_highlight_color:
            dim_highlight_color = style_manager.get_color(ColorRole.TEXT_FOUND_DIM)

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
            cursor.setPosition(end, QTextCursor.KeepAnchor)

            extra_selection = QTextEdit.ExtraSelection()
            extra_selection.cursor = cursor
            extra_selection.format = current_format if i == current_match_index else other_format

            selections.append(extra_selection)

        self._text_area.setExtraSelections(selections)

    def clear_highlights(self):
        """Clear all highlights from the message."""
        self._text_area.setExtraSelections([])

    def select_and_scroll_to_position(self, position: int):
        """
        Select text and scroll to a specific position.

        Args:
            position: Text position to scroll to

        Returns:
            QPoint: The global position of the visible cursor (for scrolling in parent)
        """
        cursor = QTextCursor(self._text_area.document())
        cursor.setPosition(position)
        self._text_area.setTextCursor(cursor)

        # Get cursor rectangle and convert to global position for parent scrolling
        cursor_rect = self._text_area.cursorRect(cursor)
        local_pos = cursor_rect.topLeft()
        return self._text_area.mapTo(self.parentWidget(), local_pos)
