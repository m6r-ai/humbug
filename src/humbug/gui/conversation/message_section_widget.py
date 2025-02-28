from typing import List, Tuple

from PySide6.QtWidgets import QVBoxLayout, QWidget, QTextEdit
from PySide6.QtCore import Signal, Qt, QPoint
from PySide6.QtGui import QCursor, QMouseEvent, QTextCursor, QTextCharFormat

from humbug.gui.conversation.conversation_highlighter import ConversationHighlighter
from humbug.gui.conversation.conversation_text_edit import ConversationTextEdit
from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager


class MessageSectionWidget(QWidget):
    """Widget for displaying a section of a message."""

    selectionChanged = Signal(bool)
    scrollRequested = Signal(QPoint)
    mouseReleased = Signal()
    codeBlockStateChanged = Signal(bool)

    def __init__(self, parent=None):
        """
        Initialize a message section widget.

        Args:
            parent: Optional parent widget
        """
        super().__init__(parent)
        self._layout = QVBoxLayout(self)
        self._layout.setContentsMargins(0, 0, 0, 0)
        self._layout.setSpacing(0)
        self.setLayout(self._layout)

        # Create text area
        self._text_area = self._create_text_area()
        self._layout.addWidget(self._text_area)

        # Connect signals
        self._text_area.selectionChanged.connect(self._on_selection_changed)
        self._text_area.mousePressed.connect(self._on_mouse_pressed)
        self._text_area.mouseReleased.connect(self._on_mouse_released)

        # Add conversation highlighter
        self._highlighter = ConversationHighlighter(self._text_area.document())
        self._highlighter.codeBlockStateChanged.connect(self._on_code_block_state_changed)

        self._mouse_left_button_pressed = False
        self._style_manager = StyleManager()

    def _create_text_area(self) -> ConversationTextEdit:
        """
        Create and configure the text area.

        Returns:
            Configured ConversationTextEdit instance
        """
        text_area = ConversationTextEdit()
        text_area.setAcceptRichText(False)
        text_area.setReadOnly(True)
        text_area.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        text_area.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOff)

        # Disable the standard context menu as our parent widget will handle that
        text_area.setContextMenuPolicy(Qt.NoContextMenu)
        return text_area

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

    def _on_code_block_state_changed(self, has_code_block: bool):
        """Handle changes in code block state."""
        self._text_area.set_has_code_block(has_code_block)
        self.codeBlockStateChanged.emit(has_code_block)
        # Ensure proper scroll behavior
        self.updateGeometry()

    def set_content(self, text: str):
        """
        Set the content of this section.

        Args:
            text: The text content for this section
        """
        self._text_area.set_incremental_text(text)

    def has_selection(self) -> bool:
        """Check if text is selected in the text area."""
        return self._text_area.textCursor().hasSelection()

    def get_selected_text(self) -> str:
        """
        Get any selected text in this section.

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

    def clear_selection(self):
        """Clear any text selection in this section."""
        cursor = self._text_area.textCursor()
        cursor.clearSelection()
        self._text_area.setTextCursor(cursor)

    def has_code_block(self) -> bool:
        """Check if this section contains a code block."""
        return self._text_area.has_code_block()

    def find_text(self, text: str) -> List[Tuple[int, int]]:
        """
        Find all instances of text in this section.

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
        highlight_color=None,
        dim_highlight_color=None
    ):
        """
        Highlight matches in this section.

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
        """Clear all highlights from the section."""
        self._text_area.setExtraSelections([])

    def select_and_scroll_to_position(self, position: int) -> QPoint:
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
        return self._text_area.mapTo(self.parentWidget().parentWidget(), local_pos)

    def apply_style(self, text_color: str, background_color: str, font):
        """
        Apply styling to this section.

        Args:
            text_color: Color string for text
            background_color: Color string for background
            font: Font to use
        """
        self._text_area.setFont(font)
        self._text_area.setStyleSheet(f"""
            QTextEdit {{
                color: {text_color};
                selection-background-color: {self._style_manager.get_color_str(ColorRole.TEXT_SELECTED)};
                border: none;
                border-radius: 0;
                padding: 1px;
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
