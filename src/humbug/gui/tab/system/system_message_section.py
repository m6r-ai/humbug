"""Widget for displaying a section of a message."""

import logging
from typing import List, Tuple

from PySide6.QtWidgets import QVBoxLayout, QFrame, QTextEdit, QWidget
from PySide6.QtCore import Signal, Qt, QPoint
from PySide6.QtGui import (
    QCursor, QMouseEvent, QTextCursor, QTextCharFormat, QColor, QFont
)

from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager
from humbug.gui.tab.system.system_text_edit import SystemTextEdit


class SystemMessageSection(QFrame):
    """Widget for displaying a section of a message with markdown support."""

    selectionChanged = Signal(bool)
    scrollRequested = Signal(QPoint)
    mouseReleased = Signal()

    def __init__(
        self,
        is_input: bool,
        parent: QWidget | None = None
    ) -> None:
        """
        Initialize a message section widget.

        Args:
            is_input: Whether this section is for user input
            language: Optional programming language for this section
            parent: Optional parent widget
        """
        super().__init__(parent)
        self.setFrameStyle(QFrame.Shape.Box | QFrame.Shadow.Plain)

        self._logger = logging.getLogger("SystemMessageSection")

        self._layout = QVBoxLayout(self)
        self.setLayout(self._layout)
        self._layout.setSpacing(10)
        self._layout.setContentsMargins(0, 0, 0, 0)

        self._is_input = is_input

        # Create text area
        self._text_area = SystemTextEdit()
        self._text_area.setAcceptRichText(False)
        self._text_area.setReadOnly(not is_input)
        self._text_area.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self._text_area.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)

        # Disable the standard context menu as our parent widget will handle that
        self._text_area.setContextMenuPolicy(Qt.ContextMenuPolicy.NoContextMenu)

        self._layout.addWidget(self._text_area)

        # Connect signals
        self._text_area.selectionChanged.connect(self._on_selection_changed)
        self._text_area.mousePressed.connect(self._on_mouse_pressed)
        self._text_area.mouseReleased.connect(self._on_mouse_released)

        self._mouse_left_button_pressed = False

        self._style_manager = StyleManager()
        self._init_colour_mode = self._style_manager.color_mode()

    def text_area(self) -> SystemTextEdit:
        """Get the text area widget."""
        return self._text_area

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

    def _on_code_block_state_changed(self, has_code_block: bool) -> None:
        """Handle changes in code block state."""
        self._text_area.set_has_code_block(has_code_block)

        # Ensure proper scroll behavior
        self.updateGeometry()

    def set_content(self, content: str) -> None:
        """
        Set the content of this section.

        Args:
            content: A string representing the content of this section
        """
        self._text_area.set_text(content)

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

    def copy_selection(self) -> None:
        """Copy selected text to clipboard."""
        self._text_area.copy()

    def clear_selection(self) -> None:
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
        highlight_color: QColor | None = None,
        dim_highlight_color: QColor | None = None
    ) -> None:
        """
        Highlight matches in this section.

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
            extra_selection.cursor = cursor # type: ignore
            extra_selection.format = current_format if i == current_match_index else other_format  # type: ignore

            selections.append(extra_selection)

        self._text_area.setExtraSelections(selections)

    def clear_highlights(self) -> None:
        """Clear all highlights from the section."""
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

    def apply_style(self, text_color: str, background_color: str, font: QFont) -> None:
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

        # Style the frame
        self.setStyleSheet(f"""
            QFrame {{
                background-color: {background_color};
                margin: 0;
                border-radius: 8px;
                border: 0;
            }}
        """)
