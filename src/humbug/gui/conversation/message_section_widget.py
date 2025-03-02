from typing import List, Tuple, Optional

from PySide6.QtWidgets import QVBoxLayout, QFrame, QTextEdit, QLabel
from PySide6.QtCore import Signal, Qt, QPoint
from PySide6.QtGui import QCursor, QMouseEvent, QTextCursor, QTextCharFormat

from humbug.gui.conversation.conversation_highlighter import ConversationHighlighter
from humbug.gui.conversation.conversation_language_highlighter import ConversationLanguageHighlighter
from humbug.gui.conversation.conversation_text_edit import ConversationTextEdit
from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager
from humbug.syntax.programming_language import ProgrammingLanguage


class MessageSectionWidget(QFrame):
    """Widget for displaying a section of a message."""

    selectionChanged = Signal(bool)
    scrollRequested = Signal(QPoint)
    mouseReleased = Signal()

    def __init__(self, is_input: bool, language: Optional[ProgrammingLanguage] = None, parent=None):
        """
        Initialize a message section widget.

        Args:
            is_input: Whether this section is for user input
            language: Optional programming language for this section
            parent: Optional parent widget
        """
        super().__init__(parent)
        self.setFrameStyle(QFrame.Box | QFrame.Plain)

        self._layout = QVBoxLayout(self)
        self.setLayout(self._layout)
        self._layout.setSpacing(10)
        self._layout.setContentsMargins(0, 0, 0, 0)

        # Create language header if needed
        self._language = language
        self._language_header = None
        if language is not None:
            self._layout.setContentsMargins(10, 10, 10, 10)
            self._language_header = QLabel(self._get_language_display_name(language))
            self._language_header.setAlignment(Qt.AlignLeft)
            self._layout.addWidget(self._language_header)

        # Create text area
        self._text_area = ConversationTextEdit()
        self._text_area.setAcceptRichText(False)
        self._text_area.setReadOnly(not is_input)
        self._text_area.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        self._text_area.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOff)

        # Disable the standard context menu as our parent widget will handle that
        self._text_area.setContextMenuPolicy(Qt.NoContextMenu)

        self._layout.addWidget(self._text_area)

        # Connect signals
        self._text_area.selectionChanged.connect(self._on_selection_changed)
        self._text_area.mousePressed.connect(self._on_mouse_pressed)
        self._text_area.mouseReleased.connect(self._on_mouse_released)

        # Add conversation highlighter
        if language is None:
            self._highlighter = ConversationHighlighter(self._text_area.document())
            self._highlighter.codeBlockStateChanged.connect(self._on_code_block_state_changed)
        else:
            self._highlighter = ConversationLanguageHighlighter(self._text_area.document())
            self._highlighter.set_language(language)
            self._text_area.set_has_code_block(True)

        self._mouse_left_button_pressed = False

        self._style_manager = StyleManager()
        self._init_colour_mode = self._style_manager.color_mode

    def _get_language_display_name(self, language: ProgrammingLanguage) -> str:
        """
        Convert a ProgrammingLanguage enum to a display name.

        Args:
            language: ProgrammingLanguage enum value

        Returns:
            Human-readable language name
        """
        language_display_names = {
            ProgrammingLanguage.C: "C",
            ProgrammingLanguage.CPP: "C++",
            ProgrammingLanguage.CSS: "CSS",
            ProgrammingLanguage.GO: "Go",
            ProgrammingLanguage.HTML: "HTML",
            ProgrammingLanguage.JAVA: "Java",
            ProgrammingLanguage.JAVASCRIPT: "JavaScript",
            ProgrammingLanguage.JSON: "JSON",
            ProgrammingLanguage.KOTLIN: "Kotlin",
            ProgrammingLanguage.METAPHOR: "Metaphor",
            ProgrammingLanguage.MOVE: "Move",
            ProgrammingLanguage.PYTHON: "Python",
            ProgrammingLanguage.RUST: "Rust",
            ProgrammingLanguage.SCHEME: "Scheme",
            ProgrammingLanguage.SWIFT: "Swift",
            ProgrammingLanguage.TYPESCRIPT: "TypeScript",
            ProgrammingLanguage.TEXT: "Plain Text"
        }
        return language_display_names.get(language, "Code")

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

        # Style the language header if present
        if self._language_header:
            label_color = self._style_manager.get_color_str(ColorRole.MESSAGE_LANGUAGE)
            self._language_header.setFont(font)
            self._language_header.setStyleSheet(f"""
                QLabel {{
                    font-weight: bold;
                    color: {label_color};
                    background-color: {background_color};
                    margin: 0;
                    padding: 0;
                }}
            """)

        # If we changed colour mode then re-highlight
        if self._style_manager.color_mode != self._init_colour_mode:
            self._init_colour_mode = self._style_manager.color_mode
            self._highlighter.rehighlight()
