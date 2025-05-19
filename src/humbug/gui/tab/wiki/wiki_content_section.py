"""Widget for displaying a section of wiki content."""

import logging
from typing import List, Tuple, cast, Optional

from PySide6.QtWidgets import (
    QVBoxLayout, QFrame, QLabel, QHBoxLayout, QWidget
)
from PySide6.QtCore import Signal, Qt, QPoint, QObject, QEvent
from PySide6.QtGui import (
    QCursor, QMouseEvent, QTextCursor, QTextCharFormat, QColor, QFont
)

from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager
from humbug.gui.markdown_block_data import HeadingBlockData
from humbug.gui.markdown_renderer import MarkdownRenderer
from humbug.gui.markdown_text_edit import MarkdownTextEdit
from humbug.gui.tab.conversation.conversation_highlighter import ConversationHighlighter
from humbug.gui.tab.conversation.conversation_language_highlighter import ConversationLanguageHighlighter
from humbug.language.language_manager import LanguageManager
from humbug.markdown.markdown_ast_node import MarkdownASTNode, MarkdownTextNode
from humbug.syntax.programming_language import ProgrammingLanguage
from humbug.syntax.programming_language_utils import ProgrammingLanguageUtils


class WikiContentSection(QFrame):
    """Widget for displaying a section of wiki content with markdown support."""

    selectionChanged = Signal(bool)
    scrollRequested = Signal(QPoint)
    mouseReleased = Signal()
    linkClicked = Signal(str)

    def __init__(
        self,
        is_input: bool,
        language: ProgrammingLanguage | None = None,
        parent: QWidget | None = None
    ) -> None:
        """
        Initialize a content section widget.

        Args:
            is_input: Whether this section is for user input (always False for wiki)
            language: Optional programming language for this section
            parent: Optional parent widget
        """
        super().__init__(parent)
        self.setFrameStyle(QFrame.Shape.Box | QFrame.Shadow.Plain)

        self._logger = logging.getLogger("WikiContentSection")
        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._handle_language_changed)

        self._style_manager = StyleManager()

        self._layout = QVBoxLayout(self)
        self.setLayout(self._layout)
        spacing = int(self._style_manager.message_bubble_spacing())
        self._layout.setSpacing(spacing)
        self._layout.setContentsMargins(0, 0, 0, 0)

        # Create language header if needed
        self._language = language
        self._language_header = None
        self._header_container = None

        if language is not None:
            self._layout.setContentsMargins(spacing, spacing, spacing, spacing)

            # Create a container for header (language label only, no buttons)
            self._header_container = QWidget()
            self._header_layout = QHBoxLayout(self._header_container)
            self._header_layout.setContentsMargins(0, 0, 0, 0)
            self._header_layout.setSpacing(4)

            # Add language label on the left
            self._language_header = QLabel()
            self._language_header.setAlignment(Qt.AlignmentFlag.AlignLeft)
            self._header_layout.addWidget(self._language_header)

            # Add stretch to fill remaining space
            self._header_layout.addStretch()

            # Add header container to main layout
            self._layout.addWidget(self._header_container)

        self._is_input = is_input  # Always False for wiki

        # Determine if this section should use markdown (always true if no language)
        self._use_markdown = language is None

        # Create text area
        self._text_area = MarkdownTextEdit()
        self._text_area.setAcceptRichText(self._use_markdown)
        self._text_area.setReadOnly(True)  # Always read-only for wiki
        self._text_area.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self._text_area.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)

        # Disable the standard context menu as our parent widget will handle that
        self._text_area.setContextMenuPolicy(Qt.ContextMenuPolicy.NoContextMenu)

        self._layout.addWidget(self._text_area)

        self._content_node: MarkdownASTNode | None = None

        # Render directly to the document
        document = self._text_area.document()
        self._renderer = MarkdownRenderer(document)

        # Connect signals
        self._text_area.selectionChanged.connect(self._on_selection_changed)
        self._text_area.mousePressed.connect(self._on_mouse_pressed)
        self._text_area.mouseReleased.connect(self._on_mouse_released)
        self._text_area.linkClicked.connect(self.linkClicked)

        # Add mouse move tracking for cursor changes on links
        self._text_area.viewport().setMouseTracking(True)
        self._text_area.viewport().installEventFilter(self)

        # Add appropriate highlighter
        self._highlighter: ConversationHighlighter | ConversationLanguageHighlighter | None = None
        self.set_language(language)

        self._mouse_left_button_pressed = False

        self._init_colour_mode = self._style_manager.color_mode()

        self._handle_language_changed()

    def text_area(self) -> MarkdownTextEdit:
        """Get the text area widget."""
        return self._text_area

    def language(self) -> ProgrammingLanguage | None:
        """Provide the language in use by this section."""
        return self._language

    def set_language(self, language: ProgrammingLanguage | None) -> None:
        """Set the programming language to use for this section"""
        self._language = language

        if language is None:
            self._use_markdown = True
            self._highlighter = None
        else:
            self._use_markdown = False
            highlighter = ConversationLanguageHighlighter(self._text_area.document())
            highlighter.set_language(language)
            self._highlighter = highlighter
            self._text_area.set_has_code_block(True)

        strings = self._language_manager.strings()
        if self._language_header:
            language_header = strings.highlighting.format(
                language=ProgrammingLanguageUtils.get_display_name(cast(ProgrammingLanguage, self._language))
            )
            self._language_header.setText(language_header)

    def _handle_language_changed(self) -> None:
        """Update text when language changes."""
        strings = self._language_manager.strings()

        if self._language_header:
            language_header = strings.highlighting.format(
                language=ProgrammingLanguageUtils.get_display_name(cast(ProgrammingLanguage, self._language))
            )
            self._language_header.setText(language_header)

    def _on_mouse_pressed(self, event: QMouseEvent) -> None:
        """Handle mouse press from text area."""
        if event.buttons() == Qt.MouseButton.LeftButton:
            self._mouse_left_button_pressed = True

    def _on_mouse_released(self, _event: QMouseEvent) -> None:
        """Handle mouse release from text area."""
        self._mouse_left_button_pressed = False
        self.mouseReleased.emit()

    def eventFilter(self, watched: QObject, event: QEvent) -> bool:
        """
        Filter events for the text area viewport.

        Args:
            watched: The object being watched
            event: The event that occurred

        Returns:
            True if the event was handled, False to pass it along
        """
        # Handle mouse move events for the text area viewport
        if watched == self._text_area.viewport() and event.type() == QEvent.Type.MouseMove:
            mouse_event = cast(QMouseEvent, event)
            self._handle_mouse_move(mouse_event)
            # Return False to allow normal processing
            return False

        # Pass all other events to the parent class
        return super().eventFilter(watched, event)

    def _handle_mouse_move(self, event: QMouseEvent) -> None:
        """
        Handle mouse movement to update cursor for links.

        Args:
            event: The mouse event
        """
        # Check if the mouse is over a link
        url = self._text_area.anchorAt(event.pos())

        # Update cursor shape based on whether we're hovering over a link
        if url:
            self._text_area.viewport().setCursor(Qt.CursorShape.PointingHandCursor)

        else:
            self._text_area.viewport().setCursor(Qt.CursorShape.IBeamCursor)

    def _on_selection_changed(self) -> None:
        """Handle selection changes in the text area."""
        cursor = self._text_area.textCursor()
        has_selection = cursor.hasSelection()

        if has_selection and self._mouse_left_button_pressed:
            # Emit global mouse position for accurate scroll calculations
            self.scrollRequested.emit(QCursor.pos())

        self.selectionChanged.emit(has_selection)

    def set_content(self, content: MarkdownASTNode) -> None:
        """
        Set the content of this section.

        Args:
            content: A MarkdownASTNode text content
        """
        # If we have code block node, extract its content as plain text
        if not self._use_markdown:
            text_content = cast(MarkdownTextNode, content)
            self._text_area.set_text(text_content.content)
            return

        # Store for re-styling
        self._content_node = content
        self._renderer.visit(content)

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

    def find_element_by_id(self, element_id: str) -> Optional[Tuple[int, int]]:
        """
        Find an element with the given ID.

        Args:
            element_id: The ID to search for

        Returns:
            Tuple of (block_number, position) if found, None otherwise
        """
        document = self._text_area.document()

        # Iterate through all blocks looking for matching elements
        for block_num in range(document.blockCount()):
            block = document.findBlockByNumber(block_num)

            user_data = block.userData()
            if isinstance(user_data, HeadingBlockData) and user_data.element_id == element_id:
                return (block_num, block.position())

        return None

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

            extra_selection = self._text_area.ExtraSelection()
            extra_selection.cursor = cursor  # type: ignore
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
        # Style the frame
        self.setStyleSheet(f"""
            QFrame {{
                background-color: {background_color};
                margin: 0;
                border-radius: {int(self._style_manager.message_bubble_spacing())}px;
                border: 0;
            }}
        """)

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

        # Style the language header container if present
        if self._header_container:
            self._header_container.setStyleSheet(f"""
                QWidget {{
                    background-color: {background_color};
                    margin: 0;
                    padding: 0;
                }}
            """)

        # Style the language header if present, or the inline code style if it's not
        if self._language_header:
            label_color = self._style_manager.get_color_str(ColorRole.MESSAGE_LANGUAGE)
            self._language_header.setFont(font)
            self._language_header.setStyleSheet(f"""
                QLabel {{
                    color: {label_color};
                    background-color: {background_color};
                    margin: 0;
                    padding: 0;
                }}
            """)
        else:
            if self._content_node:
                self._renderer.visit(self._content_node)

        # If we changed colour mode then re-highlight
        if self._style_manager.color_mode() != self._init_colour_mode:
            self._init_colour_mode = self._style_manager.color_mode()
            if self._highlighter:
                self._highlighter.rehighlight()
