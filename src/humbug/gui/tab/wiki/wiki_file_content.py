"""Widget for displaying source code content in the wiki."""

import logging
from typing import List, Tuple, Callable

from PySide6.QtWidgets import (
    QVBoxLayout, QWidget, QHBoxLayout, QLabel, QToolButton
)
from PySide6.QtCore import QPoint, Qt, QSize
from PySide6.QtGui import (
    QCursor, QMouseEvent, QTextCursor, QTextCharFormat, QColor, QIcon
)

from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager
from humbug.gui.tab.wiki.wiki_content import WikiContent
from humbug.gui.markdown_text_edit import MarkdownTextEdit
from humbug.gui.tab.conversation.conversation_language_highlighter import ConversationLanguageHighlighter
from humbug.language.language_manager import LanguageManager
from humbug.lib.syntax.programming_language import ProgrammingLanguage
from humbug.lib.syntax.programming_language_utils import ProgrammingLanguageUtils


class WikiFileContent(WikiContent):
    """Widget for displaying file content in the wiki with editing capabilities."""

    def __init__(self, parent: QWidget | None = None) -> None:
        """
        Initialize the source content widget.

        Args:
            parent: Optional parent widget
        """
        super().__init__(parent)
        self._logger = logging.getLogger("WikiFileContent")
        self._content = ""
        self._file_path = ""

        # Initialize managers
        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._handle_language_changed)
        self._style_manager = StyleManager()

        # Container for source content
        self._content_container = QWidget(self)
        self._content_layout = QVBoxLayout(self._content_container)
        spacing = int(self._style_manager.message_bubble_spacing())
        self._content_layout.setSpacing(spacing)
        self._content_layout.setContentsMargins(0, 0, 0, 0)
        self._layout.addWidget(self._content_container)
        self._layout.setSpacing(spacing)
        self._layout.setContentsMargins(spacing, spacing, spacing, spacing)

        # Create header with edit button
        self._header_container = QWidget()
        self._header_layout = QHBoxLayout(self._header_container)
        self._header_layout.setContentsMargins(0, 0, 0, 0)
        self._header_layout.setSpacing(4)

        # Add language label
        self._language_header = QLabel()
        self._language_header.setAlignment(Qt.AlignmentFlag.AlignLeft)
        self._header_layout.addWidget(self._language_header)

        # Add stretch to fill space
        self._header_layout.addStretch()

        # Add Edit button with icon
        self._edit_button = QToolButton()
        self._edit_button.clicked.connect(self._on_edit_clicked)
        self._header_layout.addWidget(self._edit_button)

        # Add header to layout
        self._content_layout.addWidget(self._header_container)

        # Create text area for source code
        self._text_area = MarkdownTextEdit()
        self._text_area.setAcceptRichText(False)  # No rich text for source code
        self._text_area.setReadOnly(True)  # Always read-only in wiki
        self._text_area.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAsNeeded)
        self._text_area.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAsNeeded)

        # Disable the standard context menu as our parent widget will handle that
        self._text_area.setContextMenuPolicy(Qt.ContextMenuPolicy.NoContextMenu)

        self._content_layout.setSpacing(spacing)
        self._content_layout.addWidget(self._text_area)

        # Initialize variables
        self._language: ProgrammingLanguage | None = None
        self._highlighter: ConversationLanguageHighlighter | None = None
        self._mouse_left_button_pressed = False
        self._init_colour_mode = self._style_manager.color_mode()

        # Connect signals
        self._text_area.selectionChanged.connect(self._on_selection_changed)
        self._text_area.mousePressed.connect(self._on_mouse_pressed)
        self._text_area.mouseReleased.connect(self._on_mouse_released)

        # Apply initial styles
        self._handle_language_changed()
        self._style_manager.style_changed.connect(self._handle_style_changed)
        self._handle_style_changed()

    def _on_edit_clicked(self) -> None:
        """Handle the edit button being clicked."""
        self.edit_clicked.emit()

    def _on_mouse_pressed(self, event: QMouseEvent) -> None:
        """
        Handle mouse press from text area.

        Args:
            event: Mouse event
        """
        if event.buttons() == Qt.MouseButton.LeftButton:
            self._mouse_left_button_pressed = True

    def _on_mouse_released(self, _event: QMouseEvent) -> None:
        """
        Handle mouse release from text area.

        Args:
            _event: Mouse event
        """
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

    def _handle_language_changed(self) -> None:
        """Update text when language changes."""
        strings = self._language_manager.strings()

        if self._language:
            language_header = strings.highlighting.format(
                language=ProgrammingLanguageUtils.get_display_name(self._language)
            )
            self._language_header.setText(language_header)

        if self._edit_button:
            self._edit_button.setToolTip(strings.tooltip_edit_file)

    def set_content(self, text: str, path: str | None) -> None:
        """
        Set content, processing source code.

        Args:
            text: The content text
            path: Path to the file
        """
        self._content = text
        self._file_path = path if path else ""

        # Determine language based on file extension
        if path:
            self._language = ProgrammingLanguageUtils.from_file_extension(path)

        else:
            # Default to TEXT if no path provided
            self._language = ProgrammingLanguage.TEXT

        if self._language is None:
            # Default to text if no language detected
            self._language = ProgrammingLanguage.TEXT

        highlighter = ConversationLanguageHighlighter(self._text_area.document())
        highlighter.set_language(self._language)
        self._highlighter = highlighter
        self._text_area.set_has_code_block(True)

        # Update header text
        strings = self._language_manager.strings()
        language_name = ProgrammingLanguageUtils.get_display_name(self._language)
        self._language_header.setText(strings.highlighting.format(language=language_name))

        self._text_area.set_text(text)

    def has_selection(self) -> bool:
        """
        Check if any content has selected text.

        Returns:
            True if there is selected text, False otherwise
        """
        return self._text_area.textCursor().hasSelection()

    def get_selected_text(self) -> str:
        """
        Get any selected text in this content.

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
        """Clear any text selection in this content."""
        cursor = self._text_area.textCursor()
        cursor.clearSelection()
        self._text_area.setTextCursor(cursor)

    def find_text(self, text: str) -> List[Tuple[int, int, int]]:
        """
        Find all instances of text in this content.

        Args:
            text: Text to search for

        Returns:
            List of (section, start_position, end_position) tuples for each match
        """
        document = self._text_area.document()
        matches = []
        cursor = QTextCursor(document)

        while True:
            cursor = document.find(text, cursor)
            if cursor.isNull():
                break

            # For compatibility with the WikiContent interface, we use section 0
            # as there's only one section in source content
            matches.append((0, cursor.selectionStart(), cursor.selectionEnd()))

        return matches

    def highlight_matches(
        self,
        matches: List[Tuple[int, int, int]],
        current_match_index: int = -1,
        highlight_color: QColor | None = None,
        dim_highlight_color: QColor | None = None
    ) -> None:
        """
        Highlight matches in this content.

        Args:
            matches: List of (section, start_position, end_position) tuples to highlight
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
        for i, (_section, start, end) in enumerate(matches):
            cursor = QTextCursor(self._text_area.document())
            cursor.setPosition(start)
            cursor.setPosition(end, QTextCursor.MoveMode.KeepAnchor)

            extra_selection = self._text_area.ExtraSelection()
            extra_selection.cursor = cursor  # type: ignore
            extra_selection.format = current_format if i == current_match_index else other_format  # type: ignore

            selections.append(extra_selection)

        self._text_area.setExtraSelections(selections)

    def clear_highlights(self) -> None:
        """Clear all highlights from the content."""
        self._text_area.setExtraSelections([])

    def select_and_scroll_to_position(self, _section_num: int, position: int) -> QPoint:
        """
        Select text and get position for scrolling.

        Args:
            section_num: Section number to scroll to (ignored in source content)
            position: Text position to scroll to

        Returns:
            QPoint: Position to scroll to, relative to this widget
        """
        cursor = QTextCursor(self._text_area.document())
        cursor.setPosition(position)
        self._text_area.setTextCursor(cursor)

        # Get cursor rectangle in text area coordinates
        cursor_rect = self._text_area.cursorRect(cursor)

        # Convert to position relative to this widget
        local_pos = self._text_area.mapTo(self, cursor_rect.topLeft())

        return local_pos

    def find_element_by_id(self, _element_id: str) -> Tuple[int, int, int] | None:
        """
        Find an element with the given ID (not supported for source content).

        Args:
            _element_id: The ID to search for

        Returns:
            None as IDs are not supported for source content
        """
        return None

    def get_context_menu_actions(self) -> List[Tuple[str, Callable]]:
        """
        Get context menu actions for this content.

        Returns:
            List of (action_name, callback) tuples
        """
        actions = []

        # Add copy action if text is selected
        if self.has_selection():
            actions.append(("Copy", self.copy_selection))

        # Add edit action
        actions.append(("Edit", self.edit_clicked.emit))

        return actions

    def _handle_style_changed(self) -> None:
        """Handle style changes."""
        factor = self._style_manager.zoom_factor()
        font = self.font()
        base_font_size = self._style_manager.base_font_size()
        font.setPointSizeF(base_font_size * factor)
        self.setFont(font)

        background_color = self._style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)
        text_color = self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)

        # Style text area
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
            QScrollBar:vertical {{
                width: 12px;
                background: {self._style_manager.get_color_str(ColorRole.SCROLLBAR_BACKGROUND)};
            }}
            QScrollBar::handle:vertical {{
                background: {self._style_manager.get_color_str(ColorRole.SCROLLBAR_HANDLE)};
                min-height: 20px;
            }}
            QScrollBar::add-page:vertical, QScrollBar::sub-page:vertical {{
                background: none;
            }}
            QScrollBar::add-line:vertical, QScrollBar::sub-line:vertical {{
                height: 0px;
            }}
        """)

        # Style header container
        self._header_container.setStyleSheet(f"""
            QWidget {{
                background-color: {background_color};
                margin: 0;
                padding: 0;
            }}
        """)

        # Style language header
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

        # Style buttons
        button_style = f"""
            QToolButton {{
                background-color: {background_color};
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: none;
                border-radius: 0;
                padding: 0px;
            }}
            QToolButton:hover {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_HOVER)};
            }}
            QToolButton:pressed {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_PRESSED)};
            }}
        """

        icon_base_size = 14
        icon_scaled_size = int(icon_base_size * self._style_manager.zoom_factor())
        icon_size = QSize(icon_scaled_size, icon_scaled_size)

        self._edit_button.setIcon(QIcon(self._style_manager.scale_icon(
            self._style_manager.get_icon_path("edit"), icon_base_size
        )))
        self._edit_button.setIconSize(icon_size)
        self._edit_button.setStyleSheet(button_style)

        self.setStyleSheet(f"""
            QWidget {{
                background-color: {background_color};
                margin: 0;
                border-radius: {int(self._style_manager.message_bubble_spacing())}px;
                border: 0;
            }}
        """)

        # If we changed colour mode then re-highlight
        if self._style_manager.color_mode() != self._init_colour_mode:
            self._init_colour_mode = self._style_manager.color_mode()
            if self._highlighter:
                self._highlighter.rehighlight()

    def supports_editing(self) -> bool:
        """
        Check if this content type supports editing.

        Returns:
            True if this content type supports editing, False otherwise
        """
        return True

    def get_content_type(self) -> str:
        """
        Get the type of this content.

        Returns:
            String identifier for the content type
        """
        return "source"

    def get_serializable_data(self) -> dict:
        """
        Get serializable data for this content.

        Returns:
            Dictionary of serializable data
        """
        return {
            "type": self.get_content_type(),
            "content": self._content,
            "path": self._file_path
        }

    def update_from_serialized_data(self, data: dict) -> bool:
        """
        Update this content from serialized data.

        Args:
            data: Dictionary of serialized data

        Returns:
            True if the update was successful, False otherwise
        """
        if data.get("type") != self.get_content_type():
            return False

        content = data.get("content")
        path = data.get("path", "")

        if not isinstance(content, str):
            return False

        self.set_content(content, path)
        return True
