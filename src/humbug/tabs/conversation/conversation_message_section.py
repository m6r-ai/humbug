"""Widget for displaying a section of a message."""

import logging
from typing import List, Tuple, cast

from PySide6.QtWidgets import (
    QVBoxLayout, QFrame, QTextEdit, QLabel, QHBoxLayout,
    QToolButton, QFileDialog, QWidget
)
from PySide6.QtCore import Signal, Qt, QPoint, QSize
from PySide6.QtGui import (
    QCursor, QMouseEvent, QTextCursor, QTextCharFormat, QIcon, QColor, QFont
)

from dmarkdown import MarkdownASTNode, MarkdownASTTextNode
from syntax import ProgrammingLanguage, ProgrammingLanguageUtils

from humbug.color_role import ColorRole
from humbug.language.language_manager import LanguageManager
from humbug.message_box import MessageBox, MessageBoxType
from humbug.style_manager import StyleManager
from humbug.tabs.conversation.conversation_highlighter import ConversationHighlighter
from humbug.tabs.conversation.conversation_language_highlighter import ConversationLanguageHighlighter
from humbug.tabs.markdown_renderer import MarkdownRenderer
from humbug.tabs.markdown_text_edit import MarkdownTextEdit


class ConversationMessageSection(QFrame):
    """Widget for displaying a section of a message with markdown support."""

    selection_changed = Signal(bool)
    scroll_requested = Signal(QPoint)
    mouse_released = Signal()

    def __init__(
        self,
        is_input: bool,
        language: ProgrammingLanguage | None = None,
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

        self._logger = logging.getLogger("ConversationMessageSection")
        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._on_language_changed)

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
        self._copy_button = None
        self._save_as_button = None

        if language is not None:
            self._layout.setContentsMargins(spacing, spacing, spacing, spacing)

            # Create a container for header (language label + buttons)
            self._header_container = QWidget()
            self._header_layout = QHBoxLayout(self._header_container)
            self._header_layout.setContentsMargins(0, 0, 0, 0)
            self._header_layout.setSpacing(4)

            # Add language label on the left
            self._language_header = QLabel()
            self._language_header.setIndent(0)
            self._language_header.setAlignment(Qt.AlignmentFlag.AlignLeft)
            self._header_layout.addWidget(self._language_header)

            # Add stretch to push buttons to the right
            self._header_layout.addStretch()

            # Add Copy button with icon
            self._copy_button = QToolButton()
            self._copy_button.clicked.connect(self._copy_all_content)
            self._header_layout.addWidget(self._copy_button)

            # Add Save As button with icon
            self._save_as_button = QToolButton()
            self._save_as_button.clicked.connect(self._save_as)
            self._header_layout.addWidget(self._save_as_button)

            # Add header container to main layout
            self._layout.addWidget(self._header_container)

        self._is_input = is_input

        # Determine if this section should use markdown (only AI responses without language)
        self._use_markdown = not is_input and language is None

        # Create text area
        self._text_area = MarkdownTextEdit()
        self._text_area.setAcceptRichText(self._use_markdown)
        self._text_area.setReadOnly(not is_input)
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
        self._text_area.mouse_pressed.connect(self._on_mouse_pressed)
        self._text_area.mouse_released.connect(self._on_mouse_released)

        # Add conversation highlighter - use lazy creation for expensive language highlighters
        self._highlighter: ConversationHighlighter | ConversationLanguageHighlighter | None = None
        self._needs_highlighting = False
        self.set_language(language)

        self._mouse_left_button_pressed = False

        self._init_colour_mode = self._style_manager.color_mode()

        self._on_language_changed()

    def text_area(self) -> MarkdownTextEdit:
        """Get the text area widget."""
        return self._text_area

    def language(self) -> ProgrammingLanguage | None:
        """Provide the language in use by this section."""
        return self._language

    def set_language(self, language: ProgrammingLanguage | None) -> None:
        """Set the programming language to use for this message section"""
        self._language = language

        if language is None:
            self._use_markdown = not self._is_input
            if self._use_markdown:
                self._highlighter = None
                self._needs_highlighting = False

            else:
                self._highlighter = ConversationHighlighter(self._text_area.document())
                self._highlighter.code_block_state_changed.connect(self._on_code_block_state_changed)
                self._needs_highlighting = False

        else:
            self._use_markdown = False
            # Defer creation of expensive language highlighter until section becomes visible
            self._highlighter = None
            self._needs_highlighting = True
            self._text_area.set_has_code_block(True)

        strings = self._language_manager.strings()
        if self._language_header:
            language_header = strings.highlighting.format(
                language=ProgrammingLanguageUtils.get_display_name(cast(ProgrammingLanguage, self._language))
            )
            self._language_header.setText(language_header)

    def ensure_highlighting(self) -> None:
        """Create highlighter if needed - called when section becomes visible."""
        if self._needs_highlighting and self._highlighter is None:
            highlighter = ConversationLanguageHighlighter(self._text_area.document())
            highlighter.set_language(cast(ProgrammingLanguage, self._language))
            self._highlighter = highlighter
            self._needs_highlighting = False

    def _on_language_changed(self) -> None:
        """Update text when language changes."""
        strings = self._language_manager.strings()

        if self._language_header:
            language_header = strings.highlighting.format(
                language=ProgrammingLanguageUtils.get_display_name(cast(ProgrammingLanguage, self._language))
            )
            self._language_header.setText(language_header)

        if self._copy_button:
            self._copy_button.setToolTip(strings.tooltip_copy_contents)

        if self._save_as_button:
            self._save_as_button.setToolTip(strings.tooltip_save_contents)

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

    def _on_code_block_state_changed(self, has_code_block: bool) -> None:
        """Handle changes in code block state."""
        self._text_area.set_has_code_block(has_code_block)

        # Ensure proper scroll behavior
        self.updateGeometry()

    def _copy_all_content(self) -> None:
        """Copy all content in the text area to clipboard."""
        # Store current selection
        old_cursor = self._text_area.textCursor()

        # Select all text
        cursor = QTextCursor(self._text_area.document())
        cursor.select(QTextCursor.SelectionType.Document)
        self._text_area.setTextCursor(cursor)

        # Copy to clipboard
        self._text_area.copy()

        # Restore previous selection
        self._text_area.setTextCursor(old_cursor)

    def _save_as(self) -> bool:
        """Show save as dialog and save file."""
        strings = self._language_manager.strings()

        # Determine the suggested file extension based on language
        extension = ProgrammingLanguageUtils.get_file_extension(self._language)
        default_filename = f"code{extension}" if extension else "code.txt"

        # Show file dialog
        export_dialog = QFileDialog()
        export_dialog.setWindowTitle(strings.file_dialog_save_file)
        export_dialog.setAcceptMode(QFileDialog.AcceptMode.AcceptSave)
        export_dialog.selectFile(default_filename)

        if export_dialog.exec_() != QFileDialog.DialogCode.Accepted:
            return False

        filename = export_dialog.selectedFiles()[0]

        # Save the file
        try:
            content = self._text_area.toPlainText()
            with open(filename, 'w', encoding='utf-8') as f:
                f.write(content)

            return True

        except Exception as e:
            self._logger.error("Failed to save file: %s", str(e))
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.error_saving_file_title,
                strings.could_not_save.format(filename, str(e))
            )
            return False

    def set_content(self, content: MarkdownASTNode) -> None:
        """
        Set the content of this section.

        Args:
            content: A MarkdownASTNode text content
        """
        # If we have code block node, extract its content as plain text
        if not self._use_markdown:
            text_content = cast(MarkdownASTTextNode, content)
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

    def apply_style(self, font: QFont) -> None:
        """
        Apply styling to this section.
        """
        self.setFont(font)
        self._text_area.setFont(font)

        # Style the language header if present, or the inline code style if it's not
        if not self._language_header:
            if self._content_node:
                self._renderer.visit(self._content_node)

        icon_base_size = 14
        icon_scaled_size = int(icon_base_size * self._style_manager.zoom_factor())
        icon_size = QSize(icon_scaled_size, icon_scaled_size)

        if self._copy_button:
            self._copy_button.setIcon(QIcon(self._style_manager.scale_icon(
                self._style_manager.get_icon_path("copy"), icon_base_size
            )))
            self._copy_button.setIconSize(icon_size)

        if self._save_as_button:
            self._save_as_button.setIcon(QIcon(self._style_manager.scale_icon(
                self._style_manager.get_icon_path("save"), icon_base_size
            )))
            self._save_as_button.setIconSize(icon_size)

        # If we changed colour mode then re-highlight
        if self._style_manager.color_mode() != self._init_colour_mode:
            self._init_colour_mode = self._style_manager.color_mode()
            if self._highlighter:
                self._highlighter.rehighlight()
