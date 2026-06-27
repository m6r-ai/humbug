"""Widget for displaying a section of a message."""

import logging
from typing import List, Tuple, cast

from PySide6.QtWidgets import (
    QVBoxLayout, QFrame, QTextEdit, QLabel, QHBoxLayout,
    QToolButton, QFileDialog, QWidget, QSizePolicy
)
from PySide6.QtCore import Signal, Qt, QPoint, QRegularExpression
from PySide6.QtGui import (
    QCursor, QMouseEvent, QTextCursor, QTextCharFormat, QColor, QTextDocument
)

from dmarkdown import MarkdownASTNode, MarkdownASTCodeBlockNode, MarkdownASTTextNode
from syntax import ProgrammingLanguage, ProgrammingLanguageUtils

from desktop.color_role import ColorRole
from desktop.language.language_manager import LanguageManager
from desktop.message_box import MessageBox, MessageBoxType
from desktop.style_manager import StyleManager
from desktop.markdown import MarkdownCodeBlockTextEdit, MarkdownRenderer, MarkdownTextEdit
from desktop.conversation_tab.conversation_message_style import ConversationMessageStyle


class ConversationMessageSection(QFrame):
    """Widget for displaying a section of a message with markdown support."""

    selection_changed = Signal(bool)
    scroll_requested = Signal(QPoint)
    mouse_released = Signal()

    def __init__(
        self,
        is_input: bool,
        syntax: ProgrammingLanguage | None = None,
        parent: QWidget | None = None
    ) -> None:
        """
        Initialize a message section widget.

        Args:
            is_input: Whether this section is for user input
            syntax: Optional syntax for this section
            parent: Optional parent widget
        """
        super().__init__(parent)
        self.setFrameStyle(QFrame.Shape.Box | QFrame.Shadow.Plain)

        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)

        self.setObjectName("ConversationMessageSection")

        self._logger = logging.getLogger("ConversationMessageSection")
        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._on_language_changed)

        style_manager = StyleManager()
        self._style_manager = style_manager

        # Create syntax header if needed
        self._syntax = syntax
        self._syntax_header = None
        self._filename_label: str | None = None
        self._header_container = None
        self._copy_button: QToolButton | None = None
        self._save_as_button: QToolButton | None = None

        self._header_layout: QHBoxLayout | None = None

        self._layout = QVBoxLayout(self)
        self._layout.setContentsMargins(0, 0, 0, 0)

        self._is_input = is_input

        # Create text area - use MarkdownCodeBlockTextEdit for code blocks, MarkdownTextEdit for everything else
        self._text_area: MarkdownTextEdit | MarkdownCodeBlockTextEdit
        self._renderer: MarkdownRenderer | None = None

        if syntax is None:
            # Markdown or user input - use rich text widget
            self._text_area = MarkdownTextEdit(is_input, self)

            # Create a markdown renderer for non-input sections.  Input sections are syntax highlighted only.
            if not is_input:
                document = self._text_area.document()
                self._renderer = MarkdownRenderer(
                    document,
                    animated_gif_callback=self._text_area.register_animated_gif
                )

        else:
            # Create a container for header (language label + buttons)
            self._header_container = QWidget()
            self._header_container.setObjectName("_header_container")
            self._header_layout = QHBoxLayout(self._header_container)
            self._header_layout.setContentsMargins(0, 0, 0, 0)
            self._header_layout.setSpacing(4)

            # Add syntax label on the left
            self._syntax_header = QLabel(self._header_container)
            self._syntax_header.setIndent(0)
            self._syntax_header.setAlignment(Qt.AlignmentFlag.AlignLeft)
            self._header_layout.addWidget(self._syntax_header)

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

            # Code block - use code block text widget
            self._text_area = MarkdownCodeBlockTextEdit(self)
            self._text_area.set_syntax(syntax)

        # Disable the standard context menu as our parent widget will handle that
        self._text_area.setContextMenuPolicy(Qt.ContextMenuPolicy.NoContextMenu)

        self._content_node: MarkdownASTNode | None = None

        self._rendered_width: int = -1

        # Connect signals
        self._text_area.selectionChanged.connect(self._on_selection_changed)
        self._text_area.mouse_pressed.connect(self._on_mouse_pressed)
        self._text_area.mouse_released.connect(self._on_mouse_released)
        if self._renderer is not None and isinstance(self._text_area, MarkdownTextEdit):
            self._text_area.text_width_changed.connect(self._on_text_width_changed)

        self._mouse_left_button_pressed = False

        self._on_language_changed()

        self._layout.addWidget(self._text_area)
        self.setLayout(self._layout)

    def set_filename_label(self, filename: str) -> None:
        """
        Override the section header label with a plain filename.

        Args:
            filename: The filename to display instead of the language name
        """
        self._filename_label = filename
        if self._syntax_header is not None:
            self._syntax_header.setText(filename)

    def prime_width(self, container_width: int) -> None:
        """
        Prime the text area document width before content is set.

        Subtracts this section's own content margins so the document lays out
        at the correct width from the very first paint, preventing a resize bounce.
        Only applies to MarkdownTextEdit (rich-text sections); code block sections
        use QPlainTextEdit which manages its own layout width via widget geometry.

        Args:
            container_width: Width of the parent sections container in pixels
        """
        if not isinstance(self._text_area, MarkdownTextEdit):
            return

        margins = self._layout.contentsMargins()
        effective_width = container_width - margins.left() - margins.right()
        self._text_area.prime_document_width(effective_width)

    def text_area(self) -> MarkdownTextEdit | MarkdownCodeBlockTextEdit:
        """Get the text area widget."""
        return self._text_area

    def syntax(self) -> ProgrammingLanguage | None:
        """Provide the language in use by this section."""
        return self._syntax

    def set_syntax(self, syntax: ProgrammingLanguage | None) -> None:
        """Set the programming language to use for this message section"""
        if self._syntax == syntax:
            return

        self._syntax = syntax

        if syntax is not None:
            if isinstance(self._text_area, MarkdownCodeBlockTextEdit):
                self._text_area.set_syntax(syntax)

        if self._syntax_header:
            strings = self._language_manager.strings()
            if self._filename_label is not None:
                self._syntax_header.setText(self._filename_label)

            else:
                syntax_header = strings.highlighting.format(
                    syntax=ProgrammingLanguageUtils.get_display_name(cast(ProgrammingLanguage, self._syntax))
                )
                self._syntax_header.setText(syntax_header)

    def _on_language_changed(self) -> None:
        """Update text when language changes."""
        strings = self._language_manager.strings()

        if self._syntax_header:
            if self._filename_label is not None:
                self._syntax_header.setText(self._filename_label)

            else:
                syntax_header = strings.highlighting.format(
                    syntax=ProgrammingLanguageUtils.get_display_name(cast(ProgrammingLanguage, self._syntax))
                )
                self._syntax_header.setText(syntax_header)

        if self._copy_button:
            self._copy_button.setToolTip(strings.tooltip_copy_contents)

        if self._save_as_button:
            self._save_as_button.setToolTip(strings.tooltip_save_contents)

    def _on_mouse_pressed(self, event: QMouseEvent) -> None:
        """Handle mouse press from text area."""
        if event.buttons() == Qt.MouseButton.LeftButton:
            self._mouse_left_button_pressed = True

    def _on_text_width_changed(self) -> None:
        """
        Re-render content when the document text width changes.

        Fixed-width table frames are sized at render time, so a width change
        requires a full re-render to recompute the correct table dimensions.
        """
        if (self._renderer is not None and self._content_node is not None
                and isinstance(self._text_area, MarkdownTextEdit)):
            new_width = self._text_area.viewport().width()
            if new_width == self._rendered_width:
                return

            self._rendered_width = new_width
            self._text_area.clear_animated_gifs()
            self._text_area.document().setTextWidth(new_width)
            self._renderer.visit(self._content_node)

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
        extension = ProgrammingLanguageUtils.get_file_extension(self._syntax)
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
        self._content_node = content

        if isinstance(self._text_area, MarkdownCodeBlockTextEdit):
            assert isinstance(content, MarkdownASTCodeBlockNode), "Content must be code block node"
            self._text_area.set_text_with_highlighting(content.content, content.tokens_by_line, content.states_by_line)
            return

        if self._renderer is not None:
            self._text_area.clear_animated_gifs()
            self._rendered_width = self._text_area.viewport().width()
            self._text_area.document().setTextWidth(self._text_area.viewport().width())
            self._renderer.visit(content)
            return

        # We have a text node, extract its content as plain text
        assert isinstance(content, MarkdownASTTextNode), "Content must be text node"
        self._text_area.set_text(content.content)

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

    def find_text(self, text: str, case_sensitive: bool = False, regexp: bool = False) -> List[Tuple[int, int]]:
        """
        Find all instances of text in this section.

        Args:
            text: Text to search for
            case_sensitive: If True, match case exactly.
            regexp: If True, treat text as a regular expression.

        Returns:
            List of (start_position, end_position) tuples for each match
        """
        max_matches = 500
        document = self._text_area.document()
        matches = []
        cursor = QTextCursor(document)

        if regexp:
            flags = QRegularExpression.PatternOption(0)
            if not case_sensitive:
                flags |= QRegularExpression.PatternOption.CaseInsensitiveOption

            pattern = QRegularExpression(text, flags)
            if not pattern.isValid():
                return []

            find_flags = QTextDocument.FindFlag(0)
            if case_sensitive:
                find_flags |= QTextDocument.FindFlag.FindCaseSensitively

            while True:
                cursor = document.find(pattern, cursor, find_flags)
                if cursor.isNull():
                    break

                matches.append((cursor.selectionStart(), cursor.selectionEnd()))
                if len(matches) >= max_matches:
                    break

        else:
            find_flags = QTextDocument.FindFlag(0)
            if case_sensitive:
                find_flags |= QTextDocument.FindFlag.FindCaseSensitively

            while True:
                cursor = document.find(text, cursor, find_flags)
                if cursor.isNull():
                    break

                matches.append((cursor.selectionStart(), cursor.selectionEnd()))
                if len(matches) >= max_matches:
                    break

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

    def apply_style(self, style: ConversationMessageStyle | None = None) -> None:
        """Apply styling to this section."""
        if style is None:
            return

        self._layout.setSpacing(style.spacing)

        if self._syntax_header:
            self._syntax_header.setFont(style.font)
            self._layout.setContentsMargins(style.spacing, style.spacing, style.spacing, style.spacing)

        self._text_area.apply_style()

        if self._copy_button:
            self._copy_button.setIcon(style.copy_icon)
            self._copy_button.setIconSize(style.icon_size)

        if self._save_as_button:
            self._save_as_button.setIcon(style.save_icon)
            self._save_as_button.setIconSize(style.icon_size)

        # Only apply renderer style for MarkdownTextEdit
        if self._renderer is not None and self._content_node is not None and isinstance(self._text_area, MarkdownTextEdit):
            self._renderer.apply_style()
            self._text_area.clear_animated_gifs()
            self._rendered_width = self._text_area.viewport().width()
            self._renderer.visit(self._content_node)
