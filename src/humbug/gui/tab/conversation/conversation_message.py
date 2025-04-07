from datetime import datetime
import logging
from typing import List, Tuple, Optional

from PySide6.QtWidgets import (
    QFrame, QVBoxLayout, QLabel, QHBoxLayout, QWidget, QToolButton, QFileDialog
)
from PySide6.QtCore import Signal, QPoint, QSize
from PySide6.QtGui import QIcon, QGuiApplication

from humbug.ai.ai_message_source import AIMessageSource
from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager
from humbug.gui.tab.conversation.conversation_message_section import ConversationMessageSection
from humbug.gui.message_box import MessageBox, MessageBoxType
from humbug.language.language_manager import LanguageManager
from humbug.markdown.markdown_converter import MarkdownConverter, MarkdownDocumentNode, MarkdownTextNode
from humbug.syntax.programming_language import ProgrammingLanguage


class ConversationMessage(QFrame):
    """Widget for displaying a single message in the conversation history with header."""

    selectionChanged = Signal(bool)
    scrollRequested = Signal(QPoint)
    mouseReleased = Signal()

    def __init__(self, parent=None, is_input=False):
        """
        Initialize the message widget.

        Args:
            parent: Optional parent widget
            is_input: Whether this is an input widget (affects styling)
        """
        super().__init__(parent)
        self.setFrameStyle(QFrame.Box | QFrame.Plain)
        self._is_input = is_input

        self._logger = logging.getLogger("ConversationMessage")

        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._handle_language_changed)
        self._message_source = None
        self._message_timestamp = None
        self._message_content = ""

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
        self._timestamp_label = QLabel(self)
        self._header_layout.addWidget(self._role_label)
        self._header_layout.addWidget(self._timestamp_label)
        self._header_layout.addStretch()

        self._copy_message_button = None
        self._save_message_button = None
        if not is_input:
            self._copy_message_button = QToolButton(self)
            self._copy_message_button.clicked.connect(self._copy_message)
            self._header_layout.addWidget(self._copy_message_button)

            self._save_message_button = QToolButton(self)
            self._save_message_button.clicked.connect(self._save_message)
            self._header_layout.addWidget(self._save_message_button)

        # Add header widget to main layout
        self._layout.addWidget(self._header)

        # Container for message sections
        self._sections_container = QWidget(self)
        self._sections_layout = QVBoxLayout(self._sections_container)
        self._sections_layout.setContentsMargins(0, 0, 0, 0)
        self._sections_layout.setSpacing(15)
        self._layout.addWidget(self._sections_container)

        # Track sections
        self._sections: List[ConversationMessageSection] = []
        self._section_with_selection: Optional[ConversationMessageSection] = None

        # If this is an input widget then create the input section
        if is_input:
            section = self._create_section_widget()
            self._sections.append(section)
            self._sections_layout.addWidget(section)

        # Initialize markdown converter
        self._markdown_converter = MarkdownConverter()

        self._is_focused = False
        self._is_bookmarked = False

        # Track current message style
        self._current_style: AIMessageSource = None

        self._style_manager = StyleManager()
        self._style_manager.style_changed.connect(self._handle_style_changed)
        self._handle_style_changed()
        self._handle_language_changed()

    def is_focused(self) -> bool:
        """Check if this message is focused."""
        return self._is_focused

    def set_focused(self, focused: bool):
        """Set the focused state of this message."""
        if self._is_focused == focused:
            return

        self._is_focused = focused
        if focused:
            self.setFocus()

        self._handle_style_changed()

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

            strings = self._language_manager.strings
            if self._copy_message_button:
                self._copy_message_button.setToolTip(strings.tooltip_copy_message)

            if self._save_message_button:
                self._save_message_button.setToolTip(strings.tooltip_save_message)

    def _update_role_text(self) -> None:
        """Update the role text based on current language."""
        if not self._message_source:
            return

        strings = self._language_manager.strings
        role_text = {
            AIMessageSource.USER: strings.role_you,
            AIMessageSource.AI: strings.role_assistant,
            AIMessageSource.REASONING: strings.role_reasoning,
            AIMessageSource.SYSTEM: strings.role_system
        }.get(self._message_source, "Unknown")

        # Format with timestamp
        if self._message_timestamp:
            timestamp_str = self._message_timestamp.strftime("%Y-%m-%d %H:%M:%S.%f")[:-3]
            self._role_label.setText(f"{role_text} @ {timestamp_str}")
        else:
            self._role_label.setText(role_text)

    def _create_section_widget(self, language: Optional[ProgrammingLanguage] = None) -> ConversationMessageSection:
        """
        Create a new section widget.

        Args:
            language: Optional programming language for the section

        Returns:
            A new ConversationMessageSection instance
        """
        section = ConversationMessageSection(self._is_input, language, self._sections_container)
        section.selectionChanged.connect(
            lambda has_selection: self._handle_section_selection_changed(section, has_selection)
        )
        section.scrollRequested.connect(self.scrollRequested)
        section.mouseReleased.connect(self.mouseReleased)
        return section

    def _handle_section_selection_changed(self, section: ConversationMessageSection, has_selection: bool):
        """
        Handle selection changes in a section widget.

        Args:
            section: The section widget where selection changed
            has_selection: Whether there is a selection
        """
        if not has_selection:
            if self._section_with_selection == section:
                self._section_with_selection = None
            return

        # Clear selection in other sections
        if self._section_with_selection and self._section_with_selection != section:
            self._section_with_selection.clear_selection()

        self._section_with_selection = section
        self.selectionChanged.emit(has_selection)

    def set_content(self, text: str, style: AIMessageSource, timestamp: datetime):
        """
        Set content with style, handling incremental updates for AI responses.

        Args:
            text: The message text content
            style: The style type ('user', 'ai', 'system', or 'error')
            timestamp: datetime object for the message timestamp
        """
        self._message_source = style
        self._message_timestamp = timestamp
        self._message_content = text

        # Check if style changed - if so, we need to recreate all sections
        if style != self._current_style:
            # Update header text with proper role
            self._update_role_text()
            self._current_style = style

            # Clear existing sections
            for section in self._sections:
                self._sections_layout.removeWidget(section)
                section.deleteLater()

            self._sections = []
            self._section_with_selection = None
            self._handle_style_changed()

        # Extract sections directly using the markdown converter
        if not self._is_input:
            # Process the content and extract sections in one step
            sections_data = self._markdown_converter.extract_sections(text)
        else:
            # Input widgets don't use markdown processing
            text_node = MarkdownDocumentNode()
            text_node.add_child(MarkdownTextNode(text))
            sections_data = [(text_node, None)]

        # Create or update sections
        for i, (node, language) in enumerate(sections_data):
            # Create new section if needed
            if i >= len(self._sections):
                section = self._create_section_widget(language)
                self._sections.append(section)
                self._sections_layout.addWidget(section)

                text_color = self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)
                background_color = self._style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)
                color = self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE) if language is not None else background_color
                factor = self._style_manager.zoom_factor
                font = self.font()
                base_font_size = self._style_manager.base_font_size
                font.setPointSizeF(base_font_size * factor)
                section.apply_style(text_color, color, font)
                section.set_content(node)
                continue

            if i == len(self._sections) - 1:
                # Update the last section with new content
                section = self._sections[-1]
                if language != section.language:
                    section.set_language(language)

                section.set_content(node)

        # Remove any extra sections
        while len(self._sections) > len(sections_data):
            section = self._sections.pop()
            self._sections_layout.removeWidget(section)
            section.deleteLater()

    def _copy_message(self):
        """Copy the entire message content to clipboard."""
        content = self._message_content
        QGuiApplication.clipboard().setText(content)

    def _save_message(self):
        """Show save as dialog and save message as a markdown file."""
        strings = self._language_manager.strings

        # Show file dialog
        export_dialog = QFileDialog()
        export_dialog.setWindowTitle(strings.file_dialog_save_file)
        export_dialog.setAcceptMode(QFileDialog.AcceptSave)
        export_dialog.selectFile("message.md")

        if export_dialog.exec_() != QFileDialog.Accepted:
            return False

        filename = export_dialog.selectedFiles()[0]

        # Save the file
        try:
            with open(filename, 'w', encoding='utf-8') as f:
                f.write(self._message_content)

            return True

        except Exception as e:
            self._logger.error("Failed to save message: %s", str(e))
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.error_saving_file_title,
                strings.could_not_save.format(filename, str(e))
            )
            return False

    def has_selection(self) -> bool:
        """Check if any section has selected text."""
        return self._section_with_selection is not None and self._section_with_selection.has_selection()

    def get_selected_text(self) -> str:
        """
        Get any selected text in this message.

        Returns:
            Currently selected text or empty string
        """
        if self._section_with_selection:
            return self._section_with_selection.get_selected_text()

        return ""

    def copy_selection(self):
        """Copy selected text to clipboard."""
        if self._section_with_selection:
            self._section_with_selection.copy_selection()

    def clear_selection(self):
        """Clear any text selection in this message."""
        if self._section_with_selection:
            self._section_with_selection.clear_selection()
            self._section_with_selection = None

    def resizeEvent(self, event):
        """Handle resize events."""
        super().resizeEvent(event)

    def _handle_style_changed(self):
        """Handle the style changing"""
        factor = self._style_manager.zoom_factor
        font = self.font()
        base_font_size = self._style_manager.base_font_size
        font.setPointSizeF(base_font_size * factor)
        self.setFont(font)

        # Map message types to role colors
        role_colours = {
            AIMessageSource.USER: ColorRole.MESSAGE_USER,
            AIMessageSource.AI: ColorRole.MESSAGE_AI,
            AIMessageSource.REASONING: ColorRole.MESSAGE_REASONING,
            AIMessageSource.SYSTEM: ColorRole.MESSAGE_SYSTEM
        }

        role = role_colours.get(self._current_style, ColorRole.MESSAGE_USER)
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

        # Timestamp label styling (normal weight)
        self._timestamp_label.setFont(font)
        self._timestamp_label.setStyleSheet(f"""
            QLabel {{
                color: {label_color};
                padding: 0;
                margin: 0;
                background-color: {background_color};
            }}
        """)

        # Button styling for message action buttons
        button_style = f"""
            QToolButton {{
                background-color: {background_color};
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: none;
                padding: 0px;
            }}
            QToolButton:hover {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_HOVER)};
            }}
            QToolButton:pressed {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_PRESSED)};
            }}
        """

        # Apply icon and styling to copy and save buttons
        icon_base_size = 14
        icon_size = QSize(16 * self._style_manager.zoom_factor, 14 * self._style_manager.zoom_factor)

        if self._copy_message_button:
            self._copy_message_button.setIcon(QIcon(self._style_manager.scale_icon(
                self._style_manager.get_icon_path("copy"), icon_base_size
            )))
            self._copy_message_button.setIconSize(icon_size)
            self._copy_message_button.setStyleSheet(button_style)

        if self._save_message_button:
            self._save_message_button.setIcon(QIcon(self._style_manager.scale_icon(
                self._style_manager.get_icon_path("save"), icon_base_size
            )))
            self._save_message_button.setIconSize(icon_size)
            self._save_message_button.setStyleSheet(button_style)

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

        # Apply styling to all sections
        for section in self._sections:
            language = section.language
            color = self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE) if language is not None else background_color
            section.apply_style(text_color, color, font)

        # Determine border color based on state (focused takes precedence over bookmarked)
        border = ColorRole.MESSAGE_FOCUSED if self._is_focused and self.hasFocus() else \
                 ColorRole.MESSAGE_BOOKMARK if self._is_bookmarked else \
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

    def find_text(self, text: str) -> List[Tuple[int, int, int]]:
        """
        Find all instances of text in this message.

        Args:
            text: Text to search for

        Returns:
            List of (section, start_position, end_position) tuples for each match
        """
        all_matches: List[Tuple[int, int, int]] = []
        for i, section in enumerate(self._sections):
            section_matches = section.find_text(text)
            if section_matches:
                # Include the section with each match
                for match in section_matches:
                    all_matches.append((i, match[0], match[1]))

        return all_matches

    def highlight_matches(
        self,
        matches: List[Tuple[int, int, int]],
        current_match_index: int = -1,
        highlight_color=None,
        dim_highlight_color=None
    ):
        """
        Highlight matches in this message.

        Args:
            matches: List of (section, start_position, end_position) tuples to highlight
            current_match_index: Index of current match to highlight differently, or -1 for none
            highlight_color: QColor for current match, defaults to system highlight color
            dim_highlight_color: QColor for other matches, defaults to dimmer highlight color
        """
        # First clear all highlights
        self.clear_highlights()

        if not matches:
            return

        # Group matches by section
        section_matches = {}
        for section in self._sections:
            section_matches[section] = []

        # Distribute matches to their respective sections
        for i, match in enumerate(matches):
            section_num, start, end = match
            section = self._sections[section_num]
            if section in section_matches:
                section_matches[section].append((start, end, i))

        # Highlight matches in each section
        for section, section_matches_list in section_matches.items():
            if not section_matches_list:
                continue

            # Extract position tuples (without the index)
            positions = [(start, end) for start, end, _ in section_matches_list]

            # Find if current match is in this section
            section_current_idx = -1
            for i, (_, _, idx) in enumerate(section_matches_list):
                if idx == current_match_index:
                    section_current_idx = i
                    break

            # Highlight this section's matches
            section.highlight_matches(
                positions,
                section_current_idx,
                highlight_color,
                dim_highlight_color
            )

    def clear_highlights(self):
        """Clear all highlights from the message."""
        for section in self._sections:
            section.clear_highlights()

    def select_and_scroll_to_position(self, section_num: int, position: int) -> QPoint:
        """
        Select text and get position for scrolling.

        Args:
            section_num: Section number to scroll to
            position: Text position to scroll to

        Returns:
            QPoint: Position to scroll to, relative to this widget
        """
        if 0 <= section_num < len(self._sections):
            section = self._sections[section_num]
            # Get position relative to the section
            pos_in_section = section.select_and_scroll_to_position(position)

            # Map from section to this widget's coordinates
            return section.mapTo(self, pos_in_section)

        return QPoint(0, 0)
