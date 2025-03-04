from datetime import datetime
import logging
import re
from typing import List, Tuple, Optional

from PySide6.QtWidgets import (
    QFrame, QVBoxLayout, QLabel, QHBoxLayout, QWidget
)
from PySide6.QtCore import Signal, QPoint

from humbug.conversation.message_source import MessageSource
from humbug.gui.conversation.message_section_widget import MessageSectionWidget
from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager
from humbug.language.language_manager import LanguageManager
from humbug.syntax.conversation_parser import ConversationParser, ConversationParserState
from humbug.syntax.programming_language import ProgrammingLanguage


class MessageWidget(QFrame):
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

        self._logger = logging.getLogger("MessageWidget")

        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._handle_language_changed)
        self._message_source = None
        self._message_timestamp = None

        # Create layout
        self._layout = QVBoxLayout(self)
        self.setLayout(self._layout)
        self._layout.setSpacing(10)
        self._layout.setContentsMargins(10, 10, 10, 10)

        # Create header area with horizontal layout
        self._header = QWidget(self)
        self._header_layout = QHBoxLayout(self._header)
        self._header_layout.setContentsMargins(0, 0, 0, 0)
        self._header_layout.setSpacing(0)

        # Create role and timestamp labels
        self._role_label = QLabel(self)
        self._timestamp_label = QLabel(self)
        self._header_layout.addWidget(self._role_label)
        self._header_layout.addWidget(self._timestamp_label)
        self._header_layout.addStretch()

        # Add header widget to main layout
        self._layout.addWidget(self._header)

        # Container for message sections
        self._sections_container = QWidget(self)
        self._sections_layout = QVBoxLayout(self._sections_container)
        self._sections_layout.setContentsMargins(0, 0, 0, 0)
        self._sections_layout.setSpacing(15)
        self._layout.addWidget(self._sections_container)

        # Track sections
        self._sections: List[MessageSectionWidget] = []
        self._section_with_selection: Optional[MessageSectionWidget] = None

        # If this is an input widget then create the input section
        if is_input:
            section = self._create_section_widget()
            self._sections.append(section)
            self._sections_layout.addWidget(section)

        # Add bookmark status
        self._is_bookmarked = False

        self._parser_state: ConversationParserState = None
        self._next_str: str = ""
        self._text_list = []
        self._language_list = [None]
        self._in_fence_region = False
        self._current_language = None

        # Track current message style
        self._current_style: MessageSource = None

        self._style_manager = StyleManager()
        self._style_manager.style_changed.connect(self._handle_style_changed)
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

    def _parse_line(self, text: str) -> None:
        """Apply highlighting to the given block of text."""
        try:
            prev_parser_state = self._parser_state
            parser = ConversationParser()
            self._parser_state = parser.parse(prev_parser_state, text)

            while True:
                token = parser.get_next_token()
                if token is None:
                    break

                match token.type:
                    case 'FENCE_START':
                        self._in_fence_region = True
                        self._text_list.append(self._next_str)
                        self._language_list.append(None)
                        self._next_str = ""

                        # Update the current language based on the parser state
                        if self._parser_state and self._parser_state.language != ProgrammingLanguage.UNKNOWN:
                            self._current_language = self._parser_state.language
                        continue

                    case 'FENCE_END':
                        self._in_fence_region = False
                        self._text_list.append(self._next_str)
                        self._language_list.append(self._current_language)
                        self._next_str = ""
                        self._current_language = None
                        continue

                    case 'LANGUAGE':
                        if self._parser_state and self._parser_state.language != ProgrammingLanguage.UNKNOWN:
                            self._current_language = self._parser_state.language

                        continue

                self._next_str += token.value

        except Exception:
            self._logger.exception("highlighting exception")

    def _parse_content_sections(self, text: str) -> List[tuple]:
        """
        Parse content into sections with associated languages.

        Args:
            text: The message text content

        Returns:
            List of (section_text, language) tuples
        """
        lines = text.splitlines(keepends=True)
        self._next_str = ""
        self._text_list = []
        self._language_list = []
        self._parser_state = None
        self._in_fence_region = False
        self._current_language = None

        for line in lines:
            self._parse_line(line)

        # Handle any remaining text
        if self._next_str:
            self._text_list.append(self._next_str)
            self._language_list.append(self._current_language)

        # Strip any leading and trailing blank lines from each block.  Also strip blank blocks.
        new_text_list = []
        new_langugage_list = []
        for i, text_block in enumerate(self._text_list):
            text_block = re.sub(r'^(\s*\n)+', '', text_block)
            text_block = re.sub(r'(\n\s*)+$', '', text_block)
            if text_block:
                new_text_list.append(text_block)
                new_langugage_list.append(self._language_list[i])

        self._text_list = new_text_list
        self._language_list = new_langugage_list

        # Create a list of section tuples with (text, language)
        return list(zip(self._text_list, self._language_list))

    def _create_section_widget(self, language: Optional[ProgrammingLanguage] = None) -> MessageSectionWidget:
        """
        Create a new section widget.

        Args:
            language: Optional programming language for the section

        Returns:
            A new MessageSectionWidget instance
        """
        section = MessageSectionWidget(self._is_input, language, self._sections_container)
        section.selectionChanged.connect(
            lambda has_selection: self._handle_section_selection_changed(section, has_selection)
        )
        section.scrollRequested.connect(self.scrollRequested)
        section.mouseReleased.connect(self.mouseReleased)
        return section

    def _handle_section_selection_changed(self, section: MessageSectionWidget, has_selection: bool):
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

    def set_content(self, text: str, style: MessageSource, timestamp: datetime):
        """
        Set content with style, handling incremental updates for AI responses.

        Args:
            text: The message text content
            style: The style type ('user', 'ai', 'system', or 'error')
            timestamp: datetime object for the message timestamp
        """
        self._message_source = style
        self._message_timestamp = timestamp

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

        # Parse content into sections with language information
        orig_num_sections = len(self._sections)
        section_data = self._parse_content_sections(text)

        # Create or update sections
        for i, (section_text, language) in enumerate(section_data):
            # Create new section if needed
            if i >= len(self._sections):
                section = self._create_section_widget(language)
                self._sections.append(section)
                self._sections_layout.addWidget(section)

            # Update section content if we've not previously seen this
            if i >= orig_num_sections - 1:
                self._sections[i].set_content(section_text)

        # Remove any extra sections
        while len(self._sections) > len(section_data):
            section = self._sections.pop()
            self._sections_layout.removeWidget(section)
            section.deleteLater()

        # Apply styling
        self._handle_style_changed()

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
            MessageSource.USER: ColorRole.MESSAGE_USER,
            MessageSource.AI: ColorRole.MESSAGE_AI,
            MessageSource.REASONING: ColorRole.MESSAGE_REASONING,
            MessageSource.SYSTEM: ColorRole.MESSAGE_SYSTEM
        }

        role = role_colours.get(self._current_style, ColorRole.MESSAGE_USER)
        label_color = self._style_manager.get_color_str(role)
        background_color = self._style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)
        text_color = self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)

        # Role label styling (bold)
        self._role_label.setFont(font)
        self._role_label.setStyleSheet(f"""
            QLabel {{
                font-weight: bold;
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
                font-weight: normal;
                color: {label_color};
                padding: 0;
                margin: 0;
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

        # Apply styling to all sections
        for i, section in enumerate(self._sections):
            language = self._language_list[i]
            color = self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE) if language is not None else background_color
            section.apply_style(text_color, color, font)

        # Main frame styling
        border = ColorRole.MESSAGE_BOOKMARK if self._is_bookmarked else ColorRole.MESSAGE_BACKGROUND
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

    def find_text(self, text: str) -> List[Tuple[int, int, MessageSectionWidget]]:
        """
        Find all instances of text in this message.

        Args:
            text: Text to search for

        Returns:
            List of (start_position, end_position, section) tuples for each match
        """
        all_matches = []
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
        Select text and scroll to a specific position.

        Args:
            section_num: Section number to scroll to
            position: Text position to scroll to

        Returns:
            QPoint: The global position of the visible cursor (for scrolling in parent)
        """
        section = self._sections[section_num]
        result = section.select_and_scroll_to_position(position)
        if result:
            return result

        # If we get here, position wasn't found
        return QPoint(0, 0)
