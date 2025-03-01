from datetime import datetime
from typing import List, Tuple, Dict, Optional

from PySide6.QtWidgets import (
    QFrame, QVBoxLayout, QLabel, QHBoxLayout, QWidget
)
from PySide6.QtCore import Signal, Qt, QPoint

from humbug.conversation.message_source import MessageSource
from humbug.gui.conversation.message_section_widget import MessageSectionWidget
from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager
from humbug.language.language_manager import LanguageManager
from humbug.syntax.conversation_parser import ConversationParser, ConversationParserState


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

        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._handle_language_changed)
        self._message_source = None
        self._message_timestamp = None

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

        # Container for message sections
        self._sections_container = QWidget(self)
        self._sections_layout = QVBoxLayout(self._sections_container)
        self._sections_layout.setContentsMargins(0, 0, 0, 0)
        self._sections_layout.setSpacing(8)
        self._layout.addWidget(self._sections_container)

        # Track sections
        self._sections: List[MessageSectionWidget] = []
        self._section_with_selection: Optional[MessageSectionWidget] = None
        self._has_code_block = False

        section = self._create_section_widget()
        self._sections.append(section)
        self._sections_layout.addWidget(section)

        # Add bookmark status
        self._is_bookmarked = False

        # Track current message style
        self._current_style: MessageSource = None

        self._style_manager = StyleManager()
        self._init_colour_mode = self._style_manager.color_mode

        self._style_manager.style_changed.connect(self._handle_style_changed)
        self._handle_style_changed()

        self._parser_state: ConversationParserState = None
        self._fence_depth = 0
        self._next_str: str = ""
        self._text_list = []

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
                        self._fence_depth += 1
                        self._text_list.append(self._next_str)
                        self._next_str = token.value
                        continue

                    case 'FENCE_END':
                        self._fence_depth -= 1
                        self._next_str += token.value
                        self._text_list.append(self._next_str)
                        self._next_str = ""
                        continue

                self._next_str += token.value

        except Exception:
            self._logger.exception("highlighting exception")

    def _parse_content_sections(self, text: str) -> List[str]:
        """
        Parse content into sections.

        This is a simple implementation that treats the entire content as one section.
        Subclasses can override this to implement more complex section parsing.

        Args:
            text: The message text content

        Returns:
            List of section text strings
        """
        lines = text.splitlines(keepends=True)
        self._next_str = ""
        self._text_list = []
        self._parser_state = None

        for line in lines:
            self._parse_line(line)

        if self._next_str:
            self._text_list.append(self._next_str)

        print(f"list:\n{self._text_list}")
        return self._text_list

    def _create_section_widget(self) -> MessageSectionWidget:
        """
        Create a new section widget.

        Returns:
            A new MessageSectionWidget instance
        """
        section = MessageSectionWidget(self._is_input, self._sections_container)
        section.selectionChanged.connect(
            lambda has_selection: self._handle_section_selection_changed(section, has_selection)
        )
        section.scrollRequested.connect(self.scrollRequested)
        section.mouseReleased.connect(self.mouseReleased)
        section.codeBlockStateChanged.connect(self._handle_code_block_state_changed)
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

    def _handle_code_block_state_changed(self, has_code_block: bool):
        """
        Handle changes in code block state.

        Args:
            has_code_block: Whether any section has a code block
        """
        if has_code_block:
            self._has_code_block = True
        else:
            # Check if any section still has a code block
            self._has_code_block = any(section.has_code_block() for section in self._sections)

        # Update horizontal scroll policy for sections with code blocks
        for section in self._sections:
            if section.has_code_block():
                section._text_area.setHorizontalScrollBarPolicy(Qt.ScrollBarAsNeeded)
            else:
                section._text_area.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)

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
        style_changed = style != self._current_style
        if style_changed:
            # Update header text with proper role
            self._update_role_text()
            self._current_style = style

            # Clear existing sections
            for section in self._sections:
                self._sections_layout.removeWidget(section)
                section.deleteLater()

            self._sections = []
            self._section_with_selection = None
            self._has_code_block = False

        # Parse content into sections
        orig_num_sections = len(self._sections)
        section_texts = self._parse_content_sections(text)

        # Create or update sections
        for i, section_text in enumerate(section_texts):
            # Create new section if needed
            if i >= len(self._sections):
                section = self._create_section_widget()
                self._sections.append(section)
                self._sections_layout.addWidget(section)

            # Update section content if we've not previously seen this
            if i >= orig_num_sections - 1:
                self._sections[i].set_content(section_text)

        # Remove any extra sections
        while len(self._sections) > len(section_texts):
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
                padding: 1px;
                margin: 0;
                background-color: {background_color};
            }}
        """)

        # Apply styling to all sections
        for section in self._sections:
            section.apply_style(text_color, background_color, font)

        # Main frame styling
        border = ColorRole.MESSAGE_BOOKMARK if self._is_bookmarked else ColorRole.MESSAGE_BACKGROUND
        self.setStyleSheet(f"""
            QFrame {{
                background-color: {background_color};
                margin: 0;
                border-radius: 8px;
                border: 2px solid {self._style_manager.get_color_str(border)}
            }}
        """)

        # If we changed colour mode then re-highlight
        if self._style_manager.color_mode != self._init_colour_mode:
            self._init_colour_mode = self._style_manager.color_mode
            # No need to call rehighlight directly, as it's handled by each section

    def find_text(self, text: str) -> List[Tuple[int, int]]:
        """
        Find all instances of text in this message.

        Args:
            text: Text to search for

        Returns:
            List of (start_position, end_position) tuples for each match
        """
        all_matches = []
        for section in self._sections:
            section_matches = section.find_text(text)
            if section_matches:
                all_matches.extend(section_matches)
        return all_matches

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
        # First clear all highlights
        self.clear_highlights()

        # Group matches by section
        section_matches: Dict[MessageSectionWidget, List[Tuple[int, int, int]]] = {}
        match_index = 0

        for _section_idx, section in enumerate(self._sections):
            section_matches[section] = []

            # Find matches in this section
            for match in section.find_text(matches[0][0] if matches else ""):
                # Store match with global index
                section_matches[section].append((match[0], match[1], match_index))
                match_index += 1

        # Highlight matches in each section
        for section, matches_with_index in section_matches.items():
            if not matches_with_index:
                continue

            # Extract just the position info
            positions = [(start, end) for start, end, _ in matches_with_index]

            # Find if current match is in this section
            section_current_idx = -1
            for i, (_, _, idx) in enumerate(matches_with_index):
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

    def select_and_scroll_to_position(self, position: int) -> QPoint:
        """
        Select text and scroll to a specific position.

        Args:
            position: Text position to scroll to

        Returns:
            QPoint: The global position of the visible cursor (for scrolling in parent)
        """
        # Find which section contains this position
        for section in self._sections:
            # Try to select in this section
            result = section.select_and_scroll_to_position(position)
            if result:
                return result

        # If we get here, position wasn't found
        return QPoint(0, 0)
