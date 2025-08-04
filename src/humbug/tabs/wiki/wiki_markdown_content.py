"""Widget for displaying a markdown content block in the wiki."""

import logging
from typing import Dict, List, Tuple, Callable

from PySide6.QtWidgets import QVBoxLayout, QWidget
from PySide6.QtCore import QPoint
from PySide6.QtGui import QColor

from dmarkdown import MarkdownConverter
from syntax import ProgrammingLanguage

from humbug.color_role import ColorRole
from humbug.language.language_manager import LanguageManager
from humbug.tabs.wiki.wiki_content_widget import WikiContentWidget
from humbug.tabs.wiki.wiki_markdown_content_section import WikiMarkdownContentSection


class WikiMarkdownContent(WikiContentWidget):
    """Widget for displaying markdown content in the wiki with sections."""

    def __init__(self, parent: QWidget | None = None, contained: bool = False) -> None:
        """
        Initialize the markdown content widget.

        Args:
            parent: Optional parent widget
            contained: Whether this content is contained within another widget
        """
        super().__init__(parent)

        # Set object name for QSS targeting
        self.setObjectName("WikiMarkdownContent")

        self._logger = logging.getLogger("WikiMarkdownContent")
        self._content = ""
        self._contained = contained

        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._on_language_changed)

        # Container for content sections
        self._sections_container = QWidget(self)
        self._sections_container.setObjectName("_sections_container")
        self._sections_layout = QVBoxLayout(self._sections_container)
        self._sections_layout.setContentsMargins(0, 0, 0, 0)
        self._sections_layout.setSpacing(15)
        self._layout.addWidget(self._sections_container)

        # Track sections
        self._sections: List[WikiMarkdownContentSection] = []
        self._section_with_selection: WikiMarkdownContentSection | None = None

        # Initialize markdown converter
        self._markdown_converter = MarkdownConverter()

        self.setProperty("contained", contained)

        self._style_manager.style_changed.connect(self._on_style_changed)
        self._on_style_changed()
        self._on_language_changed()

    def _on_language_changed(self) -> None:
        """Update text when language changes."""

    def _create_section_widget(self, language: ProgrammingLanguage | None = None) -> WikiMarkdownContentSection:
        """
        Create a new section widget.

        Args:
            language: Optional programming language for the section

        Returns:
            A new WikiMarkdownContentSection instance
        """
        is_input = False  # Wiki sections are never input
        section = WikiMarkdownContentSection(is_input, language, self._sections_container)
        section_type = "code" if language is not None else "text"
        section.setProperty("section_type", section_type)
        section.setProperty("contained", self._contained)

        section.selection_changed.connect(
            lambda has_selection: self._on_section_selection_changed(section, has_selection)
        )
        section.scroll_requested.connect(self.scroll_requested)
        section.mouse_released.connect(self.mouse_released)
        section.link_clicked.connect(self._on_link_clicked)

        return section

    def _on_link_clicked(self, url: str) -> None:
        """
        Handle link clicks from a section and forward them.

        Args:
            url: The URL that was clicked
        """
        # Forward the signal with the URL
        self.link_clicked.emit(url)

    def _on_section_selection_changed(self, section: WikiMarkdownContentSection, has_selection: bool) -> None:
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
        self.selection_changed.emit(has_selection)

    def set_content(self, text: str, path: str | None) -> None:
        """
        Set content, processing markdown.

        Args:
            text: The content text
            path: Path to the file
        """
        self._content = text

        # Extract sections directly using the markdown converter
        sections_data = self._markdown_converter.extract_sections(text, path)

        # Create or update sections
        for i, (node, language) in enumerate(sections_data):
            # Create new section if needed
            if i >= len(self._sections):
                section = self._create_section_widget(language)
                self._sections.append(section)
                self._sections_layout.addWidget(section)

                # Apply font and set content
                factor = self._style_manager.zoom_factor()
                font = self.font()
                base_font_size = self._style_manager.base_font_size()
                font.setPointSizeF(base_font_size * factor)
                section.apply_font(font)
                section.set_content(node)
                continue

            if i == len(self._sections) - 1:
                # Update the last section with new content
                section = self._sections[-1]
                if language != section.language():
                    section.set_language(language)

                    # Update section type property
                    section_type = "code" if language is not None else "text"
                    section.setProperty("section_type", section_type)
                    section.setProperty("contained", self._contained)

                section.set_content(node)

        # Remove any extra sections
        while len(self._sections) > len(sections_data):
            section = self._sections.pop()
            self._sections_layout.removeWidget(section)
            section.deleteLater()

        # Apply stylesheet after content changes
        self._apply_stylesheet()

    def has_selection(self) -> bool:
        """Check if any section has selected text."""
        return self._section_with_selection is not None and self._section_with_selection.has_selection()

    def get_selected_text(self) -> str:
        """
        Get any selected text in this content.

        Returns:
            Currently selected text or empty string
        """
        if self._section_with_selection:
            return self._section_with_selection.get_selected_text()

        return ""

    def copy_selection(self) -> None:
        """Copy selected text to clipboard."""
        if self._section_with_selection:
            self._section_with_selection.copy_selection()

    def clear_selection(self) -> None:
        """Clear any text selection in this content."""
        if self._section_with_selection:
            self._section_with_selection.clear_selection()
            self._section_with_selection = None

    def _build_container_styles(self) -> str:
        """Build styles for the main container."""
        style_manager = self._style_manager
        return f"""
            QWidget#WikiMarkdownContent {{
                background-color: {style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
            }}

            QWidget#WikiMarkdownContent[contained="true"] {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
            }}

            #WikiMarkdownContent QWidget#_sections_container {{
                background-color: transparent;
                border: none;
                margin: 0;
                padding: 0;
            }}
        """

    def _build_header_styles(self) -> str:
        """Build styles for language headers within sections."""
        style_manager = self._style_manager
        return f"""
            QFrame#WikiMarkdownContentSection QLabel {{
                color: {style_manager.get_color_str(ColorRole.MESSAGE_LANGUAGE)};
                background-color: transparent;
                margin: 0;
                padding: 0;
            }}
        """

    def _build_section_styles(self) -> str:
        """Build styles for wiki sections with all 3 background combinations."""
        style_manager = self._style_manager
        border_radius = int(style_manager.message_bubble_spacing())

        return f"""
            /* Default section styling */
            QFrame#WikiMarkdownContentSection {{
                margin: 0;
                border-radius: {border_radius}px;
                border: 0;
            }}

            /* Text sections - normal (not contained) */
            QFrame#WikiMarkdownContentSection[section_type="text"][contained="false"] {{
                background-color: {style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
            }}

            /* Text sections - contained */
            QFrame#WikiMarkdownContentSection[section_type="text"][contained="true"] {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
            }}

            /* Code sections - normal (not contained) */
            QFrame#WikiMarkdownContentSection[section_type="code"][contained="false"] {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
            }}

            /* Code sections - contained */
            QFrame#WikiMarkdownContentSection[section_type="code"][contained="true"] {{
                background-color: {style_manager.get_color_str(ColorRole.BACKGROUND_TERTIARY)};
            }}

            /* Text areas within sections */
            #WikiMarkdownContentSection QTextEdit {{
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                background-color: transparent;
                border: none;
                padding: 0;
                margin: 0;
                selection-background-color: {style_manager.get_color_str(ColorRole.TEXT_SELECTED)};
            }}

            /* Header containers within sections */
            #WikiMarkdownContentSection QWidget {{
                background-color: transparent;
                margin: 0;
                padding: 0;
            }}

            /* Scrollbars within sections */
            #WikiMarkdownContentSection QScrollBar:horizontal {{
                height: 12px;
                background: {style_manager.get_color_str(ColorRole.SCROLLBAR_BACKGROUND)};
            }}
            #WikiMarkdownContentSection QScrollBar::handle:horizontal {{
                background: {style_manager.get_color_str(ColorRole.SCROLLBAR_HANDLE)};
                min-width: 20px;
            }}
            #WikiMarkdownContentSection QScrollBar::add-page:horizontal,
            #WikiMarkdownContentSection QScrollBar::sub-page:horizontal {{
                background: none;
            }}
            #WikiMarkdownContentSection QScrollBar::add-line:horizontal,
            #WikiMarkdownContentSection QScrollBar::sub-line:horizontal {{
                width: 0px;
            }}
        """

    def _apply_stylesheet(self) -> None:
        """Apply the stylesheet to this content widget and all sections."""
        # Build sections: container, sections, headers
        stylesheet_parts = [
            self._build_container_styles(),
            self._build_header_styles(),
            self._build_section_styles()
        ]

        shared_stylesheet = "\n".join(stylesheet_parts)
        self.setStyleSheet(shared_stylesheet)

    def _on_style_changed(self) -> None:
        """Handle the style changing."""
        factor = self._style_manager.zoom_factor()
        font = self.font()
        base_font_size = self._style_manager.base_font_size()
        font.setPointSizeF(base_font_size * factor)
        self.setFont(font)

        # Apply fonts to all sections
        for section in self._sections:
            section.apply_font(font)

        self._apply_stylesheet()

    def find_text(self, text: str) -> List[Tuple[int, int, int]]:
        """
        Find all instances of text in this content.

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

    def find_element_by_id(self, element_id: str) -> Tuple[int, int, int] | None:
        """
        Find an element with the given ID.

        Args:
            element_id: The ID to search for

        Returns:
            Tuple of (section_index, block_number, position) if found, None otherwise
        """
        for i, section in enumerate(self._sections):
            position = section.find_element_by_id(element_id)
            if position is not None:
                return (i, position[0], position[1])

        return None

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
        # First clear all highlights
        self.clear_highlights()

        if not matches:
            return

        # Group matches by section
        section_matches: Dict[WikiMarkdownContentSection, List[Tuple[int, int, int]]] = {}
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

    def clear_highlights(self) -> None:
        """Clear all highlights from the content."""
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

        return actions

    def supports_editing(self) -> bool:
        """
        Check if this content type supports editing.

        Returns:
            True if this content type supports editing, False otherwise
        """
        return False

    def get_content_type(self) -> str:
        """
        Get the type of this content.

        Returns:
            String identifier for the content type
        """
        return "markdown"

    def get_serializable_data(self) -> dict:
        """
        Get serializable data for this content.

        Returns:
            Dictionary of serializable data
        """
        return {
            "type": self.get_content_type(),
            "content": self._content
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
        if not isinstance(content, str):
            return False

        self.set_content(content, None)
        return True
