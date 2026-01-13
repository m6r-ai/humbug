"""Widget for displaying a markdown content block in the preview."""

import logging
from typing import Dict, List, Tuple, Callable

from PySide6.QtWidgets import QVBoxLayout, QWidget
from PySide6.QtCore import QPoint
from PySide6.QtGui import QColor

from dmarkdown import MarkdownConverter
from syntax import ProgrammingLanguage

from humbug.tabs.preview.preview_content_widget import PreviewContentWidget
from humbug.tabs.preview.preview_markdown_content_section import PreviewMarkdownContentSection


class PreviewMarkdownContent(PreviewContentWidget):
    """Widget for displaying markdown content in the preview with sections."""

    def __init__(self, parent: QWidget | None = None, contained: bool = False) -> None:
        """
        Initialize the markdown content widget.

        Args:
            parent: Optional parent widget
            contained: Whether this content is contained within another widget
        """
        super().__init__(parent)

        # Set object name for QSS targeting
        self.setObjectName("PreviewMarkdownContent")

        self._logger = logging.getLogger("PreviewMarkdownContent")
        self._content = ""
        self._contained = contained

        # Container for content sections
        self._sections_container = QWidget(self)
        self._sections_container.setObjectName("_sections_container")
        self._sections_layout = QVBoxLayout(self._sections_container)
        self._sections_layout.setContentsMargins(0, 0, 0, 0)
        self._sections_layout.setSpacing(15)
        self._layout.addWidget(self._sections_container)

        # Track sections
        self._sections: List[PreviewMarkdownContentSection] = []
        self._section_with_selection: PreviewMarkdownContentSection | None = None

        # Initialize markdown converter
        self._markdown_converter = MarkdownConverter()

        self.setProperty("contained", contained)

        # Call apply_style directly to initialize styling
        self.apply_style()

    def _create_section_widget(self, syntax: ProgrammingLanguage | None = None) -> PreviewMarkdownContentSection:
        """
        Create a new section widget.

        Args:
            syntax: Optional programming language for the section

        Returns:
            A new PreviewMarkdownContentSection instance
        """
        section = PreviewMarkdownContentSection(syntax, self._sections_container)
        section_type = "code" if syntax is not None else "text"
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

    def _on_section_selection_changed(self, section: PreviewMarkdownContentSection, has_selection: bool) -> None:
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
        for _, (node, syntax) in enumerate(sections_data):
            section = self._create_section_widget(syntax)
            section.set_content(node)
            section.apply_style()
            self._sections.append(section)
            self._sections_layout.addWidget(section)

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

    def apply_style(self) -> None:
        """Apply styling to this content widget and all sections."""
        for section in self._sections:
            section.apply_style()

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
        section_matches: Dict[PreviewMarkdownContentSection, List[Tuple[int, int, int]]] = {}
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
