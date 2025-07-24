"""Widget for displaying markdown preview content in the wiki."""

import logging
from typing import List, Tuple, Callable

from PySide6.QtWidgets import QWidget
from PySide6.QtCore import QPoint
from PySide6.QtGui import QColor

from humbug.color_role import ColorRole
from humbug.style_manager import StyleManager
from humbug.tabs.wiki.wiki_content_widget import WikiContentWidget
from humbug.tabs.wiki.wiki_markdown_content import WikiMarkdownContent


class WikiMarkdownPreviewContent(WikiContentWidget):
    """Widget for displaying markdown preview content in the wiki with a distinct visual container."""

    def __init__(self, parent: QWidget | None = None) -> None:
        """
        Initialize the markdown preview content widget.

        Args:
            parent: Optional parent widget
        """
        super().__init__(parent)
        self._logger = logging.getLogger("WikiMarkdownPreviewContent")
        self._content = ""

        self._style_manager = StyleManager()

        # Create layout
        spacing = int(self._style_manager.message_bubble_spacing())
        self._layout.setContentsMargins(spacing, spacing, spacing, spacing)

        # Create the actual markdown content widget
        self._markdown_content = WikiMarkdownContent(self, True)
        self._markdown_content.selectionChanged.connect(self.selectionChanged)
        self._markdown_content.scrollRequested.connect(self.scrollRequested)
        self._markdown_content.mouseReleased.connect(self.mouseReleased)
        self._markdown_content.linkClicked.connect(self.linkClicked)
        self._layout.addWidget(self._markdown_content)

        self._path: str | None = None
        self._style_manager.style_changed.connect(self._on_style_changed)
        self._on_style_changed()

    def set_content(self, text: str, path: str | None) -> None:
        """
        Set content, processing markdown.

        Args:
            text: The content text
            path: Path to the file
        """
        self._content = text
        self._path = path
        self._markdown_content.set_content(text, path)

    def has_selection(self) -> bool:
        """Check if any section has selected text."""
        return self._markdown_content.has_selection()

    def get_selected_text(self) -> str:
        """
        Get any selected text in this content.

        Returns:
            Currently selected text or empty string
        """
        return self._markdown_content.get_selected_text()

    def copy_selection(self) -> None:
        """Copy selected text to clipboard."""
        self._markdown_content.copy_selection()

    def clear_selection(self) -> None:
        """Clear any text selection in this content."""
        self._markdown_content.clear_selection()

    def find_text(self, text: str) -> List[Tuple[int, int, int]]:
        """
        Find all instances of text in this content.

        Args:
            text: Text to search for

        Returns:
            List of (section, start_position, end_position) tuples for each match
        """
        return self._markdown_content.find_text(text)

    def find_element_by_id(self, element_id: str) -> Tuple[int, int, int] | None:
        """
        Find an element with the given ID.

        Args:
            element_id: The ID to search for

        Returns:
            Tuple of (section_index, block_number, position) if found, None otherwise
        """
        return self._markdown_content.find_element_by_id(element_id)

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
        self._markdown_content.highlight_matches(
            matches, current_match_index, highlight_color, dim_highlight_color
        )

    def clear_highlights(self) -> None:
        """Clear all highlights from the content."""
        self._markdown_content.clear_highlights()

    def select_and_scroll_to_position(self, section_num: int, position: int) -> QPoint:
        """
        Select text and get position for scrolling.

        Args:
            section_num: Section number to scroll to
            position: Text position to scroll to

        Returns:
            QPoint: Position to scroll to, relative to this widget
        """
        point = self._markdown_content.select_and_scroll_to_position(section_num, position)
        return self._markdown_content.mapTo(self, point)

    def get_context_menu_actions(self) -> List[Tuple[str, Callable]]:
        """
        Get context menu actions for this content.

        Returns:
            List of (action_name, callback) tuples
        """
        return self._markdown_content.get_context_menu_actions()

    def supports_editing(self) -> bool:
        """
        Check if this content type supports editing.

        Returns:
            True if this content type supports editing, False otherwise
        """
        return self._markdown_content.supports_editing()

    def get_content_type(self) -> str:
        """
        Get the type of this content.

        Returns:
            String identifier for the content type
        """
        return "markdown_preview"

    def get_serializable_data(self) -> dict:
        """
        Get serializable data for this content.

        Returns:
            Dictionary of serializable data
        """
        return {
            "type": self.get_content_type(),
            "content": self._content,
            "path": self._path
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
        path = data.get("path")

        if not isinstance(content, str):
            return False

        self.set_content(content, path)
        return True

    def _on_style_changed(self) -> None:
        """Handle style changes."""
        factor = self._style_manager.zoom_factor()
        font = self.font()
        base_font_size = self._style_manager.base_font_size()
        font.setPointSizeF(base_font_size * factor)
        self.setFont(font)

        # Style the frame - use same style as file content for consistency
        background_color = self._style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)

        self.setStyleSheet(f"""
            QFrame {{
                background-color: {background_color};
                margin: 0;
                border-radius: {int(self._style_manager.message_bubble_spacing())}px;
                border: 0;
            }}
        """)
