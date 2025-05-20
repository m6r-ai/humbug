"""Base class for all wiki content types."""

import logging
from typing import List, Tuple, Optional

from PySide6.QtWidgets import QFrame, QVBoxLayout, QWidget
from PySide6.QtCore import Signal, QPoint
from PySide6.QtGui import QColor

from humbug.gui.style_manager import StyleManager


class WikiContent(QFrame):
    """
    Base class for all types of wiki content.

    This abstract base class defines the common interface and functionality
    that all wiki content types must implement. Subclasses will provide
    specific implementations for different content types (markdown, images, etc.).

    Signals:
        selectionChanged: Emitted when text selection changes, with a boolean indicating if there's a selection
        scrollRequested: Emitted when scrolling is requested, with the target position
        mouseReleased: Emitted when the mouse button is released
        linkClicked: Emitted when a link is clicked, with the URL
        edit_clicked: Emitted when the edit button is clicked
    """

    selectionChanged = Signal(bool)
    scrollRequested = Signal(QPoint)
    mouseReleased = Signal()
    linkClicked = Signal(str)
    edit_clicked = Signal()

    def __init__(self, parent: QWidget | None = None) -> None:
        """
        Initialize the base content widget.

        Args:
            parent: Optional parent widget
        """
        super().__init__(parent)
        self._logger = logging.getLogger(self.__class__.__name__)

        # Set frame style
        self.setFrameStyle(QFrame.Shape.Box | QFrame.Shadow.Plain)

        # Initialize style manager
        self._style_manager = StyleManager()
        self._style_manager.style_changed.connect(self._handle_style_changed)

        # Create layout
        self._layout = QVBoxLayout(self)
        self.setLayout(self._layout)
        self._layout.setContentsMargins(0, 0, 0, 0)
        self._layout.setSpacing(0)

    def _handle_style_changed(self) -> None:
        """
        Handle style changes.

        This method is called when the application style changes, such as
        when the zoom factor or color scheme is updated. Subclasses should
        override this to update their specific styling.
        """

    def has_selection(self) -> bool:
        """
        Check if any content has selected text.

        Returns:
            True if there is selected text, False otherwise
        """
        return False

    def get_selected_text(self) -> str:
        """
        Get any selected text in this content.

        Returns:
            Currently selected text or empty string
        """
        return ""

    def copy_selection(self) -> None:
        """
        Copy selected text to clipboard.

        This method should copy any selected text to the system clipboard.
        If no text is selected, this method should do nothing.
        """

    def clear_selection(self) -> None:
        """
        Clear any text selection in this content.

        This method should clear any text selection in the content widget.
        """

    def find_text(self, _text: str) -> List[Tuple[int, int, int]]:
        """
        Find all instances of text in this content.

        This method searches for the given text in the content and returns
        a list of tuples containing the section index, start position, and
        end position of each match.

        Args:
            text: Text to search for

        Returns:
            List of (section, start_position, end_position) tuples for each match
        """
        return []

    def find_element_by_id(self, _element_id: str) -> Optional[Tuple[int, int, int]]:
        """
        Find an element with the given ID.

        This method searches for an element with the given ID in the content
        and returns a tuple containing the section index, block number, and
        position of the element if found.

        Args:
            element_id: The ID to search for

        Returns:
            Tuple of (section_index, block_number, position) if found, None otherwise
        """
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

        This method highlights the specified matches in the content. The current
        match is highlighted with a different color.

        Args:
            matches: List of (section, start_position, end_position) tuples to highlight
            current_match_index: Index of current match to highlight differently, or -1 for none
            highlight_color: QColor for current match, defaults to system highlight color
            dim_highlight_color: QColor for other matches, defaults to dimmer highlight color
        """

    def clear_highlights(self) -> None:
        """
        Clear all highlights from the content.

        This method removes any search highlights from the content.
        """

    def select_and_scroll_to_position(self, _section_num: int, _position: int) -> QPoint:
        """
        Select text and get position for scrolling.

        This method selects text at the specified position and returns the
        position to scroll to in order to make the selection visible.

        Args:
            section_num: Section number to scroll to
            position: Text position to scroll to

        Returns:
            QPoint: Position to scroll to, relative to this widget
        """
        return QPoint(0, 0)

    def get_context_menu_actions(self) -> List[Tuple[str, callable]]:
        """
        Get context menu actions for this content.

        This method returns a list of actions that should be available
        in the context menu for this content. Each action is represented
        as a tuple of (name, callback).

        Returns:
            List of (action_name, callback) tuples
        """
        return []

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

        This method returns a string identifier for the content type.

        Returns:
            String identifier for the content type
        """
        return "base"

    def get_serializable_data(self) -> dict:
        """
        Get serializable data for this content.

        This method returns a dictionary of data that can be serialized
        to JSON for storage or transmission.

        Returns:
            Dictionary of serializable data
        """
        return {
            "type": self.get_content_type()
        }

    def update_from_serialized_data(self, data: dict) -> bool:
        """
        Update this content from serialized data.

        This method updates the content based on the provided serialized data.

        Args:
            data: Dictionary of serialized data

        Returns:
            True if the update was successful, False otherwise
        """
        return data.get("type") == self.get_content_type()
