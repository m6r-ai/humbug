from typing import List, Tuple, Set
from PySide6.QtWidgets import QWidget
from PySide6.QtCore import QObject, Signal

from humbug.gui.color_role import ColorRole
from humbug.gui.conversation.message_widget import MessageWidget
from humbug.gui.style_manager import StyleManager

class ConversationFind(QObject):
    """Handles find operations in conversation messages."""

    # Signal to request scrolling to a specific widget and position
    scrollRequested = Signal(QWidget, int)  # Widget to scroll to, position within widget

    def __init__(self):
        """Initialize find handler."""
        super().__init__()
        self._matches: List[Tuple[MessageWidget, List[Tuple[int, int]]]] = []  # List of (widget, [(start, end)])
        self._current_widget_index = -1
        self._current_match_index = -1
        self._last_search = ""

        # Track text edits with highlights
        self._highlighted_widgets: Set[MessageWidget] = set()

        self._style_manager = StyleManager()
        self._style_manager.style_changed.connect(self._handle_style_changed)

    def find_text(self, text: str, widgets: List[MessageWidget], forward: bool = True) -> None:
        """
        Find all instances of text and highlight them.

        Args:
            text: Text to search for
            widgets: List of message widgets to search in
            forward: Whether to search forward from current position
        """
        # Clear existing highlights if search text changed
        if text != self._last_search:
            self._clear_highlights()
            self._matches = []
            self._current_widget_index = -1
            self._current_match_index = -1
            self._last_search = text

        # Find all matches if this is a new search
        if not self._matches and text:
            for widget in widgets:
                widget_matches = widget.find_text(text)

                if widget_matches:
                    self._matches.append((widget, widget_matches))

        if not self._matches:
            return

        # Move to next/previous match
        if self._current_widget_index == -1:
            # First search - start at beginning or end depending on direction
            if forward:
                self._current_widget_index = 0
                self._current_match_index = 0
            else:
                self._current_widget_index = len(self._matches) - 1
                self._current_match_index = len(self._matches[self._current_widget_index][1]) - 1
        else:
            # Move to next/previous match
            if forward:
                self._current_match_index += 1
                # If we've reached the end of matches in current widget
                if self._current_match_index >= len(self._matches[self._current_widget_index][1]):
                    self._current_widget_index += 1
                    # If we've reached the end of widgets, wrap around
                    if self._current_widget_index >= len(self._matches):
                        self._current_widget_index = 0
                    self._current_match_index = 0
            else:
                self._current_match_index -= 1
                # If we've reached the start of matches in current widget
                if self._current_match_index < 0:
                    self._current_widget_index -= 1
                    # If we've reached the start of widgets, wrap around
                    if self._current_widget_index < 0:
                        self._current_widget_index = len(self._matches) - 1
                    self._current_match_index = len(self._matches[self._current_widget_index][1]) - 1

        # Highlight all matches
        self._highlight_matches()

        # Scroll to current match
        self._scroll_to_match()

    def _handle_style_changed(self) -> None:
        """Handle style changes"""
        self._highlight_matches()

    def _highlight_matches(self) -> None:
        """Update the highlighting of all matches."""
        self._clear_highlights()

        # Get colors from style manager
        highlight_color = self._style_manager.get_color(ColorRole.TEXT_FOUND)
        dim_highlight_color = self._style_manager.get_color(ColorRole.TEXT_FOUND_DIM)

        # Highlight matches in each widget
        for widget_idx, (widget, matches) in enumerate(self._matches):
            # Set current_match_index to highlight the current match
            current_match_index = self._current_match_index if widget_idx == self._current_widget_index else -1

            # Highlight matches in this widget
            widget.highlight_matches(matches, current_match_index, highlight_color, dim_highlight_color)

            # Track highlighted widgets
            self._highlighted_widgets.add(widget)

    def _scroll_to_match(self) -> None:
        """Request scroll to ensure the current match is visible."""
        if not self._matches:
            return

        widget, matches = self._matches[self._current_widget_index]
        start, _ = matches[self._current_match_index]

        # Use the widget's method to select and get position for scrolling
        global_pos = widget.select_and_scroll_to_position(start)

        # Emit signal for parent to handle scrolling
        self.scrollRequested.emit(widget, start)

    def _clear_highlights(self) -> None:
        """Clear all search highlights."""
        # Clear highlights from all tracked widgets
        for widget in self._highlighted_widgets:
            widget.clear_highlights()

        self._highlighted_widgets.clear()

    def get_match_status(self) -> Tuple[int, int]:
        """
        Get the current match status.

        Returns:
            Tuple of (current_match, total_matches)
        """
        if not self._matches:
            return 0, 0

        total_matches = sum(len(matches) for _, matches in self._matches)
        if self._current_widget_index == -1:
            return 0, total_matches

        current_match = sum(len(matches) for _, matches in self._matches[:self._current_widget_index])
        current_match += self._current_match_index + 1

        return current_match, total_matches

    def clear(self) -> None:
        """Clear all find state."""
        self._clear_highlights()
        self._matches = []
        self._current_widget_index = -1
        self._current_match_index = -1
        self._last_search = ""
