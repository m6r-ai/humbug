from typing import List, Tuple

from PySide6.QtGui import QTextCursor, QTextCharFormat
from PySide6.QtWidgets import QTextEdit

from humbug.gui.color_role import ColorRole
from humbug.gui.message_widget import MessageWidget
from humbug.gui.style_manager import StyleManager

class ConversationFind:
    """Handles find operations in conversation messages."""

    def __init__(self):
        """Initialize find handler."""
        self._matches: List[Tuple[MessageWidget, List[Tuple[int, int]]]] = []  # List of (widget, [(start, end)])
        self._current_widget_index = -1
        self._current_match_index = -1
        self._last_search = ""
        self._extra_selections = []

        self._style_manager = StyleManager()

    def find_text(self, text: str, widgets: List[MessageWidget], forward: bool = True) -> None:
        """Find all instances of text and highlight them.
        
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
                text_edit = widget._text_area
                document = text_edit.document()
                widget_matches = []

                cursor = QTextCursor(document)
                while True:
                    cursor = document.find(text, cursor)
                    if cursor.isNull():
                        break
                    widget_matches.append((cursor.selectionStart(), cursor.selectionEnd()))

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

    def _highlight_matches(self) -> None:
        """Update the highlighting of all matches."""
        self._clear_highlights()

        # Create selection format for current match
        current_format = QTextCharFormat()
        current_format.setBackground(self._style_manager.get_color(ColorRole.TEXT_SELECTED))
        current_format.setForeground(self._style_manager.get_color(ColorRole.TEXT_PRIMARY))

        # Create less prominent format for other matches
        other_format = QTextCharFormat()
        other_color = self._style_manager.get_color(ColorRole.TEXT_SELECTED).lighter(120)
        other_format.setBackground(other_color)

        # Create a dictionary to collect selections per text edit
        selections_by_editor = {}

        # Highlight all matches
        for widget_idx, (widget, matches) in enumerate(self._matches):
            text_edit = widget._text_area
            if text_edit not in selections_by_editor:
                selections_by_editor[text_edit] = []

            for match_idx, (start, end) in enumerate(matches):
                cursor = QTextCursor(text_edit.document())
                cursor.setPosition(start)
                cursor.setPosition(end, QTextCursor.KeepAnchor)

                # Create extra selection
                extra_selection = QTextEdit.ExtraSelection()
                extra_selection.cursor = cursor

                # Use different format for current match
                if widget_idx == self._current_widget_index and match_idx == self._current_match_index:
                    extra_selection.format = current_format
                else:
                    extra_selection.format = other_format

                selections_by_editor[text_edit].append(extra_selection)

        # Apply all selections at once for each text edit
        for text_edit, selections in selections_by_editor.items():
            text_edit.setExtraSelections(selections)

    def _scroll_to_match(self) -> None:
        """Scroll to ensure the current match is visible."""
        if not self._matches:
            return

        widget, matches = self._matches[self._current_widget_index]
        start, _ = matches[self._current_match_index]

        text_edit = widget._text_area
        cursor = QTextCursor(text_edit.document())
        cursor.setPosition(start)
        text_edit.setTextCursor(cursor)
        text_edit.ensureCursorVisible()

        # Ensure the widget itself is visible in the scroll area
        widget.ensureVisible(0, 0, 0, 50)

    def _clear_highlights(self) -> None:
        """Clear all search highlights."""
        for text_edit, _ in self._extra_selections:
            text_edit.setExtraSelections([])
        self._extra_selections = []

    def get_match_status(self) -> Tuple[int, int]:
        """Get the current match status.
        
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
