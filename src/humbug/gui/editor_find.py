"""Implementation of find functionality for EditorTab."""

from typing import List, Tuple

from PySide6.QtGui import QTextCursor
from PySide6.QtWidgets import QTextEdit

class EditorFind:
    """Handles find operations in editor text."""

    def __init__(self, editor):
        """Initialize find handler.
        
        Args:
            editor: The QTextEdit instance to search in
        """
        self._editor = editor
        self._matches: List[Tuple[int, int]] = []  # List of (start, end) positions
        self._current_match = -1
        self._last_search = ""
        self._extra_selections = []

    def find_text(self, text: str, forward: bool = True) -> None:
        """Find all instances of text and highlight them.
        
        Args:
            text: Text to search for
            forward: Whether to search forward from current position
        """
        # Clear existing highlights if search text changed
        if text != self._last_search:
            self._clear_highlights()
            self._matches = []
            self._current_match = -1
            self._last_search = text

        document = self._editor.document()

        # Find all matches if this is a new search
        if not self._matches and text:
            cursor = QTextCursor(document)
            while True:
                cursor = document.find(text, cursor)
                if cursor.isNull():
                    break
                self._matches.append((cursor.selectionStart(), cursor.selectionEnd()))

        if not self._matches:
            return

        # Move to next/previous match
        if forward:
            self._current_match = (self._current_match + 1) % len(self._matches)
        else:
            self._current_match = (self._current_match - 1) if self._current_match > 0 else len(self._matches) - 1

        # Highlight all matches
        self._highlight_matches()

        # Scroll to current match
        self._scroll_to_match(self._current_match)

    def _highlight_matches(self) -> None:
        """Update the highlighting of all matches."""
        self._clear_highlights()

                    # Create selection format
        selection_format = self._editor.currentCharFormat()
        selection_format.setBackground(self._editor.palette().highlight())
        selection_format.setForeground(self._editor.palette().highlightedText())

        # Highlight all matches
        for i, (start, end) in enumerate(self._matches):
            cursor = QTextCursor(self._editor.document())
            cursor.setPosition(start)
            cursor.setPosition(end, QTextCursor.KeepAnchor)

            # Create extra selection
            extra_selection = QTextEdit.ExtraSelection()
            extra_selection.cursor = cursor
            
            # Use different format for current match
            if i == self._current_match:
                extra_selection.format = selection_format
            else:
                # Create less prominent format for other matches
                other_format = self._editor.currentCharFormat()
                other_format.setBackground(self._editor.palette().highlight().color().lighter())
                extra_selection.format = other_format

            self._extra_selections.append(extra_selection)

        self._editor.setExtraSelections(self._extra_selections)

    def _scroll_to_match(self, match_index: int) -> None:
        """Scroll to ensure the given match is visible.
        
        Args:
            match_index: Index of match to scroll to
        """
        if 0 <= match_index < len(self._matches):
            cursor = QTextCursor(self._editor.document())
            cursor.setPosition(self._matches[match_index][0])
            self._editor.setTextCursor(cursor)
            self._editor.ensureCursorVisible()

    def _clear_highlights(self) -> None:
        """Clear all search highlights."""
        self._extra_selections = []
        self._editor.setExtraSelections([])

    def get_match_status(self) -> Tuple[int, int]:
        """Get the current match status.
        
        Returns:
            Tuple of (current_match, total_matches)
        """
        if not self._matches:
            return 0, 0
        return self._current_match + 1, len(self._matches)

    def clear(self) -> None:
        """Clear all find state."""
        self._clear_highlights()
        self._matches = []
        self._current_match = -1
        self._last_search = ""