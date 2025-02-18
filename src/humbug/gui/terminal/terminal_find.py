"""Implementation of find functionality for TerminalWidget."""

from dataclasses import dataclass
from typing import List, Tuple


@dataclass
class TerminalMatch:
    """Represents a match in the terminal buffer."""
    row: int           # Row in buffer where match was found
    start_col: int     # Starting column of match
    end_col: int       # Ending column of match
    is_current: bool   # Whether this is the currently selected match


class TerminalFind:
    """Handles find operations in terminal text."""

    def __init__(self, terminal_widget):
        """
        Initialize find handler.

        Args:
            terminal_widget: The TerminalWidget instance to search in
        """
        self._terminal = terminal_widget
        self._matches: List[TerminalMatch] = []
        self._current_match = -1
        self._last_search = ""
        self._selection_active = False

    def find_text(self, text: str, forward: bool = True) -> None:
        """
        Find all instances of text and highlight them.

        Args:
            text: Text to search for
            forward: Whether to search forward from current position
        """
        # Clear existing highlights if search text changed
        if text != self._last_search:
            self._terminal.clear_search_highlights()
            self._matches = []
            self._current_match = -1
            self._last_search = text

            # Find all matches
            if text:
                buffer = self._terminal._state.current_buffer
                rows = buffer.history_lines

                # Search through all lines in buffer
                for row in range(rows):
                    line = buffer.lines[row]
                    line_text = ""

                    # Build text for this line
                    for col in range(buffer.cols):
                        char, _, _, _ = line.get_character(col)
                        line_text += char

                    # Find all matches in this line
                    pos = 0
                    while True:
                        pos = line_text.find(text, pos)
                        if pos == -1:
                            break

                        # Add match to list
                        self._matches.append(TerminalMatch(
                            row=row,
                            start_col=pos,
                            end_col=pos + len(text),
                            is_current=False  # All matches start as non-current
                        ))
                        pos += 1

                # Set selection active if we found any matches
                self._selection_active = bool(self._matches)

        if not self._matches:
            return

        # Move to next/previous match
        if self._current_match == -1:
            # First search - start at beginning or end depending on direction
            self._current_match = 0 if forward else len(self._matches) - 1
        else:
            # Move to next/previous match
            if forward:
                self._current_match = (self._current_match + 1) % len(self._matches)
            else:
                self._current_match = (self._current_match - 1) if self._current_match > 0 else len(self._matches) - 1

        # Update current match status
        self._update_current_match()

        # Update highlights
        self._update_highlights()

        # Ensure current match is visible
        self._scroll_to_match()

    def _update_current_match(self) -> None:
        """Update which match is marked as current."""
        for i, match in enumerate(self._matches):
            match.is_current = (i == self._current_match)

    def _update_highlights(self) -> None:
        """Update highlight display in terminal."""
        # Clear existing highlights
        self._terminal.clear_search_highlights()

        if not self._selection_active:
            return

        # Group matches by row
        row_matches = {}
        for match in self._matches:
            if match.row not in row_matches:
                row_matches[match.row] = []
            row_matches[match.row].append(
                (match.start_col, match.end_col, match.is_current)
            )

        # Update terminal with new highlights
        for row, highlights in row_matches.items():
            self._terminal.set_search_highlights(row, highlights)

    def clear(self) -> None:
        """Clear all find state."""
        self._matches = []
        self._current_match = -1
        self._last_search = ""
        self._selection_active = False
        self._terminal.clear_search_highlights()

    def _scroll_to_match(self) -> None:
        """Scroll to ensure the current match is visible."""
        if not self._matches or self._current_match == -1:
            return

        match = self._matches[self._current_match]
        self._terminal.scroll_to_match(match.row)

    def get_match_status(self) -> Tuple[int, int]:
        """
        Get the current match status.

        Returns:
            Tuple of (current_match, total_matches)
        """
        if not self._matches:
            return 0, 0

        return self._current_match + 1, len(self._matches)

    def get_matches_for_row(self, row: int) -> List[Tuple[int, int, bool]]:
        """
        Get all matches for a given row with their current status.

        Args:
            row: Row to get matches for

        Returns:
            List of (start_col, end_col, is_current) tuples
        """
        if not self._selection_active:
            return []

        matches = []
        for match in self._matches:
            if match.row == row:
                matches.append((match.start_col, match.end_col, match.is_current))

        return matches
