"""Implementation of find functionality for TerminalWidget."""

from dataclasses import dataclass
from typing import List, Tuple, Optional

from PySide6.QtGui import QTextCharFormat, QFontMetrics

from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager


@dataclass
class TerminalMatch:
    """Represents a match in the terminal buffer."""
    row: int  # Row in buffer where match was found
    start_col: int  # Starting column of match
    end_col: int  # Ending column of match


class TerminalFind:
    """Handles find operations in terminal text."""

    def __init__(self, terminal_widget):
        """
        Initialize find handler.

        Args:
            terminal_widget: The TerminalWidget instance to search in
        """
        super().__init__()
        self._terminal = terminal_widget
        self._matches: List[TerminalMatch] = []
        self._current_match = -1
        self._last_search = ""
        self._selection_active = False

        self._style_manager = StyleManager()
        self._style_manager.style_changed.connect(self._handle_style_changed)

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
                            end_col=pos + len(text)
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

        # Update highlights
        self._update_highlights()

        # Ensure current match is visible
        self._scroll_to_match()

    def _update_highlights(self) -> None:
        """Update highlight display in terminal."""
        # Clear existing highlights
        self._terminal.clear_search_highlights()

        if not self._selection_active:
            return

        # Group matches by row
        row_matches = {}
        for i, match in enumerate(self._matches):
            if match.row not in row_matches:
                row_matches[match.row] = []

            fmt = self.get_highlight_format(i)
            if fmt:
                row_matches[match.row].append(
                    (match.start_col, match.end_col, fmt)
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

    def _find_all_matches(self, text: str) -> None:
        """
        Find all instances of text in terminal buffer.

        Args:
            text: Text to search for
        """
        if not text:
            return

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
                    end_col=pos + len(text)
                ))
                pos += 1

        self._selection_active = bool(self._matches)

    def _handle_style_changed(self) -> None:
        """Handle style changes by ensuring highlighted matches are redrawn."""
        if self._matches:
            self._terminal.viewport().update()

    def _scroll_to_match(self) -> None:
        """Scroll to ensure the current match is visible."""
        if not self._matches or self._current_match == -1:
            return

        match = self._matches[self._current_match]

        # Calculate scroll position needed to make match visible
        viewport_height = self._terminal.viewport().height()
        font_metrics = QFontMetrics(self._terminal.font())
        char_height = font_metrics.height()
        visible_lines = viewport_height // char_height

        # Get effective row position in viewport
        scroll_pos = self._terminal.verticalScrollBar().value()
        viewport_row = match.row - scroll_pos

        # If match is outside visible area, scroll to it
        if viewport_row < 0:
            # Match is above visible area - scroll up
            self._terminal.verticalScrollBar().setValue(match.row)
        elif viewport_row >= visible_lines:
            # Match is below visible area - scroll down
            scroll_to = match.row - visible_lines + 1
            self._terminal.verticalScrollBar().setValue(scroll_to)

    def get_match_status(self) -> Tuple[int, int]:
        """
        Get the current match status.

        Returns:
            Tuple of (current_match, total_matches)
        """
        if not self._matches:
            return 0, 0

        return self._current_match + 1, len(self._matches)

    def get_highlight_format(self, match_index: int) -> Optional[QTextCharFormat]:
        """
        Get highlight format for a match.

        Args:
            match_index: Index of match to get format for

        Returns:
            QTextCharFormat for highlighting or None if not a match
        """
        if not self._selection_active:
            return None

        fmt = QTextCharFormat()
        if match_index == self._current_match:
            fmt.setBackground(self._style_manager.get_color(ColorRole.TEXT_SELECTED))
        else:
            fmt.setBackground(self._style_manager.get_color(ColorRole.TEXT_DIM_SELECTED))
        return fmt

    def get_matches_for_row(self, row: int) -> List[Tuple[int, int, QTextCharFormat]]:
        """
        Get all matches for a given row with their formats.

        Args:
            row: Row to get matches for

        Returns:
            List of (start_col, end_col, format) tuples
        """
        if not self._selection_active:
            return []

        matches = []
        for i, match in enumerate(self._matches):
            if match.row == row:
                fmt = self.get_highlight_format(i)
                if fmt:
                    matches.append((match.start_col, match.end_col, fmt))

        return matches
