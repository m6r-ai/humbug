"""Terminal widget implementation."""

from dataclasses import dataclass


@dataclass
class TerminalSelection:
    """Represents a selection in the terminal."""
    start_row: int
    start_col: int
    end_row: int
    end_col: int

    def is_empty(self) -> bool:
        """Check if selection is empty."""
        return (
            self.start_row == self.end_row and
            self.start_col == self.end_col
        )

    def normalize(self) -> 'TerminalSelection':
        """Return normalized selection (start before end)."""
        if (
            (self.start_row > self.end_row) or
            (self.start_row == self.end_row and self.start_col > self.end_col)
        ):
            return TerminalSelection(
                self.end_row,
                self.end_col,
                self.start_row,
                self.start_col
            )

        return self
