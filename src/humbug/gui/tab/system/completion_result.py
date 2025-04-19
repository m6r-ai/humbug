"""A class representing the result of a tab completion operation."""

from typing import NamedTuple


class CompletionResult(NamedTuple):
    """Represents the result of a tab completion request."""
    success: bool
    replacement: str | None = None     # The text to insert as replacement
    start_pos: int = 0                 # Start position for replacement
    end_pos: int = 0                   # End position for replacement
    add_space: bool = False            # Whether to add a space after
