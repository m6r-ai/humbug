"""Row descriptor and view mode for the diff view."""

from dataclasses import dataclass
from enum import Enum, auto


class DiffViewMode(Enum):
    """Layout mode for the diff tab."""

    INLINE = auto()          # Single-column unified diff (narrow)
    SIDE_BY_SIDE = auto()    # Two-column split (wide)


class DiffRowType(Enum):
    """The visual kind of a single row in the diff view."""

    CONTEXT = auto()    # Unchanged line, shown identically on both sides
    REMOVED = auto()    # Line present only on the left (old) side
    ADDED = auto()      # Line present only on the right (new) side
    CHANGED = auto()    # Paired removal/addition — one line on each side


@dataclass
class DiffRow:
    """
    A single row in the diff view.

    For CONTEXT and CHANGED rows both sides carry text.
    For REMOVED rows only left_text is set; right_text is empty.
    For ADDED rows only right_text is set; left_text is empty.
    Line numbers are 1-indexed and None when the side has no content.
    """

    row_type: DiffRowType
    left_text: str
    right_text: str
    left_line_no: int | None
    right_line_no: int | None
