"""Row descriptor for the side-by-side diff view."""

from dataclasses import dataclass
from enum import Enum, auto


class DiffRowType(Enum):
    """The visual kind of a single row in the side-by-side view."""

    CONTEXT = auto()    # Unchanged line, shown identically on both sides
    REMOVED = auto()    # Line present only on the left (old) side
    ADDED = auto()      # Line present only on the right (new) side
    CHANGED = auto()    # Paired removal/addition — one line on each side
    FILLER = auto()     # Blank placeholder to keep the two sides aligned
    HEADER = auto()     # Hunk header (@@ ... @@), spans the full width


@dataclass
class DiffRow:
    """A single row in the side-by-side diff view.

    For CONTEXT and CHANGED rows both sides carry text.
    For REMOVED rows only left_text is set; right_text is empty.
    For ADDED rows only right_text is set; left_text is empty.
    For FILLER rows both sides are empty and line numbers are None.
    For HEADER rows left_text holds the hunk header; right_text is empty.
    Line numbers are 1-indexed and None when the side has no content.
    """

    row_type: DiffRowType
    left_text: str
    right_text: str
    left_line_no: int | None
    right_line_no: int | None
