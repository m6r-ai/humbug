"""Git working-tree status data types."""

from dataclasses import dataclass
from enum import Enum, auto


class GitStatusCode(Enum):
    """Normalised status category for a changed file."""
    MODIFIED = auto()
    ADDED = auto()
    DELETED = auto()
    RENAMED = auto()
    COPIED = auto()
    UNTRACKED = auto()
    UNKNOWN = auto()


@dataclass(frozen=True)
class GitFileStatus:
    """Status information for a single file reported by git."""
    code: GitStatusCode
    path: str                       # Absolute path to the file (new path for renames)
    original_path: str | None       # Absolute path to the original file (renames/copies only)
