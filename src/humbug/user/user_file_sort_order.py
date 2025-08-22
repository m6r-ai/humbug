"""User file sort order enumeration."""

from enum import Enum


class UserFileSortOrder(Enum):
    """Enumeration of available file sort orders for mindspace views."""
    DIRECTORIES_FIRST = "directories_first"
    ALPHABETICAL = "alphabetical"
