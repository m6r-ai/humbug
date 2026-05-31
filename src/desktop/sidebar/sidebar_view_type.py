from enum import Enum, auto


class SidebarViewType(Enum):
    """Enumeration of available sidebar panel views."""
    SEARCH = auto()
    CONVERSATIONS = auto()
    VCS = auto()
    FILES = auto()
    PREVIEW = auto()
