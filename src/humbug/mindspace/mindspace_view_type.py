from enum import Enum, auto


class MindspaceViewType(Enum):
    """Enumeration of available mindspace views."""
    CONVERSATIONS = auto()
    VCS = auto()
    FILES = auto()
    PREVIEW = auto()
