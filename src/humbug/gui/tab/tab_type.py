from enum import Enum, auto


class TabType(Enum):
    """Enumeration of available tab types."""
    CONVERSATION = auto()
    EDITOR = auto()
    SYSTEM = auto()
    TERMINAL = auto()
    WIKI = auto()
