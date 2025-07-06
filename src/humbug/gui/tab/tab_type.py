from enum import Enum, auto


class TabType(Enum):
    """Enumeration of available tab types."""
    CONVERSATION = auto()
    EDITOR = auto()
    SHELL = auto()
    TERMINAL = auto()
    WIKI = auto()
