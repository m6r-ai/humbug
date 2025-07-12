from enum import Enum, auto


class TabType(Enum):
    """Enumeration of available tab types."""
    CONVERSATION = auto()
    EDITOR = auto()
    LOG = auto()
    SHELL = auto()
    TERMINAL = auto()
    WIKI = auto()
