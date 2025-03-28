from enum import Enum, auto


class TabType(Enum):
    """Enumeration of available tab types."""
    CONVERSATION = auto()
    EDITOR = auto()
    TERMINAL = auto()
