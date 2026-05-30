from enum import Enum, auto


class ContextType(Enum):
    """Classification of open contexts within a mindspace."""
    CONVERSATION = auto()
    DIFF         = auto()
    EDITOR       = auto()
    LOG          = auto()
    PREVIEW      = auto()
    SHELL        = auto()
    TERMINAL     = auto()
