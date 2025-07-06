from enum import Enum

class MindspaceMessageSource(Enum):
    """Enumeration of possible mindspace message sources."""
    USER = "user"
    SUCCESS = "success"
    ERROR = "error"
