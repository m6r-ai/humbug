from enum import Enum

class ShellEventSource(Enum):
    """Enumeration of possible shell event sources."""
    USER = "user"
    SUCCESS = "success"
    ERROR = "error"
