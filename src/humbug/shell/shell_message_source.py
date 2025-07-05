from enum import Enum

class ShellMessageSource(Enum):
    """Enumeration of possible shell message sources."""
    USER = "user"
    SUCCESS = "success"
    ERROR = "error"
