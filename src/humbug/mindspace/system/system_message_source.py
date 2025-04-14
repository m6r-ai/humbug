from enum import Enum

class SystemMessageSource(Enum):
    """Enumeration of possible system message sources."""
    USER = "user"
    SUCCESS = "success"
    ERROR = "error"
