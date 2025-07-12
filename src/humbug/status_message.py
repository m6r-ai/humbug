"""Status message container for application-wide status bar."""

from dataclasses import dataclass


@dataclass
class StatusMessage:
    """Container for status bar message information."""
    text: str
    timeout: int | None = None  # Milliseconds before reverting to previous message
