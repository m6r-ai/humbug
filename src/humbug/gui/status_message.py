"""Status message container for application-wide status bar."""

from dataclasses import dataclass
from typing import Optional


@dataclass
class StatusMessage:
    """Container for status bar message information."""
    text: str
    timeout: Optional[int] = None  # Milliseconds before reverting to previous message
