"""AI tool call representation."""

from dataclasses import dataclass
from typing import Any, Dict


@dataclass
class AIToolCall:
    """Represents a tool call request from the AI."""
    id: str                     # Unique identifier for this tool call
    name: str                   # Name of the tool being called
    arguments: Dict[str, Any]   # Arguments for the tool call, as a dictionary

    def to_dict(self) -> Dict[str, Any]:
        """
        Convert the tool call to a dictionary.

        Returns:
            Dictionary representation of the tool call
        """
        return {
            'id': self.id,
            'name': self.name,
            'arguments': self.arguments
        }
