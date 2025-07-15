"""AI tool result representation."""

from dataclasses import dataclass
from typing import Any, Dict


@dataclass
class AIToolResult:
    """Result of a tool execution."""
    id: str
    name: str
    content: str
    error: str | None = None

    def to_dict(self) -> Dict[str, Any]:
        """
        Convert the tool result to a dictionary.

        Returns:
            Dictionary representation of the tool result
        """
        return {
            'id': self.id,
            'name': self.name,
            'content': self.content,
            'error': self.error
        }
