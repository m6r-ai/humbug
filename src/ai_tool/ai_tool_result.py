"""AI tool result representation."""

import asyncio
from dataclasses import dataclass
from typing import Any, Dict


@dataclass
class AIToolResult:
    """Result of a tool execution."""
    id: str
    name: str
    content: str
    error: str | None = None
    context: str | None = None
    continuation: asyncio.Task | None = None

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
