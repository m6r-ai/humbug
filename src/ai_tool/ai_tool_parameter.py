"""AI tool parameter definition."""

from dataclasses import dataclass
from typing import Dict, List


@dataclass
class AIToolParameter:
    """Definition of a tool parameter."""
    name: str
    type: str  # "string", "number", "boolean", "array", "object"
    description: str
    required: bool = True
    enum: List[str] | None = None
    properties: Dict[str, 'AIToolParameter'] | None = None  # For object types
