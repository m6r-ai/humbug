"""AI tool parameter definition."""

from dataclasses import dataclass


@dataclass
class AIToolParameter:
    """Definition of a tool parameter."""
    name: str
    type: str  # "string", "number", "boolean", "array", "object"
    description: str
    required: bool = True
    enum: list[str] | None = None
    properties: dict[str, 'AIToolParameter'] | None = None  # For object types
