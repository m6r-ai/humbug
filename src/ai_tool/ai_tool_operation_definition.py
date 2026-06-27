"""AI tool operation definition."""

from collections.abc import Callable
from dataclasses import dataclass


@dataclass
class AIToolOperationDefinition:
    """Definition of a tool operation (sub-command)."""
    name: str
    handler: Callable
    extract_context: Callable | None
    allowed_parameters: set[str]
    required_parameters: set[str]
    description: str
