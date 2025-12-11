"""AI tool operation definition."""

from dataclasses import dataclass
from typing import Callable, Set


@dataclass
class AIToolOperationDefinition:
    """Definition of a tool operation (sub-command)."""
    name: str
    handler: Callable
    extract_context: Callable | None
    allowed_parameters: Set[str]
    required_parameters: Set[str]
    description: str
