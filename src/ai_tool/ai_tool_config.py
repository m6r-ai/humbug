"""AI tool configuration."""

from dataclasses import dataclass


@dataclass
class AIToolConfig:
    """Configuration for an AI tool."""
    name: str
    display_name: str
    description: str
    enabled_by_default: bool = True
