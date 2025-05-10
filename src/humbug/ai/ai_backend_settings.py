"""Base class for AI backends."""

from dataclasses import dataclass


@dataclass
class AIBackendSettings:
    """Settings for a specific AI backend."""
    enabled: bool = False
    api_key: str = ""
    url: str = ""
