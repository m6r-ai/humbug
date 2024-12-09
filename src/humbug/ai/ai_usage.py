"""Base class for AI backends."""

from dataclasses import dataclass, asdict
from typing import Dict


@dataclass
class AIUsage:
    """Token usage information from AI responses."""
    prompt_tokens: int
    completion_tokens: int
    total_tokens: int

    def to_dict(self) -> Dict:
        """Convert usage to dictionary."""
        return asdict(self)
