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

    def copy(self) -> 'AIUsage':
        """Create a deep copy of the usage."""
        return AIUsage(
            prompt_tokens=self.prompt_tokens,
            completion_tokens=self.completion_tokens,
            total_tokens=self.total_tokens
        )
