"""Conversation state management for the Humbug application."""

from dataclasses import dataclass
from typing import Dict


@dataclass
class Usage:
    """Token usage information."""
    prompt_tokens: int
    completion_tokens: int
    total_tokens: int

    def to_dict(self) -> Dict:
        """Convert usage to dictionary for transcript."""
        return {
            "prompt_tokens": self.prompt_tokens,
            "completion_tokens": self.completion_tokens,
            "total_tokens": self.total_tokens
        }
