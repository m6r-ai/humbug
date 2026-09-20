"""Base class for AI backends."""

from dataclasses import dataclass, asdict


@dataclass
class AIUsage:
    """
    Token usage information from AI responses.

    prompt_tokens is the total input token count and always includes any tokens
    served from or written to the provider cache.  cache_write_tokens and
    cache_read_tokens are subsets of prompt_tokens, not additional to it.
    """
    prompt_tokens: int
    completion_tokens: int
    total_tokens: int
    cache_write_tokens: int = 0
    cache_read_tokens: int = 0

    def to_dict(self) -> dict:
        """Convert usage to dictionary."""
        return asdict(self)

    def copy(self) -> 'AIUsage':
        """Create a deep copy of the usage."""
        return AIUsage(
            prompt_tokens=self.prompt_tokens,
            completion_tokens=self.completion_tokens,
            total_tokens=self.total_tokens,
            cache_write_tokens=self.cache_write_tokens,
            cache_read_tokens=self.cache_read_tokens,
        )
