"""Base classes for AI backends."""

from abc import ABC, abstractmethod
from dataclasses import dataclass, asdict
from typing import Dict, Optional, List, AsyncGenerator


@dataclass
class AIUsage:
    """Token usage information from AI responses."""
    prompt_tokens: int
    completion_tokens: int
    total_tokens: int

    def to_dict(self) -> Dict:
        """Convert usage to dictionary."""
        return asdict(self)


@dataclass
class AIResponse:
    """Response from an AI backend."""
    content: str
    usage: Optional[AIUsage] = None
    error: Optional[Dict] = None
    model: Optional[str] = None
    temperature: Optional[float] = None


class AIBackend(ABC):
    """Abstract base class for AI backends."""

    @abstractmethod
    async def stream_message(
        self,
        message: str,
        conversation_history: List[str]
    ) -> AsyncGenerator[AIResponse, None]:
        """Send a message to the AI backend and stream the response.

        Args:
            message: The message to send
            conversation_history: List of previous messages in the conversation

        Yields:
            AIResponse containing chunks of the AI's response and usage information
        """
