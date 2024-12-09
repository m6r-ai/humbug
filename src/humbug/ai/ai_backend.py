"""Base class for AI backends."""

from abc import ABC, abstractmethod
from typing import List, AsyncGenerator

from humbug.ai.ai_response import AIResponse


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
