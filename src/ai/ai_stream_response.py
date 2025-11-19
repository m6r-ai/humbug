"""Base class for handling streaming responses."""

import logging
from abc import ABC, abstractmethod
from typing import Dict, List

from ai.ai_response import AIError
from ai.ai_usage import AIUsage
from ai_tool import AIToolCall


class AIStreamResponse(ABC):
    """
    Base class for handling streaming responses from AI APIs.

    This class defines the common interface and functionality for all AI stream response handlers.
    Specific implementations should inherit from this class and override the update_from_chunk method.
    """

    def __init__(self) -> None:
        """Initialize stream response handler with default values."""
        self.reasoning = ""
        self.content = ""
        self.usage: AIUsage | None = None
        self.error: AIError | None = None
        self.tool_calls: List[AIToolCall] = []
        self.signature = ""
        self.redacted_reasoning = ""
        self._logger = logging.getLogger(self.__class__.__name__)

    @abstractmethod
    def update_from_chunk(self, chunk: Dict) -> None:
        """
        Update internal state from a response chunk.

        This method must be implemented by subclasses to handle the specific format
        of chunks from each AI provider's API.

        Args:
            chunk: Response chunk from AI API

        Raises:
            NotImplementedError: If not implemented by subclass
        """

    def _handle_error(self, error_data: Dict, error_code: str = "stream_error") -> None:
        """
        Handle error data from API response.

        Args:
            error_data: Error information from the API
            error_code: Error code to use if not provided in error_data
        """
        self._logger.debug("Got error message: %s", error_data)

        # Extract message from error data, defaulting to "Unknown error"
        error_message = "Unknown error"
        if isinstance(error_data, dict):
            if "message" in error_data:
                error_message = error_data["message"]

            elif isinstance(error_data.get("error"), dict) and "message" in error_data["error"]:
                error_message = error_data["error"]["message"]

        self.error = AIError(
            code=error_code,
            message=error_message,
            retries_exhausted=True,
            details=error_data
        )

    def _update_usage(self, prompt_tokens: int, completion_tokens: int, total_tokens: int) -> None:
        """
        Update usage statistics.

        Args:
            prompt_tokens: Number of tokens in the prompt
            completion_tokens: Number of tokens in the completion
            total_tokens: Total number of tokens used
        """
        self.usage = AIUsage(
            prompt_tokens=prompt_tokens,
            completion_tokens=completion_tokens,
            total_tokens=total_tokens
        )

    def _add_tool_call(self, tool_call: AIToolCall) -> None:
        """
        Add a tool call to the response.

        Args:
            tool_call: The tool call to add
        """
        self.tool_calls.append(tool_call)
        self._logger.debug("Added tool call: %s", tool_call.name)
