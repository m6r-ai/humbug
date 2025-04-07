"""Handles streaming response from Google Gemini API."""

import logging

from humbug.ai.ai_usage import AIUsage
from humbug.ai.ai_response import AIError


class GoogleStreamResponse:
    """Handles streaming response from Google Gemini API."""

    def __init__(self) -> None:
        """Initialize stream response handler."""
        self.reasoning = ""
        self.content = ""
        self.usage: AIUsage | None = None
        self.error: AIError | None = None
        self._logger = logging.getLogger("GoogleStreamResponse")

    def update_from_chunk(self, chunk: dict) -> None:
        """
        Update from a response chunk and return new content if any.

        Args:
            chunk: Response chunk from Google API
        """
        if "error" in chunk:
            self._logger.debug("Got error message: %s", chunk["error"])
            error_data = chunk["error"]
            self.error = AIError(
                code="stream_error",
                message=error_data.get("message", "Unknown error"),
                retries_exhausted=True,
                details=error_data
            )
            return

        if "candidates" in chunk and chunk["candidates"]:
            candidate = chunk["candidates"][0]

            # Extract text content
            if "content" in candidate and "parts" in candidate["content"]:
                for part in candidate["content"]["parts"]:
                    if "text" in part:
                        text = part["text"]
                        if text:
                            self.content += text

            # Check for completion reason
            if candidate.get("finishReason") == "STOP" and "usageMetadata" in chunk:
                metadata = chunk["usageMetadata"]
                self.usage = AIUsage(
                    prompt_tokens=metadata.get("promptTokenCount", 0),
                    completion_tokens=metadata.get("candidatesTokenCount", 0),
                    total_tokens=metadata.get("totalTokenCount", 0)
                )
