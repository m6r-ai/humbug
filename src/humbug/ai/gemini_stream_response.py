"""Handles streaming response from Google Gemini API."""

import logging
from typing import Optional

from humbug.ai.ai_usage import AIUsage


class GeminiStreamResponse:
    """Handles streaming response from Gemini API."""

    def __init__(self):
        """Initialize stream response handler."""
        self.content = ""
        self.usage: Optional[AIUsage] = None
        self.error = None
        self._logger = logging.getLogger("GeminiStreamResponse")

    def update_from_chunk(self, chunk: dict) -> None:
        """Update from a response chunk and return new content if any.

        Args:
            chunk: Response chunk from Gemini API
        """
        if "error" in chunk:
            self._logger.debug("Got error message: %s", chunk["error"])
            self.error = {
                "code": "stream_error",
                "message": chunk["error"].get("message", "Unknown error"),
                "details": chunk["error"]
            }
            return

        if "candidates" in chunk and chunk["candidates"]:
            candidate = chunk["candidates"][0]

            # Extract text content
            if "content" in candidate and "parts" in candidate["content"]:
                for part in candidate["content"]["parts"]:
                    if "text" in part:
                        self.content += part["text"]

            # Check for completion reason
            if candidate.get("finishReason") == "STOP" and "usageMetadata" in chunk:
                metadata = chunk["usageMetadata"]
                self.usage = AIUsage(
                    prompt_tokens=metadata.get("promptTokenCount", 0),
                    completion_tokens=metadata.get("candidatesTokenCount", 0),
                    total_tokens=metadata.get("totalTokenCount", 0)
                )
