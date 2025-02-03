"""Handles streaming response from Ollama API."""

import logging
from typing import Optional

from humbug.ai.ai_usage import AIUsage
from humbug.ai.ai_response import AIError


class OllamaStreamResponse:
    """Handles streaming response from Ollama API."""

    def __init__(self):
        """Initialize stream response handler."""
        self.content = ""
        self.usage: Optional[AIUsage] = None
        self.error = None
        self._logger = logging.getLogger("OllamaStreamResponse")

    def update_from_chunk(self, chunk: dict) -> None:
        """Update from a response chunk and return new content if any."""
        if "error" in chunk:
            self._logger.debug("Got error message: %s", chunk["error"])
            self.error = AIError(
                code="stream_error",
                message=chunk["error"].get("message", "Unknown error"),
                retries_exhausted=True,
                details=chunk["error"]
            )
            return

        if chunk.get("done"):
            self.usage = AIUsage(
                prompt_tokens=chunk.get("prompt_eval_count", 0),
                completion_tokens=chunk.get("eval_count", 0),
                total_tokens=chunk.get("prompt_eval_count", 0) + chunk.get("eval_count", 0)
            )

        if "message" in chunk and "content" in chunk["message"]:
            self.content += chunk["message"]["content"]
