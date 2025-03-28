"""Handles streaming response from OpenAI API."""

import logging
from typing import Optional

from humbug.ai.ai_usage import AIUsage
from humbug.ai.ai_response import AIError


class OpenAIStreamResponse:
    """Handles streaming response from OpenAI API."""

    def __init__(self):
        """Initialize stream response handler."""
        self.reasoning = ""
        self.content = ""
        self.usage: Optional[AIUsage] = None
        self.error: Optional[AIError] = None
        self._logger = logging.getLogger("OpenAIStreamResponse")

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

        if "usage" in chunk:
            usage = chunk["usage"]
            if usage:
                self.usage = AIUsage(
                    prompt_tokens=usage.get("prompt_tokens", 0),
                    completion_tokens=usage.get("completion_tokens", 0),
                    total_tokens=usage.get("total_tokens", 0)
                )

        if "choices" not in chunk:
            return

        choices = chunk["choices"]
        if not choices:
            return

        delta = choices[0].get("delta", {})
        if "content" in delta:
            new_content = delta["content"]
            if new_content:
                self.content += new_content
