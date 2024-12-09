"""Handles streaming response from OpenAI API."""

import logging
from typing import Optional

from humbug.ai.base import AIUsage


class OpenAIStreamResponse:
    """Handles streaming response from OpenAI API."""

    def __init__(self):
        """Initialize stream response handler."""
        self.content = ""
        self.usage: Optional[AIUsage] = None
        self.error = None
        self.logger = logging.getLogger("OpenAIStreamResponse")

    def update_from_chunk(self, chunk: dict) -> None:
        """Update from a response chunk and return new content if any."""
        if "error" in chunk:
            self.logger.debug("Got error message: %s", chunk["error"])
            self.error = {
                "code": "stream_error",
                "message": chunk["error"].get("message", "Unknown error"),
                "details": chunk["error"]
            }
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
            self.content += new_content
