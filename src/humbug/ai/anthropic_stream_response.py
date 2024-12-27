"""Handles streaming response from Anthropic API."""

import logging
from typing import Optional

from humbug.ai.ai_usage import AIUsage


class AnthropicStreamResponse:
    """Handles streaming response from Anthropic API."""

    def __init__(self):
        """Initialize stream response handler."""
        self.content = ""
        self.usage: Optional[AIUsage] = None
        self.error = None
        self._logger = logging.getLogger("AnthropicStreamResponse")

    def update_from_chunk(self, chunk: dict) -> None:
        """Update from a response chunk and return new content if any."""
        if "error" in chunk:
            self._logger.debug("Got error message: %s", chunk["error"])
            self.error = {
                "code": chunk["error"].get("type", "stream_error"),
                "message": chunk["error"].get("message", "Unknown error"),
                "details": chunk["error"]
            }
            return

        event_type = chunk.get("type")

        if event_type == "message_start":
            # Capture initial usage stats
            if "message" in chunk and "usage" in chunk["message"]:
                usage = chunk["message"]["usage"]
                self.usage = AIUsage(
                    prompt_tokens=usage.get("input_tokens", 0),
                    completion_tokens=usage.get("output_tokens", 0),
                    total_tokens=usage.get("input_tokens", 0) + usage.get("output_tokens", 0)
                )
        elif event_type == "content_block_delta":
            delta = chunk.get("delta", {})
            if delta.get("type") == "text_delta":
                self.content += delta.get("text", "")
        elif event_type == "message_delta":
            # Update usage stats if provided
            if "usage" in chunk:
                usage = chunk["usage"]
                self.usage = AIUsage(
                    prompt_tokens=self.usage.prompt_tokens if self.usage else 0,
                    completion_tokens=usage.get("output_tokens", 0),
                    total_tokens=(self.usage.prompt_tokens if self.usage else 0) + usage.get("output_tokens", 0)
                )
