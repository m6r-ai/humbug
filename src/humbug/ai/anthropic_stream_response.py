"""Handles streaming response from Anthropic API."""

import logging
from typing import Optional

from humbug.ai.ai_usage import AIUsage
from humbug.ai.ai_response import AIError


class AnthropicStreamResponse:
    """Handles streaming response from Anthropic API."""

    def __init__(self):
        """Initialize stream response handler."""
        self.reasoning = ""
        self.content = ""
        self.usage: Optional[AIUsage] = None
        self.error: Optional[AIError] = None
        self._logger = logging.getLogger("AnthropicStreamResponse")

        # Internal tracking of tokens
        self._input_tokens = 0
        self._output_tokens = 0

    def update_from_chunk(self, chunk: dict) -> None:
        """Update from a response chunk and return new content if any."""
        if "error" in chunk:
            self._logger.debug("Got error message: %s", chunk["error"])
            error_data = chunk["error"]
            self.error = AIError(
                code=error_data.get("type", "stream_error"),
                message=error_data.get("message", "Unknown error"),
                retries_exhausted=True,
                details=error_data
            )
            return

        event_type = chunk.get("type")

        if event_type == "message_start":
            # Track input tokens but don't expose them yet
            if "message" in chunk and "usage" in chunk["message"]:
                usage = chunk["message"]["usage"]
                self._input_tokens = usage.get("input_tokens", 0)
        elif event_type == "content_block_delta":
            delta = chunk.get("delta", {})
            if delta.get("type") == "text_delta":
                self.content += delta.get("text", "")
            elif delta.get("type") == "thinking_delta":
                self.reasoning += delta.get("thinking", "")
            elif delta.get("type") == "signature_delta":
                self.reasoning += delta.get("signature", "")
        elif event_type == "message_delta":
            # Track output tokens but don't expose them yet
            if "usage" in chunk:
                usage = chunk["usage"]
                self._output_tokens = usage.get("output_tokens", 0)
        elif event_type == "message_stop":
            # Only now do we create and expose the usage stats
            self.usage = AIUsage(
                prompt_tokens=self._input_tokens,
                completion_tokens=self._output_tokens,
                total_tokens=self._input_tokens + self._output_tokens
            )
