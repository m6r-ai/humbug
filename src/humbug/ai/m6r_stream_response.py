"""Handles streaming response from M6R API."""

import logging
from typing import Optional

from humbug.ai.ai_usage import AIUsage
from humbug.ai.ai_response import AIError


class M6RStreamResponse:
    """Handles streaming response from M6R API."""

    def __init__(self):
        """Initialize stream response handler."""
        self.content = ""
        self.usage: Optional[AIUsage] = None
        self.error: Optional[AIError] = None
        self._logger = logging.getLogger("M6RStreamResponse")

        # Internal tracking of tokens
        self._input_tokens = 0
        self._output_tokens = 0
        self._done = False

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

        if event_type == "start":
            # Track input tokens
            if "usage" in chunk:
                usage = chunk["usage"]
                self._input_tokens = usage.get("input_tokens", 0)
        elif event_type == "content":
            # Add new content
            if "text" in chunk:
                text = chunk["text"]
                if text:
                    self.content += text

            # Track output tokens if provided
            if "usage" in chunk:
                usage = chunk["usage"]
                self._output_tokens = usage.get("output_tokens", 0)
        elif event_type == "done":
            self._done = True
            # Create final usage stats
            self.usage = AIUsage(
                prompt_tokens=self._input_tokens,
                completion_tokens=self._output_tokens,
                total_tokens=self._input_tokens + self._output_tokens
            )