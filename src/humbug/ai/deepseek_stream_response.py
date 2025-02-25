"""Handles streaming response from Deepseek API."""

import logging
from typing import Optional

from humbug.ai.ai_usage import AIUsage
from humbug.ai.ai_response import AIError


class DeepseekStreamResponse:
    """Handles streaming response from Deepseek API."""

    def __init__(self):
        """Initialize stream response handler."""
        self.content = ""
        self.usage: Optional[AIUsage] = None
        self.error: Optional[AIError] = None
        self._logger = logging.getLogger("DeepseekStreamResponse")
        self._reasoning_started = False
        self._content_started = False

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
        if "reasoning_content" in delta:
            new_content = delta["reasoning_content"]
            if new_content:
                if not self._reasoning_started:
                    self.content += "Reasoning:\n"
                    self._reasoning_started = True

                self.content += new_content

        if "content" in delta:
            new_content = delta["content"]
            if new_content:
                if not self._content_started:
                    if self._reasoning_started:
                        self.content += "\nResponse:\n"

                    self._content_started = True

                self.content += new_content
