"""Handles streaming response from M6R API."""

from typing import Dict

from humbug.ai.ai_stream_response import AIStreamResponse


class M6RStreamResponse(AIStreamResponse):
    """Handles streaming response from M6R API."""

    def __init__(self) -> None:
        """Initialize stream response handler."""
        super().__init__()

        # Internal tracking of tokens
        self._input_tokens = 0
        self._output_tokens = 0
        self._done = False

    def update_from_chunk(self, chunk: Dict) -> None:
        """
        Update from a response chunk and return new content if any.

        Args:
            chunk: Response chunk from M6R API
        """
        if "error" in chunk:
            error_data = chunk["error"]
            self._handle_error(
                error_data,
                error_code=error_data.get("type", "stream_error")
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
            self._update_usage(
                prompt_tokens=self._input_tokens,
                completion_tokens=self._output_tokens,
                total_tokens=self._input_tokens + self._output_tokens
            )
