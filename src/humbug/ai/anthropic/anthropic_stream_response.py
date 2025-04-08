"""Handles streaming response from Anthropic API."""

from typing import Dict

from humbug.ai.ai_stream_response import AIStreamResponse


class AnthropicStreamResponse(AIStreamResponse):
    """Handles streaming response from Anthropic API."""

    def __init__(self) -> None:
        """Initialize stream response handler."""
        super().__init__()

        # Internal tracking of tokens
        self._input_tokens = 0
        self._output_tokens = 0

    def update_from_chunk(self, chunk: Dict) -> None:
        """
        Update from a response chunk and return new content if any.

        Args:
            chunk: Response chunk from Anthropic API
        """
        if "error" in chunk:
            error_data = chunk["error"]
            self._handle_error(
                error_data,
                error_code=error_data.get("type", "stream_error")
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
                self.reasoning += "\n\nSignature: " + delta.get("signature", "") + "\n"

        elif event_type == "content_block_start":
            content_block = chunk.get("content_block", {})
            if content_block.get("type") == "redacted_thinking":
                self.reasoning += "\n\nRedacted: " + content_block.get("data", "") + "\n"

        elif event_type == "message_delta":
            # Track output tokens but don't expose them yet
            if "usage" in chunk:
                usage = chunk["usage"]
                self._output_tokens = usage.get("output_tokens", 0)

        elif event_type == "message_stop":
            # Only now do we create and expose the usage stats
            self._update_usage(
                prompt_tokens=self._input_tokens,
                completion_tokens=self._output_tokens,
                total_tokens=self._input_tokens + self._output_tokens
            )
