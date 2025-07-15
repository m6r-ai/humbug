"""Enhanced Anthropic streaming response handler with tool support."""

import json
from typing import Dict

from ai.ai_stream_response import AIStreamResponse
from ai_tool import AIToolCall


class AnthropicStreamResponse(AIStreamResponse):
    """Handles streaming response from Anthropic API."""

    def __init__(self) -> None:
        """Initialize stream response handler."""
        super().__init__()

        # Internal tracking of tokens
        self._input_tokens = 0
        self._output_tokens = 0

        # Tool call tracking
        self._current_tool_call: Dict | None = None
        self._current_tool_arguments: str = ""

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
                self._input_tokens += usage.get("cache_creation_input_tokens", 0)
                self._input_tokens += usage.get("cache_read_input_tokens", 0)

        elif event_type == "content_block_start":
            content_block = chunk.get("content_block", {})
            block_type = content_block.get("type")

            if block_type == "tool_use":
                # Start of a tool call
                self._current_tool_call = {
                    "id": content_block.get("id", ""),
                    "name": content_block.get("name", "")
                }
                self._current_tool_arguments = ""

            elif block_type == "redacted_thinking":
                self.readacted_reasoning = content_block.get("data", "")

        elif event_type == "content_block_delta":
            delta = chunk.get("delta", {})
            delta_type = delta.get("type")

            if delta_type == "text_delta":
                # Regular text content
                self.content += delta.get("text", "")

            elif delta_type == "thinking_delta":
                # Reasoning content
                self.reasoning += delta.get("thinking", "")

            elif delta_type == "signature_delta":
                # Signature content (part of reasoning)
                self.signature = delta.get("signature", "")

            elif delta_type == "input_json_delta":
                # Tool arguments being streamed
                if self._current_tool_call is not None:
                    partial_json = delta.get("partial_json", "")
                    self._current_tool_arguments += partial_json

        elif event_type == "content_block_stop":
            # End of a content block
            if self._current_tool_call is not None:
                # Complete the tool call
                json_args = {}
                try:
                    if self._current_tool_arguments:
                        json_args = json.loads(self._current_tool_arguments)

                except json.JSONDecodeError as e:
                    self._logger.warning("Failed to parse tool arguments: %s (%s)", self._current_tool_arguments, str(e))

                tool_call = AIToolCall(
                    id=self._current_tool_call["id"],
                    name=self._current_tool_call["name"],
                    arguments=json_args
                )

                # Add to our tool calls list
                self._add_tool_call(tool_call)

                # Reset tool call tracking
                self._current_tool_call = None
                self._current_tool_arguments = ""

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
