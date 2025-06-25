"""Handles streaming response from Mistral API."""

import json
from typing import Dict

from humbug.ai.ai_stream_response import AIStreamResponse
from humbug.ai.ai_tool_manager import AIToolCall


class MistralStreamResponse(AIStreamResponse):
    """Handles streaming response from Mistral API."""

    def __init__(self) -> None:
        """Initialize stream response handler."""
        super().__init__()

        # Track streaming tool calls
        self._current_tool_calls: Dict[str, Dict] = {}

    def _handle_choices(self, choices: Dict) -> None:
        """
        Handle choices from the Mistral API response.
        Args:
            choices: Choices from Mistral API response
        """
        if not choices:
            return

        delta = choices[0].get("delta", {})
        if "content" in delta:
            new_content = delta["content"]
            if new_content:
                self.content += new_content

        if "tool_calls" in delta:
            tool_calls = delta["tool_calls"]
            for tool_call_delta in tool_calls:
                tool_call_index = tool_call_delta.get("index", -1)
                if tool_call_index == -1:
                    continue

                tool_call_id = tool_call_delta.get("id", "")

                # Initialize tool call if we haven't seen it before
                if tool_call_index not in self._current_tool_calls:
                    self._current_tool_calls[tool_call_index] = {
                        "index": tool_call_index,
                        "id": tool_call_id,
                        "name": "",
                        "arguments": ""
                    }

                current_call = self._current_tool_calls[tool_call_index]

                # Update function name if provided
                function = tool_call_delta.get("function", {})
                if "name" in function:
                    current_call["name"] = function["name"]

                # Accumulate arguments if provided
                if "arguments" in function:
                    current_call["arguments"] += function["arguments"]

    def _handle_usage(self, usage: Dict) -> None:
        """
        Handle usage data from the Mistral API response.

        Args:
            usage: Usage data from Mistral API response
        """
        if not usage:
            return

        self._update_usage(
            prompt_tokens=usage.get("prompt_tokens", 0),
            completion_tokens=usage.get("completion_tokens", 0),
            total_tokens=usage.get("total_tokens", 0)
        )

        # Process all accumulated tool calls
        for call_data in self._current_tool_calls.values():
            # Create the tool call
            json_args = {}
            try:
                if call_data["arguments"]:
                    json_args = json.loads(call_data["arguments"])

            except json.JSONDecodeError as e:
                self._logger.warning("Failed to parse tool arguments: %s (%s)", call_data["arguments"], str(e))

            tool_call = AIToolCall(
                id=call_data["id"],
                name=call_data["name"],
                arguments=json_args
            )

            # Add to our tool calls list
            self._add_tool_call(tool_call)

    def update_from_chunk(self, chunk: Dict) -> None:
        """
        Update from a response chunk and return new content if any.

        Args:
            chunk: Response chunk from Mistral API
        """
        if "error" in chunk:
            self._handle_error(chunk["error"])
            return

        if "choices" in chunk:
            self._handle_choices(chunk["choices"])

        if "usage" in chunk:
            self._handle_usage(chunk["usage"])
