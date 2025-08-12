"""Handles streaming response from Zai API."""

import json
from typing import Dict

from ai.ai_stream_response import AIStreamResponse
from ai_tool import AIToolCall


class ZaiStreamResponse(AIStreamResponse):
    """Handles streaming response from Zai API."""

    def __init__(self) -> None:
        """Initialize stream response handler."""
        super().__init__()

        # Track streaming tool calls
        self._current_tool_calls: Dict[str, Dict] = {}

    def _handle_choices(self, choices: Dict) -> None:
        """
        Handle choices from the Zai API response.

        Args:
            choices: Choices from Zai API response
        """
        if not choices:
            return

        delta = choices[0].get("delta", {})
        if "reasoning_content" in delta:
            new_reasoning = delta["reasoning_content"]
            if new_reasoning:
                self.reasoning += new_reasoning

        if "content" in delta:
            new_content = delta["content"]
            if new_content:
                self.content += new_content

        if "tool_calls" in delta:
            tool_calls = delta["tool_calls"]
            for tool_call_delta in tool_calls:
                tool_call_id = tool_call_delta.get("id", "")
                if not tool_call_id:
                    continue

                # Initialize tool call if we haven't seen this ID before
                if tool_call_id not in self._current_tool_calls:
                    self._current_tool_calls[tool_call_id] = {
                        "id": tool_call_id,
                        "name": "",
                        "arguments": ""
                    }

                current_call = self._current_tool_calls[tool_call_id]

                # Update function name if provided
                function = tool_call_delta.get("function", {})
                if "name" in function:
                    current_call["name"] = function["name"]

                # Accumulate arguments if provided
                if "arguments" in function:
                    current_call["arguments"] += function["arguments"]

    def _handle_usage(self, usage: Dict) -> None:
        """
        Handle usage data from the Zai API response.

        Args:
            usage: Usage data from Zai API response
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
            chunk: Response chunk from Zai API
        """
        if "error" in chunk:
            self._handle_error(chunk["error"])
            return

        if "choices" in chunk:
            self._handle_choices(chunk["choices"])

        if "usage" in chunk:
            self._handle_usage(chunk["usage"])
