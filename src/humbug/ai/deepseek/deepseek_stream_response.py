"""Handles streaming response from Deepseek API."""

import json
from typing import Dict

from humbug.ai.ai_stream_response import AIStreamResponse
from humbug.ai.ai_tool_manager import ToolCall


class DeepseekStreamResponse(AIStreamResponse):
    """Handles streaming response from Deepseek API."""

    def __init__(self) -> None:
        """Initialize stream response handler."""
        super().__init__()

        # Track streaming tool calls
        self._current_tool_calls: Dict[str, Dict] = {}

    def update_from_chunk(self, chunk: Dict) -> None:
        """
        Update from a response chunk and return new content if any.

        Args:
            chunk: Response chunk from Deepseek API
        """
        print("-------------------------------------------------------")
        print("Received chunk:", chunk)
        if "error" in chunk:
            self._handle_error(chunk["error"])
            return

        if "usage" in chunk:
            usage = chunk["usage"]
            if usage:
                self._update_usage(
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

                # Initialize tool call if we haven't seen it before
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

        # Check if we have usage (indicating completion) and pending tool calls
        if "usage" in chunk and self._current_tool_calls:
            # Process all accumulated tool calls
            for call_data in self._current_tool_calls.values():
                try:
                    # Parse the accumulated arguments
                    arguments = json.loads(call_data["arguments"]) if call_data["arguments"] else {}

                    # Create the tool call
                    tool_call = ToolCall(
                        id=call_data["id"],
                        name=call_data["name"],
                        arguments=arguments
                    )

                    # Add to our tool calls list
                    self._add_tool_call(tool_call)
                    print("Tool call added:", tool_call)

                except json.JSONDecodeError as e:
                    self._logger.warning("Failed to parse tool arguments for %s: %s", call_data["id"], e)

            # Clear the accumulated tool calls
            self._current_tool_calls.clear()
