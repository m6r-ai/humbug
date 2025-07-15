"""Handles streaming response from Ollama API."""

from typing import Dict

from ai.ai_stream_response import AIStreamResponse
from ai_tool import AIToolCall


class OllamaStreamResponse(AIStreamResponse):
    """Handles streaming response from Ollama API."""

    def __init__(self) -> None:
        """Initialize stream response handler."""
        super().__init__()

        # Track streaming tool calls
        self._current_tool_calls: Dict[str, Dict] = {}

    def _handle_message(self, message: Dict) -> None:
        """
        Handle messages from the Ollama API response.

        Args:
            message: Message from Ollama API response
        """
        # Handle regular content
        if "thinking" in message:
            new_reasoning = message["thinking"]
            if new_reasoning:
                self.reasoning += new_reasoning

        if "content" in message:
            content = message["content"]
            if content:
                self.content += content

        if "tool_calls" in message:
            tool_calls = message["tool_calls"]
            for i, tool_call in enumerate(tool_calls):
                tool_call_index = str(i)

                # Initialize tool call if we haven't seen it before
                if tool_call_index not in self._current_tool_calls:
                    self._current_tool_calls[tool_call_index] = {
                        "index": tool_call_index,
                        "id": f"call_{tool_call_index}",  # Generate ID since Ollama might not provide one
                        "name": "",
                        "arguments": ""
                    }

                current_call = self._current_tool_calls[tool_call_index]

                # Update function name and arguments
                function = tool_call.get("function", {})
                if "name" in function:
                    current_call["name"] = function["name"]

                if "arguments" in function:
                    current_call["arguments"] = function["arguments"]

    def _handle_done(self, chunk: Dict) -> None:
        """
        Handle "done" from the Ollama API response.

        Args:
            chunk: Message chunk from Ollama API response
        """
        done = chunk["done"]
        if not done:
            return

        self._update_usage(
            prompt_tokens=chunk.get("prompt_eval_count", 0),
            completion_tokens=chunk.get("eval_count", 0),
            total_tokens=chunk.get("prompt_eval_count", 0) + chunk.get("eval_count", 0)
        )

        # Process all accumulated tool calls when done
        for call_data in self._current_tool_calls.values():
            # Create the tool call
            tool_call = AIToolCall(
                id=call_data["id"],
                name=call_data["name"],
                arguments=call_data["arguments"]
            )

            # Add to our tool calls list
            self._add_tool_call(tool_call)

    def update_from_chunk(self, chunk: Dict) -> None:
        """
        Update from a response chunk and return new content if any.

        Args:
            chunk: Response chunk from Ollama API
        """
        if "error" in chunk:
            self._handle_error(chunk["error"])
            return

        if "message" in chunk:
            # Handle tool calls
            self._handle_message(chunk["message"])

        if "done" in chunk:
            # Handle completion of the response
            self._handle_done(chunk)
