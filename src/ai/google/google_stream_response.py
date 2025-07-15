"""Handles streaming response from Google Gemini API."""

from typing import Dict

from ai.ai_stream_response import AIStreamResponse
from ai_tool import AIToolCall


class GoogleStreamResponse(AIStreamResponse):
    """Handles streaming response from Google Gemini API."""

    def __init__(self) -> None:
        """Initialize stream response handler."""
        super().__init__()

        # Track streaming tool calls
        self._current_tool_calls: Dict[str, Dict] = {}

    def _handle_candidates(self, chunk: Dict) -> None:
        # Extract text content and function calls
        candidate = chunk["candidates"][0]
        if "content" in candidate and "parts" in candidate["content"]:
            for part in candidate["content"]["parts"]:
                # Handle text content
                if "text" in part:
                    text = part["text"]
                    if text:
                        self.content += text

                # Handle function calls
                if "functionCall" in part:
                    function_call = part["functionCall"]
                    function_name = function_call.get("name", "")
                    function_args = function_call.get("args", {})

                    if not function_name:
                        return

                    # Use function name as key since Google doesn't provide call IDs in streaming
                    call_key = function_name

                    # Store the complete function call
                    self._current_tool_calls[call_key] = {
                        "name": function_name,
                        "arguments": function_args
                    }

        # Check for completion reason and process usage
        if candidate.get("finishReason") == "STOP" and "usageMetadata" in chunk:
            metadata = chunk["usageMetadata"]
            self._update_usage(
                prompt_tokens=metadata.get("promptTokenCount", 0),
                completion_tokens=metadata.get("candidatesTokenCount", 0),
                total_tokens=metadata.get("totalTokenCount", 0)
            )

            # Process all accumulated tool calls
            if self._current_tool_calls:
                for i, (call_key, call_data) in enumerate(self._current_tool_calls.items()):
                    # Generate a unique ID for the tool call
                    tool_call_id = f"call_{i}"

                    tool_call = AIToolCall(
                        id=tool_call_id,
                        name=call_data["name"],
                        arguments=call_data["arguments"]
                    )

                    # Add to our tool calls list
                    self._add_tool_call(tool_call)

    def update_from_chunk(self, chunk: Dict) -> None:
        """
        Update from a response chunk and return new content if any.

        Args:
            chunk: Response chunk from Google API
        """
        if "error" in chunk:
            error_data = chunk["error"]
            self._handle_error(error_data)
            return

        if "candidates" in chunk and chunk["candidates"]:
            self._handle_candidates(chunk)
