"""Handles streaming response from the OpenAI Responses API."""

import json

from ai.ai_stream_response import AIStreamResponse
from ai_tool import AIToolCall


class OpenAIStreamResponse(AIStreamResponse):
    """Handles streaming response from the OpenAI Responses API."""

    def __init__(self) -> None:
        """Initialize stream response handler."""
        super().__init__()

        # Call ids of function calls already emitted, so each tool call is recorded once
        self._emitted_call_ids: set[str] = set()

        # Encrypted reasoning content returned for stateless reasoning replay
        self.encrypted_reasoning = ""

    def _handle_output_item(self, item: dict) -> None:
        """
        Handle a completed output item.

        Args:
            item: A single output item from the Responses API
        """
        item_type = item.get("type")

        if item_type == "function_call":
            call_id = item.get("call_id", "")
            if call_id in self._emitted_call_ids:
                return

            arguments = item.get("arguments", "")

            json_args = {}
            try:
                if arguments:
                    json_args = json.loads(arguments)

            except json.JSONDecodeError as e:
                self._logger.warning("Failed to parse tool arguments: %s (%s)", arguments, str(e))

            self._emitted_call_ids.add(call_id)
            self._add_tool_call(
                AIToolCall(
                    id=call_id,
                    name=item.get("name", ""),
                    arguments=json_args
                )
            )
            return

        if item_type == "reasoning":
            encrypted = item.get("encrypted_content", "")
            if encrypted:
                self.encrypted_reasoning = encrypted

    def _handle_usage(self, usage: dict) -> None:
        """
        Handle usage data from the Responses API response.

        Args:
            usage: Usage data from the Responses API response
        """
        if not usage:
            return

        input_tokens_details = usage.get("input_tokens_details", {}) or {}
        self._update_usage(
            prompt_tokens=usage.get("input_tokens", 0),
            completion_tokens=usage.get("output_tokens", 0),
            total_tokens=usage.get("total_tokens", 0),
            cache_read_tokens=input_tokens_details.get("cached_tokens", 0),
        )

    def update_from_chunk(self, chunk: dict) -> None:
        """
        Update internal state from a response event.

        Args:
            chunk: A server-sent event from the Responses API
        """
        event_type = chunk.get("type", "")

        if event_type == "error":
            self._handle_error(chunk)
            return

        if event_type == "response.output_text.delta":
            self.content += chunk.get("delta", "")
            return

        if event_type == "response.output_item.done":
            self._handle_output_item(chunk.get("item", {}))
            return

        if event_type == "response.completed":
            response = chunk.get("response", {})
            self._handle_usage(response.get("usage", {}))
            return
