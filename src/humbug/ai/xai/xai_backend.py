"""xAI backend implementation."""
from typing import Dict, List, Any

from humbug.ai.ai_backend import AIBackend
from humbug.ai.ai_conversation_settings import AIConversationSettings
from humbug.ai.xai.xai_stream_response import XAIStreamResponse


class XAIBackend(AIBackend):
    """xAI API backend implementation with streaming support."""

    @classmethod
    def get_default_url(cls) -> str:
        """
        Get the default API URL.

        Returns:
            The default URL
        """
        return "https://api.x.ai/v1/chat/completions"

    def _build_request_data(self, conversation_history: List[Dict[str, str]], settings: AIConversationSettings) -> dict:
        """Build xAI-specific request data."""
        # conversation_history already contains properly formatted messages
        messages = conversation_history.copy()

        data = {
            "model": AIConversationSettings.get_name(settings.model),
            "messages": messages,
            "stream": True,
            "stream_options": {"include_usage": True}
        }

        # Only include temperature if supported by model
        if AIConversationSettings.supports_temperature(settings.model):
            data["temperature"] = settings.temperature

        self._logger.debug("stream message %r", data)
        return data

    def _create_stream_response_handler(self) -> XAIStreamResponse:
        """Create an xAI-specific stream response handler."""
        return XAIStreamResponse()

    def _get_api_url(self, settings: AIConversationSettings) -> str:
        """Get the xAI API URL."""
        return self._api_url

    def _get_headers(self) -> dict:
        """Get the xAI API headers."""
        return {
            "Content-Type": "application/json",
            "Authorization": f"Bearer {self._api_key}"
        }

    def _format_messages_for_context(self, conversation_history: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """Format conversation history for xAI's API format (OpenAI-compatible)."""
        result = []

        for message in conversation_history:
            role = message["role"]
            content = message["content"]

            msg_dict = {
                "role": role,
                "content": content
            }

            # Handle assistant messages with tool calls
            if role == "assistant" and "tool_calls" in message:
                # For xAI, tool calls are added as a separate field (OpenAI-compatible)
                msg_dict["tool_calls"] = [
                    {
                        "id": tool_call["id"],
                        "type": "function",
                        "function": {
                            "name": tool_call["name"],
                            "arguments": tool_call["arguments"]
                        }
                    }
                    for tool_call in message["tool_calls"]
                ]

            # Handle user messages with tool results
            elif role == "user" and "tool_results" in message:
                # For xAI, tool results come as separate tool messages (OpenAI-compatible)
                if content:
                    result.append(msg_dict)

                for tool_result in message["tool_results"]:
                    result.append({
                        "role": "tool",
                        "tool_call_id": tool_result["tool_call_id"],
                        "content": tool_result["content"]
                    })
                continue

            result.append(msg_dict)

        return result

    def _add_tools_to_request_data(self, data: dict, settings: AIConversationSettings) -> None:
        """Add tool definitions to request data in xAI format (OpenAI-compatible)."""
        if not self._supports_tools(settings) or not self._tool_manager.has_tools():
            return

        tool_definitions = self._tool_manager.get_tool_definitions_for_provider("xai")

        if tool_definitions:
            data["tools"] = tool_definitions
            data["tool_choice"] = "auto"
            self._logger.debug("Added %d tool definitions for xai", len(tool_definitions))
