"""OpenAI backend implementation."""
from typing import Dict, List, Any

from humbug.ai.ai_backend import AIBackend, RequestConfig
from humbug.ai.ai_conversation_settings import AIConversationSettings
from humbug.ai.openai.openai_stream_response import OpenAIStreamResponse


class OpenAIBackend(AIBackend):
    """OpenAI API backend implementation with streaming support."""

    @classmethod
    def get_default_url(cls) -> str:
        """
        Get the default API URL.

        Returns:
            The default URL
        """
        return "https://api.openai.com/v1/chat/completions"

    def _format_messages_for_provider(self, conversation_history: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """Format conversation history for OpenAI's API format."""
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
                # For OpenAI, tool calls are added as a separate field
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
                # For OpenAI, tool results come as separate tool messages
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

    def _build_request_config(
        self,
        conversation_history: List[Dict[str, Any]],
        settings: AIConversationSettings
    ) -> RequestConfig:
        """Build complete request configuration for OpenAI."""
        # Use the pre-formatted messages directly
        messages = self._format_messages_for_provider(conversation_history)

        # Build request data
        data = {
            "model": AIConversationSettings.get_name(settings.model),
            "messages": messages,
            "stream": True,
            "stream_options": {"include_usage": True}
        }

        # Only include temperature if supported by model
        if AIConversationSettings.supports_temperature(settings.model):
            data["temperature"] = settings.temperature

        # Add tools if supported
        if self._supports_tools(settings) and self._tool_manager.has_tools():
            tool_definitions = self._tool_manager.get_tool_definitions_for_provider("openai")
            if tool_definitions:
                data["tools"] = tool_definitions
                data["tool_choice"] = "auto"
                self._logger.debug("Added %d tool definitions for openai", len(tool_definitions))

        self._logger.debug("stream message %r", data)

        # Build headers
        headers = {
            "Content-Type": "application/json",
            "Authorization": f"Bearer {self._api_key}"
        }

        return RequestConfig(
            url=self._api_url,
            headers=headers,
            data=data
        )

    def _create_stream_response_handler(self) -> OpenAIStreamResponse:
        """Create an OpenAI-specific stream response handler."""
        return OpenAIStreamResponse()
