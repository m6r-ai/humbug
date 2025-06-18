"""OpenAI backend implementation."""
from typing import Dict, List

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

    def _build_request_config(
        self,
        conversation_history: List[Dict[str, str]],
        settings: AIConversationSettings
    ) -> RequestConfig:
        """Build complete request configuration for OpenAI."""
        # Format messages for OpenAI (no special formatting needed)
        messages = conversation_history.copy()

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
